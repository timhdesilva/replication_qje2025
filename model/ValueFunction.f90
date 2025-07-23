MODULE ValueFunction_mod

#if defined(MPI)
   USE mpi
#endif

   USE Parameters, only: procid, mpiprocs, dp, positiveInf, negativeInf, epsl, &
                         tolES, OutputDir, maxabsV, gd_age, gdW_LiqW, gdR_LiqW, gd_Debt, &
                         gd_Theta, gd_ExpTheta, gd_PastL, gd_Wage, NbrPolicy, Policy, &
                         JTot, JRet, JWork, NbrWLiqW, NbrRLiqW, NbrDebt, NbrTheta, &
                         NbrPastL, minVCES, gamma, cprob, fcL, fcH, NbrEps_GH, GHw_Eps, &
                         GHx_Eps, V_E, V_NE, V_Ret, SimE, nsim
   USE types
   USE Procedures, only: WriteMatrix2d_real
   USE EconFunctions, only: lowerA
   USE OptimizeWork_mod, only: OptimizeC, OptimizeCL_A
   USE OptimizeRet_mod, only: OptimizeRet

   IMPLICIT NONE

CONTAINS

   !---------------------------------------------------------------------------------------------------------!
   ! Solve for lifecycle value function iteration via backward induction
   !---------------------------------------------------------------------------------------------------------!
   SUBROUTINE ValueFunction

      IMPLICIT NONE

      ! Temporary storage
      real(dp) :: V_C, asset, consump, Vio(2), ceq
      real(dp) :: V_CL(2, NbrEps_GH), asset_CL(2, NbrEps_GH), labor_CL(2, NbrEps_GH)
      integer :: optflag, stype
      logical :: solveE(2)
      real(dp) :: gamma1, gamma1inv

      ! Loop indices for state variables
      integer :: ij, &   ! Age
                 ipcy, & ! Policy parameter
                 lq, &   ! Liquid assets levels
                 dbt, &  ! Debt
                 iz, &   ! Theta
                 pL, &   ! Past labor choice
                 ie      ! Epsilon

      type(state_t) :: state

      integer :: t1, t2, t3, t4, rate  ! time flags
#if defined(MPI)
      real(dp), allocatable :: sbuff_W(:, :), sbuff_R(:)
      real(dp), allocatable :: rbuff_W(:), rbuff_R(:)
      integer, allocatable :: Impi_E(:, :), Impi_NE(:, :)
      integer :: i, ierr, Nmpi_E, Nmpi_NE

      ! Loop flattening for MPI
      Nmpi_E = NbrPolicy*NbrWLiqW*NbrDebt*NbrTheta
      allocate (Impi_E(Nmpi_E, 4))
      Nmpi_E = 0
      do ipcy = 1, NbrPolicy
         do lq = 1, NbrWLiqW
            do dbt = 1, NbrDebt
               do iz = 1, NbrTheta
                  Nmpi_E = Nmpi_E + 1
                  Impi_E(Nmpi_E, 1) = ipcy
                  Impi_E(Nmpi_E, 2) = lq
                  Impi_E(Nmpi_E, 3) = dbt
                  Impi_E(Nmpi_E, 4) = iz
               end do
            end do
         end do
      end do

      Nmpi_NE = Nmpi_E/NbrDebt
      allocate (Impi_NE(Nmpi_NE, 3))
      Nmpi_NE = 0
      do ipcy = 1, NbrPolicy
         do lq = 1, NbrWLiqW
            do iz = 1, NbrTheta
               Nmpi_NE = Nmpi_NE + 1
               Impi_NE(Nmpi_NE, 1) = ipcy
               Impi_NE(Nmpi_NE, 2) = lq
               Impi_NE(Nmpi_NE, 3) = iz
            end do
         end do
      end do
#endif

      ! Pre calculation
      gamma1 = 1d0 - gamma
      gamma1inv = 1d0/gamma1

      ! Allocate size of value functions and initialize
      allocate (V_E(NbrPastL, NbrTheta, NbrDebt, NbrWLiqW, NbrPolicy, JWork))
      allocate (V_NE(NbrPastL, NbrTheta, NbrWLiqW, NbrPolicy, JWork))
      allocate (V_Ret(NbrRLiqW, JRet))
      V_E = 0d0
      V_NE = 0d0
      V_Ret = 0d0

      call SYSTEM_CLOCK(t1, rate)

      ! Determine which individuals to solve for
      solveE = .FALSE.
      if (SUM(SimE) < 2*nsim) solveE(1) = .TRUE.
      if (SUM(SimE) > nsim) solveE(2) = .TRUE.

      ! ---------------------------- LAST PERIOD: DIE WITH CERTAINTY ----------------------------------------------

      ! Fill in final period based on terminal utility
      do lq = 1, NbrRLiqW
         V_Ret(lq, JRet) = 0d0
      end do

      ! ---------------------------- RETIREMENT PERIOD ------------------------------------------------------------

      do ij = JTot - 1, JWork + 1, -1 ! age (backwards)

         call SYSTEM_CLOCK(t2, rate)

#if defined(MPI)
         allocate (sbuff_R(NbrRLiqW))
         sbuff_R = 0d0
         CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)
         !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(lq,state,asset,consump,optflag)
         !$OMP DO SCHEDULE(DYNAMIC)
         do lq = (procid*NbrRLiqW)/mpiprocs + 1, ((procid + 1)*NbrRLiqW)/mpiprocs
#else
            !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(lq,state,asset,consump,optflag)
            !$OMP DO SCHEDULE(DYNAMIC)
            do lq = 1, NbrRLiqW
#endif
               state%ij = ij
               state%liqw = gdR_LiqW(lq, ij - JWork)
               state%Plabor = 0d0 ! need this because enters flow utility function
               V_Ret(lq, ij - JWork) = OptimizeRet(state, asset, consump, optflag)
#if defined(MPI)
               sbuff_R(lq) = V_Ret(lq, ij - JWork)
            end do
            !$OMP END DO
            !$OMP END PARALLEL
            allocate (rbuff_R, mold=sbuff_R)
            if (mpiprocs > 1) then
               CALL MPI_ALLREDUCE(sbuff_R, rbuff_R, NbrRLiqW, MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD, ierr)
               V_Ret(:, ij - JWork) = rbuff_R(:)
            end if
            deallocate (sbuff_R, rbuff_R)
#else
         end do
         !$OMP END DO
         !$OMP END PARALLEL
#endif

         call SYSTEM_CLOCK(t3, rate)

      end do ! age

      if (abs(MAXVAL(V_Ret)) > maxabsV .and. procid .eq. 0) print *, 'Large V_Ret:', MAXVAL(V_Ret)

      ! ---------------------------- WORKING LIFE ----------------------------------------------------------------

      do ij = JWork, 2, -1 ! age (backwards)

         call SYSTEM_CLOCK(t2, rate)

         ! +++++++++++++++++++++++++++++++++++++ EDUCATED INDIVIDUALS +++++++++++++++++++++++++++++++++++++++++++++

         if (solveE(2)) then

#if defined(MPI)

            allocate (sbuff_W(Nmpi_E, NbrPastL))
            sbuff_W = 0d0

            CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)

            !$OMP PARALLEL DEFAULT(SHARED) &
            !$OMP PRIVATE(i,ipcy,lq,dbt,iz,pL,ie,V_CL,V_C,Vio,ceq,state,asset,asset_CL,consump,labor_CL,optflag,stype)
            !$OMP DO SCHEDULE(DYNAMIC)

            do i = (procid*Nmpi_E)/mpiprocs + 1, ((procid + 1)*Nmpi_E)/mpiprocs ! MPI loop

               ipcy = Impi_E(i, 1)
               lq = Impi_E(i, 2)
               dbt = Impi_E(i, 3)
               iz = Impi_E(i, 4)

#else

               !$OMP PARALLEL DEFAULT(SHARED) &
               !$OMP PRIVATE(ipcy,lq,dbt,iz,pL,ie,V_CL,V_C,Vio,ceq,state,asset,asset_CL,consump,labor_CL,optflag,stype)
               !$OMP DO COLLAPSE(4) SCHEDULE(DYNAMIC)

               do ipcy = 1, NbrPolicy
                  do lq = 1, NbrWLiqW
                     do dbt = 1, NbrDebt
                        do iz = 1, NbrTheta

#endif
                           ! Set states
                           state%ed = 2
                           state%ij = ij
                           state%ipcy = ipcy
                           state%liqw = gdW_LiqW(lq, ij)
                           state%debt = gd_Debt(dbt, ij)
                           state%theta = gd_Theta(iz)

                           ! Optimize C and L across epsilons and calvo shocks, assuming adjustment of L occurs
                           state%Plabor = 0d0
                           do ie = 1, NbrEps_GH ! epsilon_t
                              state%epsln = GHx_Eps(ie)
                                 !! CASE 1: Calvo shock doesn't hit
                              state%calvo = 1d0
                              V_CL(1, ie) = OptimizeCL_A(state, asset_CL(1, ie), consump, labor_CL(1, ie), optflag, stype)
                                 !! CASE 2: Calvo shock hits
                              state%calvo = 0d0
                              V_CL(2, ie) = OptimizeCL_A(state, asset_CL(2, ie), consump, labor_CL(2, ie), optflag, stype)
                           end do ! epsilon_t

                           ! Calculate CEQ at each pL after integrating out calvo and epsilon shocks
                           do pL = 1, NbrPastL ! pL
                              state%Plabor = gd_PastL(pL)
                              ! Initialize
                              ceq = 0d0
                              do ie = 1, NbrEps_GH ! epsilon_t
                                 state%epsln = GHx_Eps(ie)
                                 ! Optimize C with no adjustment, which does not depend on Calvo shock
                                 V_C = OptimizeC(state, asset, consump, optflag, stype)
                                    !! CASE 1: Calvo shock doesn't hit
                                 if (V_CL(1, ie) > V_C .or. fcH < epsl) then
                                    Vio(1) = V_CL(1, ie)
                                 else
                                    Vio(1) = V_C
                                 end if
                                 Vio(1) = MAX(Vio(1), minVCES)**gamma1
                                    !! CASE 2: Calvo shock hits
                                 if (V_CL(2, ie) > V_C .or. fcL < epsl) then
                                    Vio(2) = V_CL(2, ie)
                                 else
                                    Vio(2) = V_C
                                 end if
                                 Vio(2) = MAX(Vio(2), minVCES)**gamma1
                                 ! Add to CEQ
                                 ceq = ceq + GHw_Eps(ie)*((1d0 - cprob)*Vio(1) + cprob*Vio(2))
                              end do ! epsilon_t
                              V_E(pL, iz, dbt, lq, ipcy, ij) = ceq**gamma1inv

#if defined(MPI)
                              ! Store results for MPI
                              sbuff_W(i, pL) = V_E(pL, iz, dbt, lq, ipcy, ij)
#endif

                           end do ! pL
#if defined(MPI)
                        end do ! MPI loop

                        !$OMP END DO
                        !$OMP END PARALLEL

                        allocate (rbuff_W(Nmpi_E))

                        if (mpiprocs > 1) then
                           do pL = 1, NbrPastL
                              CALL MPI_ALLREDUCE(sbuff_W(:, pL), rbuff_W, Nmpi_E, &
                                                 MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD, ierr)
                              do i = 1, Nmpi_E
                                 ipcy = Impi_E(i, 1)
                                 lq = Impi_E(i, 2)
                                 dbt = Impi_E(i, 3)
                                 iz = Impi_E(i, 4)
                                 V_E(pL, iz, dbt, lq, ipcy, ij) = rbuff_W(i)
                              end do
                           end do
                        end if

                        deallocate (sbuff_W, rbuff_W)
#else
                     end do
                  end do
               end do
            end do

            !$OMP END DO
            !$OMP END PARALLEL
#endif

         else
            V_E = negativeInf
         end if
         ! +++++++++++++++++++++++++++++++++++++ NON-EDUCATED INDIVIDUALS +++++++++++++++++++++++++++++++++++++++++++++

         if (solveE(1)) then

#if defined(MPI)

            allocate (sbuff_W(Nmpi_NE, NbrPastL))
            sbuff_W = 0d0

            CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)

            !$OMP PARALLEL DEFAULT(SHARED) &
            !$OMP PRIVATE(i,ipcy,lq,iz,pL,ie,V_CL,V_C,Vio,ceq,state,asset,asset_CL,consump,labor_CL,optflag,stype)
            !$OMP DO SCHEDULE(DYNAMIC)

            do i = (procid*Nmpi_NE)/mpiprocs + 1, ((procid + 1)*Nmpi_NE)/mpiprocs ! MPI loop

               ipcy = Impi_NE(i, 1)
               lq = Impi_NE(i, 2)
               iz = Impi_NE(i, 3)

#else

               !$OMP PARALLEL DEFAULT(SHARED) &
               !$OMP PRIVATE(ipcy,lq,iz,pL,ie,V_CL,V_C,Vio,ceq,state,asset,asset_CL,consump,labor_CL,optflag,stype)
               !$OMP DO COLLAPSE(3) SCHEDULE(DYNAMIC)

               do ipcy = 1, NbrPolicy
                  do lq = 1, NbrWLiqW
                     do iz = 1, NbrTheta

#endif
                        ! Set states
                        state%ed = 1
                        state%ij = ij
                        state%ipcy = ipcy
                        state%liqw = gdW_LiqW(lq, ij)
                        state%debt = 0d0
                        state%theta = gd_Theta(iz)

                        ! Optimize C and L across epsilons and calvo shocks, assuming adjustment of L occurs
                        state%Plabor = 0d0
                        do ie = 1, NbrEps_GH ! epsilon_t
                           state%epsln = GHx_Eps(ie)
                              !! CASE 1: Calvo shock doesn't hit
                           state%calvo = 1d0
                           V_CL(1, ie) = OptimizeCL_A(state, asset_CL(1, ie), consump, labor_CL(1, ie), optflag, stype)
                              !! CASE 2: Calvo shock hits
                           state%calvo = 0d0
                           V_CL(2, ie) = OptimizeCL_A(state, asset_CL(2, ie), consump, labor_CL(2, ie), optflag, stype)
                        end do ! epsilon_t

                        ! Calculate CEQ at each pL after integrating out calvo and epsilon shocks
                        do pL = 1, NbrPastL ! pL
                           state%Plabor = gd_PastL(pL)
                           ! Initialize
                           ceq = 0d0
                           do ie = 1, NbrEps_GH ! epsilon_t
                              state%epsln = GHx_Eps(ie)
                              ! Optimize C with no adjustment, which does not depend on Calvo shock
                              V_C = OptimizeC(state, asset, consump, optflag, stype)
                                 !! CASE 1: Calvo shock doesn't hit
                              if (V_CL(1, ie) > V_C .or. fcH < epsl) then
                                 Vio(1) = V_CL(1, ie)
                              else
                                 Vio(1) = V_C
                              end if
                              Vio(1) = MAX(Vio(1), minVCES)**gamma1
                                 !! CASE 2: Calvo shock hits
                              if (V_CL(2, ie) > V_C .or. fcL < epsl) then
                                 Vio(2) = V_CL(2, ie)
                              else
                                 Vio(2) = V_C
                              end if
                              Vio(2) = MAX(Vio(2), minVCES)**gamma1
                              ! Add to CEQ
                              ceq = ceq + GHw_Eps(ie)*((1d0 - cprob)*Vio(1) + cprob*Vio(2))
                           end do ! epsilon_t
                           V_NE(pL, iz, lq, ipcy, ij) = ceq**gamma1inv

#if defined(MPI)
                           ! Store results for MPI
                           sbuff_W(i, pL) = V_NE(pL, iz, lq, ipcy, ij)
#endif

                        end do ! pL
#if defined(MPI)

                     end do ! MPI loop

                     !$OMP END DO
                     !$OMP END PARALLEL

                     allocate (rbuff_W(Nmpi_NE))

                     if (mpiprocs > 1) then
                        do pL = 1, NbrPastL
                           CALL MPI_ALLREDUCE(sbuff_W(:, pL), rbuff_W, Nmpi_NE, &
                                              MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD, ierr)
                           do i = 1, Nmpi_NE
                              ipcy = Impi_NE(i, 1)
                              lq = Impi_NE(i, 2)
                              iz = Impi_NE(i, 3)
                              V_NE(pL, iz, lq, ipcy, ij) = rbuff_W(i)
                           end do
                        end do
                     end if

                     deallocate (sbuff_W, rbuff_W)
#else
                  end do
               end do
            end do

            !$OMP END DO
            !$OMP END PARALLEL
#endif

         else
            V_NE = negativeInf
         end if

! +++++++++++++++++++++++++++++++++++++ END NON-EDUCATED INDIVIDUALS +++++++++++++++++++++++++++++++++++++++++

         call SYSTEM_CLOCK(t3, rate)

      end do ! age

! ---------------------------- END WORKING LIFE ----------------------------------------------------------------

#if defined(MPI)
      deallocate (Impi_E, Impi_NE)
#endif

      call SYSTEM_CLOCK(t4, rate)
      if (procid .eq. 0) print *, 'Finished value functions. Seconds spent = ', real(t4 - t1)/real(rate)

   END SUBROUTINE ValueFunction

!---------------------------------------------------------------------------------------------------------!
! Clean up value functions
!---------------------------------------------------------------------------------------------------------!

   SUBROUTINE CleanSolution
      deallocate (V_E, V_NE, V_Ret)
   END SUBROUTINE CleanSolution

END MODULE ValueFunction_mod
