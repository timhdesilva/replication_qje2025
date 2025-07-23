MODULE Simulations

#if defined(MPI)
   USE mpi
#endif

   USE Parameters, only: procid, mpiprocs, sp, dp, negativeInf, negativeInf_sp, &
                         epsl, RunMode, OutputDir, LastYr, SaveLCPanel, Policy, NbrPolicy, &
                         minL, maxL, maxabsV, floorC, nsim, realind_E2, JTot, JWork, &
                         NbrWLiqW, NbrRLiqW, NbrTheta, NbrDebt, gdW_LiqW, gdR_LiqW, &
                         gd_Debt, gd_Theta, gd_age, cum_survival, numeraire, rhotheta, &
                         sigmanu, sigmaeps, sigmai, R_d, SimW, SimTheta, SimEps, SimY, &
                         SimWL, SimV, SimVS, SimLiqWealth, SimConsumption, SimAssets, &
                         SimLabor, SimLaborS, SimDebt, SimRepay, SimTT, SimFlag, SimSolType, &
                         SimSince, SimE, SimRawNu, SimRawEps, SimHELPSwitchIJ, FirstIJDeath, &
                         SimDebt0, SimAssets0, SimPolicyI, SimCalvo, AgeRepay, SMMError, &
                         SimPanel, Lifecycle, ncol_SimPanel, avgV0, SaveSimY, SaveSimV, &
                         OptimalType
   USE types
   USE Procedures, only: WriteMatrix1d_real, WriteMatrix2d_real, mean_array, is_in_array
   USE EconFunctions, only: flowU, CEF, RA, HELPIncome, DebtPayment, &
                            TaxesTransfers, HELPRate, FloorTransfer, CalcCash, CalcWage
   USE Setup, only: ReadShocks, DeallocShocks
   USE OptParameters, only: WarnFileNum
   USE OptimizeWork_mod, only: OptimizeCL, OptimizeC
   USE OptimizeRet_mod, only: OptimizeRet

   IMPLICIT NONE

CONTAINS

   !---------------------------------------------------------------------------------------------------------!
   ! Functions for dealing with simulation outputs that will be written out to files for memory minimization
   !---------------------------------------------------------------------------------------------------------!

   SUBROUTINE AllocSimFiles(size_ij)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: size_ij
      allocate (SimY(nsim, size_ij))
      allocate (SimWL, mold=SimY)
      allocate (SimLaborS(nsim, size_ij))
      allocate (SimDebt, SimRepay, mold=SimLaborS)
   END SUBROUTINE AllocSimFiles

   SUBROUTINE DeallocSimFiles
      IMPLICIT NONE
      deallocate (SimY, SimLaborS, SimWL, SimDebt, SimRepay)
   END SUBROUTINE DeallocSimFiles

   SUBROUTINE WriteSimFiles
      IMPLICIT NONE
      OPEN (UNIT=10, FORM='unformatted', FILE=trim(OutputDir)//'SimY')
      WRITE (10) SimY
      CLOSE (UNIT=10)
      OPEN (UNIT=10, FORM='unformatted', FILE=trim(OutputDir)//'SimLaborS')
      WRITE (10) SimLaborS
      CLOSE (UNIT=10)
      OPEN (UNIT=10, FORM='unformatted', FILE=trim(OutputDir)//'SimWL')
      WRITE (10) SimWL
      CLOSE (UNIT=10)
      OPEN (UNIT=10, FORM='unformatted', FILE=trim(OutputDir)//'SimDebt')
      WRITE (10) SimDebt
      CLOSE (UNIT=10)
      OPEN (UNIT=10, FORM='unformatted', FILE=trim(OutputDir)//'SimRepay')
      WRITE (10) SimRepay
      CLOSE (UNIT=10)
   END SUBROUTINE WriteSimFiles

   SUBROUTINE ReadSimFiles
      IMPLICIT NONE
      OPEN (UNIT=10, FORM='unformatted', FILE=trim(OutputDir)//'SimY')
      READ (10) SimY
      CLOSE (UNIT=10)
      OPEN (UNIT=10, FORM='unformatted', FILE=trim(OutputDir)//'SimLaborS')
      READ (10) SimLaborS
      CLOSE (UNIT=10)
      OPEN (UNIT=10, FORM='unformatted', FILE=trim(OutputDir)//'SimWL')
      READ (10) SimWL
      CLOSE (UNIT=10)
      OPEN (UNIT=10, FORM='unformatted', FILE=trim(OutputDir)//'SimDebt')
      READ (10) SimDebt
      CLOSE (UNIT=10)
      OPEN (UNIT=10, FORM='unformatted', FILE=trim(OutputDir)//'SimRepay')
      READ (10) SimRepay
      CLOSE (UNIT=10)
   END SUBROUTINE ReadSimFiles

   !---------------------------------------------------------------------------------------------------------!
   ! Main routine to simulate life cycle choices
   !---------------------------------------------------------------------------------------------------------!

   SUBROUTINE LCSimulation(switch_sim)

      IMPLICIT NONE
      integer, intent(in) :: switch_sim ! parameter controlling whether you simulate HELP change
      integer :: t1, t2, rate  ! time flags
      integer :: ind, ij ! loop indices
      integer, allocatable, dimension(:) :: errors ! errors

      ! Temporary storage
      real(dp) :: sTheta, sW, sV, sConsumption, sAssets, sLiqW, sLabor, sDebt, sRepay
      integer :: sSince, sSolType, sFlag

      type(state_t) :: state

#if defined(MPI)
      real(sp), allocatable :: rbuffR_sp(:)
      real(dp), allocatable :: rbuffR(:)
      integer, allocatable :: rbuffI(:)
      integer :: ierr
#endif
      call SYSTEM_CLOCK(t1, rate)

      ! Read in shocks
      CALL ReadShocks

      ! Allocations and initializations
      if (SaveLCPanel .eq. 0) then
         CALL AllocSimFiles(JWork)
      else
         CALL AllocSimFiles(JTot)
         allocate (SimSince(nsim, JTot))
         allocate (SimV, SimLiqWealth, SimAssets, SimConsumption, &
                   SimTT, SimW, SimTheta, SimEps, mold=SimY)
         allocate (SimFlag, SimSolType, mold=SimSince)
         SimV = negativeInf
         SimLiqWealth = negativeInf
         SimAssets = negativeInf
         SimConsumption = negativeInf
         SimTT = 0d0
         SimW = 0d0
         SimTheta = 0d0
         SimEps = 0d0
         SimSince = 0
         SimSolType = 0
         SimFlag = 0
      end if
      SimY = 0d0
      SimLaborS = negativeInf_sp
      SimWL = 0d0
      SimDebt = 0e0
      SimRepay = 0e0
      if (RunMode > 0) then
         allocate (errors(nsim))
         errors = 0
         if (RunMode >= 4) then
            allocate (SimV(nsim, 1))
            allocate (SimTT(nsim, JWork))
            SimV = negativeInf
            SimTT = 0d0
            allocate (AgeRepay(nsim))
            AgeRepay = 0
         end if
      end if

#if defined(MPI)
      CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)
#endif

      !$OMP PARALLEL DEFAULT(SHARED) &
      !$OMP PRIVATE(ind,ij,state,sTheta,sW,sV,sConsumption,sAssets,sLiqW,sLabor,sDebt,sRepay,sSince,sSolType,sFlag) &
      !$OMP PRIVATE(state_tf,junk1,junk3)
      !$OMP DO SCHEDULE(DYNAMIC)

      do ind = (procid*nsim)/mpiprocs + 1, ((procid + 1)*nsim)/mpiprocs ! MPI loop

         ! Intialization for first year of your life
         sAssets = SimAssets0(ind)/RA(epsl)
         sDebt = SimDebt0(ind)
         sTheta = sigmai*SimRawNu(ind, 1)

         ! Save
         if (SaveLCPanel > 0) then
            SimAssets(ind, 1) = sAssets
            SimTheta(ind, 1) = sTheta
         end if

         loop_age: do ij = 1, JTot - 1 ! loop through life cycle, excluding last period

            ! Exit if not saving any retirement information
            if ((SaveLCPanel .eq. 0) .and. ij > JWork) exit loop_age

            ! Exit if dead
            if (ij >= FirstIJDeath(ind)) exit loop_age

            ! Calculate policy year and exit simulation if past what's needed for simulation
            SELECT CASE (switch_sim)
            CASE (0)
               IF (RunMode .eq. 0) THEN
                  state%ipcy = SimPolicyI(ind)
               ELSE
                  state%ipcy = 1
               END IF
               sSince = 0
            CASE (1)
               if (ij < SimHELPSwitchIJ(ind)) then
                  state%ipcy = 1
               else
                  state%ipcy = 2
               end if
               sSince = ij - SimHELPSwitchIJ(ind)
               if (sSince > LastYr - 2005) exit loop_age
            END SELECT

            ! State evolution
            if (ij > 1 .and. ij <= JWork) sTheta = rhotheta*sTheta + sigmanu*SimRawNu(ind, ij)
            sLiqW = sAssets*RA(sAssets)

            ! Optimization over consumption and labor choice
            state%ed = SimE(ind)
            state%ij = ij
            state%liqw = sLiqW
            state%debt = sDebt
            state%theta = sTheta
            if (ij <= JWork) then
               state%epsln = sigmaeps*SimRawEps(ind, ij)
               state%calvo = SimCalvo(ind, ij)
               if (ij .eq. 1) then
                  state%Plabor = 0d0
               else
                  state%Plabor = sLabor
               end if
               sV = OptimizeCL(state, sAssets, sConsumption, sLabor, sFlag, sSolType)
            else
               state%epsln = 0d0
               state%Plabor = 0d0
               sV = OptimizeRet(state, sAssets, sConsumption, sFlag)
               sLabor = 0d0
               sSolType = 0
            end if

            ! Keep track of income and labor supply and debt
            if (ij <= JWork) then
               sW = CalcWage(state)
            else
               sW = 0d0
            end if
            SimWL(ind, ij) = sW*sLabor
            SimY(ind, ij) = HELPIncome(sLabor, state)
            SimLaborS(ind, ij) = real(sLabor, kind=sp)
            SimDebt(ind, ij) = real(sDebt, kind=sp)

            ! Keep track of repayment and update debt for next period
            if (SimE(ind) > 1) then
               sRepay = DebtPayment(sLabor, state)
               sDebt = R_d*sDebt - sRepay
            else
               sRepay = 0d0
               sDebt = 0d0
            end if
            SimRepay(ind, ij) = real(sRepay, kind=sp)

            ! Save
            if (SaveLCPanel > 0) then
               SimV(ind, ij) = sV
               SimLiqWealth(ind, ij) = sLiqW
               SimAssets(ind, ij + 1) = sAssets
               SimConsumption(ind, ij) = sConsumption
               SimTT(ind, ij) = TaxesTransfers(ij, Policy(state%ipcy), SimY(ind, ij), sLiqW) &
                                + FloorTransfer(sLabor, state)
               SimW(ind, ij) = sW
               SimTheta(ind, ij) = sTheta
               SimEps(ind, ij) = state%epsln
               SimSince(ind, ij) = sSince
               SimSolType(ind, ij) = sSolType
               SimFlag(ind, ij) = sFlag
            end if
            if (RunMode > 0) then
               if (RunMode >= 4) then
                  if (ij .eq. 1) SimV(ind, 1) = sV
                  SimTT(ind, ij) = TaxesTransfers(ij, Policy(state%ipcy), SimY(ind, ij), sLiqW) &
                                   + FloorTransfer(sLabor, state)
                  if (AgeRepay(ind) .eq. 0) then
                     if (ij + 1 < FirstIJDeath(ind)) then
                        if (sDebt <= 0d0 .and. sRepay > 0d0) then
                           AgeRepay(ind) = ij
                        else if (ij .eq. JWork .and. sDebt > 0d0) then
                           AgeRepay(ind) = JWork + 1
                        end if
                     else
                        AgeRepay(ind) = JWork + 1
                     end if
                  end if
               end if
            end if

            ! Check to make sure consumption and labor supply are positive and value function is not too large
            if (RunMode .eq. 0 .and. ij <= JWork) then

               if (sConsumption < 0d0) then
                  print *, "--"
                  print *, 'procid = ', procid
                  print *, "Negative consumption in simulation of:", sConsumption
                  print *, "Value function =", sV
                  print *, "States:"
                  print *, "  id =", ind
                  print *, "  Educated =", SimE(ind)
                  print *, '  Age index =', ij
                  print *, '  Policy =', Policy(state%ipcy)
                  print *, "  Liquid wealth = ", sLiqW
                  print *, "  Debt = ", sDebt
                  print *, "  Income = ", SimY(ind, ij), SimY(ind, ij)*numeraire
                  print *, "  Past labor = ", state%Plabor
                  print *, "  Cash on hand = ", CalcCash(sLabor, state)
                  print *, "Policies:"
                  print *, "  Assets = ", sAssets
                  print *, "  Labor = ", sLabor
                  print *, "  Flag = ", sFlag
                  print *, "  Type = ", sSolType
               end if
               if (sLabor < minL) then
                  print *, "--"
                  print *, "Too low labor supply in simulation of:", sLabor
                  print *, "Value function =", sV
                  print *, "States:"
                  print *, "  id =", ind
                  print *, "  Educated =", SimE(ind)
                  print *, '  Age index =', ij
                  print *, '  Policy =', Policy(state%ipcy)
                  print *, "  Liquid wealth = ", sLiqW
                  print *, "  Debt = ", sDebt
                  print *, "  Income = ", SimY(ind, ij), SimY(ind, ij)*numeraire
                  print *, "  Past labor = ", state%Plabor
                  print *, "  Cash on hand = ", CalcCash(sLabor, state)
                  print *, "Policies:"
                  print *, "  Assets = ", sAssets
                  print *, "  Labor = ", sLabor
                  print *, "  Flag = ", sFlag
                  print *, "  Type = ", sSolType
               end if
               if (abs(sV) > maxabsV) then
                  print *, "--"
                  print *, 'procid = ', procid
                  print *, "Large value function in simulation of:", sV
                  print *, "States:"
                  print *, "  id =", ind
                  print *, "  Educated =", SimE(ind)
                  print *, '  Age index =', ij
                  print *, '  Policy =', Policy(state%ipcy)
                  print *, "  Liquid wealth = ", sLiqW
                  print *, "  Debt = ", sDebt
                  print *, "  Income = ", SimY(ind, ij), SimY(ind, ij)*numeraire
                  print *, "  Past labor = ", state%Plabor
                  print *, "  Cash on hand = ", CalcCash(sLabor, state)
                  print *, "Policies:"
                  print *, "  Assets = ", sAssets
                  print *, "  Labor = ", sLabor
                  print *, "  Flag = ", sFlag
                  print *, "  Type = ", sSolType
               end if

            else if (RunMode > 0) then
               ! Exit inner (non-parallelized) loop if any warning conditions met
               if (sConsumption < 0d0 .or. sLabor < minL .or. abs(sV) > maxabsV) then
                  errors(ind) = 1
                  exit loop_age
               end if
            end if

         end do loop_age

         ! Set death period utility equal to terminal utility and consumption to zero
         if (SaveLCPanel > 0) then
            SimV(ind, JTot) = 0d0
            SimLiqWealth(ind, JTot) = SimAssets(ind, JTot)*RA(SimAssets(ind, JTot))
            SimConsumption(ind, JTot) = 0d0
            SimLaborS(ind, JTot) = 0e0
         end if

      end do

      !$OMP END DO
      !$OMP END PARALLEL

#if defined(MPI)
      if (mpiprocs > 1) then

         if (SaveLCPanel .eq. 0) then
            allocate (rbuffR_sp(nsim))
            allocate (rbuffR(nsim))
            do ij = 1, JWork
               ! SUM reduce: reals
               CALL MPI_ALLREDUCE(SimY(:, ij), rbuffR, nsim, MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD, ierr)
               SimY(:, ij) = rbuffR
               CALL MPI_ALLREDUCE(SimLaborS(:, ij), rbuffR_sp, nsim, MPI_REAL, MPI_MAX, MPI_COMM_WORLD, ierr)
               SimLaborS(:, ij) = rbuffR_sp
               CALL MPI_ALLREDUCE(SimWL(:, ij), rbuffR, nsim, MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD, ierr)
               SimWL(:, ij) = rbuffR
               CALL MPI_ALLREDUCE(SimDebt(:, ij), rbuffR_sp, nsim, MPI_REAL, MPI_SUM, MPI_COMM_WORLD, ierr)
               SimDebt(:, ij) = rbuffR_sp
               CALL MPI_ALLREDUCE(SimRepay(:, ij), rbuffR_sp, nsim, MPI_REAL, MPI_SUM, MPI_COMM_WORLD, ierr)
               SimRepay(:, ij) = rbuffR_sp
            end do
            deallocate (rbuffR_sp, rbuffR)
         else
            allocate (rbuffR_sp(nsim))
            allocate (rbuffR(nsim))
            allocate (rbuffI(nsim))
            do ij = 1, JTot
               ! MAX reduce
               CALL MPI_ALLREDUCE(SimV(:, ij), rbuffR, nsim, MPI_DOUBLE_PRECISION, MPI_MAX, MPI_COMM_WORLD, ierr)
               SimV(:, ij) = rbuffR
               CALL MPI_ALLREDUCE(SimLiqWealth(:, ij), rbuffR, nsim, MPI_DOUBLE_PRECISION, MPI_MAX, MPI_COMM_WORLD, ierr)
               SimLiqWealth(:, ij) = rbuffR
               CALL MPI_ALLREDUCE(SimAssets(:, ij), rbuffR, nsim, MPI_DOUBLE_PRECISION, MPI_MAX, MPI_COMM_WORLD, ierr)
               SimAssets(:, ij) = rbuffR
               CALL MPI_ALLREDUCE(SimConsumption(:, ij), rbuffR, nsim, MPI_DOUBLE_PRECISION, MPI_MAX, MPI_COMM_WORLD, ierr)
               SimConsumption(:, ij) = rbuffR
               CALL MPI_ALLREDUCE(SimLaborS(:, ij), rbuffR_sp, nsim, MPI_REAL, MPI_MAX, MPI_COMM_WORLD, ierr)
               SimLaborS(:, ij) = rbuffR_sp
               ! SUM reduce: reals
               CALL MPI_ALLREDUCE(SimY(:, ij), rbuffR, nsim, MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD, ierr)
               SimY(:, ij) = rbuffR
               CALL MPI_ALLREDUCE(SimWL(:, ij), rbuffR, nsim, MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD, ierr)
               SimWL(:, ij) = rbuffR
               CALL MPI_ALLREDUCE(SimDebt(:, ij), rbuffR_sp, nsim, MPI_REAL, MPI_SUM, MPI_COMM_WORLD, ierr)
               SimDebt(:, ij) = rbuffR_sp
               CALL MPI_ALLREDUCE(SimRepay(:, ij), rbuffR_sp, nsim, MPI_REAL, MPI_SUM, MPI_COMM_WORLD, ierr)
               SimRepay(:, ij) = rbuffR_sp
               CALL MPI_ALLREDUCE(SimTT(:, ij), rbuffR, nsim, MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD, ierr)
               SimTT(:, ij) = rbuffR
               CALL MPI_ALLREDUCE(SimW(:, ij), rbuffR, nsim, MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD, ierr)
               SimW(:, ij) = rbuffR
               CALL MPI_ALLREDUCE(SimTheta(:, ij), rbuffR, nsim, MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD, ierr)
               SimTheta(:, ij) = rbuffR
               CALL MPI_ALLREDUCE(SimEps(:, ij), rbuffR, nsim, MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD, ierr)
               SimEps(:, ij) = rbuffR
               ! SUM reduce: integers
               CALL MPI_ALLREDUCE(SimSince(:, ij), rbuffI, nsim, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierr)
               SimSince(:, ij) = rbuffI
               CALL MPI_ALLREDUCE(SimSolType(:, ij), rbuffI, nsim, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierr)
               SimSolType(:, ij) = rbuffI
               CALL MPI_ALLREDUCE(SimFlag(:, ij), rbuffI, nsim, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierr)
               SimFlag(:, ij) = rbuffI
            end do
            deallocate (rbuffR_sp, rbuffR, rbuffI)
         end if
         if (RunMode > 0) then
            allocate (rbuffI(nsim))
            CALL MPI_ALLREDUCE(errors, rbuffI, nsim, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierr)
            errors = rbuffI
            deallocate (rbuffI)
            if (RunMode >= 4) then
               allocate (rbuffR(nsim))
               CALL MPI_ALLREDUCE(SimV(:, 1), rbuffR, nsim, MPI_DOUBLE_PRECISION, MPI_MAX, MPI_COMM_WORLD, ierr)
               SimV(:, 1) = rbuffR
               do ij = 1, JWork
                  CALL MPI_ALLREDUCE(SimTT(:, ij), rbuffR, nsim, MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD, ierr)
                  SimTT(:, ij) = rbuffR
               end do
               deallocate (rbuffR)
               allocate (rbuffI(nsim))
               CALL MPI_ALLREDUCE(AgeRepay, rbuffI, nsim, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierr)
               AgeRepay = rbuffI
               deallocate (rbuffI)
            end if
         end if

      end if
#endif

      ! Output age of repayment
      if (RunMode >= 4) then
         if (procid .eq. 0) then
            OPEN (UNIT=10, FORM='unformatted', FILE=trim(OutputDir)//'AgeRepay')
            WRITE (10) AgeRepay
            CLOSE (UNIT=10)
         end if
         DEALLOCATE (AgeRepay)
      end if

      ! Write output variables needed for SMM moments
      if (procid .eq. 0) CALL WriteSimFiles
      CALL DeallocSimFiles

#if defined(MPI)
      CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)
#endif

      ! Deallocate shocks
      CALL DeallocShocks

      ! Count errors
      if (RunMode > 0) then
         SMMError = sum(errors)
         deallocate (errors)
         ! Calculate three value functions in period zero
         if (RunMode >= 4) then
            avgV0(1) = CEF(SimV(:, 1))
            avgV0(2) = CEF(SimV(:realind_E2 - 1, 1))
            avgV0(3) = CEF(SimV(realind_E2:, 1))
            deallocate (SimV)
         end if
      else
         SMMError = 0
      end if

      ! Print time of simulation
      call SYSTEM_CLOCK(t2, rate)
      if (procid .eq. 0) print *, 'Simulation done. Seconds spent:', real(t2 - t1)/real(rate)

   END SUBROUTINE LCSimulation

   !---------------------------------------------------------------------------------------------------------!
   ! Function to calculate average value function in first year of life, with possible t=0 cash transfer
   !---------------------------------------------------------------------------------------------------------!

   SUBROUTINE SimulateFirst(transfer, meanV0, sum_errors)

      IMPLICIT NONE
      real(dp), intent(in) :: transfer ! size of cash transfer at t=0
      real(dp), intent(out) :: meanV0(3) ! average value function in first year of life
      integer, intent(out) :: sum_errors ! number of errors
      integer :: t1, t2, rate  ! time flags
      integer :: ind ! loop index

      ! Temporary storage
      real(dp) :: sConsumption, sAssets, sLabor
      integer :: sSolType, sFlag
      real(dp), allocatable :: SimV0(:)
      integer, allocatable, dimension(:) :: errors ! errors

      type(state_t) :: state

#if defined(MPI)
      real(dp), allocatable :: rbuffR(:)
      integer, allocatable :: rbuffI(:)
      integer :: ierr
#endif

      call SYSTEM_CLOCK(t1, rate)

      ! Read in shocks
      CALL ReadShocks

      ! Allocations and initializations
      allocate (SimV0(nsim), errors(nsim))
      SimV0 = 0d0
      errors = 0

#if defined(MPI)
      CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)
#endif

      !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(ind,state,sConsumption,sAssets,sLabor,sSolType,sFlag)
      !$OMP DO SCHEDULE(DYNAMIC)

      do ind = (procid*nsim)/mpiprocs + 1, ((procid + 1)*nsim)/mpiprocs

         ! Value function in first year of life
         state%ipcy = 1 ! everyone gets first policy
         state%ed = SimE(ind)
         state%ij = 1
         state%liqw = SimAssets0(ind)
         if (ABS(transfer) > epsl) state%liqw = state%liqw + transfer
         state%debt = SimDebt0(ind)
         state%theta = sigmai*SimRawNu(ind, 1)
         state%Plabor = 0d0
         state%epsln = sigmaeps*SimRawEps(ind, 1)
         state%calvo = SimCalvo(ind, 1)
         SimV0(ind) = OptimizeCL(state, sAssets, sConsumption, sLabor, sFlag, sSolType)

         ! Check to make sure consumption and labor supply are positive and value function is not too large
         if (sConsumption < 0d0) then
            print *, "--"
            print *, 'procid = ', procid
            print *, "Negative consumption in simulation of:", sConsumption
            print *, "Value function =", SimV0(ind)
            print *, "  id =", ind
         end if
         if (sLabor < minL) then
            print *, "--"
            print *, "Too low labor supply in simulation of:", sLabor
            print *, "Value function =", SimV0(ind)
            print *, "  id =", ind
         end if
         if (abs(SimV0(ind)) > maxabsV) then
            print *, "--"
            print *, 'procid = ', procid
            print *, "Large value function in simulation of:", SimV0(ind)
            print *, "  id =", ind
         end if

      end do

      !$OMP END DO
      !$OMP END PARALLEL

      ! Deallocate shocks
      CALL DeallocShocks

#if defined(MPI)
      ! Collect results
      if (mpiprocs > 1) then
         allocate (rbuffR(nsim))
         CALL MPI_ALLREDUCE(SimV0, rbuffR, nsim, MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD, ierr)
         SimV0 = rbuffR
         deallocate (rbuffR)
         allocate (rbuffI(nsim))
         CALL MPI_ALLREDUCE(errors, rbuffI, nsim, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierr)
         errors = rbuffI
         deallocate (rbuffI)
      end if
#endif

      ! Calculate three value functions in period zero and number of errors
      meanV0(1) = CEF(SimV0)
      meanV0(2) = CEF(SimV0(:realind_E2 - 1))
      meanV0(3) = CEF(SimV0(realind_E2:))
      sum_errors = SUM(errors)
      deallocate (SimV0, errors)

      ! Print time of simulation
      call SYSTEM_CLOCK(t2, rate)
      if (procid .eq. 0) print *, 'SimulateFirst done. Seconds spent:', real(t2 - t1)/real(rate)

   END SUBROUTINE SimulateFirst

   !---------------------------------------------------------------------------------------------------------!
   ! Routine to simulate life cycle choices, which allows you to write out or read in labor supply
   ! choices from a file. This is used in the following PROGRAMs:
   !     Main_ComparePolicies.f90
   !     Main_OptimalPolicy.f90
   !---------------------------------------------------------------------------------------------------------!

   SUBROUTINE LCSimulation_ReadL(pcyL)

      IMPLICIT NONE
      integer, intent(in), optional :: pcyL ! policy to use labor supply from - if not present, labor supply written out
      integer :: t1, t2, rate  ! time flags
      integer :: ind, ij ! loop indices
      integer, allocatable, dimension(:) :: errors ! errors

      ! Temporary storage
      real(dp) :: sTheta, sV, sConsumption, sAssets, sLiqW, sLabor, sDebt, sRepay
      integer :: sSolType, sFlag
      character(len=100) :: policy_str, filename

      type(state_t) :: state

#if defined(MPI)
      real(sp), allocatable :: rbuffR_sp(:)
      real(dp), allocatable :: rbuffR(:)
      integer, allocatable :: rbuffI(:)
      integer :: ierr
#endif

      call SYSTEM_CLOCK(t1, rate)

      ! Read in shocks
      CALL ReadShocks

      ! Allocations and initializations
      IF (RunMode .eq. 8 .and. SaveSimV) THEN
         allocate (SimV(nsim, JWork))
      ELSE
         allocate (SimV(nsim, 1))
      END IF
      allocate (SimTT(nsim, JWork))
      allocate (SimRepay(nsim, JWork))
      allocate (errors(nsim))
      SimV = negativeInf
      SimTT = 0d0
      SimRepay = 0e0
      errors = 0
      IF (RunMode .eq. 4) THEN
         allocate (AgeRepay(nsim))
         AgeRepay = 0
      ELSE IF (RunMode .eq. 8 .and. SaveSimY) THEN
         allocate (SimY, mold=SimTT)
         SimY = 0d0
      END IF

      ! Read in or initialize labor supply
      allocate (SimLabor, mold=SimTT)
      IF (PRESENT(pcyL)) THEN
         WRITE (policy_str, '(I0)') pcyL
         OPEN (UNIT=10, FORM='unformatted', FILE=trim(OutputDir)//'SimLabor4_'//trim(policy_str))
         READ (10) SimLabor
         CLOSE (UNIT=10)
      ELSE
         WRITE (policy_str, '(I0)') Policy(1)
         SimLabor = negativeInf
      END IF

#if defined(MPI)
      CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)
#endif

      !$OMP PARALLEL DEFAULT(SHARED) &
      !$OMP PRIVATE(ind,ij,state,sTheta,sV,sConsumption,sAssets,sLiqW,sLabor,sDebt,sRepay,sSolType,sFlag,i_s0)
      !$OMP DO SCHEDULE(DYNAMIC)

      do ind = (procid*nsim)/mpiprocs + 1, ((procid + 1)*nsim)/mpiprocs ! MPI loop

         ! Intialization for first year of your life
         sAssets = SimAssets0(ind)/RA(epsl)
         sDebt = SimDebt0(ind)
         sTheta = sigmai*SimRawNu(ind, 1)

         loop_age: do ij = 1, JWork ! loop through working age

            ! Exit if dead
            if (ij >= FirstIJDeath(ind)) exit loop_age

            ! State evolution
            if (ij > 1 .and. ij <= JWork) sTheta = rhotheta*sTheta + sigmanu*SimRawNu(ind, ij)
            sLiqW = sAssets*RA(sAssets)

            ! Optimization over consumption and labor choice
            state%ipcy = 1 ! fixed
            state%ed = SimE(ind)
            state%ij = ij
            state%liqw = sLiqW
            state%debt = sDebt
            state%theta = sTheta
            state%epsln = sigmaeps*SimRawEps(ind, ij)
            state%calvo = SimCalvo(ind, ij)
            if (ij .eq. 1) then
               state%Plabor = 0d0
            else
               state%Plabor = sLabor
            end if
            sV = OptimizeCL(state, sAssets, sConsumption, sLabor, sFlag, sSolType)

            ! Set labor supply based on pcyL or store it
            IF (PRESENT(pcyL)) THEN
               sLabor = SimLabor(ind, ij)
            ELSE
               SimLabor(ind, ij) = sLabor
            END IF

            ! Keep track of repayment and update debt for next period
            if (SimE(ind) > 1) then
               sRepay = DebtPayment(sLabor, state)
               sDebt = R_d*sDebt - sRepay
            else
               sRepay = 0d0
               sDebt = 0d0
            end if
            SimRepay(ind, ij) = real(sRepay, kind=sp)

            ! Keep track of taxes and transfers
            SimTT(ind, ij) = TaxesTransfers(ij, Policy(state%ipcy), HELPIncome(sLabor, state), sLiqW) &
                             + FloorTransfer(sLabor, state)

            ! Save value function
            IF (ij .eq. 1 .or. SIZE(SimV, 2) .eq. JWork) SimV(ind, ij) = sV

            ! Save age of repayment and income
            IF (RunMode .eq. 4) THEN
               if (AgeRepay(ind) .eq. 0) then
                  if (ij + 1 < FirstIJDeath(ind)) then
                     if (sDebt <= 0d0 .and. sRepay > 0d0) then
                        AgeRepay(ind) = ij
                     else if (ij .eq. JWork .and. sDebt > 0d0) then
                        AgeRepay(ind) = JWork + 1
                     end if
                  else
                     AgeRepay(ind) = JWork + 1
                  end if
               end if
            ELSE IF (RunMode .eq. 8 .and. SaveSimY) THEN
               SimY(ind, ij) = HELPIncome(sLabor, state)
            END IF

         end do loop_age

      end do

      !$OMP END DO
      !$OMP END PARALLEL

      ! Deallocate shocks
      CALL DeallocShocks

#if defined(MPI)
      if (mpiprocs > 1) then
         ! Double precision reals
         allocate (rbuffR(nsim))
         do ij = 1, SIZE(SimV, 2)
            CALL MPI_ALLREDUCE(SimV(:, ij), rbuffR, nsim, MPI_DOUBLE_PRECISION, MPI_MAX, MPI_COMM_WORLD, ierr)
            SimV(:, ij) = rbuffR
         end do
         do ij = 1, JWork
            CALL MPI_ALLREDUCE(SimTT(:, ij), rbuffR, nsim, MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD, ierr)
            SimTT(:, ij) = rbuffR
         end do
         IF (.NOT. PRESENT(pcyL)) THEN
            do ij = 1, JWork
               CALL MPI_ALLREDUCE(SimLabor(:, ij), rbuffR, nsim, MPI_DOUBLE_PRECISION, MPI_MAX, MPI_COMM_WORLD, ierr)
               SimLabor(:, ij) = rbuffR
            end do
         END IF
         IF (RunMode .eq. 8 .and. SaveSimY) THEN
            do ij = 1, JWork
               CALL MPI_ALLREDUCE(SimY(:, ij), rbuffR, nsim, MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD, ierr)
               SimY(:, ij) = rbuffR
            end do
         END IF
         deallocate (rbuffR)
         ! Single precision reals
         allocate (rbuffR_sp(nsim))
         do ij = 1, JWork
            CALL MPI_ALLREDUCE(SimRepay(:, ij), rbuffR_sp, nsim, MPI_REAL, MPI_SUM, MPI_COMM_WORLD, ierr)
            SimRepay(:, ij) = rbuffR_sp
         end do
         deallocate (rbuffR_sp)
         ! Integers
         allocate (rbuffI(nsim))
         CALL MPI_ALLREDUCE(errors, rbuffI, nsim, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierr)
         errors = rbuffI
         IF (RunMode .eq. 4) THEN
            CALL MPI_ALLREDUCE(AgeRepay, rbuffI, nsim, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierr)
            AgeRepay = rbuffI
         END IF
         deallocate (rbuffI)
      end if
#endif

      IF (procid .eq. 0) THEN
         ! Output labor supply
         IF (.NOT. PRESENT(pcyL)) THEN
            OPEN (UNIT=10, FORM='unformatted', FILE=trim(OutputDir)//'SimLabor4_'//trim(policy_str))
            WRITE (10) SimLabor
            CLOSE (UNIT=10)
         END IF
         ! Output age of repayment
         IF (RunMode .eq. 4) THEN
            OPEN (UNIT=10, FORM='unformatted', FILE=trim(OutputDir)//'AgeRepay')
            WRITE (10) AgeRepay
            CLOSE (UNIT=10)
         END IF
         ! Output SimY
         IF (RunMode .eq. 8 .and. SaveSimY) THEN
            SimY = SimY*numeraire
            WRITE (filename, '(A,A,I0,A)') trim(OutputDir), 'SimY_', OptimalType, '.txt'
            CALL WriteMatrix2d_real(filename, 31, 0, nsim, JWork, SimY)
         END IF
      END IF
      DEALLOCATE (SimLabor)
      IF (RunMode .eq. 4) DEALLOCATE (AgeRepay)
      IF (RunMode .eq. 8 .and. SaveSimY) DEALLOCATE (SimY)

      ! Count errors
      SMMError = sum(errors)
      deallocate (errors)

      ! Calculate three value functions in period zero and save value function
      avgV0(1) = CEF(SimV(:, 1))
      avgV0(2) = CEF(SimV(:realind_E2 - 1, 1))
      avgV0(3) = CEF(SimV(realind_E2:, 1))
      IF (procid .eq. 0 .and. SIZE(SimV, 2) .eq. JWork) THEN
         OPEN (UNIT=10, FORM='unformatted', FILE=trim(OutputDir)//'SimV')
         WRITE (10) SimV
         CLOSE (UNIT=10)
      END IF
      deallocate (SimV)

#if defined(MPI)
      CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)
#endif

      ! Print time of simulation
      call SYSTEM_CLOCK(t2, rate)
      if (procid .eq. 0) print *, 'LCSimulation_ReadL done. Seconds spent:', real(t2 - t1)/real(rate)

   END SUBROUTINE LCSimulation_ReadL

   !---------------------------------------------------------------------------------------------------------!
   ! Function to save individual-level panel dataset for simulation
   !---------------------------------------------------------------------------------------------------------!

   SUBROUTINE SaveLCSimulationPanel(iter, switch_sim)
      IMPLICIT NONE
      integer, intent(in) :: iter, switch_sim
      character(len=15) :: fileid
      integer :: ind, ij, SimPolicy
      integer :: JSave = JWork
      real(dp) :: switchIJ, mr_size

      ! Read in files
      CALL AllocSimFiles(JTot)
      CALL ReadSimFiles
      ! Interval for calculating marginal repayment rate
      mr_size = 100d0/numeraire
      ! Allocate arrays
      allocate (SimPanel(nsim*JSave, ncol_SimPanel))
      ! Fill in columns
      do ij = 1, JSave
         do ind = 1, nsim
            SELECT CASE (switch_sim)
            CASE DEFAULT
               SimPolicy = Policy(SimPolicyI(ind))
               switchIJ = 0d0
            CASE (1)
               if (SimSince(ind, ij) < 0) then
                  SimPolicy = Policy(1)
               else
                  SimPolicy = Policy(2)
               end if
               switchIJ = real(SimHELPSwitchIJ(ind), kind=dp)
            END SELECT
            SimPanel(ind*JSave - JSave + ij, 1) = real(ind, kind=dp)
            SimPanel(ind*JSave - JSave + ij, 2) = real(SimE(ind), kind=dp)
            SimPanel(ind*JSave - JSave + ij, 3) = switchIJ
            SimPanel(ind*JSave - JSave + ij, 4) = gd_age(ij)
            SimPanel(ind*JSave - JSave + ij, 5) = real(SimPolicy, kind=dp)
            SimPanel(ind*JSave - JSave + ij, 6) = real(SimSince(ind, ij), kind=dp)
            if (abs(SimV(ind, ij)) <= maxabsV) then
               SimPanel(ind*JSave - JSave + ij, 7) = SimV(ind, ij)
            else
               SimPanel(ind*JSave - JSave + ij, 7) = -99d0
            end if
            SimPanel(ind*JSave - JSave + ij, 8) = real(SimRepay(ind, ij), kind=dp)/MAX(SimY(ind, ij), 1d-5)
            SimPanel(ind*JSave - JSave + ij, 9) = SimY(ind, ij)
            SimPanel(ind*JSave - JSave + ij, 10) = SimW(ind, ij)
            SimPanel(ind*JSave - JSave + ij, 11) = SimTheta(ind, ij)
            SimPanel(ind*JSave - JSave + ij, 12) = SimEps(ind, ij)
            if (abs(SimConsumption(ind, ij)) <= maxabsV) then
               SimPanel(ind*JSave - JSave + ij, 13) = SimConsumption(ind, ij)
            else
               SimPanel(ind*JSave - JSave + ij, 13) = -99d0
            end if
            if (abs(SimLaborS(ind, ij)) <= real(maxabsV, kind=sp)) then
               SimPanel(ind*JSave - JSave + ij, 14) = real(SimLaborS(ind, ij), kind=dp)
            else
               SimPanel(ind*JSave - JSave + ij, 14) = -99d0
            end if
            if (abs(SimLiqWealth(ind, ij)) <= maxabsV) then
               SimPanel(ind*JSave - JSave + ij, 15) = SimLiqWealth(ind, ij)
            else
               SimPanel(ind*JSave - JSave + ij, 15) = -99d0
            end if
            SimPanel(ind*JSave - JSave + ij, 16) = real(SimDebt(ind, ij), kind=dp)
            SimPanel(ind*JSave - JSave + ij, 17) = real(SimRepay(ind, ij), kind=dp)
            SimPanel(ind*JSave - JSave + ij, 18) = SimTT(ind, ij)
            SimPanel(ind*JSave - JSave + ij, 19) = SimFlag(ind, ij)
            SimPanel(ind*JSave - JSave + ij, 20) = SimSolType(ind, ij)
         end do
      end do
      ! Write to .txt with specific file name
      write (fileid, '(i0)') iter
      SELECT CASE (switch_sim)
      CASE DEFAULT
         CALL WriteMatrix2d_real(trim(OutputDir)//'LCSim0Panel'//trim(adjustl(fileid))//'.txt', 31, 0, &
                                 nsim*JSave, ncol_SimPanel, SimPanel)
      CASE (1)
         CALL WriteMatrix2d_real(trim(OutputDir)//'LCSim1Panel'//trim(adjustl(fileid))//'.txt', 31, 0, &
                                 nsim*JSave, ncol_SimPanel, SimPanel)
      END SELECT
      deallocate (SimPanel)
      ! Deallocate files
      CALL DeallocSimFiles

   END SUBROUTINE SaveLCSimulationPanel

   !---------------------------------------------------------------------------------------------------------!
   ! Clean after simulation, excluding variables which are written out
   !---------------------------------------------------------------------------------------------------------!

   SUBROUTINE CleanSimulation
      if (SaveLCPanel > 0) deallocate (SimV, SimLiqWealth, SimAssets, SimConsumption, &
                                       SimTT, SimW, SimTheta, SimEps, SimSince, SimFlag, SimSolType)
   END SUBROUTINE CleanSimulation

END MODULE Simulations
