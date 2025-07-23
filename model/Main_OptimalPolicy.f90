program Main_OptimalPolicy

   ! Load modules
#if defined(MPI)
   USE mpi
#endif
!$ USE omp_lib
   USE Parameters, only: procid, mpiprocs, dp, sp, NbrThreads, positiveInf, &
                         OutputDir, RunMode, FixLabor, Policy, NbrPolicy, &
                         DebtPayoff, R_d, R, gamma, cprob, fcL, fcH, c_gain, nsim, &
                         nsim_dp, nsim_op, JTot, JWork, numeraire, SMMError, avgV0, &
                         SimDebt0, SimRepay, SimTT, FirstIJDeath, OptimalType, OptimalP, &
                         SaveSimY, SaveSimV, nbins_s0, bins_s0, avgV0_a, avgV0_aA, &
                         avgV0_ai, avgV0_aD, SimV, SimAssets0, SimRawNu, SimE
   USE Setup, only: Preliminaries, SetBorrowing, CreateGrids, SimulateShocks, &
                    CreateWageGrids, ReadShocks, DeallocShocks
   USE Procedures, only: rtbis, WriteMatrix1d_realsp, WriteMatrix2d_realsp, &
                         WriteMatrix2d_realdp, is_in_array
   USE ValueFunction_mod, only: ValueFunction, CleanSolution
   USE Simulations, only: LCSimulation_ReadL, SimulateFirst
   USE SMMFunctions, only: SetParamsSMM
   USE amoeba_mod, only: amoeba
   USE TikTak_mod, only: TikTak

   IMPLICIT NONE

   ! Parameters for code operation
   integer, parameter, dimension(10) :: Optimals = (/1, 2, 3, 4, 5, 6, 7, 8, 9, 10/) ! values of OptimalType to search over
   integer :: i_op, i_mh ! loop indices
   character(len=60) :: filename ! file name for output
   logical :: real_policy ! whether you're solving for a real policy or counterfactual
   integer :: ReadL ! control whether you are reading in or writing out labor

   ! Parameters for government budget
   integer, parameter :: basepcy = -3 ! baseline policy
   real(dp), dimension(JTot) :: R_g ! discount rate for NPV of government BC
   real(dp) :: FixedPoint ! value to target

   ! Parameters for constrained optimization routine
   real(dp), parameter :: tol_gbc = 1d0/numeraire ! absolute tolerance for GBC
   integer, parameter :: n_iter_pen = 10 ! number of iterations increasing penalty terms
   integer :: iter_pen ! loop index of maximization
   real(dp) :: pens(2) ! penalty parameters for all methods

   ! Parameters for TikTak optimization within constrained optimization
   integer, parameter :: N_global = 200 ! number of global iterations in TikTak
   integer, parameter :: N_local = 5 ! number of local iterations in TikTak
   real(dp), parameter :: tol_nm = 1d-5 ! tolerance for Nelder-Mead
   integer, parameter :: iter_nm = 100 ! maximum number of iterations for Nelder-Mead
   integer :: Ng_op ! number of global iterations within loop of i_op
   real(dp) :: perc_op ! percent of local iterations within loop of i_op
   real(dp) :: obj, res

   ! Variables for consumption equivalent welfare gains
   real(dp), parameter :: maxabs_gain = 5d-2 ! maximum absolute value
   integer, parameter :: n_gains = 30 ! number of points between 0 and maxabs_gain in each direction
   integer :: ig ! loop index

   ! Parameters to be allocated based on choice of repayment contract
   integer :: n_optimal ! number of optimal policy parameters
   real(dp), dimension(:), allocatable :: lb_op, ub_op ! lower and upper bounds on optimal policy parameters
   real(dp), dimension(:), allocatable :: step_op ! NM step size
   real(dp), dimension(:), allocatable :: theta0, theta1

#if defined(MPI)
   integer :: ierr
   CALL MPI_INIT(ierr)
   IF (ierr /= 0) STOP 'error with mpi_init'

   CALL MPI_COMM_SIZE(MPI_COMM_WORLD, mpiprocs, ierr)
   IF (ierr /= 0) STOP 'error with mpi_comm_size'

   CALL MPI_COMM_RANK(MPI_COMM_WORLD, procid, ierr)
   IF (ierr /= 0) STOP 'error with mpi_comm_rank'

   IF (procid == 0) THEN
      print *, '==================================================='
      print *, '====  Starting MPI with mpiprocs:', mpiprocs, ' ===='
      print *, '==================================================='
   END IF

#else
   procid = 0
   mpiprocs = 1
#endif
! Select number of threads for parallelization
!$ call GET_ENVIRONMENT_VARIABLE("OMP_NUM_THREADS", NbrThreads)
!$ if (procid == 0) print *, "Available threads: ", OMP_GET_MAX_THREADS(), "Threads in use: ", NbrThreads

   ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ! Setup
   ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   ! Check simulation parameters
   IF (nsim > nsim_op) STOP 'FATAL ERROR: nsim > nsim_op! Adjust simulation parameters'

   ! Set display and mode
   RunMode = 8

   ! Set fixed model parameters
   FixLabor = 0
   allocate (Policy(1))
   NbrPolicy = 1
   DebtPayoff = 1

   ! Set fixed budget parameters that are used in other files
   R_d = 1d0
   c_gain = 0d0

   ! Set structural parameters
   CALL SetParamsSMM

   ! Initializations
   CALL Preliminaries
   CALL SetBorrowing(1)
   CALL SimulateShocks
   CALL CreateWageGrids

   ! Compute discount rate, which is parameter dependent
   DO ig = 1, JTot
      R_g(ig) = R**real(ig - 1, kind=dp)
   END DO

   ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ! DO OPTIMAL POLICY FOR DIFFERENT CONTRACTS AND DIFFERENT MH SPECIFICATIONS
   ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   DO i_op = 1, size(Optimals) ! i_op

      ! Allocate contract parameters
      OptimalType = Optimals(i_op)
      CALL AllocateOptimalP

      DO i_mh = 0, 1 ! i_mh

         ! Method in which moral hazard is being computed
         IF (procid .eq. 0) PRINT *, 'OptimalType = ', OptimalType, 'Endogenous Labor = ', i_mh
         real_policy = (i_mh .eq. 1)

         ! Open output file
         IF (procid .eq. 0) THEN
            WRITE (filename, '(A,A,I0,A,I0,A)') trim(OutputDir), 'OptimalPolicy', OptimalType, '_', i_mh, '.txt'
            OPEN (331, FILE=filename, STATUS='replace')
         END IF

         ! Reset interest rate
         R_d = 1d0

         ! Reset turning off saving SimY and SimV
         SaveSimY = .FALSE.
         SaveSimV = .FALSE.

         ! Set baseline policy
         Policy(1) = basepcy

         ! Solve and simulate model at baseline policy and set parameters for fixed labor case
         ReadL = 0
         IF (i_mh .eq. 0) THEN
            CALL SolveSimulate
            DEALLOCATE (SimRepay, SimTT)
            ReadL = 1
            cprob = 0d0
            fcL = 0d0
            fcH = 0d0
            FixLabor = 1
         ELSE
            CALL SetParamsSMM
            FixLabor = 0
         END IF

         ! Calculate fixed point at baseline policy
         FixedPoint = 0d0
         FixedPoint = GBC(lb_op)
         IF (procid .eq. 0) PRINT *, "FIXED POINT:", FixedPoint

         ! Grid-search over consumption-equivalent welfare gains at base policy and reset c_gain
         IF (i_op .eq. 1) THEN
            IF (procid .eq. 0) THEN
               WRITE (filename, '(A,A,I0,A)') trim(OutputDir), 'ConsumptionEquivalent_', i_mh, '.txt'
               OPEN (332, FILE=filename, STATUS='replace')
            END IF
            DO ig = -n_gains, n_gains
               CALL Eval_cgain(real(ig, kind=dp)/real(n_gains, kind=dp)*maxabs_gain)
            END DO
            c_gain = 0d0
            IF (procid .eq. 0) CLOSE (332)
         END IF

         ! Grid search over cash transfers at base policy
         IF (i_op .eq. 1) THEN
            IF (procid .eq. 0) THEN
               WRITE (filename, '(A,A,I0,A)') trim(OutputDir), 'CashTransfers_', i_mh, '.txt'
               OPEN (334, FILE=filename, STATUS='replace')
            END IF
            CALL CreateGrids
            CALL ValueFunction
            DO ig = -8000, 20000, 250
               CALL EvaluateTransfer(real(ig, kind=dp)/numeraire)
            END DO
            CALL CleanSolution
            IF (procid .eq. 0) CLOSE (334)
         END IF

         ! Value function heterogeneity at base policy
         IF (i_op .eq. 1 .and. real_policy) THEN
            SaveSimV = .TRUE.
            CALL OutputVHeterogeneity
            SaveSimV = .FALSE.
         END IF

         ! Switch to optimal policy
         Policy(1) = -1

         ! Set penalty parameters
         pens = 99999d0

         ! Set TikTak parameters, using weaker parameters for certain contracts
         SELECT CASE (OptimalType)
         CASE DEFAULT
            Ng_op = N_global
            perc_op = real(N_local, kind=dp)/real(Ng_op, kind=dp)
         CASE (1)
            iter_pen = 0
            theta0 = 0d0
            obj = Objective(theta0)
            CYCLE
         CASE (9)
            Ng_op = 10
            perc_op = 1d0/real(Ng_op, kind=dp)
         CASE (4, 6, 10)
            Ng_op = 20
            perc_op = 1d0/real(Ng_op, kind=dp)
         END SELECT

         ! Iterative procedure to find constrained optimal policy
         DO iter_pen = 1, n_iter_pen
            SaveSimY = .FALSE.
            IF (iter_pen .eq. 1) THEN
               CALL TikTak(0, Objective, n_optimal, lb_op, ub_op, step_op, &
                           Ng_op, perc_op, tol_nm, iter_nm, .FALSE., theta1, obj, .TRUE.)
            ELSE
               CALL TikTak(0, Objective, n_optimal, theta0 - step_op, theta0 + step_op, step_op, &
                           Ng_op, perc_op, tol_nm, iter_nm, .FALSE., theta1, obj, .TRUE.)
            END IF
            IF (procid .eq. 0) PRINT *, "FINISHED iter_pen ITERATION", iter_pen, "OF", n_iter_pen
            theta0 = theta1
            res = GBC(theta0)
            IF (ABS(res) > tol_gbc) THEN
               IF (procid .eq. 0) THEN
                  PRINT *, " UNSUCCESSFUL CONSTRAINT RESIDUAL:", res
                  PRINT *, "  CURRENT THETA:", theta0
               END IF
               IF (real_policy) SaveSimY = .TRUE.
               obj = Objective(theta0)
               EXIT
            ELSE
               IF (procid .eq. 0) PRINT *, " SUCCESSFUL CONSTRAINT RESIDUAL:", res
               IF (real_policy) SaveSimY = .TRUE.
               obj = Objective(theta0)
               EXIT
            END IF
         END DO
         IF (procid .eq. 0) THEN
            PRINT *, "FINISHED CONSTRAINED OPTIMIZATION"
            PRINT *, "  OPTIMAL:", theta0
         END IF

         ! If labor supply was held fixed, evaluate objective function with endogenous labor supply at optimal contract
         IF (.not. real_policy) THEN
            CALL SetParamsSMM
            FixLabor = 0
            obj = Objective(theta0)
         END IF

         ! Output value function heterogeneity
         IF (real_policy) THEN
            SaveSimV = .TRUE.
            CALL UnpackOptimalP(theta0)
            CALL OutputVHeterogeneity
            SaveSimV = .FALSE.
         END IF

         ! Close file
         IF (procid .eq. 0) CLOSE (331)

      END DO ! i_mh

      ! Deallocate contract parameters
      CALL DeallocateOptimalP

   END DO ! i_op

#if defined(MPI)
   IF (procid == 0) print *, 'Ended MPI'
   CALL MPI_FINALIZE(ierr)
#endif

CONTAINS

   !---------------------------------------------------------------------------------------------------------!
   ! ROUTINES TO ALLOCATE AND DEALLOCATE PARAMETERS BASED ON CHOICE OF OPTIMAL TYPE
   !---------------------------------------------------------------------------------------------------------!

   SUBROUTINE AllocateOptimalP
      IMPLICIT NONE
      ! Parameters that vary across contracts
      SELECT CASE (OptimalType)
      CASE DEFAULT
         STOP 'INVALID SPECIFICATION OF OptimalType!'
      CASE (1) ! Debt Cancellation = Upper Bound
         n_optimal = 1
         ALLOCATE (OptimalP(n_optimal), lb_op(n_optimal), ub_op(n_optimal))
         lb_op = (/0d0/)
         ub_op = (/0d0/)
      CASE (2, 3) ! Standard IBR with and without 20-year forgiveness
         n_optimal = 2
         ALLOCATE (OptimalP(n_optimal), lb_op(n_optimal), ub_op(n_optimal))
         lb_op = (/0.01d0, 0d0/)
         ub_op = (/0.7d0, 2d0/)
      CASE (4) ! 9-Year ISA with no cap or threshold
         n_optimal = 1
         ALLOCATE (OptimalP(n_optimal), lb_op(n_optimal), ub_op(n_optimal))
         lb_op = (/0.03d0/)
         ub_op = (/0.06d0/)
      CASE (5) ! 9-Year ISA with threshold and no cap
         n_optimal = 2
         ALLOCATE (OptimalP(n_optimal), lb_op(n_optimal), ub_op(n_optimal))
         lb_op = (/0d0, 0d0/)
         ub_op = (/0.4d0, 2d0/)
      CASE (6) ! Lifetime ISA with no cap or threshold
         n_optimal = 1
         ALLOCATE (OptimalP(n_optimal), lb_op(n_optimal), ub_op(n_optimal))
         lb_op = (/0d0/)
         ub_op = (/0.03d0/)
      CASE (7) ! Lifetime ISA with threshold and no cap
         n_optimal = 2
         ALLOCATE (OptimalP(n_optimal), lb_op(n_optimal), ub_op(n_optimal))
         lb_op = (/0d0, 0d0/)
         ub_op = (/0.4d0, 2d0/)
      CASE (8) ! IBR without forgiveness and discontinuity in *average* rate
         n_optimal = 2
         ALLOCATE (OptimalP(n_optimal), lb_op(n_optimal), ub_op(n_optimal))
         lb_op = (/0d0, 0d0/)
         ub_op = (/0.4d0, 2d0/)
      CASE (9) ! 25-Year Fixed repayment contract + forbearance at Smooth_UI phase-out point
         n_optimal = 1
         ALLOCATE (OptimalP(n_optimal), lb_op(n_optimal), ub_op(n_optimal))
         lb_op = (/0.99d0/)
         ub_op = (/1.05d0/)
      CASE (10) ! Constant payment + forbearance at Smooth_UI phase-out point
         n_optimal = 1
         ALLOCATE (OptimalP(n_optimal), lb_op(n_optimal), ub_op(n_optimal))
         lb_op = (/0d0/)
         ub_op = (/0.1d0/)
      END SELECT

      ! Parameters that don't vary across contracts
      ALLOCATE (step_op, theta0, theta1, mold=OptimalP)
      step_op = 0.25d0*(ub_op - lb_op)

   END SUBROUTINE AllocateOptimalP

   SUBROUTINE DeallocateOptimalP
      IMPLICIT NONE
      DEALLOCATE (OptimalP, lb_op, ub_op, step_op, theta0, theta1)
   END SUBROUTINE DeallocateOptimalP

   !---------------------------------------------------------------------------------------------------------!
   ! Routine to unpack OptimalP - NEED TO MANUALLY MODIFY FOR Objective_FP and GBC_FP
   !---------------------------------------------------------------------------------------------------------!

   SUBROUTINE UnpackOptimalP(theta)
      IMPLICIT NONE
      REAL(dp), INTENT(IN) :: theta(n_optimal)
      OptimalP = theta
      IF (OptimalType .eq. 9 .and. Policy(1) .eq. -1) R_d = OptimalP(1)
   END SUBROUTINE UnpackOptimalP

   !---------------------------------------------------------------------------------------------------------!
   ! Routine to solve and simulate from model collecting results of interest
   !---------------------------------------------------------------------------------------------------------!

   SUBROUTINE SolveSimulate
      IMPLICIT NONE
      ! Solve and simulate from model
      CALL CreateGrids
      CALL CreateWageGrids
      CALL ValueFunction
      SELECT CASE (ReadL)
      CASE DEFAULT
         CALL LCSimulation_ReadL
      CASE (1)
         CALL LCSimulation_ReadL(basepcy)
      END SELECT
      CALL CleanSolution
   END SUBROUTINE SolveSimulate

   !---------------------------------------------------------------------------------------------------------!
   ! Functions to PDV of government budget per individual relative to fixed point
   !---------------------------------------------------------------------------------------------------------!

   REAL(dp) FUNCTION CalcGBC()
      IMPLICIT NONE
      INTEGER :: ind, ij
      ! Calculate PDV of government budget per individual
      CalcGBC = -SUM(SimDebt0)
      DO ij = 1, JWork
         DO ind = 1, nsim
            IF (ij >= FirstIJDeath(ind)) CYCLE
            CalcGBC = CalcGBC + (real(SimRepay(ind, ij), kind=dp) - SimTT(ind, ij))/R_g(ij)
         END DO
      END DO
      CalcGBC = CalcGBC/nsim_dp
      ! Subtract value you want to find fixed point relative to
      CalcGBC = CalcGBC - FixedPoint
      DEALLOCATE (SimRepay, SimTT)
   END FUNCTION CalcGBC

   REAL(dp) FUNCTION GBC(theta)
      IMPLICIT NONE
      REAL(dp), INTENT(IN) :: theta(n_optimal)
      INTEGER :: ip
      ! Exit if constraint not satisfied
      DO ip = 1, n_optimal
         IF (theta(ip) > ub_op(ip) .or. theta(ip) < lb_op(ip)) THEN
            GBC = positiveInf
            RETURN
         END IF
      END DO
      ! Set policy parameters
      CALL UnpackOptimalP(theta)
      ! Solve and simulate
      CALL SolveSimulate
      ! Calculate budget
      GBC = CalcGBC()
   END FUNCTION GBC

   !---------------------------------------------------------------------------------------------------------!
   ! Unconstrained objective function to maximize, which embeds constraint penalty function
   !---------------------------------------------------------------------------------------------------------!

   REAL(dp) FUNCTION Objective(theta)
      IMPLICIT NONE
      REAL(dp), INTENT(IN) :: theta(n_optimal)
      INTEGER :: ip
      REAL(dp) :: output(n_optimal + 5), budget, penterm
      character(len=15) :: lstring1
      ! Exit if constraint not satisfied
      DO ip = 1, n_optimal
         IF (theta(ip) > ub_op(ip) .or. theta(ip) < lb_op(ip)) THEN
            Objective = positiveInf
            RETURN
         END IF
      END DO
      ! Set policy parameters
      CALL UnpackOptimalP(theta)
      ! Solve and simulate
      CALL SolveSimulate
      ! Calculate objective to minimize
      if (SMMError > 0) then
         budget = 999999d0
         penterm = budget
         avgV0 = -budget
         Objective = budget
         DEALLOCATE (SimTT, SimRepay)
      else
         budget = CalcGBC()
         penterm = Penalty(budget)
         Objective = -avgV0(1) + penterm
      end if
      ! Format output and write out
      output(1:n_optimal) = theta
      output(n_optimal + 1) = -avgV0(1)
      output(n_optimal + 2) = budget
      output(n_optimal + 3) = penterm
      output(n_optimal + 4) = Objective
      output(n_optimal + 5) = real(iter_pen, kind=dp)
      if (procid .eq. 0) then
         write (UNIT=lstring1, FMT='(I5)') size(output)
         write (331, '('//trim(lstring1)//'F24.15)') (output)
      end if
   END FUNCTION Objective

   !---------------------------------------------------------------------------------------------------------!
   ! Penalty function
   !---------------------------------------------------------------------------------------------------------!
   REAL(dp) FUNCTION Penalty(residual)
      IMPLICIT NONE
      REAL(dp), INTENT(IN) :: residual
      IF (residual >= -tol_gbc) THEN
         Penalty = 0d0
      ELSE
         Penalty = pens(1)
      END IF
   END FUNCTION Penalty

   !---------------------------------------------------------------------------------------------------------!
   ! Routine to calculate the welfare at t=0 of a particular cash transfer
   !---------------------------------------------------------------------------------------------------------!

   SUBROUTINE EvaluateTransfer(transfer)
      IMPLICIT NONE
      ! Function arguments
      real(dp), intent(in) :: transfer ! size of cash transfer
      ! Storage
      real(dp) :: meanV0(3), output(2)
      integer :: sum_errors
      character(len=15) :: lstring1
      ! Simulate first period
      CALL SimulateFirst(transfer, meanV0, sum_errors)
      ! Format output
      output(1) = transfer*numeraire
      output(2) = meanV0(1)
      if (sum_errors > 0) output(2) = -99d0
      ! Append outputs to files
      if (procid .eq. 0) then
         write (UNIT=lstring1, FMT='(I5)') size(output)
         write (334, '('//trim(lstring1)//'F24.15)') (output)
      end if
   END SUBROUTINE EvaluateTransfer

   !---------------------------------------------------------------------------------------------------------!
   ! Routine to calculate the welfare at t=0 of a particular consumption equivalent welfare gain
   !---------------------------------------------------------------------------------------------------------!

   SUBROUTINE Eval_cgain(gain)
      IMPLICIT NONE
      ! Function arguments
      real(dp), intent(in) :: gain ! size of consumption equivalent welfare gain
      ! Storage
      real(dp) :: meanV0(3), output(2)
      integer :: sum_errors
      character(len=15) :: lstring1
      ! Set consumption equivalent welfare gain
      c_gain = gain
      ! Solve and simulate from model
      CALL CreateGrids
      CALL ValueFunction
      CALL SimulateFirst(0d0, meanV0, sum_errors)
      CALL CleanSolution
      ! Format output
      output(1) = gain
      output(2) = meanV0(1)
      if (sum_errors > 0) output(2) = -99d0
      ! Append outputs to files
      if (procid .eq. 0) then
         write (UNIT=lstring1, FMT='(I5)') size(output)
         write (332, '('//trim(lstring1)//'F24.15)') (output)
      end if
   END SUBROUTINE Eval_cgain

   !---------------------------------------------------------------------------------------------------------!
   ! Routines to calculate average value functions by heterogeneous initial states
   !---------------------------------------------------------------------------------------------------------!

   SUBROUTINE OutputVHeterogeneity
      IMPLICIT NONE
      CHARACTER(len=5) :: policy_str
      INTEGER :: i2, i3
      REAL(sp), ALLOCATABLE, DIMENSION(:, :) :: dfout
      ! Solve and simulate
      CALL SolveSimulate
      DEALLOCATE (SimRepay, SimTT)
      ! Read in value function and shocks
      ALLOCATE (SimV(nsim, JWork))
      OPEN (UNIT=10, FORM='unformatted', FILE=trim(OutputDir)//'SimV')
      READ (10) SimV
      CLOSE (UNIT=10)
      CALL ReadShocks
      ! Initialize result storage
      ALLOCATE (avgV0_a(JWork, 1), avgV0_aA(JWork, nbins_s0, 1))
      ALLOCATE (avgV0_ai, avgV0_aD, mold=avgV0_aA)
      ! Calculate heterogeneity across initial states for SimE=2
      DO i2 = 1, JWork
         avgV0_a(i2, 1) = CEF_s(2, i2, -1, -1, -1)
         DO i3 = 1, nbins_s0
            avgV0_aA(i2, i3, 1) = CEF_s(2, i2, i3, -1, -1)
         END DO
         DO i3 = 1, nbins_s0
            avgV0_ai(i2, i3, 1) = CEF_s(2, i2, -1, i3, -1)
         END DO
         DO i3 = 1, nbins_s0
            avgV0_aD(i2, i3, 1) = CEF_s(2, i2, -1, -1, i3)
         END DO
      END DO
      ! Make matrix of initial value functions and states (in single precision)
      ALLOCATE (dfout(nsim, 4))
      DO i2 = 1, nsim
         dfout(i2, 1) = real(SimV(i2, 1), kind=sp)
         dfout(i2, 2) = real(SimAssets0(i2), kind=sp)
         dfout(i2, 3) = real(SimRawNu(i2, 1), kind=sp)
         dfout(i2, 4) = real(SimDebt0(i2), kind=sp)
      END DO
      ! Deallocate
      DEALLOCATE (SimV)
      CALL DeallocShocks
      ! Output value functions and dfout to files
      IF (procid .eq. 0) THEN
         IF (Policy(1) .eq. -1) THEN
            WRITE (policy_str, '(I0)') OptimalType
            CALL WriteMatrix1d_realsp(trim(OutputDir)//'initialV'//trim(policy_str)//'.txt', 31, 0, &
                                      nsim, dfout(:, 1))
         ELSE
            policy_str = 'base'
            CALL WriteMatrix2d_realsp(trim(OutputDir)//'initialV'//trim(policy_str)//'.txt', 31, 0, &
                                      nsim, 4, dfout)
         END IF
         CALL WriteMatrix2d_realdp(trim(OutputDir)//'avgV0_a'//trim(policy_str)//'.txt', 31, 0, &
                                   SIZE(avgV0_a, 1), SIZE(avgV0_a, 2), avgV0_a)
         CALL WriteMatrix2d_realdp(trim(OutputDir)//'avgV0_aA'//trim(policy_str)//'.txt', 31, 0, &
                                   SIZE(avgV0_aA, 1), SIZE(avgV0_aA, 2), avgV0_aA(:, :, 1))
         CALL WriteMatrix2d_realdp(trim(OutputDir)//'avgV0_ai'//trim(policy_str)//'.txt', 31, 0, &
                                   SIZE(avgV0_ai, 1), SIZE(avgV0_ai, 2), avgV0_ai(:, :, 1))
         CALL WriteMatrix2d_realdp(trim(OutputDir)//'avgV0_aD'//trim(policy_str)//'.txt', 31, 0, &
                                   SIZE(avgV0_aD, 1), SIZE(avgV0_aD, 2), avgV0_aD(:, :, 1))
      END IF
      DEALLOCATE (avgV0_a, avgV0_aA, avgV0_ai, avgV0_aD, dfout)
#if defined(MPI)
      CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)
#endif
   END SUBROUTINE OutputVHeterogeneity

! Function to calculate CEF of value functions at a given state s
   REAL(dp) FUNCTION CEF_s(iE, ij, AQ, iQ, DQ)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: iE ! education
      INTEGER, INTENT(IN) :: ij ! age index
      INTEGER, INTENT(IN) :: AQ, iQ, DQ ! quantiles of Assets0, delta_i, and Debt0
      INTEGER :: AQ_, iQ_, DQ_, ind
      REAL(dp) :: Nmean, gamma1, Vt
      ! Initializations
      gamma1 = 1d0 - gamma
      CEF_s = 0d0
      Nmean = 0d0
      AQ_ = MAX(AQ, 1)
      iQ_ = MAX(iQ, 1)
      DQ_ = MAX(DQ, 1)
      ! Loop over individuals
      DO ind = 1, nsim
         ! If condition to select states
         IF ((SimE(ind) .eq. iE .or. iE .eq. -1) .and. &
             ((SimAssets0(ind) >= bins_s0(1, AQ_) .and. SimAssets0(ind) < bins_s0(1, AQ_ + 1)) .or. AQ .eq. -1) .and. &
             ((SimRawNu(ind, 1) >= bins_s0(2, iQ_) .and. SimRawNu(ind, 1) < bins_s0(1, iQ_ + 1)) .or. iQ .eq. -1) .and. &
             ((SimDebt0(ind) >= bins_s0(3, DQ_) .and. SimDebt0(ind) < bins_s0(3, DQ_ + 1)) .or. DQ .eq. -1) &
             ) THEN
            IF (ij >= FirstIJDeath(ind)) CYCLE
            Vt = SimV(ind, ij)
            Vt = Vt**gamma1
            CEF_s = CEF_s + Vt
            Nmean = Nmean + 1d0
         END IF
      END DO
      IF (Nmean > 0d0) THEN
         CEF_s = CEF_s/Nmean
         CEF_s = CEF_s**(1d0/gamma1)
      END IF
   END FUNCTION CEF_s

end program Main_OptimalPolicy
