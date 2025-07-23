program Main_ComparePolicies

   ! Load modules
#if defined(MPI)
   USE mpi
#endif
!$ USE omp_lib
   USE Parameters, only: procid, mpiprocs, dp, NbrThreads, OutputDir, RunMode, &
                         FixLabor, Policy, NbrPolicy, DebtPayoff, R_d, R, nsim, &
                         nsim_dp, JWork, JTot, numeraire, c_gain, SMMError, avgV0, &
                         SimDebt0, SimRepay, SimTT, FirstIJDeath, AgeRepay
   USE Setup, only: Preliminaries, SetBorrowing, CreateGrids, SimulateShocks, &
                    CreateWageGrids
   USE ValueFunction_mod, only: ValueFunction, CleanSolution
   USE Simulations, only: LCSimulation_ReadL, SimulateFirst
   USE SMMFunctions, only: SetParamsSMM

   implicit none

   ! Parameters for calculations
   integer, parameter :: pcy_base = -3 ! base policy for labor supply

   ! Program variables
   integer :: it
   character(len=40) :: filename

   ! Variables for consumption equivalent welfare gains
   real(dp), parameter :: maxabs_gain = 5d-2 ! maximum absolute value
   integer, parameter :: n_gains = 30 ! number of points between 0 and maxabs_gain in each direction
   integer :: ig ! loop index

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

   ! Set display and mode
   RunMode = 4

   ! Set fixed model parameters
   FixLabor = 0
   allocate (Policy(1))
   NbrPolicy = 1

   ! Zero out consumption equivalent welfare gain
   c_gain = 0d0

   ! Initializations
   CALL Preliminaries
   CALL SimulateShocks
   CALL SetBorrowing(1)

   ! Set structural parameters
   CALL SetParamsSMM

   ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ! COMPARE POLICIES
   ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   ! Evaluate different policies
   WRITE (filename, '(A,A,I0,A)') trim(OutputDir), 'ComparePolicies.txt'
   IF (procid .eq. 0) OPEN (331, FILE=filename, STATUS='replace')
   CALL EvaluatePolicy(pcy_base, 1d0, 1)
   CALL EvaluatePolicy(-2, 1d0, 1)
   CALL EvaluatePolicy(-4, 1d0, 1)
   CALL EvaluatePolicy(-5, 1d0, 1)
   CALL EvaluatePolicy(-101, 1d0, 1)
   CALL EvaluatePolicy(-102, 1d0, 1)
   CALL EvaluatePolicy(-103, 1d0, 1)
   CALL EvaluatePolicy(-104, 1d0, 1)
   CALL EvaluatePolicy(-201, 1d0, 1)
   CALL EvaluatePolicy(-202, 1d0, 1)
   CALL EvaluatePolicy(-402, 1d0, 1)
   CALL EvaluatePolicy(-403, 1d0, 1)
   CALL EvaluatePolicy(-6, 1d0, 1)
   CALL EvaluatePolicy(0, 1d0, 1)
   CALL EvaluatePolicy(-101, 1d0, 0)
   IF (procid .eq. 0) CLOSE (331)

   ! Grid search over cash transfers at base policy
   IF (procid .eq. 0) OPEN (332, FILE=trim(OutputDir)//'CashTransfers_CP.txt', STATUS='replace')
   Policy(1) = pcy_base
   R_d = 1d0
   DebtPayoff = 1
   CALL CreateGrids
   CALL ValueFunction
   DO it = -8000, 20000, 250
      CALL EvaluateTransfer(real(it, kind=dp)/numeraire, 332)
   END DO
   CALL CleanSolution
   IF (procid .eq. 0) CLOSE (332)

   ! Grid-search over consumption-equivalent welfare gains at base policy and reset c_gain
   IF (procid .eq. 0) OPEN (334, FILE=trim(OutputDir)//'ConsumptionEquivalent_CP.txt', STATUS='replace')
   DO ig = -n_gains, n_gains
      CALL Eval_cgain(real(ig, kind=dp)/real(n_gains, kind=dp)*maxabs_gain, 334)
   END DO
   c_gain = 0d0
   IF (procid .eq. 0) CLOSE (334)

#if defined(MPI)
   IF (procid == 0) print *, 'Ended MPI'
   CALL MPI_FINALIZE(ierr)
#endif

CONTAINS

   !---------------------------------------------------------------------------------------------------------!
   ! Routine to evaluate a particular policy and output its results to ComparePolicies.txt
   !---------------------------------------------------------------------------------------------------------!
   SUBROUTINE EvaluatePolicy(pcy, int, payoff)

      IMPLICIT NONE

      ! Function arguments
      integer, intent(in) :: pcy ! policy
      real(dp), intent(in) :: int ! interest rate
      integer, intent(in) :: payoff ! whether debt paid off

      ! Storage
      real(dp) :: R_g(JTot) ! discount rate for NPV of government BC
      real(dp) :: output(13)
      character(len=15) :: lstring1
      integer :: ind, ij

      ! Set policy
      Policy(1) = pcy
      R_d = int
      DebtPayoff = payoff

      ! Solve and simulate from model
      CALL CreateGrids
      CALL CreateWageGrids
      CALL ValueFunction
      CALL LCSimulation_ReadL

      ! Set discount rate for government NPV calculation
      DO ij = 1, JTot
         R_g(ij) = R**real(ij - 1, kind=dp)
      END DO

      ! Calculate results of interest
      IF (DebtPayoff .eq. 0) THEN
         output(1) = pcy*10
      ELSE
         output(1) = pcy
      END IF
      output(2) = int
      output(3) = payoff
      if (SMMError > 0) then
         output(4:) = -99d0
         print *, 'SMM Error with policy:', pcy
         DEALLOCATE (SimRepay, SimTT)
      else
         ! Welfare and budget of policy
         output(4) = avgV0(1)
         output(5) = SUM(SimDebt0)
         output(6) = real(SUM(SimRepay), kind=dp)
         output(7) = 0d0
         output(8) = -SUM(SimTT)
         output(9) = 0d0
         do ij = 1, JWork
            do ind = 1, nsim
               IF (ij >= FirstIJDeath(ind)) CYCLE
               output(7) = output(7) + real(SimRepay(ind, ij), kind=dp)/R_g(ij)
               output(9) = output(9) - SimTT(ind, ij)/R_g(ij)
            end do
         end do
         output(5:9) = output(5:9)*numeraire/nsim_dp
         DEALLOCATE (SimRepay, SimTT)
         ! Age at which individuals repay
         ALLOCATE (AgeRepay(nsim))
         OPEN (UNIT=10, FORM='unformatted', FILE=trim(OutputDir)//'AgeRepay')
         READ (10) AgeRepay
         CLOSE (UNIT=10)
         output(10:11) = 0d0
         do ind = 1, nsim
            IF (AgeRepay(ind) .eq. JWork + 1) THEN
               output(11) = output(11) + 1d0
            ELSE
               output(10) = output(10) + real(AgeRepay(ind), kind=dp)
            END IF
         end do
         output(10) = output(10)/MAX(nsim_dp - output(11), 0.1d0)
         output(11) = output(11)/nsim_dp
         DEALLOCATE (AgeRepay)
         ! Budget of policy assuming labor supply stays fixed at pcy_base
         output(12:13) = 0d0
         CALL LCSimulation_ReadL(pcy_base)
         do ij = 1, JWork
            do ind = 1, nsim
               IF (ij >= FirstIJDeath(ind)) CYCLE
               output(12) = output(12) + real(SimRepay(ind, ij), kind=dp)/R_g(ij)
               output(13) = output(13) - SimTT(ind, ij)/R_g(ij)
            end do
         end do
         output(12:13) = output(12:13)*numeraire/nsim_dp
         DEALLOCATE (SimRepay, SimTT)
      end if

      ! Clean solution
      CALL CleanSolution

      ! Append outputs to files
      if (procid .eq. 0) then
         write (UNIT=lstring1, FMT='(I5)') size(output)
         write (331, '('//trim(lstring1)//'F30.15)') (output)
      end if

   END SUBROUTINE EvaluatePolicy

   !---------------------------------------------------------------------------------------------------------!
   ! Routine to calculate the welfare at t=0 of a particular cash transfer
   !---------------------------------------------------------------------------------------------------------!

   SUBROUTINE EvaluateTransfer(transfer, fileid)

      IMPLICIT NONE

      ! Function arguments
      real(dp), intent(in) :: transfer ! size of cash transfer
      integer, intent(in) :: fileid ! file id to write to

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
         write (fileid, '('//trim(lstring1)//'F24.15)') (output)
      end if

   END SUBROUTINE EvaluateTransfer

   !---------------------------------------------------------------------------------------------------------!
   ! Routine to calculate the welfare at t=0 of a particular consumption equivalent welfare gain
   !---------------------------------------------------------------------------------------------------------!

   SUBROUTINE Eval_cgain(gain, fileid)
      IMPLICIT NONE
      ! Function arguments
      real(dp), intent(in) :: gain ! size of consumption equivalent welfare gain
      integer, intent(in) :: fileid ! file id to write to
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
         write (fileid, '('//trim(lstring1)//'F24.15)') (output)
      end if
   END SUBROUTINE Eval_cgain

end program Main_ComparePolicies
