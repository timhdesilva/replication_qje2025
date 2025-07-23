program Main_GS

! Load modules
#if defined(MPI)
   USE mpi
#endif
!$ USE omp_lib
   USE Parameters, only: procid, mpiprocs, dp, NbrThreads, OutputDir, RunMode, &
                         FixLabor, gamma, invEIS, SaveLCPanel, Policy, NbrPolicy, R_d, &
                         DebtPayoff
   USE Procedures, only: WriteMatrix2d_real, WriteMatrix2d_realdp, combine_gs
   USE Setup, only: Preliminaries, SetBorrowing, CreateGrids, CreateWageGrids, SimulateShocks
   USE ValueFunction_mod, only: ValueFunction, CleanSolution
   USE Simulations, only: LCSimulation, SaveLCSimulationPanel, CleanSimulation
   USE SMMFunctions, only: n_mnts, QuadraticForm, FormDataMoments, &
                           FormModelMoments, FormWeightMatrix, DataMoments, ModelMoments, &
                           WeightingMatrix, x, UnpackParams

   IMPLICIT NONE

   ! Parameters for code output
   real(dp), allocatable, dimension(:, :) :: MomOutput, SMMOutput
   ! Miscellaneous
   integer :: n_gs, n_pms, ip
   ! Set grid of parameters to search over
   real(dp), allocatable :: p_grid(:, :), x_fix(:)

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
   RunMode = 0

   ! Set grid of parameters to search over
   allocate (x_fix(18))
   n_pms = size(x_fix)
   x_fix(1:2) = (/gamma, invEIS/)
   x_fix(3:) = x
   CALL combine_gs(x_fix, p_grid, &
                   (/x_fix(1)/), &
                   (/x_fix(2)/), &
                   (/x_fix(3)/), &
                   (/x_fix(4)/), &
                   (/x_fix(5)/), &
                   (/x_fix(6)/), &
                   (/x_fix(7)/), &
                   (/x_fix(8)/), &
                   (/x_fix(9)/), &
                   (/x_fix(10)/), &
                   (/x_fix(11)/), &
                   (/x_fix(12)/), &
                   (/x_fix(13)/), &
                   (/x_fix(14)/), &
                   (/x_fix(15)/), &
                   (/x_fix(16)/), &
                   (/x_fix(17)/), &
                   (/x_fix(18)/) &
                   )
   n_gs = size(p_grid, 1)
   deallocate (x_fix)

   ! Set model parameters
   FixLabor = 0
   DebtPayoff = 1
   R_d = 1d0
   allocate (Policy(2))
   Policy = (/2004, 2005/)
   NbrPolicy = size(Policy)

! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Solve and simulate model over a range of parameters
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

! Initializations
   CALL Preliminaries
   CALL SetBorrowing(1)
   CALL CreateGrids
   CALL SimulateShocks
   CALL FormDataMoments

! Allocate size of SMM outputs
   allocate (MomOutput(n_gs + 1, n_pms + n_mnts))
   allocate (SMMOutput(n_gs, n_pms + 1))

! Loop over all parameter combination in this partition
   if (procid .eq. 0) print *, '----------------------------------------------------------------------------'
   do ip = 1, n_gs
      ! Set parameters
      CALL UnpackParams(p_grid(ip, 3:))
      ! Print iteration number, parameters, and save aprameter vector
      if (procid .eq. 0) print *, 'Parameter iteration number:', ip
      if (procid .eq. 0) print *, 'Parameters:', p_grid(ip, :)
      ! Create wage grids
      CALL CreateWageGrids
      ! Solve for value & policy functions
      CALL ValueFunction
      ! Simulation
      CALL LCSimulation(1)
      CALL CleanSolution
      if (SaveLCPanel .eq. 1 .and. procid .eq. 0) CALL SaveLCSimulationPanel(ip, 1)
      ! Calculate and output model moments
      CALL FormModelMoments
      CALL FormWeightMatrix
      MomOutput(ip + 1, :n_pms) = p_grid(ip, :)
      MomOutput(ip + 1, n_pms + 1:) = ModelMoments(:, 1)
      SMMOutput(ip, :n_pms) = p_grid(ip, :)
      SMMOutput(ip, n_pms + 1) = QuadraticForm(DataMoments, ModelMoments, WeightingMatrix)/100d0
      if (procid .eq. 0) print *, '  SMM Objective:', SMMOutput(ip, n_pms + 1)
      CALL CleanSimulation
      deallocate (ModelMoments, WeightingMatrix)
      if (procid .eq. 0) print *, '----------------------------------------------------------------------------'
   end do
   if (procid .eq. 0) then
      ! Add in data moments to moment outputs
      MomOutput(1, :n_pms) = 0d0
      MomOutput(1, n_pms + 1:) = DataMoments(:, 1)
      ! Write SMM and moment outputs to file
      CALL WriteMatrix2d_realdp(trim(OutputDir)//'SMM_GS.txt', 5, 0, n_gs, n_pms + 1, SMMOutput)
      CALL WriteMatrix2d_realdp(trim(OutputDir)//'ModelMoments_GS.txt', 5, 0, n_gs + 1, n_pms + n_mnts, MomOutput)
   end if

#if defined(MPI)
   IF (procid == 0) print *, 'Ended MPI'
   CALL MPI_FINALIZE(ierr)
#endif

end program Main_GS
