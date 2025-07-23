program Main_SMM

   ! Load modules
#if defined(MPI)
   USE mpi
#endif
!$ USE omp_lib
   USE Parameters, only: procid, mpiprocs, dp, NbrThreads, OutputDir, RunMode, FixLabor, &
                         Policy, NbrPolicy, R_d, DebtPayoff
   USE types
   USE Setup, only: Preliminaries, SetBorrowing, CreateGrids, SimulateShocks
   USE SMMFunctions, only: n_pms, n_mnts, nsim_data, FormDataMoments, DataMoments, f_fcn, x, lb, ub, step_nm
   USE amoeba_mod, only: amoeba
   USE TikTak_mod, only: TikTak

   implicit none

   ! Program variables
   real(dp) :: xopt(n_pms), fopt
   real(dp), allocatable, dimension(:) :: empty
   character(len=15) :: str
   integer :: date_time(8), ier
   character(len=12) real_clock(3)

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
   RunMode = 1

   ! Set model parameters
   FixLabor = 0
   DebtPayoff = 1
   R_d = 1d0
   allocate (Policy(2))
   Policy = (/2004, 2005/)
   NbrPolicy = size(Policy)

   ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ! SMM
   ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   ! Initializations
   CALL Preliminaries
   CALL SetBorrowing(1)
   CALL CreateGrids
   CALL SimulateShocks
   CALL FormDataMoments

   if (procid .eq. 0) then

      ! Open files for outputs
      open (331, FILE=trim(OutputDir)//'SMM.txt', STATUS='replace')
      open (332, FILE=trim(OutputDir)//'ModelMoments.txt', STATUS='replace')

      ! Write data moments to file
      allocate (empty(n_pms + n_mnts))
      empty(:n_pms) = 0d0
      empty(n_pms + 1:) = DataMoments(:, 1)
      write (UNIT=str, FMT='(I5)') size(empty)
      write (332, '('//trim(str)//'F24.15)') empty
      deallocate (empty)

      ! Start clock
      call date_and_time(real_clock(1), real_clock(2), real_clock(3), date_time)

   end if

   ! TikTak routine
   if (procid .eq. 0) then
      WRITE (*, *) 'NM STEP SIZE', step_nm
      WRITE (*, '(/, "  ****   END OF DRIVER ROUTINE OUTPUT BEFORE TIKTAK   ****")')
   end if
   CALL TikTak(0, f_fcn, n_pms, lb, ub, step_nm, 8000, 0.00125d0, 1d-2, 400, .TRUE., xopt, fopt)
   if (procid .eq. 0) then
      WRITE (*, '(/, "  ****   AFTER TIKTAK   ****")')
      WRITE (*, *) 'SOLUTION', xopt
      WRITE (*, *) 'OPTIMAL FUNCTION VALUE', fopt
   end if

   ! Polishing with Nelder-Mead
   x = xopt ! reset based on TikTak solution
   if (procid .eq. 0) then
      WRITE (*, *) 'STARTING VALUES', x
      WRITE (*, *) 'NM STEP SIZE', step_nm
      WRITE (*, '(/, "  ****   END OF DRIVER ROUTINE OUTPUT BEFORE NELDER-MEAD   ****")')
   end if
   CALL amoeba(n_pms, x, xopt, fopt, f_fcn, step_nm, 1d-4, 1000, ier)
   if (procid .eq. 0) then
      WRITE (*, '(/, "  ****   AFTER NELDER-MEAD   ****")')
      WRITE (*, *) 'SOLUTION', xopt
      WRITE (*, *) 'OPTIMAL FUNCTION VALUE', fopt
      WRITE (*, *) 'OPTIMIZATION FLAG', ier
   end if

   if (procid .eq. 0) then

      ! Close files
      close (331)
      close (332)

      ! Write out time
      write (*, "('Start date: ',i4,'-',i2,'-',i2)") date_time(1:3)
      write (*, "('Start time: ',i2,':',i2,':',i2)") date_time(5:7)
      call date_and_time(real_clock(1), real_clock(2), real_clock(3), date_time)
      write (*, "('End date: ',i4,'-',i2,'-',i2)") date_time(1:3)
      write (*, "('End time: ',i2,':',i2,':',i2)") date_time(5:7)

   end if

#if defined(MPI)
   IF (procid == 0) print *, 'Ended MPI'
   CALL MPI_FINALIZE(ierr)
#endif

end program Main_SMM
