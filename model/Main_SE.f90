program Main_SE

   ! Load modules
#if defined(MPI)
   USE mpi
#endif
!$ USE omp_lib
   USE Parameters, only: procid, mpiprocs, dp, NbrThreads, OutputDir, RunMode, FixLabor, Policy, NbrPolicy, &
                         R_d, DebtPayoff, ind_E2, FirstIJDeath, FirstAge, FirstYr, LastYr, SimHELPSwitchIJ
   USE types
   USE Setup, only: Preliminaries, SetBorrowing, CreateGrids, SimulateShocks
   USE SMMFunctions, only: n_pms, n_mnts, &
                           nsim_data, FormDataMoments, FormDataVCV, DataMoments, VCV, &
                           ModelMoments, ReweightingVector, WeightingMatrix, &
                           f_fcn_se, x, step_nm, lb_e, ub_e
   USE Procedures, only: WriteMatrix2d_realdp, StandardErrors, JTest, AGS_Sensitivity, IdentityMatrix

   implicit none

   ! Parameters
   integer, parameter :: n_data = 1701464 ! number of 26-year-old debtholder observations in Table 2 column (3) in data
   integer, parameter :: ij_26 = 26 - FirstAge + 1 ! ij index of 26-year-olds

   ! Program variables
   real(dp), allocatable, dimension(:) :: empty, g_up, g_dw, m_up, m_dw, reweight
   real(dp), allocatable, dimension(:, :) :: Jacobian, Elasticity, Lambda, f_base
   real(dp) :: obj, x_up(n_pms), x_dw(n_pms), ses(n_pms), Jstat, delta, S
   integer :: ind, year_ij, iM, iP
   character(len=15) :: str

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
   RunMode = 2

   ! Set model parameters
   FixLabor = 0
   DebtPayoff = 1
   R_d = 1d0
   allocate (Policy(2))
   Policy = (/2004, 2005/)
   NbrPolicy = size(Policy)

   ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ! Standard errors
   ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   ! Initializations
   CALL Preliminaries
   CALL SetBorrowing(1)
   CALL CreateGrids
   CALL SimulateShocks
   CALL FormDataMoments
   CALL FormDataVCV

   if (procid .eq. 0) then

      ! Open files for outputs
      open (331, FILE=trim(OutputDir)//'SMM_SE.txt', STATUS='replace')
      open (332, FILE=trim(OutputDir)//'ModelMoments_SE.txt', STATUS='replace')

      ! Write data moments to file
      allocate (empty(n_pms + n_mnts))
      empty(:n_pms) = 0d0
      empty(n_pms + 1:) = DataMoments(:, 1)
      write (UNIT=str, FMT='(I5)') size(empty)
      write (332, '('//trim(str)//'F24.15)') empty
      deallocate (empty)

   end if

   ! Calculate ratio of simulation to data observations
   S = 0d0
   DO ind = ind_E2, nsim_data
      IF (FirstIJDeath(ind) <= ij_26) CYCLE ! alive until 26
      year_ij = ij_26 - SimHELPSwitchIJ(ind) + 2005 ! year in which individual is age 26
      IF (year_ij >= FirstYr .and. year_ij <= LastYr) S = S + 1d0
   END DO
   S = S/real(n_data, kind=dp)

   ! Calculate Jacobian and elasticity of model moments via two-sided differentiation
   allocate (Jacobian(n_mnts, n_pms))
   allocate (Elasticity, mold=Jacobian)
   allocate (g_up(n_mnts))
   allocate (g_dw, m_up, m_dw, mold=g_up)
   DO iP = 1, n_pms
      if (procid .eq. 0) print *, 'PARAMETER ITERATION', iP, 'OF', n_pms
      ! Set up and down parameter values
      x_up = x
      x_dw = x
      x_up(iP) = x(iP) + 0.05d0*step_nm(iP)
      x_dw(iP) = x(iP) - 0.05d0*step_nm(iP)
      IF (x_dw(iP) <= lb_e(iP)) THEN
         x_dw(iP) = x(iP)
         x_up(iP) = x(iP) + 0.1d0*step_nm(iP)
      ELSE IF (x_up(iP) >= ub_e(iP)) THEN
         x_up(iP) = x(iP)
         x_dw(iP) = x(iP) - 0.1d0*step_nm(iP)
      END IF
      ! Solve model at up and down
      obj = f_fcn_se(x_up)
      CALL Compute_g(g_up)
      m_up = ModelMoments(:, 1)
      DEALLOCATE (ModelMoments, WeightingMatrix)
      obj = f_fcn_se(x_dw)
      CALL Compute_g(g_dw)
      m_dw = ModelMoments(:, 1)
      DEALLOCATE (ModelMoments, WeightingMatrix)
      ! Compute Jacobian
      DO iM = 1, n_mnts
         Jacobian(iM, iP) = (g_up(iM) - g_dw(iM))/(x_up(iP) - x_dw(iP))
         Elasticity(iM, iP) = (LOG(ABS(m_up(iM))) - LOG(ABS(m_dw(iM))))/(LOG(ABS(x_up(iP))) - LOG(ABS(x_dw(iP))))
         IF (x_up(iP) < 0d0 .and. x_dw(iP) < 0d0) THEN
            Elasticity(iM, iP) = -Elasticity(iM, iP)
         ELSE IF (x_up(iP) > 0d0 .and. x_dw(iP) > 0d0) THEN
            Elasticity(iM, iP) = Elasticity(iM, iP)
         ELSE
            IF (procid .eq. 0) PRINT *, 'Sign switch for parameter:', iP
         END IF
         IF (m_up(iM) < 0d0 .and. m_dw(iM) < 0d0) THEN
            Elasticity(iM, iP) = -Elasticity(iM, iP)
         ELSE IF (m_up(iM) > 0d0 .and. m_dw(iM) > 0d0) THEN
            Elasticity(iM, iP) = Elasticity(iM, iP)
         ELSE
            IF (procid .eq. 0) PRINT *, 'Sign switch for moment:', iM
         END IF
      END DO
   END DO
   deallocate (g_up, g_dw, m_up, m_dw)

   ! Write out Jacobian
   IF (procid .eq. 0) CALL WriteMatrix2d_realdp(trim(OutputDir)//'Jacobian.txt', 314, 0, n_mnts, n_pms, Jacobian)
   IF (procid .eq. 0) CALL WriteMatrix2d_realdp(trim(OutputDir)//'Elasticity.txt', 314, 0, n_mnts, n_pms, Elasticity)
   DEALLOCATE (Elasticity)

   ! Evaluate at baseline
   obj = f_fcn_se(x)
   allocate (f_base(n_mnts, 1))
   CALL Compute_g(f_base(:, 1))

   ! Compute moment reweighting
   ALLOCATE (reweight(n_mnts))
   CALL ReweightingVector(reweight)

   ! Adjust weighting matrix and data VCV matrix if you are using arc-sin weighting matrix
   CALL IdentityMatrix(n_mnts, WeightingMatrix)
   DO iM = 1, n_mnts
      IF (1d-2 > ABS(ModelMoments(iM, 1)) + ABS(DataMoments(iM, 1))) THEN
         delta = 1d-2**(-1d0)
      ELSE
         delta = ABS(ModelMoments(iM, 1))*ABS(DataMoments(iM, 1)) + ModelMoments(iM, 1)*DataMoments(iM, 1)
         delta = delta/ABS(DataMoments(iM, 1))/((ABS(ModelMoments(iM, 1)) + ABS(DataMoments(iM, 1)))**2d0)
      END IF
      ! Factor in moment reweighting, as you do in FormWeightMatrix
      VCV(iM, iM) = VCV(iM, iM)*reweight(iM)
      VCV(iM, iM) = VCV(iM, iM)*4d0*(delta**2d0)
   END DO
   DEALLOCATE (reweight)

   ! Calculate standard errors
   CALL StandardErrors(n_mnts, n_pms, S, Jacobian, WeightingMatrix, VCV, ses)

   ! Do J-test
   Jstat = JTest(n_mnts, n_pms, S, S*real(n_data, kind=dp), Jacobian, WeightingMatrix, VCV, f_base)

   ! Calculate and output AGS sensitivity
   allocate (Lambda(n_pms, n_mnts))
   CALL AGS_Sensitivity(n_mnts, n_pms, Jacobian, WeightingMatrix, Lambda)
   IF (procid .eq. 0) CALL WriteMatrix2d_realdp(trim(OutputDir)//'Sensitivity.txt', 314, 0, n_pms, n_mnts, Lambda)

   if (procid .eq. 0) then

      ! Print results
      print *, 'STANDARD ERRORS:', ses
      print *, 'J-STATISTIC:', Jstat

      ! Close files
      close (331)
      close (332)

   end if

   ! Deallocate
   deallocate (ModelMoments, WeightingMatrix, Jacobian, Lambda, f_base)

#if defined(MPI)
   IF (procid == 0) print *, 'Ended MPI'
   CALL MPI_FINALIZE(ierr)
#endif

CONTAINS

   !---------------------------------------------------------------------------------------------------------!
   ! ROUTINE TO CALCULATE g(\theta) FOR CALCULATION JACOBIAN, WHICH NEEDS TO INCORPORATE PART OF WEIGHTING
   ! MATRIX IF WEIGHTING MATRIX IS PARAMETER-DEPENDENT
   !---------------------------------------------------------------------------------------------------------!

   SUBROUTINE Compute_g(g)
      IMPLICIT NONE
      REAL(dp), INTENT(OUT) :: g(n_mnts)
      INTEGER :: i
      g = ModelMoments(:, 1) - DataMoments(:, 1)
      DO i = 1, n_mnts
         g(i) = g(i)*SQRT(WeightingMatrix(i, i))
      END DO
   END SUBROUTINE Compute_g

end program Main_SE
