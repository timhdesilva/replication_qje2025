MODULE SMMFunctions

#if defined(MPI)
   USE mpi
#endif

   USE Parameters, only: procid, mpiprocs, sp, dp, positiveInf, epsl, InputDir, SEDir, OutputDir, SMMError, FixLabor, &
                         beta, phi, cprob, fcL, fcH, kappa, lerr, deltas, deltaEs, rhotheta, sigmanu, sigmaeps, sigmai, &
                         numeraire, FirstYr, LastYr, FirstAge, gd_age, nsim, JWork, ind_E2, pe1_sim, p_e, &
                         SimY, SimWL, SimLaborS, SimE, SimDebt, SimRepay, SimHELPSwitchIJ, SimLerr, FirstIJDeath, &
                         HELPTsh04, HELPTsh05, price04, price06, price07, price08
   USE Procedures, only: OLS, mean_matrix, variance_array, kurtosis_array, quantile_array, &
                         IdentityMatrix, ArcSinMatrix, QuadraticForm, &
                         custom_round, WriteMatrix2d_real, is_in_array, slice_array, kurtosis_array
   USE types
   USE Setup, only: CreateWageGrids
   USE ValueFunction_mod, only: ValueFunction, CleanSolution
   USE Simulations, only: LCSimulation, CleanSimulation

   IMPLICIT NONE

   ! SMM Parameters
   integer, parameter :: n_pms = 16 ! number of parameters optimizing over
   integer, parameter :: n_restarts = 2 ! number of restarts in if SMMOptimizer = 1
   real(dp), dimension(n_pms) :: x = (/ &
                                 0.149490301014516d0, &
                                 0.153107396115931d0, &
                                 9.656622793083034d0, &
                                 0.064560574728113d0, &
                                 -0.000892058817093d0, &
                                 -0.473352659492287d0, &
                                 0.019062967569465d0, &
                                 0.928501413056034d0, &
                                 0.224248506569689d0, &
                                 0.149577479803016d0, &
                                 0.568833946056649d0, &
                                 0.937302546630436d0, &
                                 0.009442063869102d0, &
                                 0.426074640769878d0, &
                                 0.079773158223959d0, &
                                 0.034152844616048d0 &
                                 /)
   real(dp), parameter, dimension(n_pms) :: lb = (/ & ! lower bound
                                            0.01d0, & ! phi
                                            0d0, & ! cprob
                                            8.5d0, & ! delta_0
                                            0.06d0, & ! delta_1
                                            -0.0015d0, & ! delta_2
                                            -0.6d0, & ! deltaE_0
                                            0.015d0, & ! deltaE_1
                                            0.8d0, & ! rho_theta
                                            0.05d0, & ! sigma_nu
                                            1d-2, & ! sigma_eps
                                            0.4d0, & ! sigma_i
                                            0.85d0, & ! beta
                                            0d0, & ! fcL
                                            -2d0, & ! kappa
                                            0d0, & ! fcH
                                            0.02d0 & ! lerr
                                            /)
   real(dp), parameter, dimension(n_pms) :: ub = (/ & ! upper bound
                                            0.35d0, & ! phi
                                            0.8d0, & ! cprob
                                            10d0, & ! delta_0
                                            0.08d0, & ! delta_1
                                            -0.0005d0, & ! delta_2
                                            -0.4d0, & ! deltaE_0
                                            0.025d0, & ! deltaE_1
                                            0.98d0, & ! rho_theta
                                            0.4d0, & ! sigma_nu
                                            0.3d0, & ! sigma_eps
                                            0.8d0, & ! sigma_i
                                            0.99d0, & ! beta
                                            0.04d0, & ! fcL
                                            1d0, & ! kappa
                                            0.2d0, & ! fcH
                                            0.04d0 & ! lerr
                                            /)
   real(dp), parameter, dimension(n_pms) :: step_nm = (/ & ! Nelder-Mead step size ~= (ub - lb)/4
                                            0.085d0, & ! phi
                                            0.2d0, & ! cprob
                                            0.375d0, & ! delta_0
                                            0.005d0, & ! delta_1
                                            0.00025d0, & ! delta_2
                                            0.05d0, & ! deltaE_0
                                            0.0025d0, & ! deltaE_1
                                            0.045d0, & ! rho_theta
                                            0.0875d0, & ! sigma_nu
                                            0.075d0, & ! sigma_eps
                                            0.1d0, & ! sigma_i
                                            0.035d0, & ! beta
                                            0.01d0, & ! fcL
                                            0.75d0, & ! kappa
                                            0.05d0, & ! fcH
                                            0.005d0 & ! lerr
                                            /)
   real(dp), parameter, dimension(n_pms) :: lb_e = (/ & ! lower bound for evaluation (weaker than lb)
                                            1d-3, & ! phi
                                            0d0, & ! cprob
                                            5d0, & ! delta_0
                                            0d0, & ! delta_1
                                            -0.01d0, & ! delta_2
                                            -2d0, & ! deltaE_0
                                            -0.05d0, & ! deltaE_1
                                            0d0, & ! rho_theta
                                            0d0, & ! sigma_nu
                                            0d0, & ! sigma_eps
                                            0d0, & ! sigma_i
                                            0.4d0, & ! beta
                                            0d0, & ! fcL
                                            -5d0, & ! kappa
                                            0d0, & ! fcH
                                            0d0 & ! lerr
                                            /)
   real(dp), parameter, dimension(n_pms) :: ub_e = (/ & ! upper bound for evaulation (weaker than ub)
                                            1d0, & ! phi
                                            1d0, & ! cprob
                                            15d0, & ! delta_0
                                            0.5d0, & ! delta_1
                                            0d0, & ! delta_2
                                            0.5d0, & ! deltaE_0
                                            0.2d0, & ! deltaE_1
                                            0.99999d0, & ! rho_theta
                                            2d0, & ! sigma_nu
                                            2d0, & ! sigma_eps
                                            2d0, & ! sigma_i
                                            0.99999d0, & ! beta
                                            0.2d0, & ! fcL
                                            3d0, & ! kappa
                                            1d0, & ! fcH
                                            0.5d0 & ! lerr
                                            /)

   ! Information on data moments
   real(dp), parameter, dimension(5) :: inc_var_ages = (/22d0, 32d0, 42d0, 52d0, 62d0/) ! ages for which you calculate X/S income variance
   integer, parameter :: bin_a = 5, nbins_a = ceiling(real(JWork - 1)/real(bin_a)) ! size of age bins for capita lincome age profiles
   integer, parameter, dimension(1) :: capinc_ind = (/5/) ! index of capital income age profile momemts to take

   ! Number of model moments
   integer, parameter :: n_mnts = 44

   ! Number of individuals to consider in simulation for CalcIncomeMoments and CapitalIncAP so that
   ! the fraction of E=1 individuals is the same as in the data
   integer, parameter :: nsim_data = MIN(ind_E2 - 1 + CEILING(REAL(nsim, kind=dp)*pe1_sim*p_e/(1d0 - p_e)), nsim)

   ! Global array for income distributions
   real(dp), allocatable, dimension(:, :, :) :: IncDistrib

   ! Global arrays for SMM
   real(dp), allocatable, dimension(:, :) :: DataMoments, ModelMoments, WeightingMatrix, VCV

CONTAINS

!---------------------------------------------------------------------------------------------------------!
! Function to unpack parameters and set at SMM values
!---------------------------------------------------------------------------------------------------------!

   SUBROUTINE UnpackParams(p)
      IMPLICIT NONE
      real(dp), intent(in) :: p(n_pms)
      phi = p(1)
      cprob = p(2)
      deltas = p(3:5)
      deltaEs = p(6:7)
      rhotheta = p(8)
      sigmanu = p(9)
      sigmaeps = p(10)
      sigmai = p(11)
      beta = p(12)
      fcL = p(13)
      kappa = p(14)
      fcH = p(15)
      lerr = p(16)
   END SUBROUTINE UnpackParams

   SUBROUTINE SetParamsSMM
      IMPLICIT NONE
      CALL UnpackParams(x)
   END SUBROUTINE SetParamsSMM

!---------------------------------------------------------------------------------------------------------!
! Function to read in data moments
!---------------------------------------------------------------------------------------------------------!

   SUBROUTINE FormDataMoments
      IMPLICIT NONE
      real(dp) :: inc_moments(13), capitalinc(nbins_a), ratios(6), hours(3), panel(1)
      integer, parameter :: nrow_d = 13, ncol_d = 2
      real(dp) :: distrib(ncol_d, nrow_d), IncDistrib_data(nrow_d, ncol_d, 2), panel_moments(5)
      integer :: i

      ! Read in income process moments as first elements of vector
      OPEN (12, File=InputDir//'cs_variances.txt')
      OPEN (13, File=InputDir//'age_effects.txt')
      OPEN (14, File=InputDir//'p10_growth.txt')
      OPEN (15, File=InputDir//'p90_growth.txt')
      OPEN (17, File=InputDir//'debt_premium.txt')
      OPEN (18, File=InputDir//'ageprofile_yrfe_capital.txt')
      OPEN (19, File=InputDir//'panel_moments_500.txt')

      READ (12, *) inc_moments(1)
      READ (12, *) inc_moments(2)
      READ (12, *) inc_moments(3)
      READ (12, *) inc_moments(4)
      READ (12, *) inc_moments(5)

      READ (13, *) inc_moments(6)
      READ (13, *) inc_moments(7)
      inc_moments(7) = inc_moments(7)*100d0 ! scale to similar units

      READ (14, *) inc_moments(8)
      READ (14, *) inc_moments(9)

      READ (15, *) inc_moments(10)
      READ (15, *) inc_moments(11)

      READ (17, *) inc_moments(12)
      READ (17, *) inc_moments(13)

      DO i = 1, nbins_a
         READ (18, *) capitalinc(i)
      END DO
      capitalinc = capitalinc/numeraire*100d0 ! adjust for numeraire

      DO i = 1, 5
         READ (19, *) panel_moments(i)
      END DO
      panel(1) = panel_moments(1)

      CLOSE (11)
      CLOSE (12)
      CLOSE (13)
      CLOSE (14)
      CLOSE (15)
      CLOSE (17)
      CLOSE (18)
      CLOSE (19)

      ! Read in pooled income distributions pre and post
      OPEN (91, FILE=InputDir//'bunching_pre.txt')
      READ (91, *) distrib
      CLOSE (91)
      IncDistrib_data(:, :, 1) = transpose(distrib)
      OPEN (91, FILE=InputDir//'bunching_post.txt')
      READ (91, *) distrib
      CLOSE (91)
      IncDistrib_data(:, :, 2) = transpose(distrib)

      ! Read in bunch ratios
      OPEN (91, FILE=InputDir//'ratios_pooled_500.txt')
      READ (91, *) ratios(1)
      READ (91, *) ratios(2)
      READ (91, *) ratios(3)
      CLOSE (91)
      OPEN (91, FILE=InputDir//'ratios_pooled_bydebt_500.txt')
      READ (91, *) ratios(4)
      READ (91, *) ratios(5)
      READ (91, *) ratios(6)
      CLOSE (91)

      ! Hours moments
      hours(1) = 1d0
      OPEN (91, FILE=InputDir//'fraction_noadjust.txt')
      READ (91, *) hours(2)
      CLOSE (91)
      OPEN (91, FILE=InputDir//'hours_kurtosis.txt')
      READ (91, *) hours(3)
      CLOSE (91)

      ! Moment Set
      allocate (DataMoments(n_mnts, 1))
      DataMoments(:13, 1) = inc_moments
      DataMoments(14:26, 1) = IncDistrib_data(:, 2, 1)
      DataMoments(27:39, 1) = IncDistrib_data(:, 2, 2)
      DataMoments(40, 1) = ratios(6)
      DataMoments(41:43, 1) = hours
      DataMoments(44:, 1) = panel

   END SUBROUTINE FormDataMoments

!---------------------------------------------------------------------------------------------------------!
! Function to form VCV of data moments
!---------------------------------------------------------------------------------------------------------!

   SUBROUTINE FormDataVCV
      IMPLICIT NONE
      real(dp) :: inc_moments(13), capitalinc(nbins_a), ratios(6), hours(3), panel(1)
      integer, parameter :: nrow_d = 13, ncol_d = 1
      real(dp) :: distrib(ncol_d, nrow_d), IncDistrib_data(nrow_d, ncol_d, 2), panel_moments(5)
      real(dp), allocatable :: diagVCV(:, :)
      integer :: i

      ! Read in income process moments as first elements of vector
      OPEN (12, File=SEDir//'cs_variances.txt')
      OPEN (13, File=SEDir//'age_effects.txt')
      OPEN (14, File=SEDir//'p10_growth.txt')
      OPEN (15, File=SEDir//'p90_growth.txt')
      OPEN (17, File=SEDir//'debt_premium.txt')
      OPEN (18, File=SEDir//'ageprofile_yrfe_capital.txt')
      OPEN (19, File=SEDir//'panel_moments_500.txt')

      READ (12, *) inc_moments(1)
      READ (12, *) inc_moments(2)
      READ (12, *) inc_moments(3)
      READ (12, *) inc_moments(4)
      READ (12, *) inc_moments(5)

      READ (13, *) inc_moments(6)
      READ (13, *) inc_moments(7)
      inc_moments(7) = inc_moments(7)*100d0 ! scale to similar units

      READ (14, *) inc_moments(8)
      READ (14, *) inc_moments(9)

      READ (15, *) inc_moments(10)
      READ (15, *) inc_moments(11)

      READ (17, *) inc_moments(12)
      READ (17, *) inc_moments(13)

      DO i = 1, nbins_a
         READ (18, *) capitalinc(i)
      END DO
      capitalinc = capitalinc/numeraire*100d0 ! adjust for numeraire

      DO i = 1, 5
         READ (19, *) panel_moments(i)
      END DO
      panel(1) = panel_moments(1)

      CLOSE (11)
      CLOSE (12)
      CLOSE (13)
      CLOSE (14)
      CLOSE (15)
      CLOSE (17)
      CLOSE (18)
      CLOSE (19)

      ! Read in pooled income distributions pre and post
      OPEN (91, FILE=SEDir//'bunching_pre.txt')
      READ (91, *) distrib
      CLOSE (91)
      IncDistrib_data(:, :, 1) = transpose(distrib)
      OPEN (91, FILE=SEDir//'bunching_post.txt')
      READ (91, *) distrib
      CLOSE (91)
      IncDistrib_data(:, :, 2) = transpose(distrib)

      ! Read in bunch ratios
      OPEN (91, FILE=SEDir//'ratios_pooled_500.txt')
      READ (91, *) ratios(1)
      READ (91, *) ratios(2)
      READ (91, *) ratios(3)
      CLOSE (91)
      OPEN (91, FILE=SEDir//'ratios_pooled_bydebt_500.txt')
      READ (91, *) ratios(4)
      READ (91, *) ratios(5)
      READ (91, *) ratios(6)
      CLOSE (91)

      ! Hours moments
      OPEN (91, FILE=SEDir//'hours_mean.txt')
      READ (91, *) hours(1)
      CLOSE (91)
      OPEN (91, FILE=SEDir//'fraction_noadjust.txt')
      READ (91, *) hours(2)
      CLOSE (91)
      OPEN (91, FILE=SEDir//'hours_kurtosis.txt')
      READ (91, *) hours(3)
      CLOSE (91)

      ! Moment Set
      allocate (diagVCV(n_mnts, 1))
      diagVCV(:13, 1) = inc_moments
      diagVCV(14:26, 1) = IncDistrib_data(:, 1, 1)
      diagVCV(27:39, 1) = IncDistrib_data(:, 1, 2)
      diagVCV(40, 1) = ratios(6)
      diagVCV(41:43, 1) = hours
      diagVCV(44:, 1) = panel

      ! Make VCV matrix
      allocate (VCV(n_mnts, n_mnts))
      VCV = 0d0
      DO i = 1, n_mnts
         VCV(i, i) = diagVCV(i, 1)**2d0
      END DO

   END SUBROUTINE FormDataVCV

!---------------------------------------------------------------------------------------------------------!
! Function to calculate income process moments
!---------------------------------------------------------------------------------------------------------!
   SUBROUTINE CalcIncomeMoments(inc_moments, flag)

      IMPLICIT NONE

      real(dp), intent(out) :: inc_moments(13)
      integer, intent(out) :: flag
      real(dp), parameter :: minY = 2914.6d0/numeraire/price04 ! minimum wage to be included in sample = 1/4 work at minimum wage in 2004
      integer :: minsince, maxsince, ind, ij, since_ij, ij_a, n_g1, n_g5, n_above, counter
      integer, allocatable, dimension(:) :: above_g1, above_g5
      integer, allocatable, dimension(:, :) :: above_mat, above_g1_mat, above_g5_mat
      real(dp) :: meanE, mean_Y
      real(dp), allocatable, dimension(:) :: linc_vec, betahat, g1, g5
      real(dp), allocatable, dimension(:, :) :: tempWL, age_mat, resid, g_mat
      logical :: parmpi
#if defined(MPI)
      real(dp), allocatable :: rbuff(:)
      integer :: ierr
      integer, parameter :: i_par = 1 + size(inc_var_ages) + 2 ! index of inc_moments(:) at which you start parallelizing
#endif

      ! Parameter controlling parallelization
      parmpi = mpiprocs >= 5

      ! Read in variables needed
      allocate (SimWL(nsim, JWork))
      OPEN (UNIT=10, FORM='unformatted', FILE=trim(OutputDir)//'SimWL')
      READ (10) SimWL
      CLOSE (UNIT=10)

      ! Initialize
      inc_moments = 0d0
      flag = 0

      ! Calculate bounds on event-time imposed by year restrictions
      minsince = FirstYr - 2005
      maxsince = LastYr - 2005

      ! Identify individuals above minY and between year range
      allocate (above_mat(nsim_data, JWork))
      above_mat = 0
      do ind = 1, nsim_data
         do ij = 1, JWork
            IF (ij >= FirstIJDeath(ind)) EXIT
            since_ij = ij - SimHELPSwitchIJ(ind)
            if (since_ij >= minsince .and. since_ij <= maxsince) then
               if (SimWL(ind, ij) >= minY) above_mat(ind, ij) = 1
            end if
         end do
      end do
      n_above = sum(above_mat)

      ! Calculate average income: Income moment #1
      allocate (tempWL(nsim_data, JWork))
      tempWL = SimWL(:nsim_data, :JWork)
      mean_Y = mean_matrix(nsim_data, JWork, tempWL, above_mat)

      ! Exit if mean income is just too far off to prevent calculating other moments or no-one above
      if (mean_Y < 0.5d0 .or. mean_Y > 2d0 .or. &
          real(n_above, kind=dp)/real(nsim_data*JWork, kind=dp) < 0.01d0) then
         flag = 1
         go to 99
      end if

      ! Calculate cross-sectional variance of log income by age: Income moment #2
      allocate (linc_vec(nsim_data))
      do ij = 1, size(inc_var_ages)
         ij_a = int(inc_var_ages(ij)) - FirstAge + 1
         if (sum(above_mat(:, ij_a)) < 4 .or. ij_a > JWork) then
            flag = 1
            go to 99
         end if
         linc_vec = LOG(MAX(tempWL(:, ij_a), minY))
         inc_moments(ij) = variance_array(nsim_data, linc_vec, above_mat(:, ij_a))
      end do
      deallocate (tempWL, linc_vec)

      ! Calculate age effects: Income moment #3
      allocate (linc_vec(n_above), age_mat(n_above, 3), betahat(3))
      age_mat(:, 1) = 1d0
      counter = 1
      do ind = 1, nsim_data
         do ij = 1, JWork
            if (above_mat(ind, ij) .eq. 1) then
               linc_vec(counter) = log(SimWL(ind, ij))
               age_mat(counter, 2) = gd_age(ij)
               age_mat(counter, 3) = gd_age(ij)**2d0
               counter = counter + 1
            end if
         end do
      end do
      deallocate (above_mat)
      CALL OLS(n_above, 3, age_mat, linc_vec, betahat)
      deallocate (linc_vec, age_mat)
      inc_moments(6:7) = betahat(2:3)
      inc_moments(7) = inc_moments(7)*100d0 ! scale to similar units

      IF ((parmpi .and. procid <= 3) .or. .not. parmpi) THEN

         ! Calculate residual income growth needed for later income moments
         allocate (resid(nsim_data, JWork))
         resid = 0d0
         do ind = 1, nsim_data
            do ij = 1, JWork
               if (SimWL(ind, ij) > 0d0) resid(ind, ij) = log(SimWL(ind, ij)) - betahat(1) - &
                                                          betahat(2)*gd_age(ij) - betahat(3)*(gd_age(ij)**2d0)
            end do
         end do
         deallocate (betahat)
         n_g1 = nsim_data*(JWork - 1)
         allocate (g_mat(nsim_data, JWork - 1), g1(n_g1))
         g_mat = resid(:, 2:) - resid(:, :JWork - 1)
         g1 = reshape(g_mat, (/n_g1/))
         deallocate (g_mat)
         n_g5 = nsim_data*(JWork - 5)
         allocate (g_mat(nsim_data, JWork - 5), g5(n_g5))
         g_mat = resid(:, 6:) - resid(:, :JWork - 5)
         g5 = reshape(g_mat, (/n_g5/))
         deallocate (resid, g_mat)

         ! Identify individuals above minY and between year range
         allocate (above_g1_mat(nsim_data, JWork - 1), above_g5_mat(nsim_data, JWork - 5))
         above_g1_mat = 0
         above_g5_mat = 0
         do ind = 1, nsim_data
            do ij = 2, JWork
               IF (ij >= FirstIJDeath(ind)) EXIT
               since_ij = ij - SimHELPSwitchIJ(ind)
               if ((since_ij >= minsince .and. since_ij <= maxsince) .and. &
                   SimWL(ind, ij) >= minY .and. SimWL(ind, ij - 1) >= minY) then
                  above_g1_mat(ind, ij - 1) = 1
               end if
            end do
            do ij = 6, JWork
               IF (ij >= FirstIJDeath(ind)) EXIT
               since_ij = ij - SimHELPSwitchIJ(ind)
               if ((since_ij >= minsince .and. since_ij <= maxsince) .and. &
                   SimWL(ind, ij) >= minY .and. SimWL(ind, ij - 5) >= minY) then
                  above_g5_mat(ind, ij - 5) = 1
               end if
            end do
         end do
         allocate (above_g1(n_g1))
         above_g1 = reshape(above_g1_mat, (/n_g1/))
         deallocate (above_g1_mat)
         allocate (above_g5(n_g5))
         above_g5 = reshape(above_g5_mat, (/n_g5/))
         deallocate (above_g5_mat)

         ! Calculate percentiles of income growth: Income moment #4
         IF (procid .eq. 0 .or. .not. parmpi) inc_moments(8) = quantile_array(n_g1, g1, 0.1d0, above_g1)
         IF (procid .eq. 1 .or. .not. parmpi) inc_moments(9) = quantile_array(n_g5, g5, 0.1d0, above_g5)
         IF (procid .eq. 2 .or. .not. parmpi) inc_moments(10) = quantile_array(n_g1, g1, 0.9d0, above_g1)
         IF (procid .eq. 3 .or. .not. parmpi) inc_moments(11) = quantile_array(n_g5, g5, 0.9d0, above_g5)

         deallocate (g1, g5, above_g1, above_g5)

      ELSE
         deallocate (betahat)
      END IF

      IF ((parmpi .and. procid .eq. 4) .or. .not. parmpi) THEN

         ! Identify individuals above minY, between year range, and born after 1991
         allocate (above_mat(nsim_data, JWork))
         above_mat = 0
         do ind = 1, nsim_data
            do ij = 1, JWork
               IF (ij >= FirstIJDeath(ind)) EXIT
               since_ij = ij - SimHELPSwitchIJ(ind)
               if ((since_ij >= minsince .and. since_ij <= maxsince) .and. &
                   SimHELPSwitchIJ(ind) <= 2005 - FirstYr + 1) then
                  if (SimWL(ind, ij) >= minY) above_mat(ind, ij) = 1
               end if
            end do
         end do
         n_above = sum(above_mat)

         ! Calculate difference in age effects based on debt: Income moment #6
         allocate (linc_vec(n_above), age_mat(n_above, 5), betahat(5))
         age_mat(:, 1) = 1d0
         counter = 1
         meanE = 0d0
         do ind = 1, nsim_data
            do ij = 1, JWork
               if (above_mat(ind, ij) .eq. 1) then
                  linc_vec(counter) = log(SimWL(ind, ij))
                  age_mat(counter, 2) = gd_age(ij)
                  age_mat(counter, 3) = gd_age(ij)**2d0
                  age_mat(counter, 4) = (real(SimE(ind), kind=dp) - 1d0)
                  age_mat(counter, 5) = gd_age(ij)*age_mat(counter, 4)
                  counter = counter + 1
                  meanE = meanE + real(SimE(ind), kind=dp)/real(n_above, kind=dp)
               end if
            end do
         end do
         deallocate (above_mat)
         CALL OLS(n_above, 5, age_mat, linc_vec, betahat)
         inc_moments(12:13) = betahat(4:5)
         deallocate (linc_vec, age_mat, betahat)

      END IF

      ! Deallocate
99    deallocate (SimWL)

      ! Combine moments
#if defined(MPI)
      IF (parmpi) THEN
         allocate (rbuff(13 - i_par + 1))
         CALL MPI_ALLREDUCE(inc_moments(i_par:), rbuff, 13 - i_par + 1, MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD, ierr)
         inc_moments(i_par:) = rbuff
         deallocate (rbuff)
      END IF
#endif

   END SUBROUTINE CalcIncomeMoments

!---------------------------------------------------------------------------------------------------------!
! Function to calculate hours moments
!---------------------------------------------------------------------------------------------------------!
   SUBROUTINE HoursMoments(hours)

      IMPLICIT NONE

      real(dp), intent(out) :: hours(3)
      ! Calculation parameters
      integer, parameter :: n_lag = 1 ! number of periods to lag when calculating change in hours
      real(sp), parameter :: tol_adj = 1.0/40.062 ! adjustment tolerance normalized by HILDA mean hours
      ! Program variables
      integer :: ind, ij, year_ij, n_sample
      real(dp) :: count
      integer, allocatable, dimension(:, :) :: sample
      integer, allocatable, dimension(:) :: sample1d
      real(dp), allocatable, dimension(:, :) :: d_logL
      real(dp), allocatable, dimension(:) :: d_logL1d

      ! Read in variables needed
      allocate (SimLaborS(nsim, JWork), SimLerr(nsim, JWork))
      OPEN (UNIT=10, FORM='unformatted', FILE=trim(OutputDir)//'SimLaborS')
      READ (10) SimLaborS
      CLOSE (UNIT=10)
      OPEN (UNIT=10, FORM='unformatted', FILE=trim(OutputDir)//'SimLerr')
      READ (10) SimLerr
      CLOSE (UNIT=10)

      ! Add measurement error
      do ind = 1, nsim
         do ij = 1, JWork
            SimLaborS(ind, ij) = SimLaborS(ind, ij) + real(lerr*SimLerr(ind, ij), kind=sp)
         end do
      end do
      deallocate (SimLerr)

      ! Moment #1: Mean hours among E=2
      hours = 0d0
      count = 0d0
      do ind = ind_E2, nsim ! educated only
         do ij = 2, JWork ! start after initial period
            IF (ij >= FirstIJDeath(ind)) EXIT
            year_ij = ij - SimHELPSwitchIJ(ind) + 2005
            if (year_ij >= FirstYr .and. year_ij <= LastYr) then
               hours(1) = hours(1) + real(SimLaborS(ind, ij), kind=dp)
               count = count + 1d0
            end if
         end do
      end do
      hours(1) = hours(1)/count

      ! Moments #2 and #3: Fraction adjust and kurtosis of log hours changes among full sample
      allocate (sample(nsim, JWork - n_lag))
      sample = 0
      count = 0d0
      do ind = 1, nsim ! full sample
         do ij = n_lag + 1, JWork
            IF (ij >= FirstIJDeath(ind)) EXIT
            year_ij = ij - SimHELPSwitchIJ(ind) + 2005
            if (year_ij >= FirstYr .and. year_ij <= LastYr) then
               if (SimLaborS(ind, ij) > 0e0 .and. SimLaborS(ind, ij - n_lag) > 0e0) sample(ind, ij - n_lag) = 1
               if (ABS(SimLaborS(ind, ij) - SimLaborS(ind, ij - n_lag)) <= tol_adj) hours(2) = hours(2) + 1d0 ! fraction not adjust
               count = count + 1d0
            end if
         end do
      end do
      hours(2) = hours(2)/count
      allocate (d_logL(nsim, JWork - n_lag))
      d_logL = 0d0
      do ind = 1, nsim
         do ij = 1, JWork - n_lag
            if (sample(ind, ij) .eq. 1) then
               d_logL(ind, ij) = LOG(REAL(SimLaborS(ind, ij + n_lag), kind=dp)/REAL(SimLaborS(ind, ij), kind=dp))
            end if
         end do
      end do
      deallocate (SimLaborS)
      n_sample = nsim*(JWork - n_lag)
      allocate (sample1d(n_sample))
      sample1d = reshape(sample, (/n_sample/))
      deallocate (sample)
      allocate (d_logL1d(n_sample))
      d_logL1d = reshape(d_logL, (/n_sample/))
      deallocate (d_logL)
      hours(3) = kurtosis_array(n_sample, d_logL1d, sample1d)
      deallocate (sample1d, d_logL1d)

   END SUBROUTINE HoursMoments

!---------------------------------------------------------------------------------------------------------!
! Function to calculate income distribution for debtholders and non-debt holders in pre and post periods
!---------------------------------------------------------------------------------------------------------!

   SUBROUTINE SaveDistPooled(period, tsh, brange, binw, price, ij_first, ij_last, result)
      IMPLICIT NONE
      ! Parameters for bunching calculation that need to match data in 2_moments_bunching.py
      integer, intent(in), dimension(:) :: period ! range of period
      real(dp), intent(in) :: tsh ! threshold for all years
      real(dp), intent(in), dimension(2) :: brange ! range in period in rounded dollars from year of tsh
      real(dp), intent(in) :: binw ! width of histogram bins
      real(dp), intent(in) :: price ! price level to convert from 2005 to years of interest
      integer, intent(in) :: ij_first, ij_last ! first and last ij to consider for bunching
      real(dp), intent(out), dimension(:) :: result
      ! Program variables
      integer :: ind, ij, since_ij, Nbins, b
      real(dp) :: tsh_r, y, total, dist
      real(dp), allocatable, dimension(:) :: bins
#if defined(MPI)
      real(dp), allocatable :: rbuff(:)
      integer :: ierr
#endif

      ! Read in variables needed
      allocate (SimY(nsim, JWork), SimDebt(nsim, JWork))
      OPEN (UNIT=10, FORM='unformatted', FILE=trim(OutputDir)//'SimY')
      READ (10) SimY
      CLOSE (UNIT=10)
      OPEN (UNIT=10, FORM='unformatted', FILE=trim(OutputDir)//'SimDebt')
      READ (10) SimDebt
      CLOSE (UNIT=10)

      ! Calculate rounded threshold
      tsh_r = custom_round(tsh, binw)
      ! Calculate number of bins needed and make bins
      Nbins = ceiling((brange(2) - brange(1))/binw) + 1
      allocate (bins(Nbins))
      bins = brange(1)
      do b = 2, Nbins
         bins(b) = bins(b - 1) + binw
      end do
      ! Allocate size of income distribution and initialize
      result = 0d0
      ! Loop over bins and populate results
#if defined(MPI)
      CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)
      do b = (procid*Nbins)/mpiprocs + 1, ((procid + 1)*Nbins)/mpiprocs ! MPI loop over bins
#else
         !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(b,total,dist,ind,ij,y,since_ij)
         !$OMP DO SCHEDULE(STATIC)
         do b = 1, Nbins ! bin
#endif
            total = 0d0
            dist = 0d0
            do ind = 1, nsim
               do ij = ij_first, ij_last
                  IF (ij >= FirstIJDeath(ind)) EXIT
                  since_ij = ij - SimHELPSwitchIJ(ind)
                  if (SimDebt(ind, ij) > 0e0 .and. is_in_array(2005 + since_ij, period)) then
                     ! Adjust income for numeraire and inflation
                     y = SimY(ind, ij)*numeraire*price
                     ! Round income and re-center around rounded threshold
                     y = custom_round(y - tsh - 1d0, binw) + tsh_r
                     ! Count number of debtholders in each bin and within range
                     if (y >= brange(1) .and. y <= brange(2)) total = total + 1d0 ! number of people that fall within range
                     if (y < bins(b) + binw .and. y >= bins(b)) dist = dist + 1d0 ! number in each bin
                  end if
               end do
            end do
            ! Calculate fraction
            if (total > 0d0) result(b) = dist/total*100d0
#if defined(MPI)
         end do ! MPI loop
         if (mpiprocs > 1) then
            allocate (rbuff(size(result)))
            CALL MPI_ALLREDUCE(result, rbuff, size(result), MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD, ierr)
            result = rbuff
            deallocate (rbuff)
         end if
#else
      end do ! bin
      !$OMP END DO
      !$OMP END PARALLEL
#endif
      deallocate (bins)

      ! Deallocate
      deallocate (SimY, SimDebt)

   END SUBROUTINE SaveDistPooled

!---------------------------------------------------------------------------------------------------------!
! Function to calculate ratio of people below to above a given threshold
!---------------------------------------------------------------------------------------------------------!

   SUBROUTINE SaveBunchRatio(minyr, maxyr, tsh, binw, price, ij_first, ij_last, minD, maxD, result)
      IMPLICIT NONE
      ! Parameters for bunching ratio calculation
      integer, intent(in) :: minyr, maxyr ! minimum and maximum years to consider
      real(dp), intent(in) :: tsh ! threshold for all years
      real(dp), intent(in) :: binw ! width of window below threshold
      real(dp), intent(in) :: price ! price level to convert from 2005 to years of interest
      real(dp), intent(in) :: minD, maxD ! minimum and maximum debt to consider
      integer, intent(in) :: ij_first, ij_last ! first and last ij to consider for bunching
      real(dp), intent(out) :: result
      ! Program variables
      integer :: ind, ij, year_ij
      real(dp) :: y, below, above

      ! Read in variables needed
      allocate (SimY(nsim, JWork), SimDebt(nsim, JWork))
      OPEN (UNIT=10, FORM='unformatted', FILE=trim(OutputDir)//'SimY')
      READ (10) SimY
      CLOSE (UNIT=10)
      OPEN (UNIT=10, FORM='unformatted', FILE=trim(OutputDir)//'SimDebt')
      READ (10) SimDebt
      CLOSE (UNIT=10)

      ! Loop to calculate number of people above and below
      below = 0d0
      above = 0d0
      do ind = 1, nsim
         do ij = ij_first, ij_last
            IF (ij >= FirstIJDeath(ind)) EXIT
            year_ij = ij - SimHELPSwitchIJ(ind) + 2005
            if (year_ij >= minyr .and. year_ij <= maxyr) then
               if (real(SimDebt(ind, ij), kind=dp) > minD .and. real(SimDebt(ind, ij), kind=dp) < maxD) then
                  ! Adjust income for numeraire and inflation
                  y = SimY(ind, ij)*numeraire*price
                  ! Count number of debtholders above and below
                  if (y > tsh - binw .and. y <= tsh) below = below + 1d0
                  if (y > tsh .and. y <= tsh + binw) above = above + 1d0
               end if
            end if
         end do
      end do

      ! Calculate ratio
      result = 0d0
      if (above > 0d0) result = below/above

      ! Deallocate
      deallocate (SimY, SimDebt)

   END SUBROUTINE SaveBunchRatio

!---------------------------------------------------------------------------------------------------------!
! Function to calculate quantile of debt distribution
!---------------------------------------------------------------------------------------------------------!

   SUBROUTINE CalcDebtQ(q, minyr, maxyr, ij_first, ij_last, result)
      IMPLICIT NONE
      ! Parameters
      real(dp), intent(in) :: q ! quantile to calculate
      integer, intent(in) :: minyr, maxyr ! minimum and maximum years to consider
      integer, intent(in) :: ij_first, ij_last ! first and last ij to consider for bunching
      real(dp), intent(out) :: result
      ! Program variables
      integer :: ind, ij, year_ij
      integer, allocatable :: include_mat(:, :), include_vec(:)
      real(dp), allocatable :: debt_vec(:)

      ! Read in variables needed
      allocate (SimDebt(nsim, JWork))
      OPEN (UNIT=10, FORM='unformatted', FILE=trim(OutputDir)//'SimDebt')
      READ (10) SimDebt
      CLOSE (UNIT=10)

      ! Loop to calculate people to include and reshape to vector
      allocate (include_mat(nsim, JWork))
      include_mat = 0
      do ind = 1, nsim
         do ij = ij_first, ij_last
            IF (ij >= FirstIJDeath(ind)) EXIT
            year_ij = ij - SimHELPSwitchIJ(ind) + 2005
            if (year_ij >= minyr .and. year_ij <= maxyr .and. SimDebt(ind, ij) > 0e0) include_mat(ind, ij) = 1
         end do
      end do
      allocate (include_vec(nsim*JWork))
      include_vec = reshape(include_mat, (/nsim*JWork/))
      deallocate (include_mat)

      ! Reshape debt to vector
      allocate (debt_vec(nsim*JWork))
      debt_vec = reshape(real(SimDebt, kind=dp), (/nsim*JWork/))
      deallocate (SimDebt)

      ! Calculate quantile
      result = quantile_array(nsim*JWork, debt_vec, q, include_vec)

      ! Deallocate
      deallocate (include_vec, debt_vec)

   END SUBROUTINE CalcDebtQ

!---------------------------------------------------------------------------------------------------------!
! Function to calculate within-individual panel moments around policy change
!---------------------------------------------------------------------------------------------------------!
   SUBROUTINE CalcPanelMoments(binw, ij_first, ij_last, result)

      IMPLICIT NONE
      ! Parameters for bunching ratio calculation
      real(dp), intent(in) :: binw ! width of window below thresholds
      integer, intent(in) :: ij_first, ij_last ! first and last ij to consider for bunching
      real(dp), intent(out), dimension(:) :: result
      ! Code variables
      real(dp) :: y04, y05
      integer :: ind, ij, year_ij
      real(dp) :: panel(1), counts(3) ! moments and counts for averages

      ! Read in variables needed
      allocate (SimY(nsim, JWork), SimDebt(nsim, JWork))
      OPEN (UNIT=10, FORM='unformatted', FILE=trim(OutputDir)//'SimY')
      READ (10) SimY
      CLOSE (UNIT=10)
      OPEN (UNIT=10, FORM='unformatted', FILE=trim(OutputDir)//'SimDebt')
      READ (10) SimDebt
      CLOSE (UNIT=10)

      ! Loop over people and specified ages
      panel = 0d0
      counts = 0d0
      do ind = 1, nsim
         do ij = ij_first, ij_last
            year_ij = ij - SimHELPSwitchIJ(ind) + 2005
            if (year_ij .eq. 2004) then
               IF (ij + 1 >= FirstIJDeath(ind)) EXIT
               if (SimDebt(ind, ij) > 0e0) then
                  ! 2004 income, adjusted for numeraire and inflation
                  y04 = SimY(ind, ij)*numeraire*price04
                  if (y04 > HELPTsh04(1) - binw .and. y04 <= HELPTsh04(1)) then ! bunching in 2004
                     counts(1) = counts(1) + 1d0
                     y05 = SimY(ind, ij + 1)*numeraire
                     if (SimDebt(ind, ij + 1) > 0e0) then ! still has debt in 2005
                        ! Determine if bunching in 2005
                        if (y05 > HELPTsh05(1) - binw .and. y05 <= HELPTsh05(1)) panel(1) = panel(1) + 1d0
                     end if
                  end if
               end if
            end if
         end do
      end do

      ! Compute averages and fractions
      result = 0d0
      if (counts(1) > 0d0) result(1) = panel(1)/counts(1)

      ! Deallocate
      deallocate (SimY, SimDebt)

   END SUBROUTINE CalcPanelMoments

!---------------------------------------------------------------------------------------------------------!
! Function to form model moments
!---------------------------------------------------------------------------------------------------------!
   SUBROUTINE FormModelMoments

      IMPLICIT NONE
      integer :: flag
      real(dp) :: debtQ1, debtQ4, rQ1, rQ4
      logical :: parmpi
      integer, allocatable :: yr_pre(:), yr_post(:)
#if defined(MPI)
      real(dp), allocatable :: rbuff(:)
      integer :: ierr, n_mpi
#endif

      ! Parameter controlling parallelization
      parmpi = mpiprocs >= 7

      ! Years for calculating income distribution
      allocate (yr_pre(3), yr_post(3))
      yr_pre = (/2002, 2003, 2004/)
      yr_post = (/2005, 2006, 2007/)

      ! Allocate
      allocate (ModelMoments(n_mnts, 1))
      ModelMoments = 0d0
      ! Income Process Moments
      CALL CalcIncomeMoments(ModelMoments(:13, 1), flag)
      if (flag .eq. 0) then
         ! Pooled Income Distributions in Pre and Post Periods
         CALL SaveDistPooled(yr_pre, HELPTsh04(1), (/22500d0, 28500d0/), 500d0, price04, &
                             2, JWork, ModelMoments(13 + 1:13 + 13, 1))
         CALL SaveDistPooled(yr_post, HELPTsh05(1), (/32500d0, 38500d0/), 500d0, 1d0, &
                             2, JWork, ModelMoments(13 + 14:13 + 26, 1))
         ! Hardcoded for SMMChoice = 8: + Ratio of Debt Moments - Capital Income
         IF ((parmpi .and. procid .eq. 1) .or. .not. parmpi) THEN
            CALL CalcDebtQ(0.25d0, 2005, 2018, 2, JWork, debtQ1)
            CALL SaveBunchRatio(2005, 2018, HELPTsh05(1), 500d0, 1d0, 2, JWork, 0d0, debtQ1, rQ1)
            CALL CalcDebtQ(0.75d0, 2005, 2018, 2, JWork, debtQ4)
            CALL SaveBunchRatio(2005, 2018, HELPTsh05(1), 500d0, 1d0, 2, JWork, debtQ4, 1d10, rQ4)
            ModelMoments(40, 1) = 0d0
            if (rQ1 > 0d0) ModelMoments(40, 1) = rQ4/rQ1
         END IF
         IF ((parmpi .and. procid .eq. 2) .or. .not. parmpi) THEN
            CALL HoursMoments(ModelMoments(41:43, 1))
         END IF
         IF ((parmpi .and. procid .eq. 3) .or. .not. parmpi) THEN
            CALL CalcPanelMoments(500d0, 3, JWork - 1, ModelMoments(44:, 1))
         END IF
#if defined(MPI)
         IF (parmpi) THEN
            n_mpi = 39
            allocate (rbuff(n_mnts - n_mpi))
            CALL MPI_ALLREDUCE(ModelMoments(n_mpi + 1:, 1), rbuff, n_mnts - n_mpi, &
                               MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD, ierr)
            ModelMoments(n_mpi + 1:, 1) = rbuff
            deallocate (rbuff)
         END IF
#endif
      else
         ModelMoments = 0d0
      end if

      deallocate (yr_pre, yr_post)

   END SUBROUTINE FormModelMoments

!---------------------------------------------------------------------------------------------------------!
! Function to calculate moment reweighting vector
!---------------------------------------------------------------------------------------------------------!
   SUBROUTINE ReweightingVector(out)
      IMPLICIT NONE
      REAL(dp), INTENT(OUT) :: out(n_mnts)
      INTEGER, PARAMETER :: n_gp1 = 6
      INTEGER, PARAMETER, DIMENSION(n_gp1) :: group1 = (/40, 44, 18, 19, 31, 32/)
      REAL(dp), PARAMETER :: nr_gp1 = REAL(n_gp1, KIND=dp)
      INTEGER :: iM
      REAL(dp) :: nr_mnts
      nr_mnts = REAL(n_mnts, KIND=dp)
      DO iM = 1, n_mnts
         IF (is_in_array(iM, group1)) THEN
            out(iM) = nr_mnts/nr_gp1/2d0
         ELSE
            out(iM) = nr_mnts/(nr_mnts - nr_gp1)/2d0
         END IF
      END DO
   END SUBROUTINE ReweightingVector

!---------------------------------------------------------------------------------------------------------!
! Function to populate weight matrix
!---------------------------------------------------------------------------------------------------------!
   SUBROUTINE FormWeightMatrix
      IMPLICIT NONE
      INTEGER :: iM
      REAL(dp), ALLOCATABLE, DIMENSION(:) :: ReweightVector
      allocate (WeightingMatrix(n_mnts, n_mnts))
      CALL ArcSinMatrix(n_mnts, ModelMoments, DataMoments, WeightingMatrix)
      WeightingMatrix = WeightingMatrix*100d0 ! scaling for accuracy
      ALLOCATE (ReweightVector(n_mnts))
      CALL ReweightingVector(ReweightVector)
      DO iM = 1, n_mnts
         WeightingMatrix(iM, iM) = WeightingMatrix(iM, iM)*ReweightVector(iM)
      END DO
      DEALLOCATE (ReweightVector)
   END SUBROUTINE FormWeightMatrix

!---------------------------------------------------------------------------------------------------------!
! SMM function to be optimized - must be called fcn for simulated annealing
!---------------------------------------------------------------------------------------------------------!
   SUBROUTINE fcn(n, p, out, dealloc)

      implicit none

      ! Function arguments
      integer, intent(in) :: n ! number of parameters
      real(dp), intent(in) :: p(n) ! parameters
      real(dp), intent(out) :: out ! function output
      logical, intent(in), optional :: dealloc ! whether to not deallocate ModelMoments and WeightingMatrix

      ! Storage
      real(dp), dimension(1, n + n_mnts) :: MomOutput
      real(dp), dimension(1, n + 1) :: SMMOutput
      character(len=15) :: lstring1, lstring2
      integer :: i

      ! Parameters
      CALL UnpackParams(p)

      ! Check parameter evaluation boundaries
      do i = 1, n_pms
         if (p(i) > ub_e(i) .or. p(i) < lb_e(i)) then
            out = positiveInf
            return
         end if
      end do

      ! Check model specification
      if ((fcH <= fcL) .or. & ! upper adjustment cost not greater than lower
          (FixLabor .eq. 1 .and. (cprob .ne. 0d0 .or. fcL .ne. 0d0 .or. fcH .ne. 0d0)) & ! no frictions if fixing labor
          ) then
         out = positiveInf
         return
      end if

      ! Solve and simulate from model
      CALL CreateWageGrids
      CALL ValueFunction
      CALL LCSimulation(1)
      CALL CleanSolution

      if (SMMError > 0) then
         allocate (ModelMoments(n_mnts, 1))
         ModelMoments = -99d0
         allocate (WeightingMatrix(n_mnts, n_mnts))
         out = 999999d0
      else
         ! Calculate model moments
         CALL FormModelMoments
         CALL FormWeightMatrix
         ! Calculate SMM objective function
         out = QuadraticForm(DataMoments, ModelMoments, WeightingMatrix)
         out = out/100d0 ! undo scaling for accuracy
      end if

      ! Organize outputs
      SMMOutput(1, :n) = p
      SMMOutput(1, n + 1) = out
      MomOutput(1, :n) = p
      MomOutput(1, n + 1:) = ModelMoments(:, 1)

      ! Append outputs to files
      if (procid .eq. 0) then
         write (UNIT=lstring1, FMT='(I5)') size(SMMOutput, 2)
         write (UNIT=lstring2, FMT='(I5)') size(MomOutput, 2)
         write (331, '('//trim(lstring1)//'F24.15)') (SMMOutput(1, :))
         write (332, '('//trim(lstring2)//'F24.15)') (MomOutput(1, :))
      end if

      ! Clean
      CALL CleanSimulation
      if (present(dealloc)) then
         if (dealloc) deallocate (ModelMoments, WeightingMatrix)
      else
         deallocate (ModelMoments, WeightingMatrix)
      end if

   END SUBROUTINE fcn

   ! SMM Objective function to be maximized if you use Nelder-Mead or TikTak
   real(dp) function f_fcn(p)
      real(dp), intent(in) :: p(n_pms)
      CALL fcn(n_pms, p, f_fcn, .TRUE.)
   end function f_fcn

   ! SMM Objective function to be called when calculating standard errors
   real(dp) function f_fcn_se(p)
      real(dp), intent(in) :: p(n_pms)
      CALL fcn(n_pms, p, f_fcn_se, .FALSE.)
   end function f_fcn_se

END MODULE SMMFunctions
