MODULE Setup

#if defined(MPI)
   USE mpi
#endif

   USE Parameters, only: dp, procid, InputDir, OutputDir, RunMode, &
                         JTot, JRet, JWork, FirstAge, FirstCohort, LastCohort, n_cohorts, &
                         FirstIJDeath, NbrPolicy, NbrWLiqW, NbrWLiqW_B, NbrRLiqW, NbrDebt, &
                         NbrTheta, NbrPastL, NbrNu_GH, NbrEps_GH, numeraire, AProfile, &
                         deltas, deltaEs, sigmai, sigmanu, sigmaeps, floorC, wedgeB, wedgeB0, &
                         constraintB, gd_age, survival, cum_survival, eqscale, gd_Debt, &
                         gdW_LiqW, gdR_LiqW, gd_PastL, gd_Theta, gd_Wage, gd_ExpTheta, &
                         GHw_Nu, GHx_Nu, GHw_Eps, GHx_Eps, gdcurve_LiqWealth, gdcurve_Debt, &
                         gdcurve_Theta, gdsigma_Theta, nsim, ind_E2, realind_E2, &
                         SimHELPSwitchIJ, R_d, SimRawNu, SimRawEps, SimE, SimPolicyI, &
                         SimDebt0, SimAssets0, SimCalvo, SimLerr, mu_d, sigma_d, maxdebt0, &
                         nbins_s0, bins_s0, smoothtax
   USE types
   USE EconFunctions, only: RA, CalcWage
   USE Procedures, only: discretize_AR, PowerSpacedGrid, DoublePowerSpacedGrid, &
                         gauher_normal, RandomDiscrete, RandomDiscrete1, WriteMatrix1d_real, &
                         WriteMatrix2d_real, WriteMatrix2d_realdp, quantile_array, interp1d
   USE random, only: random_normal

   IMPLICIT NONE

CONTAINS

   real(dp) function i_dp(int)
      IMPLICIT NONE
      integer, intent(in) :: int
      i_dp = real(int, kind=dp)
   end function i_dp

!---------------------------------------------------------------------------------------------------------!
! Allocate and setup preliminaries that are parameter invariant
!---------------------------------------------------------------------------------------------------------!

   SUBROUTINE Preliminaries

      INTEGER :: ij
      REAL(dp) :: junk, mortality(2, 101), us_eqscale(69, 2)

      ! Allocate wage and income grid parameters
      allocate (gd_Theta(NbrTheta), gd_ExpTheta(NbrTheta))
      allocate (gd_Wage(NbrEps_GH, NbrTheta, JWork, 2))

      ! Allocate arrays that store simulation results of exogeneous processes
      allocate (SimE(nsim))
      allocate (SimDebt0(nsim), SimAssets0(nsim))

      ! Read in parameters for initial debt distribution
      OPEN (98, File=InputDir//'debtmax_distribution.txt')
      READ (98, *) junk
      READ (98, *) mu_d
      READ (98, *) sigma_d
      READ (98, *) maxdebt0
      CLOSE (98)

      ! Create age grid
      do ij = 1, JTot
         gd_age(ij) = i_dp(FirstAge) + i_dp(ij - 1)
      end do

      ! Construct survival probability and cumulative survival probability grids
      OPEN (98, File=InputDir//'mortality.txt')
      READ (98, *) mortality
      CLOSE (98)
      survival = 0d0
      DO ij = 1, JTot - 2 ! skip last two period because in ij=JTot-1 is last period of life
         survival(ij) = 1d0 - mortality(2, FirstAge + ij)
      END DO
      cum_survival = 0d0
      cum_survival(1) = 1d0
      DO ij = 2, JTot - 1
         cum_survival(ij) = cum_survival(ij - 1)*survival(ij - 1)
      END DO
      CALL WriteMatrix1d_real(trim(OutputDir)//'cum_survival.txt', 31, 0, JTot, cum_survival)

      ! Create consumption equivalence scale
      OPEN (98, FILE=InputDir//'HILDA_equivscale.txt')
      DO ij = 1, SIZE(us_eqscale, 1)
         READ (98, *) us_eqscale(ij, 1), us_eqscale(ij, 2)
      END DO
      CLOSE (98)
      DO ij = 1, JTot
         eqscale(ij) = interp1d(0, 0d0, SIZE(us_eqscale, 1), us_eqscale(:, 1), us_eqscale(:, 2), gd_age(ij))
      END DO
      eqscale = eqscale/SUM(eqscale)*JTot ! normalize so 1 on average

      ! Read in smoothed tax function
      OPEN (98, FILE=InputDir//'smoothed_tax.txt')
      READ (98, *) smoothtax(1)
      READ (98, *) smoothtax(2)
      CLOSE (98)

   END SUBROUTINE Preliminaries

!---------------------------------------------------------------------------------------------------------!
! Routine to setup borrowing constraint and borrowing rate, which must be called after creating age grid
!---------------------------------------------------------------------------------------------------------!
   SUBROUTINE SetBorrowing(on, brate, bcscale)

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: on
      REAL(dp), INTENT(IN), OPTIONAL :: brate, bcscale

      REAL(dp) :: bmodel(5)
      INTEGER :: ij

      IF (on .eq. 0) THEN
         constraintB = 0d0
         wedgeB = 0d0
      ELSE
         ! Calculate borrowing constraint in each period using model fitted from HILDA data
         OPEN (98, File=InputDir//'cc_limit_agemodel.txt')
         READ (98, *) bmodel
         CLOSE (98)
         DO ij = 1, JTot - 1
            constraintB(ij) = bmodel(1) + bmodel(2)*gd_age(ij) + bmodel(3)*(gd_age(ij)**2d0) + &
                              bmodel(4)*(gd_age(ij)**3d0) + bmodel(5)*(gd_age(ij)**4d0)
         END DO
         constraintB(JTot) = 0d0
         constraintB = constraintB/numeraire ! adjust for numeraire
         IF (present(brate)) THEN
            wedgeB = brate
         ELSE
            wedgeB = wedgeB0
         END IF
         IF (present(bcscale)) constraintB = constraintB*bcscale
      END IF
      CALL WriteMatrix1d_real(trim(OutputDir)//'constraintB.txt', 31, 0, JTot, constraintB)

   END SUBROUTINE SetBorrowing

!---------------------------------------------------------------------------------------------------------!
! Create grids for state variables that are parameter invariant
!---------------------------------------------------------------------------------------------------------!

   SUBROUTINE CreateGrids

      IMPLICIT NONE

      INTEGER :: ij

      ! Grid mins and maxes
      real(dp) :: min_LiqW, max_LiqW
      real(dp) :: min_Debt, max_Debt(JTot)
      real(dp) :: min_PastL, max_PastL, mean_PastL

      ! Grids for liquid wealth holdings (in units of numeraire)
      max_LiqW = 100d0
      do ij = 1, JTot
         if (ij < JWork) then
            min_LiqW = constraintB(ij)*RA(constraintB(ij))
         else
            if (ij < JTot) then
               min_LiqW = constraintB(JWork)*RA(constraintB(JWork))*i_dp(JRet - 1 - (ij - JWork))/i_dp(JRet - 1)
            else
               min_LiqW = 0d0
            end if
         end if
         if (ij <= JWork) then
            if (min_LiqW < 0d0 .and. NbrWLiqW_B > 0) then
               CALL PowerSpacedGrid(NbrWLiqW_B, 1d0, min_LiqW, min_LiqW/i_dp(NbrWLiqW_B), gdW_LiqW(1:NbrWLiqW_B, ij))
               CALL PowerSpacedGrid(NbrWLiqW - NbrWLiqW_B, gdcurve_LiqWealth, 0d0, max_LiqW, gdW_LiqW(NbrWLiqW_B + 1:NbrWLiqW, ij))
            else
               CALL PowerSpacedGrid(NbrWLiqW, gdcurve_LiqWealth, min_LiqW, max_LiqW, gdW_LiqW(:, ij))
            end if
         else
            CALL PowerSpacedGrid(NbrRLiqW, gdcurve_LiqWealth, min_LiqW, max_LiqW, gdR_LiqW(:, ij - JWork))
         end if
      end do

      ! Grid for debt level (in units of numeraire)
      min_Debt = 0d0
      do ij = 1, JWork
         if (ij == 1) then
            max_Debt(ij) = maxdebt0/numeraire
         else
            max_Debt(ij) = max_Debt(ij - 1)*R_d
         end if
         IF (max_Debt(ij) <= min_Debt) THEN
            max_Debt(ij) = max_Debt(ij) + 0.1d0
            IF (procid .eq. 0) PRINT *, 'WARNING: Had to manually adjust max_Debt for ij = ', ij
         END IF
         CALL PowerSpacedGrid(NbrDebt, gdcurve_Debt, min_Debt, max_Debt(ij), gd_Debt(:, ij))
      end do

      ! Grid for past labor supply
      min_PastL = 0d0
      max_PastL = 2d0
      mean_PastL = 0.5d0*(min_PastL + max_PastL)
      CALL DoublePowerSpacedGrid(NbrPastL, 0.5d0, min_PastL - mean_PastL, max_PastL - mean_PastL, &
                                 gd_PastL, floor(i_dp(NbrPastL)/2d0))
      gd_PastL = gd_PastL + mean_PastL

   END SUBROUTINE CreateGrids

!---------------------------------------------------------------------------------------------------------!
! Simulate underlying shocks that are parameter invariant
!---------------------------------------------------------------------------------------------------------!

   SUBROUTINE AllocateShocks
      IMPLICIT NONE
      allocate (SimRawNu(nsim, JWork))
      allocate (SimRawEps, SimCalvo, mold=SimRawNu)
   END SUBROUTINE AllocateShocks

   SUBROUTINE DeallocShocks
      IMPLICIT NONE
      deallocate (SimRawNu, SimRawEps, SimCalvo)
   END SUBROUTINE DeallocShocks

   SUBROUTINE ReadShocks
      IMPLICIT NONE
      CALL AllocateShocks
      OPEN (1353, FORM='unformatted', FILE=trim(OutputDir)//'shocks')
      READ (1353) SimRawNu, SimRawEps, SimCalvo
      CLOSE (1353)
   END SUBROUTINE ReadShocks

   SUBROUTINE SimulateShocks

      IMPLICIT NONE

      integer :: seed_size, ind, ij, pos, alive
      integer, allocatable :: seed(:)
      real(dp) :: dist_c(n_cohorts)
      real(dp) :: p_a0(2), mu_a0(2), sigma_a0(2), max_a0(2)
#if defined(MPI)
      integer :: ierr
#endif

      ! Set random number seed, which will ensure all the random shocks drawn below are seeded
      CALL RANDOM_SEED(size=seed_size)
      allocate (seed(seed_size))
      seed = 123456789
      CALL RANDOM_SEED(put=seed)
      deallocate (seed)

      ! Simulate and write out underlying shocks for nu, epsilon, and Calvo probability
      CALL AllocateShocks
      SimRawNu = 0d0
      SimRawEps = 0d0
      DO ij = 1, JWork
         DO ind = 1, nsim
            SimRawEps(ind, ij) = random_normal()
            SimRawNu(ind, ij) = random_normal()
         END DO
      END DO
      CALL random_number(SimCalvo)
      IF (procid .eq. 0) THEN
         OPEN (1353, FORM='unformatted', FILE=trim(OutputDir)//'shocks')
         WRITE (1353) SimRawNu, SimRawEps, SimCalvo
         CLOSE (1353)
      END IF
      CALL DeallocShocks
#if defined(MPI)
      CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)
#endif

      ! Assign education level for each agent: 1 = no education, 2 = education
      SELECT CASE (RunMode)
      CASE (0, 1, 2)
         SimE = 1
         SimE(ind_E2:) = 2
      CASE (4, 8)
         SimE = 2
      END SELECT

      ! Simulate initial debt from lognormal distribution
      SimDebt0 = 0d0
      do ind = 1, nsim
         if (SimE(ind) > 1) SimDebt0(ind) = MIN(EXP(mu_d + random_normal()*sigma_d), maxdebt0)
      end do
      SimDebt0 = SimDebt0/numeraire ! adjust for numeraire

      ! Simulate initial assets from lognormal distribution that depends on whether individual has debt or not
      OPEN (98, File=InputDir//'initial_assets_nd.txt')
      READ (98, *) p_a0(1)
      READ (98, *) mu_a0(1)
      READ (98, *) sigma_a0(1)
      READ (98, *) max_a0(1)
      CLOSE (98)
      OPEN (98, File=InputDir//'initial_assets_d.txt')
      READ (98, *) p_a0(2)
      READ (98, *) mu_a0(2)
      READ (98, *) sigma_a0(2)
      READ (98, *) max_a0(2)
      CLOSE (98)
      SimAssets0 = 0d0
      do ind = 1, nsim
         CALL RandomDiscrete1(pos, 2, (/p_a0(SimE(ind)), 1d0 - p_a0(SimE(ind))/))
         if (pos .eq. 2) SimAssets0(ind) = EXP(MIN(mu_a0(SimE(ind)) + random_normal()*sigma_a0(SimE(ind)), max_a0(SimE(ind))))
      end do
      SimAssets0 = SimAssets0/numeraire ! adjust for numeraire

      ! Simulate first year in which individuals are dead
      allocate (FirstIJDeath, mold=SimE)
      FirstIJDeath = JTot
      DO ind = 1, nsim
         loop_age: DO ij = 2, JTot - 1
            CALL RandomDiscrete1(alive, 2, (/1d0 - survival(ij - 1), survival(ij - 1)/))
            IF (alive .eq. 1) THEN
               FirstIJDeath(ind) = ij
               EXIT loop_age
            END IF
         END DO loop_age
      END DO

      ! Simulate ij in which people switch from 2004 to 2005 HELP at a random date during working life, which is
      ! done by first simulating birth cohorts based on data and then calculating implied ij in 2005
      allocate (SimHELPSwitchIJ, mold=SimE)
      OPEN (98, File=InputDir//'birth_cohorts.txt')
      do ij = 1, n_cohorts
         READ (98, *) dist_c(ij)
      end do
      CLOSE (98)
      CALL RandomDiscrete(nsim, SimHELPSwitchIJ, n_cohorts, dist_c)
      SimHELPSwitchIJ = FirstCohort + SimHELPSwitchIJ - 1 ! cohort between FirstCohort and LastCohort
      SimHELPSwitchIJ = -(SimHELPSwitchIJ - 2005) + 1 ! ij in which people switch from 2004 to 2005 HELP
      SELECT CASE (RunMode)
      CASE (4, 8)
         OPEN (1354, FORM='unformatted', FILE=trim(OutputDir)//'SimHELPSwitchIJ')
         WRITE (1354) SimHELPSwitchIJ
         CLOSE (1354)
         deallocate (SimHELPSwitchIJ)
      END SELECT

      ! Calculate quantiles of initial continuous states
      SELECT CASE (RunMode)
      CASE (8)
         bins_s0(1, 1) = MINVAL(SimAssets0)
         DO ij = 1, nbins_s0 - 1
            bins_s0(1, 1 + ij) = quantile_array(nsim, SimAssets0, i_dp(ij)/i_dp(nbins_s0))
         END DO
         bins_s0(1, nbins_s0 + 1) = MAXVAL(SimAssets0)
         CALL ReadShocks
         bins_s0(2, 1) = MINVAL(SimRawNu(:, 1))
         DO ij = 1, nbins_s0 - 1
            bins_s0(2, 1 + ij) = quantile_array(nsim, SimRawNu(:, 1), i_dp(ij)/i_dp(nbins_s0))
         END DO
         bins_s0(2, nbins_s0 + 1) = MAXVAL(SimRawNu(:, 1))
         CALL DeallocShocks
         bins_s0(3, 1) = MINVAL(SimDebt0)
         DO ij = 1, nbins_s0 - 1
            bins_s0(3, 1 + ij) = quantile_array(nsim, SimDebt0, i_dp(ij)/i_dp(nbins_s0), SimE - 1)
         END DO
         bins_s0(3, nbins_s0 + 1) = MAXVAL(SimDebt0)
         CALL WriteMatrix2d_realdp(trim(OutputDir)//'bins_s0.txt', 31, 0, &
                                   SIZE(bins_s0, 1), SIZE(bins_s0, 2), bins_s0)
      END SELECT

      ! Simulate measurement error shocks
      allocate (SimLerr(nsim, JWork))
      SimLerr = 0d0
      DO ind = 1, nsim
         DO ij = 1, JWork
            SimLerr(ind, ij) = random_normal()
         END DO
      END DO
      IF (procid .eq. 0) THEN
         OPEN (1353, FORM='unformatted', FILE=trim(OutputDir)//'SimLerr')
         WRITE (1353) SimLerr
         CLOSE (1353)
      END IF
      deallocate (SimLerr)

   END SUBROUTINE SimulateShocks

!---------------------------------------------------------------------------------------------------------!
! Create grids of wages, which are depend on the parameters of the wage process
!---------------------------------------------------------------------------------------------------------!

   SUBROUTINE CreateWageGrids

      IMPLICIT NONE

      INTEGER :: ed, ij, iz, ie ! loops
      real(dp) :: theta_bound
      type(state_t) :: s

      ! Age profile
      DO ij = 1, JWork
         AProfile(ij, 1) = deltas(1) + deltas(2)*(gd_age(ij)) + deltas(3)*((gd_age(ij))**2d0)
         AProfile(ij, 2) = AProfile(ij, 1) + deltaEs(1) + deltaEs(2)*(gd_age(ij))
      END DO

      ! Normalize by numeraire
      AProfile = AProfile - log(numeraire)

      ! Calculate theta grid
      theta_bound = gdsigma_Theta*SQRT((sigmai**2d0) + (sigmanu**2d0))
      CALL DoublePowerSpacedGrid(NbrTheta, gdcurve_Theta, -theta_bound, theta_bound, gd_Theta, &
                                 floor(i_dp(NbrTheta)/2d0))
      DO iz = 1, NbrTheta
         gd_ExpTheta(iz) = exp(gd_Theta(iz))
      END DO

      ! Calculate Gauss-Hermite weights and nodes, adjusted for normal
      CALL gauher_normal(0d0, sigmanu, GHx_Nu, GHw_Nu)
      CALL gauher_normal(0d0, sigmaeps, GHx_Eps, GHw_Eps)

      ! Create wage rate grid for all methods, using age profile and grid for theta
      DO ed = 1, 2
         DO ij = 1, JWork
            DO iz = 1, NbrTheta
               DO ie = 1, NbrEps_GH
                  s%ed = ed
                  s%ij = ij
                  s%theta = gd_Theta(iz)
                  s%epsln = GHx_Eps(ie)
                  gd_Wage(ie, iz, ij, ed) = CalcWage(s)
               END DO
            END DO
         END DO
      END DO

   END SUBROUTINE CreateWageGrids

END MODULE Setup
