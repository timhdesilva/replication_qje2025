MODULE EconFunctions

   USE Parameters, only: dp, negativeInf, epsl, procid, beta, gamma, invEIS, phi, cprob, fcL, fcH, kappa, &
                         constraintB, wedgeB, floorC, survival, eqscale, FixLabor, JTot, JWork, AProfile, &
                         numeraire, DebtPayoff, Policy, R, HELPTsh04, HELPTsh05, R_d, price04, &
                         smoothtax, RunMode, c_gain, mu_d, sigma_d, ForceFixedPayoff, OptimalP, OptimalType

   USE types

   IMPLICIT NONE

CONTAINS

!---------------------------------------------------------------------------------------------------------!
! Preferences
!---------------------------------------------------------------------------------------------------------!

   ! Disutility of labor
   real(dp) function disutilityL(L)
      IMPLICIT NONE
      real(dp), intent(in) :: L
      disutilityL = (1d1**kappa)/(1d0 + 1d0/phi)*(L**(1d0 + 1d0/phi))
   end function disutilityL

   ! Function to determine relevant adjustment cost based on Calvo shock
   real(dp) function fixedCost(s)
      IMPLICIT NONE
      type(state_t), intent(in) :: s
      if (s%ij > 1 .and. s%ij <= JWork) then
         if (s%calvo <= cprob) then
            fixedCost = fcL
         else
            fixedCost = fcH
         end if
      else
         fixedCost = 0d0
      end if
   end function fixedCost

   ! Flow utility
   real(dp) function flowU(C, L, s)
      IMPLICIT NONE
      real(dp), intent(in) :: C, L
      type(state_t), intent(in) :: s
      real(dp) :: Cadj
      ! Intializations
      flowU = 0d0
      Cadj = C/eqscale(s%ij)
      SELECT CASE (RunMode)
      CASE (4, 8)
         if (ABS(c_gain) > epsl) Cadj = (1d0 + c_gain)*Cadj
      END SELECT
      ! Adjust preferences for Labor supply
      if (FixLabor .ne. 1) then
         Cadj = Cadj - disutilityL(L)
         if (ABS(L - s%Plabor) > epsl) Cadj = Cadj - fixedCost(s)
      end if
      ! If non-positive consumption, exit (can't happen with FixLabor .eq. 1)
      if (Cadj <= 0d0) then
         flowU = negativeInf
         return
      end if
      ! Add in consumption utility and make sure it satisfies bound
      flowU = MAX(flowU + Cadj, negativeInf)
   end function flowU

   ! Intertemporal aggregator
   real(dp) function AggUV(Ut, EVt1, s)
      IMPLICIT NONE
      real(dp), intent(in) :: Ut, EVt1
      type(state_t), intent(in) :: s
      real(dp) :: invEIS1, gamma1, beta1, CEQ
      beta1 = 1d0 - beta
      invEIS1 = 1d0 - invEIS
      gamma1 = 1d0 - gamma
      ! Calculate certainty equivalent, taking care with case when EV_{t+1} = 0 in terminal condition
      if (EVt1 .ne. 0d0) then
         CEQ = (survival(s%ij)*EVt1)**(invEIS1/gamma1)
      else
         CEQ = 0d0
      end if
      ! Calculate value function
      AggUV = (beta1*(Ut**invEIS1)*eqscale(s%ij) + beta*CEQ)**(1d0/invEIS1)
   end function AggUV

   ! Certainty equivalence functional that takes input vector of values
   real(dp) function CEF(Vt)
      IMPLICIT NONE
      real(dp), intent(in) :: Vt(:)
      real(dp) :: curv1, Nmean
      curv1 = 1d0 - gamma
      Nmean = real(SIZE(Vt), kind=dp)
      IF (.NOT. MINVAL(Vt) >= 0d0) THEN
         CEF = -99d0
         IF (procid .eq. 0) PRINT *, 'WARNING in CEF: Vt contains negative values!'
      ELSE
         CEF = SUM(Vt**curv1)/Nmean
         CEF = CEF**(1d0/curv1)
      END IF
   end function CEF

!---------------------------------------------------------------------------------------------------------!
! Environment functions
!---------------------------------------------------------------------------------------------------------!

   ! Lower bound on assets at t+1 depending on current state at t
   real(dp) function lowerA(s)
      IMPLICIT NONE
      type(state_t), intent(in) :: s
      lowerA = constraintB(s%ij + 1)
   end function lowerA

   ! Return on assets
   real(dp) function RA(savings)
      IMPLICIT NONE
      real(dp), intent(in) :: savings
      if (savings >= 0d0) then
         RA = R
      else
         RA = R + wedgeB
      end if
   end function RA

   ! HELP Repayment Thresholds
   real(dp) function HELPRate(income_t, pcy)
      IMPLICIT NONE
      real(dp), intent(in) :: income_t
      integer, intent(in) :: pcy
      real(dp) :: rincome
      ! 2003-04
      if (pcy .eq. 2004) then
         rincome = price04*income_t*numeraire
         if (rincome <= HELPTsh04(1)) then
            HELPRate = 0d0
         else if (rincome <= HELPTsh04(2)) then
            HELPRate = 0.03d0
         else if (rincome <= HELPTsh04(3)) then
            HELPRate = 0.035d0
         else if (rincome <= HELPTsh04(4)) then
            HELPRate = 0.04d0
         else if (rincome <= HELPTsh04(5)) then
            HELPRate = 0.045d0
         else if (rincome <= HELPTsh04(6)) then
            HELPRate = 0.05d0
         else if (rincome <= HELPTsh04(7)) then
            HELPRate = 0.055d0
         else
            HELPRate = 0.06d0
         end if
         ! 2004-05
      else if (pcy .eq. 2005) then
         rincome = income_t*numeraire
         if (rincome <= HELPTsh05(1)) then
            HELPRate = 0d0
         else if (rincome <= HELPTsh05(2)) then
            HELPRate = 0.04d0
         else if (rincome <= HELPTsh05(3)) then
            HELPRate = 0.045d0
         else if (rincome <= HELPTsh05(4)) then
            HELPRate = 0.05d0
         else if (rincome <= HELPTsh05(5)) then
            HELPRate = 0.055d0
         else if (rincome <= HELPTsh05(6)) then
            HELPRate = 0.06d0
         else if (rincome <= HELPTsh05(7)) then
            HELPRate = 0.065d0
         else if (rincome <= HELPTsh05(8)) then
            HELPRate = 0.07d0
         else if (rincome <= HELPTsh05(9)) then
            HELPRate = 0.075d0
         else
            HELPRate = 0.08d0
         end if
      else
         HELPRate = 0d0
      end if
   end function HELPRate

   ! HELP repayment function
   real(dp) function HELPPayment(income_t, debt_t, pcy)
      IMPLICIT NONE
      real(dp), intent(in) :: income_t, debt_t
      integer, intent(in) :: pcy
      HELPPayment = HELPRate(income_t, pcy)*income_t
      if (DebtPayoff .eq. 1) HELPPayment = MIN(HELPPayment, R_d*debt_t)
   end function HELPPayment

   ! US fixed repayment function
   real(dp) function US_Fixed(ij_t, debt_t, Ts, Tp)
      IMPLICIT NONE
      integer, intent(in) :: ij_t
      real(dp), intent(in) :: debt_t
      integer, intent(in) :: Ts, Tp ! period of first and final debt repayment
      real(dp) :: irate
      if (ij_t < Ts) then
         US_Fixed = 0d0
      else if (ij_t < Tp) then
         if (ABS(R_d - 1d0) < 1d-5) then ! lower bound on net rate
            irate = 1d-5
         else
            irate = R_d - 1d0
         end if
         US_Fixed = irate/(1d0 - (1d0 + irate)**(-real(Tp - ij_t + 1, kind=dp)))*debt_t
      else if (ij_t >= Tp) then ! in principle above line gives same, but do this to ensure debt paid off exactly
         US_Fixed = R_d*debt_t
      end if
   end function US_Fixed

   ! US IBR payment function
   real(dp) function US_IBR(ij_t, income_t, debt_t, rate, pov_frac, forgiveT, cap_fixed)
      IMPLICIT NONE
      integer, intent(in) :: ij_t, forgiveT
      real(dp), intent(in) :: income_t, debt_t, rate, pov_frac
      logical, intent(in) :: cap_fixed
      real(dp), parameter :: pov = 14580d0*0.65d0*1.3d0/numeraire
      if (ij_t > forgiveT) then
         US_IBR = 0d0
      else
         US_IBR = rate*MAX(income_t - pov_frac*pov, 0d0)
         if (DebtPayoff .eq. 1) then
            if (cap_fixed) US_IBR = MIN(US_IBR, US_Fixed(ij_t, debt_t, 1, 25)) ! cap at 25-year fixed repayment
            US_IBR = MIN(US_IBR, R_d*debt_t)
         end if
      end if
   end function US_IBR

   ! Custom IBR function
   real(dp) function Custom_IBR(ij_t, income_t, debt_t, rate, incK, forgiveT)
      IMPLICIT NONE
      integer, intent(in) :: ij_t, forgiveT
      real(dp), intent(in) :: income_t, debt_t, rate, incK
      if (ij_t > forgiveT) then
         Custom_IBR = 0d0
      else
         Custom_IBR = rate*MAX(income_t - incK, 0d0)
         if (DebtPayoff .eq. 1) Custom_IBR = MIN(Custom_IBR, R_d*debt_t)
      end if
   end function Custom_IBR

   ! ISA Repayment function
   real(dp) function ISA(ij_t, ed, income_t, debt_t, rate, Tcap, thresh, cap)
      IMPLICIT NONE
      integer, intent(in) :: ij_t, ed, Tcap
      real(dp), intent(in) :: income_t, debt_t, rate
      logical, intent(in) :: thresh, cap
      real(dp), parameter :: thresh_val = 20000d0*0.79d0*1.3d0/numeraire
      real(dp), parameter :: cap_val = 2.5d0
      real(dp) :: meanD0
      meanD0 = EXP(mu_d + 0.5d0*(sigma_d**2d0))/numeraire
      if (ij_t > Tcap .or. ed .ne. 2 .or. &
          (income_t < thresh_val .and. thresh) .or. &
          (debt_t < meanD0*(1d0 - cap_val) .and. cap)) then
         ISA = 0d0
      else
         ISA = rate*income_t
      end if
   end function ISA

   ! Debt repayment functions for optimal policy
   real(dp) function DOptimal_Y(L, s)
      IMPLICIT NONE
      real(dp), intent(in) :: L
      type(state_t), intent(in) :: s
      real(dp) :: ij_dp, income_t
      real(dp) :: psi, K
      ! No payments for uneducated individuals
      IF (s%ed .eq. 1) THEN
         DOptimal_Y = 0d0
         RETURN
      END IF
      ! Calculation of variables for repayment formulas
      ij_dp = REAL(s%ij, kind=dp)
      income_t = HELPIncome(L, s)
      ! Calculation of repayment
      SELECT CASE (OptimalType)
      CASE DEFAULT
         STOP 'INVALID SPECIFICATION OF OptimalType!'
      CASE (1)
         DOptimal_Y = 0d0
      CASE (2, 3, 11)
         psi = OptimalP(1)
         K = OptimalP(2)
         DOptimal_Y = MIN(psi*MAX(income_t - K, 0d0), R_d*s%debt)
         IF (s%ij > JWork .AND. OptimalType .EQ. 2) DOptimal_Y = 0d0
         IF (s%ij > 20 .AND. OptimalType .EQ. 3) DOptimal_Y = 0d0
         IF (s%ij > JWork .AND. OptimalType .EQ. 11) DOptimal_Y = 0d0
      CASE (4, 6)
         psi = OptimalP(1)
         DOptimal_Y = psi*income_t
         IF (s%ij > 9 .AND. OptimalType .EQ. 4) DOptimal_Y = 0d0
         IF (s%ij > JWork .AND. OptimalType .EQ. 6) DOptimal_Y = 0d0
      CASE (5, 7)
         psi = OptimalP(1)
         K = OptimalP(2)
         DOptimal_Y = psi*MAX(income_t - K, 0d0)
         IF (s%ij > 9 .AND. OptimalType .EQ. 5) DOptimal_Y = 0d0
         IF (s%ij > JWork .AND. OptimalType .EQ. 7) DOptimal_Y = 0d0
      CASE (8)
         psi = OptimalP(1)
         K = OptimalP(2)
         DOptimal_Y = MIN(psi*MAX(income_t, 0d0), R_d*s%debt)
         IF (s%ij > JWork .or. income_t <= K) DOptimal_Y = 0d0
      CASE (9)
         DOptimal_Y = US_Fixed(s%ij, s%debt, 1, 25)
         IF (income_t < 16863d0/numeraire) DOptimal_Y = 0d0
         DOptimal_Y = MIN(DOptimal_Y, CashPreDebt(L, s))
      CASE (10)
         DOptimal_Y = MIN(OptimalP(1), R_d*s%debt)
         IF (income_t < 16863d0/numeraire) DOptimal_Y = 0d0
         IF (s%ij > JWork) DOptimal_Y = 0d0
      CASE (12)
         DOptimal_Y = MAX(OptimalP(1) + OptimalP(2)*income_t + OptimalP(3)*(income_t**2d0), 0d0)
         DOptimal_Y = MIN(DOptimal_Y, R_d*s%debt)
         IF (s%ij > JWork) DOptimal_Y = 0d0
      CASE (13)
         DOptimal_Y = MAX(OptimalP(1) + OptimalP(2)*income_t + OptimalP(3)*(income_t**2d0) + OptimalP(4)*ij_dp, 0d0)
         DOptimal_Y = MIN(DOptimal_Y, R_d*s%debt)
         IF (s%ij > JWork) DOptimal_Y = 0d0
      CASE (14)
         DOptimal_Y = MAX(OptimalP(1) + OptimalP(2)*income_t + OptimalP(3)*(income_t**2d0) + OptimalP(4)*s%debt, 0d0)
         DOptimal_Y = MIN(DOptimal_Y, R_d*s%debt)
         IF (s%ij > JWork) DOptimal_Y = 0d0
      END SELECT
   end function DOptimal_Y

   ! Calculate after-tax income using ATO 2003-2004 schedule
   real(dp) function PostTaxY(income_t, pcy)
      IMPLICIT NONE
      real(dp), intent(in) :: income_t
      integer, intent(in) :: pcy
      real(dp) :: rincome
      if (income_t > 0d0) then
         ! 2003-04
         if (pcy .eq. 2004) then
            rincome = price04*income_t*numeraire
            if (rincome <= 6000d0) then
               PostTaxY = rincome
            else if (rincome <= 21600d0) then
               PostTaxY = rincome - (rincome - 6000d0)*0.17d0
            else if (rincome <= 52000d0) then
               PostTaxY = rincome - (21600d0 - 6000d0)*0.17d0 - (rincome - 21600d0)*0.3d0
            else if (rincome <= 62500d0) then
               PostTaxY = rincome - (21600d0 - 6000d0)*0.17d0 - (52000d0 - 21600d0)*0.3d0 - (rincome - 52000d0)*0.42d0
            else
               PostTaxY = rincome - (21600d0 - 6000d0)*0.17d0 - (52000d0 - 21600d0)*0.3d0 - (62500d0 - 52000d0)*0.42d0 &
                          - (rincome - 62500d0)*0.47d0
            end if
            ! 2004-05 = default for all other debt policies
         else
            rincome = income_t*numeraire
            if (rincome <= 6000d0) then
               PostTaxY = rincome
            else if (rincome <= 21600d0) then
               PostTaxY = rincome - (rincome - 6000d0)*0.17d0
            else if (rincome <= 58000d0) then
               PostTaxY = rincome - (21600d0 - 6000d0)*0.17d0 - (rincome - 21600d0)*0.3d0
            else if (rincome <= 70000d0) then
               PostTaxY = rincome - (21600d0 - 6000d0)*0.17d0 - (58000d0 - 21600d0)*0.3d0 - (rincome - 58000d0)*0.42d0
            else
               PostTaxY = rincome - (21600d0 - 6000d0)*0.17d0 - (58000d0 - 21600d0)*0.3d0 - (70000d0 - 58000d0)*0.42d0 &
                          - (rincome - 70000d0)*0.47d0
            end if
         end if
         PostTaxY = PostTaxY/numeraire
      else
         PostTaxY = income_t
      end if
   end function PostTaxY

   ! Calculate unemployment benefit payment
   real(dp) function UBenefits(ij_t, income_t, wealth_t)
      IMPLICIT NONE
      integer, intent(in) :: ij_t
      real(dp), intent(in) :: income_t, wealth_t
      real(dp) :: income, wealth
      wealth = wealth_t*numeraire
      if (wealth >= 153000d0 .or. ij_t > JWork) then
         UBenefits = 0d0
      else
         income = income_t*numeraire/26d0 ! fortnightly income
         if (income <= 62d0) then
            UBenefits = 394.60d0
         else if (income <= 142d0) then
            UBenefits = 394.60d0 - 0.5d0*(income - 62d0)
         else if (income <= 648.57d0) then ! 40 = (142 - 62)*0.5
            UBenefits = 394.60d0 - 40d0 - 0.7d0*(income - 142d0)
         else
            UBenefits = 0d0
         end if
         UBenefits = 26d0/numeraire*UBenefits
      end if
   end function UBenefits

   ! Smooth post-tax income following functional form from Heathcote et al.
   real(dp) function Smooth_PostTaxY(income_t)
      IMPLICIT NONE
      real(dp), intent(in) :: income_t
      real(dp) :: rincome
      rincome = income_t*numeraire
      Smooth_PostTaxY = EXP(smoothtax(1))*(rincome**smoothtax(2))
      Smooth_PostTaxY = Smooth_PostTaxY/numeraire
   end function Smooth_PostTaxY

   ! Smoothed version of baseline UI that phases out at same point but with constant marginal rate
   real(dp) function Smooth_UI(income_t)
      IMPLICIT NONE
      real(dp), intent(in) :: income_t
      real(dp), parameter :: phase_out = 16863d0
      real(dp), parameter :: max_ui = 394.6d0*26d0
      real(dp) :: rincome
      rincome = income_t*numeraire
      IF (rincome > phase_out) THEN
         Smooth_UI = 0d0
      ELSE
         Smooth_UI = MAX(max_ui - rincome*max_ui/phase_out, 0d0)/numeraire
      END IF
   end function Smooth_UI

   ! Function to calculate total taxes and transfers paid/received
   !! Note: positive = outflow for government
   real(dp) function TaxesTransfers(ij_t, pcy, income_t, wealth_t)
      IMPLICIT NONE
      integer, intent(in) :: ij_t, pcy
      real(dp), intent(in) :: income_t, wealth_t
      SELECT CASE (RunMode)
      CASE DEFAULT
         TaxesTransfers = PostTaxY(income_t, pcy) - income_t + UBenefits(ij_t, income_t, wealth_t)
      CASE (4, 8)
         TaxesTransfers = Smooth_PostTaxY(income_t) - income_t + Smooth_UI(income_t)
      END SELECT
   end function TaxesTransfers

   ! Function to calculate wage given current state
   real(dp) function CalcWage(s)
      IMPLICIT NONE
      type(state_t), intent(in) :: s
      if (s%ij <= JWork) then
         CalcWage = exp(AProfile(s%ij, s%ed) + s%theta + s%epsln)
      else
         CalcWage = 0d0
      end if
   end function CalcWage

   ! Function to calculate wage income
   real(dp) function WageIncome(L, s)
      IMPLICIT NONE
      real(dp), intent(in) :: L
      type(state_t), intent(in) :: s
      if (s%ij <= JWork) then
         WageIncome = CalcWage(s)*L
      else
         WageIncome = 0d0
      end if
   end function WageIncome

   ! Function to calculate capital income
   real(dp) function CapitalIncome(s)
      IMPLICIT NONE
      type(state_t), intent(in) :: s
      CapitalIncome = (RA(s%liqw) - 1d0)/RA(s%liqw)*s%liqw
      CapitalIncome = MAX(CapitalIncome, 0d0)
   end function CapitalIncome

   ! Function to calculate HELP income used for debt repayments and taxes/transfers
   real(dp) function HELPIncome(L, s)
      IMPLICIT NONE
      real(dp), intent(in) :: L
      type(state_t), intent(in) :: s
      HELPIncome = WageIncome(L, s)
      SELECT CASE (RunMode)
      CASE (0, 1, 2)
         HELPIncome = HELPIncome + CapitalIncome(s)
      END SELECT
   end function HELPIncome

   ! Calculate cash available for consumption-savings pre-debt payment
   real(dp) function CashPreDebt(L, s)
      IMPLICIT NONE
      real(dp), intent(in) :: L
      type(state_t), intent(in) :: s
      CashPreDebt = s%liqw + WageIncome(L, s)
      if (s%ij <= JWork) then ! default to first policy in retirement to reduce state variables
         CashPreDebt = CashPreDebt + TaxesTransfers(s%ij, Policy(s%ipcy), HELPIncome(L, s), s%liqw)
      else
         CashPreDebt = CashPreDebt + TaxesTransfers(s%ij, Policy(1), HELPIncome(L, s), s%liqw)
      end if
   end function CashPreDebt

   ! Aggregated debt repayment function
   real(dp) function DebtPayment(L, s)
      IMPLICIT NONE
      real(dp), intent(in) :: L
      type(state_t), intent(in) :: s
      real(dp) :: income_t
      integer :: pcy
      pcy = Policy(s%ipcy)
      if (pcy .eq. -1) then
         DebtPayment = DOptimal_Y(L, s)
      else
         income_t = HELPIncome(L, s)
         if (pcy > 0) then
            DebtPayment = HELPPayment(income_t, s%debt, pcy)
         else if (pcy .eq. 0) then
            DebtPayment = 0d0
         else if (pcy .eq. -2) then
            DebtPayment = US_Fixed(s%ij, s%debt, 1, 10)
         else if (pcy .eq. -3) then
            DebtPayment = US_Fixed(s%ij, s%debt, 1, 25)
         else if (pcy .eq. -4) then
            DebtPayment = HELPPayment(income_t, s%debt, 2004)
         else if (pcy .eq. -5) then
            DebtPayment = HELPPayment(income_t, s%debt, 2005)
         else if (pcy .eq. -6) then
            DebtPayment = US_Fixed(s%ij, s%debt, 11, 20)
         else if (pcy .eq. -101) then ! 2014 IBR no forgiveness
            DebtPayment = US_IBR(s%ij, income_t, s%debt, 0.1d0, 1.5d0, JWork, .FALSE.)
         else if (pcy .eq. -102) then ! 2022 proposed IBR no forgiveness
            DebtPayment = US_IBR(s%ij, income_t, s%debt, 0.05d0, 2.25d0, JWork, .FALSE.)
         else if (pcy .eq. -103) then ! 2014 IBR no forgiveness + fixed repayment cap
            DebtPayment = US_IBR(s%ij, income_t, s%debt, 0.1d0, 1.5d0, JWork, .TRUE.)
         else if (pcy .eq. -104) then ! 2022 proposed IBR no forgiveness + fixed repayment cap
            DebtPayment = US_IBR(s%ij, income_t, s%debt, 0.05d0, 2.25d0, JWork, .TRUE.)
         else if (pcy .eq. -201) then ! 2014 IBR with forgiveness
            DebtPayment = US_IBR(s%ij, income_t, s%debt, 0.1d0, 1.5d0, 20, .FALSE.)
         else if (pcy .eq. -202) then ! 2022 proposed IBR with forgiveness
            DebtPayment = US_IBR(s%ij, income_t, s%debt, 0.05d0, 2.25d0, 20, .FALSE.)
         else if (pcy .eq. -401) then ! Lumni ISA parameters
            DebtPayment = ISA(s%ij, s%ed, income_t, s%debt, 0.18d0, 6, .FALSE., .FALSE.)
         else if (pcy .eq. -402) then ! Purdue ISA from Figure 1 Mumford without threshold
            DebtPayment = ISA(s%ij, s%ed, income_t, s%debt, 0.04d0, 9, .FALSE., .TRUE.)
         else if (pcy .eq. -403) then ! Purdue ISA from Figure 1 Mumford with threshold
            DebtPayment = ISA(s%ij, s%ed, income_t, s%debt, 0.04d0, 9, .TRUE., .TRUE.)
         else
            print *, 'Unknown policy code: ', pcy
            RETURN
         end if
         SELECT CASE (pcy)
         CASE (-2, -3, -6)
            DebtPayment = MIN(DebtPayment, CashPreDebt(L, s))
         END SELECT
      end if
      DebtPayment = MAX(DebtPayment, 0d0)
   end function DebtPayment

   ! Retirement pension formula
   real(dp) function RetirementPension(s)
      IMPLICIT NONE
      type(state_t), intent(in) :: s
      real(dp) :: wealth
      SELECT CASE (RunMode)
      CASE DEFAULT
         wealth = s%liqw*numeraire
         if (wealth <= 153000d0) then
            RetirementPension = 12402d0
         else if (wealth <= 312000d0) then
            RetirementPension = 12402d0 - 3d0*26d0*real(floor((wealth - 153000d0)/1000d0), kind=dp)
         else
            RetirementPension = 0d0
         end if
      CASE (4, 8)
         RetirementPension = 12402d0
      END SELECT
      RetirementPension = RetirementPension/numeraire
   end function RetirementPension

   ! Function to calculate government transfer to stay above net consumption floor
   real(dp) function FloorTransfer(L, s)
      IMPLICIT NONE
      real(dp), intent(in) :: L
      type(state_t), intent(in) :: s
      real(dp) :: cash
      if (s%ij > JWork .or. FixLabor .eq. 1) then
         FloorTransfer = 0d0
      else
         cash = CashPreDebt(L, s) - DebtPayment(L, s)
         FloorTransfer = max(floorC - cash, 0d0)
      end if
   end function FloorTransfer

   ! Function to calculate cash on hand available for consumption-savings as a function of
   ! labor and current state
   real(dp) function CalcCash(L, s)
      IMPLICIT NONE
      real(dp), intent(in) :: L
      type(state_t), intent(in) :: s
      CalcCash = CashPreDebt(L, s)
      if (s%ij <= JWork) then
         CalcCash = CalcCash - DebtPayment(L, s)
      else
         CalcCash = CalcCash + RetirementPension(s)
      end if
      CalcCash = CalcCash + FloorTransfer(L, s)
   end function CalcCash

END MODULE EconFunctions
