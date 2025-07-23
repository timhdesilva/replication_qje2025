MODULE EVf_mod

   USE Parameters, only: dp, positiveInf, negativeInf, minVCES, minTransProb, &
                         gdW_LiqW, gdR_LiqW, gd_Debt, gd_ExpTheta, gd_PastL, &
                         NbrWLiqW, NbrRLiqW, NbrDebt, NbrTheta, NbrPastL, &
                         JTot, JWork, NbrNu_GH, GHw_Nu, GHx_Nu, gamma, rhotheta, &
                         R_d, V_E, V_NE, V_Ret
   USE types
   USE EconFunctions, only: lowerA, RA, HELPIncome, DebtPayment
   USE Procedures, only: bisection, interp1d, interp2d_pt, interp3d_pt, interp4d_pt

   IMPLICIT NONE

CONTAINS

   !---------------------------------------------------------------------------------------------------------!
   ! Supporting functions
   !---------------------------------------------------------------------------------------------------------!

   ! Function for interpolating when you're interpolating over theta
   real(dp) function interpV(s_next, s_next_pt, p)
      IMPLICIT NONE
      type(state_t), intent(in) :: s_next
      type(cstate_pt), intent(in) :: s_next_pt
      type(policies_t), intent(in) :: p
      IF (s_next%ij <= JWork) THEN ! ---------------------------- WORKING LIFE ----------------------------------------------
         SELECT CASE (s_next%ed)
         CASE (1) ! non-educated
            interpV = interp3d_pt(0, gamma, NbrPastL, gd_PastL, &
                                  NbrTheta, gd_ExpTheta, &
                                  NbrWLiqW, gdW_LiqW(:, s_next%ij), &
                                  V_NE(:, :, :, s_next%ipcy, s_next%ij), &
                                  p%labor, exp(s_next%theta), s_next%liqw, &
                                  s_next_pt%pL, s_next_pt%iz, s_next_pt%lq, 0)
         CASE (2) ! educated
            interpV = interp4d_pt(0, gamma, NbrPastL, gd_PastL, &
                                  NbrTheta, gd_ExpTheta, &
                                  NbrDebt, gd_Debt(:, s_next%ij), &
                                  NbrWLiqW, gdW_LiqW(:, s_next%ij), &
                                  V_E(:, :, :, :, s_next%ipcy, s_next%ij), &
                                  p%labor, exp(s_next%theta), s_next%debt, s_next%liqw, &
                                  s_next_pt%pL, s_next_pt%iz, s_next_pt%dbt, s_next_pt%lq, 0)
         END SELECT
      ELSE ! ---------------------------- RETIREMENT -----------------------------------------------------------------------
         interpV = interp1d(0, gamma, NbrRLiqW, gdR_LiqW(:, s_next%ij - JWork), &
                            V_Ret(:, s_next%ij - JWork), s_next%liqw, 0)
      END IF
      interpV = MAX(interpV, minVCES)**(1d0 - gamma)
   end function interpV

   !--------------------------------------------------------------------------------------------------------------------------------
   ! MAIN ROUTINE: Function to calculate expected continuation value
   !--------------------------------------------------------------------------------------------------------------------------------

   real(dp) function EVf(s, p)

      IMPLICIT NONE

      type(state_t), intent(in) :: s
      type(policies_t), intent(in) :: p
      type(state_t) :: s_next
      type(cstate_pt) :: s_next_pt
      real(dp) :: prob_
      integer :: iz_ ! indexes for integrating over shocks

      ! Set next period states that face no uncertainty
      s_next%ed = s%ed
      s_next%ij = s%ij + 1
      s_next%ipcy = s%ipcy

      ! Initialize
      EVf = 0d0

      ! Only try to calculate coninuation value if it's not going to be zero
      if (s_next%ij < JTot) then
         ! Calculate liquid wealth next period
         s_next%liqw = RA(p%saving)*p%saving
         if (s_next%ij <= JWork) then ! ---------------------------- WORKING LIFE ----------------------------------------------
            ! Calculate debt balance in next period and find next grid point ahead of time
            if (s_next%ed > 1) then
               s_next%debt = R_d*s%debt - DebtPayment(p%labor, s)
               s_next_pt%dbt = bisection(gd_Debt(:, s_next%ij), NbrDebt, s_next%debt)
            end if
            ! Find other next grid points ahead of time
            s_next_pt%lq = bisection(gdW_LiqW(:, s_next%ij), NbrWLiqW, s_next%liqw)
            s_next_pt%pL = bisection(gd_PastL, NbrPastL, p%labor)
            ! Integrate value function over shocks to state variables
            do iz_ = 1, NbrNu_GH ! \nu_t+1
               s_next%theta = rhotheta*s%theta + GHx_Nu(iz_)
               s_next_pt%iz = bisection(gd_ExpTheta, NbrTheta, exp(s_next%theta))
               prob_ = GHw_Nu(iz_)
               if (prob_ > minTransProb) EVf = EVf + interpV(s_next, s_next_pt, p)*prob_
            end do ! \nu_t+1
         else ! ---------------------------- RETIREMENT -----------------------------------------------------------------------
            EVf = interpV(s_next, s_next_pt, p)
         end if
      end if

   end function EVf

END MODULE EVf_mod

