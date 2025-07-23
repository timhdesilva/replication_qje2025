
MODULE OptimizeRet_mod

   USE Parameters, only: dp, positiveInf, negativeInf, JTot
   USE types
   USE EconFunctions, only: flowU, AggUV, lowerA, CalcCash
   USE GoldenSection, only: min_GoldenSection
   USE EVf_mod, only: EVf

   IMPLICIT NONE

CONTAINS

   !---------------------------------------------------------------------------------------------------------!
   ! Supporting functions
   !---------------------------------------------------------------------------------------------------------!

   ! Function to optimize when using golden search, no boundary constraints needed
   real(dp) function ObjectiveGS(saving, args)
      IMPLICIT NONE
      real(dp), intent(in) :: saving
      type(args_opt), intent(in):: args
      real(dp) :: EVt1, Ut
      type(policies_t) :: p
      p%saving = saving
      ObjectiveGS = negativeInf
      EVt1 = EVf(args%s, p)
      Ut = flowU(args%highs(1) - p%saving, 0d0, args%s)
      if (abs(EVt1) < positiveInf .and. Ut > negativeInf) ObjectiveGS = AggUV(Ut, EVt1, args%s)
      ObjectiveGS = -ObjectiveGS ! minus 1 since minimize
   end function ObjectiveGS

   !---------------------------------------------------------------------------------------------------------!
   ! MAIN ROUTINE: Optimization of consumption and labor supply choices in retirement
   !---------------------------------------------------------------------------------------------------------!

   real(dp) function OptimizeRet(s, savings_opt, consump_opt, flagout)

      IMPLICIT NONE

      ! Arguments
      type(state_t), intent(in) :: s
      real(dp), intent(out) :: savings_opt, consump_opt ! policy functions
      integer, intent(out) :: flagout ! error for optimization
      ! For program
      type(args_opt) :: args
      real(dp) :: optval

      ! Copy states to args
      args%s = s
      ! Optimization via Golden Section Search
      args%lows(1) = lowerA(s)
      args%highs(1) = CalcCash(0d0, s)
      CALL min_GoldenSection(ObjectiveGS, savings_opt, optval, args%lows(1), args%highs(1), flagout, args)
      ! Format outputs
      OptimizeRet = -optval
      if (OptimizeRet > negativeInf) then
         consump_opt = args%highs(1) - savings_opt
      else
         consump_opt = 0d0
      end if

   end function OptimizeRet

END MODULE OptimizeRet_mod
