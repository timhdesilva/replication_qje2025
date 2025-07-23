MODULE OptimizeWork_mod

   USE Parameters, only: dp, positiveInf, negativeInf, epsl, FixLabor, startL, size_start, minL, maxL, beta
   USE types
   USE EconFunctions, only: fixedCost, flowU, AggUV, lowerA, CalcCash
   USE OptParameters, only: WarnFileNum, WarnToFile
   USE NelderMead, only: amoeba
   USE GoldenSection, only: min_GoldenSection
   USE EVf_mod, only: EVf

   IMPLICIT NONE

CONTAINS

   !---------------------------------------------------------------------------------------------------------!
   ! Supporting functions
   !---------------------------------------------------------------------------------------------------------!

   ! Evaluation of utility
   real(dp) function continuationValue(s, p, cashonhand)
      IMPLICIT NONE
      type(state_t), intent(in) :: s
      type(policies_t), intent(in) :: p
      real(dp), intent(in) :: cashonhand
      real(dp) :: EVt1, Ut
      EVt1 = EVf(s, p)
      Ut = flowU(cashonhand - p%saving, p%labor, s)
      if (abs(EVt1) < positiveInf .and. Ut > negativeInf) then
         continuationValue = AggUV(Ut, EVt1, s)
      else
         continuationValue = negativeInf
      end if
   end function continuationValue

   ! Function to optimize when using golden search over just assets, no boundary constraints needed
   real(dp) function ObjectiveGS(saving, args)
      IMPLICIT NONE
      real(dp), intent(in) :: saving
      type(args_opt), intent(in) :: args
      type(policies_t) :: p
      p%saving = saving
      p%labor = args%laborGS
      ObjectiveGS = -continuationValue(args%s, p, args%highs(1)) ! minus 1 since minimize
   end function ObjectiveGS

   ! Function to optimize when doing NM over consumption and labor, imposing boundary constraints
   real(dp) function Objective(input, args)
      IMPLICIT NONE
      real(dp), intent(in) :: input(2)
      type(args_opt), intent(in):: args
      real(dp) :: cashonhand
      type(policies_t) :: p
      p%saving = input(1)
      p%labor = input(2)
      cashonhand = CalcCash(p%labor, args%s)
      Objective = negativeInf
      if (cashonhand > args%lows(1) .and. p%saving < cashonhand .and. p%saving >= args%lows(1) &
          .and. p%labor <= args%highs(2) .and. p%labor >= args%lows(2)) then
         Objective = continuationValue(args%s, p, cashonhand)
      end if
      Objective = -Objective ! minus 1 since minimize
   end function Objective

   ! Wrapper for repeated Nelder-Mead minimization
   subroutine wrapper_NM(args, startsC, startsL, sol, optval, flag)
      IMPLICIT NONE
      type(args_opt), intent(in) :: args
      real(dp), intent(in), dimension(size_start) :: startsC, startsL
      real(dp), intent(out) :: sol(2), optval
      integer, intent(out) :: flag
      integer :: maxNMs, counter, flag0
      real(dp) :: start(2), sol0(2), optval0
      ! Count maximum NMs to run
      counter = 0
      maxNMs = size_start
      ! Run Nelder-Mead maxNMs times or until convergence
      optval = positiveInf
      flag = 0
      do while ((optval >= positiveInf .or. flag > 0) .and. counter + 1 <= maxNMs)
         counter = counter + 1
         start = (/startsC(counter), startsL(counter)/)
         CALL amoeba(2, start, sol0, optval0, Objective, flag0, args) ! NOTE: No explicit boundary here, set in Objective()
         if (optval0 < optval .or. counter .eq. 1) then
            optval = optval0
            sol = sol0
            flag = flag0
         end if
      end do
   end subroutine wrapper_NM

   !---------------------------------------------------------------------------------------------------------!
   ! MAIN ROUTINE 1: Optimization of consumption and labor supply choices in working life
   !---------------------------------------------------------------------------------------------------------!

   real(dp) function OptimizeCL_A(s, savings_opt, consump_opt, labor_opt, flagout, typeout)

      IMPLICIT NONE

      ! Arguments
      type(state_t), intent(in) :: s
      real(dp), intent(out) :: savings_opt, consump_opt, labor_opt ! policy functions
      integer, intent(out) :: flagout, typeout ! error and solution type for optimization
      ! For program
      real(dp) :: optval, solution(2), startA(size_start)
      real(dp) :: soltest(2), boundaries(2, 2), xb(3, 2) ! for boundaries
      integer :: opflag, soltype, istart, l1, l2
      type(args_opt) :: args

      ! Copy states to args
      args%s = s
      ! Optimization
      if (FixLabor .eq. 1) then ! Golden Section Search over A with fixed L
         args%laborGS = 1d0
         args%lows(1) = lowerA(s)
         args%highs(1) = CalcCash(args%laborGS, args%s)
         CALL min_GoldenSection(ObjectiveGS, solution(1), optval, args%lows(1), args%highs(1), opflag, args)
         OptimizeCL_A = optval
         solution(2) = args%laborGS
         soltype = 0
      else ! Nelder-Mead over A and L
         ! Set boundaries and starting point
         args%lows(1) = lowerA(s)
         args%lows(2) = minL
         args%highs(2) = maxL
         args%highs(1) = CalcCash(args%highs(2), args%s)
         do istart = 1, size_start
            startA(istart) = beta*CalcCash(startL(istart), args%s)
         end do
         ! Run baseline NM
         CALL wrapper_NM(args, startA, startL, solution, optval, opflag)
         OptimizeCL_A = optval
         soltype = 0
         ! Check whether boundaries better (Nelder-Mead doesn't do this)
         boundaries(1, :) = args%lows
         boundaries(2, :) = args%highs
         do l1 = 1, 2
            xb(1, :) = (/boundaries(l1, 1), solution(2)/)
            xb(2, :) = (/solution(1), boundaries(l1, 2)/)
            xb(3, :) = (/boundaries(l1, 1), boundaries(l1, 2)/)
            do l2 = 1, 3
               soltest = xb(l2, :)
               optval = Objective(soltest, args)
               if (optval < OptimizeCL_A) then
                  solution = soltest
                  OptimizeCL_A = optval
                  opflag = 0
                  soltype = 1
               end if
            end do
         end do
      end if
      ! Format outputs
      OptimizeCL_A = -OptimizeCL_A ! Undo -1 in function for minimization
      savings_opt = solution(1)
      labor_opt = solution(2)
      flagout = opflag
      typeout = soltype
      if (OptimizeCL_A > negativeInf) then
         consump_opt = CalcCash(labor_opt, args%s) - savings_opt
      else
         consump_opt = 0d0
      end if
      ! Optimization warnings
      if (WarnToFile .eq. 1) then
         if (savings_opt <= args%lows(1) .and. OptimizeCL_A > negativeInf) then
            write (WarnFileNum, *) 'Optimization warning: savings hitting lower bound of', savings_opt
         end if
         if (labor_opt >= args%highs(2) .and. OptimizeCL_A > negativeInf) then
            write (WarnFileNum, *) 'Optimization warning: labor hitting upper bound of', labor_opt
         else if (labor_opt <= args%lows(2) .and. OptimizeCL_A > negativeInf) then
            write (WarnFileNum, *) 'Optimization warning: labor hitting lower bound of', labor_opt
         end if
         if (opflag >= 1 .and. soltype .ne. 1) then
            write (WarnFileNum, *) 'WARNING: opflag greater than 0 with soltype =', soltype
         end if
      end if

   end function OptimizeCL_A

   ! Wrapper to include checking non-adjustment
   real(dp) function OptimizeCL(s, savings_opt, consump_opt, labor_opt, flagout, typeout)

      IMPLICIT NONE

      ! Arguments
      type(state_t), intent(in) :: s
      real(dp), intent(out) :: savings_opt, consump_opt, labor_opt ! policy functions
      integer, intent(out) :: flagout, typeout ! error and solution type for optimization
      ! For program
      real(dp) :: opt_na, savings_na, consump_na
      integer :: flag_na, type_na

      ! Optimize both savings and labor
      OptimizeCL = OptimizeCL_A(s, savings_opt, consump_opt, labor_opt, flagout, typeout)
      ! Check if non-adjustment is better
      if (s%ij > 1 .and. fixedCost(s) > epsl) then
         opt_na = OptimizeC(s, savings_na, consump_na, flag_na, type_na)
         if (opt_na > OptimizeCL) then
            OptimizeCL = opt_na
            savings_opt = savings_na
            labor_opt = s%Plabor
            consump_opt = consump_na
            flagout = flag_na
            typeout = type_na
         end if
      end if

   end function OptimizeCL

   !---------------------------------------------------------------------------------------------------------!
   ! MAIN ROUTINE 2: Optimization of consumption choices in working life
   !---------------------------------------------------------------------------------------------------------!

   real(dp) function OptimizeC(s, savings_opt, consump_opt, flagout, typeout)

      IMPLICIT NONE

      ! Arguments
      type(state_t), intent(in) :: s
      real(dp), intent(out) :: savings_opt, consump_opt ! policy functions
      integer, intent(out) :: flagout, typeout ! error and solution type for optimization
      ! For program
      type(args_opt) :: args
      real(dp) :: optval

      ! Copy states to args
      args%s = s
      ! Optimization via Golden Section Search with fixed L
      args%laborGS = s%Plabor
      args%lows(1) = lowerA(s)
      args%highs(1) = CalcCash(args%laborGS, args%s)
      CALL min_GoldenSection(ObjectiveGS, savings_opt, optval, args%lows(1), args%highs(1), flagout, args)
      ! Format outputs
      OptimizeC = -optval
      typeout = 0
      if (OptimizeC > negativeInf) then
         consump_opt = args%highs(1) - savings_opt
      else
         consump_opt = 0d0
      end if
      ! Optimization warnings
      if (WarnToFile .eq. 1) then
         if (savings_opt <= args%lows(1) .and. OptimizeC > negativeInf) then
            write (WarnFileNum, *) 'Optimization warning: savings hitting lower bound of', savings_opt
         end if
      end if

   end function OptimizeC

END MODULE OptimizeWork_mod
