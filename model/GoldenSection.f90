MODULE GoldenSection

   USE Parameters, only: positiveInf
   USE types, only: args_opt
   USE OptParameters, only: goldentol, goldenalpha2, goldenalpha1

   IMPLICIT NONE

   integer, parameter :: dp = selected_real_kind(15, 307)

CONTAINS

!---------------------------------------------------------------------------------------------------------!
! Golden-Section search method for minimization.
! This is based on code given in Miranda and Fackler: "Applied Computational Economics and Finance" pg. 65
! This is a derivative-free minimizer that will only work for univariate convex functions,
! but is guaranteed to work in that case.
!---------------------------------------------------------------------------------------------------------!

   subroutine min_GoldenSection(fn, x, y, low, high, flag, args)

      IMPLICIT NONE

      ! Arguments
      real(dp), external :: fn
      real(dp), intent(inout) :: x, y
      real(dp), intent(in) :: low, high
      type(args_opt), intent(in) :: args
      integer, intent(inout) :: flag
      ! For program
      real(dp) :: a, b, f0, f1, f2, f3, x1, x2, diff

      ! Return if boundary has no mass
      if (low >= high) then
         x = 0d0
         y = positiveInf
         flag = 2
         RETURN
      end if

      ! Boundaries for golden search
      a = low
      b = high
      ! Use golden search method to optimize
      x1 = a + goldenalpha1*(b - a)
      x2 = a + goldenalpha2*(b - a)
      f1 = -fn(x1, args)
      f2 = -fn(x2, args)
      diff = goldenalpha1*goldenalpha2*(b - a)
      do while (diff .gt. goldentol) ! search until tolerance hit
         diff = diff*goldenalpha2
         if (f2 .lt. f1) then
            x2 = x1
            x1 = x1 - diff
            f2 = f1
            f1 = -fn(x1, args)
         else
            x1 = x2
            x2 = x2 + diff
            f1 = f2
            f2 = -fn(x2, args)
         end if
      end do
      ! Choose result from golden search method
      if (f2 .gt. f1) then
         x = x2
         y = f2
      else
         x = x1
         y = f1
      end if
      ! Check whether boundaries are better
      f0 = -fn(a, args)
      f3 = -fn(b, args)
      if (f0 .ge. y) then
         x = a
         y = f0
         flag = 1
      else if (f3 .ge. y) then
         x = b
         y = f3
         flag = 1
      else
         flag = 0
      end if
      ! Flip y sign since minimization
      y = -y
      ! Silence flag so code doesn't stop
      flag = 0

   end subroutine min_GoldenSection

END MODULE GoldenSection
