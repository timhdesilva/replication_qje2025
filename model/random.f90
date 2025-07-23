MODULE random

   USE Parameters, only: dp

! A module for random number generation from the following distributions:
!
!     Distribution                    Function/subroutine name
!
!     Normal (Gaussian)               random_normal

! The compilers own random number generator, SUBROUTINE RANDOM_NUMBER(r),
! is used to provide a source of uniformly distributed random numbers.

! N.B. At this stage, only one random number is generated at each call to
! one of the functions above.

!     Author: Alan Miller
!     e-mail: amiller @ bigpond.net.au

   IMPLICIT NONE
   real(dp), PRIVATE      :: half = 0.5

CONTAINS

   FUNCTION random_normal() RESULT(fn_val)

! Adapted from the following Fortran 77 code
!      ALGORITHM 712, COLLECTED ALGORITHMS FROM ACM.
!      THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,
!      VOL. 18, NO. 4, DECEMBER, 1992, PP. 434-435.

!  The function random_normal() returns a normally distributed pseudo-random
!  number with zero mean and unit variance.

!  The algorithm uses the ratio of uniforms method of A.J. Kinderman
!  and J.F. Monahan augmented with quadratic bounding curves.

      real(dp) :: fn_val

!     Local variables
      real(dp)     :: s = 0.449871d0, t = -0.386595d0, a = 0.19600d0, b = 0.25472d0, &
                      r1 = 0.27597d0, r2 = 0.27846d0, u, v, x, y, q

!     Generate P = (u,v) uniform in rectangle enclosing acceptance region

      DO
         CALL RANDOM_NUMBER(u)
         CALL RANDOM_NUMBER(v)
         v = 1.7156d0*(v - half)

!     Evaluate the quadratic form
         x = u - s
         y = ABS(v) - t
         q = x**2 + y*(a*y - b*x)

!     Accept P if inside inner ellipse
         IF (q < r1) EXIT
!     Reject P if outside outer ellipse
         IF (q > r2) CYCLE
!     Reject P if outside acceptance region
         IF (v**2 < -4d0*LOG(u)*u**2) EXIT
      END DO

!     Return ratio of P's coordinates as the normal deviate
      fn_val = v/u
      RETURN

   END FUNCTION random_normal

END MODULE random