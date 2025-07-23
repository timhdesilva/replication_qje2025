MODULE NelderMead

   USE types, only: args_opt
   USE OptParameters, only: neldertol, nelderstepfrac, neldermaxiter

   IMPLICIT NONE

   INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(15, 307)

CONTAINS

   ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ! Main routine
   ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   SUBROUTINE amoeba(ndim, start, xopt, fopt, func, flag, args)

      IMPLICIT NONE

      ! Routine arguments
      INTEGER, INTENT(IN) :: ndim ! dimension of parameter vector
      REAL(dp), DIMENSION(ndim), INTENT(IN) :: start ! starting point that determines initial simplex
      REAL(dp), DIMENSION(ndim), INTENT(OUT) :: xopt ! best parameter vector
      REAL(dp), INTENT(OUT) :: fopt ! best function value
      REAL(dp), EXTERNAL :: func ! function to optimize
      INTEGER, INTENT(OUT) :: flag ! flag for exceeding number of iterations
      type(args_opt), intent(in) :: args

      ! Program variables
      REAL(dp), DIMENSION(ndim + 1, ndim) :: p ! simplex of parameters
      REAL(dp), DIMENSION(ndim) :: p2 ! temporary array
      REAL(dp), DIMENSION(ndim + 1) :: y ! function values at each point on simplex
      REAL(dp), PARAMETER :: TINY = 1d-10
      INTEGER :: iter, is, ihi, iopt
      REAL(dp), DIMENSION(ndim) :: psum

      ! Initialize starting simplex based on steps around initial point
      p(1, :) = start
      do is = 1, ndim
         p(1 + is, :) = start
         p(1 + is, is) = p(1 + is, is)*(1d0 + nelderstepfrac)
      end do

      ! Initialize by evaluating at each of the N+1 ponts on the starting simplex
      DO is = 1, size(p, 1)
         p2 = p(is, :)
         y(is) = func(p2, args)
      END DO

      ! Call main routine
      CALL amoeba_private

      ! Determine optimum based on best value on final simplex
      iopt = iminloc(y(:))
      xopt = p(iopt, :)
      fopt = y(iopt)


   CONTAINS

      SUBROUTINE amoeba_private
         IMPLICIT NONE
         INTEGER :: i, ilo, inhi
         REAL(dp) :: rtol, ysave, ytry, ytmp
         iter = 0
         flag = -1
         psum(:) = sum(p(:, :), dim=1)
         do
            ilo = iminloc(y(:))
            ihi = imaxloc(y(:))
            ytmp = y(ihi)
            y(ihi) = y(ilo)
            inhi = imaxloc(y(:))
            y(ihi) = ytmp
            rtol = 2d0*abs(y(ihi) - y(ilo))/(abs(y(ihi)) + abs(y(ilo)) + TINY)
            if (rtol < neldertol) then ! routine converges
               flag = 0
               RETURN
            end if
            if (iter >= neldermaxiter) then ! routine fails to converge
               flag = 1
               RETURN
            end if
            ytry = amotry(-1d0)
            iter = iter + 1
            if (ytry <= y(ilo)) then
               ytry = amotry(2d0)
               iter = iter + 1
            else if (ytry >= y(inhi)) then
               ysave = y(ihi)
               ytry = amotry(0.5d0)
               iter = iter + 1
               if (ytry >= ysave) then
                  p(:, :) = 0.5d0*(p(:, :) + spread(p(ilo, :), 1, size(p, 1)))
                  do i = 1, ndim + 1
                     p2 = p(i, :)
                     if (i /= ilo) y(i) = func(p2, args)
                  end do
                  iter = iter + ndim
                  psum(:) = sum(p(:, :), dim=1)
               end if
            end if
         end do
      END SUBROUTINE amoeba_private

      FUNCTION amotry(fac)
         IMPLICIT NONE
         REAL(dp), INTENT(IN) :: fac
         REAL(dp) :: amotry
         REAL(dp) :: fac1, fac2, ytry
         REAL(dp), DIMENSION(size(p, 2)) :: ptry
         fac1 = (1d0 - fac)/ndim
         fac2 = fac1 - fac
         ptry(:) = psum(:)*fac1 - p(ihi, :)*fac2
         ytry = func(ptry, args)
         if (ytry < y(ihi)) then
            y(ihi) = ytry
            psum(:) = psum(:) - p(ihi, :) + ptry(:)
            p(ihi, :) = ptry(:)
         end if
         amotry = ytry
      END FUNCTION amotry

   END SUBROUTINE amoeba

   ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ! External routines copied over from nrutil.f90
   ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   INTEGER FUNCTION iminloc(arr)
      REAL(dp), DIMENSION(:), INTENT(IN) :: arr
      INTEGER, DIMENSION(1) :: imin
      imin = minloc(arr(:))
      iminloc = imin(1)
   END FUNCTION iminloc

   INTEGER FUNCTION imaxloc(arr)
      REAL(dp), DIMENSION(:), INTENT(IN) :: arr
      INTEGER, DIMENSION(1) :: imax
      imax = maxloc(arr(:))
      imaxloc = imax(1)
   END FUNCTION imaxloc

END MODULE NelderMead
