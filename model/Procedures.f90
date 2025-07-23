MODULE Procedures

   USE svd, only: moorepenrose

   IMPLICIT NONE

   integer, parameter :: sp = selected_real_kind(6, 37)
   integer, parameter :: dp = selected_real_kind(15, 307)

CONTAINS

!################################################################################################################################
! functions for writing to files
!################################################################################################################################

   SUBROUTINE WriteMatrix1d_realsp(f, nfile, open, n1, vec)
      CHARACTER(len=*), INTENT(IN) :: f
      INTEGER, INTENT(IN) :: n1, nfile, open
      real(sp), INTENT(in) :: vec(n1)
      CHARACTER ::lstring*80
      INTEGER ::i1
      if (open .eq. 0) OPEN (nfile, FILE=f, STATUS='replace')
      WRITE (UNIT=lstring, FMT='(I5)') n1
      lstring = '('//trim(lstring)//'F24.6)'
      DO i1 = 1, n1
         WRITE (nfile, lstring) (vec(i1))
      END DO
      if (open .eq. 0) CLOSE (nfile)
   END SUBROUTINE WriteMatrix1d_realsp

   SUBROUTINE WriteMatrix1d_real(f, nfile, open, n1, vec)
      CHARACTER(len=*), INTENT(IN) :: f
      INTEGER, INTENT(IN) :: n1, nfile, open
      real(dp), INTENT(in) :: vec(n1)
      CHARACTER ::lstring*80
      INTEGER ::i1
      if (open .eq. 0) OPEN (nfile, FILE=f, STATUS='replace')
      WRITE (UNIT=lstring, FMT='(I10)') n1
      lstring = '('//trim(lstring)//'F24.6)'
      DO i1 = 1, n1
         WRITE (nfile, lstring) (vec(i1))
      END DO
      if (open .eq. 0) CLOSE (nfile)
   END SUBROUTINE WriteMatrix1d_real

   SUBROUTINE WriteMatrix1d_int(f, nfile, open, n1, vec)
      CHARACTER(len=*), INTENT(IN) :: f
      INTEGER, INTENT(IN) :: n1, nfile, open
      INTEGER, INTENT(in) :: vec(n1)
      CHARACTER ::lstring*80
      INTEGER ::i1
      if (open .eq. 0) OPEN (nfile, FILE=f, STATUS='replace')
      WRITE (UNIT=lstring, FMT='(I5)') n1
      lstring = '('//trim(lstring)//'I16)'
      DO i1 = 1, n1
         WRITE (nfile, lstring) (vec(i1))
      END DO
      if (open .eq. 0) CLOSE (nfile)
   END SUBROUTINE WriteMatrix1d_int

   SUBROUTINE WriteMatrix2d_real(f, nfile, open, n1, n2, mat)
      CHARACTER(len=*), INTENT(IN) :: f
      INTEGER, INTENT(IN) :: n1, n2, nfile, open
      real(dp), INTENT(in) :: mat(n1, n2)
      CHARACTER ::lstring*80
      INTEGER ::i1
      if (open .eq. 0) OPEN (nfile, FILE=f, STATUS='replace')
      WRITE (UNIT=lstring, FMT='(I5)') n2
      lstring = '('//trim(lstring)//'F24.6)'
      DO i1 = 1, n1
         WRITE (nfile, lstring) (mat(i1, :))
      END DO
      if (open .eq. 0) CLOSE (nfile)
   END SUBROUTINE WriteMatrix2d_real

   SUBROUTINE WriteMatrix2d_realsp(f, nfile, open, n1, n2, mat)
      CHARACTER(len=*), INTENT(IN) :: f
      INTEGER, INTENT(IN) :: n1, n2, nfile, open
      real(sp), INTENT(in) :: mat(n1, n2)
      CHARACTER ::lstring*80
      INTEGER ::i1
      if (open .eq. 0) OPEN (nfile, FILE=f, STATUS='replace')
      WRITE (UNIT=lstring, FMT='(I5)') n2
      lstring = '('//trim(lstring)//'F24.6)'
      DO i1 = 1, n1
         WRITE (nfile, lstring) (mat(i1, :))
      END DO
      if (open .eq. 0) CLOSE (nfile)
   END SUBROUTINE WriteMatrix2d_realsp

   SUBROUTINE WriteMatrix2d_realdp(f, nfile, open, n1, n2, mat)
      CHARACTER(len=*), INTENT(IN) :: f
      INTEGER, INTENT(IN) :: n1, n2, nfile, open
      real(dp), INTENT(in) :: mat(n1, n2)
      CHARACTER ::lstring*80
      INTEGER ::i1
      if (open .eq. 0) OPEN (nfile, FILE=f, STATUS='replace')
      WRITE (UNIT=lstring, FMT='(I5)') n2
      lstring = '('//trim(lstring)//'F24.15)'
      DO i1 = 1, n1
         WRITE (nfile, lstring) (mat(i1, :))
      END DO
      if (open .eq. 0) CLOSE (nfile)
   END SUBROUTINE WriteMatrix2d_realdp

   SUBROUTINE WriteMatrix2d_int(f, nfile, open, n1, n2, mat)
      CHARACTER(len=*), INTENT(IN) :: f
      INTEGER, INTENT(IN) :: n1, n2, nfile, open
      INTEGER, INTENT(in) :: mat(n1, n2)
      CHARACTER ::lstring*80
      INTEGER ::i1
      if (open .eq. 0) OPEN (nfile, FILE=f, STATUS='replace')
      WRITE (UNIT=lstring, FMT='(I5)') n2
      lstring = '('//trim(lstring)//'I16)'
      DO i1 = 1, n1
         WRITE (nfile, lstring) (mat(i1, :))
      END DO
      if (open .eq. 0) CLOSE (nfile)
   END SUBROUTINE WriteMatrix2d_int

   SUBROUTINE WriteMatrix3d_real(f, nfile, open, n1, n2, n3, mat)
      CHARACTER(len=*), INTENT(IN) :: f
      INTEGER, INTENT(IN) :: n1, n2, n3, nfile, open
      real(dp), INTENT(in) :: mat(n1, n2, n3)
      CHARACTER ::lstring*80
      INTEGER ::i3, i2
      if (open .eq. 0) OPEN (nfile, FILE=f, STATUS='replace')
      WRITE (UNIT=lstring, FMT='(I5)') n1
      lstring = '('//trim(lstring)//'F24.6)'
      DO i3 = 1, n3
         DO i2 = 1, n2
            WRITE (nfile, lstring) (mat(:, i2, i3))
         END DO
      END DO
      if (open .eq. 0) CLOSE (nfile)
   END SUBROUTINE WriteMatrix3d_real

   SUBROUTINE WriteMatrix3d_int(f, nfile, open, n1, n2, n3, mat)
      CHARACTER(len=*), INTENT(IN) :: f
      INTEGER, INTENT(IN) :: n1, n2, n3, nfile, open
      INTEGER, INTENT(in) :: mat(n1, n2, n3)
      CHARACTER ::lstring*80
      INTEGER ::i3, i2
      if (open .eq. 0) OPEN (nfile, FILE=f, STATUS='replace')
      WRITE (UNIT=lstring, FMT='(I5)') n1
      lstring = '('//trim(lstring)//'I16)'
      DO i3 = 1, n3
         DO i2 = 1, n2
            WRITE (nfile, lstring) (mat(:, i2, i3))
         END DO
      END DO
      if (open .eq. 0) CLOSE (nfile)
   END SUBROUTINE WriteMatrix3d_int

!################################################################################################################################
! Function to check if value in list
!################################################################################################################################

   logical function is_in_array(x, arr)
      IMPLICIT NONE
      integer, intent(in) :: x
      integer, dimension(:), intent(in) :: arr
      integer :: i
      is_in_array = .FALSE.
      do i = 1, size(arr)
         if (x == arr(i)) then
            is_in_array = .TRUE.
            exit
         end if
      end do
   end function is_in_array

!################################################################################################################################
! Routine to slice an array with another integer array
!################################################################################################################################

   subroutine slice_array(arr, indices, sliced_arr)
      IMPLICIT NONE
      real(dp), intent(in) :: arr(:) ! input array to be sliced
      integer, intent(in) :: indices(:) ! indices to be sliced
      real(dp), intent(out) :: sliced_arr(size(indices)) ! sliced array
      integer :: i, j
      j = 1
      do i = 1, size(arr)
         if (i == indices(j)) then
            sliced_arr(j) = arr(i)
            j = j + 1
            if (j > size(indices)) RETURN
         end if
      end do
   end subroutine slice_array

!################################################################################################################################
! Interpolation functions
!################################################################################################################################

! Bi-section search
   integer function bisection(grid, gridsize, pt)
      IMPLICIT NONE
      integer, intent(in) :: gridsize
      real(dp), intent(in) :: grid(gridsize), pt
      integer :: lo, hi, i
      lo = 1
      hi = gridsize
      do while (hi - lo > 1)
         i = (hi + lo)/2
         if (grid(i) > pt) then
            hi = i
         else
            lo = i
         end if
      end do
      bisection = lo
   end function bisection

! Given a point this function will return the nearest gridpoint to that point, breaking ties to the lower grid point
   integer function find_closest_grid_point(grid, gridsize, pt)
      IMPLICIT NONE
      integer, intent(in) :: gridsize
      real(dp), intent(in) :: grid(gridsize), pt
      integer :: n
      n = bisection(grid, gridsize, pt)
      if (grid(n + 1) - pt < pt - grid(n)) then
         find_closest_grid_point = n + 1
      else
         find_closest_grid_point = n
      end if
   end function find_closest_grid_point

! This function USEs 1-d linear interpolation to estimate the value of a function f at point x
   real(dp) FUNCTION interp1d(crra, crracurv, x_len, x_array, f, x, talk_in)
! set crra=1 if you want quasi-linear interpolation for CRRA

      IMPLICIT NONE

      integer, intent(in) :: crra, x_len
      real(dp), dimension(x_len), intent(in) :: x_array
      real(dp), dimension(x_len), intent(in) :: f
      real(dp), intent(in) :: crracurv, x
      integer, intent(in), optional :: talk_in

      integer :: i, talk
      real(dp) :: xL, xH, fL, fH

      if (present(talk_in)) then
         talk = talk_in
      else
         talk = 1
      end if

      i = bisection(x_array, x_len, x)
      xL = x_array(i)
      xH = x_array(i + 1)

      if (crra == 0) then
         fL = f(i)
         fH = f(i + 1)
         interp1d = (fL*(xH - x) + fH*(x - xL))/(xH - xL)
      else
         fL = ((1d0 - crracurv)*f(i))**(1d0/(1d0 - crracurv))
         fH = ((1d0 - crracurv)*f(i + 1))**(1d0/(1d0 - crracurv))
         interp1d = (fL*(xH - x) + fH*(x - xL))/(xH - xL)
         interp1d = (interp1d**(1d0 - crracurv))/(1d0 - crracurv)
      end if

      if (talk .eq. 1) then
         if (interp1d > HUGE(interp1d)) then
            print *, 'Large positive value returned in interp1d interpolation:', interp1d
         else if (interp1d < -HUGE(interp1d)) then
            print *, 'Large negative value returned in interp1d interpolation:', interp1d
         else if (interp1d /= interp1d) then
            print *, 'NaN returned in interp1d interpolation:', interp1d
         end if
      end if

   END FUNCTION interp1d

! This function USEs 1-d linear interpolation to estimate the value of a function f at point x
   real(dp) FUNCTION interp1d_pt(crra, crracurv, x_len, x_array, f, x, i, talk_in)
! set crra=1 if you want quasi-linear interpolation for CRRA

      IMPLICIT NONE

      integer, intent(in) :: crra, x_len, i
      real(dp), dimension(x_len), intent(in) :: x_array
      real(dp), dimension(x_len), intent(in) :: f
      real(dp), intent(in) :: crracurv, x
      integer, intent(in), optional :: talk_in

      integer :: talk
      real(dp) :: xL, xH, fL, fH

      if (present(talk_in)) then
         talk = talk_in
      else
         talk = 1
      end if

      xL = x_array(i)
      xH = x_array(i + 1)

      if (crra == 0) then
         fL = f(i)
         fH = f(i + 1)
         interp1d_pt = (fL*(xH - x) + fH*(x - xL))/(xH - xL)
      else
         fL = ((1d0 - crracurv)*f(i))**(1d0/(1d0 - crracurv))
         fH = ((1d0 - crracurv)*f(i + 1))**(1d0/(1d0 - crracurv))
         interp1d_pt = (fL*(xH - x) + fH*(x - xL))/(xH - xL)
         interp1d_pt = (interp1d_pt**(1d0 - crracurv))/(1d0 - crracurv)
      end if

      if (talk .eq. 1) then
         if (interp1d_pt > HUGE(interp1d_pt)) then
            print *, 'Large positive value returned in interp1d_pt interpolation:', interp1d_pt
         else if (interp1d_pt < -HUGE(interp1d_pt)) then
            print *, 'Large negative value returned in interp1d_pt interpolation:', interp1d_pt
         else if (interp1d_pt /= interp1d_pt) then
            print *, 'NaN returned in interp1d_pt interpolation:', interp1d_pt
         end if
      end if

   END FUNCTION interp1d_pt

! This function USEs 2-d linear interpolation to estimate the value of a function f at point x,y, extrapolating out
! of range if necessary
   real(dp) FUNCTION interp2d(crra, crracurv, nx, x, ny, y, f, xi, yi, talk_in)
! set crra=1 if you want quasi-linear interpolation for CRRA

      IMPLICIT NONE

      INTEGER, INTENT(in) :: crra, nx, ny
      real(dp), INTENT(in) :: crracurv, x(nx), y(ny), f(nx, ny), xi, yi
      real(dp) :: xL, xH, yL, yH, fLL, fHH, fLH, fHL, dxdy
      INTEGER :: xlocL, ylocL

      integer, intent(in), optional :: talk_in
      integer :: talk

      if (present(talk_in)) then
         talk = talk_in
      else
         talk = 1
      end if

      ! Find grid points
      xlocL = bisection(x, nx, xi)
      ylocL = bisection(y, ny, yi)

      ! Grid values
      xL = x(xlocL)
      xH = x(xlocL + 1)
      yL = y(ylocL)
      yH = y(ylocL + 1)

      ! Calculate function values
      fLL = f(xlocL, ylocL)
      fLH = f(xlocL, ylocL + 1)
      fHL = f(xlocL + 1, ylocL)
      fHH = f(xlocL + 1, ylocL + 1)

      IF (crra == 0) THEN
         dxdy = (xH - xL)*(yH - yL)
         interp2d = fLL*(xH - xi)*(yH - yi)/(dxdy) + fHL*(xi - xL)*(yH - yi)/(dxdy) + &
                    fLH*(xH - xi)*(yi - yL)/(dxdy) + fHH*(xi - xL)*(yi - yL)/(dxdy)
      ELSE
         fLL = ((1d0 - crracurv)*fLL)**(1d0/(1d0 - crracurv))
         fLH = ((1d0 - crracurv)*fLH)**(1d0/(1d0 - crracurv))
         fHL = ((1d0 - crracurv)*fHL)**(1d0/(1d0 - crracurv))
         fHH = ((1d0 - crracurv)*fHH)**(1d0/(1d0 - crracurv))

         dxdy = (xH - xL)*(yH - yL)
         interp2d = fLL*(xH - xi)*(yH - yi)/(dxdy) + fHL*(xi - xL)*(yH - yi)/(dxdy) + &
                    fLH*(xH - xi)*(yi - yL)/(dxdy) + fHH*(xi - xL)*(yi - yL)/(dxdy)
         interp2d = ((interp2d**(1d0 - crracurv))/(1d0 - crracurv))
      END IF

      if (talk .eq. 1) then
         if (interp2d > HUGE(interp2d)) then
            print *, 'Large positive value returned in interp2d interpolation:', interp2d
         else if (interp2d < -HUGE(interp2d)) then
            print *, 'Large negative value returned in interp2d interpolation:', interp2d
         else if (interp2d /= interp2d) then
            print *, 'NaN returned in interp2d interpolation:', interp2d
         end if
      end if

   END FUNCTION interp2d

! Same function as above, grid points of lower left corner are already provided
   real(dp) FUNCTION interp2d_pt(crra, crracurv, nx, x, ny, y, f, xi, yi, xlocL, ylocL, talk_in)
! set crra=1 if you want quasi-linear interpolation for CRRA

      IMPLICIT NONE

      INTEGER, INTENT(in) :: crra, nx, ny, xlocL, ylocL
      real(dp), INTENT(in) :: crracurv, x(nx), y(ny), f(nx, ny), xi, yi
      real(dp) :: xL, xH, yL, yH, fLL, fHH, fLH, fHL, dxdy

      integer, intent(in), optional :: talk_in
      integer :: talk

      if (present(talk_in)) then
         talk = talk_in
      else
         talk = 1
      end if

      ! Grid values based on points provided
      xL = x(xlocL)
      xH = x(xlocL + 1)
      yL = y(ylocL)
      yH = y(ylocL + 1)

      ! Calculate function values
      fLL = f(xlocL, ylocL)
      fLH = f(xlocL, ylocL + 1)
      fHL = f(xlocL + 1, ylocL)
      fHH = f(xlocL + 1, ylocL + 1)

      IF (crra == 0) THEN
         dxdy = (xH - xL)*(yH - yL)
         interp2d_pt = fLL*(xH - xi)*(yH - yi)/(dxdy) + fHL*(xi - xL)*(yH - yi)/(dxdy) + &
                       fLH*(xH - xi)*(yi - yL)/(dxdy) + fHH*(xi - xL)*(yi - yL)/(dxdy)
      ELSE
         fLL = ((1d0 - crracurv)*fLL)**(1d0/(1d0 - crracurv))
         fLH = ((1d0 - crracurv)*fLH)**(1d0/(1d0 - crracurv))
         fHL = ((1d0 - crracurv)*fHL)**(1d0/(1d0 - crracurv))
         fHH = ((1d0 - crracurv)*fHH)**(1d0/(1d0 - crracurv))

         dxdy = (xH - xL)*(yH - yL)
         interp2d_pt = fLL*(xH - xi)*(yH - yi)/(dxdy) + fHL*(xi - xL)*(yH - yi)/(dxdy) + &
                       fLH*(xH - xi)*(yi - yL)/(dxdy) + fHH*(xi - xL)*(yi - yL)/(dxdy)
         interp2d_pt = ((interp2d_pt**(1d0 - crracurv))/(1d0 - crracurv))
      END IF

      if (talk .eq. 1) then
         if (interp2d_pt > HUGE(interp2d_pt)) then
            print *, 'Large positive value returned in interp2d_pt interpolation:', interp2d_pt
         else if (interp2d_pt < -HUGE(interp2d_pt)) then
            print *, 'Large negative value returned in interp2d_pt interpolation:', interp2d_pt
         else if (interp2d_pt /= interp2d_pt) then
            print *, 'NaN returned in interp2d_pt interpolation:', interp2d_pt
         end if
      end if

   END FUNCTION interp2d_pt

! This function USEs 3-d linear interpolation to estimate the quasi-linear version of the value
! of a function f at point x , y, z
!    http://paulbourke.net/miscellaneous/interpolation/
   real(dp) FUNCTION interp3d(crra, crracurv, nx, x, ny, y, nz, z, f, xi, yi, zi, talk_in)
      ! set crra=1 if you want quasi-linear interpolation for CRRA

      IMPLICIT NONE
      integer, intent(in) :: crra, nx, ny, nz
      real(dp), intent(in) :: crracurv, x(nx), y(ny), z(nz), f(nx, ny, nz), xi, yi, zi
      real(dp) ::  xin, yin, zin
      integer :: llc(3)

      integer, intent(in), optional :: talk_in
      integer :: talk

      if (present(talk_in)) then
         talk = talk_in
      else
         talk = 1
      end if

! Find grid points
      llc(1) = bisection(x, nx, xi)
      llc(2) = bisection(y, ny, yi)
      llc(3) = bisection(z, nz, zi)

      ! Normalize points to make the length exactly 1 in each direction
      xin = (xi - x(llc(1)))/(x(llc(1) + 1) - x(llc(1)))
      yin = (yi - y(llc(2)))/(y(llc(2) + 1) - y(llc(2)))
      zin = (zi - z(llc(3)))/(z(llc(3) + 1) - z(llc(3)))

      ! CRRA
      if (crra == 1) then
         interp3d = &
            (((1d0 - crracurv)*f(llc(1), llc(2), llc(3)))**(1d0/(1d0 - crracurv)))*(1d0 - xin)*(1d0 - yin)*(1d0 - zin) + &
            (((1d0 - crracurv)*f(llc(1) + 1, llc(2), llc(3)))**(1d0/(1d0 - crracurv)))*xin*(1d0 - yin)*(1d0 - zin) + &
            (((1d0 - crracurv)*f(llc(1), llc(2) + 1, llc(3)))**(1d0/(1d0 - crracurv)))*(1d0 - xin)*yin*(1d0 - zin) + &
            (((1d0 - crracurv)*f(llc(1), llc(2), llc(3) + 1))**(1d0/(1d0 - crracurv)))*(1d0 - xin)*(1d0 - yin)*zin + &
            (((1d0 - crracurv)*f(llc(1), llc(2) + 1, llc(3) + 1))**(1d0/(1d0 - crracurv)))*(1d0 - xin)*yin*zin + &
            (((1d0 - crracurv)*f(llc(1) + 1, llc(2), llc(3) + 1))**(1d0/(1d0 - crracurv)))*xin*(1d0 - yin)*zin + &
            (((1d0 - crracurv)*f(llc(1) + 1, llc(2) + 1, llc(3)))**(1d0/(1d0 - crracurv)))*xin*yin*(1d0 - zin) + &
            (((1d0 - crracurv)*f(llc(1) + 1, llc(2) + 1, llc(3) + 1))**(1d0/(1d0 - crracurv)))*xin*yin*zin

         interp3d = ((interp3d**(1d0 - crracurv))/(1d0 - crracurv))

      else
         interp3d = &
            f(llc(1), llc(2), llc(3))*(1d0 - xin)*(1d0 - yin)*(1d0 - zin) + &
            f(llc(1) + 1, llc(2), llc(3))*xin*(1d0 - yin)*(1d0 - zin) + &
            f(llc(1), llc(2) + 1, llc(3))*(1d0 - xin)*yin*(1d0 - zin) + &
            f(llc(1), llc(2), llc(3) + 1)*(1d0 - xin)*(1d0 - yin)*zin + &
            f(llc(1), llc(2) + 1, llc(3) + 1)*(1d0 - xin)*yin*zin + &
            f(llc(1) + 1, llc(2), llc(3) + 1)*xin*(1d0 - yin)*zin + &
            f(llc(1) + 1, llc(2) + 1, llc(3))*xin*yin*(1d0 - zin) + &
            f(llc(1) + 1, llc(2) + 1, llc(3) + 1)*xin*yin*zin
      end if

      if (talk .eq. 1) then
         if (interp3d > HUGE(interp3d)) then
            print *, 'Large positive value returned in interp3d interpolation:', interp3d
         else if (interp3d < -HUGE(interp3d)) then
            print *, 'Large negative value returned in interp3d interpolation:', interp3d
         else if (interp3d /= interp3d) then
            print *, 'NaN returned in interp3d interpolation:', interp3d
         end if
      end if

   END FUNCTION interp3d

! Same function as above, grid points of lower left corner are already provided
   real(dp) FUNCTION interp3d_pt(crra, crracurv, nx, x, ny, y, nz, z, f, xi, yi, zi, ptx, pty, ptz, talk_in)
      ! set crra=1 if you want quasi-linear interpolation for CRRA

      IMPLICIT NONE
      integer, intent(in) :: crra, nx, ny, nz, ptx, pty, ptz
      real(dp), intent(in) :: crracurv, x(nx), y(ny), z(nz), f(nx, ny, nz), xi, yi, zi
      real(dp) ::  xin, yin, zin
      integer :: llc(3)

      integer, intent(in), optional :: talk_in
      integer :: talk

      if (present(talk_in)) then
         talk = talk_in
      else
         talk = 1
      end if

      ! Set lower left corner of grid points
      llc(1) = ptx
      llc(2) = pty
      llc(3) = ptz

      ! Normalize points to make the length exactly 1 in each direction
      xin = (xi - x(llc(1)))/(x(llc(1) + 1) - x(llc(1)))
      yin = (yi - y(llc(2)))/(y(llc(2) + 1) - y(llc(2)))
      zin = (zi - z(llc(3)))/(z(llc(3) + 1) - z(llc(3)))
      ! CRRA
      if (crra == 1) then
         interp3d_pt = &
            (((1d0 - crracurv)*f(llc(1), llc(2), llc(3)))**(1d0/(1d0 - crracurv)))*(1d0 - xin)*(1d0 - yin)*(1d0 - zin) + &
            (((1d0 - crracurv)*f(llc(1) + 1, llc(2), llc(3)))**(1d0/(1d0 - crracurv)))*xin*(1d0 - yin)*(1d0 - zin) + &
            (((1d0 - crracurv)*f(llc(1), llc(2) + 1, llc(3)))**(1d0/(1d0 - crracurv)))*(1d0 - xin)*yin*(1d0 - zin) + &
            (((1d0 - crracurv)*f(llc(1), llc(2), llc(3) + 1))**(1d0/(1d0 - crracurv)))*(1d0 - xin)*(1d0 - yin)*zin + &
            (((1d0 - crracurv)*f(llc(1), llc(2) + 1, llc(3) + 1))**(1d0/(1d0 - crracurv)))*(1d0 - xin)*yin*zin + &
            (((1d0 - crracurv)*f(llc(1) + 1, llc(2), llc(3) + 1))**(1d0/(1d0 - crracurv)))*xin*(1d0 - yin)*zin + &
            (((1d0 - crracurv)*f(llc(1) + 1, llc(2) + 1, llc(3)))**(1d0/(1d0 - crracurv)))*xin*yin*(1d0 - zin) + &
            (((1d0 - crracurv)*f(llc(1) + 1, llc(2) + 1, llc(3) + 1))**(1d0/(1d0 - crracurv)))*xin*yin*zin

         interp3d_pt = ((interp3d_pt**(1d0 - crracurv))/(1d0 - crracurv))

      else
         interp3d_pt = &
            f(llc(1), llc(2), llc(3))*(1d0 - xin)*(1d0 - yin)*(1d0 - zin) + &
            f(llc(1) + 1, llc(2), llc(3))*xin*(1d0 - yin)*(1d0 - zin) + &
            f(llc(1), llc(2) + 1, llc(3))*(1d0 - xin)*yin*(1d0 - zin) + &
            f(llc(1), llc(2), llc(3) + 1)*(1d0 - xin)*(1d0 - yin)*zin + &
            f(llc(1), llc(2) + 1, llc(3) + 1)*(1d0 - xin)*yin*zin + &
            f(llc(1) + 1, llc(2), llc(3) + 1)*xin*(1d0 - yin)*zin + &
            f(llc(1) + 1, llc(2) + 1, llc(3))*xin*yin*(1d0 - zin) + &
            f(llc(1) + 1, llc(2) + 1, llc(3) + 1)*xin*yin*zin
      end if

      if (talk .eq. 1) then
         if (interp3d_pt > HUGE(interp3d_pt)) then
            print *, 'Large positive value returned in interp3d_pt interpolation:', interp3d_pt
         else if (interp3d_pt < -HUGE(interp3d_pt)) then
            print *, 'Large negative value returned in interp3d_pt interpolation:', interp3d_pt
         else if (interp3d_pt /= interp3d_pt) then
            print *, 'NaN returned in interp3d_pt interpolation:', interp3d_pt
         end if
      end if

   END FUNCTION interp3d_pt

! 4D linear interpolation routine
   real(dp) FUNCTION interp4d(crra, crracurv, nx, x, ny, y, nz, z, nq, q, f, xi, yi, zi, qi, talk_in)
      ! set crra=1 if you want quasi-linear interpolation for CRRA

      IMPLICIT NONE
      integer, intent(in) :: crra, nx, ny, nz, nq
      real(dp), intent(in) :: crracurv, x(nx), y(ny), z(nz), q(nq), f(nx, ny, nz, nq), xi, yi, zi, qi
      real(dp) :: xin, yin, zin, qin
      integer :: llc(4)
      real(dp) :: V0000, V1000, V0100, V0010, V0001, V1100, V1010, V1001, &
                  V0110, V0101, V0011, V1110, V1101, &
                  V1011, V0111, V1111

      integer, intent(in), optional :: talk_in
      integer :: talk

      if (present(talk_in)) then
         talk = talk_in
      else
         talk = 1
      end if

      ! Find grid points
      llc(1) = bisection(x, nx, xi)
      llc(2) = bisection(y, ny, yi)
      llc(3) = bisection(z, nz, zi)
      llc(4) = bisection(q, nq, qi)

      ! Normalize points to make the length exactly 1 in each direction
      xin = (xi - x(llc(1)))/(x(llc(1) + 1) - x(llc(1)))
      yin = (yi - y(llc(2)))/(y(llc(2) + 1) - y(llc(2)))
      zin = (zi - z(llc(3)))/(z(llc(3) + 1) - z(llc(3)))
      qin = (qi - q(llc(4)))/(q(llc(4) + 1) - q(llc(4)))

      ! Function evaluations
      V0000 = f(llc(1), llc(2), llc(3), llc(4))
      V1000 = f(llc(1) + 1, llc(2), llc(3), llc(4))
      V0100 = f(llc(1), llc(2) + 1, llc(3), llc(4))
      V0010 = f(llc(1), llc(2), llc(3) + 1, llc(4))
      V0001 = f(llc(1), llc(2), llc(3), llc(4) + 1)
      V1100 = f(llc(1) + 1, llc(2) + 1, llc(3), llc(4))
      V1010 = f(llc(1) + 1, llc(2), llc(3) + 1, llc(4))
      V1001 = f(llc(1) + 1, llc(2), llc(3), llc(4) + 1)
      V0110 = f(llc(1), llc(2) + 1, llc(3) + 1, llc(4))
      V0101 = f(llc(1), llc(2) + 1, llc(3), llc(4) + 1)
      V0011 = f(llc(1), llc(2), llc(3) + 1, llc(4) + 1)
      V1110 = f(llc(1) + 1, llc(2) + 1, llc(3) + 1, llc(4))
      V1101 = f(llc(1) + 1, llc(2) + 1, llc(3), llc(4) + 1)
      V1011 = f(llc(1) + 1, llc(2), llc(3) + 1, llc(4) + 1)
      V0111 = f(llc(1), llc(2) + 1, llc(3) + 1, llc(4) + 1)
      V1111 = f(llc(1) + 1, llc(2) + 1, llc(3) + 1, llc(4) + 1)

      ! Quasi-linear CRRA transformation
      if (crra == 1) then
         V0000 = (1d0 - crracurv)*V0000**(1d0/(1d0 - crracurv))
         V1000 = (1d0 - crracurv)*V1000**(1d0/(1d0 - crracurv))
         V0100 = (1d0 - crracurv)*V0100**(1d0/(1d0 - crracurv))
         V0010 = (1d0 - crracurv)*V0010**(1d0/(1d0 - crracurv))
         V0001 = (1d0 - crracurv)*V0001**(1d0/(1d0 - crracurv))
         V1100 = (1d0 - crracurv)*V1100**(1d0/(1d0 - crracurv))
         V1010 = (1d0 - crracurv)*V1010**(1d0/(1d0 - crracurv))
         V1001 = (1d0 - crracurv)*V1001**(1d0/(1d0 - crracurv))
         V0110 = (1d0 - crracurv)*V0110**(1d0/(1d0 - crracurv))
         V0101 = (1d0 - crracurv)*V0101**(1d0/(1d0 - crracurv))
         V0011 = (1d0 - crracurv)*V0011**(1d0/(1d0 - crracurv))
         V1110 = (1d0 - crracurv)*V1110**(1d0/(1d0 - crracurv))
         V1101 = (1d0 - crracurv)*V1101**(1d0/(1d0 - crracurv))
         V1011 = (1d0 - crracurv)*V1011**(1d0/(1d0 - crracurv))
         V0111 = (1d0 - crracurv)*V0111**(1d0/(1d0 - crracurv))
         V1111 = (1d0 - crracurv)*V1111**(1d0/(1d0 - crracurv))
      end if

      ! Interpolation
      interp4d = &
         V0000*(1d0 - xin)*(1d0 - yin)*(1d0 - zin)*(1d0 - qin) + &
         V1000*xin*(1d0 - yin)*(1d0 - zin)*(1d0 - qin) + &
         V0100*(1d0 - xin)*yin*(1d0 - zin)*(1d0 - qin) + &
         V0010*(1d0 - xin)*(1d0 - yin)*zin*(1d0 - qin) + &
         V0001*(1d0 - xin)*(1d0 - yin)*(1d0 - zin)*qin + &
         V1100*xin*yin*(1d0 - zin)*(1d0 - qin) + &
         V1010*xin*(1d0 - yin)*zin*(1d0 - qin) + &
         V1001*xin*(1d0 - yin)*(1d0 - zin)*qin + &
         V0110*(1d0 - xin)*yin*zin*(1d0 - qin) + &
         V0101*(1d0 - xin)*yin*(1d0 - zin)*qin + &
         V0011*(1d0 - xin)*(1d0 - yin)*zin*qin + &
         V1110*xin*yin*zin*(1d0 - qin) + &
         V1101*xin*yin*(1d0 - zin)*qin + &
         V1011*xin*(1d0 - yin)*zin*qin + &
         V0111*(1d0 - xin)*yin*zin*qin + &
         V1111*xin*yin*zin*qin

      ! Undo transformation
      if (crra == 1) interp4d = (interp4d**(1d0 - crracurv))/(1d0 - crracurv)

      if (talk_in .eq. 1) then
         if (interp4d > HUGE(interp4d)) then
            print *, 'Large positive value returned in interp4d interpolation:', interp4d
         else if (interp4d < -HUGE(interp4d)) then
            print *, 'Large negative value returned in interp4d interpolation:', interp4d
         else if (interp4d /= interp4d) then
            print *, 'NaN returned in interp4d interpolation:', interp4d
         end if
      end if

   end function interp4d

   ! Same function as above, grid points of lower left corner are already provided
   real(dp) FUNCTION interp4d_pt(crra, crracurv, nx, x, ny, y, nz, z, nq, q, f, xi, yi, zi, qi, ptx, pty, ptz, ptq, talk_in)
      ! set crra=1 if you want quasi-linear interpolation for CRRA

      IMPLICIT NONE
      integer, intent(in) :: crra, nx, ny, nz, nq, ptx, pty, ptz, ptq
      real(dp), intent(in) :: crracurv, x(nx), y(ny), z(nz), q(nq), f(nx, ny, nz, nq), xi, yi, zi, qi
      real(dp) :: xin, yin, zin, qin
      integer :: llc(4)
      real(dp) :: V0000, V1000, V0100, V0010, V0001, V1100, V1010, V1001, &
                  V0110, V0101, V0011, V1110, V1101, &
                  V1011, V0111, V1111

      integer, intent(in), optional :: talk_in
      integer :: talk

      if (present(talk_in)) then
         talk = talk_in
      else
         talk = 1
      end if

      ! Find grid points
      llc(1) = ptx
      llc(2) = pty
      llc(3) = ptz
      llc(4) = ptq

      ! Normalize points to make the length exactly 1 in each direction
      xin = (xi - x(llc(1)))/(x(llc(1) + 1) - x(llc(1)))
      yin = (yi - y(llc(2)))/(y(llc(2) + 1) - y(llc(2)))
      zin = (zi - z(llc(3)))/(z(llc(3) + 1) - z(llc(3)))
      qin = (qi - q(llc(4)))/(q(llc(4) + 1) - q(llc(4)))

      ! Function evaluations
      V0000 = f(llc(1), llc(2), llc(3), llc(4))
      V1000 = f(llc(1) + 1, llc(2), llc(3), llc(4))
      V0100 = f(llc(1), llc(2) + 1, llc(3), llc(4))
      V0010 = f(llc(1), llc(2), llc(3) + 1, llc(4))
      V0001 = f(llc(1), llc(2), llc(3), llc(4) + 1)
      V1100 = f(llc(1) + 1, llc(2) + 1, llc(3), llc(4))
      V1010 = f(llc(1) + 1, llc(2), llc(3) + 1, llc(4))
      V1001 = f(llc(1) + 1, llc(2), llc(3), llc(4) + 1)
      V0110 = f(llc(1), llc(2) + 1, llc(3) + 1, llc(4))
      V0101 = f(llc(1), llc(2) + 1, llc(3), llc(4) + 1)
      V0011 = f(llc(1), llc(2), llc(3) + 1, llc(4) + 1)
      V1110 = f(llc(1) + 1, llc(2) + 1, llc(3) + 1, llc(4))
      V1101 = f(llc(1) + 1, llc(2) + 1, llc(3), llc(4) + 1)
      V1011 = f(llc(1) + 1, llc(2), llc(3) + 1, llc(4) + 1)
      V0111 = f(llc(1), llc(2) + 1, llc(3) + 1, llc(4) + 1)
      V1111 = f(llc(1) + 1, llc(2) + 1, llc(3) + 1, llc(4) + 1)

      ! Quasi-linear CRRA transformation
      if (crra == 1) then
         V0000 = (1d0 - crracurv)*V0000**(1d0/(1d0 - crracurv))
         V1000 = (1d0 - crracurv)*V1000**(1d0/(1d0 - crracurv))
         V0100 = (1d0 - crracurv)*V0100**(1d0/(1d0 - crracurv))
         V0010 = (1d0 - crracurv)*V0010**(1d0/(1d0 - crracurv))
         V0001 = (1d0 - crracurv)*V0001**(1d0/(1d0 - crracurv))
         V1100 = (1d0 - crracurv)*V1100**(1d0/(1d0 - crracurv))
         V1010 = (1d0 - crracurv)*V1010**(1d0/(1d0 - crracurv))
         V1001 = (1d0 - crracurv)*V1001**(1d0/(1d0 - crracurv))
         V0110 = (1d0 - crracurv)*V0110**(1d0/(1d0 - crracurv))
         V0101 = (1d0 - crracurv)*V0101**(1d0/(1d0 - crracurv))
         V0011 = (1d0 - crracurv)*V0011**(1d0/(1d0 - crracurv))
         V1110 = (1d0 - crracurv)*V1110**(1d0/(1d0 - crracurv))
         V1101 = (1d0 - crracurv)*V1101**(1d0/(1d0 - crracurv))
         V1011 = (1d0 - crracurv)*V1011**(1d0/(1d0 - crracurv))
         V0111 = (1d0 - crracurv)*V0111**(1d0/(1d0 - crracurv))
         V1111 = (1d0 - crracurv)*V1111**(1d0/(1d0 - crracurv))
      end if

      ! Interpolation
      interp4d_pt = &
         V0000*(1d0 - xin)*(1d0 - yin)*(1d0 - zin)*(1d0 - qin) + &
         V1000*xin*(1d0 - yin)*(1d0 - zin)*(1d0 - qin) + &
         V0100*(1d0 - xin)*yin*(1d0 - zin)*(1d0 - qin) + &
         V0010*(1d0 - xin)*(1d0 - yin)*zin*(1d0 - qin) + &
         V0001*(1d0 - xin)*(1d0 - yin)*(1d0 - zin)*qin + &
         V1100*xin*yin*(1d0 - zin)*(1d0 - qin) + &
         V1010*xin*(1d0 - yin)*zin*(1d0 - qin) + &
         V1001*xin*(1d0 - yin)*(1d0 - zin)*qin + &
         V0110*(1d0 - xin)*yin*zin*(1d0 - qin) + &
         V0101*(1d0 - xin)*yin*(1d0 - zin)*qin + &
         V0011*(1d0 - xin)*(1d0 - yin)*zin*qin + &
         V1110*xin*yin*zin*(1d0 - qin) + &
         V1101*xin*yin*(1d0 - zin)*qin + &
         V1011*xin*(1d0 - yin)*zin*qin + &
         V0111*(1d0 - xin)*yin*zin*qin + &
         V1111*xin*yin*zin*qin

      ! Undo transformation
      if (crra == 1) interp4d_pt = (interp4d_pt**(1d0 - crracurv))/(1d0 - crracurv)

      if (talk_in .eq. 1) then
         if (interp4d_pt > HUGE(interp4d_pt)) then
            print *, 'Large positive value returned in interp4d_pt interpolation:', interp4d_pt
         else if (interp4d_pt < -HUGE(interp4d_pt)) then
            print *, 'Large negative value returned in interp4d_pt interpolation:', interp4d_pt
         else if (interp4d_pt /= interp4d_pt) then
            print *, 'NaN returned in interp4d_pt interpolation:', interp4d_pt
         end if
      end if

   end function interp4d_pt

   ! 5D linear interpolation routine
   real(dp) FUNCTION interp5d(crra, crracurv, nx, x, ny, y, nz, z, nq, q, nw, w, f, xi, yi, zi, qi, wi, talk_in)
      ! set crra=1 if you want quasi-linear interpolation for CRRA

      IMPLICIT NONE
      integer, intent(in) :: crra, nx, ny, nz, nq, nw
      real(dp), intent(in) :: crracurv, x(nx), y(ny), z(nz), q(nq), w(nw), f(nx, ny, nz, nq, nw), xi, yi, zi, qi, wi
      real(dp) :: xin, yin, zin, qin, win
      integer :: llc(5)
      real(dp) :: V00000, V10000, V01000, V00100, V00010, V00001, V11000, V10100, V10010, &
                  V10001, V01100, V01010, V01001, V00110, V00101, V00011, V11100, V11010, V11001, &
                  V10110, V10101, V10011, V01110, V01101, V01011, V00111, V11110, V11101, V11011, &
                  V10111, V01111, V11111

      integer, intent(in), optional :: talk_in
      integer :: talk

      if (present(talk_in)) then
         talk = talk_in
      else
         talk = 1
      end if

! Find grid points
      llc(1) = bisection(x, nx, xi)
      llc(2) = bisection(y, ny, yi)
      llc(3) = bisection(z, nz, zi)
      llc(4) = bisection(q, nq, qi)
      llc(5) = bisection(w, nw, wi)

      ! Normalize points to make the length exactly 1 in each direction
      xin = (xi - x(llc(1)))/(x(llc(1) + 1) - x(llc(1)))
      yin = (yi - y(llc(2)))/(y(llc(2) + 1) - y(llc(2)))
      zin = (zi - z(llc(3)))/(z(llc(3) + 1) - z(llc(3)))
      qin = (qi - q(llc(4)))/(q(llc(4) + 1) - q(llc(4)))
      win = (wi - w(llc(5)))/(w(llc(5) + 1) - w(llc(5)))

      ! Function evaluations
      V00000 = f(llc(1), llc(2), llc(3), llc(4), llc(5))
      V10000 = f(llc(1) + 1, llc(2), llc(3), llc(4), llc(5))
      V01000 = f(llc(1), llc(2) + 1, llc(3), llc(4), llc(5))
      V00100 = f(llc(1), llc(2), llc(3) + 1, llc(4), llc(5))
      V00010 = f(llc(1), llc(2), llc(3), llc(4) + 1, llc(5))
      V00001 = f(llc(1), llc(2), llc(3), llc(4), llc(5) + 1)
      V11000 = f(llc(1) + 1, llc(2) + 1, llc(3), llc(4), llc(5))
      V10100 = f(llc(1) + 1, llc(2), llc(3) + 1, llc(4), llc(5))
      V10010 = f(llc(1) + 1, llc(2), llc(3), llc(4) + 1, llc(5))
      V10001 = f(llc(1) + 1, llc(2), llc(3), llc(4), llc(5) + 1)
      V01100 = f(llc(1), llc(2) + 1, llc(3) + 1, llc(4), llc(5))
      V01010 = f(llc(1), llc(2) + 1, llc(3), llc(4) + 1, llc(5))
      V01001 = f(llc(1), llc(2) + 1, llc(3), llc(4), llc(5) + 1)
      V00110 = f(llc(1), llc(2), llc(3) + 1, llc(4) + 1, llc(5))
      V00101 = f(llc(1), llc(2), llc(3) + 1, llc(4), llc(5) + 1)
      V00011 = f(llc(1), llc(2), llc(3), llc(4) + 1, llc(5) + 1)
      V11100 = f(llc(1) + 1, llc(2) + 1, llc(3) + 1, llc(4), llc(5))
      V11010 = f(llc(1) + 1, llc(2) + 1, llc(3), llc(4) + 1, llc(5))
      V11001 = f(llc(1) + 1, llc(2) + 1, llc(3), llc(4), llc(5) + 1)
      V10110 = f(llc(1) + 1, llc(2), llc(3) + 1, llc(4) + 1, llc(5))
      V10101 = f(llc(1) + 1, llc(2), llc(3) + 1, llc(4), llc(5) + 1)
      V10011 = f(llc(1) + 1, llc(2), llc(3), llc(4) + 1, llc(5) + 1)
      V01110 = f(llc(1), llc(2) + 1, llc(3) + 1, llc(4) + 1, llc(5))
      V01101 = f(llc(1), llc(2) + 1, llc(3) + 1, llc(4), llc(5) + 1)
      V01011 = f(llc(1), llc(2) + 1, llc(3), llc(4) + 1, llc(5) + 1)
      V00111 = f(llc(1), llc(2), llc(3) + 1, llc(4) + 1, llc(5) + 1)
      V11110 = f(llc(1) + 1, llc(2) + 1, llc(3) + 1, llc(4) + 1, llc(5))
      V11101 = f(llc(1) + 1, llc(2) + 1, llc(3) + 1, llc(4), llc(5) + 1)
      V11011 = f(llc(1) + 1, llc(2) + 1, llc(3), llc(4) + 1, llc(5) + 1)
      V10111 = f(llc(1) + 1, llc(2), llc(3) + 1, llc(4) + 1, llc(5) + 1)
      V01111 = f(llc(1), llc(2) + 1, llc(3) + 1, llc(4) + 1, llc(5) + 1)
      V11111 = f(llc(1) + 1, llc(2) + 1, llc(3) + 1, llc(4) + 1, llc(5) + 1)

      ! Quasi-linear CRRA transformation
      if (crra == 1) then
         V00000 = (1d0 - crracurv)*V00000**(1d0/(1d0 - crracurv))
         V10000 = (1d0 - crracurv)*V10000**(1d0/(1d0 - crracurv))
         V01000 = (1d0 - crracurv)*V01000**(1d0/(1d0 - crracurv))
         V00100 = (1d0 - crracurv)*V00100**(1d0/(1d0 - crracurv))
         V00010 = (1d0 - crracurv)*V00010**(1d0/(1d0 - crracurv))
         V00001 = (1d0 - crracurv)*V00001**(1d0/(1d0 - crracurv))
         V11000 = (1d0 - crracurv)*V11000**(1d0/(1d0 - crracurv))
         V10100 = (1d0 - crracurv)*V10100**(1d0/(1d0 - crracurv))
         V10010 = (1d0 - crracurv)*V10010**(1d0/(1d0 - crracurv))
         V10001 = (1d0 - crracurv)*V10001**(1d0/(1d0 - crracurv))
         V01100 = (1d0 - crracurv)*V01100**(1d0/(1d0 - crracurv))
         V01010 = (1d0 - crracurv)*V01010**(1d0/(1d0 - crracurv))
         V01001 = (1d0 - crracurv)*V01001**(1d0/(1d0 - crracurv))
         V00110 = (1d0 - crracurv)*V00110**(1d0/(1d0 - crracurv))
         V00101 = (1d0 - crracurv)*V00101**(1d0/(1d0 - crracurv))
         V00011 = (1d0 - crracurv)*V00011**(1d0/(1d0 - crracurv))
         V11100 = (1d0 - crracurv)*V11100**(1d0/(1d0 - crracurv))
         V11010 = (1d0 - crracurv)*V11010**(1d0/(1d0 - crracurv))
         V11001 = (1d0 - crracurv)*V11001**(1d0/(1d0 - crracurv))
         V10110 = (1d0 - crracurv)*V10110**(1d0/(1d0 - crracurv))
         V10101 = (1d0 - crracurv)*V10101**(1d0/(1d0 - crracurv))
         V10011 = (1d0 - crracurv)*V10011**(1d0/(1d0 - crracurv))
         V01110 = (1d0 - crracurv)*V01110**(1d0/(1d0 - crracurv))
         V01101 = (1d0 - crracurv)*V01101**(1d0/(1d0 - crracurv))
         V01011 = (1d0 - crracurv)*V01011**(1d0/(1d0 - crracurv))
         V00111 = (1d0 - crracurv)*V00111**(1d0/(1d0 - crracurv))
         V11110 = (1d0 - crracurv)*V11110**(1d0/(1d0 - crracurv))
         V11101 = (1d0 - crracurv)*V11101**(1d0/(1d0 - crracurv))
         V11011 = (1d0 - crracurv)*V11011**(1d0/(1d0 - crracurv))
         V10111 = (1d0 - crracurv)*V10111**(1d0/(1d0 - crracurv))
         V01111 = (1d0 - crracurv)*V01111**(1d0/(1d0 - crracurv))
         V11111 = (1d0 - crracurv)*V11111**(1d0/(1d0 - crracurv))
      end if

      ! Interpolation
      interp5d = &
         V00000*(1d0 - xin)*(1d0 - yin)*(1d0 - zin)*(1d0 - qin)*(1d0 - win) + &
         V10000*xin*(1d0 - yin)*(1d0 - zin)*(1d0 - qin)*(1d0 - win) + &
         V01000*(1d0 - xin)*yin*(1d0 - zin)*(1d0 - qin)*(1d0 - win) + &
         V00100*(1d0 - xin)*(1d0 - yin)*zin*(1d0 - qin)*(1d0 - win) + &
         V00010*(1d0 - xin)*(1d0 - yin)*(1d0 - zin)*qin*(1d0 - win) + &
         V00001*(1d0 - xin)*(1d0 - yin)*(1d0 - zin)*(1d0 - qin)*win + &
         V11000*xin*yin*(1d0 - zin)*(1d0 - qin)*(1d0 - win) + &
         V10100*xin*(1d0 - yin)*zin*(1d0 - qin)*(1d0 - win) + &
         V10010*xin*(1d0 - yin)*(1d0 - zin)*qin*(1d0 - win) + &
         V10001*xin*(1d0 - yin)*(1d0 - zin)*(1d0 - qin)*win + &
         V01100*(1d0 - xin)*yin*zin*(1d0 - qin)*(1d0 - win) + &
         V01010*(1d0 - xin)*yin*(1d0 - zin)*qin*(1d0 - win) + &
         V01001*(1d0 - xin)*yin*(1d0 - zin)*(1d0 - qin)*win + &
         V00110*(1d0 - xin)*(1d0 - yin)*zin*qin*(1d0 - win) + &
         V00101*(1d0 - xin)*(1d0 - yin)*zin*(1d0 - qin)*win + &
         V00011*(1d0 - xin)*(1d0 - yin)*(1d0 - zin)*qin*win + &
         V11100*xin*yin*zin*(1d0 - qin)*(1d0 - win) + &
         V11010*xin*yin*(1d0 - zin)*qin*(1d0 - win) + &
         V11001*xin*yin*(1d0 - zin)*(1d0 - qin)*win + &
         V10110*xin*(1d0 - yin)*zin*qin*(1d0 - win) + &
         V10101*xin*(1d0 - yin)*zin*(1d0 - qin)*win + &
         V10011*xin*(1d0 - yin)*(1d0 - zin)*qin*win + &
         V01110*(1d0 - xin)*yin*zin*qin*(1d0 - win) + &
         V01101*(1d0 - xin)*yin*zin*(1d0 - qin)*win + &
         V01011*(1d0 - xin)*yin*(1d0 - zin)*qin*win + &
         V00111*(1d0 - xin)*(1d0 - yin)*zin*qin*win + &
         V11110*xin*yin*zin*qin*(1d0 - win) + &
         V11101*xin*yin*zin*(1d0 - qin)*win + &
         V11011*xin*yin*(1d0 - zin)*qin*win + &
         V10111*xin*(1d0 - yin)*zin*qin*win + &
         V01111*(1d0 - xin)*yin*zin*qin*win + &
         V11111*xin*yin*zin*qin*win

      ! Undo transformation
      if (crra == 1) interp5d = (interp5d**(1d0 - crracurv))/(1d0 - crracurv)

      if (talk_in .eq. 1) then
         if (interp5d > HUGE(interp5d)) then
            print *, 'Large positive value returned in interp5d interpolation:', interp5d
         else if (interp5d < -HUGE(interp5d)) then
            print *, 'Large negative value returned in interp5d interpolation:', interp5d
         else if (interp5d /= interp5d) then
            print *, 'NaN returned in interp5d interpolation:', interp5d
         end if
      end if

   end function interp5d

!################################################################################################################################
! Grid creation functions
!################################################################################################################################

   SUBROUTINE PowerSpacedGrid(n, k, low, high, y)
      !gives a grid spaced between low and high based on the unit interval with a function x^(1/k)
      !k = 1 is linear, k = 0 is L-shaped
      IMPLICIT NONE
      INTEGER, INTENT(in)        :: n
      real(dp), INTENT(in)        :: k, low, high
      real(dp), INTENT(out) :: y(:)
      INTEGER                                :: i
      real(dp)                                :: x(n), z(n)
      IF (n < 2) THEN
         write (*, *) 'n must be at least 2 to make grids'
         return
      END IF
      IF (n == 2) THEN
         y(1) = low
         y(2) = high
         return
      END IF
      x(1) = 0d0
      x(n) = 1d0
      DO i = 2, n - 1
         x(i) = real(i - 1, kind=dp)/real(n - 1, kind=dp)
      END DO
      z = x**(1d0/k)
      y = low + (high - low)*z
   END SUBROUTINE PowerSpacedGrid

   SUBROUTINE DoublePowerSpacedGrid(n, k, low, high, y, nneg)
      IMPLICIT NONE
      INTEGER, INTENT(in)        :: n, nneg
      real(dp), INTENT(in)        :: k, low, high
      real(dp), INTENT(out) :: y(:)
      real(dp) :: yneg(nneg + 1), ypos(n - nneg), zero
      integer :: i
      zero = 0d0
      ! Positive side
      CALL PowerSpacedGrid(n - nneg, k, zero, high, ypos)
      ! Negative side
      CALL PowerSpacedGrid(nneg + 1, k, zero, -low, yneg)
      ! Combine the two
      do i = 1, nneg
         y(i) = -yneg(nneg + 2 - i)
      end do
      do i = 1, n - nneg
         y(nneg + i) = ypos(i)
      end do
   END SUBROUTINE DoublePowerSpacedGrid

!################################################################################################################################
! Code to sort an array
! Source: https://gist.github.com/1AdAstra1/6f7785373efe5bb6c254d2e20c78ccc4
! Downloaded by Tim on 11/10/22
!################################################################################################################################
   recursive subroutine quicksort(a)
      IMPLICIT NONE
      real(dp) :: a(:)
      real(dp) x, t
      integer :: first = 1, last
      integer i, j

      last = size(a, 1)
      x = a((first + last)/2)
      i = first
      j = last

      do
         do while (a(i) < x)
            i = i + 1
         end do
         do while (x < a(j))
            j = j - 1
         end do
         if (i >= j) exit
         t = a(i); a(i) = a(j); a(j) = t
         i = i + 1
         j = j - 1
      end do

      if (first < i - 1) call quicksort(a(first:i - 1))
      if (j + 1 < last) call quicksort(a(j + 1:last))
   end subroutine quicksort

!################################################################################################################################
! Functions to combine parameter vectors into one big vector for parrallelization
!################################################################################################################################

   ! 3D
   subroutine combine_loop3d(nx, x, ny, y, nz, z, xxx, yyy, zzz)
      IMPLICIT NONE

      integer, intent(in) :: nx, ny, nz
      real(dp), intent(in) :: x(nx), y(ny), z(nz)
      real(dp), intent(out) :: xxx(nx*ny*nz), yyy(nx*ny*nz), zzz(nx*ny*nz)
      real(dp), dimension(nx, ny, nz) :: xx, yy, zz
      integer :: i, j, k

      do j = 1, ny
         do k = 1, nz
            xx(:, j, k) = x
         end do
      end do

      do i = 1, nx
         do k = 1, nz
            yy(i, :, k) = y
         end do
      end do

      do i = 1, nx
         do j = 1, ny
            zz(i, j, :) = z
         end do
      end do

      xxx = reshape(xx, (/nx*ny*nz/))
      yyy = reshape(yy, (/nx*ny*nz/))
      zzz = reshape(zz, (/nx*ny*nz/))

   end subroutine combine_loop3d

   ! 4D
   subroutine combine_loop4d(nx, x, ny, y, nz, z, nf, f, xxx, yyy, zzz, fff)
      IMPLICIT NONE

      integer, intent(in) :: nx, ny, nz, nf
      real(dp), intent(in) :: x(nx), y(ny), z(nz), f(nf)
      real(dp), intent(out) :: xxx(nx*ny*nz*nf), yyy(nx*ny*nz*nf), zzz(nx*ny*nz*nf), fff(nx*ny*nz*nf)
      real(dp), dimension(nx, ny, nz, nf) :: xx, yy, zz, ff
      integer :: i, j, k, l

      do j = 1, ny
         do k = 1, nz
            do l = 1, nf
               xx(:, j, k, l) = x
            end do
         end do
      end do

      do i = 1, nx
         do k = 1, nz
            do l = 1, nf
               yy(i, :, k, l) = y
            end do
         end do
      end do

      do i = 1, nx
         do j = 1, ny
            do l = 1, nf
               zz(i, j, :, l) = z
            end do
         end do
      end do

      do i = 1, nx
         do j = 1, ny
            do k = 1, nz
               ff(i, j, k, :) = f
            end do
         end do
      end do

      xxx = reshape(xx, (/nx*ny*nz*nf/))
      yyy = reshape(yy, (/nx*ny*nz*nf/))
      zzz = reshape(zz, (/nx*ny*nz*nf/))
      fff = reshape(ff, (/nx*ny*nz*nf/))

   end subroutine combine_loop4d

   ! 5D
   subroutine combine_loop5d(nx, x, ny, y, nz, z, nf, f, ng, g, xxx, yyy, zzz, fff, ggg)
      IMPLICIT NONE

      integer, intent(in) :: nx, ny, nz, nf, ng
      real(dp), intent(in) :: x(nx), y(ny), z(nz), f(nf), g(ng)
      real(dp), intent(out) :: xxx(nx*ny*nz*nf*ng), yyy(nx*ny*nz*nf*ng), zzz(nx*ny*nz*nf*ng), &
                               fff(nx*ny*nz*nf*ng), ggg(nx*ny*nz*nf*ng)
      real(dp), dimension(nx, ny, nz, nf, ng) :: xx, yy, zz, ff, gg
      integer :: i, j, k, l, m

      do j = 1, ny
         do k = 1, nz
            do l = 1, nf
               do m = 1, ng
                  xx(:, j, k, l, m) = x
               end do
            end do
         end do
      end do

      do i = 1, nx
         do k = 1, nz
            do l = 1, nf
               do m = 1, ng
                  yy(i, :, k, l, m) = y
               end do
            end do
         end do
      end do

      do i = 1, nx
         do j = 1, ny
            do l = 1, nf
               do m = 1, ng
                  zz(i, j, :, l, m) = z
               end do
            end do
         end do
      end do

      do i = 1, nx
         do j = 1, ny
            do k = 1, nz
               do m = 1, ng
                  ff(i, j, k, :, m) = f
               end do
            end do
         end do
      end do

      do i = 1, nx
         do j = 1, ny
            do k = 1, nz
               do l = 1, nf
                  gg(i, j, k, l, :) = g
               end do
            end do
         end do
      end do

      xxx = reshape(xx, (/nx*ny*nz*nf*ng/))
      yyy = reshape(yy, (/nx*ny*nz*nf*ng/))
      zzz = reshape(zz, (/nx*ny*nz*nf*ng/))
      fff = reshape(ff, (/nx*ny*nz*nf*ng/))
      ggg = reshape(gg, (/nx*ny*nz*nf*ng/))

   end subroutine combine_loop5d

   ! 6D
   subroutine combine_loop6d(nx, x, ny, y, nz, z, nf, f, ng, g, nh, h, &
                             xxx, yyy, zzz, fff, ggg, hhh)
      IMPLICIT NONE

      integer, intent(in) :: nx, ny, nz, nf, ng, nh
      real(dp), intent(in) :: x(nx), y(ny), z(nz), f(nf), g(ng), h(nh)
      real(dp), intent(out) :: xxx(nx*ny*nz*nf*ng*nh), yyy(nx*ny*nz*nf*ng*nh), &
                               zzz(nx*ny*nz*nf*ng*nh), fff(nx*ny*nz*nf*ng*nh), &
                               ggg(nx*ny*nz*nf*ng*nh), hhh(nx*ny*nz*nf*ng*nh)
      real(dp), dimension(nx, ny, nz, nf, ng, nh) :: xx, yy, zz, ff, gg, hh
      integer :: i, j, k, l, m, n

      do j = 1, ny
         do k = 1, nz
            do l = 1, nf
               do m = 1, ng
                  do n = 1, nh
                     xx(:, j, k, l, m, n) = x
                  end do
               end do
            end do
         end do
      end do

      do i = 1, nx
         do k = 1, nz
            do l = 1, nf
               do m = 1, ng
                  do n = 1, nh
                     yy(i, :, k, l, m, n) = y
                  end do
               end do
            end do
         end do
      end do

      do i = 1, nx
         do j = 1, ny
            do l = 1, nf
               do m = 1, ng
                  do n = 1, nh
                     zz(i, j, :, l, m, n) = z
                  end do
               end do
            end do
         end do
      end do

      do i = 1, nx
         do j = 1, ny
            do k = 1, nz
               do m = 1, ng
                  do n = 1, nh
                     ff(i, j, k, :, m, n) = f
                  end do
               end do
            end do
         end do
      end do

      do i = 1, nx
         do j = 1, ny
            do k = 1, nz
               do l = 1, nf
                  do n = 1, nh
                     gg(i, j, k, l, :, n) = g
                  end do
               end do
            end do
         end do
      end do

      do i = 1, nx
         do j = 1, ny
            do k = 1, nz
               do l = 1, nf
                  do m = 1, ng
                     hh(i, j, k, l, m, :) = h
                  end do
               end do
            end do
         end do
      end do

      xxx = reshape(xx, (/nx*ny*nz*nf*ng*nh/))
      yyy = reshape(yy, (/nx*ny*nz*nf*ng*nh/))
      zzz = reshape(zz, (/nx*ny*nz*nf*ng*nh/))
      fff = reshape(ff, (/nx*ny*nz*nf*ng*nh/))
      ggg = reshape(gg, (/nx*ny*nz*nf*ng*nh/))
      hhh = reshape(hh, (/nx*ny*nz*nf*ng*nh/))

   end subroutine combine_loop6d

   ! Function to produce an array of rows of parameters to search over
   subroutine combine_gs(fix, out_uq, &
                         grid1, grid2, grid3, grid4, grid5, grid6, grid7, grid8, grid9, grid10, &
                         grid11, grid12, grid13, grid14, grid15, grid16, grid17, grid18, grid19, grid20, &
                         grid21, grid22, grid23, grid24, grid25, grid26, grid27, grid28, grid29, grid30, &
                         grid31, grid32, grid33, grid34, grid35, grid36, grid37, grid38, grid39, grid40)

      IMPLICIT NONE

      real(dp), intent(in) :: fix(:) ! value to fix other parameters at
      real(dp), allocatable, intent(out) :: out_uq(:, :) ! matrix output where each row is a parameter vector
      real(dp), allocatable :: out(:, :)

! Optional grids of parameters
      real(dp), intent(in), optional, dimension(:) :: &
         grid1, grid2, grid3, grid4, grid5, grid6, grid7, grid8, grid9, grid10, &
         grid11, grid12, grid13, grid14, grid15, grid16, grid17, grid18, grid19, grid20, &
         grid21, grid22, grid23, grid24, grid25, grid26, grid27, grid28, grid29, grid30, &
         grid31, grid32, grid33, grid34, grid35, grid36, grid37, grid38, grid39, grid40
      integer :: i, j, count

! Length of parameters
      integer :: len(40)

      if (present(grid1)) then
         len(1) = size(grid1)
      else
         len(1) = 0
      end if

      if (present(grid2)) then
         len(2) = size(grid2)
      else
         len(2) = 0
      end if

      if (present(grid3)) then
         len(3) = size(grid3)
      else
         len(3) = 0
      end if

      if (present(grid4)) then
         len(4) = size(grid4)
      else
         len(4) = 0
      end if

      if (present(grid5)) then
         len(5) = size(grid5)
      else
         len(5) = 0
      end if

      if (present(grid6)) then
         len(6) = size(grid6)
      else
         len(6) = 0
      end if

      if (present(grid7)) then
         len(7) = size(grid7)
      else
         len(7) = 0
      end if

      if (present(grid8)) then
         len(8) = size(grid8)
      else
         len(8) = 0
      end if

      if (present(grid8)) then
         len(8) = size(grid8)
      else
         len(8) = 0
      end if

      if (present(grid9)) then
         len(9) = size(grid9)
      else
         len(9) = 0
      end if

      if (present(grid10)) then
         len(10) = size(grid10)
      else
         len(10) = 0
      end if

      if (present(grid11)) then
         len(11) = size(grid11)
      else
         len(11) = 0
      end if

      if (present(grid12)) then
         len(12) = size(grid12)
      else
         len(12) = 0
      end if

      if (present(grid13)) then
         len(13) = size(grid13)
      else
         len(13) = 0
      end if

      if (present(grid14)) then
         len(14) = size(grid14)
      else
         len(14) = 0
      end if

      if (present(grid15)) then
         len(15) = size(grid15)
      else
         len(15) = 0
      end if

      if (present(grid16)) then
         len(16) = size(grid16)
      else
         len(16) = 0
      end if

      if (present(grid17)) then
         len(17) = size(grid17)
      else
         len(17) = 0
      end if

      if (present(grid18)) then
         len(18) = size(grid18)
      else
         len(18) = 0
      end if

      if (present(grid19)) then
         len(19) = size(grid19)
      else
         len(19) = 0
      end if

      if (present(grid20)) then
         len(20) = size(grid20)
      else
         len(20) = 0
      end if

      if (present(grid21)) then
         len(21) = size(grid21)
      else
         len(21) = 0
      end if

      if (present(grid22)) then
         len(22) = size(grid22)
      else
         len(22) = 0
      end if

      if (present(grid23)) then
         len(23) = size(grid23)
      else
         len(23) = 0
      end if

      if (present(grid24)) then
         len(24) = size(grid24)
      else
         len(24) = 0
      end if

      if (present(grid25)) then
         len(25) = size(grid25)
      else
         len(25) = 0
      end if

      if (present(grid26)) then
         len(26) = size(grid26)
      else
         len(26) = 0
      end if

      if (present(grid27)) then
         len(27) = size(grid27)
      else
         len(27) = 0
      end if

      if (present(grid28)) then
         len(28) = size(grid28)
      else
         len(28) = 0
      end if

      if (present(grid29)) then
         len(29) = size(grid29)
      else
         len(29) = 0
      end if

      if (present(grid30)) then
         len(30) = size(grid30)
      else
         len(30) = 0
      end if

      if (present(grid31)) then
         len(31) = size(grid31)
      else
         len(31) = 0
      end if

      if (present(grid32)) then
         len(32) = size(grid32)
      else
         len(32) = 0
      end if

      if (present(grid33)) then
         len(33) = size(grid33)
      else
         len(33) = 0
      end if

      if (present(grid34)) then
         len(34) = size(grid34)
      else
         len(34) = 0
      end if

      if (present(grid35)) then
         len(35) = size(grid35)
      else
         len(35) = 0
      end if

      if (present(grid36)) then
         len(36) = size(grid36)
      else
         len(36) = 0
      end if

      if (present(grid37)) then
         len(37) = size(grid37)
      else
         len(37) = 0
      end if

      if (present(grid38)) then
         len(38) = size(grid38)
      else
         len(38) = 0
      end if

      if (present(grid39)) then
         len(39) = size(grid39)
      else
         len(39) = 0
      end if

      if (present(grid39)) then
         len(40) = size(grid40)
      else
         len(40) = 0
      end if

      ! Allocate size of output array
      allocate (out(SUM(len) + 1, SIZE(fix)))

      ! Allocate elements to array
      out(1, :) = fix
      count = 1

      if (len(1) > 0) then
      do j = 1, len(1)
         i = j + count
         out(i, :) = fix
         out(i, 1) = grid1(j)
      end do
      end if
      count = i

      if (len(2) > 0) then
      do j = 1, len(2)
         i = j + count
         out(i, :) = fix
         out(i, 2) = grid2(j)
      end do
      end if
      count = i

      if (len(3) > 0) then
         do j = 1, len(3)
            i = j + count
            out(i, :) = fix
            out(i, 3) = grid3(j)
         end do
      end if
      count = i

      if (len(4) > 0) then
         do j = 1, len(4)
            i = j + count
            out(i, :) = fix
            out(i, 4) = grid4(j)
         end do
      end if
      count = i

      if (len(5) > 0) then
         do j = 1, len(5)
            i = j + count
            out(i, :) = fix
            out(i, 5) = grid5(j)
         end do
      end if
      count = i

      if (len(6) > 0) then
         do j = 1, len(6)
            i = j + count
            out(i, :) = fix
            out(i, 6) = grid6(j)
         end do
      end if
      count = i

      if (len(7) > 0) then
         do j = 1, len(7)
            i = j + count
            out(i, :) = fix
            out(i, 7) = grid7(j)
         end do
      end if
      count = i

      if (len(8) > 0) then
         do j = 1, len(8)
            i = j + count
            out(i, :) = fix
            out(i, 8) = grid8(j)
         end do
      end if
      count = i

      if (len(9) > 0) then
         do j = 1, len(9)
            i = j + count
            out(i, :) = fix
            out(i, 9) = grid9(j)
         end do
      end if
      count = i

      if (len(10) > 0) then
         do j = 1, len(10)
            i = j + count
            out(i, :) = fix
            out(i, 10) = grid10(j)
         end do
      end if
      count = i

      if (len(11) > 0) then
         do j = 1, len(11)
            i = j + count
            out(i, :) = fix
            out(i, 11) = grid11(j)
         end do
      end if
      count = i

      if (len(12) > 0) then
         do j = 1, len(12)
            i = j + count
            out(i, :) = fix
            out(i, 12) = grid12(j)
         end do
      end if
      count = i

      if (len(13) > 0) then
         do j = 1, len(13)
            i = j + count
            out(i, :) = fix
            out(i, 13) = grid13(j)
         end do
      end if
      count = i

      if (len(14) > 0) then
         do j = 1, len(14)
            i = j + count
            out(i, :) = fix
            out(i, 14) = grid14(j)
         end do
      end if
      count = i

      if (len(15) > 0) then
         do j = 1, len(15)
            i = j + count
            out(i, :) = fix
            out(i, 15) = grid15(j)
         end do
      end if
      count = i

      if (len(16) > 0) then
         do j = 1, len(16)
            i = j + count
            out(i, :) = fix
            out(i, 16) = grid16(j)
         end do
      end if
      count = i

      if (len(17) > 0) then
         do j = 1, len(17)
            i = j + count
            out(i, :) = fix
            out(i, 17) = grid17(j)
         end do
      end if
      count = i

      if (len(18) > 0) then
         do j = 1, len(18)
            i = j + count
            out(i, :) = fix
            out(i, 18) = grid18(j)
         end do
      end if
      count = i

      if (len(19) > 0) then
         do j = 1, len(19)
            i = j + count
            out(i, :) = fix
            out(i, 19) = grid19(j)
         end do
      end if
      count = i

      if (len(20) > 0) then
         do j = 1, len(20)
            i = j + count
            out(i, :) = fix
            out(i, 20) = grid20(j)
         end do
      end if
      count = i

      if (len(21) > 0) then
         do j = 1, len(21)
            i = j + count
            out(i, :) = fix
            out(i, 21) = grid21(j)
         end do
      end if
      count = i

      if (len(22) > 0) then
         do j = 1, len(22)
            i = j + count
            out(i, :) = fix
            out(i, 22) = grid22(j)
         end do
      end if
      count = i

      if (len(23) > 0) then
         do j = 1, len(23)
            i = j + count
            out(i, :) = fix
            out(i, 23) = grid23(j)
         end do
      end if
      count = i

      if (len(24) > 0) then
         do j = 1, len(24)
            i = j + count
            out(i, :) = fix
            out(i, 24) = grid24(j)
         end do
      end if
      count = i

      if (len(25) > 0) then
         do j = 1, len(25)
            i = j + count
            out(i, :) = fix
            out(i, 25) = grid25(j)
         end do
      end if
      count = i

      if (len(26) > 0) then
         do j = 1, len(26)
            i = j + count
            out(i, :) = fix
            out(i, 26) = grid26(j)
         end do
      end if
      count = i

      if (len(27) > 0) then
         do j = 1, len(27)
            i = j + count
            out(i, :) = fix
            out(i, 27) = grid27(j)
         end do
      end if
      count = i

      if (len(28) > 0) then
         do j = 1, len(28)
            i = j + count
            out(i, :) = fix
            out(i, 28) = grid28(j)
         end do
      end if
      count = i

      if (len(29) > 0) then
         do j = 1, len(29)
            i = j + count
            out(i, :) = fix
            out(i, 29) = grid29(j)
         end do
      end if
      count = i

      if (len(30) > 0) then
         do j = 1, len(30)
            i = j + count
            out(i, :) = fix
            out(i, 30) = grid30(j)
         end do
      end if
      count = i

      if (len(31) > 0) then
         do j = 1, len(31)
            i = j + count
            out(i, :) = fix
            out(i, 31) = grid31(j)
         end do
      end if
      count = i

      if (len(32) > 0) then
         do j = 1, len(32)
            i = j + count
            out(i, :) = fix
            out(i, 32) = grid32(j)
         end do
      end if
      count = i

      if (len(33) > 0) then
         do j = 1, len(33)
            i = j + count
            out(i, :) = fix
            out(i, 33) = grid33(j)
         end do
      end if
      count = i

      if (len(34) > 0) then
         do j = 1, len(34)
            i = j + count
            out(i, :) = fix
            out(i, 34) = grid34(j)
         end do
      end if
      count = i

      if (len(35) > 0) then
         do j = 1, len(35)
            i = j + count
            out(i, :) = fix
            out(i, 35) = grid35(j)
         end do
      end if
      count = i

      if (len(36) > 0) then
         do j = 1, len(36)
            i = j + count
            out(i, :) = fix
            out(i, 36) = grid36(j)
         end do
      end if
      count = i

      if (len(37) > 0) then
         do j = 1, len(37)
            i = j + count
            out(i, :) = fix
            out(i, 37) = grid37(j)
         end do
      end if
      count = i

      if (len(38) > 0) then
         do j = 1, len(38)
            i = j + count
            out(i, :) = fix
            out(i, 38) = grid38(j)
         end do
      end if
      count = i

      if (len(39) > 0) then
         do j = 1, len(39)
            i = j + count
            out(i, :) = fix
            out(i, 39) = grid39(j)
         end do
      end if
      count = i

      if (len(40) > 0) then
         do j = 1, len(40)
            i = j + count
            out(i, :) = fix
            out(i, 40) = grid40(j)
         end do
      end if
      count = i

      ! Drop duplicates
      CALL drop_duplicates(out, out_uq)
      deallocate (out)

   end subroutine combine_gs

   ! Removes duplicate values from an array
   subroutine drop_duplicates(in, out)
      IMPLICIT NONE
      real(dp), dimension(:, :), intent(in) :: in
      real(dp), dimension(:, :), allocatable, intent(out) :: out
      real(dp), allocatable, dimension(:, :) :: tmp
      integer :: nrows, ncols, i, j, dup, n_unique

      ! Get array size
      nrows = size(in, 1)
      ncols = size(in, 2)
      allocate (tmp(nrows, ncols))
      tmp = 0d0

      ! Loop over rows of array
      tmp(1, :) = in(1, :)
      n_unique = 1
      do i = 2, nrows
         dup = 0
         do j = 1, i - 1
            if (all(in(i, :) == in(j, :))) then
               dup = dup + 1
            end if
         end do
         if (dup .eq. 0) then
            n_unique = n_unique + 1
            tmp(n_unique, :) = in(i, :)
         end if
      end do

      ! Fill unique array
      allocate (out(n_unique, ncols))
      out = tmp(:n_unique, :)
      deallocate (tmp)

   end subroutine drop_duplicates

   !##############################################################################
   ! The following functions are too assert equations in different dimensions:
   ! The goal is check whether two integers are the same and return that value
   ! if it's true
!
   ! Github download: https://github.com/fabiankindermann/ce-fortran/
   ! Adapted by Tim on 10/29/21
   !##############################################################################

   ! 2d
   function assert_eq2(n1, n2, string)
      integer, intent(in) :: n1, n2
      character(len=*), intent(in) :: string
      integer :: assert_eq2
      if (n1 == n2) then
         assert_eq2 = n1
      else
         write (*, *) 'Error: an assert_eq2 failed with this tag:', string
         STOP 'program terminated by assert_eq2'
      end if
   end function assert_eq2

   ! 3d
   function assert_eq3(n1, n2, n3, string)
      integer, intent(in) :: n1, n2, n3
      character(len=*), intent(in) :: string
      integer :: assert_eq3
      if (n1 == n2 .and. n2 == n3) then
         assert_eq3 = n1
      else
         write (*, *) 'Error: an assert_eq3 failed with this tag:', string
         STOP 'program terminated by assert_eq3'
      end if
   end function assert_eq3

! 4d
   function assert_eq4(n1, n2, n3, n4, string)
      integer, intent(in) :: n1, n2, n3, n4
      character(len=*), intent(in) :: string
      integer :: assert_eq4
      if (n1 == n2 .and. n2 == n3 .and. n3 == n4) then
         assert_eq4 = n1
      else
         write (*, *) 'Error: an assert_eq4 failed with this tag:', string
         STOP 'program terminated by assert_eq4'
      end if
   end function assert_eq4

! 5d
   function assert_eq5(n1, n2, n3, n4, n5, string)
      integer, intent(in) :: n1, n2, n3, n4, n5
      character(len=*), intent(in) :: string
      integer :: assert_eq5
      if (n1 == n2 .and. n2 == n3 .and. n3 == n4 .and. n4 == n5) then
         assert_eq5 = n1
      else
         write (*, *) 'Error: an assert_eq5 failed with this tag:', string
         STOP 'program terminated by assert_eq5'
      end if
   end function assert_eq5

! N
   function assert_eqn(nn, string)
      integer, intent(in) :: nn(:)
      character(len=*), intent(in) :: string
      integer :: assert_eqn
      if (all(nn(2:) == nn(1))) then
         assert_eqn = nn(1)
      else
         write (*, *) 'Error: assert_eqn failed with this tag:', string
         STOP 'program terminated by assert_eqn'
      end if
   end function assert_eqn

!################################################################################################################################
! The next three functions are adapted from Numerical Recipes to get the Gauss-Hermite wi and xi
!################################################################################################################################

   FUNCTION arth(first, increment, n)
      INTEGER, INTENT(IN) :: first, increment, n
      INTEGER, PARAMETER :: NPAR_ARTH = 16, NPAR2_ARTH = 8
      INTEGER, DIMENSION(n) :: arth
      INTEGER :: k, k2, temp
      if (n > 0) arth(1) = first
      if (n <= NPAR_ARTH) then
         do k = 2, n
            arth(k) = arth(k - 1) + increment
         end do
      else
         do k = 2, NPAR2_ARTH
            arth(k) = arth(k - 1) + increment
         end do
         temp = increment*NPAR2_ARTH
         k = NPAR2_ARTH
         do
            if (k >= n) exit
            k2 = k + k
            arth(k + 1:min(k2, n)) = temp + arth(1:min(k, n - k))
            temp = temp + temp
            k = k2
         end do
      end if
   END FUNCTION arth

! Given n, this routine returns arrays x(1:n) and w(1:n) containing the abscissas and
! weights of the n-point Gauss-Hermite quadrature formula. The largest abscissa is returned
! in x(1), the most negative in x(n). These results are USEd to approximate an integral of
! the form \int_{-\infty}^\infty e^{-x^2}f(x)dx with \sum_{j=1}^n w(j)*f[x(j)].
! n is determined based on the variables x and w that you input below
   SUBROUTINE gauher(x, w)
      IMPLICIT NONE
      real(dp), DIMENSION(:), INTENT(OUT) :: x, w
      real(dp), PARAMETER :: EPS = 3.0d-13, PIM4 = 0.7511255444649425d0, PI = 3.141592653589793238462643383279502884197d0
      INTEGER :: its, j, m, n
      INTEGER, PARAMETER :: MAXIT = 10
      real(dp) :: anu
      real(dp), PARAMETER :: C1 = 9.084064d-01, C2 = 5.214976d-02, &
                             C3 = 2.579930d-03, C4 = 3.986126d-03
      real(dp), DIMENSION((size(x) + 1)/2) :: rhs, r2, r3, theta
      real(dp), DIMENSION((size(x) + 1)/2) :: p1, p2, p3, pp, z, z1
      LOGICAL, DIMENSION((size(x) + 1)/2) :: unfinished
      n = assert_eq2(size(x), size(w), 'gauher')
      m = (n + 1)/2
      anu = 2d0*real(n, kind=dp) + 1d0
      rhs = arth(3, 4, m)*PI/anu
      r3 = rhs**(1d0/3d0)
      r2 = r3**2
      theta = r3*(C1 + r2*(C2 + r2*(C3 + r2*C4)))
      z = sqrt(anu)*cos(theta)
      unfinished = .true.
      do its = 1, MAXIT
         where (unfinished)
            p1 = PIM4
            p2 = 0d0
         end where
         do j = 1, n
            where (unfinished)
               p3 = p2
               p2 = p1
               p1 = z*sqrt(2d0/real(j, kind=dp))*p2 - sqrt(real(j - 1, kind=dp)/real(j, kind=dp))*p3
            end where
         end do
         where (unfinished)
            pp = sqrt(2d0*real(n, kind=dp))*p2
            z1 = z
            z = z1 - p1/pp
            unfinished = (abs(z - z1) > EPS)
         end where
         if (.not. any(unfinished)) exit
      end do
      if (its == MAXIT + 1) print *, "too many iterations in gauher"
      x(1:m) = z
      x(n:n - m + 1:-1) = -z
      w(1:m) = 2d0/pp**2
      w(n:n - m + 1:-1) = w(1:m)
   END SUBROUTINE gauher

! This function returns the Gauss-Hermite nodes and weights in order to use Gauss-Hermite to
! to integrate a function with respect to a normal random variable with mean mu and std sigma.
! Assuming y is N(mu, sigma) and we want to calculate E(f(y)), using the definition of a normal
! density and the change of variable x = (y - mu)/(sqrt(2)*sigma), we can calculate this integral
! as \sum_{i=1}^n\Tilde{w}_if(\Tilde{x}_i), where \Tilde{w} and \Tilde{x} are the appropriately
! scaled GH weights and nodes: \Tilde{w} = PI^{-0.5}*w, \Tilde{x} = mu + sqrt(2)*sigma*x
   SUBROUTINE gauher_normal(mu, sigma, x, w)
      IMPLICIT NONE
      real(dp), INTENT(IN) :: mu, sigma
      real(dp), DIMENSION(:), INTENT(OUT) :: x, w
      real(dp), PARAMETER :: PI = 3.141592653589793238462643383279502884197d0
      integer :: n, ni
      real(dp), DIMENSION(size(x)) :: x0, w0
      n = assert_eq2(size(x), size(w), 'gauher_normal')
      CALL gauher(x0, w0)
      do ni = 1, n ! loop to adjust nodes from standard normal - see Greenwald notes
         x(ni) = mu + sqrt(2d0)*sigma*x0(ni)
         w(ni) = (PI**(-0.5d0))*w0(ni)
      end do
   END SUBROUTINE

!################################################################################################################################
! Functions for random numbers
!################################################################################################################################

   SUBROUTINE RandomDiscrete(Nout, Xout, Nin, Pin)
      !generates Nout random draws from the integers 1 to Nin
      !using probabilities in Pin
      IMPLICIT NONE
      INTEGER, INTENT(in)                        :: Nout, Nin
      INTEGER, INTENT(out)                :: Xout(:)
      real(dp), INTENT(in)  :: Pin(:)

      INTEGER                        ::i1, i2
      real(dp)      :: lran(Nout)

      !IF(sum(Pin) .ne. 1.0) write(*,*) 'error in RandomDiscrete: Pin doesnt sum to 1.0'

      CALL RANDOM_NUMBER(lran)

      Xout(:) = 0
      DO i1 = 1, Nout
         IF (lran(i1) .le. Pin(1)) THEN
            Xout(i1) = 1
         ELSE
            i2 = 2
            DO WHILE (i2 .le. Nin)
               IF ((lran(i1) .le. SUM(Pin(1:i2))) .and. (lran(i1) > SUM(Pin(1:i2 - 1)))) THEN
                  Xout(i1) = i2
                  i2 = Nin + 1
               ELSE
                  i2 = i2 + 1
               END IF
            END DO
         END IF
      END DO

   END SUBROUTINE RandomDiscrete

   SUBROUTINE RandomDiscrete1(Xout, Nin, Pin)
      !generates one random draw from the integers 1 to Nin
      !using probabilities in Pin
      IMPLICIT NONE
      INTEGER, INTENT(in)                        :: Nin
      INTEGER, INTENT(out)                :: Xout
      real(dp), INTENT(in)  ::Pin(:)

      INTEGER                        ::i2
      real(dp)      :: lran

      !IF(sum(Pin) .ne. 1.0) write(*,*) 'error in RandomDiscrete: Pin doesnt sum to 1.0'

      CALL RANDOM_NUMBER(lran)

      Xout = 0
      IF (lran .le. Pin(1)) THEN
         Xout = 1
      ELSE
         i2 = 2
         DO WHILE (i2 .le. Nin)
            IF ((lran .le. SUM(Pin(1:i2))) .and. (lran > SUM(Pin(1:i2 - 1)))) THEN
               Xout = i2
               i2 = Nin + 1
            ELSE
               i2 = i2 + 1
            END IF
         END DO
      END IF
   END SUBROUTINE RandomDiscrete1


!################################################################################################################################
! Find root of one-dimensional function by Bi-Section Method
! Author: Section 9.1 of Numerical Recipes
! Adapted by Tim on 10/10/22
!################################################################################################################################

   real(dp) function rtbis(func, x1, x2, xacc, jmax, flag)

      IMPLICIT NONE

      ! Function to find zero of
      real(dp), external :: func

      ! Lower and upper points where a root is known to lie
      real(dp), intent(in) :: x1, x2

      ! Absolute tolerance for convergence
      real(dp), intent(in) :: xacc

      ! Max number of bi-sections
      integer, intent(in) :: jmax

      ! Flag for failure to bracket
      integer, intent(out) :: flag

      ! For program
      integer :: j
      real(dp) :: dx, f, fmid, xmid

      fmid = func(x2)
      f = func(x1)
      if (f*fmid .ge. 0d0) then
         print *, 'Root must be bracketed between x1 and x2 in rtbis'
         rtbis = f
         flag = 1
         return
      else
         flag = 0
      end if
      if (f .lt. 0d0) then ! Orient the search so that f>0 lies at x+dx.
         rtbis = x1
         dx = x2 - x1
      else
         rtbis = x2
         dx = x1 - x2
      end if
      do j = 1, jmax ! Bisection loop.
         dx = dx*0.5d0
         xmid = rtbis + dx
         fmid = func(xmid)
         if (fmid .le. 0d0) rtbis = xmid
         if (abs(dx) .lt. xacc .or. fmid .eq. 0d0) return
      end do
      print *, 'Exceeded maximum number of bi-sections in rtbis'

   end function rtbis

   ! Same function as above, except convergence tolerance is based on the function value instead of the x value
   real(dp) function rtbis_f(func, x1, x2, ftol, jmax, flag)

      IMPLICIT NONE

      ! Function to find zero of
      real(dp), external :: func

      ! Lower and upper points where a root is known to lie
      real(dp), intent(in) :: x1, x2

      ! Absolute tolerance for convergence OF FUNCTION VALUE
      real(dp), intent(in) :: ftol

      ! Max number of bi-sections
      integer, intent(in) :: jmax

      ! Flag for failure to bracket
      integer, intent(out) :: flag

      ! For program
      integer :: j
      real(dp) :: dx, f, fmid, xmid

      fmid = func(x2)
      f = func(x1)
      if (f*fmid .ge. 0d0) then
         print *, 'Root must be bracketed between x1 and x2 in rtbis_f'
         rtbis_f = f
         flag = 1
         return
      else
         flag = 0
      end if
      if (f .lt. 0d0) then ! Orient the search so that f>0 lies at x+dx.
         rtbis_f = x1
         dx = x2 - x1
      else
         rtbis_f = x2
         dx = x1 - x2
      end if
      do j = 1, jmax ! Bisection loop.
         dx = dx*0.5d0
         xmid = rtbis_f + dx
         fmid = func(xmid)
         if (fmid .le. 0d0) rtbis_f = xmid
         if (abs(fmid) .lt. ftol .or. fmid .eq. 0d0) return
      end do
      print *, 'Exceeded maximum number of bi-sections in rtbis_f'

   end function rtbis_f

!################################################################################################################################
! Discretizes an AR(1) process of the form z_j = \rho*z_{j-1} + eps using
!     the Rouwenhorst method.
!
! Details: https://www.ce-fortran.com/discretize_ar/
! Github download: https://github.com/fabiankindermann/ce-fortran/
! Adapted by Tim on 10/29/21
!################################################################################################################################

   subroutine discretize_AR(in_log, rho, mu_in, sigma_eps, z, pi, w)

      IMPLICIT NONE

      ! set to one if the process follows an AR1 in logs, zero else
      integer, intent(in) :: in_log
      ! autoregression parameter
      real(dp), intent(in) :: rho
      ! unconditional mean of the process in levels
      real(dp), intent(in) :: mu_in
      ! variance of the shock
      real(dp), intent(in) :: sigma_eps
      ! discrete shock values
      real(dp), intent(out) :: z(:)
      ! transition matrix - P(x_t+1 = x_j | x_t = X_i) = pi(i, j)
      real(dp), intent(out) :: pi(:, :)
      ! the stationary distribution
      real(dp), intent(out), optional :: w(:)

      ! other variables
      integer :: n, in
      real(dp) :: psi, sigma_eta, mu1, sigma1

      ! assert size equality and get approximation points
      n = assert_eq3(size(z), size(pi, 1), size(pi, 2), 'discretize_AR')

      ! calculate variance of the overall process
      sigma_eta = sigma_eps/(1d0 - rho**2)

      ! transform if you want a log discretization
      if (in_log .eq. 0) then
         sigma1 = sigma_eta
         mu1 = mu_in
      else
         sigma1 = log(1d0 + sigma_eta/mu_in**2)
         mu1 = log(mu_in) - 0.5d0*sigma1
      end if

      ! determine the transition matrix using subroutine below
      call rouwenhorst_matrix(rho, pi)

      ! determine the nodes
      psi = sqrt(real(n - 1, kind=dp))*sqrt(sigma1)
      do in = 1, n
         z(in) = -psi + 2d0*psi*real(in - 1, kind=dp)/real(n - 1, kind=dp)
      end do
      z = z + mu1

      ! Adjust node if you're log-discretiziing
      if (in_log .eq. 1) z = exp(z)

      if (present(w)) then
         w = 1d0/real(n, kind=dp)
         do in = 1, 10000
            w = matmul(transpose(pi), w)
         end do
      end if

   contains

      recursive subroutine rouwenhorst_matrix(ar1coef, pi_new)

         IMPLICIT NONE
         real(dp), intent(in) :: ar1coef
         real(dp), intent(out) :: pi_new(:, :)
         integer :: nr
         real(dp) :: p, pi_old(size(pi_new, 1) - 1, size(pi_new, 1) - 1)

         nr = size(pi_new, 1)
         p = (1d0 + ar1coef)/2d0

         if (nr == 2) then
            pi_new(1, :) = (/p, 1d0 - p/)
            pi_new(2, :) = (/1d0 - p, p/)
         else
            call rouwenhorst_matrix(ar1coef, pi_old)
            pi_new = 0d0

            pi_new(1:nr - 1, 1:nr - 1) = pi_new(1:nr - 1, 1:nr - 1) + p*pi_old
            pi_new(1:nr - 1, 2:nr) = pi_new(1:nr - 1, 2:nr) + (1d0 - p)*pi_old
            pi_new(2:nr, 1:nr - 1) = pi_new(2:nr, 1:nr - 1) + (1d0 - p)*pi_old
            pi_new(2:nr, 2:nr) = pi_new(2:nr, 2:nr) + p*pi_old

            pi_new(2:nr - 1, :) = pi_new(2:nr - 1, :)/2d0
         end if
      end subroutine

   end subroutine discretize_AR

!################################################################################################################################
! Function to compute the inverse of a (square) matrix

! Author: Louisda16th a.k.a Ashwith J. Rego
! Reference: Algorithm explained at http://math.uww.edu/~mcfarlat/inverse.htm

! Modified by Tim on 1/11/22
!################################################################################################################################

   SUBROUTINE InvertMatrix(matrix, inverse, n, errorflag, displayflag_in)

      IMPLICIT NONE

      !Declarations
      INTEGER, INTENT(IN)  :: n
      INTEGER, INTENT(IN), optional  :: displayflag_in
      INTEGER, INTENT(OUT) :: errorflag  !Return error status. -1 for error, 0 for normal
      REAL(dp), INTENT(IN)        :: matrix(n, n)  !Input matrix
      REAL(dp), INTENT(OUT)        :: inverse(n, n) !Inverted matrix

      LOGICAL :: FLAG = .TRUE.
      INTEGER :: i, j, k, displayflag
      REAL(dp) :: m
      REAL(dp), DIMENSION(n, 2*n) :: augmatrix !augmented matrix

      ! Set display flag to zer if not provided
      if (present(displayflag_in)) then
         displayflag = displayflag_in
      else
         displayflag = 0
      end if

      !Augment input matrix with an identity matrix
      DO i = 1, n
         DO j = 1, 2*n
            IF (j <= n) THEN
               augmatrix(i, j) = matrix(i, j)
            ELSE IF ((i + n) == j) THEN
               augmatrix(i, j) = 1
            Else
               augmatrix(i, j) = 0
            END IF
         END DO
      END DO

      !Reduce augmented matrix to upper traingular form
      DO k = 1, n - 1
         IF (augmatrix(k, k) == 0d0) THEN
            FLAG = .FALSE.
            DO i = k + 1, n
               IF (augmatrix(i, k) /= 0d0) THEN
                  DO j = 1, 2*n
                     augmatrix(k, j) = augmatrix(k, j) + augmatrix(i, j)
                  END DO
                  FLAG = .TRUE.
                  EXIT
               END IF
               IF (FLAG .EQV. .FALSE.) THEN
                  IF (displayflag .ne. 0) PRINT *, "WARNING: Matrix not invertible and zero returned."
                  inverse = 0
                  errorflag = -1
                  return
               END IF
            END DO
         END IF
         DO j = k + 1, n
            m = augmatrix(j, k)/augmatrix(k, k)
            DO i = k, 2*n
               augmatrix(j, i) = augmatrix(j, i) - m*augmatrix(k, i)
            END DO
         END DO
      END DO

      !Test for invertibility
      DO i = 1, n
         IF (augmatrix(i, i) == 0d0) THEN
            IF (displayflag .ne. 0) PRINT *, "WARNING: Matrix not invertible and zero returned."
            inverse = 0
            errorflag = -1
            return
         END IF
      END DO

      !Make diagonal elements as 1
      DO i = 1, n
         m = augmatrix(i, i)
         DO j = i, (2*n)
            augmatrix(i, j) = (augmatrix(i, j)/m)
         END DO
      END DO

      !Reduced right side half of augmented matrix to identity matrix
      DO k = n - 1, 1, -1
         DO i = 1, k
            m = augmatrix(i, k + 1)
            DO j = k, (2*n)
               augmatrix(i, j) = augmatrix(i, j) - augmatrix(k + 1, j)*m
            END DO
         END DO
      END DO

      !store answer
      DO i = 1, n
         DO j = 1, n
            inverse(i, j) = augmatrix(i, j + n)
         END DO
      END DO
      errorflag = 0

   END SUBROUTINE InvertMatrix

!################################################################################################################################
! Function to run OLS regression
!################################################################################################################################

   SUBROUTINE OLS(n, k, x, y, betahat, displayflag_in)

      !x is n x k
      !y is n x 1
      !betahat is k x 1
      IMPLICIT NONE
      INTEGER, INTENT(in)                :: n, k
      REAL(dp), INTENT(in)                :: x(n, k), y(n)
      REAL(dp), INTENT(out)        :: betahat(k)
      integer, intent(in), optional :: displayflag_in
      REAL(dp)                                        :: lxx(k, k), lxxinv(k, k)
      INTEGER                                        :: ierr, displayflag

      ! Set display flag to zer if not provided
      if (present(displayflag_in)) then
         displayflag = displayflag_in
      else
         displayflag = 0
      end if

      ! (X'X)^-1 X'Y
      lxx = matmul(transpose(x), x)
      CALL InvertMatrix(lxx, lxxinv, k, ierr)
      betahat = matmul(lxxinv, matmul(transpose(x), y))

   END SUBROUTINE OLS

!################################################################################################################################
! Normal density function
!################################################################################################################################
   real(dp) function normal_pdf(x, mu, sigma)
      IMPLICIT NONE
      real(dp), intent(in) :: x, mu, sigma
      real(dp), parameter :: PI = 3.141592653589793238462643383279502884197d0
      real(dp) :: power
      power = -0.5d0*(((x - mu)/sigma)**2)
      normal_pdf = 1d0/(sigma*SQRT(2d0*PI))*EXP(power)
   end function normal_pdf

!################################################################################################################################
! Functions to calculate statistics on arrays, which are designed to replicate Python functions
!################################################################################################################################

   ! Function for rounding x to nearest multiple of mult
   real(dp) function custom_round(x, mult)
      IMPLICIT NONE
      real(dp), intent(in) :: x, mult
      custom_round = mult*real(floor(x/mult), kind=dp)
   end function custom_round

   real(dp) function mean_array(n, array, cond_in)

      IMPLICIT NONE
      integer, intent(in) :: n
      real(dp), intent(in), dimension(n) :: array
      integer, intent(in), optional, dimension(n) :: cond_in ! integer array that allows you to condition
      integer :: i, nc, cond(n)

      cond = 1
      if (present(cond_in)) cond = cond_in
      nc = sum(cond)
      mean_array = 0d0
      if (nc <= 0) return
      do i = 1, n
         if (cond(i) .eq. 1) mean_array = mean_array + array(i)
      end do
      mean_array = mean_array/real(nc, kind=dp)

   end function mean_array

   real(dp) function variance_array(n, array, cond_in) ! bias adjusted

      IMPLICIT NONE
      integer, intent(in) :: n
      real(dp), intent(in), dimension(n) :: array
      integer, intent(in), optional, dimension(n) :: cond_in
      integer :: cond(n), i
      real(dp) :: mean, nr, array2(n)

      cond = 1
      if (present(cond_in)) cond = cond_in
      nr = real(sum(cond), kind=dp)
      if (nr <= 0d0) then
         variance_array = 0d0
         return
      end if
      mean = mean_array(n, array, cond)
      array2 = 0d0
      do i = 1, n
         if (cond(i) .eq. 1) array2(i) = (array(i) - mean)**2d0
      end do
      variance_array = mean_array(n, array2, cond)
      variance_array = variance_array*nr/(nr - 1d0)

   end function variance_array

   real(dp) function kurtosis_array(n, array, cond_in) ! bias-adjustment formula:
      ! https://stats.stackexchange.com/questions/388198/error-of-bias-corrected-kurtosis-estimators#:~:text=Kurt%5BX%5D%3DE%5B,(notation%20taken%20from%20Wikipedia).

      IMPLICIT NONE
      integer, intent(in) :: n
      real(dp), intent(in), dimension(n) :: array
      integer, intent(in), optional, dimension(n) :: cond_in
      integer :: cond(n), i
      real(dp) :: mean, nr, m2, m4, array4(n)

      cond = 1
      if (present(cond_in)) cond = cond_in
      nr = real(sum(cond), kind=dp)
      if (nr <= 0d0) then
         kurtosis_array = 0d0
         return
      end if
      mean = mean_array(n, array, cond)
      m2 = variance_array(n, array, cond)/nr*(nr - 1d0) ! undo dof adjustment
      array4 = 0d0
      do i = 1, n
         if (cond(i) .eq. 1) array4(i) = (array(i) - mean)**4d0
      end do
      m4 = mean_array(n, array4, cond)
      kurtosis_array = (nr + 1d0)*m4/(m2**2d0) - 3d0*(nr - 1d0)
      kurtosis_array = (nr - 1d0)/(nr - 2d0)/(nr - 3d0)*kurtosis_array

   end function kurtosis_array

   real(dp) function quantile_array(n, array, q, cond_in) ! designed to match Python output exactly

      IMPLICIT NONE
      integer, intent(in) :: n
      real(dp), intent(in), dimension(n) :: array
      real(dp), intent(in) :: q ! quantile between 0 and 1
      integer, intent(in), optional, dimension(n) :: cond_in
      integer :: nc, cond(n), i, ic, qi_floor
      real(dp), allocatable, dimension(:) :: sorted
      real(dp) :: qi, fracL

      cond = 1
      if (present(cond_in)) cond = cond_in
      nc = sum(cond)
      if (nc <= 0) then
         quantile_array = 0d0
         return
      end if
      allocate (sorted(nc))
      if (nc < n) then
         ic = 1
         do i = 1, n
            if (cond(i) .eq. 1) then
               sorted(ic) = array(i)
               ic = ic + 1
            end if
         end do
      else
         sorted = array
      end if
      CALL quicksort(sorted)
      qi = q*real(nc - 1, kind=dp)
      qi_floor = floor(qi)
      fracL = 1d0 - (qi - real(qi_floor, kind=dp))
      quantile_array = fracL*sorted(qi_floor + 1) + (1d0 - fracL)*sorted(qi_floor + 2)
      deallocate (sorted)
   end function quantile_array

   real(dp) function mean_matrix(nx, ny, array, cond_in)

      IMPLICIT NONE
      integer, intent(in) :: nx, ny
      real(dp), intent(in), dimension(nx, ny) :: array
      integer, intent(in), optional, dimension(nx, ny) :: cond_in ! integer array that allows you to condition
      integer :: ix, iy, nc, cond(nx, ny)

      cond = 1
      if (present(cond_in)) cond = cond_in
      nc = sum(cond)
      mean_matrix = 0d0
      if (nc <= 0) return
      do ix = 1, nx
         do iy = 1, ny
            if (cond(ix, iy) .eq. 1) mean_matrix = mean_matrix + array(ix, iy)
         end do
      end do
      mean_matrix = mean_matrix/real(nc, kind=dp)

   end function mean_matrix

!################################################################################################################################
! SMM functions
!################################################################################################################################

   ! Function to make identity matrix
   SUBROUTINE IdentityMatrix(size, mat)
      integer, intent(in) :: size
      real(dp), intent(out), dimension(size, size) :: mat
      integer :: i1
      mat = 0d0
      do i1 = 1, size
         mat(i1, i1) = 1d0
      end do
   END SUBROUTINE IdentityMatrix


   ! Function to make weighting matrix such that moments will be arc-sin deviation
   SUBROUTINE ArcSinMatrix(size, model, data, mat)
      integer, intent(in) :: size
      real(dp), intent(in), dimension(size, 1) :: model, data
      real(dp), intent(out), dimension(size, size) :: mat
      integer :: i1
      mat = 0d0
      do i1 = 1, size
         mat(i1, i1) = (MAX(0.01d0, abs(model(i1, 1)) + abs(data(i1, 1)))/2d0)**(-2d0)
      end do
   END SUBROUTINE ArcSinMatrix

   ! Function to make quadratic forms and return
   real(dp) function QuadraticForm(data, model, weightmat)
      real(dp), intent(in) :: data(:, :), model(:, :), weightmat(:, :)
      real(dp) :: res(1, 1)
      res = matmul(transpose(data - model), matmul(weightmat, (data - model)))
      QuadraticForm = res(1, 1)
   end function QuadraticForm

   ! Routine for SMM Standard Errors
   subroutine StandardErrors(n, k, S, G, W, Omega, ses)
      integer, intent(in) :: n, k
      real(dp), intent(in) :: S, G(n, k), W(n, n), Omega(n, n)
      real(dp), intent(out) :: ses(k)
      integer :: flag, el
      real(dp) :: Chunk(k, k), ChunkInv(k, k), XXX(k, n), V(k, k)

      Chunk = matmul(transpose(G), matmul(W, G))
      CALL InvertMatrix(Chunk, ChunkInv, k, flag)
      if (flag .eq. 0) then
         XXX = matmul(ChunkInv, transpose(G))
         V = matmul(XXX, matmul(W, matmul(Omega, matmul(transpose(W), transpose(XXX)))))
         V = (1d0 + 1d0/S)*V ! adjust for simulation error
         do el = 1, k
            ses(el) = sqrt(V(el, el)) ! No div by n because accounted for in W
         end do
      else
         print *, 'Problem with InvertMatrix'
         ses = -1d0
      end if

   end subroutine StandardErrors

   ! Routine for calculating AGS sensitivity matrix: Lambda = -(G'WG)^-1G'W
   subroutine AGS_Sensitivity(n, k, G, W, Lambda)
      integer, intent(in) :: n, k
      real(dp), intent(in) :: G(n, k), W(n, n)
      real(dp), intent(out) :: Lambda(k, n)
      integer :: flag
      real(dp) :: Chunk(k, k), ChunkInv(k, k)

      Chunk = matmul(transpose(G), matmul(W, G))
      CALL InvertMatrix(Chunk, ChunkInv, k, flag)
      if (flag .eq. 0) then
         Lambda = -matmul(ChunkInv, matmul(transpose(G), W))
      else
         print *, 'Problem with InvertMatrix'
         Lambda = -1d0
      end if

   end subroutine AGS_Sensitivity

   ! Calculate Hansen J stat, taking into account you might not use optimal weighting matrix
   ! This computes the optimal weighting matrix for SMM estimation.
   ! Note: This assumes Omega has already been divided by the sample size.
   real(dp) function JTest(n, k, S, ndata, G, W, Omega, gdiff)
      integer, intent(in) :: n, k
      real(dp), intent(in) :: S, ndata, G(n, k), W(n, n), Omega(n, n), gdiff(n, 1)
      real(dp) :: GWG(k, k), iGWG(k, k), GiGWGG(n, n), eyeGG(n, n), IdentityN(n, n), &
                  vpe(n, n), ivpe(n, n), res(1, 1)
      integer :: flag

      GWG = matmul(matmul(transpose(G), W), G)
      CALL InvertMatrix(GWG, iGWG, k, flag)
      GiGWGG = matmul(matmul(G, iGWG), transpose(G))
      CALL IdentityMatrix(n, IdentityN)
      eyeGG = IdentityN - matmul(GiGWGG, W)
      vpe = matmul(matmul(eyeGG, Omega), transpose(eyeGG))
      vpe = (1d0 + 1d0/S)*vpe
      CALL moorepenrose(n, n, vpe, ivpe)
      res = matmul(matmul(transpose(gdiff), ivpe), gdiff)
      JTest = res(1, 1)*ndata

   end function JTest

END MODULE Procedures
