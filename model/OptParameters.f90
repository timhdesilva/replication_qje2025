MODULE OptParameters

   IMPLICIT NONE

   integer, parameter :: dp = selected_real_kind(15, 307)

   ! Golden Section Search
   real(dp), parameter :: goldentol = 0.00001d0 ! tolerance for golden section search
   real(dp), parameter :: goldenalpha2 = 0.611003399d0 ! golden rule parameter
   real(dp), parameter :: goldenalpha1 = 1.0d0 - goldenalpha2 ! golden rule parameter

   ! Nelder Mead
   real(dp), parameter :: neldertol = 1d-8 ! tolerance
   real(dp), parameter :: nelderstepfrac = 0.2d0 ! fraction of starting points to determine step size
   integer, parameter :: neldermaxiter = 5000 ! maximum number of iterations

END MODULE OptParameters
