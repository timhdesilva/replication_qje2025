MODULE types

   USE Parameters, only: dp

   IMPLICIT NONE

   ! Current state variables
   TYPE state_t
      integer :: ed
      integer :: ij
      integer :: ipcy
      real(dp) :: liqw
      real(dp) :: debt
      real(dp) :: theta
      real(dp) :: epsln
      real(dp) :: Plabor
      real(dp) :: calvo
   END TYPE state_t

   ! Gridpoints of continuous state variables
   TYPE cstate_pt
      integer :: lq
      integer :: dbt
      integer :: iz
      integer :: pL
   END TYPE cstate_pt

   ! Policies
   TYPE policies_t
      real(dp) :: saving
      real(dp) :: labor
   END TYPE policies_t

   ! Arguments to pass to objectives when optimizing
   TYPE args_opt
      type(state_t) :: s
      real(dp) :: laborGS
      real(dp) :: lows(2)
      real(dp) :: highs(2)
   END TYPE args_opt

END MODULE types
