MODULE Parameters

   IMPLICIT NONE

! Machine specific real kinds
   integer, parameter :: sp = selected_real_kind(6, 37)
   integer, parameter :: dp = selected_real_kind(15, 307)
   real(sp) :: dummy_sp
   real(dp) :: dummy_dp

! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Options
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

! Code Operation
   !! Outputs from Simulations
   integer, parameter :: SaveLCPanel = 0 ! save simulated panel
   !! Miscellaneous
   integer :: RunMode ! keep track of Main_.f90 file you're running

! Model Specification
   !! Preferences
   integer :: FixLabor = 0 ! turn off labor supply decision

! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Demographic parameters
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Age parameters
   integer, parameter :: FirstAge = 22, &       ! Youngest age
                         JWork = 43, & ! Nbr of work periods
                         JRet = 26, &   ! Nbr of retirement periods (includes 1 extra year for certain death)
                         JTot = JWork + JRet     ! Total nbr of periods
! Cohorts
   integer, parameter :: FirstCohort = 1963, LastCohort = 2019, n_cohorts = LastCohort - FirstCohort + 1
! Numeraire = average annualized AWE in 2004 Australia in AUD
   real(dp), parameter :: numeraire = 40000d0 ! ~751.90*52 = AWE*52
! Price levels relative to 2005 based on HELP threshold inflation rate
   real(dp), parameter :: price04 = 48571d0/50929d0
   real(dp), parameter :: price06 = 36184d0/35000d0
   real(dp), parameter :: price07 = 38148d0/35000d0
   real(dp), parameter :: price08 = 39824d0/35000d0
! Age, survival probability, and equivalence scale grids
   real(dp), dimension(JTot) :: gd_age, survival, cum_survival, eqscale
! Bounds on years to consider in SMM moments
   integer, parameter :: FirstYr = 1991, LastYr = 2019

! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Grids parameters for (non-age) state variables
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ! Number of grid points
   integer, parameter :: NbrWLiqW = 31
   integer, parameter :: NbrDebt = 11
   integer, parameter :: NbrTheta = 21
   integer, parameter :: NbrPastL = 25
   integer, parameter :: NbrWLiqW_B = 0 ! number of liquid wealth points to place below zero (= 0 => grid spaced from lower to upper)
   integer, parameter :: NbrRLiqW = 101 ! liquid wealth in retirement
   ! Grid curvature parameters
   real(dp), parameter :: gdcurve_LiqWealth = 0.2d0
   real(dp), parameter :: gdcurve_Debt = 0.35d0
   ! Grids for income process components
   real(dp), allocatable, dimension(:) :: gd_Theta, gd_ExpTheta
   real(dp), allocatable, dimension(:, :, :, :) :: gd_Wage
   ! Grids for other states
   real(dp) :: gdW_LiqW(NbrWLiqW, JWork), gdR_LiqW(NbrRLiqW, JRet)
   real(dp) :: gd_Debt(NbrDebt, JWork), gd_PastL(NbrPastL)
   ! Parameters for custom grid for theta
   real(dp), parameter :: gdcurve_Theta = 0.7d0 ! curvature for theta grid (if too low, may get V=NaN at low wages from extrapolation)
   real(dp), parameter :: gdsigma_Theta = 4d0 ! maximum standard deviation of theta grid

! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! DGPs of assets and wage rate
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Liquid risk-free asset
   real(dp), parameter :: R = 1.0184d0 !1d0/0.972755660726326d0
! Wedge on borrowing rate
!! Note: if you turn this too high, things will break when solving in retirement because there will be people who can't pay
!! everything off by the time they die given there is no consumption floor in retirement. Values around 1d0-1.25d0 should work.
   real(dp), parameter :: wedgeB0 = 0.146d0
! Components of wage rate process
   real(dp) :: deltas(3), deltaEs(2), rhotheta, sigmanu, sigmai, sigmaeps, AProfile(JWork, 2)
! Smoothed tax rate parameters
   real(dp) :: smoothtax(2)

! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Debt parameters
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   real(dp) :: R_d ! interest rate on debt
   real(dp), parameter :: p_e = 0.30856899d0 ! fraction in data with debt from .txt Inputs/
   real(dp) :: mu_d, sigma_d, maxdebt0 ! parameters for initial debt distribution
   integer, allocatable :: Policy(:) ! list of policies
   integer :: NbrPolicy ! number of policies
   integer :: DebtPayoff ! set to 0 if you never payoff debt
   integer :: ForceFixedPayoff = 0 ! set to 1 if you want to force repayment with fixed debt contracts
   real(dp), parameter, dimension(7) :: HELPTsh04 = (/25347d0, 26731d0, 28805d0, 33414d0, 40328d0, 42447d0, 45628d0/)
   real(dp), parameter, dimension(9) :: HELPTsh05 = (/35000d0, 38987d0, 42972d0, 45232d0, 48621d0, 52657d0, 55429d0, &
                                                      60971d0, 64999d0/)

! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Repayment policy comparison parameters
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ! Bin parameters when calculating average value function across different states
   integer, parameter :: nbins_s0 = 3 ! number of bins for Assets0, delta_i, and Debt0
   real(dp) :: bins_s0(3, nbins_s0 + 1) ! bin edges for Assets0, delta_i, and Debt0
   ! Storages of average value function across different states
   real(dp) :: avgV0(3) ! average V at t=0 in simulation for all, E=1, and E=2
   real(dp), allocatable :: avgV0_a(:, :) ! average V by age and E
   real(dp), allocatable :: avgV0_aA(:, :, :) ! average V by age, Assets0, and E
   real(dp), allocatable :: avgV0_ai(:, :, :) ! average V by age, delta_i, and E
   real(dp), allocatable :: avgV0_aD(:, :, :) ! average V by age, Debt0, and E

! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Optimal Policy Parameters
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ! Optimal repayment contract structure
   integer :: OptimalType
   ! Optimal policy parameters
   real(dp), dimension(:), allocatable :: OptimalP
   ! Whether to save simulation outputs to to txt files
   logical :: SaveSimY = .FALSE., SaveSimV = .FALSE.

! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Tolerances and optimization bounds
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   real(dp), parameter :: floorC = 1d-3 ! consumption floor
   real(dp), parameter :: minVCES = 1d-5 ! minimum interpolated V with CES to prevent interpolation from going negative
   real(dp), parameter :: maxL = 10d0, minL = 0d0 ! bounds for L when optimizing
   real(dp), parameter, dimension(3) :: startL = (/1d0, 0.1d0, 0.9d0*maxL/) ! starting points for L when optimizing
   integer, parameter :: size_start = size(startL)
   real(dp), parameter :: minTransProb = 1d-12 ! min probability for any income state to matter
   real(dp), parameter :: maxabsV = 1d14 ! max abs value function allowed in checks and printed out in policies and simulations datasets
   real(dp), parameter :: negativeInf = -1d30, positiveInf = 1d30 ! dp infinities
   real(sp), parameter :: negativeInf_sp = -1e30 ! sp infinties
   real(dp), parameter :: tolES = 0.1d0 ! tolerance for elasticities of substitution to be bounded away from 1
   real(dp), parameter :: epsl = EPSILON(dummy_dp)

! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Integration parameters
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ! Integration parameters for Gauss-Hermite
   integer, parameter :: NbrNu_GH = 9, NbrEps_GH = 7 ! number of nodes
   real(dp), dimension(NbrNu_GH) :: GHx_Nu, GHw_Nu ! GH weights and nodes for nu
   real(dp), dimension(NbrEps_GH) :: GHx_Eps, GHw_Eps ! GH weights and nodes for epsilon

! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Structural parameters
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   real(dp), parameter :: gamma = 2.23d0 ! risk aversion
   real(dp), parameter :: invEIS = gamma ! inverse of elasticity of intertemporal substitution
   real(dp) :: beta, phi, cprob, fcL, fcH, kappa, lerr, constraintB(JTot), wedgeB
   real(dp) :: c_gain = 0d0

! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Directories
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   character(len=*), parameter :: InputDir = "Input/"
   character(len=*), parameter :: SEDir = "Input/SEs/"
   character(len=*), parameter :: OutputDir = "Output/"

! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Parallelization parameters
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ! OpenMP parameters
   character(10) :: NbrThreads ! Nbr of CPUs per job
   ! MPI parameters
   integer :: procid ! id of current process
   integer :: mpiprocs  ! number of MPI processes

! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Value functions
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   real(dp), allocatable, dimension(:, :, :, :, :, :) :: V_E
   real(dp), allocatable, dimension(:, :, :, :, :) :: V_NE
   real(dp), allocatable, dimension(:, :) :: V_Ret

! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Simulation parameters and results
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   integer, parameter :: maxnsim_welfare = 1400000 ! maximum number of nsim when computing welfare to stay within memory
   integer, parameter :: nsim_op = 50000 ! nsim when doing optimal policy
   integer, parameter :: nsim = 1600000 ! Number of individual level simulations
   ! integer, parameter :: nsim = maxnsim_welfare
   ! integer, parameter :: nsim = nsim_op
   real(dp), parameter :: nsim_dp = real(nsim, kind=dp)
   real(dp), parameter :: pe1_sim = 0.1d0 ! probability of E=1 in simulation
   integer, parameter :: ind_E2 = ceiling(pe1_sim*nsim_dp) ! index of first individual with E=2 in simulation when rescaling
   integer, parameter :: realind_E2 = ceiling((1d0 - p_e)*nsim_dp)! index of first individual with E=2 in simulation when realistic
   integer :: SMMError ! parameter to keep track of if current parameterization caused error when doing SMM
   real(dp), allocatable, dimension(:, :) :: SimY, SimWL, SimV, SimLiqWealth, SimAssets, SimLabor, SimConsumption, &
                                             SimTT, SimPanel, Lifecycle, &
                                             SimW, SimTheta, SimEps, SimRawNu, SimRawEps, SimCalvo, SimLerr
   real(sp), allocatable, dimension(:, :) :: SimVS, SimLaborS, SimDebt, SimRepay
   real(dp), allocatable, dimension(:) :: SimDebt0, SimAssets0
   integer, allocatable, dimension(:, :) :: SimFlag, SimSolType, SimSince
   integer, allocatable, dimension(:) :: SimE, SimPolicyI, SimHELPSwitchIJ, FirstIJDeath, AgeRepay
   integer, parameter :: ncol_SimPanel = 20 ! number of stats to keep track of in individual panel

END MODULE Parameters
