!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       FUNCTION TO OBTAIN THE THULE DOMAIN       !
!     PROPOSED FOR THE CALVING MIP EXPERIMENT     !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
FUNCTION Thule( Model, nodenumber, x) RESULT(bed)
  USE types
  USE DefUtils
  IMPLICIT NONE
  TYPE(Model_t) :: Model
  INTEGER :: nodenumber
  REAL(KIND=dp) :: x(2)
  REAL(KIND=dp) :: d,theta,l,a,bed
  !! parameters for the topography in m
  REAL(KIND=dp), PARAMETER :: R=800D3 
  REAL(KIND=dp), PARAMETER :: Bc=0.9D3
  REAL(KIND=dp), PARAMETER :: Bl=-2D3
  REAL(KIND=dp), PARAMETER :: Ba=1.1D3
  REAL(KIND=dp), PARAMETER :: rc=0.0D0
  d=sqrt(x(1)**2+x(2)**2)
  theta=atan2(x(2),x(1))
  l=R-cos(2*theta)*R/2
  a=Bc-(Bc-Bl)*(d-rc)**2/(R-rc)**2
  Bed=Ba*cos(3*pi*d/l)+a
END FUNCTION Thule


