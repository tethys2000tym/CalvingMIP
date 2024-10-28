!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       FUNCTION TO OBTAIN THE CIRCULAR DOMAIN    !
!     PROPOSED FOR THE CALVING MIP EXPERIMENT     !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
FUNCTION Circular(Model, nodenumber, x) RESULT(bed)
  USE types
  USE DefUtils
  IMPLICIT NONE
  TYPE(Model_t) :: Model
  INTEGER :: nodenumber
  REAL(KIND=dp) :: x(2)
  REAL(KIND=dp) :: bed,d,theta
  !! parameters for the circular topography in m
  REAL(KIND=dp), PARAMETER :: R=800D3    
  REAL(KIND=dp), PARAMETER :: Bc=0.9D3
  REAL(KIND=dp), PARAMETER :: Bl=-2D3 
  REAL(KIND=dp), PARAMETER :: rc=0.0D0
  
  d = sqrt(x(1)**2+x(2)**2)
  theta=atan2(x(2),x(1))
  bed = Bc-(Bc-Bl)*(d-rc)**2/(R-rc)**2

END FUNCTION Circular

