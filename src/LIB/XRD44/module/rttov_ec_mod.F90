MODULE RTTOV_EC_MOD       

!     Purpose.
!     --------

!     Types used in the interface between IFS and SATRAD projects, e.g.
!     where RADTR_ML calls RTTOV_EC. Defaults are for ECMWF normal usage.

!     Author.
!     -------
!        A.Geer     ECMWF

!     Modifications.
!     --------------
!        Original: 2013-06-27
!     05/07/2014 Inclusion of cldstr_threshold (S. Migliorini)
!     ------------------------------------------------------------------

USE PARKIND1 , ONLY : JPIM, JPRB

IMPLICIT NONE

SAVE

TYPE RTTOV_EC_OPTS

  ! switch for cloud computations
  LOGICAL :: LCLOUD = .FALSE.    

  ! when lcldstr_simple is false, only streams whose weight is larger than cldstr_threshold are considered
  REAL(KIND=JPRB) :: CLDSTR_THRESHOLD = -1.0_JPRB

  ! switch for internal rttov interpolation 
  LOGICAL :: LINTERP = .TRUE.    

  ! switch for tracers
  LOGICAL, DIMENSION(4) :: LTRACER = (/.FALSE.,.FALSE.,.FALSE.,.FALSE./)

  ! For complex cloudy IR, true uses 2-column, false uses full cloudy streams method
  LOGICAL :: LCLDSTR_SIMPLE = .TRUE. 

END TYPE RTTOV_EC_OPTS

! Key to the contents of the pav array 
TYPE TYPE_PAV_KEY
  INTEGER(KIND=JPIM) :: T       ! Temperature
  INTEGER(KIND=JPIM) :: Q       ! Humidity
  INTEGER(KIND=JPIM) :: O3      ! Ozone
  INTEGER(KIND=JPIM) :: L       ! Cloud liquid water
  INTEGER(KIND=JPIM) :: I       ! Cloud ice water
  INTEGER(KIND=JPIM) :: A       ! Cloud fraction
  INTEGER(KIND=JPIM) :: CO2    
  INTEGER(KIND=JPIM) :: N2O
  INTEGER(KIND=JPIM) :: CH4
  INTEGER(KIND=JPIM) :: CO
END TYPE

TYPE(TYPE_PAV_KEY), PARAMETER :: IPAV=TYPE_PAV_KEY(1,2,3,4,5,6,7,8,9,10)

END MODULE RTTOV_EC_MOD
