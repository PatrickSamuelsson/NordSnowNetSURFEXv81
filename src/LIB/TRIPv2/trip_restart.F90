!#########
SUBROUTINE TRIP_RESTART (TP, TPG, &
                         KLISTING,KYEAR,KMONTH,KDAY,PTIME,KLON,KLAT)
!############################################
!
!!****  *TRIP_RESTART*  
!!
!!    PURPOSE
!!    -------
!
!     TRIP river routing restart.
!     
!!      
!!    AUTHOR
!!    ------
!!      B. Decharme
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    28/05/05 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE MODD_TRIP, ONLY : TRIP_t
USE MODD_TRIP_GRID, ONLY : TRIP_GRID_t
!
USE MODE_RW_TRIP
!
USE MODN_TRIP,      ONLY : CGROUNDW, LFLOOD
!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(TRIP_t), INTENT(INOUT) :: TP
TYPE(TRIP_GRID_t), INTENT(INOUT) :: TPG
!
INTEGER, INTENT(IN)             :: KLISTING
INTEGER, INTENT(IN)             :: KYEAR
INTEGER, INTENT(IN)             :: KMONTH
INTEGER, INTENT(IN)             :: KDAY
REAL,    INTENT(IN)             :: PTIME
INTEGER, INTENT(IN)             :: KLON
INTEGER, INTENT(IN)             :: KLAT
!
!*      0.2    declarations of local variables
!
LOGICAL,           PARAMETER :: LDOUBLE=.TRUE.
!
 CHARACTER(LEN=15), PARAMETER :: YFILE ='TRIP_RESTART.nc'
 CHARACTER(LEN=20)            :: YVNAME
!
LOGICAL,DIMENSION(KLON,KLAT) :: LMASK
LOGICAL,DIMENSION(KLON,KLAT) :: LMASK_GW
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
! * Store output in diag file
!
IF (LHOOK) CALL DR_HOOK('TRIP_RESTART',0,ZHOOK_HANDLE)
!
! * Current date
!
 CALL WRITE_TRIP_DATE(KLISTING,YFILE,KYEAR,KMONTH,KDAY,PTIME)
!
! * Trip mask
!
LMASK(:,:) = TPG%GMASK(:,:)
!
! * Groundwater specific mask
!
IF(CGROUNDW/='DEF')THEN
  LMASK_GW(:,:) = TPG%GMASK_GW(:,:)        
ENDIF
!
! * Write variables
!
YVNAME = 'SURF_STO'
 CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK,TP%XSURF_STO,ODOUBLE=LDOUBLE)
!
IF(CGROUNDW=='CST')THEN
  YVNAME = 'GROUND_STO'
  CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK_GW,TP%XGROUND_STO,ODOUBLE=LDOUBLE)
  ELSEIF(CGROUNDW=='DIF')THEN
  YVNAME = 'HGROUND'   
  CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK_GW,TP%XHGROUND,ODOUBLE=LDOUBLE)
ENDIF
!
IF(LFLOOD)THEN
  YVNAME = 'FLOOD_STO'
  CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK,TP%XFLOOD_STO,ODOUBLE=LDOUBLE)           
  YVNAME = 'FFLOOD'
  CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK,TP%XFFLOOD,ODOUBLE=LDOUBLE)        
  YVNAME = 'HFLOOD'
  CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK,TP%XHFLOOD,ODOUBLE=LDOUBLE)        
ENDIF
!
IF (LHOOK) CALL DR_HOOK('TRIP_RESTART',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE TRIP_RESTART
