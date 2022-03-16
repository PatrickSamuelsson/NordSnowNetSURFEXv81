!     #########
SUBROUTINE TRIP_HS_VEL (PTSTEP,OMASK_VEL,PLEN,PWIDTH,PSLOPEBED,PN,PSURF_STO,PHS,PVEL)
!     ################################################################
!
!!****  *TRIP_HS_VEL*  
!!
!!    PURPOSE
!!    -------
!
!     Calculate the river height and velocity 
!     Where OMASK_VEL=true the Manning equation is used
!
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!
!     None
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      
!!    REFERENCE
!!    ---------
!!      
!!    AUTHOR
!!    ------
!!      B. Decharme
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/02/09 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODN_TRIP,     ONLY : CVIT, XCVEL
USE MODD_TRIP_PAR, ONLY : XUNDEF, XM, XVELMIN, &
                          XHSMIN, XRHOLW
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABORT_TRIP
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                     :: PTSTEP ! Trip timestep value (10800s)
!
LOGICAL, DIMENSION(:,:), INTENT(IN)    :: OMASK_VEL  ! Variable velocity mask
REAL,    DIMENSION(:,:), INTENT(IN)    :: PLEN       ! river length       [m] 
REAL,    DIMENSION(:,:), INTENT(IN)    :: PWIDTH     ! river widths                 [m]
REAL,    DIMENSION(:,:), INTENT(IN)    :: PSLOPEBED  ! river bed slopes             [m/m]
REAL,    DIMENSION(:,:), INTENT(IN)    :: PN         ! Manning roughness coeficient [-] (0.03 to 0.065)
REAL,    DIMENSION(:,:), INTENT(IN)    :: PSURF_STO  ! river channel storage at t  [kg]
REAL,    DIMENSION(:,:), INTENT(OUT)   :: PHS   ! river channel height [m]
REAL,    DIMENSION(:,:), INTENT(OUT)   :: PVEL  ! River channel velocity  [m/s]
!
!*      0.2    declarations of local variables
!
REAL, PARAMETER :: ZLOG_MIN = 1.E-12
!
REAL, DIMENSION(SIZE(PLEN,1),SIZE(PLEN,2)) :: ZRADIUS
REAL, DIMENSION(SIZE(PLEN,1),SIZE(PLEN,2)) :: ZHS
REAL, DIMENSION(SIZE(PLEN,1),SIZE(PLEN,2)) :: ZVEL
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
! * Init
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_HS_VEL',0,ZHOOK_HANDLE)
!
ZRADIUS    (:,:) = 0.0
ZVEL       (:,:) = 0.0
ZHS        (:,:) = 0.0
!
!-------------------------------------------------------------------------------
! * River channel velocity             
!-------------------------------------------------------------------------------
!
!Constant streamflow velocity
PHS (:,:)=XUNDEF
PVEL(:,:)=XCVEL
!
IF(CVIT == 'VAR')THEN
  WHERE(OMASK_VEL(:,:))       
      !Variable streamflow velocity
      PHS     (:,:)=PSURF_STO(:,:)/(XRHOLW*PLEN(:,:)*PWIDTH(:,:))
      ZHS     (:,:)=MAX(PHS(:,:),ZLOG_MIN)
      ZRADIUS (:,:)=LOG(PWIDTH(:,:)*ZHS(:,:)/(PWIDTH(:,:)+2.0*ZHS(:,:)))
      ZVEL    (:,:)=EXP(XM*ZRADIUS(:,:))*SQRT(PSLOPEBED(:,:))/PN(:,:)
      ZVEL    (:,:)=MAX(XVELMIN,ZVEL(:,:))
      ZVEL    (:,:)=MIN(ZVEL(:,:),PLEN(:,:)/PTSTEP)
      !Velocity limitation if the river is very dry
      PVEL    (:,:)=ZVEL(:,:)*MIN(1.0,MAX(0.0,(PHS(:,:)-XHSMIN))/XHSMIN)
  ENDWHERE
ENDIF
!
IF (LHOOK) CALL DR_HOOK('TRIP_HS_VEL',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE TRIP_HS_VEL
