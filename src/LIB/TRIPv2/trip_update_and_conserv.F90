!     #########
      SUBROUTINE TRIP_UPDATE_AND_CONSERV(OPRINT,OFLOOD,HGROUNDW,PAREA,PWEFF,   &
                                         PSURF_STO2,PFLOOD_STO2,PGROUND_STO2,  &
                                         PSURF_STO,PFLOOD_STO,PGROUND_STO,     &
                                         OMASK_GW,PHGROUND,PRECUP_ALL          )  
!     ################################################################
!
!!****  *TRIP_UPDATE_AND_CONSERV*  
!!
!!    PURPOSE
!!    -------
!
! Update all reservoir and conserve water mass as possible
!     
!!**  METHOD
!!    ------
!
!     Direct calculation
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
!!      Original    01/12/13 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODN_TRIP,     ONLY : XCVEL
USE MODD_TRIP_PAR, ONLY : XRHOLW
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
LOGICAL,                 INTENT(IN)    :: OPRINT   !Printable budget key 
LOGICAL,                 INTENT(IN)    :: OFLOOD       !Flood scheme key
 CHARACTER(LEN=3),        INTENT(IN)    :: HGROUNDW     !Groundwater scheme key
LOGICAL, DIMENSION(:,:), INTENT(IN)    :: OMASK_GW     !Groundwater mask
REAL,    DIMENSION(:,:), INTENT(IN)    :: PAREA        ! Grid-cell area                  [m2]
REAL,    DIMENSION(:,:), INTENT(IN)    :: PWEFF        ! Effective porosity              [-]
REAL,    DIMENSION(:,:), INTENT(IN)    :: PSURF_STO2   ! river channel storage           [kg]
REAL,    DIMENSION(:,:), INTENT(IN)    :: PFLOOD_STO2  ! Floodplain water storage        [kg]
REAL,    DIMENSION(:,:), INTENT(IN)    :: PGROUND_STO2 ! groundwater storage             [kg]
!
REAL,    DIMENSION(:,:), INTENT(OUT)   :: PSURF_STO    ! river channel storage at t+1    [kg]
REAL,    DIMENSION(:,:), INTENT(OUT)   :: PFLOOD_STO   ! Floodplain water storage at t+1 [kg]
REAL,    DIMENSION(:,:), INTENT(OUT)   :: PGROUND_STO  ! groundwater storage at t+1      [kg]
REAL,    DIMENSION(:,:), INTENT(INOUT) :: PHGROUND     ! water table elevation           [m]
REAL,                    INTENT(OUT)   :: PRECUP_ALL   ! Global none conserved water mass[kg/m2]
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PAREA,1),SIZE(PAREA,2)) :: ZRECUP_FLD  ! ensure water conservation        [kg]    
REAL, DIMENSION(SIZE(PAREA,1),SIZE(PAREA,2)) :: ZRECUP_SURF ! ensure water conservation        [kg]    
REAL, DIMENSION(SIZE(PAREA,1),SIZE(PAREA,2)) :: ZRECUP_FINAL! ensure water conservation        [kg]    
!
INTEGER :: ILON, ILAT, JLON, JLAT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_UPDATE_AND_CONSERV',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
! * Init
!-------------------------------------------------------------------------------
!
ILON = SIZE(PAREA,1)
ILAT = SIZE(PAREA,2)
!
ZRECUP_FLD   (:,:) = 0.0
ZRECUP_SURF  (:,:) = 0.0
ZRECUP_FINAL (:,:) = 0.0
!
!-------------------------------------------------------------------------------
! * Update and conserve water mass
!-------------------------------------------------------------------------------
!
IF(OFLOOD)THEN
   ZRECUP_FLD(:,:) = MIN(0.0,PFLOOD_STO2(:,:)) 
   PFLOOD_STO(:,:) = MAX(0.0,PFLOOD_STO2(:,:))
ENDIF
!
ZRECUP_SURF(:,:) = MIN(0.0,PSURF_STO2(:,:)+ZRECUP_FLD(:,:))
PSURF_STO  (:,:) = MAX(0.0,PSURF_STO2(:,:)+ZRECUP_FLD(:,:))
!
IF(HGROUNDW=='CST')THEN
!        
  WHERE(OMASK_GW(:,:)) 
       ZRECUP_FINAL(:,:) = MIN(0.0,PGROUND_STO2(:,:)+ZRECUP_SURF(:,:))
       PGROUND_STO (:,:) = MAX(0.0,PGROUND_STO2(:,:)+ZRECUP_SURF(:,:))
  ELSEWHERE
       ZRECUP_FINAL(:,:) = ZRECUP_SURF(:,:)
       PGROUND_STO (:,:) = 0.0
  ENDWHERE
!  
ELSEIF(HGROUNDW=='DIF')THEN
!
  WHERE(OMASK_GW(:,:).AND.ZRECUP_SURF(:,:)<0.0)
       ZRECUP_FINAL(:,:) = 0.0
       PHGROUND    (:,:) = PHGROUND(:,:)+ZRECUP_SURF(:,:)/(PWEFF(:,:)*PAREA(:,:)*XRHOLW)
  ELSEWHERE
       ZRECUP_FINAL(:,:) = ZRECUP_SURF(:,:)
  ENDWHERE
!  
ELSE
!        
  ZRECUP_FINAL (:,:) = ZRECUP_SURF(:,:)
!  
ENDIF
!
!-------------------------------------------------------------------------------
! * Global unconserved water mass calculation
!-------------------------------------------------------------------------------
!
IF(OPRINT)THEN
!
  PRECUP_ALL = 0.0
!
  DO JLAT=1,ILAT
     DO JLON=1,ILON
        PRECUP_ALL = PRECUP_ALL + ZRECUP_FINAL(JLON,JLAT)/PAREA(JLON,JLAT)
     ENDDO
  ENDDO   
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('TRIP_UPDATE_AND_CONSERV',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
END SUBROUTINE TRIP_UPDATE_AND_CONSERV
