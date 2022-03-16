!     #########
      SUBROUTINE TRIP_GW_BUFFER_TANK(PTSTEP,OPRINT,PAREA,OMASK_GW,PGROUND_STO,PGROUND_STO2,     &
                                     PDRAIN,PTAUG,PGOUT,PGSTO_ALL,PGSTO2_ALL,PGIN_ALL,PGOUT_ALL )  
!     #############################################################################
!
!!****  *TRIP_GW_BUFFER_TANK*  
!!
!!    PURPOSE
!!    -------
!
!     Calculate the storage in the next time step based on the storage
!     of current time step. The deep drainage is constant during the time step.
!
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
!!      Original    01/02/05 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TRIP_PAR, ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                     :: PTSTEP
!                                       KTSTEP = timestep value (=FRC) [s]
!                                              = 10800s
!
LOGICAL, INTENT(IN)                  :: OPRINT   !Printable budget key 
!
REAL,    DIMENSION(:,:), INTENT(IN)  :: PAREA      ! Grid-cell area    [m²]
LOGICAL, DIMENSION(:,:), INTENT(IN)  :: OMASK_GW   !Groundwater mask
!
REAL, DIMENSION(:,:), INTENT(IN)     :: PDRAIN, PTAUG
!                                       PDRAIN  = Surface runoff from ISBA    [kg/s]
!                                       PTAUG   = ground water transfer time  [s]
!
REAL, DIMENSION(:,:), INTENT(IN   )  :: PGROUND_STO
REAL, DIMENSION(:,:), INTENT(INOUT)  :: PGROUND_STO2
!                                       PGROUND_STO  = ground water storage at t    [kg]
!                                       PGROUND_STO2 = ground water storage at t+1  [kg]
!
REAL, DIMENSION(:,:), INTENT(OUT)    :: PGOUT
!                                       PGOUT = Outflow from the ground reservoir  
!
REAL,                 INTENT(OUT)    :: PGSTO_ALL,PGSTO2_ALL,PGIN_ALL,PGOUT_ALL
!                                       Final budget variable
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PGROUND_STO,1),SIZE(PGROUND_STO,2)) :: ZGSTOMAX
REAL, DIMENSION(SIZE(PGROUND_STO,1),SIZE(PGROUND_STO,2)) :: ZGOUT
REAL, DIMENSION(SIZE(PGROUND_STO,1),SIZE(PGROUND_STO,2)) :: ZDRAIN
REAL, DIMENSION(SIZE(PGROUND_STO,1),SIZE(PGROUND_STO,2)) :: ZDRAIN_NEG
!
INTEGER :: ILON, ILAT, JLON, JLAT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_GW_BUFFER_TANK',0,ZHOOK_HANDLE)
!
ILON = SIZE(PAREA,1)
ILAT = SIZE(PAREA,2)
!
PGROUND_STO2(:,:) = 0.0
PGOUT       (:,:) = 0.0
!
ZGSTOMAX    (:,:) = 0.0
ZGOUT       (:,:) = 0.0
!
ZDRAIN    (:,:) = MAX(0.0,PDRAIN(:,:))
ZDRAIN_NEG(:,:) = MIN(0.0,PDRAIN(:,:))
!
!-------------------------------------------------------------------------------
! * Groundwater case
!-------------------------------------------------------------------------------
!
DO JLAT=1,ILAT
   DO JLON=1,ILON
!   
      IF(OMASK_GW(JLON,JLAT))THEN
!
!       ground water storage calculation dG/dt = Drain - G/tau
        PGROUND_STO2(JLON,JLAT) = PGROUND_STO(JLON,JLAT)*EXP(-PTSTEP/PTAUG(JLON,JLAT)) &
                                + (1.0-EXP(-PTSTEP/PTAUG(JLON,JLAT)))*ZDRAIN(JLON,JLAT)&
                                * PTAUG(JLON,JLAT)  
!
!       supress numerical artifacs
        ZGSTOMAX(JLON,JLAT)=PGROUND_STO(JLON,JLAT)+ZDRAIN(JLON,JLAT)*PTSTEP      
        PGROUND_STO2(JLON,JLAT)=MIN(ZGSTOMAX(JLON,JLAT),PGROUND_STO2(JLON,JLAT))
!
!       ground water discharge calculation                
        ZGOUT(JLON,JLAT)=(PGROUND_STO(JLON,JLAT)-PGROUND_STO2(JLON,JLAT))/PTSTEP+ZDRAIN(JLON,JLAT)
!      
!       supress numerical artifacs
        PGOUT(JLON,JLAT)=MAX(0.0,ZGOUT(JLON,JLAT))
        PGROUND_STO2(JLON,JLAT) = PGROUND_STO2(JLON,JLAT) + (PGOUT(JLON,JLAT)-ZGOUT(JLON,JLAT))
!      
!       account for negative drainage
        PGROUND_STO2(JLON,JLAT) = PGROUND_STO2(JLON,JLAT) + ZDRAIN_NEG(JLON,JLAT)*PTSTEP
!        
     ENDIF
!     
   ENDDO
ENDDO        
!
!-------------------------------------------------------------------------------
! * No groundwater case
!-------------------------------------------------------------------------------
!
WHERE(.NOT.OMASK_GW(:,:)) PGOUT(:,:)=ZDRAIN(:,:)
!
!-------------------------------------------------------------------------------
! * Budget calculation
!-------------------------------------------------------------------------------
!
IF(OPRINT)THEN
!
  PGSTO_ALL  = 0.0
  PGSTO2_ALL = 0.0
  PGIN_ALL   = 0.0
  PGOUT_ALL  = 0.0
!
  WHERE(OMASK_GW(:,:)) ZDRAIN(:,:)=ZDRAIN(:,:)+ZDRAIN_NEG(:,:)
!
  DO JLAT=1,ILAT
     DO JLON=1,ILON
        IF(OMASK_GW(JLON,JLAT))THEN
          PGSTO_ALL  = PGSTO_ALL  + PGROUND_STO (JLON,JLAT) / PAREA(JLON,JLAT)
          PGSTO2_ALL = PGSTO2_ALL + PGROUND_STO2(JLON,JLAT) / PAREA(JLON,JLAT)
          PGIN_ALL   = PGIN_ALL   + ZDRAIN      (JLON,JLAT) / PAREA(JLON,JLAT)
          PGOUT_ALL  = PGOUT_ALL  + PGOUT       (JLON,JLAT) / PAREA(JLON,JLAT)
        ENDIF
     ENDDO
  ENDDO
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('TRIP_GW_BUFFER_TANK',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
END SUBROUTINE TRIP_GW_BUFFER_TANK
