!     #########
      SUBROUTINE FLOOD_UPDATE (PTAB_F,PTAB_H,PTAB_VF,PAREA,PFLOOD_STO, &
                               PLEN,PHFLOOD,PFFLOOD,PFLOOD_LEN,PWFLOOD )  
!     ##########################################################################
!
!!****  *FLOOD_UPDATE*  
!!
!!    PURPOSE
!!    -------
!
!     Compute HFLOOD, FFLOOD, LFLOOD, WFLOOD.
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
!!      Original    01/11/06 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODN_TRIP,     ONLY : XRATMED
!
USE MODD_TRIP_PAR, ONLY : XUNDEF, XRHOLW
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,DIMENSION(:,:,:), INTENT(IN)  :: PTAB_F  ! Flood fraction array
REAL,DIMENSION(:,:,:), INTENT(IN)  :: PTAB_H  ! Topo height array
REAL,DIMENSION(:,:,:), INTENT(IN)  :: PTAB_VF ! Flood volume array
REAL,DIMENSION(:,:),   INTENT(IN)  :: PAREA   ! grid area                 [m²]
REAL,DIMENSION(:,:),   INTENT(IN)  :: PFLOOD_STO ! Floodplain water mass  [kg]
REAL,DIMENSION(:,:),   INTENT(IN)  :: PLEN    ! River lenght              [m]
!
REAL,DIMENSION(:,:),   INTENT(OUT) :: PHFLOOD ! Floodplain fraction        [-]
REAL,DIMENSION(:,:),   INTENT(OUT) :: PFFLOOD    ! Floodplain water depth  [m]
REAL,DIMENSION(:,:),   INTENT(OUT) :: PFLOOD_LEN ! Floodplain lenght       [m]
REAL,DIMENSION(:,:),   INTENT(OUT) :: PWFLOOD    ! Floodplain width        [m]
!
!*      0.2    declarations of local variables
!
REAL,    DIMENSION(SIZE(PAREA,1),SIZE(PAREA,2)) :: ZFLOOD_STO !kg/m2
!
INTEGER, DIMENSION(SIZE(PAREA,1),SIZE(PAREA,2)) :: IUP, IDOWN
!
INTEGER :: ILON, ILAT, JLON, JLAT, JPAS, IPAS
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
! * Initialize local variable
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('FLOOD_UPDATE',0,ZHOOK_HANDLE)
!
ILON = SIZE(PTAB_F,1)
ILAT = SIZE(PTAB_F,2)
IPAS = SIZE(PTAB_F,3)
!
PHFLOOD   (:,:) = 0.0
PFFLOOD   (:,:) = 0.0
PWFLOOD   (:,:) = 0.0
PFLOOD_LEN(:,:) = 0.0
!
ZFLOOD_STO(:,:) = PFLOOD_STO(:,:)/PAREA(:,:)
!
DO JLAT=1,ILAT
   DO JLON=1,ILON
      IF(ZFLOOD_STO(JLON,JLAT)>0.0)THEN
        DO JPAS=1,IPAS-1
           IF(ZFLOOD_STO(JLON,JLAT)>=PTAB_VF(JLON,JLAT,JPAS))THEN
             IUP  (JLON,JLAT) = JPAS+1
             IDOWN(JLON,JLAT) = JPAS
           ENDIF
        ENDDO
      ENDIF
   ENDDO
ENDDO
!
!-------------------------------------------------------------------------------
! * Calculate new Fflood and Hflood
!-------------------------------------------------------------------------------
!
DO JLAT=1,ILAT
   DO JLON=1,ILON
      IF(ZFLOOD_STO(JLON,JLAT)>0.0)THEN
         PFFLOOD(JLON,JLAT) = PTAB_F(JLON,JLAT,IDOWN(JLON,JLAT))                                      &
                            + (ZFLOOD_STO           (JLON,JLAT) -PTAB_VF(JLON,JLAT,IDOWN(JLON,JLAT))) &
                            * (PTAB_F (JLON,JLAT,IUP(JLON,JLAT))-PTAB_F (JLON,JLAT,IDOWN(JLON,JLAT))) &
                            / (PTAB_VF(JLON,JLAT,IUP(JLON,JLAT))-PTAB_VF(JLON,JLAT,IDOWN(JLON,JLAT)))
         PHFLOOD(JLON,JLAT) = PTAB_H(JLON,JLAT,IDOWN(JLON,JLAT))                                      &
                            + (ZFLOOD_STO (JLON,JLAT)           -PTAB_VF(JLON,JLAT,IDOWN(JLON,JLAT))) &
                            * (PTAB_H (JLON,JLAT,IUP(JLON,JLAT))-PTAB_H (JLON,JLAT,IDOWN(JLON,JLAT))) &
                            / (PTAB_VF(JLON,JLAT,IUP(JLON,JLAT))-PTAB_VF(JLON,JLAT,IDOWN(JLON,JLAT))) 
      ENDIF
      IF(PFFLOOD(JLON,JLAT)>=1.0)THEN
         PFFLOOD(JLON,JLAT) = 1.0
         PHFLOOD(JLON,JLAT) = PTAB_H(JLON,JLAT,IUP(JLON,JLAT))                                   &
                            + (ZFLOOD_STO(JLON,JLAT)-PTAB_VF(JLON,JLAT,IUP(JLON,JLAT))) / XRHOLW
      ENDIF
   ENDDO
ENDDO
!
!-------------------------------------------------------------------------------
! * Calculate new Wflood, Lflood
!-------------------------------------------------------------------------------
!
WHERE(ZFLOOD_STO(:,:)>0.0)
  PFLOOD_LEN(:,:) = MIN(PLEN(:,:),XRATMED*SQRT(PFFLOOD(:,:)*PAREA(:,:)))
  PWFLOOD   (:,:) = PAREA(:,:)*PFFLOOD(:,:)/PFLOOD_LEN(:,:)
ENDWHERE
!
IF (LHOOK) CALL DR_HOOK('FLOOD_UPDATE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE FLOOD_UPDATE
