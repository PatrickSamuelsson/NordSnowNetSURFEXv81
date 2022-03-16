!     ###################################################################
      SUBROUTINE GWF (TPG,                                                &
                      KLON,KLAT,OPRINT,PTSTEP_RUN,PTSTEP,OMASK,PNUM_AQUI, &
                      PDRAIN,PLEN,PWIDTH,PHC_BED,PTOPO_RIV,PTAUG,         &
                      PAREA,PTRANS,PWEFF,PTABGW_F,PTABGW_H,PHS,           &
                      PHGROUND,PHG_OLD,PSURF_STO,PQGCELL,PWTD,PFWTD,      &
                      PHGHS,PGOUT,PGNEG,                                  &
                      PGSTO_ALL,PGSTO2_ALL,PGIN_ALL,PGOUT_ALL             )
!     ###################################################################
!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!!
!!     
!!
!!    AUTHOR
!!    ------
!!	J.P Vergnes   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original     08/12
!!      09/16   B. Decharme  limit wtd to -1000m
!-------------------------------------------------------------------------------
!
USE MODD_TRIP_GRID, ONLY : TRIP_GRID_t
!
USE MODD_TRIP_PAR
!
USE MODI_GET_LAT_GWF
USE MODI_GWF_INT
USE MODI_GWF_BUDGET
USE MODI_GWF_SOLVER
USE MODI_GWF_CPL_UPDATE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(TRIP_GRID_t), INTENT(INOUT) :: TPG
!
INTEGER,              INTENT(IN)    :: KLON
INTEGER,              INTENT(IN)    :: KLAT
LOGICAL,              INTENT(IN)    :: OPRINT
!
REAL,                 INTENT(IN)    :: PTSTEP_RUN
REAL,                 INTENT(IN)    :: PTSTEP
!
LOGICAL, DIMENSION(:,:), INTENT(IN) :: OMASK      ! Aquifer mask
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PNUM_AQUI  ! Aquifer ref number
REAL, DIMENSION(:,:), INTENT(IN)    :: PDRAIN     ! Input drainage                [kg/s]
REAL, DIMENSION(:,:), INTENT(IN)    :: PLEN       ! river length                  [m]
REAL, DIMENSION(:,:), INTENT(IN)    :: PWIDTH     ! river widths                  [m]
REAL, DIMENSION(:,:), INTENT(IN)    :: PHC_BED    ! River bed depth               [m]
REAL, DIMENSION(:,:), INTENT(IN)    :: PTOPO_RIV  ! River elevatation             [m]
REAL, DIMENSION(:,:), INTENT(IN)    :: PTAUG      ! ground water transfer time    [s]
REAL, DIMENSION(:,:), INTENT(IN)    :: PAREA      ! Grid-cell area                [m2]
REAL, DIMENSION(:,:), INTENT(IN)    :: PTRANS     ! Transmissivity                [m2/s]
REAL, DIMENSION(:,:), INTENT(IN)    :: PWEFF      ! Effective porosity            [-]
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PTABGW_F   !  Groundwater fraction         [-]
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PTABGW_H   ! Topo height                   [m]
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PHS        ! river height at t             [m]
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PHGROUND   ! water table elevation         [m]
REAL, DIMENSION(:,:), INTENT(INOUT) :: PHG_OLD    ! water table elevation at t-1  [m]
!
REAL, DIMENSION(:,:), INTENT(INOUT), OPTIONAL :: PSURF_STO  ! river channel storage at t    [kg]
!
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL   :: PQGCELL
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL   :: PWTD
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL   :: PFWTD
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL   :: PHGHS
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL   :: PGOUT
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL   :: PGNEG
!
REAL,                 INTENT(OUT), OPTIONAL   :: PGSTO_ALL
REAL,                 INTENT(OUT), OPTIONAL   :: PGSTO2_ALL
REAL,                 INTENT(OUT), OPTIONAL   :: PGIN_ALL
REAL,                 INTENT(OUT), OPTIONAL   :: PGOUT_ALL
!
!*      0.2    declarations of local parameter
!
REAL, PARAMETER       :: ZEPSILON = 1.E-12
INTEGER, PARAMETER    :: IITERMAX = 100
!
REAL, PARAMETER       :: ZGWDZMIN = 10.0    ! Thickness to start to decrease lateral Transmissivity [m]
REAL, PARAMETER       :: ZGWERR   = 0.01    ! Limit of 1cm to limit negatif ground_sto [m]
!
!
!*      0.3    declarations of local variables
!
!
REAL, DIMENSION(KLAT) :: ZLAT
REAL                  :: ZGRID_RES
!
REAL, DIMENSION(KLON,KLAT)          :: ZHDRAIN_RIV      !
REAL, DIMENSION(KLON,KLAT)          :: ZRIVERBED        !
REAL, DIMENSION(KLON,KLAT)          :: ZGWDEEP          !
REAL, DIMENSION(KLON,KLAT)          :: ZCRIV            !
REAL, DIMENSION(KLON,KLAT)          :: ZCC              !
REAL, DIMENSION(KLON,KLAT)          :: ZCR              !
REAL, DIMENSION(KLON,KLAT)          :: ZHCOF            !
REAL, DIMENSION(KLON,KLAT)          :: ZRHS             !
REAL, DIMENSION(KLON,KLAT)          :: ZQDRAIN
REAL, DIMENSION(KLON,KLAT)          :: ZTAUG
REAL, DIMENSION(KLON,KLAT)          :: ZSLOPE
REAL, DIMENSION(KLON,KLAT)          :: ZTRANS
!
REAL                                :: ZEVOL            !
REAL                                :: ZNPTS            ! Number of points in aquifer basins
!
INTEGER                             :: IITER, JLON, JLAT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
! *     1.  INITIALIZATION
!           --------------
!
IF (LHOOK) CALL DR_HOOK('GWF',0,ZHOOK_HANDLE)
!
! * Initialisation variables
!
ZHCOF   (:,:) = XUNDEF
ZRHS    (:,:) = XUNDEF
ZGWDEEP (:,:) = 0.0
ZCR     (:,:) = 0.0
ZCC     (:,:) = 0.0
ZCRIV   (:,:) = 0.0
ZQDRAIN (:,:) = 0.0
ZTRANS  (:,:) = 0.0
!
! * groundwater mask
!
ZNPTS = REAL(COUNT(OMASK(:,:)))
!
! * save old hground
!
PHG_OLD(:,:) = PHGROUND(:,:)
!
! * Niveau de drainage en riviere
!
ZRIVERBED  (:,:) = PTOPO_RIV(:,:) - PHC_BED(:,:)
ZHDRAIN_RIV(:,:) = ZRIVERBED(:,:) + MIN(PHC_BED(:,:),PHS(:,:))
!
! * grid 
!
 CALL GET_LAT_GWF(TPG, &
                 KLAT,ZGRID_RES,ZLAT)
!
! * Coefficients nappe/riviere
!
WHERE(OMASK(:,:))
     ZSLOPE(:,:) = MIN(1.0,MAX(0.0,PHGROUND(:,:)-ZRIVERBED(:,:))/PHC_BED(:,:))
     ZTAUG (:,:) = PTAUG(:,:)-(PTAUG(:,:)-XDAY)*ZSLOPE(:,:)
     ZCRIV (:,:) = PWIDTH(:,:) * PLEN(:,:)/ZTAUG(:,:)
ENDWHERE
!
! * Transmissivity
!
WHERE(OMASK(:,:))
     ZSLOPE(:,:) = MIN(1.0,MAX(0.0,PHGROUND(:,:)-PTOPO_RIV(:,:)+XGWDZMAX-ZGWERR)/ZGWDZMIN)
     ZTRANS(:,:) = PTRANS(:,:)*ZSLOPE(:,:)
ENDWHERE
!
 CALL GWF_INT(KLON,KLAT,ZGRID_RES,ZLAT,OMASK,PNUM_AQUI,ZTRANS,ZCR,ZCC)
!
! *     2.  ITERATION LOOP
!           --------------
!
ZEVOL    = 1.0
IITER    = 0
!
DO WHILE (ZEVOL>ZEPSILON.AND.IITER<=IITERMAX)
!      
  DO JLAT=1, KLAT
     DO JLON=1, KLON
!     
        IF(OMASK(JLON,JLAT))THEN
        
!          initialisations des coefficients
           IF(PHGROUND(JLON,JLAT)<ZHDRAIN_RIV(JLON,JLAT).AND.PHS(JLON,JLAT)<=XHSMIN)THEN
              ZCRIV   (JLON,JLAT) = 0.0
           ENDIF
           IF(PHGROUND(JLON,JLAT)<ZRIVERBED(JLON,JLAT))THEN
              ZCRIV   (JLON,JLAT) = 0.0
              ZGWDEEP (JLON,JLAT) = MAX(0.,(PHS(JLON,JLAT)-XHSMIN))*PWIDTH(JLON,JLAT)*PLEN(JLON,JLAT)/ZTAUG(JLON,JLAT)
           ENDIF
!           
!          formulation des coefficients 
           ZHCOF(JLON,JLAT) = ZCRIV (JLON,JLAT) + (PWEFF(JLON,JLAT)*PAREA(JLON,JLAT)/PTSTEP_RUN)
           ZRHS (JLON,JLAT) = PDRAIN(JLON,JLAT)/XRHOLW + ZGWDEEP (JLON,JLAT) + ZCRIV(JLON,JLAT)*ZHDRAIN_RIV(JLON,JLAT)  &
                      + (PWEFF(JLON,JLAT)*PAREA(JLON,JLAT)/PTSTEP_RUN)*PHG_OLD(JLON,JLAT)
!
        ENDIF
!
     ENDDO
   ENDDO
!
!  approximation
   CALL GWF_SOLVER(KLON,KLAT,ZNPTS,OMASK,ZHCOF,ZRHS,ZCR,ZCC,PHGROUND,ZEVOL)
!
   IITER = IITER +1
!
ENDDO
!
IF(.NOT.PRESENT(PSURF_STO))THEN
  IF (LHOOK) CALL DR_HOOK('GWF',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
! *     3.   WATER BUDGET
!            ------------
!
 CALL GWF_BUDGET(KLON,KLAT,OMASK,PHGROUND,ZHDRAIN_RIV, &
                ZGWDEEP,ZCR,ZCC,ZCRIV,PQGCELL,ZQDRAIN )
!
DO JLAT=1, KLAT
   DO JLON=1, KLON
      IF(OMASK(JLON,JLAT))THEN
         PHGHS    (JLON,JLAT) = PHGROUND(JLON,JLAT)-ZHDRAIN_RIV(JLON,JLAT)
         PGOUT    (JLON,JLAT) = MAX(0.0,ZQDRAIN(JLON,JLAT))
         PGNEG    (JLON,JLAT) = MIN(0.0,ZQDRAIN(JLON,JLAT))
         PSURF_STO(JLON,JLAT) = PSURF_STO(JLON,JLAT)+PGNEG(JLON,JLAT)*PTSTEP_RUN
      ELSE
         PGOUT (JLON,JLAT) = PDRAIN(JLON,JLAT)
         PGNEG (JLON,JLAT) = 0.0
      ENDIF
   ENDDO
ENDDO
!
! *     4.   GLOBAL BUDGET
!            -------------
!
IF(OPRINT)THEN
!
  PGSTO_ALL  = 0.0
  PGSTO2_ALL = 0.0
  PGIN_ALL   = 0.0
  PGOUT_ALL  = 0.0
!
!
  DO JLAT=1, KLAT
     DO JLON=1, KLON
        IF(OMASK(JLON,JLAT))THEN
           PGSTO_ALL  = PGSTO_ALL  + PWEFF(JLON,JLAT)*PHG_OLD (JLON,JLAT)*XRHOLW
           PGSTO2_ALL = PGSTO2_ALL + PWEFF(JLON,JLAT)*PHGROUND(JLON,JLAT)*XRHOLW
           PGIN_ALL   = PGIN_ALL   + PDRAIN(JLON,JLAT)*PTSTEP_RUN/(PTSTEP*PAREA(JLON,JLAT))
           PGOUT_ALL  = PGOUT_ALL  + (PGOUT(JLON,JLAT)+PGNEG(JLON,JLAT)-PQGCELL(JLON,JLAT)) &
                                   * PTSTEP_RUN/(PTSTEP*PAREA(JLON,JLAT))
        ENDIF
     ENDDO
  ENDDO
!
ENDIF
!
! *     5.   WTD COUPLING
!            ------------
!
PWTD   (:,:) = XUNDEF
PFWTD  (:,:) = XUNDEF
PHG_OLD(:,:) = XUNDEF
!
 CALL GWF_CPL_UPDATE(PTABGW_H,PTABGW_F,OMASK,PTOPO_RIV, &
                    PHC_BED,PHGROUND,PHG_OLD,PWTD,PFWTD)
!
IF (LHOOK) CALL DR_HOOK('GWF',1,ZHOOK_HANDLE)
!
END SUBROUTINE GWF
