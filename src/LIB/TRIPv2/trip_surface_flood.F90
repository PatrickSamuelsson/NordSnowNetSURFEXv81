SUBROUTINE TRIP_SURFACE_FLOOD (KLISTING,PTSTEP,OPRINT,OMASK_FLD,        &
                               PTAB_F,PTAB_H,PTAB_VF,PAREA, PVEL,       &
                               PLEN,PWIDTH,PN_FLOOD,PHC,                &
                               PHS,PSURF_STO,PFLOOD_STO,PSOURCE,        &
                               PFLOOD_STO2,PHFLOOD,PFFLOOD,PFLOOD_LEN,  &
                               PWFLOOD,PQFR,PQRF,PVFIN,PVFOUT,PHSF,     &
                               PFSTO_ALL,PFSTO2_ALL,PSOURCE_ALL,        &
                               PFIN_ALL,PFOUT_ALL,PHF_ALL,PFF_ALL       ) 
!                               
!     ################################################################
!
!!****  *TRIP_SURFACE_FLOOD*  
!!
!!    PURPOSE
!!    -------
!
!     Calculate the flood storage in the next time step based on storages
!     of current time step using the Manning equation.
!     
!     Decharme et al., Clim. Dyn., 2012
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
!!      Original    09/01/14 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TRIP_PAR,    ONLY : XRHOLW, XM, XUNDEF
!
USE MODI_ABORT_TRIP
USE MODI_FLOOD_UPDATE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER, INTENT(IN)                  :: KLISTING
REAL,    INTENT(IN)                  :: PTSTEP ! Trip timestep value (10800s)
LOGICAL, INTENT(IN)                  :: OPRINT   !Printable budget key
!
LOGICAL, DIMENSION(:,:), INTENT(IN)  :: OMASK_FLD  !Floodplain mask
!
REAL,DIMENSION(:,:,:), INTENT(IN)    :: PTAB_F  ! Flood fraction array
REAL,DIMENSION(:,:,:), INTENT(IN)    :: PTAB_H  ! Topo height array
REAL,DIMENSION(:,:,:), INTENT(IN)    :: PTAB_VF ! Flood volume array
!
REAL,DIMENSION(:,:), INTENT(IN)      :: PAREA      ! Grid-cell area    [m²]
REAL,DIMENSION(:,:), INTENT(IN)      :: PVEL       ! river velocity    [m/s]
REAL,DIMENSION(:,:), INTENT(IN)      :: PLEN       ! river length       [m] 
REAL,DIMENSION(:,:), INTENT(IN)      :: PWIDTH     ! river widths                 [m]
REAL,DIMENSION(:,:), INTENT(IN)      :: PN_FLOOD   ! Manning coeficient over floodplains   [-] (0.1)
REAL,DIMENSION(:,:), INTENT(IN)      :: PHC        ! River bed depth              [m]
REAL,DIMENSION(:,:), INTENT(IN)      :: PHS        ! river channel height [m]
REAL,DIMENSION(:,:), INTENT(IN)      :: PSURF_STO  ! river channel storage at t    [kg]
REAL,DIMENSION(:,:), INTENT(IN)      :: PFLOOD_STO ! Floodplain water storage at t [kg]
REAL,DIMENSION(:,:), INTENT(IN)      :: PSOURCE    ! precip-infiltration-evaporation [kg/s]
!
REAL,DIMENSION(:,:), INTENT(INOUT)   :: PFLOOD_STO2! Floodplain water storage at t+1  [kg]
!
REAL,DIMENSION(:,:), INTENT(OUT)     :: PHFLOOD    ! Floodplain water depth       [m]
REAL,DIMENSION(:,:), INTENT(OUT)     :: PFFLOOD    ! Fraction of flood [-]
REAL,DIMENSION(:,:), INTENT(OUT)     :: PFLOOD_LEN ! Floodplain length along the river [m]
REAL,DIMENSION(:,:), INTENT(OUT)     :: PWFLOOD    ! Floodplain width             [m]
!
REAL,DIMENSION(:,:), INTENT(OUT)     :: PQFR  ! Flood flow to river          [kg/s]
REAL,DIMENSION(:,:), INTENT(OUT)     :: PQRF  ! River flow to floodplain     [kg/s]
REAL,DIMENSION(:,:), INTENT(OUT)     :: PVFIN ! River flow to flood velocity [m/s]
REAL,DIMENSION(:,:), INTENT(OUT)     :: PVFOUT! Flood flow to river velocity [m/s]
REAL,DIMENSION(:,:), INTENT(OUT)     :: PHSF  ! River-Floodplain depth comparison [m] during dt
!
REAL,                 INTENT(OUT)    :: PFSTO_ALL,PFSTO2_ALL,PSOURCE_ALL, &
                                        PFIN_ALL,PFOUT_ALL,PHF_ALL,PFF_ALL 
!                                       Final budget variable
!
!*      0.2    declarations of local variables
!
REAL, PARAMETER                            :: ZLEN_MIN = 1.E-6
!
REAL, DIMENSION(SIZE(PAREA,1),SIZE(PAREA,2)) :: ZMF
REAL, DIMENSION(SIZE(PAREA,1),SIZE(PAREA,2)) :: ZSLOPE
REAL, DIMENSION(SIZE(PAREA,1),SIZE(PAREA,2)) :: ZRADIUS
REAL, DIMENSION(SIZE(PAREA,1),SIZE(PAREA,2)) :: ZDIST
REAL, DIMENSION(SIZE(PAREA,1),SIZE(PAREA,2)) :: ZDELTA
REAL, DIMENSION(SIZE(PAREA,1),SIZE(PAREA,2)) :: ZMF_IN
REAL, DIMENSION(SIZE(PAREA,1),SIZE(PAREA,2)) :: ZMF_OUT
REAL, DIMENSION(SIZE(PAREA,1),SIZE(PAREA,2)) :: ZFLD_LEN
REAL, DIMENSION(SIZE(PAREA,1),SIZE(PAREA,2)) :: ZAREA_SG
REAL, DIMENSION(SIZE(PAREA,1),SIZE(PAREA,2)) :: ZVRIV
REAL, DIMENSION(SIZE(PAREA,1),SIZE(PAREA,2)) :: ZVFLD
REAL, DIMENSION(SIZE(PAREA,1),SIZE(PAREA,2)) :: ZVINER
!
REAL    :: ZAREA
!
INTEGER :: ILON, ILAT, JLON, JLAT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
! * Init
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_SURFACE_FLOOD',0,ZHOOK_HANDLE)
!
ILON = SIZE(PAREA,1)
ILAT = SIZE(PAREA,2)
!
PFLOOD_STO2(:,:) = 0.0
PHSF       (:,:) = 0.0
PVFIN      (:,:) = 0.0
PVFOUT     (:,:) = 0.0
PQFR       (:,:) = 0.0
PQRF       (:,:) = 0.0
!
ZMF        (:,:) = 0.0
ZSLOPE     (:,:) = 0.0
ZRADIUS    (:,:) = 0.0
ZDIST      (:,:) = 0.0
ZDELTA     (:,:) = 0.0
ZMF_IN     (:,:) = 0.0
ZMF_OUT    (:,:) = 0.0
ZFLD_LEN   (:,:) = 0.0
!
WHERE(OMASK_FLD(:,:))
   ZAREA_SG(:,:) = PAREA(:,:)-(PLEN(:,:)*PWIDTH(:,:))
ELSEWHERE
   ZAREA_SG(:,:) = PAREA(:,:)
ENDWHERE
!
!-------------------------------------------------------------------------------
! * Update the floodplain storage due to source (Precip inter - LEf - Infil)
!-------------------------------------------------------------------------------
!
WHERE(OMASK_FLD(:,:))
  PFLOOD_STO2(:,:)=PFLOOD_STO(:,:)+PTSTEP*PSOURCE(:,:)
ENDWHERE
!
!------------------------------------------------------------------
! * Update the floodplain geomorphological properties
!------------------------------------------------------------------
! 
CALL FLOOD_UPDATE(PTAB_F,PTAB_H,PTAB_VF,ZAREA_SG,PFLOOD_STO2, &
                  PLEN,PHFLOOD,PFFLOOD,PFLOOD_LEN,PWFLOOD     ) 
!   
ZFLD_LEN(:,:)=MAX(ZLEN_MIN,PFLOOD_LEN(:,:))
!
!------------------------------------------------------------------
! * Update the floodplain storage due to inflow or outflow
!------------------------------------------------------------------
!
DO JLAT=1,ILAT
   DO JLON=1,ILON
!
      IF(OMASK_FLD(JLON,JLAT))THEN
!
!        ------------------------------------------------------------------
!        Calculate the potential water mass exchange
!           
         PHSF(JLON,JLAT)=PHS(JLON,JLAT)-PHC(JLON,JLAT)-PHFLOOD(JLON,JLAT)
!
         ZMF   (JLON,JLAT)=PHSF(JLON,JLAT)*ZFLD_LEN(JLON,JLAT)*PWIDTH(JLON,JLAT)*XRHOLW
         ZDIST (JLON,JLAT)=0.5*(PWIDTH(JLON,JLAT)+PWFLOOD(JLON,JLAT))
         ZSLOPE(JLON,JLAT)=PHSF(JLON,JLAT)/ZDIST(JLON,JLAT)
!
      ENDIF
!                
!     ------------------------------------------------------------------
!     Floodplain inflow case
!
      IF(ZMF(JLON,JLAT)>0.0.AND.PFFLOOD(JLON,JLAT)<1.0)THEN
!
        ZRADIUS(JLON,JLAT) = PHS(JLON,JLAT)-PHC(JLON,JLAT)
        ZRADIUS(JLON,JLAT) = EXP(XM*LOG(ZRADIUS(JLON,JLAT)))
!
        ZVFLD (JLON,JLAT) = ZRADIUS(JLON,JLAT)*SQRT(ZSLOPE(JLON,JLAT))/PN_FLOOD(JLON,JLAT)
        ZVRIV (JLON,JLAT) = MAX(PVEL(JLON,JLAT),ZVFLD(JLON,JLAT))
        ZVINER(JLON,JLAT) = SQRT(ZVRIV(JLON,JLAT)*ZVFLD(JLON,JLAT))
        PVFIN (JLON,JLAT) = MIN(ZVINER(JLON,JLAT),ZDIST(JLON,JLAT)/PTSTEP)        
        PVFOUT(JLON,JLAT) = 0.0
!        
        ZMF_IN (JLON,JLAT) = ZMF(JLON,JLAT)
        ZMF_OUT(JLON,JLAT) = 0.0
!
      ENDIF
!                
!     ------------------------------------------------------------------
!     Floodplain outflow case
!
      IF(ZMF(JLON,JLAT)<0.0.AND.PFFLOOD(JLON,JLAT)>0.0)THEN
!
        ZRADIUS(JLON,JLAT) = PHFLOOD(JLON,JLAT)
        ZRADIUS(JLON,JLAT) = EXP(XM*LOG(ZRADIUS(JLON,JLAT)))
!
        PVFIN (JLON,JLAT) = 0.0
        ZVFLD (JLON,JLAT) = ZRADIUS(JLON,JLAT)*SQRT(-1.0*ZSLOPE(JLON,JLAT))/PN_FLOOD(JLON,JLAT)
        PVFOUT(JLON,JLAT) = MIN(ZVFLD(JLON,JLAT),ZDIST(JLON,JLAT)/PTSTEP)
!        
        ZMF_IN (JLON,JLAT) = 0.0
        ZMF_OUT(JLON,JLAT) = ABS(ZMF(JLON,JLAT))
!
        ZDELTA(JLON,JLAT) = MERGE(1.0,0.0,PFLOOD_STO2(JLON,JLAT)<ZMF_OUT(JLON,JLAT))
!
      ENDIF
!
!     ------------------------------------------------------------------
!     Update the floodplain storage
!
      IF(OMASK_FLD(JLON,JLAT).AND.ZMF(JLON,JLAT)/=0.0)THEN
!
         PQRF(JLON,JLAT) = ZMF_IN (JLON,JLAT)*PVFIN (JLON,JLAT)/ZDIST(JLON,JLAT)
!
         PQFR(JLON,JLAT) = (1.0-ZDELTA(JLON,JLAT)) * ZMF_OUT(JLON,JLAT)*PVFOUT(JLON,JLAT)/ZDIST(JLON,JLAT) &
                         +      ZDELTA(JLON,JLAT)  * PFLOOD_STO2(JLON,JLAT)/PTSTEP
!
         PFLOOD_STO2(JLON,JLAT) = PFLOOD_STO2(JLON,JLAT)+PTSTEP*(PQRF(JLON,JLAT)-PQFR(JLON,JLAT))      
!
      ENDIF
!
  ENDDO
ENDDO
!
!------------------------------------------------------------------
! * Update the floodplain geomorphological properties
!------------------------------------------------------------------
!
CALL FLOOD_UPDATE(PTAB_F,PTAB_H,PTAB_VF,ZAREA_SG,PFLOOD_STO2, &
                  PLEN,PHFLOOD,PFFLOOD,PFLOOD_LEN,PWFLOOD     )
!
!-------------------------------------------------------------------------------
! * Budget calculation
!-------------------------------------------------------------------------------
!
IF(OPRINT)THEN
!
  PFSTO_ALL   = 0.0
  PFSTO2_ALL  = 0.0
  PSOURCE_ALL = 0.0
  PFIN_ALL    = 0.0
  PFOUT_ALL   = 0.0
  PHF_ALL     = 0.0
  PFF_ALL     = 0.0
  ZAREA       = 0.0
!
  DO JLAT=1,ILAT
     DO JLON=1,ILON
        IF(OMASK_FLD(JLON,JLAT))THEN                
           PFSTO_ALL  = PFSTO_ALL  + PFLOOD_STO (JLON,JLAT) / PAREA(JLON,JLAT)
           PFSTO2_ALL = PFSTO2_ALL + PFLOOD_STO2(JLON,JLAT) / PAREA(JLON,JLAT)
           PFIN_ALL   = PFIN_ALL   + PQRF       (JLON,JLAT) / PAREA(JLON,JLAT)
           PFOUT_ALL  = PFOUT_ALL  + PQFR       (JLON,JLAT) / PAREA(JLON,JLAT)
           PSOURCE_ALL= PSOURCE_ALL+ PSOURCE    (JLON,JLAT) / PAREA(JLON,JLAT)
           PHF_ALL    = PHF_ALL    + PHFLOOD    (JLON,JLAT) * PAREA(JLON,JLAT)
           PFF_ALL    = PFF_ALL    + PFFLOOD    (JLON,JLAT) * PAREA(JLON,JLAT)
           ZAREA      = ZAREA      + PAREA      (JLON,JLAT)
        ENDIF
    ENDDO
  ENDDO
!
  IF(ZAREA>0.0)THEN
    PHF_ALL = PHF_ALL / ZAREA
    PFF_ALL = PFF_ALL / ZAREA
  ENDIF
!
ENDIF
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('TRIP_SURFACE_FLOOD',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
END SUBROUTINE TRIP_SURFACE_FLOOD
