!     #########
      SUBROUTINE TRIP (KLISTING,HGROUNDW,OFLOOD,OPRINT,PTSTEP,            &
                         KGRCN,KSEQ,KNEXTX,KNEXTY,KSEQMAX,PAREA,PLEN,     &
                         OMASK_GW,OMASK_VEL,OMASK_FLD,                    &
                         PTAUG,PFLOOD_LEN,PSLOPEBED,PWIDTH,PN,            &
                         PN_FLOOD,PHC_BED,PWFLOOD,PTAB_F,PTAB_H,PTAB_VF,  &
                         PDRAIN,PRUNOFF,PSOURCE,PHS,PVEL,                 &
                         PGROUND_STO,PSURF_STO,                           &
                         PFLOOD_STO,PSOUT,PGOUT,PHFLOOD,                  &
                         PFFLOOD,PQFR,PQRF,PVFIN,PVFOUT,                  &
                         PHSF,PSIN,KTRIP,KTSEPT,KTSTEP_END,               &  
                         PGSTO_ALL,PGSTO2_ALL,PGIN_ALL,PGOUT_ALL,         &
                         PHGROUND,PWEFF                                   )
!     ###################################################################
!
!!****  *TRIP*  
!!
!!    PURPOSE
!!    -------
!
!     TRIP river routing and Floodplains schemes.
!     
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
!!      Modif.      28/05/08 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TRIP_PAR, ONLY : XRHOLW,XSEA,XYEAR
!
USE MODI_TRIP_GW_BUFFER_TANK
USE MODI_TRIP_SURFACE_WATER
USE MODI_TRIP_SURFACE_FLOOD
!
USE MODI_TRIP_UPDATE_AND_CONSERV
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER, INTENT(IN)                  :: KLISTING
INTEGER, INTENT(IN)                  :: KTRIP
INTEGER, INTENT(IN)                  :: KTSEPT
INTEGER, INTENT(IN)                  :: KTSTEP_END
!
 CHARACTER(LEN=3), INTENT(IN)         :: HGROUNDW !Groundwater scheme key
!
LOGICAL, INTENT(IN)                  :: OFLOOD   !Flood scheme key
LOGICAL, INTENT(IN)                  :: OPRINT   !Printable budget key 
!
REAL, INTENT(IN)                     :: PTSTEP   !Trip timestep
!
INTEGER, INTENT(IN)                  :: KSEQMAX  !maximum down flow
INTEGER, DIMENSION(:,:),INTENT(IN)   :: KGRCN    !Flow direction (1->8)
INTEGER, DIMENSION(:,:),INTENT(IN)   :: KSEQ     !River sequence
INTEGER, DIMENSION(:,:),INTENT(IN)   :: KNEXTX   !returns x and y point
INTEGER, DIMENSION(:,:),INTENT(IN)   :: KNEXTY   !of destination grid
!
LOGICAL, DIMENSION(:,:), INTENT(IN)  :: OMASK_GW   !Groundwater mask
LOGICAL, DIMENSION(:,:), INTENT(IN)  :: OMASK_VEL  !Variable velocity mask
LOGICAL, DIMENSION(:,:), INTENT(IN)  :: OMASK_FLD  !Floodplain mask
!
REAL,DIMENSION(:,:), INTENT(IN)      :: PTAUG      ! ground water transfer time  [s]
REAL,DIMENSION(:,:), INTENT(IN)      :: PLEN       ! river length       [m] 
REAL,DIMENSION(:,:), INTENT(IN)      :: PSLOPEBED  ! river bed slopes             [m/m]
REAL,DIMENSION(:,:), INTENT(IN)      :: PWIDTH     ! river widths                 [m]
REAL,DIMENSION(:,:), INTENT(IN)      :: PN         ! Manning roughness coeficient [-] (0.03 to 0.065)
REAL,DIMENSION(:,:), INTENT(IN)      :: PN_FLOOD   ! Manning coeficient over floodplains   [-] (0.1)
REAL,DIMENSION(:,:), INTENT(IN)      :: PHC_BED    ! River bed depth              [m]
REAL,DIMENSION(:,:), INTENT(IN)      :: PSOURCE    ! precip-infiltration-evaporation [kg/s]
REAL,DIMENSION(:,:), INTENT(IN)      :: PAREA      ! Grid-cell area    [m2]
REAL,DIMENSION(:,:), INTENT(IN)      :: PDRAIN     ! Subsurface runoff from ISBA [kg/s]
REAL,DIMENSION(:,:), INTENT(INOUT)   :: PRUNOFF    ! Surface runoff from ISBA    [kg/s]
!
REAL,DIMENSION(:,:), INTENT(IN)      :: PHS   ! River height [m]
REAL,DIMENSION(:,:), INTENT(IN)      :: PVEL  ! River channel velocity  [m/s]
!
REAL,DIMENSION(:,:), INTENT(INOUT)   :: PHFLOOD    ! Floodplain water depth       [m]
REAL,DIMENSION(:,:), INTENT(INOUT)   :: PWFLOOD    ! Floodplain width             [m]
REAL,DIMENSION(:,:), INTENT(INOUT)   :: PFLOOD_LEN ! Floodplain length along the river [m]
REAL,DIMENSION(:,:), INTENT(INOUT)   :: PFFLOOD    ! Fraction of flood [-]
REAL,DIMENSION(:,:), INTENT(INOUT)   :: PSURF_STO  ! river channel storage at t    [kg]
REAL,DIMENSION(:,:), INTENT(INOUT)   :: PGROUND_STO! groundwater storage at t    [kg]
REAL,DIMENSION(:,:), INTENT(INOUT)   :: PFLOOD_STO ! Floodplain water storage at t [kg]
!
REAL,DIMENSION(:,:), INTENT(OUT)     :: PSOUT ! Outflow from the surface river reservoir [kg/s]
REAL,DIMENSION(:,:), INTENT(OUT)     :: PSIN  ! Inflow to the surface river reservoir [kg/s]
REAL,DIMENSION(:,:), INTENT(INOUT)   :: PGOUT ! ground water outflow        [kg/s]
REAL,DIMENSION(:,:), INTENT(OUT)     :: PQFR  ! Flood flow to river
REAL,DIMENSION(:,:), INTENT(OUT)     :: PQRF  ! River flow to floodplain
!
REAL,DIMENSION(:,:), INTENT(OUT)     :: PVFIN ! River flow to flood velocity [m/s]
REAL,DIMENSION(:,:), INTENT(OUT)     :: PVFOUT! Flood flow to river velocity [m/s]
REAL,DIMENSION(:,:), INTENT(OUT)     :: PHSF  ! River-Floodplain depth comparison [m]
!
REAL,DIMENSION(:,:,:), INTENT(IN)    :: PTAB_F  ! Flood fraction array
REAL,DIMENSION(:,:,:), INTENT(IN)    :: PTAB_H  ! Topo height array
REAL,DIMENSION(:,:,:), INTENT(IN)    :: PTAB_VF ! Flood volume array
!
REAL,                 INTENT(INOUT)  :: PGSTO_ALL  !Global groundwater storage at t    [kg]
REAL,                 INTENT(INOUT)  :: PGSTO2_ALL !Global groundwater storage at t-1  [kg]
REAL,                 INTENT(INOUT)  :: PGIN_ALL   !Global gw recharge + lateral input [kg/m2/s]
REAL,                 INTENT(INOUT)  :: PGOUT_ALL  !Global gw outflow                  [kg/m2/s]
!
REAL,DIMENSION(:,:), INTENT(INOUT)   :: PHGROUND   ! water table elevation         [m]
REAL,DIMENSION(:,:), INTENT(IN)      :: PWEFF      ! Effective porosity            [-]
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PLEN,1),SIZE(PLEN,2)) :: ZSURF_STO2  ! river channel storage at t+1     [kg]    
REAL, DIMENSION(SIZE(PLEN,1),SIZE(PLEN,2)) :: ZGROUND_STO2! Groundwater storage at t+1       [kg] 
REAL, DIMENSION(SIZE(PLEN,1),SIZE(PLEN,2)) :: ZFLOOD_STO2 ! Floodplain water storage at t+1  [kg] 
REAL, DIMENSION(SIZE(PLEN,1),SIZE(PLEN,2)) :: ZQFR
REAL, DIMENSION(SIZE(PLEN,1),SIZE(PLEN,2)) :: ZQRF
!
REAL    ::   ZFSTO_ALL, ZFSTO2_ALL, ZSOURCE_ALL, ZSSTO_ALL, ZSSTO2_ALL, &
             ZSIN_ALL, ZDRUN_ALL, ZSOUT_ALL, ZVEL_ALL, ZFIN_ALL,        &
             ZFOUT_ALL, ZHS_ALL,ZHF_ALL,ZFF_ALL, ZRECUP_ALL 
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
! * Init
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP',0,ZHOOK_HANDLE)
!
ZFSTO_ALL   = 0.0
ZFSTO2_ALL  = 0.0
ZSOURCE_ALL = 0.0
ZSSTO_ALL   = 0.0
ZSSTO2_ALL  = 0.0
ZSIN_ALL    = 0.0
ZDRUN_ALL   = 0.0
ZSOUT_ALL   = 0.0
ZVEL_ALL    = 0.0
ZFIN_ALL    = 0.0
ZFOUT_ALL   = 0.0
ZHS_ALL     = 0.0
ZHF_ALL     = 0.0
ZFF_ALL     = 0.0
ZRECUP_ALL  = 0.0
!
ZSURF_STO2   (:,:) = 0.0
ZGROUND_STO2 (:,:) = 0.0
ZFLOOD_STO2  (:,:) = 0.0
!
ZQFR(:,:) = 0.0
ZQRF(:,:) = 0.0
!
!-------------------------------------------------------------------------------
! *  Ground water storage
!-------------------------------------------------------------------------------
!
IF(HGROUNDW=='DEF')THEN
!
   PGOUT(:,:)=MAX(PDRAIN(:,:),0.0)
!
ELSEIF(HGROUNDW=='CST')THEN
!        
  CALL TRIP_GW_BUFFER_TANK(PTSTEP,OPRINT,PAREA,OMASK_GW,PGROUND_STO,ZGROUND_STO2,    &
                           PDRAIN,PTAUG,PGOUT,PGSTO_ALL,PGSTO2_ALL,PGIN_ALL,PGOUT_ALL)  
!
ENDIF
!
!-------------------------------------------------------------------------------
! * Floodplains storage
!-------------------------------------------------------------------------------
!
IF(OFLOOD)THEN    
!        
   CALL TRIP_SURFACE_FLOOD(KLISTING,PTSTEP,OPRINT,OMASK_FLD,        &
                           PTAB_F,PTAB_H,PTAB_VF,PAREA,PVEL,        &
                           PLEN,PWIDTH,PN_FLOOD,PHC_BED,            &
                           PHS,PSURF_STO,PFLOOD_STO,PSOURCE,        &
                           ZFLOOD_STO2,PHFLOOD,PFFLOOD,PFLOOD_LEN,  &
                           PWFLOOD,PQFR,PQRF,PVFIN,PVFOUT,PHSF,     &
                           ZFSTO_ALL,ZFSTO2_ALL,ZSOURCE_ALL,        &
                           ZFIN_ALL,ZFOUT_ALL,ZHF_ALL,ZFF_ALL       )
!
   ZQFR(:,:) = PQFR(:,:)
   ZQRF(:,:) = PQRF(:,:)
!
ENDIF
!
!-------------------------------------------------------------------------------
! * Surface water storage
!-------------------------------------------------------------------------------
!       
 CALL TRIP_SURFACE_WATER(KLISTING,PTSTEP,KGRCN,KSEQ,KNEXTX,KNEXTY,KSEQMAX, &
                        OPRINT,OMASK_VEL,PLEN,PRUNOFF,                    &
                        PVEL,PHS,PSURF_STO,ZSURF_STO2,PGOUT,PSIN,PSOUT,   &
                        PAREA,ZQFR,ZQRF,                                  &
                        ZSSTO_ALL,ZSSTO2_ALL,ZSIN_ALL,ZDRUN_ALL,          &
                        ZSOUT_ALL,ZVEL_ALL,ZHS_ALL                        )
!
!-------------------------------------------------------------------------------
! * Update all reservoir and conserve water mass as possible
!-------------------------------------------------------------------------------
!
 CALL TRIP_UPDATE_AND_CONSERV(OPRINT,OFLOOD,HGROUNDW,PAREA,PWEFF,   &
                             ZSURF_STO2,ZFLOOD_STO2,ZGROUND_STO2,  &
                             PSURF_STO,PFLOOD_STO,PGROUND_STO,     &
                             OMASK_GW,PHGROUND,ZRECUP_ALL          )
!
!-------------------------------------------------------------------------------
! * Writting the budget
!-------------------------------------------------------------------------------
!
IF(OPRINT.AND.KTRIP==1.AND.KTSEPT==1)THEN
!
  WRITE(KLISTING,*)''
  WRITE(KLISTING,*)'        START RUN ISBA-TRIP-FP     '
  WRITE(KLISTING,*)'          Budget en  kg/m2         '
  WRITE(KLISTING,*)''
  WRITE(KLISTING,'(a90)')'RUN TSTEP   S_err    F_err  G_err    MR(mm/y) Vel(m/s)     Hs       Hf        Ff    UNCSV'
  WRITE(KLISTING,*)''
!  
ENDIF
!
IF(OPRINT) THEN
!
    WRITE(KLISTING,'(i3,1x,i3,3(2x,g8.2),f8.2,f8.2,8(2x,f8.3))')         &
    KTRIP,KTSEPT,                                                        &
!   surface budget S_err
    (ZSSTO_ALL-ZSSTO2_ALL)+PTSTEP*(ZSIN_ALL-ZSOUT_ALL),                  &
!   floodplains budget F_err
    (ZFSTO_ALL-ZFSTO2_ALL)+PTSTEP*(ZSOURCE_ALL+ZFIN_ALL-ZFOUT_ALL),      &
!   ground budget G_err
    (PGSTO_ALL-PGSTO2_ALL)+PTSTEP*(PGIN_ALL-PGOUT_ALL),                  &
!   output flow to the sea, MR(mm/y) 
    (ZSSTO_ALL-ZSSTO2_ALL +PTSTEP*ZDRUN_ALL)/XSEA*XYEAR*REAL(KTSTEP_END),&
!   mean flow velocity, Vel(m/s), and mean stream depth, Hs (m) 
!   mean floddplains depth, Hf (m), and mean flood fraction, Ff (%) 
    ZVEL_ALL,ZHS_ALL,ZHF_ALL,100.*ZFF_ALL,ZRECUP_ALL
!    
ENDIF
!
IF (LHOOK) CALL DR_HOOK('TRIP',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
END SUBROUTINE TRIP
