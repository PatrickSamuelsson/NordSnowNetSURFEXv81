!     #########
SUBROUTINE TRIP_SURFACE_WATER (KLISTING,PTSTEP,KGRCN,KSEQ,KNEXTX,KNEXTY,KSEQMAX, &
                               OPRINT,OMASK_VEL,PLEN,PRUNOFF,                    &
                               PVEL,PHS,PSURF_STO,PSURF_STO2,PGOUT,PSIN,PSOUT,   &
                               PAREA,PQFR,PQRF,                                  &
                               PSSTO_ALL,PSSTO2_ALL,PSIN_ALL,PDRUN_ALL,          &
                               PSOUT_ALL,PVEL_ALL,PHS_ALL                        ) 
!     ################################################################
!
!!****  *TRIP_SURFACE_WATER*  
!!
!!    PURPOSE
!!    -------
!
!     Calculate the river storage in the next time step based on the storage of current time step 
!     Where OMASK_VEL=true the Manning equation is used to compute a variable flow velocity.
!
!     
!!**  METHOD
!!    ------
!
!     RK Ordre 4 Rang 4
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
!!    B. Decharme
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/02/09 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODN_TRIP,     ONLY : XCVEL
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
INTEGER, INTENT(IN)                  :: KLISTING
!
REAL, INTENT(IN)                     :: PTSTEP ! Trip timestep value (10800s)
!
INTEGER, DIMENSION(:,:),INTENT(IN)   :: KGRCN  ! Flow direction (1->8)
INTEGER, DIMENSION(:,:),INTENT(IN)   :: KSEQ   ! River sequence
INTEGER, DIMENSION(:,:),INTENT(IN)   :: KNEXTX ! returns x and y point
INTEGER, DIMENSION(:,:),INTENT(IN)   :: KNEXTY ! of destination grid:
!                                                                    8 1 2
!                                                                    7   3
!                                                                    6 5 4
!
INTEGER, INTENT(IN)                    :: KSEQMAX ! maximum down flow
LOGICAL, INTENT(IN)                    :: OPRINT   !Printable budget key
!
LOGICAL, DIMENSION(:,:), INTENT(IN)    :: OMASK_VEL  ! Variable velocity mask
REAL,    DIMENSION(:,:), INTENT(IN)    :: PLEN       ! river length       [m] 
REAL,    DIMENSION(:,:), INTENT(IN)    :: PAREA      ! Grid-cell area    [m²]
REAL,    DIMENSION(:,:), INTENT(IN)    :: PRUNOFF    ! Surface runoff from ISBA    [kg/s]
REAL,    DIMENSION(:,:), INTENT(IN)    :: PGOUT      ! ground water outflow        [kg/s]
REAL,    DIMENSION(:,:), INTENT(IN)    :: PQFR       ! Flood flow to river         [kg/s]
REAL,    DIMENSION(:,:), INTENT(IN)    :: PQRF       ! River flow to floodplain    [kg/s]
REAL,    DIMENSION(:,:), INTENT(IN)    :: PHS        ! river channel height [m]
REAL,    DIMENSION(:,:), INTENT(IN)    :: PVEL       ! River channel velocity  [m/s]
REAL,    DIMENSION(:,:), INTENT(IN)    :: PSURF_STO  ! river channel storage at t  [kg]
!
REAL,    DIMENSION(:,:), INTENT(INOUT) :: PSURF_STO2 ! river channel storage at t+1[kg]
!
REAL,    DIMENSION(:,:), INTENT(OUT)   :: PSIN  ! Inflow to the surface river reservoir [kg/s]
REAL,    DIMENSION(:,:), INTENT(OUT)   :: PSOUT ! Outflow from the surface river reservoir [kg/s]
!
REAL,                    INTENT(OUT)   :: PSSTO_ALL,PSSTO2_ALL,PSIN_ALL,    &
                                          PDRUN_ALL,PSOUT_ALL,PVEL_ALL,     &
                                          PHS_ALL 
!                                         Final budget variable
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PLEN,1),SIZE(PLEN,2)) :: ZQIN
REAL, DIMENSION(SIZE(PLEN,1),SIZE(PLEN,2)) :: ZRADIUS
REAL, DIMENSION(SIZE(PLEN,1),SIZE(PLEN,2)) :: ZQOUT
REAL, DIMENSION(SIZE(PLEN,1),SIZE(PLEN,2)) :: ZSTOMAX
!
REAL    :: ZAREA
!
INTEGER :: ILON, ILAT, JLON, JLAT, ISEQ, INEXTX, INEXTY
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
! * Init
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_SURFACE_WATER',0,ZHOOK_HANDLE)
!
ILON = SIZE(PLEN,1)
ILAT = SIZE(PLEN,2)
!
PSURF_STO2 (:,:) = 0.0
PSIN       (:,:) = 0.0
PSOUT      (:,:) = 0.0
!
ZQIN       (:,:) = 0.0
ZRADIUS    (:,:) = 0.0
ZQOUT      (:,:) = 0.0
ZSTOMAX    (:,:) = 0.0
!
!-------------------------------------------------------------------------------
! * Sequence loop (optimized computation)
!-------------------------------------------------------------------------------
!
ISEQ=1
 CALL SEQUENCE_LOOP(ISEQ)
!
IF(KSEQMAX>2)THEN
  ISEQ=2
  CALL SEQUENCE_LOOP(ISEQ)
ENDIF
!
IF(KSEQMAX>3)THEN
  DO ISEQ=3,KSEQMAX-1
     CALL SEQUENCE_LOOP(ISEQ)
  ENDDO
ENDIF
!
IF(KSEQMAX>1)THEN
  ISEQ=KSEQMAX
  CALL SEQUENCE_LOOP(ISEQ)
ENDIF
!
!-------------------------------------------------------------------------------
! * Budget calculation
!-------------------------------------------------------------------------------
!
IF(OPRINT)THEN
!
  PDRUN_ALL   = 0.0
  PSSTO_ALL   = 0.0
  PSSTO2_ALL  = 0.0
  PSIN_ALL    = 0.0
  PSOUT_ALL   = 0.0
  PVEL_ALL    = 0.0
  PHS_ALL     = 0.0
  ZAREA       = 0.0
!
  DO JLAT=1,ILAT
     DO JLON=1,ILON
        IF(KSEQ(JLON,JLAT)>0)THEN
           PDRUN_ALL  = PDRUN_ALL  + PRUNOFF(JLON,JLAT)+PGOUT(JLON,JLAT)+PQFR(JLON,JLAT)-PQRF(JLON,JLAT)
           PSSTO_ALL  = PSSTO_ALL  + PSURF_STO (JLON,JLAT) / PAREA(JLON,JLAT)
           PSSTO2_ALL = PSSTO2_ALL + PSURF_STO2(JLON,JLAT) / PAREA(JLON,JLAT)
           PSIN_ALL   = PSIN_ALL   + ZQIN      (JLON,JLAT) / PAREA(JLON,JLAT)
           PSOUT_ALL  = PSOUT_ALL  + PSOUT     (JLON,JLAT) / PAREA(JLON,JLAT)
        ENDIF
        IF(OMASK_VEL(JLON,JLAT))THEN
         PVEL_ALL   = PVEL_ALL   + PVEL (JLON,JLAT) * PAREA(JLON,JLAT)
         PHS_ALL    = PHS_ALL    + PHS  (JLON,JLAT) * PAREA(JLON,JLAT)              
         ZAREA      = ZAREA      + PAREA(JLON,JLAT)         
        ENDIF
     ENDDO
  ENDDO
!
  IF(ZAREA>0.0)THEN
    PVEL_ALL = PVEL_ALL / ZAREA
    PHS_ALL  = PHS_ALL  / ZAREA
  ENDIF
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('TRIP_SURFACE_WATER',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
 CONTAINS
!-------------------------------------------------------------------------------
!
SUBROUTINE SEQUENCE_LOOP(KNUM)
!
INTEGER, INTENT(IN) :: KNUM
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('TRIP_SURFACE_WATER:SEQUENCE_LOOP',0,ZHOOK_HANDLE)
!
DO JLAT=1,ILAT
   DO JLON=1,ILON
!      
      IF(KSEQ(JLON,JLAT)==KNUM)THEN
!
!       ---------------------------------------------------------------------
!       inflow calculation
!
        ZQIN(JLON,JLAT)=ZQIN(JLON,JLAT)+PRUNOFF(JLON,JLAT)+PGOUT(JLON,JLAT)+PQFR(JLON,JLAT)-PQRF(JLON,JLAT)
        PSIN(JLON,JLAT)=ZQIN(JLON,JLAT)
!
!       ------------------------------------------------------------------
!       river channel storage calculation
!
        ZSTOMAX   (JLON,JLAT) = PSURF_STO(JLON,JLAT)+ZQIN(JLON,JLAT)*PTSTEP
!
        PSURF_STO2(JLON,JLAT) = ZSTOMAX(JLON,JLAT)/(1.0+PTSTEP*PVEL(JLON,JLAT)/PLEN(JLON,JLAT))
!
!       -------------------------------------------------------------------
!       supress numerical artifacs
!   
        PSURF_STO2(JLON,JLAT)=MIN(ZSTOMAX(JLON,JLAT),PSURF_STO2(JLON,JLAT))
!
!       ------------------------------------------------------------------
!       river channel outflow calculation and supress numerical artifacs
!
        ZQOUT(JLON,JLAT) = (PSURF_STO(JLON,JLAT)-PSURF_STO2(JLON,JLAT))/PTSTEP+ZQIN(JLON,JLAT)
        PSOUT(JLON,JLAT) = MAX(ZQOUT(JLON,JLAT),0.0)
!             
        PSURF_STO2(JLON,JLAT) = PSURF_STO2(JLON,JLAT) + (PSOUT(JLON,JLAT)-ZQOUT(JLON,JLAT))*PTSTEP        
!
!       ------------------------------------------------------------------
        IF(KGRCN(JLON,JLAT)>=1.AND.KGRCN(JLON,JLAT)<=8)THEN
          INEXTX=KNEXTX(JLON,JLAT)
          INEXTY=KNEXTY(JLON,JLAT)
          ZQIN(INEXTX,INEXTY)=ZQIN(INEXTX,INEXTY)+PSOUT(JLON,JLAT)
        ENDIF
!
      ENDIF
!
   ENDDO
ENDDO
!
IF (LHOOK) CALL DR_HOOK('TRIP_SURFACE_WATER:SEQUENCE_LOOP',1,ZHOOK_HANDLE)
!
END SUBROUTINE SEQUENCE_LOOP
!
!-------------------------------------------------------------------------------
END SUBROUTINE TRIP_SURFACE_WATER
