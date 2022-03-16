SUBROUTINE TRIP_RUN (TPDG, TP, TPG, &
                     OOASIS, OXIOS,                   &
                    KLISTING,KLON,KLAT,KNB_TSTEP_RUN, &
                    PRUNTIME,KLON_OL,KLAT_OL,KNB_OL,  &
                    KYEAR,KMONTH,KDAY,PTIME           )
!#############################################
!
!!****  *TRIP_RUN*  
!!
!!    PURPOSE
!!    -------
!!   
!!    Run trip
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
!!      Original    06/08 
!!      B. Decharme 10/2016  bug surface/groundwater coupling   
!!      S.Sénési    08/11/16 : interface to XIOS
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TRIP_DIAG, ONLY : TRIP_DIAG_t
USE MODD_TRIP,      ONLY : TRIP_t
USE MODD_TRIP_GRID, ONLY : TRIP_GRID_t
!
USE MODD_TRIP_LISTING
!
USE MODN_TRIP_RUN, ONLY : LRESTART, LPRINT,   &
                          XTSTEP_RUN, XTSTEP_DIAG
!
USE MODD_TRIP_PAR, ONLY : XUNDEF, NUNDEF, XDAY
!
USE MODI_TRIP_FORCING
USE MODI_TRIP_INTERFACE
USE MODI_TRIP_DATE
!
USE MODI_TRIP_OASIS_RECV
USE MODI_TRIP_OASIS_SEND
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!-------------------------------------------------------------------------------
!
!*      0.1    declarations of arguments
!
!
TYPE(TRIP_DIAG_t), INTENT(INOUT) :: TPDG
TYPE(TRIP_t),      INTENT(INOUT) :: TP
TYPE(TRIP_GRID_t), INTENT(INOUT) :: TPG
!
LOGICAL, INTENT(IN)  :: OOASIS        ! Oasis coupling or not
LOGICAL, INTENT(IN)  :: OXIOS         ! Do we use XIOS
!
INTEGER, INTENT(IN)  :: KLISTING      ! Listing ID
INTEGER, INTENT(IN)  :: KLON          ! Number of longitude
INTEGER, INTENT(IN)  :: KLAT          ! Number of latittude
INTEGER, INTENT(IN)  :: KNB_TSTEP_RUN ! number of time step in the run
REAL,    INTENT(IN)  :: PRUNTIME      ! total simulated time
!
INTEGER, INTENT(IN)  :: KLON_OL       ! Number of longitude if forcing offline
INTEGER, INTENT(IN)  :: KLAT_OL       ! Number of latittude if forcing offline
INTEGER, INTENT(IN)  :: KNB_OL        ! number of time step if forcing offline
!
INTEGER, INTENT(OUT) :: KYEAR         ! current year         (UTC)
INTEGER, INTENT(OUT) :: KMONTH        ! current month        (UTC)
INTEGER, INTENT(OUT) :: KDAY          ! current day          (UTC)
REAL,    INTENT(OUT) :: PTIME         ! current time           (s)
!
!-------------------------------------------------------------------------------
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(KLON_OL,KLAT_OL,KNB_OL) :: ZDRAIN_OL         ! Drainage from the forcing file          (kg)
REAL, DIMENSION(KLON_OL,KLAT_OL,KNB_OL) :: ZRUNOFF_OL        ! Surface runoff from the forcing file    (kg)
REAL, DIMENSION(KLON_OL,KLAT_OL,KNB_OL) :: ZSRC_FLOOD_OL     ! Flood source term from the forcing file (kg)
!
REAL, DIMENSION(KLON,KLAT) :: ZRUNOFF           ! Surface runoff               (kg/s)
REAL, DIMENSION(KLON,KLAT) :: ZDRAIN            ! Drainage                     (kg/s)
REAL, DIMENSION(KLON,KLAT) :: ZCALVING          ! Calving flux                 (kg/s)
REAL, DIMENSION(KLON,KLAT) :: ZSRC_FLOOD        ! Input P-E-I flood source term(kg/s)
!
REAL                       :: ZTIMEC            ! cumulated current time (s)
REAL                       :: ZTIME_CPL         ! Coupling time
INTEGER                    :: JNB_TSTEP_RUN     ! TSTEP_RUN counter 
INTEGER                    :: JNB_TSTEP_DIAG    ! DIAG call counter 
INTEGER                    :: ICOUNT
 CHARACTER(LEN=3)          :: YWORK
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! --------------------------------------------------------------------------------------
! * 1. Initialize
! --------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_RUN',0,ZHOOK_HANDLE)
!
ZRUNOFF   (:,:) = XUNDEF
ZDRAIN    (:,:) = XUNDEF
ZCALVING  (:,:) = XUNDEF
ZSRC_FLOOD(:,:) = XUNDEF
!
! --------------------------------------------------------------------------------------
! * 2. Read and prepare drainage and runoff if offline
! --------------------------------------------------------------------------------------
!
IF(.NOT.OOASIS)THEN
  ZDRAIN_OL    (:,:,:) = XUNDEF
  ZRUNOFF_OL   (:,:,:) = XUNDEF
  ZSRC_FLOOD_OL(:,:,:) = XUNDEF
  CALL TRIP_FORCING(TPG, &
                    KLISTING,KLON,KLAT,KNB_TSTEP_RUN, &
                    ZDRAIN_OL,ZRUNOFF_OL,ZSRC_FLOOD_OL)
ENDIF
!
! --------------------------------------------------------------------------------------
! * 3. Temporal loops
! --------------------------------------------------------------------------------------
!
ZTIMEC         = 0
ICOUNT         = 0
JNB_TSTEP_DIAG = 0
!
DO JNB_TSTEP_RUN = 1, KNB_TSTEP_RUN
!
! * TRIP INPUT FLUXES (kg/s)
!
   IF(OOASIS)THEN           
     CALL TRIP_OASIS_RECV(TP, TPG, &
                          KLISTING,KLON,KLAT,ZTIMEC,ZRUNOFF,  &
                          ZDRAIN,ZCALVING,ZSRC_FLOOD          )
   ELSE
     ZDRAIN    (:,:) = ZDRAIN_OL    (:,:,JNB_TSTEP_RUN) / XTSTEP_RUN
     ZRUNOFF   (:,:) = ZRUNOFF_OL   (:,:,JNB_TSTEP_RUN) / XTSTEP_RUN
     ZSRC_FLOOD(:,:) = ZSRC_FLOOD_OL(:,:,JNB_TSTEP_RUN) / XTSTEP_RUN
     ZCALVING  (:,:) = 0.0
   ENDIF
!
! * TRIP PHYSIC CALL
!
   CALL TRIP_INTERFACE(TPDG, TP, TPG, &
                       KLISTING,KLON,KLAT,PTIME,ZTIMEC,    &
                       LPRINT,JNB_TSTEP_RUN,JNB_TSTEP_DIAG,&
                       XTSTEP_RUN,XTSTEP_DIAG,ZRUNOFF,     &
                       ZDRAIN,ZCALVING,ZSRC_FLOOD,OXIOS    )
!
! * TRIP OUTPUT FLUXES
!
   IF(OOASIS)THEN
     ZTIME_CPL=ZTIMEC-XTSTEP_RUN
     CALL TRIP_OASIS_SEND(TP, TPG, &
                          KLISTING,KLON,KLAT,ZTIME_CPL)
   ENDIF
!                   
   IF (LPRINT.AND.MOD(ZTIMEC,XDAY)==0.0) THEN
      ICOUNT = ICOUNT +1
      WRITE(*,'(A10,I5,A2,I5)')'TRIP DAY :',ICOUNT,' /',INT(PRUNTIME/XDAY)
   ENDIF
!
! * TRIP DATE INCREMENT
!
   CALL TRIP_DATE(KYEAR,KMONTH,KDAY,PTIME)
!
ENDDO
!
! --------------------------------------------------------------------------------------
! * 4. End TRIP run
! --------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_RUN',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE TRIP_RUN
