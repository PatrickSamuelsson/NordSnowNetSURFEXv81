MODULE MODE_SEAICE_SICE_SNOW
USE MODE_SEAICE_SICE
USE YOMHOOK,   ONLY : LHOOK,   DR_HOOK
USE PARKIND1,  ONLY : JPRB
IMPLICIT NONE
PRIVATE

TYPE, PUBLIC :: SNOW_RESPONSE_t
  INTEGER :: RESP_SIZE
  REAL, POINTER, DIMENSION(:) :: &
    HEAT_FLUX, &
    WATER_FLUX
END TYPE SNOW_RESPONSE_t

TYPE, PUBLIC :: SICE_SNOW_t
  INTEGER :: NUM_LAYERS !< Number of snow layers
  INTEGER :: NUM_POINTS
  REAL, POINTER, DIMENSION(:) :: &
    ALBEDO,     & !< Snow albedo
    THRUFAL,    & !< Rate that liquid water leaves snow pack:
    GRND_FLUX,  & !< Soil/snow interface heat flux
    EVAP_COR      !< Evaporation/sublimation correction term
  REAL, POINTER, DIMENSION( :, : ) :: &
    HEAT,       & !< Snow layers heat content
    RHO,        & !< Snow layers averaged density
    SWE,        & !< Snow layers liquid Water Equivalent
    AGE,        & !< Snow grain age
    LIQ_WATER,  & !< Snow layes liquid water content
    T,          & !< Snow layers temperature
    DZ            !< Snow layers thickness
  REAL, POINTER, DIMENSION( : ) :: &
    DZ_TOT        !< Total snow thickness

  TYPE( MODEL_FIELD ), ALLOCATABLE :: MF(:)

  CONTAINS
    PROCEDURE, PASS :: INIT
    PROCEDURE, PASS :: PREP
    PROCEDURE, PASS :: ASSIM
    PROCEDURE, PASS :: RUN
    PROCEDURE, PASS :: DEALLOC

    PROCEDURE, PASS :: EXISTS
    PROCEDURE, PASS :: GET_RESPONSE
    PROCEDURE, PASS :: GET_MODEL_FIELDS

    PROCEDURE, PASS :: ALLOCA

    PROCEDURE, PASS, PRIVATE :: RUN_INTERNAL
    PROCEDURE, PASS, PRIVATE :: POST_RUN
    PROCEDURE, PASS, PRIVATE :: SAFETY_GUARD
END TYPE SICE_SNOW_t

CONTAINS

SUBROUTINE INIT(THIS, HPROGRAM)
IMPLICIT NONE
  CLASS(SICE_SNOW_t) ::THIS
  CHARACTER(LEN=6), INTENT(IN)  :: HPROGRAM

  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('SEAICE_SICE_SNOW:INIT', 0, ZHOOK_HANDLE)
  THIS%NUM_LAYERS = 12
  IF (LHOOK) CALL DR_HOOK('SEAICE_SICE_SNOW:INIT', 1, ZHOOK_HANDLE)
END SUBROUTINE INIT

SUBROUTINE PREP(THIS, KLU, HPROGRAM, HATMFILE, HATMFILETYPE, HPGDFILE, HPGDFILETYPE)
USE MODD_SURF_PAR, ONLY: XUNDEF
USE MODD_SICE_SNOW_PAR, ONLY: XRHOSMIN_ES
IMPLICIT NONE
  CLASS(SICE_SNOW_t) :: THIS
  INTEGER, INTENT(IN) :: KLU
  CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  !< program calling surf. schemes
  CHARACTER(LEN=28),  INTENT(IN)  :: HATMFILE    !< name of the Atmospheric file
  CHARACTER(LEN=6),   INTENT(IN)  :: HATMFILETYPE!< type of the Atmospheric file
  CHARACTER(LEN=28),  INTENT(IN)  :: HPGDFILE    !< name of the PGD file
  CHARACTER(LEN=6),   INTENT(IN)  :: HPGDFILETYPE!< type of the PGD file

  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('SEAICE_SICE_SNOW:PREP', 0, ZHOOK_HANDLE)

  THIS%NUM_POINTS = KLU
  CALL THIS%ALLOCA()

  THIS%ALBEDO    = XUNDEF
  THIS%HEAT      = 0.0
  THIS%RHO       = XRHOSMIN_ES
  THIS%SWE       = 0.0
  THIS%AGE       = 0.0

  THIS%LIQ_WATER = 0.0

  THIS%GRND_FLUX = 0.0
  THIS%THRUFAL   = 0.0
  THIS%EVAP_COR  = 0.0

  THIS%T         = XUNDEF
  THIS%DZ        = 0.0
  THIS%DZ_TOT    = 0.0

  IF (LHOOK) CALL DR_HOOK('SEAICE_SICE_SNOW:PREP', 1, ZHOOK_HANDLE)
END SUBROUTINE PREP

SUBROUTINE ASSIM(THIS)
IMPLICIT NONE
  CLASS(SICE_SNOW_t) :: THIS
END SUBROUTINE ASSIM

SUBROUTINE RUN(THIS, PTSTEP, FORC, PTG, PD_G, PSOILCOND, PALB)
USE MODD_SNOW_PAR, ONLY: XSNOWDMIN, XEMISSN
USE MODD_SICE_SNOW_PAR, ONLY: XRHOSMAX_ES, XRHOSMIN_ES
IMPLICIT NONE
  CLASS(SICE_SNOW_t) :: THIS
  REAL, INTENT(IN) :: PTSTEP
  TYPE(SEA_ICE_FORCING_t), INTENT(IN) :: FORC
  REAL, INTENT(IN) :: PTG(:)
  REAL, INTENT(IN) :: PD_G(:)
  REAL, INTENT(IN) :: PSOILCOND(:)
  REAL, INTENT(IN) :: PALB(:)

  INTEGER :: JJ, JWRK
  INTEGER :: ISIZE_SNOW
  INTEGER :: NMASK(THIS%NUM_POINTS)
  REAL, DIMENSION(FORC%KSIZE) :: &
    ZSNOW, &
    ZSNOWFALL, &
    ZSNOWH, &
    ZSNOWH1, &
    ZSNOWSWE_1D

  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('SEAICE_SICE_SNOW:RUN', 0, ZHOOK_HANDLE)

  DO JJ = 1, FORC%KSIZE
    ZSNOWFALL(JJ) = FORC%PRATE_S(JJ)*PTSTEP/XRHOSMAX_ES    ! maximum possible snowfall depth (m)
  END DO

  ! Calculate preliminary snow depth (m)
  ZSNOW(:)       = 0.
  ZSNOWH(:)      = 0.
  ZSNOWSWE_1D(:) = 0.
  ZSNOWH1(:)     = THIS%HEAT(:,1)*THIS%SWE(:,1)/THIS%RHO(:,1) ! sfc layer only
!
  DO JWRK = 1, THIS%NUM_LAYERS
    DO JJ = 1, FORC%KSIZE
      ZSNOWSWE_1D(JJ) = ZSNOWSWE_1D(JJ) + THIS%SWE(JJ, JWRK)
      ZSNOW(JJ)       = ZSNOW(JJ) + THIS%SWE(JJ, JWRK)/THIS%RHO(JJ, JWRK)
      ZSNOWH(JJ)      = ZSNOWH(JJ) + THIS%HEAT(JJ, JWRK)*THIS%SWE(JJ, JWRK)/THIS%RHO(JJ, JWRK)
    END DO
  END DO

! === Packing: Only call snow model when there is snow on the surface
!              exceeding a minimum threshold OR if the equivalent
!              snow depth falling during the current time step exceeds
!              this limit.
!
! counts the number of points where the computations will be made
!
  ISIZE_SNOW = 0
  NMASK(:) = 0
!
  DO JJ = 1, SIZE(ZSNOW)
    IF (ZSNOW(JJ) >= XSNOWDMIN .OR. ZSNOWFALL(JJ) >= XSNOWDMIN) THEN
      ISIZE_SNOW = ISIZE_SNOW + 1
      NMASK(ISIZE_SNOW) = JJ
    END IF
  END DO
!
  IF (ISIZE_SNOW > 0) THEN
    CALL THIS%RUN_INTERNAL(FORC, PTSTEP, PTG, PD_G, PSOILCOND, PALB, ISIZE_SNOW, NMASK)
  END IF

  CALL THIS%POST_RUN(FORC, PTSTEP, ZSNOWH, ZSNOWSWE_1D)
  CALL THIS%SAFETY_GUARD()

  THIS%DZ_TOT(:) = SUM(THIS%DZ(:, :), DIM=2)

  IF (LHOOK) CALL DR_HOOK('SEAICE_SICE_SNOW:RUN', 1, ZHOOK_HANDLE)
END SUBROUTINE RUN

SUBROUTINE RUN_INTERNAL(THIS, FORC, PTSTEP, PTG, PD_G, PSOILCOND, PALB, KSIZE_SNOW, KMASK)
USE MODD_TYPE_DATE_SURF, ONLY: DATE_TIME
USE MODD_CSTS, ONLY: XLVTT, XLSTT
USE MODD_SURF_PAR, ONLY: XUNDEF
USE MODD_SNOW_PAR, ONLY: XSNOWDMIN, XEMISSN, XZ0SN, XZ0HSN
USE MODD_SICE_SNOW_PAR, ONLY: XRHOSMAX_ES, XRHOSMIN_ES
USE MODI_SEAICE_SICE_SNOW3L, SNOW3L => SEAICE_SICE_SNOW3L
IMPLICIT NONE
  CLASS(SICE_SNOW_t) :: THIS
  TYPE(SEA_ICE_FORCING_t), INTENT(IN) :: FORC
  REAL, INTENT(IN) :: PTSTEP
  REAL, INTENT(IN) :: PTG(:)
  REAL, INTENT(IN) :: PD_G(:)
  REAL, INTENT(IN) :: PSOILCOND(:)
  REAL, INTENT(IN) :: PALB(:)
  INTEGER, INTENT(IN) :: KSIZE_SNOW
  INTEGER, INTENT(IN) :: KMASK(KSIZE_SNOW)

  REAL, DIMENSION(KSIZE_SNOW) :: &
    ZP_ALB,         &
    ZP_CDSNOW,      &
    ZP_CHSNOW,      &
    ZP_D_G,         &
    ZP_DELHEATN,    &
    ZP_DELHEATN_SFC,&
    ZP_DIRCOSZW,    &
    ZP_EMISNOW,     &
    ZP_EVAP,        &
    ZP_EVAPCOR,     &
    ZP_EXNA,        &
    ZP_EXNS,        &
    ZP_FOREST,      & ! Fraction of forest
    ZP_GFLUXSNOW,   &
    ZP_GFLXCOR,     &
    ZP_GRNDFLUX,    &
    ZP_GSFCSNOW,    &
    ZP_HPSNOW,      &
    ZP_HSNOW,       &
    ZP_LAT,         &
    ZP_LEL3L,       &
    ZP_LES3L,       &
    ZP_LON,         &
    ZP_LSTT,        &
    ZP_LVTT,        &
    ZP_LW_RAD,      &
    ZP_LWNETSNOW,   &
    ZP_PEQ_A_COEF,  &
    ZP_PEQ_B_COEF,  &
    ZP_PET_A_COEF,  &
    ZP_PET_B_COEF,  &
    ZP_PEW_A_COEF,  &
    ZP_PEW_B_COEF,  &
    ZP_PS,          &
    ZP_PSN3L,       &
    ZP_QA,          &
    ZP_QS,          &
    ZP_RHOA,        &
    ZP_RI,          &
    ZP_RNSNOW,      &
    ZP_RRSNOW,      &
    ZP_SNDRIFT,     &
    ZP_SNOWALB,     &
    ZP_SNOWHMASS,   &
    ZP_SNOWSFCH,    &
    ZP_SOILCOND,    &
    ZP_SOILCOR,     &
    ZP_SRSNOW,      &
    ZP_SW_RAD,      &
    ZP_SWNETSNOW,   &
    ZP_SWNETSNOWS,  &
    ZP_TA,          &
    ZP_TG,          &
    ZP_THRUFAL,     &
    ZP_UREF,        &
    ZP_USTARSNOW,   &
    ZP_VEGTYPE,     & ! Fraction of permanent snow
    ZP_VMOD,        &
    ZP_Z0EFF,       &
    ZP_Z0HNAT,      &
    ZP_Z0NAT,       &
    ZP_ZENITH,      &
    ZP_ZREF


  REAL, DIMENSION(KSIZE_SNOW, THIS%NUM_LAYERS) :: &
    ZP_SNOWAGE,     &
    ZP_SNOWDZ,      &
    ZP_SNOWGRAN1,   &
    ZP_SNOWGRAN2,   &
    ZP_SNOWHEAT,    &
    ZP_SNOWHIST,    &
    ZP_SNOWLIQ,     &
    ZP_SNOWRHO,     &
    ZP_SNOWSWE,     &
    ZP_SNOWTEMP

  TYPE(DATE_TIME) :: TPTIME

  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('SEAICE_SICE_SNOW:RUN_INTERNAL', 0, ZHOOK_HANDLE)

  ! Unused or irrelevant for 'snow-over-sea ice' parameters
  ZP_FOREST = 0.
  ZP_VEGTYPE = 0.
  ZP_PSN3L = 1.
  ZP_DIRCOSZW = 1.

  ZP_SNOWGRAN1(:, :) = XUNDEF
  ZP_SNOWGRAN2(:, :) = XUNDEF
  ZP_SNOWHIST (:, :) = XUNDEF

  ZP_LAT         (:) = XUNDEF
  ZP_LON         (:) = XUNDEF
  ZP_SWNETSNOW   (:) = XUNDEF
  ZP_SWNETSNOWS  (:) = XUNDEF
  ZP_LWNETSNOW   (:) = XUNDEF

  ZP_EMISNOW (:) = XEMISSN
  ZP_LVTT    (:) = XLVTT
  ZP_LSTT    (:) = XLSTT
  ZP_Z0NAT   (:) = XZ0SN
  ZP_Z0HNAT  (:) = XZ0HSN
  ZP_Z0EFF   (:) = XZ0SN


  ! Pack atmospheric forcing
  ZP_EXNA      (:) = FORC%EXNA       ( KMASK(:) )
  ZP_EXNS      (:) = FORC%EXNS       ( KMASK(:) )
  ZP_LW_RAD    (:) = FORC%LW         ( KMASK(:) )
  ZP_PEQ_A_COEF(:) = FORC%PPEQ_A_COEF( KMASK(:) )
  ZP_PEQ_B_COEF(:) = FORC%PPEQ_B_COEF( KMASK(:) )
  ZP_PET_A_COEF(:) = FORC%PPET_A_COEF( KMASK(:) )
  ZP_PET_B_COEF(:) = FORC%PPET_B_COEF( KMASK(:) )
  ZP_PEW_A_COEF(:) = FORC%PPEW_A_COEF( KMASK(:) )
  ZP_PEW_B_COEF(:) = FORC%PPEW_B_COEF( KMASK(:) )
  ZP_PS        (:) = FORC%PSURF      ( KMASK(:) )
  ZP_QA        (:) = FORC%QA         ( KMASK(:) )
  ZP_RHOA      (:) = FORC%RHOA       ( KMASK(:) )
  ZP_RRSNOW    (:) = FORC%PRATE_R    ( KMASK(:) )
  ZP_SRSNOW    (:) = FORC%PRATE_S    ( KMASK(:) )
  ZP_SW_RAD    (:) = FORC%SW         ( KMASK(:) )
  ZP_TA        (:) = FORC%TA         ( KMASK(:) )
  ZP_UREF      (:) = FORC%UREF       ( KMASK(:) )
  ZP_VMOD      (:) = FORC%WIND       ( KMASK(:) )
  ZP_ZENITH    (:) = FORC%ZENITH     ( KMASK(:) )
  ZP_ZREF      (:) = FORC%ZREF       ( KMASK(:) )

  ! Pack ice parameters
  ZP_TG        (:) = PTG             ( KMASK(:) )
  ZP_D_G       (:) = PD_G            ( KMASK(:) )
  ZP_SOILCOND  (:) = PSOILCOND       ( KMASK(:) )
  ZP_ALB       (:) = PALB            ( KMASK(:) )

  ! Pack snow variables
  ZP_SNOWALB (:)    = THIS%ALBEDO    (KMASK(:)   )
  ZP_GRNDFLUX(:)    = THIS%GRND_FLUX (KMASK(:)   )

  ZP_SNOWSWE (:, :) = THIS%SWE       (KMASK(:), :)
  ZP_SNOWRHO (:, :) = THIS%RHO       (KMASK(:), :)
  ZP_SNOWHEAT(:, :) = THIS%HEAT      (KMASK(:), :)
  ZP_SNOWAGE (:, :) = THIS%AGE       (KMASK(:), :)

  ZP_SNOWTEMP(:, :) = THIS%T         (KMASK(:), :)
  ZP_SNOWLIQ (:, :) = THIS%LIQ_WATER (KMASK(:), :)
  ZP_SNOWDZ  (:, :) = THIS%DZ        (KMASK(:), :)

  ! Output variables
  ZP_DELHEATN(:) = 0.
  ZP_DELHEATN_SFC(:) = 0.
  ZP_SNOWSFCH(:) = 0.
  ZP_RNSNOW(:) = 0.
  ZP_HSNOW(:) = 0.
  ZP_HPSNOW(:) = 0.
  ZP_LES3L(:) = 0.
  ZP_LEL3L(:) = 0.
  ZP_EVAP(:) = 0.

  ! conversion of snow heat from J/m3 into J/m2
  WHERE(ZP_SNOWSWE(:,:) > 0.)
    ZP_SNOWHEAT(:,:) = ZP_SNOWHEAT(:,:)/ZP_SNOWRHO (:,:)*ZP_SNOWSWE (:,:)
  END WHERE

  ! Call ISBA-SNOW3L model:
  !
  CALL SNOW3L('DEF', TPTIME, 'OLD', &
              ZP_PEW_A_COEF, ZP_PEW_B_COEF,                                 &
              ZP_PET_A_COEF, ZP_PEQ_A_COEF,ZP_PET_B_COEF, ZP_PEQ_B_COEF,    &
              ZP_SNOWSWE, ZP_SNOWRHO, ZP_SNOWHEAT, ZP_SNOWALB,              &
              ZP_SNOWGRAN1, ZP_SNOWGRAN2, ZP_SNOWHIST, ZP_SNOWAGE, PTSTEP,  &
              ZP_PS, ZP_SRSNOW, ZP_RRSNOW, ZP_PSN3L, ZP_TA, ZP_TG,          &
              ZP_SW_RAD, ZP_QA, ZP_VMOD, ZP_LW_RAD, ZP_RHOA, ZP_UREF,       &
              ZP_EXNS, ZP_EXNA, ZP_DIRCOSZW, ZP_ZREF, ZP_Z0NAT, ZP_Z0EFF,   &
              ZP_Z0HNAT, ZP_ALB, ZP_SOILCOND, ZP_D_G,                       &
              ZP_LVTT, ZP_LSTT, ZP_SNOWLIQ,                                 &
              ZP_SNOWTEMP, ZP_SNOWDZ, ZP_THRUFAL, ZP_GRNDFLUX ,             &
              ZP_EVAPCOR, ZP_SOILCOR, ZP_GFLXCOR, ZP_SNOWSFCH,              &
              ZP_DELHEATN, ZP_DELHEATN_SFC,                                 &
              ZP_SWNETSNOW, ZP_SWNETSNOWS, ZP_LWNETSNOW, ZP_GSFCSNOW,       &
              ZP_RNSNOW, ZP_HSNOW, ZP_GFLUXSNOW, ZP_HPSNOW, ZP_LES3L,       &
              ZP_LEL3L, ZP_EVAP, ZP_SNDRIFT, ZP_RI,                         &
              ZP_EMISNOW, ZP_CDSNOW, ZP_USTARSNOW,                          &
              ZP_CHSNOW, ZP_SNOWHMASS, ZP_QS, ZP_VEGTYPE,  ZP_FOREST,       &
              ZP_ZENITH, ZP_LAT, ZP_LON, OSNOWDRIFT = .FALSE., OSNOWDRIFT_SUBLIM = .FALSE.  )

  !conversion of snow heat from J/m2 into J/m3
  WHERE(ZP_SNOWSWE (:,:) > 0.)
    ZP_SNOWHEAT(:,:) = ZP_SNOWHEAT(:,:)*ZP_SNOWRHO(:,:)/ZP_SNOWSWE(:,:)
  ENDWHERE

  WHERE(ZP_SNOWALB(:) < 0.75)
    ZP_SNOWALB(:) = 0.75
  ENDWHERE
  ! Clear the current state
  CALL PRUNE(THIS%MF)

  ! Unpack snow variables
  THIS%ALBEDO   (KMASK(:)   ) = ZP_SNOWALB  (:)
  THIS%GRND_FLUX(KMASK(:)   ) = ZP_GRNDFLUX (:)
  THIS%THRUFAL  (KMASK(:)   ) = ZP_THRUFAL  (:)

  THIS%SWE      (KMASK(:), :) = ZP_SNOWSWE (:, :)
  THIS%RHO      (KMASK(:), :) = ZP_SNOWRHO (:, :)
  THIS%HEAT     (KMASK(:), :) = ZP_SNOWHEAT(:, :)
  THIS%AGE      (KMASK(:), :) = ZP_SNOWAGE (:, :)

  THIS%T        (KMASK(:), :) = ZP_SNOWTEMP(:, :)
  THIS%LIQ_WATER(KMASK(:), :) = ZP_SNOWLIQ (:, :)
  THIS%DZ       (KMASK(:), :) = ZP_SNOWDZ  (:, :)

  IF (LHOOK) CALL DR_HOOK('SEAICE_SICE_SNOW:RUN_INTERNAL', 1, ZHOOK_HANDLE)
END SUBROUTINE RUN_INTERNAL

SUBROUTINE POST_RUN(THIS, FORC, PTSTEP, PSNOWH, PSNOWSWE_1D)
USE MODD_SNOW_PAR, ONLY: XSNOWDMIN, XEMISSN
USE MODD_SICE_SNOW_PAR, ONLY: XRHOSMAX_ES, XRHOSMIN_ES
USE MODD_CSTS, ONLY: XTT, XLMTT, XLSTT
IMPLICIT NONE
  CLASS(SICE_SNOW_t) :: THIS
  TYPE(SEA_ICE_FORCING_t), INTENT(IN) :: FORC
  REAL, INTENT(IN) :: PTSTEP
  REAL, INTENT(IN) :: PSNOWH(FORC%KSIZE)
  REAL, INTENT(IN) :: PSNOWSWE_1D(FORC%KSIZE)

  INTEGER :: JJ, JWRK
  INTEGER :: M
  REAL, DIMENSION(FORC%KSIZE) :: &
    ZSNOWD, &
    ZSNOWABLAT_DELTA, &
    PEVAP
  LOGICAL :: LREMOVE_SNOW(FORC%KSIZE)

  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('SEAICE_SICE_SNOW:POST_RUN', 0, ZHOOK_HANDLE)

  M = FORC%KSIZE
  ZSNOWD(:) = 0.
  DO JWRK = 1, THIS%NUM_LAYERS
    DO JJ = 1, FORC%KSIZE
      ZSNOWD(JJ) = ZSNOWD(JJ) + THIS%SWE(JJ,JWRK)/THIS%RHO(JJ,JWRK)
    END DO
  END DO

  LREMOVE_SNOW(:) = ZSNOWD(:) < XSNOWDMIN*1.1

  ZSNOWABLAT_DELTA(:) = 0.0
  !ZTHRUFAL        (:) = PTHRUFAL(:)
  !
  PEVAP(:) = 0.
  WHERE(LREMOVE_SNOW(:))
    !PLES3L(:)           = MIN(PLES3L(:), XLSTT*(PSNOWSWE_1D(:)/PTSTEP + FORC%PRATE_S(:)))
    !PLEL3L(:)           = 0.0
    !PEVAP(:)            = PLES3L(:)/XLSTT
    !
    ZSNOWABLAT_DELTA(:) = 1.0
    !PFLSN_COR(:)        = 0.0
    !

    !
    !PEVAPCOR(:)         = 0.0
    !ZSOILCOR(:)         = 0.0
    !
    THIS%GRND_FLUX(:M)   = (PSNOWH(:) - FORC%PRATE_S(:)*(XLMTT*PTSTEP))/PTSTEP + THIS%GRND_FLUX(:M)
    THIS%ALBEDO(:M)      = XUNDEF
    THIS%THRUFAL(:M)     = MAX(0.0, PSNOWSWE_1D(:)/PTSTEP + FORC%PRATE_S(:) - PEVAP(:) + FORC%PRATE_R(:)) ! kg m-2 s-1
    !
  END WHERE

  DO JWRK = 1, THIS%NUM_LAYERS
    DO JJ = 1, FORC%KSIZE
      THIS%SWE      (JJ,JWRK) = (1.0-ZSNOWABLAT_DELTA(JJ))*THIS%SWE      (JJ,JWRK)
      THIS%HEAT     (JJ,JWRK) = (1.0-ZSNOWABLAT_DELTA(JJ))*THIS%HEAT     (JJ,JWRK)
      THIS%RHO      (JJ,JWRK) = (1.0-ZSNOWABLAT_DELTA(JJ))*THIS%RHO      (JJ,JWRK) + &
                                     ZSNOWABLAT_DELTA(JJ) *XRHOSMIN_ES
      THIS%AGE      (JJ,JWRK) = (1.0-ZSNOWABLAT_DELTA(JJ))*THIS%AGE      (JJ,JWRK)
      THIS%T        (JJ,JWRK) = (1.0-ZSNOWABLAT_DELTA(JJ))*THIS%T        (JJ,JWRK) + &
                                     ZSNOWABLAT_DELTA(JJ) *XTT
      THIS%LIQ_WATER(JJ,JWRK) = (1.0-ZSNOWABLAT_DELTA(JJ))*THIS%LIQ_WATER(JJ,JWRK)
      THIS%DZ       (JJ,JWRK) = (1.0-ZSNOWABLAT_DELTA(JJ))*THIS%DZ       (JJ,JWRK)
    ENDDO
  ENDDO

  IF (LHOOK) CALL DR_HOOK('SEAICE_SICE_SNOW:POST_RUN', 1, ZHOOK_HANDLE)
END SUBROUTINE POST_RUN

SUBROUTINE SAFETY_GUARD(THIS)
USE MODI_ABOR1_SFX
IMPLICIT NONE
  CLASS(SICE_SNOW_t), INTENT(IN) :: THIS

  REAL, PARAMETER :: ZCHECK_TEMP = 50.0
  INTEGER :: JWRK, JJ
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('SEAICE_SICE_SNOW:SAFETY_GUARD', 0, ZHOOK_HANDLE)

  DO JWRK = 1, THIS%NUM_LAYERS
    DO JJ = 1, THIS%NUM_POINTS
      IF (THIS%SWE(JJ, JWRK) > 0.0) THEN
        IF (THIS%T(JJ,JWRK) < ZCHECK_TEMP) THEN
          WRITE(*,*) 'Suspicious low temperature :', THIS%T(JJ,JWRK)
          CALL ABOR1_SFX('SICE-SNOW: Suspicious low temperature')
        END IF
      END IF
    END DO
  END DO

  IF (LHOOK) CALL DR_HOOK('SEAICE_SICE_SNOW:SAFETY_GUARD', 1, ZHOOK_HANDLE)
END SUBROUTINE SAFETY_GUARD

FUNCTION EXISTS(THIS) RESULT (RES)
USE MODD_SNOW_PAR, ONLY: XSNOWDMIN
IMPLICIT NONE
  CLASS(SICE_SNOW_t), INTENT(IN) :: THIS
  LOGICAL :: RES(THIS%NUM_POINTS)

  INTEGER :: JJ, JWRK
  REAL :: ZSNOWD(THIS%NUM_POINTS)
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('SEAICE_SICE_SNOW:EXISTS', 0, ZHOOK_HANDLE)

  ZSNOWD(:) = 0.
  DO JWRK = 1, THIS%NUM_LAYERS
    DO JJ = 1, THIS%NUM_POINTS
      ZSNOWD(JJ) = ZSNOWD(JJ) + THIS%SWE(JJ,JWRK)/THIS%RHO(JJ,JWRK)
    END DO
  END DO

  RES(:) = ZSNOWD(:) > XSNOWDMIN

  IF (LHOOK) CALL DR_HOOK('SEAICE_SICE_SNOW:EXISTS', 1, ZHOOK_HANDLE)
END FUNCTION EXISTS

SUBROUTINE DEALLOC(THIS)
IMPLICIT NONE
  CLASS(SICE_SNOW_t) :: THIS
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('SEAICE_SICE_SNOW:DEALLOC', 0, ZHOOK_HANDLE)

  IF(ASSOCIATED(THIS%ALBEDO))    DEALLOCATE(THIS%ALBEDO)
  IF(ASSOCIATED(THIS%THRUFAL))   DEALLOCATE(THIS%THRUFAL)
  IF(ASSOCIATED(THIS%GRND_FLUX)) DEALLOCATE(THIS%GRND_FLUX)
  IF(ASSOCIATED(THIS%EVAP_COR))  DEALLOCATE(THIS%EVAP_COR)
  IF(ASSOCIATED(THIS%DZ_TOT))    DEALLOCATE(THIS%DZ_TOT)

  IF(ASSOCIATED(THIS%HEAT))      DEALLOCATE(THIS%HEAT)
  IF(ASSOCIATED(THIS%RHO))       DEALLOCATE(THIS%RHO)
  IF(ASSOCIATED(THIS%SWE))       DEALLOCATE(THIS%SWE)
  IF(ASSOCIATED(THIS%AGE))       DEALLOCATE(THIS%AGE)
  IF(ASSOCIATED(THIS%LIQ_WATER)) DEALLOCATE(THIS%LIQ_WATER)
  IF(ASSOCIATED(THIS%T))         DEALLOCATE(THIS%T)
  IF(ASSOCIATED(THIS%DZ))        DEALLOCATE(THIS%DZ)

  IF(ALLOCATED(THIS%MF))         DEALLOCATE(THIS%MF)

  IF (LHOOK) CALL DR_HOOK('SEAICE_SICE_SNOW:DEALLOC', 1, ZHOOK_HANDLE)
END SUBROUTINE DEALLOC

SUBROUTINE GET_RESPONSE(THIS, M, RESPONSE, PTSUR, PALB, PDEPTH)
IMPLICIT NONE
  CLASS(SICE_SNOW_t) :: THIS
  INTEGER, INTENT(IN) :: M
  TYPE(SNOW_RESPONSE_t), INTENT(OUT), OPTIONAL :: RESPONSE
  REAL, INTENT(OUT), OPTIONAL :: PTSUR(M)
  REAL, INTENT(OUT), OPTIONAL :: PALB(M)
  REAL, INTENT(OUT), OPTIONAL :: PDEPTH(M) !< Total snow thickness
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('SEAICE_SICE_SNOW:GET_RESPONSE', 0, ZHOOK_HANDLE)

  IF(PRESENT(RESPONSE)) THEN
    RESPONSE%RESP_SIZE = M
    RESPONSE%HEAT_FLUX => THIS%GRND_FLUX(:M)
    RESPONSE%WATER_FLUX => THIS%THRUFAL(:M)
  END IF

  IF(PRESENT(PTSUR)) THEN
    PTSUR(:) = THIS%T(:M, 1)
  END IF

  IF(PRESENT(PALB)) THEN
    PALB(:) = THIS%ALBEDO(:M)
  END IF

  IF(PRESENT(PDEPTH)) THEN
    PDEPTH(:) = SUM(THIS%DZ(:M, :), DIM=2)
  END IF

  IF (LHOOK) CALL DR_HOOK('SEAICE_SICE_SNOW:GET_RESPONSE', 1, ZHOOK_HANDLE)
END SUBROUTINE GET_RESPONSE

SUBROUTINE ALLOCA(THIS)
IMPLICIT NONE
  CLASS(SICE_SNOW_t) :: THIS
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('SEAICE_SICE_SNOW:ALLOCA', 0, ZHOOK_HANDLE)

  ALLOCATE( &
    THIS%ALBEDO   (THIS%NUM_POINTS), &
    THIS%THRUFAL  (THIS%NUM_POINTS), &
    THIS%GRND_FLUX(THIS%NUM_POINTS), &
    THIS%EVAP_COR (THIS%NUM_POINTS), &
    THIS%DZ_TOT   (THIS%NUM_POINTS))

  ALLOCATE( &
    THIS%HEAT     (THIS%NUM_POINTS, THIS%NUM_LAYERS), &
    THIS%RHO      (THIS%NUM_POINTS, THIS%NUM_LAYERS), &
    THIS%SWE      (THIS%NUM_POINTS, THIS%NUM_LAYERS), &
    THIS%AGE      (THIS%NUM_POINTS, THIS%NUM_LAYERS), &
    THIS%LIQ_WATER(THIS%NUM_POINTS, THIS%NUM_LAYERS), &
    THIS%T        (THIS%NUM_POINTS, THIS%NUM_LAYERS), &
    THIS%DZ       (THIS%NUM_POINTS, THIS%NUM_LAYERS)  )

  CALL THIS%GET_MODEL_FIELDS(THIS%MF)

  IF (LHOOK) CALL DR_HOOK('SEAICE_SICE_SNOW:ALLOCA', 1, ZHOOK_HANDLE)
END SUBROUTINE ALLOCA

SUBROUTINE GET_MODEL_FIELDS( THIS, MF )
USE MODD_SICE_SNOW_PAR, ONLY: XRHOSMIN_ES
IMPLICIT NONE
  CLASS(SICE_SNOW_t), INTENT(IN) :: THIS
  TYPE( MODEL_FIELD ), ALLOCATABLE, INTENT( OUT ) :: MF(:)

  INTEGER, PARAMETER :: NUM_FIELDS = 12

  REAL( KIND = JPRB ) :: ZHOOK_HANDLE
  IF( LHOOK ) CALL DR_HOOK( 'SEAICE_SICE_SNOW:GET_MODEL_FIELDS', 0, ZHOOK_HANDLE )

  ALLOCATE( MF(NUM_FIELDS) )

  MF(:) = [MODEL_FIELD(                      &
      'WSN_ICE',                             &
      'Snow water equivalent',               &
      'Kg/m2',                               &
      [THIS%NUM_POINTS, THIS%NUM_LAYERS],    &
      P2 = THIS%SWE,                         &
      XDEFAULT = 0.                          &
    ),                                       &
    MODEL_FIELD(                             &
      'RSN_ICE',                             &
      'Snow density',                        &
      'Kg/m3',                               &
      [THIS%NUM_POINTS, THIS%NUM_LAYERS],    &
      P2 = THIS%RHO,                         &
      XDEFAULT = XRHOSMIN_ES                 &
    ),                                       &
    MODEL_FIELD(                             &
      'HSN_ICE',                             &
      'Snow heat content',                   &
      'Kg/m3',                               &
      [THIS%NUM_POINTS, THIS%NUM_LAYERS],    &
      P2 = THIS%HEAT,                        &
      XDEFAULT = 0.                          &
    ),                                       &
    MODEL_FIELD(                             &
      'GSN_ICE',                             &
      'Snow age',                            &
      's',                                   &
      [THIS%NUM_POINTS, THIS%NUM_LAYERS],    &
      P2 = THIS%AGE                          &
    ),                                       &
    MODEL_FIELD(                             &
      'ASN_ICE',                             &
      'Snow albedo',                         &
      'dimensionless',                       &
      [THIS%NUM_POINTS, 0],                  &
      P1 = THIS%ALBEDO                       &
    ),                                       &
    MODEL_FIELD(                             &
      'TSN_ICE',                             &
      'Snow temperature',                    &
      'K',                                   &
      [THIS%NUM_POINTS, THIS%NUM_LAYERS],    &
      P2 = THIS%T                            &
    ),                                       &
    MODEL_FIELD(                             &
      'DSN_ICE',                             &
      'Snow thickness',                      &
      'm',                                   &
      [THIS%NUM_POINTS, THIS%NUM_LAYERS],    &
      .TRUE.,                                &
      P2 = THIS%DZ,                          &
      XDEFAULT = 0.                          &
    ),                                       &
    MODEL_FIELD(                             &
      'LWSN_ICE',                            &
      'Snow liquid water',                   &
      'm',                                   &
      [THIS%NUM_POINTS, THIS%NUM_LAYERS],    &
      .TRUE.,                                &
      P2 = THIS%LIQ_WATER,                   &
      XDEFAULT = 0.                          &
    ),                                       &
    MODEL_FIELD(                             &
      'DSN_T_ICE',                           &
      'Total snow thickness',                &
      'm',                                   &
      [THIS%NUM_POINTS, 0],                  &
      .TRUE.,                                &
      P1 = THIS%DZ_TOT,                      &
      XDEFAULT = 0.                          &
    ),                                       &
    MODEL_FIELD( NCONFIG=[0,0], P1 = THIS%THRUFAL,   LINTERNAL = .TRUE., XDEFAULT = 0. ), &
    MODEL_FIELD( NCONFIG=[0,0], P1 = THIS%GRND_FLUX, LINTERNAL = .TRUE., XDEFAULT = 0. ), &
    MODEL_FIELD( NCONFIG=[0,0], P1 = THIS%EVAP_COR,  LINTERNAL = .TRUE., XDEFAULT = 0. )  &
  ]
  IF( LHOOK ) CALL DR_HOOK( 'SEAICE_SICE_SNOW:GET_MODEL_FIELDS', 1, ZHOOK_HANDLE )
END SUBROUTINE GET_MODEL_FIELDS
END MODULE MODE_SEAICE_SICE_SNOW
