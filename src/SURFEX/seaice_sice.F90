MODULE ICE_SICE
USE ABSTRACT_ICE
USE MODE_SEAICE_SICE
USE MODE_SEAICE_SICE_SNOW
USE MODD_SURF_PAR, ONLY: XUNDEF
USE YOMHOOK,       ONLY: LHOOK, DR_HOOK
USE PARKIND1,      ONLY: JPRB
IMPLICIT NONE

TYPE, PUBLIC, EXTENDS(SEA_ICE_t) :: SICE_t
  PRIVATE
    TYPE(SICE_CONFIG_t) :: CONFIG
    TYPE(SICE_SNOW_t), POINTER :: SNOW

    INTEGER :: NUM_LAYERS !< Number of ice layers
    INTEGER :: NUM_POINTS !< Size of the SEA tile

    REAL, POINTER :: Z(:,:) !< Depth of lower boundary for each vertical layer. [m]
    REAL, POINTER :: DZ(:,:) !< Thickness of each layer. [m]
    REAL, POINTER :: Z_DIFF(:,:) !< Distance between two layers. [m]
    REAL, POINTER :: T(:,:) !< Mean temperature of ice layers. [K]

    REAL, POINTER :: AGE(:) !< Ice age [s]
    REAL, POINTER :: THICKNESS(:) !< Ice thickness [m]

    REAL, POINTER :: XTICE(:)
    REAL, POINTER :: XSIC(:)
    REAL, POINTER :: XICE_ALB(:)

    ! Part of input data are copied during COUPLING_ICEFLUX call
    REAL, POINTER :: XCH(:)
    REAL, POINTER :: XRHOA(:)
    REAL, POINTER :: XWIND(:)
    REAL, POINTER :: XPS(:)
    REAL, POINTER :: XEXNA(:)
    REAL, POINTER :: XEXNS(:)
    REAL, POINTER :: PTA(:)
    REAL, POINTER :: PQA(:)
    REAL, POINTER :: PZREF(:)
    REAL, POINTER :: PUREF(:)

    TYPE( MODEL_FIELD ), ALLOCATABLE :: MF(:)

    REAL :: SIC_EFOLDING_TIME = 0.0
    LOGICAL :: LDAMP_SIC = .FALSE.
  CONTAINS
    PROCEDURE :: INIT
    PROCEDURE :: PREP
    PROCEDURE :: ASSIM
    PROCEDURE :: RUN
    PROCEDURE :: DEALLOC

    PROCEDURE :: READSURF
    PROCEDURE :: WRITESURF
    PROCEDURE :: WRITE_DIAG

    PROCEDURE :: GET_RESPONSE
    PROCEDURE :: DIAG_MISC

    PROCEDURE :: SET_DAMPING

    PROCEDURE, PASS :: COUPLING_ICEFLUX => COUPLING_ICEFLUX_SICE

    PROCEDURE, PRIVATE, PASS :: GET_MODEL_FIELDS
    PROCEDURE, PRIVATE, PASS :: ALLOCA
    PROCEDURE, PRIVATE, PASS :: REGRID
    PROCEDURE, PRIVATE, PASS :: RUN_INTERNAL
    PROCEDURE, PRIVATE, PASS :: GROWTH

    PROCEDURE, PRIVATE, NOPASS :: IO
END TYPE SICE_t

CONTAINS

SUBROUTINE INIT(THIS, HPROGRAM)
IMPLICIT NONE
  CLASS(SICE_t) :: THIS
  CHARACTER(LEN=6), INTENT(IN)  :: HPROGRAM
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('ICE_SICE:INIT', 0, ZHOOK_HANDLE)

  THIS%NUM_LAYERS = 4 ! Hard-coded default value
  THIS%CONFIG = READ_SICE_CONFIG(HPROGRAM)

  NULLIFY(THIS%THICKNESS)

  IF(THIS%CONFIG%LICE_HAS_SNOW) THEN
    ALLOCATE(THIS%SNOW)
    CALL THIS%SNOW%INIT(HPROGRAM)
  ELSE
    NULLIFY(THIS%SNOW)
  ENDIF

  IF (LHOOK) CALL DR_HOOK('ICE_SICE:INIT', 1, ZHOOK_HANDLE)
END SUBROUTINE INIT

SUBROUTINE PREP(THIS, DTCO, U, GCP, YDCTL, KLU, KLAT, &
                HPROGRAM, HATMFILE, HATMFILETYPE, HPGDFILE, HPGDFILETYPE)
USE MODD_DATA_COVER_n, ONLY: DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY: SURF_ATM_t
USE MODD_GRID_CONF_PROJ_n, ONLY: GRID_CONF_PROJ_t
USE MODE_PREP_CTL, ONLY: PREP_CTL
USE MODD_CSTS, ONLY: XTTSI
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODE_POS_SURF
IMPLICIT NONE
  CLASS(SICE_t) :: THIS
  TYPE(DATA_COVER_t),    INTENT(INOUT) :: DTCO
  TYPE(SURF_ATM_t),      INTENT(INOUT) :: U
  TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
  TYPE (PREP_CTL),       INTENT(INOUT) :: YDCTL
  INTEGER, INTENT(IN) :: KLU
  INTEGER, INTENT(IN) :: KLAT
  CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  !< program calling surf. schemes
  CHARACTER(LEN=28),  INTENT(IN)  :: HATMFILE    !< name of the Atmospheric file
  CHARACTER(LEN=6),   INTENT(IN)  :: HATMFILETYPE!< type of the Atmospheric file
  CHARACTER(LEN=28),  INTENT(IN)  :: HPGDFILE    !< name of the PGD file
  CHARACTER(LEN=6),   INTENT(IN)  :: HPGDFILETYPE!< type of the PGD file

  REAL, ALLOCATABLE :: ZTMP(:)

  INTEGER :: ILUNAM
  INTEGER :: ILUOUT

  LOGICAL :: GFOUND
  INTEGER :: JI, IN
  LOGICAL, ALLOCATABLE :: GPRUNE_ALL(:)

  REAL :: XICE_TUNIF
  LOGICAL :: LINIT_FROM_SST
  INTEGER :: NUM_LAYERS
  REAL :: XICE_THICKNESS
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  NAMELIST /NAM_PREP_SEAICE_SICE/  &
    XICE_TUNIF,                 &
    LINIT_FROM_SST,             &
    NUM_LAYERS,                 &
    XICE_THICKNESS

  IF (LHOOK) CALL DR_HOOK('ICE_SICE:PREP', 0, ZHOOK_HANDLE)

  XICE_TUNIF = XTTSI
  LINIT_FROM_SST = .FALSE.
  NUM_LAYERS = THIS%NUM_LAYERS
  XICE_THICKNESS = THIS%CONFIG%XNEW_ICE_THK

  CALL GET_LUOUT(HPROGRAM,ILUOUT)

  CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
  CALL POSNAM(ILUNAM,'NAM_PREP_SEAICE_SICE',GFOUND,ILUOUT)
  IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_PREP_SEAICE_SICE)
  CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)

  THIS%NUM_LAYERS = NUM_LAYERS
  THIS%NUM_POINTS = KLU
  THIS%CONFIG%XNEW_ICE_THK = XICE_THICKNESS

  IF(THIS%CONFIG%LICE_HAS_SNOW) THEN
    CALL THIS%SNOW%PREP(KLU, HPROGRAM, HATMFILE, HATMFILETYPE, HPGDFILE, HPGDFILETYPE)
  END IF

  CALL THIS%ALLOCA()
  IF(.NOT. ALLOCATED(THIS%MF)) THEN
    CALL THIS%GET_MODEL_FIELDS(THIS%MF)
  END IF
  ! Reset all model fields to the default state before initialization
  ALLOCATE(GPRUNE_ALL(THIS%NUM_POINTS))
  GPRUNE_ALL(:) = .TRUE.
  CALL PRUNE(THIS%MF, GPRUNE_ALL)
  DEALLOCATE(GPRUNE_ALL)

  CALL THIS%REGRID()
  FILL_BY_UNIFORM: IF(.NOT. LINIT_FROM_SST) THEN
    WRITE(ILUOUT, *) 'Filling ice by the uniform prescribed profile...'
    THIS%T = XICE_TUNIF
  ELSE FILL_BY_UNIFORM
    WRITE(ILUOUT, *) 'Filling ice by the SST data...'
    IN = 0
    ALLOCATE(ZTMP( THIS%NUM_LAYERS + 2 ))
    DO JI = 1, THIS%NUM_POINTS
      IF(THIS%XSST(JI) < XTTSI) THEN
        CALL LIN_SPACE(THIS%XSST(JI), XTTSI,                               &
            [THIS%Z(JI,:) - .5*THIS%DZ(JI,:), THIS%Z(JI,THIS%NUM_LAYERS)], &
            ZTMP(:THIS%NUM_LAYERS + 1))
        THIS%T(JI,:) = ZTMP(:THIS%NUM_LAYERS)
        IN = IN + 1
      END IF
    END DO
    DEALLOCATE(ZTMP)
    WRITE(ILUOUT, *) 'Done for', IN, 'point(s) of', THIS%NUM_POINTS
  END IF FILL_BY_UNIFORM

  IF (LHOOK) CALL DR_HOOK('ICE_SICE:PREP', 1, ZHOOK_HANDLE)
END SUBROUTINE PREP

SUBROUTINE ASSIM(THIS, HPROGRAM, PSIC_IN, PLON_IN, PLAT_IN)
USE MODD_CSTS, ONLY: XTTSI
USE MODI_OL_PROPAGATE_ICE
USE MODI_ABOR1_SFX
IMPLICIT NONE
  CLASS(SICE_t) :: THIS
  CHARACTER(LEN=6),   INTENT(IN) :: HPROGRAM
  REAL,               INTENT(IN) :: PSIC_IN(:)
  REAL,               INTENT(IN) :: PLON_IN(:)
  REAL,               INTENT(IN) :: PLAT_IN(:)

  REAL, DIMENSION(THIS%NUM_POINTS) :: ZSDF, ZW, ZMIXED_TICE, ZICE_THICKNESS
  REAL :: Z_TI(THIS%NUM_LAYERS)
  LOGICAL :: GMISSING_OLD_ICE
  REAL :: ZEDGE_THK ! Mean ice thickness along the old ice edge
  REAL :: Z_T0, Z_TF, Z_H, Z_P1X, Z_P1Y
  INTEGER :: JI
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('ICE_SICE:ASSIM', 0, ZHOOK_HANDLE)

  ZSDF = 0.
  ZW = 0.
  ZICE_THICKNESS(:) = THIS%THICKNESS(:)

  IF(HPROGRAM == 'AROME ') THEN
#ifdef SFX_ARO
    CALL ARO_PROPAGATE_ICE( &
      THIS%NUM_POINTS,      &
      THIS%NUM_LAYERS,      &
      PLON_IN,              &
      PLAT_IN,              &
      THIS%XSIC,            &
      PSIC_IN,              &
      THIS%T,               &
      ZICE_THICKNESS,       &
      ZSDF,                 &
      GMISSING_OLD_ICE,     &
      ZEDGE_THK)
#else
    CALL ABOR1_SFX('SFX_ARO should be defined while calling SURFEX from AROME')
#endif
  ELSE
    CALL OL_PROPAGATE_ICE(  &
      THIS%NUM_POINTS,      &
      THIS%NUM_LAYERS,      &
      PLON_IN,              &
      PLAT_IN,              &
      THIS%XSIC,            &
      PSIC_IN,              &
      THIS%T,               &
      ZICE_THICKNESS,       &
      ZSDF,                 &
      GMISSING_OLD_ICE,     &
      ZEDGE_THK)
  END IF

  IF(GMISSING_OLD_ICE) THEN
    DO JI = 1, THIS%NUM_POINTS
      IF(PSIC_IN(JI) > 0) THIS%T(JI,:) = XTTSI
    END DO

    CALL PRUNE(THIS%MF, .NOT. (PSIC_IN > 0.))
    CALL THIS%REGRID()
    IF (LHOOK) CALL DR_HOOK('ICE_SICE:ASSIM', 1, ZHOOK_HANDLE)
    RETURN
  END IF

  WHERE(ZSDF > 0.)
    ZW(:) = EXP(-10*EXP(-(1.0 - 0.8*PSIC_IN(:))*ZSDF(:)/2500.0))
    WHERE(ZICE_THICKNESS(:) > ZEDGE_THK)
      ZICE_THICKNESS(:) = ZW(:)*ZEDGE_THK + (1. - ZW(:))*ZICE_THICKNESS(:)
    ENDWHERE
  ENDWHERE

  DO JI = 1, THIS%NUM_LAYERS
    WHERE(ZSDF > 0.)
      ZMIXED_TICE(:) = ZW(:)*XTTSI + (1. - ZW(:))*THIS%T(:,JI)
    ELSEWHERE
      ZMIXED_TICE(:) = THIS%T(:,JI)
    ENDWHERE
    THIS%T(:,JI) = ZMIXED_TICE(:)
  END DO

  CALL PRUNE(THIS%MF, .NOT. (PSIC_IN > 0.))
  CALL THIS%REGRID(ZICE_THICKNESS)
  IF (LHOOK) CALL DR_HOOK('ICE_SICE:ASSIM', 1, ZHOOK_HANDLE)
END SUBROUTINE ASSIM

SUBROUTINE RUN( &
    THIS, HPROGRAM, PTIMEC, PTSTEP, KSTEP, ESM_CPL, PFSIC, PFSIT, PSI_FLX_DRV, PFREEZING_SST, &
    PZENITH, PSW_TOT, PLW, &
    PPEW_A_COEF, PPEW_B_COEF, PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF)
USE MODD_CSTS, ONLY: XDAY
USE MODI_ABOR1_SFX
IMPLICIT NONE
  CLASS(SICE_t) :: THIS
  CHARACTER(LEN=6),    INTENT(IN) :: HPROGRAM  !< program calling surf. schemes
  REAL,                INTENT(IN) :: PTIMEC    !< current duration since start of the run (s)
  REAL,                INTENT(IN) :: PTSTEP    !< surface time-step (s)
  INTEGER, INTENT(IN) :: KSTEP !< number of ice time steps within a surface time-step
  TYPE(ESM_CPL_t), INTENT(INOUT) :: ESM_CPL
  REAL, INTENT(IN) :: PFSIC(:)
  REAL, INTENT(IN) :: PFSIT(:)
  REAL, INTENT(IN) :: PSI_FLX_DRV
  REAL, INTENT(IN) :: PFREEZING_SST

  REAL, INTENT(IN) :: PZENITH(:) !< Zenithal angle at t  (radian from the vertical)
  REAL, INTENT(IN) :: PSW_TOT(:) !< Shortwave radiation flux at the surface.
  REAL, INTENT(IN) :: PLW(:) !< Longwave radiation flux at the surface.

  REAL, INTENT(IN) :: PPEW_A_COEF(:) ! implicit coefficients   (m2s/kg)
  REAL, INTENT(IN) :: PPEW_B_COEF(:) ! needed if HCOUPLING='I' (m/s)
  REAL, INTENT(IN) :: PPET_A_COEF(:)
  REAL, INTENT(IN) :: PPEQ_A_COEF(:)
  REAL, INTENT(IN) :: PPET_B_COEF(:)
  REAL, INTENT(IN) :: PPEQ_B_COEF(:)

  INTEGER :: IMASK(THIS%NUM_POINTS)
  INTEGER :: JJ, ISIZE
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('ICE_SICE:RUN', 0, ZHOOK_HANDLE)

  IF(THIS%LINTERPOL_SIC) THEN
     IF(SIZE(THIS%XSIC) /= SIZE(PFSIC)) THEN
        CALL ABOR1_SFX('XSIC and PFSIC size mismatch')
     ENDIF
     IF(THIS%LDAMP_SIC) THEN
        WHERE(PFSIC /= XUNDEF)
           WHERE(ABS(THIS%XSIC - PFSIC) < 0.001)
              THIS%XSIC = PFSIC
           ELSE WHERE
              THIS%XSIC = THIS%XSIC + PTSTEP/(THIS%SIC_EFOLDING_TIME*XDAY)* &
                         (PFSIC - THIS%XSIC)
           END WHERE
        END WHERE
     ELSE
        WHERE(PFSIC /= XUNDEF) THIS%XSIC = PFSIC
     ENDIF
  ENDIF

  IMASK(:) = 0
  ISIZE = 0
  DO JJ = 1, THIS%NUM_POINTS
     IF( THIS%XSIC(JJ) > 0. ) THEN
       ISIZE=ISIZE+1
       IMASK(ISIZE)=JJ
     END IF
  END DO

  IF(ISIZE > 0) THEN
    CALL PACK_AND_RUN(ISIZE, IMASK)
  END IF

  IF(THIS%NUM_POINTS > 0) THEN
    CALL PRUNE(THIS%MF, .NOT. (THIS%XSIC(:) > 0.))
  END IF
  ! Resets input accumulation fields for next step
  !
  ESM_CPL%XCPL_SEA_RAIN=0.
  ESM_CPL%XCPL_SEA_SNOW=0.
  ESM_CPL%XCPL_SEA_EVAP=0.
  ESM_CPL%XCPL_SEA_SNET=0.
  ESM_CPL%XCPL_SEA_HEAT=0.
  ESM_CPL%XCPL_SEAICE_EVAP=0.
  ESM_CPL%XCPL_SEAICE_SNET=0.
  ESM_CPL%XCPL_SEAICE_HEAT=0.

  IF (LHOOK) CALL DR_HOOK('ICE_SICE:RUN', 1, ZHOOK_HANDLE)
CONTAINS
SUBROUTINE PACK_AND_RUN(KSIZE, KMASK)
IMPLICIT NONE
  INTEGER, INTENT(IN) :: KSIZE
  INTEGER, INTENT(IN), DIMENSION(:) :: KMASK

  REAL, DIMENSION(KSIZE), TARGET  ::  &
      ZEXNA,                          & ! Exner function at atm. level
      ZRHOA,                          & ! air density                           (kg/m3)
      ZEXNS,                          & ! Exner function at sea surface
      ZQA,                            & ! air humidity forcing                  (kg/m3)
      ZRR,                            & ! liquid precipitation                  (kg/m2/s)
      ZRS,                            & ! snow precipitation                    (kg/m2/s)
      ZWIND,                          & ! module of wind at atm. wind level
      ZPS,                            & ! pressure at atmospheric model surface (Pa)
      ZSW,                            &
      ZLW,                            &
      ZZENITH,                        &
      ZTA,                            &
      ZUREF,                          &
      ZZREF,                          &
      ZCH,                            &
      ZPEW_A_COEF,                    &
      ZPEW_B_COEF,                    &
      ZPET_A_COEF,                    &
      ZPET_B_COEF,                    &
      ZPEQ_A_COEF,                    &
      ZPEQ_B_COEF
  TYPE(SEA_ICE_FORCING_t) :: FORC
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('ICE_SICE:RUN:PACK_AND_RUN', 0, ZHOOK_HANDLE)

  ZRR        (:) = ESM_CPL%XCPL_SEA_RAIN( KMASK(:KSIZE) )/PTSTEP
  ZRS        (:) = ESM_CPL%XCPL_SEA_SNOW( KMASK(:KSIZE) )/PTSTEP

  ZCH        (:) = THIS%XCH   ( KMASK(:KSIZE) )
  ZRHOA      (:) = THIS%XRHOA ( KMASK(:KSIZE) )
  ZWIND      (:) = THIS%XWIND ( KMASK(:KSIZE) )
  ZPS        (:) = THIS%XPS   ( KMASK(:KSIZE) )
  ZEXNA      (:) = THIS%XEXNA ( KMASK(:KSIZE) )
  ZEXNS      (:) = THIS%XEXNS ( KMASK(:KSIZE) )
  ZTA        (:) = THIS%PTA   ( KMASK(:KSIZE) )
  ZQA        (:) = THIS%PQA   ( KMASK(:KSIZE) )
  ZZREF      (:) = THIS%PZREF ( KMASK(:KSIZE) )
  ZUREF      (:) = THIS%PUREF ( KMASK(:KSIZE) )

  ZZENITH    (:) = PZENITH    ( KMASK(:KSIZE) )
  ZSW        (:) = PSW_TOT    ( KMASK(:KSIZE) )
  ZLW        (:) = PLW        ( KMASK(:KSIZE) )
  ZPEW_A_COEF(:) = PPEW_A_COEF( KMASK(:KSIZE) )
  ZPEW_B_COEF(:) = PPEW_B_COEF( KMASK(:KSIZE) )
  ZPET_A_COEF(:) = PPET_A_COEF( KMASK(:KSIZE) )
  ZPET_B_COEF(:) = PPET_B_COEF( KMASK(:KSIZE) )
  ZPEQ_A_COEF(:) = PPEQ_A_COEF( KMASK(:KSIZE) )
  ZPEQ_B_COEF(:) = PPEQ_B_COEF( KMASK(:KSIZE) )

  FORC%KSIZE = KSIZE

  FORC%RHOA        => ZRHOA
  FORC%PSURF       => ZPS
  FORC%WIND        => ZWIND
  FORC%SW          => ZSW
  FORC%LW          => ZLW
  FORC%PRATE_R     => ZRR
  FORC%PRATE_S     => ZRS
  FORC%EXNS        => ZEXNS
  FORC%EXNA        => ZEXNA
  FORC%CH          => ZCH
  FORC%TA          => ZTA
  FORC%QA          => ZQA
  FORC%ZREF        => ZZREF
  FORC%UREF        => ZUREF
  FORC%ZENITH      => ZZENITH

  FORC%PPEW_A_COEF => ZPEW_A_COEF
  FORC%PPEW_B_COEF => ZPEW_B_COEF
  FORC%PPET_A_COEF => ZPET_A_COEF
  FORC%PPET_B_COEF => ZPET_B_COEF
  FORC%PPEQ_A_COEF => ZPEQ_A_COEF
  FORC%PPEQ_B_COEF => ZPEQ_B_COEF

  CALL PACK(THIS%MF, KMASK(:KSIZE))
  CALL THIS%RUN_INTERNAL(HPROGRAM, PTIMEC, PTSTEP, KSTEP, PFREEZING_SST, FORC)
  CALL UNPACK(THIS%MF, KMASK(:KSIZE))

  IF (LHOOK) CALL DR_HOOK('ICE_SICE:RUN:PACK_AND_RUN', 1, ZHOOK_HANDLE)
END SUBROUTINE PACK_AND_RUN
END SUBROUTINE RUN

SUBROUTINE RUN_INTERNAL(THIS, HPROGRAM, PTIMEC, PTSTEP, KSTEP, PFREEZING_SST, FORC)
USE MODD_CSTS,     ONLY: XTTSI,   &
                         XCPD,    &
                         XSTEFAN, &
                         XLSTT,   &
                         XTT,     &
                         XCL,     &
                         XRHOLI,  &
                         XLMTT
USE MODD_SURF_PAR, ONLY: XUNDEF
USE MODE_THERMOS,  ONLY: QSATI,    &
                         DQSATI

USE MODI_SOIL_HEATDIF
IMPLICIT NONE
  CLASS(SICE_t) :: THIS
  CHARACTER(LEN=6),    INTENT(IN) :: HPROGRAM  !< program calling surf. schemes
  REAL,                INTENT(IN) :: PTIMEC    !< current duration since start of the run (s)
  REAL,                INTENT(IN) :: PTSTEP    !< surface time-step (s)
  INTEGER, INTENT(IN) :: KSTEP !< number of ice time steps within a surface time-step
  REAL, INTENT(IN) :: PFREEZING_SST
  TYPE(SEA_ICE_FORCING_t), INTENT(IN) :: FORC

  REAL, DIMENSION( FORC%KSIZE ) :: &
      ZCH,        &

      ZQSAT,      &
      ZDQSAT,     &

      ZVMOD,      &

      ZCT,        &
      ZTTSI,      &
      ZICE_ALBEDO,&
      ZICE_EMISS, &
      ZTERM2,     &
      ZTERM1,     &

      ZALPHA, ZBETA, ZGAMMA_T, ZGAMMA_Q,  &
      Z_PET_A_SFC, Z_PET_B_SFC,                 &
      Z_PEQ_A_SFC, Z_PEQ_B_SFC,                 &

      ZTMP, ZTMP2
  REAL, DIMENSION( FORC%KSIZE ), TARGET :: &
      ZDUMMY
  REAL, DIMENSION( FORC%KSIZE, THIS%NUM_LAYERS ) :: &
      ZICECOND,   &
      ZICEHCAP,   &
      ZDQ_DZ,     &
      ZQ_Z, &
      ZFLUX_COR, &
      ZFRZ
  REAL, DIMENSION( THIS%NUM_LAYERS ) :: &
      Z_EXT, Z_I0
  LOGICAL, DIMENSION( FORC%KSIZE ) :: &
      G_MELT

  INTEGER :: I, N, M
  LOGICAL :: HAS_SNOW, HAS_SNOW_POINTS( THIS%NUM_POINTS )
  TYPE(SNOW_RESPONSE_t) :: SNOW_RESP
  REAL( KIND = JPRB ) :: ZHOOK_HANDLE

  IF( LHOOK ) CALL DR_HOOK( 'ICE_SICE:RUN_INTERNAL', 0, ZHOOK_HANDLE )

  HAS_SNOW = ASSOCIATED( THIS%SNOW )
  M = FORC%KSIZE
  !
  ZCH(:) = FORC%CH(:)
  ZVMOD(:) = FORC%WIND(:)

  !-----------------------------------------------------------------------
  ! Initialization of sea ice temperature in points with new ice
  IF (THIS%CONFIG%LICE_MASS_BALANCE) THEN
      ZTERM1 = THIS%Z(:M, THIS%NUM_LAYERS)
      WHERE(THIS%T( :M, 1 ) == XUNDEF)
          ZTERM1 = THIS%CONFIG%XNEW_ICE_THK
      END WHERE
      WHERE(ZTERM1 < 0.01)
          ZTERM1 = 0.01
      END WHERE
      CALL THIS%REGRID(ZTERM1)
  ELSE
      IF(ANY( THIS%T( :M, 1 ) == XUNDEF )) CALL THIS%REGRID()
  END IF
  DO I = 1, M
      IF( THIS%T( I, 1 ) == XUNDEF ) THEN
          THIS%T(I, :) = XTTSI
      END IF
  END DO
  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------
  ! Saturated specified humidity near the ice surface
  ZQSAT  = QSATI ( THIS%T(:M,1), FORC%PSURF        )
  ZDQSAT = DQSATI( THIS%T(:M,1), FORC%PSURF, ZQSAT )
  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------
  ! Ice temperature evolution
  !Parametrization
  CALL THERMAL_PROPERTIES( THIS%T(:M,:), 3., ZICEHCAP(:,:), ZICECOND(:,:), ZFRZ(:,:) )


  ZCT = 1./(ZICEHCAP( :, 1 )*THIS%Z(:M,1))

  !   Deep temperature for lower boundary condition
  ZTTSI(:) = PFREEZING_SST + XTT

  !   Parametrization of ice albedo -- one of hightsi's albedo
  !   parametrization pack.
  ZICE_ALBEDO = ICE_ALBEDO( THIS%T(:M,1), THIS%CONFIG%NICE_ALBEDO )
  ZICE_EMISS  = .99

  DO I = 1, M
      WHERE( THIS%Z(I,:) <= 0.1 )
          Z_EXT = 17.0
          Z_I0  = 1.0
      ELSEWHERE
          Z_EXT = 1.5
          Z_I0  = 0.18
      END WHERE
      ZQ_Z  (I,:) = ( 1. - ZICE_ALBEDO(I) )*FORC%SW(I)*Z_I0(:)*EXP(-Z_EXT(:)*THIS%Z(I,:))
      ZDQ_DZ(I,:) = ZQ_Z  (I,:)*Z_EXT(:)
  END DO

  ZDQ_DZ(:M, 1) = ( 1. - ZICE_ALBEDO(:) )*FORC%SW(:) - ZQ_Z(:M, 1)
  DO I = 2, THIS%NUM_LAYERS
      ZDQ_DZ(:M, I) = ZQ_Z(:M, I - 1) - ZQ_Z(:M, I)
  END DO

  IF( HAS_SNOW ) THEN
      HAS_SNOW_POINTS = THIS%SNOW%EXISTS()
      CALL THIS%SNOW%RUN( PTSTEP, FORC, THIS%T(:M,1), THIS%Z(:M,1), ZICECOND(:,1), ZICE_ALBEDO)
      CALL THIS%SNOW%GET_RESPONSE( M, SNOW_RESP )
  ELSE
      HAS_SNOW_POINTS = .FALSE.

     ! Set dummy response from the snow scheme to make compiler happy.
     ZDUMMY(:) = 0.
     SNOW_RESP%HEAT_FLUX => ZDUMMY(:)
     SNOW_RESP%WATER_FLUX => ZDUMMY(:)
  END IF
  !coefficients for implicit coupling
  ZTMP = (1. - FORC%PPET_A_COEF*FORC%RHOA*ZCH*ZVMOD)

  Z_PET_A_SFC = -FORC%PPET_A_COEF*FORC%RHOA*ZCH*ZVMOD*(FORC%EXNA/FORC%EXNS)/ZTMP
  Z_PET_B_SFC = FORC%PPET_B_COEF*FORC%EXNA/ZTMP

  ZTMP = 1. - FORC%PPEQ_A_COEF*FORC%RHOA*ZCH*ZVMOD

  Z_PEQ_A_SFC = - FORC%PPEQ_A_COEF*FORC%RHOA*ZCH*ZVMOD*ZDQSAT/ZTMP
  Z_PEQ_B_SFC = (FORC%PPEQ_B_COEF - FORC%PPEQ_A_COEF*FORC%RHOA*ZCH*ZVMOD*( ZQSAT - ZDQSAT*THIS%T(:M,1) ) )/ZTMP


  WHERE( HAS_SNOW_POINTS(:M) )
    ZALPHA = 1 + PTSTEP*ZCT*(XCL*SNOW_RESP%WATER_FLUX(:M) + ZICECOND(:,1)/THIS%Z_DIFF( :M, 1))
    ZBETA = PTSTEP*ZCT*ZICECOND(:,1)/THIS%Z_DIFF( :M, 1)
    ZTERM2 = ZBETA/ZALPHA
    ZTERM1 = (THIS%T(:M,1) + PTSTEP*ZCT*(SNOW_RESP%HEAT_FLUX(:M) + XCL*XTT*SNOW_RESP%WATER_FLUX(:M)))/ZALPHA
  ELSEWHERE
    !surface energy balance
    ZALPHA =                                                           &
                1./(PTSTEP*ZCT)                                     &
              + 4.*ZICE_EMISS*XSTEFAN*THIS%T(:M,1)**3                    &
              + FORC%RHOA*XCPD*ZCH*ZVMOD/FORC%EXNS                    &
              + XLSTT*FORC%RHOA*ZCH*ZVMOD*ZDQSAT                     &
              + ZICECOND(:,1)/THIS%Z_DIFF(:M,1)

    ZBETA  =                                                           &
                1./(PTSTEP*ZCT)*THIS%T(:M,1)                         &
              + ( 1. - ZICE_ALBEDO )*FORC%SW - ZQ_Z(:,1)               &
              + FORC%LW                                                 &
              + 3.*ZICE_EMISS*XSTEFAN*THIS%T(:M,1)**4                    &
              - XLSTT*FORC%RHOA*ZCH*ZVMOD*( ZQSAT - ZDQSAT*THIS%T(:M,1) )

    ZGAMMA_T = FORC%RHOA*XCPD*ZCH*ZVMOD/FORC%EXNA
    ZGAMMA_Q = FORC%RHOA*XLSTT*ZCH*ZVMOD

    ZTMP = ZALPHA - ZGAMMA_T*Z_PET_A_SFC - ZGAMMA_Q*Z_PEQ_A_SFC

    ZTERM1 = (ZBETA + ZGAMMA_T*Z_PET_B_SFC + ZGAMMA_Q*Z_PEQ_B_SFC)/ZTMP
    ZTERM2 = ZICECOND(:,1)/THIS%Z_DIFF(:M,1)/ZTMP
  END WHERE


  !correction flux
  ZFLUX_COR = 0.
  DO I = 1, M
      IF(.NOT. HAS_SNOW_POINTS(I)) ZFLUX_COR(I,:) = ZDQ_DZ(I,:) !*THIS%DZ(I,:)
  END DO

  ZTMP = 0.
  ZTMP2 = 0.

  CALL SOIL_HEATDIF(PTSTEP,THIS%DZ(:M,:),THIS%Z_DIFF(:M,:),ZICECOND,      &
                    ZICEHCAP,ZCT,ZTERM1,ZTERM2,ZTMP,ZTTSI,THIS%T(:M,:), ZTMP2, PFLUX_COR = ZFLUX_COR )

  WHERE(THIS%T(:M,1) > ZFRZ(:,1))
      G_MELT(:) = .TRUE.
  ELSE WHERE
      G_MELT(:) = .FALSE.
  END WHERE

  IF(THIS%CONFIG%LICE_MASS_BALANCE) THEN
    IF(HAS_SNOW) THEN
        !ZTERM1 = THIS%Z(:M, THIS%NUM_LAYERS) + SNOW_RESP%WATER_FLUX(:)*PTSTEP/917.0
        !CALL THIS%REGRID(ZTERM1)
    ENDIF
    WHERE(G_MELT)
      THIS%T(:M,1) = ZFRZ(:,1)
      WHERE(HAS_SNOW_POINTS(:M))
        ZTERM1 = SNOW_RESP%HEAT_FLUX(:M) &
            - ZICECOND(:,1)*(THIS%T(:M,1) - THIS%T(:M,2))/THIS%Z_DIFF(:M,1)
      ELSE WHERE
        ZTERM1 = ( 1. - ZICE_ALBEDO )*FORC%SW - ZQ_Z(:,1) &
            + FORC%LW - ZICE_EMISS*XSTEFAN*THIS%T(:M,1)**4 &
            - ZGAMMA_Q*(ZQSAT(:) - Z_PEQ_A_SFC *THIS%T(:M,1) - Z_PEQ_B_SFC) &
            - ZGAMMA_T*((1. -      Z_PET_A_SFC)*THIS%T(:M,1) - Z_PET_B_SFC) &
            - ZICECOND(:,1)*(THIS%T(:M,1) - THIS%T(:M,2))/THIS%Z_DIFF(:M,1)
      END WHERE
    ELSE WHERE
      ZTERM1 = 0.
    END WHERE
    ZTERM1 = THIS%Z(:M, THIS%NUM_LAYERS) - ZTERM1*PTSTEP/(XRHOLI*XLMTT)

    WHERE(ZTERM1 < 0.01)
      ZTERM1 = 0.01
    END WHERE
    CALL THIS%REGRID(ZTERM1)

    CALL THIS%GROWTH(M, PTSTEP, ZTTSI, ZICECOND(:, THIS%NUM_LAYERS))
  ENDIF

  WHERE(THIS%T(:M,:) > ZFRZ(:,:))
      THIS%T(:M,:) = ZFRZ(:,:)
  END WHERE

  WHERE( THIS%AGE(:M) < 365*24*3600 )
    THIS%AGE(:M) = THIS%AGE(:M) + PTSTEP
  END WHERE
  IF( LHOOK ) CALL DR_HOOK( 'ICE_SICE:RUN_INTERNAL', 1, ZHOOK_HANDLE )
CONTAINS
  ELEMENTAL SUBROUTINE THERMAL_PROPERTIES( PT, PS, PC, PK, PFRZ )
    USE MODD_CSTS, ONLY: XTT
    IMPLICIT NONE
      REAL, INTENT( IN  ) :: PT, PS ! Ice temperature [K] and salinity [ppt]
      REAL, INTENT( OUT ) :: PC, PK ! Ice heat capacity [J/(m3 K)] and heat thermal conductivity [W/(K m)]
      REAL, INTENT( OUT ) :: PFRZ ! Freezing remperature of sea ice [K]

      REAL, PARAMETER ::      &
          PP_CI = 1.883E6,    & ! Volumetric heat capacity of pure ice
          PP_L  = 3.014E8,    & ! Volumetric heat of fusion of pure ice

          PP_KA = 0.03,       & ! Air heat conductivity
          PP_VA = 0.025         ! Fractional value of air in sea ice

      REAL ::      &
          ZTHETA, &  ! p_t in degC
          ZTFS,   &  ! Freezing temperature of sea ice with bulk salinity p_s

          ZKI,    &  ! Pure ice heat condictivity
          ZKB,    &  ! Brine heat conductivity
          ZKBI       ! Heat conductivity of bubbly ice

      ZTHETA = PT - XTT
      ZTFS   = -5.33E-7*PS**3 - 9.37E-6*PS**2 - 0.0592*PS + 273.15


      PC     = PP_CI - (ZTFS - XTT)/ZTHETA**2*PP_L


      ZKB    = 0.4184*( 1.25 + 0.030  *ZTHETA + 0.00014*ZTHETA**2 )
      ZKI    = 1.16  *( 1.91 - 8.66E-3*ZTHETA + 2.97E-5*ZTHETA**2 )

      ZKBI   = ( 2.0*ZKI + PP_KA - 2.0*PP_VA*(ZKI-PP_KA) )/   &
               ( 2.0*ZKI + PP_KA + 2.0*PP_VA*(ZKI-PP_KA) )*ZKI

      PK     = ZKBI - (ZKBI - ZKB)*(ZTFS - XTT)/ZTHETA
      PFRZ   = ZTFS

      IF (PK < 1.73) PK = 1.73 !hightsi-like guard

  END SUBROUTINE THERMAL_PROPERTIES
END SUBROUTINE RUN_INTERNAL

SUBROUTINE GROWTH(THIS, M, TSTEP, TDEEP, PICECOND)
USE MODD_CSTS, ONLY: XRHOLI, XLMTT
IMPLICIT NONE
  CLASS(SICE_t) :: THIS
  INTEGER, INTENT(IN) :: M
  REAL, INTENT(IN) :: TSTEP
  REAL, INTENT(IN) :: TDEEP(M)
  REAL, INTENT(IN) :: PICECOND(M)

  REAL :: ZDHDT(M), ZICE_HEAT_FLUX(M), ZNEW_THK(M)
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('ICE_SICE:GROWTH', 0, ZHOOK_HANDLE)

  ZICE_HEAT_FLUX(:) = (TDEEP - THIS%T(:M,THIS%NUM_LAYERS))/THIS%Z_DIFF(:M,THIS%NUM_LAYERS)*PICECOND
  ZDHDT = TSTEP/(XRHOLI*XLMTT)*(ZICE_HEAT_FLUX - THIS%CONFIG%XOCEAN_HEAT_FLUX)

  ZNEW_THK(:) = THIS%Z(:M, THIS%NUM_LAYERS) + ZDHDT(:)
  WHERE(ZNEW_THK < 0.01)
    ZNEW_THK = 0.01
  END WHERE
  CALL THIS%REGRID(ZNEW_THK)

  IF (LHOOK) CALL DR_HOOK('ICE_SICE:GROWTH', 1, ZHOOK_HANDLE)
END SUBROUTINE GROWTH

SUBROUTINE DEALLOC(THIS)
IMPLICIT NONE
  CLASS(SICE_t) :: THIS
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('ICE_SICE:DEALLOC', 0, ZHOOK_HANDLE)

  IF(ALLOCATED(THIS%MF)) THEN
    DEALLOCATE(THIS%MF)
  END IF

  IF(ASSOCIATED(THIS%Z))        DEALLOCATE(THIS%Z)
  IF(ASSOCIATED(THIS%DZ))       DEALLOCATE(THIS%DZ)
  IF(ASSOCIATED(THIS%Z_DIFF))   DEALLOCATE(THIS%Z_DIFF)
  IF(ASSOCIATED(THIS%T))        DEALLOCATE(THIS%T)
  IF(ASSOCIATED(THIS%AGE))      DEALLOCATE(THIS%AGE)

  IF(ASSOCIATED(THIS%XTICE))    DEALLOCATE(THIS%XTICE)
  IF(ASSOCIATED(THIS%XSIC))     DEALLOCATE(THIS%XSIC)
  IF(ASSOCIATED(THIS%XICE_ALB)) DEALLOCATE(THIS%XICE_ALB)

  IF(ASSOCIATED(THIS%XCH))      DEALLOCATE(THIS%XCH)

  IF(ASSOCIATED(THIS%XRHOA))    DEALLOCATE(THIS%XRHOA)
  IF(ASSOCIATED(THIS%XWIND))    DEALLOCATE(THIS%XWIND)
  IF(ASSOCIATED(THIS%XPS  ))    DEALLOCATE(THIS%XPS)
  IF(ASSOCIATED(THIS%XEXNA))    DEALLOCATE(THIS%XEXNA)
  IF(ASSOCIATED(THIS%XEXNS))    DEALLOCATE(THIS%XEXNS)

  IF(ASSOCIATED(THIS%PTA  ))    DEALLOCATE(THIS%PTA)
  IF(ASSOCIATED(THIS%PQA  ))    DEALLOCATE(THIS%PQA)
  IF(ASSOCIATED(THIS%PZREF))    DEALLOCATE(THIS%PZREF)
  IF(ASSOCIATED(THIS%PUREF))    DEALLOCATE(THIS%PUREF)

  IF(ASSOCIATED(THIS%SNOW)) CALL THIS%SNOW%DEALLOC()

  IF (LHOOK) CALL DR_HOOK('ICE_SICE:DEALLOC', 1, ZHOOK_HANDLE)
END SUBROUTINE DEALLOC

SUBROUTINE READSURF(THIS, G, HPROGRAM, KLU, KLUOUT)
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODI_READ_SURF
IMPLICIT NONE
  CLASS(SICE_t) :: THIS
  TYPE(GRID_t), INTENT(INOUT)   :: G
  CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM !< calling program
  INTEGER,           INTENT(IN) :: KLU      !< number of sea patch point
  INTEGER,           INTENT(IN) :: KLUOUT

  REAL :: ZICE_THICKNESS(KLU)
  INTEGER :: IRESP
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('ICE_SICE:READSURF', 0, ZHOOK_HANDLE)

  THIS%NUM_POINTS = KLU
  IF(ASSOCIATED(THIS%SNOW)) THEN
    THIS%SNOW%NUM_POINTS = THIS%NUM_POINTS
    CALL THIS%SNOW%ALLOCA()
  END IF

  CALL READ_SURF(HPROGRAM,'ICENL', THIS%NUM_LAYERS,IRESP)
  CALL THIS%ALLOCA()

  CALL READ_SURF(HPROGRAM,'SIC',   THIS%XSIC,IRESP)
  WHERE(ABS(THIS%XSIC(:)) > 0)
    WHERE( THIS%XSIC(:) < 0.05 ) THIS%XSIC(:) = 0.0
  ENDWHERE

  IF(ALLOCATED(THIS%MF)) THEN
    DEALLOCATE(THIS%MF)
  END IF
  CALL THIS%GET_MODEL_FIELDS(THIS%MF)
  CALL PRUNE(THIS%MF)

  CALL IO(THIS%MF, [''], HPROGRAM, IS_READ = .TRUE.)

  IF(THIS%NUM_POINTS > 0) THEN
    ZICE_THICKNESS = THIS%THICKNESS
    CALL THIS%REGRID(ZICE_THICKNESS)
  ENDIF

  IF (LHOOK) CALL DR_HOOK('ICE_SICE:READSURF', 1, ZHOOK_HANDLE)
END SUBROUTINE READSURF

SUBROUTINE WRITESURF(THIS, HSELECT, HPROGRAM)
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODI_WRITE_SURF
IMPLICIT NONE
  CLASS(SICE_t) :: THIS
  CHARACTER(LEN=*), INTENT(IN) :: HSELECT(:)
  CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM

  INTEGER :: IRESP
  CHARACTER(LEN=100) :: YCOMMENT
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('ICE_SICE:WRITESURF', 0, ZHOOK_HANDLE)

  YCOMMENT='Number of sea-ice layers'
  CALL WRITE_SURF(HSELECT, HPROGRAM, 'ICENL', THIS%NUM_LAYERS, IRESP, YCOMMENT)

  CALL IO(THIS%MF, HSELECT, HPROGRAM, IS_DIAG = .FALSE., IS_READ = .FALSE.)

  IF (LHOOK) CALL DR_HOOK('ICE_SICE:INIT', 1, ZHOOK_HANDLE)
END SUBROUTINE WRITESURF

SUBROUTINE WRITE_DIAG(THIS, HSELECT, HPROGRAM)
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODI_WRITE_SURF
IMPLICIT NONE
  CLASS(SICE_t) :: THIS
  CHARACTER(LEN=*), INTENT(IN) :: HSELECT(:)
  CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('ICE_SICE:WRITE_DIAG', 0, ZHOOK_HANDLE)

  CALL IO(THIS%MF, HSELECT, HPROGRAM, IS_DIAG = .TRUE., IS_READ = .FALSE.)

  IF (LHOOK) CALL DR_HOOK('ICE_SICE:WRITE_DIAG', 1, ZHOOK_HANDLE)
END SUBROUTINE WRITE_DIAG

SUBROUTINE GET_RESPONSE(THIS, PSIC, PTICE, PICE_ALB)
USE MODD_CSTS, ONLY: XTTSI
USE MODD_WATER_PAR, ONLY: XALBSEAICE
IMPLICIT NONE
  CLASS(SICE_t) :: THIS
  REAL, INTENT(OUT) :: PSIC(:)
  REAL, INTENT(OUT) :: PTICE(:)
  REAL, INTENT(OUT) :: PICE_ALB(:)

  LOGICAL :: GSNOW_POINTS(THIS%NUM_POINTS)
  REAL :: ZSNOW_TEMP(THIS%NUM_POINTS)
  REAL :: ZSNOW_ALB(THIS%NUM_POINTS)
  REAL :: ZICE_ALB(THIS%NUM_POINTS)
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('ICE_SICE:GET_RESPONSE', 0, ZHOOK_HANDLE)

  IF(ANY(THIS%XSIC > 0.)) THEN
    IF(ASSOCIATED(THIS%SNOW)) THEN
      GSNOW_POINTS(:) = THIS%SNOW%EXISTS()
      CALL THIS%SNOW%GET_RESPONSE(THIS%NUM_POINTS, PTSUR = ZSNOW_TEMP, PALB = ZSNOW_ALB)
    ELSE
      GSNOW_POINTS(:) = .FALSE.
    END IF
    WHERE(THIS%XSIC > 0.)
      ZICE_ALB(:) = ICE_ALBEDO( THIS%T(:,1), THIS%CONFIG%NICE_ALBEDO )
    ELSE WHERE
      ZICE_ALB(:) = XALBSEAICE
    END WHERE

    WHERE(GSNOW_POINTS(:))
      PTICE(:) = ZSNOW_TEMP(:)
      PICE_ALB(:) = ZSNOW_ALB(:)
    ELSE WHERE
      PTICE(:) = THIS%T(:,1)
      PICE_ALB(:) = ZICE_ALB
    END WHERE
  END IF
  ! Put some sane values over open sea. If these are
  ! just set to XUNDEF single precision setup could
  ! get FPE in some operations.
  WHERE(.NOT. THIS%XSIC > 0.)
    PTICE(:) = XTTSI
    PICE_ALB(:) = XALBSEAICE
  END WHERE

  PSIC(:) = THIS%XSIC(:)

  IF (LHOOK) CALL DR_HOOK('ICE_SICE:GET_RESPONSE', 1, ZHOOK_HANDLE)
END SUBROUTINE GET_RESPONSE

SUBROUTINE DIAG_MISC(THIS, DGMSI)
USE MODD_DIAG_MISC_SEAICE_n, ONLY: DIAG_MISC_SEAICE_t
USE MODD_SURF_PAR, ONLY: XUNDEF
IMPLICIT NONE
  CLASS(SICE_t) :: THIS
  TYPE(DIAG_MISC_SEAICE_t), INTENT(IN OUT) :: DGMSI

  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('ICE_SICE:DIAG_MISC', 0, ZHOOK_HANDLE)

  DGMSI%XSIT = THIS%THICKNESS
  IF(ASSOCIATED(THIS%SNOW)) THEN
    CALL THIS%SNOW%GET_RESPONSE(THIS%NUM_POINTS, PDEPTH=DGMSI%XSND)
  ELSE
    DGMSI%XSND = XUNDEF
  ENDIF
  DGMSI%XMLT = XUNDEF

  IF (LHOOK) CALL DR_HOOK('ICE_SICE:DIAG_MISC', 1, ZHOOK_HANDLE)
END SUBROUTINE DIAG_MISC


SUBROUTINE SET_DAMPING(THIS, PSIC_EFOLDING_TIME, PSIT_EFOLDING_TIME)
IMPLICIT NONE
  CLASS(SICE_t) :: THIS
  REAL, INTENT(IN) :: PSIC_EFOLDING_TIME
  REAL, INTENT(IN) :: PSIT_EFOLDING_TIME

  THIS%LDAMP_SIC = .FALSE.
  THIS%SIC_EFOLDING_TIME = PSIC_EFOLDING_TIME
  IF(THIS%LINTERPOL_SIC) THEN
    IF(PSIC_EFOLDING_TIME /= 0.0) THEN
      THIS%LDAMP_SIC = .TRUE.
    ENDIF
  ENDIF
END SUBROUTINE SET_DAMPING

SUBROUTINE COUPLING_ICEFLUX_SICE(THIS, KI, PTA, PEXNA, PRHOA, PTICE, PEXNS,   &
                                PQA, PRAIN, PSNOW, PWIND, PZREF, PUREF,  &
                                PPS, PTWAT, PTTS, PSFTH, PSFTQ, AT,      &
                                OHANDLE_SIC, PMASK, PQSAT, PZ0,          &
                                PUSTAR, PCD, PCDN, PCH,                  &
                                PRI, PRESA, PZ0H )
USE MODD_SURF_ATM_TURB_n, ONLY: SURF_ATM_TURB_t
USE MODI_COUPLING_ICEFLUX_n
IMPLICIT NONE
  CLASS(SICE_t) :: THIS !< Ice model

  INTEGER,             INTENT(IN)  :: KI        !< number of points
  !
  REAL, DIMENSION(KI), INTENT(IN)  :: PTA       !< air temperature forcing               [K]
  REAL, DIMENSION(KI), INTENT(IN)  :: PEXNA     !< Exner function at atm. level
  REAL, DIMENSION(KI), INTENT(IN)  :: PRHOA     !< air density                           [kg/m3]
  REAL, DIMENSION(KI), INTENT(IN)  :: PTICE     !< Ice Surface Temperature
  REAL, DIMENSION(KI), INTENT(IN)  :: PEXNS     !< Exner function at sea surface
  REAL, DIMENSION(KI), INTENT(IN)  :: PQA       !< air humidity forcing                  [kg/m3]
  REAL, DIMENSION(KI), INTENT(IN)  :: PRAIN     !< liquid precipitation                  [kg/m2/s]
  REAL, DIMENSION(KI), INTENT(IN)  :: PSNOW     !< snow precipitation                    [kg/m2/s]
  REAL, DIMENSION(KI), INTENT(IN)  :: PWIND     !< module of wind at atm. wind level
  REAL, DIMENSION(KI), INTENT(IN)  :: PZREF     !< atm. level for temp. and humidity
  REAL, DIMENSION(KI), INTENT(IN)  :: PUREF     !< atm. level for wind
  REAL, DIMENSION(KI), INTENT(IN)  :: PPS       !< pressure at atmospheric model surface [Pa]
  REAL, DIMENSION(KI), INTENT(IN)  :: PTWAT     !< Sea surface temperature
  REAL,                INTENT(IN)  :: PTTS      !< Freezing point for sea water
  REAL, DIMENSION(KI), INTENT(OUT) :: PSFTH     !< flux of heat                          [W/m2]
  REAL, DIMENSION(KI), INTENT(OUT) :: PSFTQ     !< flux of water vapor                   [kg/m2/s]
  TYPE(SURF_ATM_TURB_t), INTENT(IN) :: AT       !< atmospheric turbulence parameters
  !
  LOGICAL, INTENT(IN) , OPTIONAL:: OHANDLE_SIC  !< Should we output extended set of fields
  REAL, DIMENSION(KI), INTENT(IN) , OPTIONAL :: PMASK     !< Where to compute sea-ice fluxes (0./1.)
  !
  REAL, DIMENSION(KI), INTENT(OUT), OPTIONAL :: PQSAT     !< humidity at saturation
  REAL, DIMENSION(KI), INTENT(OUT), OPTIONAL :: PZ0       !< roughness length over the sea ice
  REAL, DIMENSION(KI), INTENT(OUT), OPTIONAL :: PUSTAR    !< friction velocity [m/s]
  REAL, DIMENSION(KI), INTENT(OUT), OPTIONAL :: PCD       !< Drag coefficient
  REAL, DIMENSION(KI), INTENT(OUT), OPTIONAL :: PCDN      !< Neutral Drag coefficient
  REAL, DIMENSION(KI), INTENT(OUT), OPTIONAL :: PCH       !< Heat transfer coefficient
  REAL, DIMENSION(KI), INTENT(OUT), OPTIONAL :: PRI       !< Richardson number
  REAL, DIMENSION(KI), INTENT(OUT), OPTIONAL :: PRESA     !< aerodynamical resistance
  REAL, DIMENSION(KI), INTENT(OUT), OPTIONAL :: PZ0H      !< heat roughness length over ice
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('ICE_SICE:COUPLING_ICEFLUX_SICE', 0, ZHOOK_HANDLE)

  CALL COUPLING_ICEFLUX_n(KI, PTA, PEXNA, PRHOA, PTICE, PEXNS,     &
                          PQA, PRAIN, PSNOW, PWIND, PZREF, PUREF,  &
                          PPS, PTWAT, PTTS, PSFTH, PSFTQ, AT,      &
                          OHANDLE_SIC, PMASK, PQSAT, PZ0,          &
                          PUSTAR, PCD, PCDN, PCH,                  &
                          PRI, PRESA, PZ0H )

  ! Store some data to be used during call to the RUN method.
  THIS%XCH  (:) = PCH  (:)
  THIS%XRHOA(:) = PRHOA(:)
  THIS%XWIND(:) = PWIND(:)
  THIS%XPS  (:) = PPS  (:)
  THIS%XEXNA(:) = PEXNA(:)
  THIS%XEXNS(:) = PEXNS(:)
  THIS%PTA  (:) = PTA  (:)
  THIS%PQA  (:) = PQA  (:)
  THIS%PZREF(:) = PZREF(:)
  THIS%PUREF(:) = PUREF(:)

  IF (LHOOK) CALL DR_HOOK('ICE_SICE:COUPLING_ICEFLUX_SICE', 1, ZHOOK_HANDLE)
END SUBROUTINE COUPLING_ICEFLUX_SICE

SUBROUTINE ALLOCA(THIS)
IMPLICIT NONE
  CLASS(SICE_t) :: THIS
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('ICE_SICE:ALLOCA', 0, ZHOOK_HANDLE)

  ALLOCATE( &
    THIS%Z(THIS%NUM_POINTS, THIS%NUM_LAYERS), &
    THIS%DZ(THIS%NUM_POINTS, THIS%NUM_LAYERS), &
    THIS%Z_DIFF(THIS%NUM_POINTS, THIS%NUM_LAYERS), &
    THIS%T(THIS%NUM_POINTS, THIS%NUM_LAYERS))

  ALLOCATE( &
    THIS%AGE(THIS%NUM_POINTS))

  ALLOCATE( &
    THIS%XTICE(THIS%NUM_POINTS), &
    THIS%XSIC(THIS%NUM_POINTS), &
    THIS%XICE_ALB(THIS%NUM_POINTS))

  ALLOCATE( &
    THIS%XCH  (THIS%NUM_POINTS), &
    THIS%XRHOA(THIS%NUM_POINTS), &
    THIS%XWIND(THIS%NUM_POINTS), &
    THIS%XPS  (THIS%NUM_POINTS), &
    THIS%XEXNA(THIS%NUM_POINTS), &
    THIS%XEXNS(THIS%NUM_POINTS), &
    THIS%PTA  (THIS%NUM_POINTS), &
    THIS%PQA  (THIS%NUM_POINTS), &
    THIS%PZREF(THIS%NUM_POINTS), &
    THIS%PUREF(THIS%NUM_POINTS)  )

  THIS%THICKNESS(1:THIS%NUM_POINTS) => THIS%Z(:, THIS%NUM_LAYERS)

  IF (LHOOK) CALL DR_HOOK('ICE_SICE:ALLOCA', 1, ZHOOK_HANDLE)
END SUBROUTINE ALLOCA

SUBROUTINE REGRID(THIS, PZNEW)
IMPLICIT NONE
  CLASS( SICE_t ) :: THIS
  REAL, OPTIONAL, INTENT(IN) :: PZNEW(:)

  REAL    :: Z_NEW_THK( THIS%NUM_POINTS )
  INTEGER :: JI, JN, INUM_ICE_LAYERS
  REAL    :: ZSKIN

  REAL, DIMENSION( THIS%NUM_POINTS, THIS%NUM_LAYERS ) :: &
      ZNEW_Z,        &
      ZNEW_DZ,       &
      ZNEW_DIFF

  REAL( KIND = JPRB ) :: ZHOOK_HANDLE

  IF( LHOOK ) CALL DR_HOOK( 'ICE_SICE:REGRID', 0, ZHOOK_HANDLE )

  Z_NEW_THK(:) = THIS%CONFIG%XNEW_ICE_THK
  IF(PRESENT(PZNEW)) THEN
      Z_NEW_THK(:SIZE(PZNEW)) = PZNEW(:)
  END IF

  INUM_ICE_LAYERS = THIS%NUM_LAYERS
  DO JI = 1, THIS%NUM_POINTS
    IF( Z_NEW_THK(JI) > 0.2 ) THEN
      ZSKIN = 0.05
    ELSE
      ZSKIN = Z_NEW_THK(JI)*0.05/0.2
    END IF
    ZSKIN = MIN( ZSKIN, (Z_NEW_THK(JI) - ZSKIN)/( INUM_ICE_LAYERS - 1.0 ) )
    ! Linear distribution of N-1 first layer thicknesses
    CALL LIN_SPACE( ZSKIN, Z_NEW_THK(JI),                   &
                    [( REAL(JN), JN = 1, INUM_ICE_LAYERS )],&
                    ZNEW_Z( JI, : )                         &
                  )
  END DO

  CALL SET_GRID(ZSKIN, THIS%CONFIG%XNEW_ICE_THK, ZNEW_Z(:,:), ZNEW_DZ(:,:), ZNEW_DIFF(:,:))

  THIS%Z     (:,:) = ZNEW_Z   (:,:)
  THIS%DZ    (:,:) = ZNEW_DZ  (:,:)
  THIS%Z_DIFF(:,:) = ZNEW_DIFF(:,:)

  IF( LHOOK ) CALL DR_HOOK( 'ICE_SICE:REGRID', 1, ZHOOK_HANDLE )
END SUBROUTINE REGRID

SUBROUTINE GET_MODEL_FIELDS( THIS, MF )
IMPLICIT NONE
  CLASS(SICE_t), INTENT(IN) :: THIS
  TYPE( MODEL_FIELD ), ALLOCATABLE, INTENT( OUT ) :: MF(:)

  INTEGER, PARAMETER :: NUM_FIELDS = 6

  TYPE( MODEL_FIELD ), ALLOCATABLE :: SNOW_FIELDS(:)
  REAL( KIND = JPRB )  :: ZHOOK_HANDLE

  IF( LHOOK ) CALL DR_HOOK( 'ICE_SICE:GET_MODEL_FIELDS', 0, ZHOOK_HANDLE )

  IF(ASSOCIATED(THIS%SNOW)) THEN
    CALL THIS%SNOW%GET_MODEL_FIELDS( SNOW_FIELDS )
  ELSE
    ALLOCATE( SNOW_FIELDS(0) )
  END IF
  ALLOCATE( MF(NUM_FIELDS + SIZE(SNOW_FIELDS)) )

  MF(:NUM_FIELDS) = [MODEL_FIELD(                                       &
      'TICE',                                                           &
      'Ice temperature',                                                &
      'K',                                                              &
      [THIS%NUM_POINTS, THIS%NUM_LAYERS],                               &
      P2 = THIS%T                                                       &
    ),                                                                  &
    MODEL_FIELD(                                                        &
      'ICE_AGE',                                                        &
      'Ice age',                                                        &
      's',                                                              &
      [THIS%NUM_POINTS, 0],                                             &
      P1 = THIS%AGE,                                                    &
      XDEFAULT = 0.                                                     &
    ),                                                                  &
    MODEL_FIELD(                                                        &
      'ICE_THK',                                                        &
      'Ice thicknesses',                                                &
      'm',                                                              &
      [THIS%NUM_POINTS, 0],                                             &
      P1 = THIS%THICKNESS,                                              &
      LDEPENDENT = .TRUE.                                               &
    ),                                                                  &
    MODEL_FIELD( NCONFIG=[0,0], P2 = THIS%DZ,     LINTERNAL = .TRUE. ), &
    MODEL_FIELD( NCONFIG=[0,0], P2 = THIS%Z_DIFF, LINTERNAL = .TRUE. ), &
    MODEL_FIELD( NCONFIG=[0,0], P2 = THIS%Z,      LINTERNAL = .TRUE. )  ]

  IF(SIZE(SNOW_FIELDS) > 0) THEN
    MF(NUM_FIELDS + 1 : NUM_FIELDS + SIZE(SNOW_FIELDS)) = SNOW_FIELDS(:)
  END IF

  DEALLOCATE( SNOW_FIELDS )
  IF( LHOOK ) CALL DR_HOOK( 'ICE_SICE:GET_MODEL_FIELDS', 1, ZHOOK_HANDLE )
END SUBROUTINE GET_MODEL_FIELDS

SUBROUTINE LIN_SPACE( PA, PB, PX, PY )
IMPLICIT NONE
  REAL, INTENT( IN  ) :: PA, &        !< Lower boundary value
                         PB           !< Upper boundary value
  REAL, INTENT( IN  ) :: PX(:       ) !< Grid
  REAL, INTENT( OUT ) :: PY(SIZE(PX)) !< Interpolated values in gridpoints

  REAL                :: ZK
  INTEGER             :: I, N

  REAL( KIND = JPRB ) :: ZHOOK_HANDLE

  IF( LHOOK ) CALL DR_HOOK( 'ICE_SICE:LIN_SPACE', 0, ZHOOK_HANDLE )

  N = SIZE(PX)

  ZK = (PB - PA)/(PX( N ) - PX( 1 ))

  DO I = 1, N
      PY( I ) = ZK*(PX( I ) - PX( 1 )) + PA
  END DO

  IF( LHOOK ) CALL DR_HOOK( 'ICE_SICE:LIN_SPACE', 1, ZHOOK_HANDLE )
END SUBROUTINE LIN_SPACE

SUBROUTINE SET_GRID( PZMIN, PZMAX, PZ, PDZ, PZ_DIFF )
USE MODD_SURF_PAR, ONLY: XUNDEF
IMPLICIT NONE
  REAL, INTENT( IN  )    :: PZMIN,    & !< Skin layer depth, [m]
                            PZMAX       !< Total depth of vertical grid, [m]
  REAL, INTENT( IN OUT ) :: PZ( :, : )  !< Depth of lower boundary for each vertical layer, [m]
  REAL, DIMENSION(SIZE(PZ,1), SIZE(PZ,2)), INTENT( OUT ) :: &
                            PDZ, &      !< Thickness of each layer, [m]
                            PZ_DIFF     !< Distanse between consecutive layer middle points, [m]

  INTEGER :: JI, IN, INPOINTS, INLAYER

  REAL( KIND = JPRB ) :: ZHOOK_HANDLE

  IF( LHOOK ) CALL DR_HOOK( 'ICE_SICE:SET_GRID', 0, ZHOOK_HANDLE )

  INPOINTS = SIZE(PZ, 1)
  INLAYER  = SIZE(PZ, 2)

  IF( ABS(PZ(1, 1) - XUNDEF) < 1.E-2 ) THEN
    DO JI = 1, INPOINTS
      CALL LIN_SPACE(PZMIN, PZMAX, [( REAL(IN), IN = 1, INLAYER )], PZ( JI, : ))
    END DO
  END IF

  PDZ    ( :, 1 ) =     PZ( :, 1 )
  PZ_DIFF( :, 1 ) = 0.5*PZ( :, 2 )

  DO JI = 2, SIZE( PZ, 2 ) - 1
    PDZ    ( :, JI ) =       PZ( :, JI     ) - PZ( :, JI - 1 )
    PZ_DIFF( :, JI ) = 0.5*( PZ( :, JI + 1 ) - PZ( :, JI - 1 ) )
  END DO

  IN = INLAYER
  PDZ    ( :, IN ) =      PZ( :, IN ) - PZ( :, IN - 1 )
  PZ_DIFF( :, IN ) = 0.5*(PZ( :, IN ) - PZ( :, IN - 1 ))

  IF( LHOOK ) CALL DR_HOOK( 'ICE_SICE:SET_GRID', 1, ZHOOK_HANDLE )
END SUBROUTINE SET_GRID
END MODULE ICE_SICE
