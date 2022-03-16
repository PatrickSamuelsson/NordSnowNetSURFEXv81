MODULE ABSTRACT_ICE
USE MODD_DATA_COVER_n, ONLY: DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY: SURF_ATM_t
USE MODD_GRID_CONF_PROJ_n, ONLY: GRID_CONF_PROJ_t
USE MODE_PREP_CTL, ONLY: PREP_CTL
USE MODD_DIAG_MISC_SEAICE_n, ONLY: DIAG_MISC_SEAICE_t
USE YOMHOOK,   ONLY : LHOOK,   DR_HOOK
USE PARKIND1,  ONLY : JPRB
IMPLICIT NONE
PRIVATE

TYPE, PUBLIC :: ESM_CPL_t
  REAL, POINTER, DIMENSION(:) :: XCPL_SEA_WIND !< 10m wind speed for ESM coupling
  REAL, POINTER, DIMENSION(:) :: XCPL_SEA_FWSU !< zonal wind stress for ESM coupling
  REAL, POINTER, DIMENSION(:) :: XCPL_SEA_FWSV !< meridian wind stress for ESM coupling
  REAL, POINTER, DIMENSION(:) :: XCPL_SEA_SNET !< Solar net heat flux
  REAL, POINTER, DIMENSION(:) :: XCPL_SEA_HEAT !< Non solar net heat flux
  REAL, POINTER, DIMENSION(:) :: XCPL_SEA_EVAP !< Evaporation for ESM coupling
  REAL, POINTER, DIMENSION(:) :: XCPL_SEA_RAIN !< Rainfall for ESM coupling
  REAL, POINTER, DIMENSION(:) :: XCPL_SEA_SNOW !< Snowfall for ESM coupling
  REAL, POINTER, DIMENSION(:) :: XCPL_SEA_FWSM !< wind stress for ESM coupling
!
  REAL, POINTER, DIMENSION(:) :: XCPL_SEAICE_SNET !< Solar net heat flux for ESM coupling
  REAL, POINTER, DIMENSION(:) :: XCPL_SEAICE_HEAT !< Non solar net heat flux
  REAL, POINTER, DIMENSION(:) :: XCPL_SEAICE_EVAP !< Ice sublimation for ESM coupling
END TYPE

TYPE, PUBLIC, ABSTRACT :: SEA_ICE_t
    REAL, POINTER :: XSEABATHY(:) !< bathymetry
    REAL, POINTER :: XSST(:) !< sea surface temperature [K]
    REAL, POINTER :: XSSS(:) !< se surface salinity [K]

    LOGICAL :: LINTERPOL_SIC
    LOGICAL :: LINTERPOL_SIT
  CONTAINS
    PROCEDURE(IINIT), DEFERRED, PASS :: INIT
    PROCEDURE(IPREP), DEFERRED, PASS :: PREP
    PROCEDURE(IASSIM), DEFERRED, PASS :: ASSIM
    PROCEDURE(IRUN), DEFERRED, PASS :: RUN
    PROCEDURE(IDEALLOC), DEFERRED, PASS :: DEALLOC

    PROCEDURE(IIO_READ), DEFERRED, PASS :: READSURF
    PROCEDURE(IIO_WRITE), DEFERRED, PASS :: WRITESURF
    PROCEDURE(IIO_WRITE), DEFERRED, PASS :: WRITE_DIAG

    PROCEDURE(IRESPONSE), DEFERRED, PASS :: GET_RESPONSE
    PROCEDURE(IDIAG), DEFERRED, PASS :: DIAG_MISC

    PROCEDURE(IDAMPING), DEFERRED, PASS :: SET_DAMPING

    PROCEDURE, PASS :: BIND_INPUTS
    PROCEDURE, PASS :: COUPLING_ICEFLUX
END TYPE SEA_ICE_t

ABSTRACT INTERFACE
  SUBROUTINE IINIT(THIS, HPROGRAM)
    IMPORT :: SEA_ICE_t
    CLASS(SEA_ICE_t) :: THIS
    CHARACTER(LEN=6), INTENT(IN)  :: HPROGRAM
  END SUBROUTINE IINIT

  SUBROUTINE IPREP(THIS, DTCO, U, GCP, YDCTL, KLU, KLAT, &
                   HPROGRAM, HATMFILE, HATMFILETYPE, HPGDFILE, HPGDFILETYPE)
    IMPORT :: SEA_ICE_t, DATA_COVER_t, SURF_ATM_t, GRID_CONF_PROJ_t, PREP_CTL
    CLASS(SEA_ICE_t) :: THIS
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
  END SUBROUTINE IPREP

  SUBROUTINE IASSIM(THIS, HPROGRAM, PSIC_IN, PLON_IN, PLAT_IN)
    IMPORT :: SEA_ICE_t
    CLASS(SEA_ICE_t) :: THIS
    CHARACTER(LEN=6),   INTENT(IN) :: HPROGRAM  !< program calling surf. schemes
    REAL,               INTENT(IN) :: PSIC_IN(:)
    REAL,               INTENT(IN) :: PLON_IN(:)
    REAL,               INTENT(IN) :: PLAT_IN(:)
  END SUBROUTINE IASSIM

  SUBROUTINE IRUN( &
      THIS, HPROGRAM, PTIMEC, PTSTEP, KSTEP, ESM_CPL, PFSIC, PFSIT, PSI_FLX_DRV, PFREEZING_SST, &
      PZENITH, PSW_TOT, PLW, &
      PPEW_A_COEF, PPEW_B_COEF, PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF)
    IMPORT :: SEA_ICE_t, ESM_CPL_t
    CLASS(SEA_ICE_t) :: THIS
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
  END SUBROUTINE IRUN

  SUBROUTINE IDEALLOC(THIS)
    IMPORT :: SEA_ICE_t
    CLASS(SEA_ICE_t) :: THIS
  END SUBROUTINE IDEALLOC

  SUBROUTINE IIO_READ(THIS, G, HPROGRAM, KLU, KLUOUT)
    USE MODD_SFX_GRID_n, ONLY : GRID_t
    IMPORT :: SEA_ICE_t
    CLASS(SEA_ICE_t) :: THIS
    TYPE(GRID_t), INTENT(INOUT)   :: G
    CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM !< calling program
    INTEGER,           INTENT(IN) :: KLU      !< number of sea patch point
    INTEGER,           INTENT(IN) :: KLUOUT
  END SUBROUTINE IIO_READ

  SUBROUTINE IIO_WRITE(THIS, HSELECT, HPROGRAM)
    USE MODD_SFX_GRID_n, ONLY : GRID_t
    IMPORT :: SEA_ICE_t
    CLASS(SEA_ICE_t) :: THIS
    CHARACTER(LEN=*), INTENT(IN) :: HSELECT(:)
    CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM
  END SUBROUTINE IIO_WRITE

  SUBROUTINE IRESPONSE(THIS, PSIC, PTICE, PICE_ALB)
    IMPORT :: SEA_ICE_t
    CLASS(SEA_ICE_t) :: THIS
    REAL, INTENT(OUT) :: PSIC(:)
    REAL, INTENT(OUT) :: PTICE(:)
    REAL, INTENT(OUT) :: PICE_ALB(:)
  END SUBROUTINE IRESPONSE

  SUBROUTINE IDIAG(THIS, DGMSI)
    IMPORT :: SEA_ICE_t, DIAG_MISC_SEAICE_t
    CLASS(SEA_ICE_t) :: THIS
    TYPE(DIAG_MISC_SEAICE_t), INTENT(IN OUT) :: DGMSI
  END SUBROUTINE IDIAG

  SUBROUTINE IDAMPING(THIS, PSIC_EFOLDING_TIME, PSIT_EFOLDING_TIME)
    IMPORT :: SEA_ICE_t
    CLASS(SEA_ICE_t) :: THIS
    REAL, INTENT(IN) :: PSIC_EFOLDING_TIME
    REAL, INTENT(IN) :: PSIT_EFOLDING_TIME
  END SUBROUTINE IDAMPING
END INTERFACE

CONTAINS

SUBROUTINE BIND_INPUTS(THIS, PSEABATHY, PSST, PSSS)
  IMPLICIT NONE
  CLASS(SEA_ICE_t) :: THIS
  REAL, POINTER, INTENT(IN) :: PSEABATHY(:)
  REAL, POINTER, INTENT(IN) :: PSST(:)
  REAL, POINTER, INTENT(IN) :: PSSS(:)
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('ABSTRACT_ICE:BIND_INPUTS', 0, ZHOOK_HANDLE)

  THIS%XSEABATHY => PSEABATHY
  THIS%XSSS => PSSS
  THIS%XSST => PSST

  IF (LHOOK) CALL DR_HOOK('ABSTRACT_ICE:BIND_INPUTS', 1, ZHOOK_HANDLE)
END SUBROUTINE

SUBROUTINE COUPLING_ICEFLUX(THIS, KI, PTA, PEXNA, PRHOA, PTICE, PEXNS,   &
                                PQA, PRAIN, PSNOW, PWIND, PZREF, PUREF,  &
                                PPS, PTWAT, PTTS, PSFTH, PSFTQ, AT,      &
                                OHANDLE_SIC, PMASK, PQSAT, PZ0,          &
                                PUSTAR, PCD, PCDN, PCH,                  &
                                PRI, PRESA, PZ0H )
USE MODD_SURF_ATM_TURB_n, ONLY: SURF_ATM_TURB_t
USE MODI_COUPLING_ICEFLUX_n
IMPLICIT NONE
  CLASS(SEA_ICE_t) :: THIS !< Ice model

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

  IF (LHOOK) CALL DR_HOOK('ABSTRACT_ICE:COUPLING_ICEFLUX', 0, ZHOOK_HANDLE)

  CALL COUPLING_ICEFLUX_n(KI, PTA, PEXNA, PRHOA, PTICE, PEXNS,     &
                          PQA, PRAIN, PSNOW, PWIND, PZREF, PUREF,  &
                          PPS, PTWAT, PTTS, PSFTH, PSFTQ, AT,      &
                          OHANDLE_SIC, PMASK, PQSAT, PZ0,          &
                          PUSTAR, PCD, PCDN, PCH,                  &
                          PRI, PRESA, PZ0H )

  IF (LHOOK) CALL DR_HOOK('ABSTRACT_ICE:BIND_INPUTS', 1, ZHOOK_HANDLE)
END SUBROUTINE COUPLING_ICEFLUX
END MODULE ABSTRACT_ICE
