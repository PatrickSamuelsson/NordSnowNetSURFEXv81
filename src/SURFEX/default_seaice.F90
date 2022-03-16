!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE DEFAULT_SEAICE(HPROGRAM,                                   &
                          HINTERPOL_SIC, HINTERPOL_SIT,               &
                          HCONSTRAIN_CSV,PFREEZING_SST,               &
                          PSEAICE_TSTEP, PSIC_EFOLDING_TIME,          &
                          PSIT_EFOLDING_TIME, PCD_ICE, PSI_FLX_DRV,   &
                          OVOLATILE_SIC    )
!     ########################################################################
!
!!****  *DEFAULT_SEAICE* - routine to set default values for the configuration for SEAICE scheme
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!       For now, only Gelato seaice scheme is handled
!!
!!       We do use MODD_GLT_PARAM, for modifying its values, in order to
!!       avoid duplicating code with Gelato sources
!!
!!       We set all its parameters to values which are sensible in Surfex context
!!       This is done by inserting a relevant 'gltpar' file as source code, and
!!       changing a few values (we used a Glt 6.0.36 version, initially)
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      S.Senesi   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2014
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling ISBA
CHARACTER(LEN=6),  INTENT(OUT) :: HINTERPOL_SIC ! Quadratic interpolation of monthly SIC
CHARACTER(LEN=6),  INTENT(OUT) :: HINTERPOL_SIT ! Quadratic interpolation of monthly SIT
CHARACTER(LEN=6),  INTENT(OUT) :: HCONSTRAIN_CSV ! Conserved variable if constraint
REAL,              INTENT(OUT) :: PFREEZING_SST ! Value marking frozen sea in SST data
REAL,              INTENT(OUT) :: PSEAICE_TSTEP ! For damping of SIC (days)
REAL,              INTENT(OUT) :: PSIC_EFOLDING_TIME ! E-folding time on SIC relaxation
REAL,              INTENT(OUT) :: PSIT_EFOLDING_TIME ! E-folding time on SIT relaxation
REAL,              INTENT(OUT) :: PCD_ICE       ! turbulent exchanges transfer coefficient on seaice
REAL,              INTENT(OUT) :: PSI_FLX_DRV   ! turbulent exchanges transfer coefficient on seaice
LOGICAL,           INTENT(OUT) :: OVOLATILE_SIC ! could SIC be updated outside the sea-ice scheme?

!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_SEAICE',0,ZHOOK_HANDLE)
!
HINTERPOL_SIC = "NONE"
HINTERPOL_SIT = "NONE"
HCONSTRAIN_CSV = "VOLUME"
PFREEZING_SST = -1.8 ! Celsius degree
PSEAICE_TSTEP = XUNDEF
PSIC_EFOLDING_TIME = 0 ! in days; 0 means no relaxation
PSIT_EFOLDING_TIME = 0 ! in days; 0 means no relaxation
PCD_ICE       = 0.0
PSI_FLX_DRV   = -20.
OVOLATILE_SIC = .FALSE.
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_SEAICE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_SEAICE
