!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################################################
MODULE MODI_ASSIM_READ_FIELD
  INTERFACE 
    SUBROUTINE ASSIM_READ_FIELD (DTCO, U, UG, USS, TPTIME, HFILEFORMAT, HMASK, HREC, PFIELD)
      USE MODD_DATA_COVER_n,    ONLY : DATA_COVER_t
      USE MODD_SURF_ATM_n,      ONLY : SURF_ATM_t
      USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
      USE MODD_SSO_n,           ONLY : SSO_t
      USE MODD_TYPE_DATE_SURF,  ONLY : DATE_TIME

      TYPE(DATA_COVER_t),      INTENT(INOUT) :: DTCO
      TYPE(SURF_ATM_t),        INTENT(INOUT) :: U
      TYPE(SURF_ATM_GRID_t),   INTENT(INOUT) :: UG
      TYPE(SSO_t),             INTENT(INOUT) :: USS
      TYPE(DATE_TIME),         INTENT(IN)    :: TPTIME
      CHARACTER(LEN=6),        INTENT(IN)    :: HFILEFORMAT
      CHARACTER(LEN=6),        INTENT(IN)    :: HMASK
      CHARACTER(LEN=*),        INTENT(IN)    :: HREC
      REAL,DIMENSION(:),       INTENT(OUT)   :: PFIELD
    END SUBROUTINE ASSIM_READ_FIELD
  END INTERFACE
END MODULE

SUBROUTINE ASSIM_READ_FIELD (DTCO, U, UG, USS, TPTIME, HFILEFORMAT, HMASK, HREC, PFIELD)

!     ###############################################################################
!
!!****  *ASSIM_READ_FIELD * - Reads a field for assimilation from file
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     T. Aspelien
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    12/2018
!!--------------------------------------------------------------------
!
USE MODD_DATA_COVER_n,    ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n,      ONLY : SURF_ATM_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SSO_n,           ONLY : SSO_t
USE MODD_TYPE_DATE_SURF,  ONLY : DATE_TIME
USE YOMHOOK,              ONLY : LHOOK,DR_HOOK
USE PARKIND1,             ONLY : JPRB

USE MODI_ABOR1_SFX
USE MODE_ASSIM_READ_FIELD_ASCLLV, ONLY : ASSIM_READ_FIELD_ASCLLV
USE MODE_ASSIM_READ_FIELD_ASCII,  ONLY : ASSIM_READ_FIELD_ASCII
USE MODE_ASSIM_READ_FIELD_FA,     ONLY : ASSIM_READ_FIELD_FA

IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!
TYPE(DATA_COVER_t),      INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t),        INTENT(INOUT) :: U
TYPE(SURF_ATM_GRID_t),   INTENT(INOUT) :: UG
TYPE(SSO_t),             INTENT(INOUT) :: USS
TYPE(DATE_TIME),         INTENT(IN)    :: TPTIME
CHARACTER(LEN=6),        INTENT(IN)    :: HFILEFORMAT
CHARACTER(LEN=6),        INTENT(IN)    :: HMASK
CHARACTER(LEN=*),        INTENT(IN)    :: HREC
REAL,DIMENSION(:),       INTENT(OUT)   :: PFIELD
REAL(KIND=JPRB)                        :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('ASSIM_READ_FIELD',0,ZHOOK_HANDLE)

IF ( TRIM(HFILEFORMAT) == "ASCLLV" ) THEN
  CALL ASSIM_READ_FIELD_ASCLLV(DTCO, U, UG, USS, HMASK, HREC, PFIELD)
ELSEIF ( TRIM(HFILEFORMAT) == "ASCII" ) THEN
  CALL ASSIM_READ_FIELD_ASCII(U, UG, TPTIME, HMASK, HREC, PFIELD)
ELSEIF ( TRIM(HFILEFORMAT) == "FA" ) THEN
#ifdef SFX_FA
  CALL ASSIM_READ_FIELD_FA(DTCO, U, HMASK, HREC, PFIELD)
#else
  CALL ABOR1_SFX("You are trying to read a FA file but you have not compiled with -DSFX_FA")
#endif
ELSE
  CALL ABOR1_SFX("HFILEFORMAT="//TRIM(HFILEFORMAT)//" not implemented!")
ENDIF

IF (LHOOK) CALL DR_HOOK('ASSIM_READ_FIELD',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE ASSIM_READ_FIELD
