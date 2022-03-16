!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
! *****************************************************************************************
MODULE MODE_ASSIM_READ_FIELD_FA

INTERFACE ASSIM_READ_FIELD_FA
  MODULE PROCEDURE ASSIM_READ_FIELD_FA
END INTERFACE

CONTAINS

SUBROUTINE ASSIM_READ_FIELD_FA(DTCO, U, HMASK, HREC, PFIELD)

USE MODD_DATA_COVER_n,    ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n,      ONLY : SURF_ATM_t
USE MODD_TYPE_DATE_SURF,  ONLY : DATE_TIME

USE YOMHOOK,              ONLY : LHOOK,DR_HOOK
USE PARKIND1,             ONLY : JPRB
USE MODI_ABOR1_SFX

IMPLICIT NONE
!
!*      0.1    declarations of arguments

TYPE(DATA_COVER_t),    INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t),      INTENT(INOUT) :: U
CHARACTER(LEN=6),      INTENT(IN)    :: HMASK
CHARACTER(LEN=*),      INTENT(IN)    :: HREC
REAL,DIMENSION(:),     INTENT(OUT)   :: PFIELD

REAL,DIMENSION(:),ALLOCATABLE        :: ZWORK
REAL(KIND=JPRB)                      :: ZHOOK_HANDLE
INTEGER                              :: JI

IF (LHOOK) CALL DR_HOOK('ASSIM_READ_FIELD_FA',0,ZHOOK_HANDLE)

ALLOCATE(ZWORK(U%NSIZE_FULL))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Define where to read an input variable
! The name is set in the called routines
! based on settings
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SELECT CASE (TRIM(HREC))
  CASE("CON_RAIN","STRAT_RAIN","CON_SNOW","STRAT_SNOW","CLOUDS","LSM","EVAP","EVAPTR","U10M","V10M")
    CALL ASSIM_READ_FIELD_FA_FG(DTCO,U,HREC,ZWORK)
  CASE("T2M","HU2M","SWE","TS")  
    CALL ASSIM_READ_FIELD_FA_CANARI(DTCO,U,HREC,ZWORK)
  CASE("SWEC","TSC")
    CALL ASSIM_READ_FIELD_FA_CLIM(DTCO,U,HREC,ZWORK)
  CASE("SST","SIC")
    CALL ASSIM_READ_FIELD_FA_SST(DTCO,U,HREC,ZWORK)
  CASE DEFAULT
    CALL ABOR1_SFX("This record is not implemented in FA reading: "//TRIM(HREC))
END SELECT

! Deal with masks
IF ( TRIM(HMASK) == "SEA" ) THEN
  IF ( U%NSIZE_SEA /= SIZE(PFIELD) ) CALL ABOR1_SFX("Mismatch in dimensions for sea mask and input field!")
  DO JI = 1,U%NSIZE_SEA
    PFIELD(JI)=ZWORK(U%NR_SEA(JI))
  ENDDO
ELSEIF ( TRIM(HMASK) == "NATURE" ) THEN
  IF ( U%NSIZE_NATURE /= SIZE(PFIELD) ) CALL ABOR1_SFX("Mismatch in dimensions for nature mask and input field!")
  DO JI = 1,U%NSIZE_NATURE
    PFIELD(JI)=ZWORK(U%NR_NATURE(JI))
  ENDDO
ELSEIF ( TRIM(HMASK) == "WATER" ) THEN
  IF ( U%NSIZE_WATER /= SIZE(PFIELD) ) CALL ABOR1_SFX("Mismatch in dimensions for water mask and input field!")
  DO JI = 1,U%NSIZE_WATER
    PFIELD(JI)=ZWORK(U%NR_WATER(JI))
  ENDDO
ELSEIF ( TRIM(HMASK) == "TOWN" ) THEN
  IF ( U%NSIZE_TOWN /= SIZE(PFIELD) ) CALL ABOR1_SFX("Mismatch in dimensions for town mask and input field!")
  DO JI = 1,U%NSIZE_TOWN
    PFIELD(JI)=ZWORK(U%NR_TOWN(JI))
  ENDDO
ELSEIF ( TRIM(HMASK) == "FULL" ) THEN
  PFIELD(:)=ZWORK(:)
ELSE
  CALL ABOR1_SFX("Mask "//TRIM(HMASK)//" not defined!")
ENDIF
DEALLOCATE(ZWORK)

IF (LHOOK) CALL DR_HOOK('ASSIM_READ_FIELD_FA',1,ZHOOK_HANDLE)

END SUBROUTINE ASSIM_READ_FIELD_FA

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! The actual reading of a FA name in a file
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE ASSIM_READ_FIELD_FA_NAME(DTCO,U,HNAME,PFIELD)
  USE MODD_DATA_COVER_n,    ONLY : DATA_COVER_t
  USE MODD_SURF_ATM_n,      ONLY : SURF_ATM_t
#ifdef SFX_FA
  USE MODD_IO_SURF_FA,      ONLY : CFILEIN_FA, CDNOMC
#endif
  USE MODI_READ_SURF
  USE MODI_INIT_IO_SURF_n
  USE MODI_END_IO_SURF_n
  IMPLICIT NONE
  TYPE(DATA_COVER_t),INTENT(INOUT) :: DTCO
  TYPE(SURF_ATM_t),  INTENT(INOUT) :: U
  CHARACTER(LEN=28), INTENT(IN)    :: HNAME
  REAL,DIMENSION(:), INTENT(OUT)   :: PFIELD
  CHARACTER(LEN=6)                 :: YPROGRAM="FA"
  INTEGER                          :: IRESP

#ifdef SFX_FA
  WRITE(*,*) 'READING '//TRIM(HNAME)//' IN '//TRIM(CFILEIN_FA)//' CADRE IS '//TRIM(CDNOMC)
  !  Open FA file (LAM version with extension zone)
  CALL INIT_IO_SURF_n(DTCO, U, YPROGRAM,'EXTZON','SURF  ','READ ')

  !  Read model forecast quantities
  CALL READ_SURF(YPROGRAM,HNAME,PFIELD,IRESP) ! accumulated fluxes (not available in LFI)

  !  Close FA file
  CALL END_IO_SURF_n(YPROGRAM)
  CALL IO_BUFF_CLEAN
#else
    CALL ABOR1_SFX("You must compile with FA support enabled to read a FA file: -DSFX_FA")
#endif

END SUBROUTINE ASSIM_READ_FIELD_FA_NAME

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Read field from the atmospheric first guess used in OI
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE ASSIM_READ_FIELD_FA_FG(DTCO,U,HREC,PFIELD)
  USE MODD_DATA_COVER_n,    ONLY : DATA_COVER_t
  USE MODD_SURF_ATM_n,      ONLY : SURF_ATM_t
#ifdef SFX_FA
  USE MODD_IO_SURF_FA,      ONLY : CFILEIN_FA, CDNOMC
#endif
  USE MODD_ASSIM,           ONLY : LAROME,LALADSURF
  IMPLICIT NONE
  TYPE(DATA_COVER_t),INTENT(INOUT) :: DTCO
  TYPE(SURF_ATM_t),  INTENT(INOUT) :: U
  CHARACTER(LEN=*),  INTENT(IN)    :: HREC
  REAL,DIMENSION(:), INTENT(OUT)   :: PFIELD
  CHARACTER(LEN=28)                :: YNAME

#ifdef SFX_FA
  CFILEIN_FA = 'FG_OI_MAIN'
  CDNOMC     = 'oimain'
#endif

  YNAME=''
  SELECT CASE (TRIM(HREC))
    CASE("CON_RAIN")
      IF ( .NOT. LAROME ) THEN
        YNAME    = 'SURFPREC.EAU.CON'
      ENDIF
    CASE("STRAT_RAIN")
      IF ( LAROME ) THEN
        YNAME    = 'SURFACCPLUIE'
      ELSE
        YNAME    = 'SURFPREC.EAU.GEC'
      ENDIF
    CASE("CON_SNOW")
      IF ( LAROME ) THEN
        YNAME    = 'SURFACCGRAUPEL'
      ELSE
        YNAME    = 'SURFPREC.NEI.CON'
      ENDIF
    CASE("STRAT_SNOW")
      IF ( LAROME ) THEN
        YNAME    = 'SURFACCNEIGE'
      ELSE
        YNAME    = 'SURFPREC.NEI.GEC'
      ENDIF
    CASE("CLOUDS")
      YNAME      = 'ATMONEBUL.BASSE'
    CASE("LSM")
      YNAME      = 'SURFIND.TERREMER'
    CASE("EVAP")
      YNAME      = 'SURFFLU.LAT.MEVA'
    CASE("EVAPTR")
      IF (.NOT.LALADSURF) THEN
        YNAME    = 'SURFXEVAPOTRANSP'
      ENDIF
    CASE("U10M")
      YNAME='CLSVENT.ZONAL'
    CASE("V10M")
      YNAME='CLSVENT.MERIDIEN'
    CASE DEFAULT
      CALL ABOR1_SFX("This record is not implemented in FA FG reading: "//TRIM(HREC))
  END SELECT
  IF ( TRIM(YNAME) /= '' ) THEN
    CALL ASSIM_READ_FIELD_FA_NAME(DTCO,U,YNAME,PFIELD)
  ELSE
    PFIELD(:)=0.
  ENDIF
END SUBROUTINE ASSIM_READ_FIELD_FA_FG

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Read fields taken from CANARI
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE ASSIM_READ_FIELD_FA_CANARI(DTCO,U,HREC,PFIELD)
  USE MODD_DATA_COVER_n,    ONLY : DATA_COVER_t
  USE MODD_SURF_ATM_n,      ONLY : SURF_ATM_t
#ifdef SFX_FA
  USE MODD_IO_SURF_FA,      ONLY : CFILEIN_FA, CDNOMC
#endif
  IMPLICIT NONE
  TYPE(DATA_COVER_t),INTENT(INOUT) :: DTCO
  TYPE(SURF_ATM_t),  INTENT(INOUT) :: U
  CHARACTER(LEN=*),  INTENT(IN)    :: HREC
  REAL,DIMENSION(:), INTENT(OUT)   :: PFIELD
  CHARACTER(LEN=28)                :: YNAME

#ifdef SFX_FA
  CFILEIN_FA = 'CANARI'        ! input CANARI analysis
  CDNOMC     = 'canari'        ! new frame name
#endif

  YNAME=''
  SELECT CASE (TRIM(HREC))
    CASE("T2M")
      YNAME='CLSTEMPERATURE'
    CASE("HU2M")
      YNAME='CLSHUMI.RELATIVE'
    CASE("TS")
      YNAME='SURFTEMPERATURE'
    CASE("SWE")
      YNAME='SURFRESERV.NEIGE'
    CASE("U10M")
      YNAME='CLSVENT.ZONAL'
    CASE("V10M")
      YNAME='CLSVENT.MERIDIEN'
    CASE DEFAULT
      CALL ABOR1_SFX("This record is not implemented in FA CANARI reading: "//TRIM(HREC))
  END SELECT
  CALL ASSIM_READ_FIELD_FA_NAME(DTCO,U,YNAME,PFIELD)

END SUBROUTINE ASSIM_READ_FIELD_FA_CANARI

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Read cliamte field used in OI
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE ASSIM_READ_FIELD_FA_CLIM(DTCO,U,HREC,PFIELD)
  USE MODD_DATA_COVER_n,    ONLY : DATA_COVER_t
  USE MODD_SURF_ATM_n,      ONLY : SURF_ATM_t
#ifdef SFX_FA
  USE MODD_IO_SURF_FA,      ONLY : CFILEIN_FA, CDNOMC
#endif
  IMPLICIT NONE
  TYPE(DATA_COVER_t),INTENT(INOUT) :: DTCO
  TYPE(SURF_ATM_t),  INTENT(INOUT) :: U
  CHARACTER(LEN=*),  INTENT(IN)    :: HREC
  REAL,DIMENSION(:), INTENT(OUT)   :: PFIELD
  CHARACTER(LEN=28)                :: YNAME

#ifdef SFX_FA
  CFILEIN_FA = 'clim_isba'               ! input climatology
  CDNOMC     = 'climat'                  ! new frame name
#endif

  YNAME=''
  SELECT CASE (TRIM(HREC))
    CASE("SWEC")
      YNAME='SURFRESERV.NEIGE'
    CASE("TSC")
      YNAME='SURFTEMPERATURE'
    CASE DEFAULT
      CALL ABOR1_SFX("This record is not implemented in FA CLIM reading: "//TRIM(HREC))
  END SELECT

  CALL ASSIM_READ_FIELD_FA_NAME(DTCO,U,YNAME,PFIELD)

END SUBROUTINE ASSIM_READ_FIELD_FA_CLIM

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Read SST and SIC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE ASSIM_READ_FIELD_FA_SST(DTCO,U,HREC,PFIELD)
  USE MODD_DATA_COVER_n,    ONLY : DATA_COVER_t
  USE MODD_SURF_ATM_n,      ONLY : SURF_ATM_t
#ifdef SFX_FA
  USE MODD_IO_SURF_FA,      ONLY : CFILEIN_FA, CDNOMC
#endif
  USE MODD_ASSIM,           ONLY : NPRINTLEV,LECSST,LPIO
  USE MODD_SURF_PAR,        ONLY : XUNDEF

  IMPLICIT NONE
  TYPE(DATA_COVER_t),INTENT(INOUT) :: DTCO
  TYPE(SURF_ATM_t),  INTENT(INOUT) :: U
  CHARACTER(LEN=*),  INTENT(IN)    :: HREC
  REAL,DIMENSION(:), INTENT(OUT)   :: PFIELD

  REAL,DIMENSION(:),ALLOCATABLE    :: ZITM
  CHARACTER(LEN=28)                :: YNAME
  REAL                             :: ZFMAX, ZFMIN, ZFMEAN

  !  Read SST from boundaries when SST analysis NOT is performed in CANARI
  !  Define FA file name for SST analysis interpolated from boundary file 

#ifdef SFX_FA
  CFILEIN_FA = 'SST_SIC'               ! input SST/SIC file
  CDNOMC     = 'CADRE SST'             ! new frame name
#endif

  YNAME=''
  SELECT CASE (TRIM(HREC))
    CASE ('SST')
      IF ( LECSST ) THEN
        YNAME="SURFSEA.TEMPERA"
      ELSE
        YNAME="SURFTEMPERATURE"
      ENDIF
    CASE('SIC')
      IF ( LECSST ) THEN
        YNAME="SURFSEA.ICECONC"
      ENDIF
    CASE DEFAULT
      CALL ABOR1_SFX("This record is not implemented in FA SST/SIC reading: "//TRIM(HREC))
  END SELECT

  IF ( TRIM(YNAME) /= '' ) THEN
    CALL ASSIM_READ_FIELD_FA_NAME(DTCO,U,YNAME,PFIELD)
  ELSE
    PFIELD(:)=0.
  ENDIF
    
  SELECT CASE (TRIM(HREC))
    CASE ('SST')

      ZFMIN = MINVAL(PFIELD)
      ZFMAX = MAXVAL(PFIELD)
      IF ( U%NSIZE_FULL > 0 ) THEN
        ZFMEAN = SUM(PFIELD)/FLOAT(U%NSIZE_FULL)
      ELSE
        ZFMEAN=XUNDEF
      ENDIF

      IF ( LECSST ) THEN

        IF (LPIO .AND. NPRINTLEV>0) THEN
          WRITE(*,*) '  ECMWF_SST_SIC'
          WRITE(*,'("  SURFSEA.TEMPERA - min, mean, max: ",3E13.4)') ZFMIN, ZFMEAN, ZFMAX
        ENDIF

        ! Replace -9999. with UNDEF
        WHERE ( PFIELD(:)< 0. )
          PFIELD(:) = XUNDEF
        ENDWHERE

      ELSE

        IF (LPIO .AND. NPRINTLEV>0) THEN
          WRITE(*,*) '  Boundary file'
          WRITE(*,'("  SURFTEMPERATURE - min, mean, max: ",3E13.4)') ZFMIN, ZFMEAN, ZFMAX
        ENDIF
        ALLOCATE(ZITM(U%NSIZE_FULL))
        YNAME      = 'SURFIND.TERREMER'
        CALL ASSIM_READ_FIELD_FA_NAME(DTCO,U,YNAME,ZITM)

        ! To avoid surface temperatures influenced by land, NATURE points are replaced with UNDEF
        WHERE (ZITM(:)>0.5 )
          PFIELD(:) = XUNDEF
        ENDWHERE
        DEALLOCATE(ZITM)
      ENDIF

      ZFMIN = MINVAL(PFIELD)
      ZFMAX = MAXVAL(PFIELD)
      IF ( U%NSIZE_FULL > 0 ) THEN
        ZFMEAN = SUM(PFIELD)/FLOAT(U%NSIZE_FULL)
      ELSE
        ZFMEAN=XUNDEF
      ENDIF

      IF (LPIO .AND. NPRINTLEV>0) THEN
        WRITE(*,*) '  Replaced land by UNDEF '
        WRITE(*,'("  SST            - min, mean, max: ",3E13.4)') ZFMIN, ZFMEAN, ZFMAX
      ENDIF
  END SELECT
END SUBROUTINE ASSIM_READ_FIELD_FA_SST

END MODULE MODE_ASSIM_READ_FIELD_FA
