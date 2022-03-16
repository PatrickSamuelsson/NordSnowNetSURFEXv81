!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_HOR_FLAKE_FIELD (DTCO, UG, U, USS, GCP, KLAT, F, &
                                 HPROGRAM,HSURF, &
                                 YFILE,YFILETYPE, &
                                 YFILEPGD, YFILEPGDTYPE, &
                                 GUNIF,GINTERP)
!     #################################################################################
!
!!****  *PREP_HOR_FLAKE_FIELD* - Reads, interpolates and prepares a water field
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     S. Malardel
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      P. Le Moigne 10/2005, Phasage Arome
!!      E. Kourzeneva 09/2010, Make possible to interpolate 
!!                             only lake surface temperature, 
!!                             but not profiles
!!------------------------------------------------------------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODD_FLAKE_n, ONLY : FLAKE_t
!
USE MODD_SURF_PAR,     ONLY : XUNDEF
USE MODD_TYPE_DATE_SURF, ONLY : DATE_TIME
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, NCOMM, NPROC
USE MODD_PREP,         ONLY : CINGRID_TYPE, CINTERP_TYPE, XZS_LS, CMASK
USE MODD_GRID_GRIB, ONLY : CINMODEL 
!
USE MODD_CSTS,       ONLY : XTT
USE MODD_PREP_FLAKE, ONLY : LCLIM_LAKE
!
USE MODI_PREP_GRIB_GRID
USE MODI_READ_PREP_FLAKE_CONF
USE MODI_PREP_FLAKE_GRIB
USE MODI_PREP_FLAKE_ASCLLV
USE MODI_PREP_FLAKE_UNIF
USE MODI_PREP_FLAKE_BUFFER
USE MODI_HOR_INTERPOL
USE MODI_GET_LUOUT
USE MODI_PREP_FLAKE_EXTERN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*      0.1    declarations of arguments
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
INTEGER, INTENT(IN) :: KLAT
TYPE(FLAKE_t), INTENT(INOUT) :: F
!
CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
CHARACTER(LEN=7),    INTENT(IN)  :: HSURF     ! type of field
CHARACTER(LEN=6),    INTENT(IN)  :: YFILETYPE ! type of input file
CHARACTER(LEN=28),   INTENT(IN)  :: YFILE     ! name of file
CHARACTER(LEN=6),    INTENT(IN)  :: YFILEPGDTYPE ! type of input file
CHARACTER(LEN=28),   INTENT(IN)  :: YFILEPGD     ! name of file
LOGICAL, INTENT(IN) :: GUNIF ! if the unified field is given
LOGICAL, INTENT(OUT) :: GINTERP ! if interpolation was done
!
!
!*      0.2    declarations of local variables
!
TYPE (DATE_TIME)                :: TZTIME_GRIB    ! current date and time 
REAL, POINTER, DIMENSION(:,:) :: ZFIELDIN=>NULL()  ! field to interpolate horizontally
REAL, ALLOCATABLE, DIMENSION(:,:) :: ZFIELDOUT ! field interpolated   horizontally
INTEGER                       :: ILUOUT    ! output listing logical unit
INTEGER :: INL, INFOMPI
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
!
!*      1.     Reading of input file name and type
!
IF (LHOOK) CALL DR_HOOK('PREP_HOR_FLAKE_FIELD',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
CMASK = 'WATER'
!
!
!-------------------------------------------------------------------------------------
!
!*      2.     Reading of input  configuration (Grid and interpolation type)
!
  IF (GUNIF) THEN
    CALL PREP_FLAKE_UNIF(ILUOUT,HSURF,ZFIELDIN)
  ELSE IF (YFILETYPE=='ASCLLV') THEN
    CALL PREP_FLAKE_ASCLLV(DTCO, UG, U, USS, HPROGRAM,HSURF,ILUOUT,ZFIELDIN)
  ELSE IF (YFILETYPE=='GRIB  ') THEN
    CALL PREP_GRIB_GRID(YFILE,ILUOUT,CINMODEL,CINGRID_TYPE,CINTERP_TYPE,TZTIME_GRIB)
    IF (NRANK==NPIO) CALL PREP_FLAKE_GRIB(HPROGRAM,HSURF,YFILE,ILUOUT,ZFIELDIN)            
  ELSE IF (YFILETYPE=='MESONH' .OR. YFILETYPE=='ASCII ' .OR. YFILETYPE=='LFI   '.OR. YFILETYPE=='FA    '&
          .OR.YFILETYPE=='NC    ') THEN
    CALL PREP_FLAKE_EXTERN(GCP,HPROGRAM,HSURF,YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,ILUOUT,ZFIELDIN)
  ELSE IF (YFILETYPE=='BUFFER') THEN
    CALL PREP_FLAKE_BUFFER(HPROGRAM,HSURF,ILUOUT,ZFIELDIN)
  ELSE
    CALL ABOR1_SFX('PREP_HOR_FLAKE_FIELD: data file type not supported : '//YFILETYPE)
  END IF
!
!
!*      4.     Horizontal interpolation
!
IF (NRANK==NPIO) THEN
  INL = SIZE(ZFIELDIN,2)
ELSEIF (.NOT.ASSOCIATED(ZFIELDIN)) THEN
 ALLOCATE(ZFIELDIN(0,0))
ENDIF
!
IF (NPROC>1) THEN
#ifdef SFX_MPI
  CALL MPI_BCAST(INL,KIND(INL)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
#endif
ENDIF
  !ALLOCATE(ZFIELDOUT(SIZE(XLAT),SIZE(ZFIELDIN,2)))
  ALLOCATE(ZFIELDOUT(KLAT,1))

  IF(GUNIF) THEN
    CALL HOR_INTERPOL(DTCO, U, GCP, ILUOUT,ZFIELDIN,ZFIELDOUT)
    GINTERP=.FALSE.
  ELSE
    IF(HSURF(1:2)=='ZS') THEN
      CALL HOR_INTERPOL(DTCO, U, GCP, ILUOUT,ZFIELDIN,ZFIELDOUT)
      GINTERP=.TRUE.
    ELSE
      IF((SIZE(ZFIELDIN,1).EQ.SIZE(ZFIELDOUT,1)).AND.(SIZE(ZFIELDIN,2).EQ.SIZE(ZFIELDOUT,2))) THEN
        ZFIELDOUT=ZFIELDIN
        GINTERP=.FALSE.
      ELSE
        IF(HSURF(1:2)=='TS') THEN
          CALL HOR_INTERPOL(DTCO, U, GCP, ILUOUT,ZFIELDIN,ZFIELDOUT)
        ELSE
          WRITE(ILUOUT,*) "WARNING! Impossible to interpolate lake profiles in horisontal!"
          WRITE(ILUOUT,*) "So, interoplate only surface temperature and start from lakes mixed down to the bottom"
          ZFIELDOUT=XUNDEF
        END IF
        GINTERP=.TRUE.
      END IF
    END IF
  END IF
!
!*      5.     Return to historical variable
!
  SELECT CASE (HSURF)
   CASE('ZS     ') 
    ALLOCATE(XZS_LS(SIZE(ZFIELDOUT,1)))
    XZS_LS(:) = ZFIELDOUT(:,1)
   CASE('TS     ')
    ALLOCATE(F%XTS(SIZE(ZFIELDOUT,1)))
    F%XTS(:) = ZFIELDOUT(:,1)
   CASE('T_SNOW ')
    ALLOCATE(F%XT_SNOW(SIZE(ZFIELDOUT,1)))
    F%XT_SNOW(:) = ZFIELDOUT(:,1)
   CASE('T_ICE  ')
    ALLOCATE(F%XT_ICE(SIZE(ZFIELDOUT,1)))
    F%XT_ICE(:) = ZFIELDOUT(:,1)
   CASE('T_WML  ')
    ALLOCATE(F%XT_WML(SIZE(ZFIELDOUT,1)))
    F%XT_WML(:) = ZFIELDOUT(:,1)
   CASE('T_MNW  ')
    ALLOCATE(F%XT_MNW(SIZE(ZFIELDOUT,1)))
    F%XT_MNW(:) = ZFIELDOUT(:,1)
   CASE('T_BOT  ')
    ALLOCATE(F%XT_BOT(SIZE(ZFIELDOUT,1)))
    F%XT_BOT(:) = ZFIELDOUT(:,1)
   CASE('T_B1   ')
    ALLOCATE(F%XT_B1(SIZE(ZFIELDOUT,1)))
    F%XT_B1(:) = ZFIELDOUT(:,1)
   CASE('CT     ')
    ALLOCATE(F%XCT(SIZE(ZFIELDOUT,1)))
    F%XCT(:) = ZFIELDOUT(:,1)
   CASE('H_SNOW ')
    ALLOCATE(F%XH_SNOW(SIZE(ZFIELDOUT,1)))
    F%XH_SNOW(:) = ZFIELDOUT(:,1)
   CASE('H_ICE  ')
    ALLOCATE(F%XH_ICE(SIZE(ZFIELDOUT,1)))
    F%XH_ICE(:) = ZFIELDOUT(:,1)
   CASE('H_ML   ')
    ALLOCATE(F%XH_ML(SIZE(ZFIELDOUT,1)))
    F%XH_ML(:) = ZFIELDOUT(:,1)
   CASE('H_B1   ')
    ALLOCATE(F%XH_B1(SIZE(ZFIELDOUT,1)))
    F%XH_B1(:) = ZFIELDOUT(:,1)
  END SELECT
!
!*      6.     Deallocations
!

  DEALLOCATE(ZFIELDIN )
  DEALLOCATE(ZFIELDOUT)

!
  
!
IF (LHOOK) CALL DR_HOOK('PREP_HOR_FLAKE_FIELD',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_HOR_FLAKE_FIELD
