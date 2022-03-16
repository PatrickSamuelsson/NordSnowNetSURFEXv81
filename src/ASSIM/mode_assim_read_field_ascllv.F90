!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################################################
MODULE MODE_ASSIM_READ_FIELD_ASCLLV

INTERFACE ASSIM_READ_FIELD_ASCLLV
  MODULE PROCEDURE ASSIM_READ_FIELD_ASCLLV
END INTERFACE

CONTAINS
SUBROUTINE ASSIM_READ_FIELD_ASCLLV (DTCO, U, UG, USS, HMASK, HRECORD, PFIELD)

!     ###############################################################################
!
!!****  *ASSIM_READ_FIELD * - Reads a ASCII LATLONVAL field for assimilation from file
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
USE MODD_PGD_GRID,        ONLY : LLATLONMASK
USE MODD_ASSIM,           ONLY : LPIO
USE MODD_SURFEX_MPI,      ONLY : NPROC
USE MODD_PGD_GRID,        ONLY : NL
USE MODD_SURF_PAR,        ONLY : XUNDEF
USE YOMHOOK,              ONLY : LHOOK,DR_HOOK
USE PARKIND1,             ONLY : JPRB

USE MODI_ABOR1_SFX
USE MODI_READ_AND_SEND_MPI
USE MODI_PGD_FIELD
USE MODI_LATLONMASK

IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!
TYPE(DATA_COVER_t),    INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t),      INTENT(INOUT) :: U
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SSO_t),           INTENT(INOUT) :: USS
CHARACTER(LEN=6),      INTENT(IN)    :: HMASK
CHARACTER(LEN=*),      INTENT(IN)    :: HRECORD
REAL,DIMENSION(:),     INTENT(OUT)   :: PFIELD

!
!*      0.2    declarations of local variables
!
!-------------------------------------------------------------------------------------
!
REAL,ALLOCATABLE, DIMENSION(:) :: ZWORK,ZWORK_FULL
CHARACTER(LEN=6)     :: YPROGRAM="NOTUSE"
CHARACTER(LEN=28)    :: YFILENAME
CHARACTER(LEN=3)     :: YAREA
CHARACTER(LEN=6)     :: YTYPE="ASCLLV"
INTEGER              :: IRESP
INTEGER              :: JI
INTEGER              :: ISIZE_FULL
REAL(KIND=JPRB)      :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('ASSIM_READ_FILE_ASCLLV',0,ZHOOK_HANDLE)

ALLOCATE(ZWORK_FULL (U%NDIM_FULL))
ALLOCATE(ZWORK(U%NSIZE_FULL))

IF ( NPROC > 1 ) THEN
  CALL ABOR1_SFX("Reading of ASCLLV does not work for multiple processors yet")
ENDIF

IF (LPIO) THEN

  NL=U%NDIM_FULL

  CALL LATLONMASK(UG%G%CGRID, UG%NGRID_FULL_PAR, UG%XGRID_FULL_PAR, LLATLONMASK)

  YAREA="ALL"
  YFILENAME=TRIM(HRECORD)//".LLV"
  CALL PGD_FIELD(DTCO, UG, U, USS,YPROGRAM,HRECORD,YAREA,YFILENAME,YTYPE,XUNDEF,ZWORK_FULL(:))

ENDIF

! Distribute ZWORK to all processors
IF (NPROC>1) THEN
  CALL READ_AND_SEND_MPI(ZWORK(:),ZWORK_FULL(:))
ELSE
  ZWORK=ZWORK_FULL
ENDIF

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
DEALLOCATE(ZWORK_FULL)

IF (LHOOK) CALL DR_HOOK('ASSIM_READ_FILE_ASCLLV',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE ASSIM_READ_FIELD_ASCLLV
END MODULE MODE_ASSIM_READ_FIELD_ASCLLV
