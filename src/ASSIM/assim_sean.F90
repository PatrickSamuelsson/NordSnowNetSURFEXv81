!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################################################
SUBROUTINE ASSIM_SEA_n (S, U, HPROGRAM,KI,PTS_IN,PSST_IN,PSIC_IN,PITM,HTEST, &
                        OLKEEPEXTZONE,OD_MASKEXT,PLON_IN,PLAT_IN)

!     ###############################################################################
!
!!****  *ASSIM_SEA_n * - Chooses the surface assimilation schemes for SEA tile
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
!!      Original       04/2012
!!      Trygve Aspelien, Separating IO  06/2013 
!!--------------------------------------------------------------------
!
!
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_ASSIM,          ONLY : NPRINTLEV,LAESST,LEXTRAP_SEA,CASSIM_SEA,LPIO
!
!
USE YOMHOOK,             ONLY : LHOOK,DR_HOOK
USE PARKIND1,            ONLY : JPRB
!
USE MODI_ABOR1_SFX
USE MODI_PACK_SAME_RANK
USE MODI_ASSIM_GATHER_WRITE_INCREMENTS
USE MODI_OI_HOR_EXTRAPOL_SURF
USE MODI_ASSIM_EXTRAPOLATE_FIELD
USE MODI_GET_LUOUT
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
CHARACTER(LEN=6),   INTENT(IN) :: HPROGRAM  ! program calling surf. schemes
INTEGER,            INTENT(IN) :: KI
REAL,DIMENSION(KI), INTENT(IN) :: PTS_IN
REAL,DIMENSION(KI), INTENT(IN) :: PSST_IN
REAL,DIMENSION(KI), INTENT(IN) :: PSIC_IN
REAL,DIMENSION(KI), INTENT(IN) :: PITM
CHARACTER(LEN=2),   INTENT(IN) :: HTEST ! must be equal to 'OK'
LOGICAL, INTENT(IN) :: OLKEEPEXTZONE
LOGICAL, DIMENSION(KI), INTENT(IN) :: OD_MASKEXT
REAL(KIND=JPRB), DIMENSION (:), INTENT(IN) ::  PLON_IN
REAL(KIND=JPRB), DIMENSION (:), INTENT(IN) ::  PLAT_IN
!
!*      0.2    declarations of local variables
!
!-------------------------------------------------------------------------------------
!
REAL, DIMENSION(KI) :: ZALT
REAL, DIMENSION(KI) :: ZSST
REAL, DIMENSION(KI) :: ZSIC
REAL, DIMENSION(KI) :: ZSST0
REAL, DIMENSION(:), ALLOCATABLE :: ZSST01, ZSST1, ZLON1, ZLAT1, ZALT1 
REAL :: ZFMAX, ZFMIN, ZFMEAN
INTEGER  :: IRESP, JI,ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('ASSIM_SEA_N',0,ZHOOK_HANDLE)
!
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('ASSIM_SEA_n: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF
!
CALL GET_LUOUT(HPROGRAM,ILUOUT)
IF (LPIO) WRITE(ILUOUT,*) 'UPDATING SST FOR SCHEME: ',TRIM(U%CSEA)
IF (U%CSEA=="NONE" ) THEN
  IF (LHOOK) CALL DR_HOOK('ASSIM_SEA_N',1,ZHOOK_HANDLE)
  RETURN
ENDIF

IF ( TRIM(CASSIM_SEA) == "INPUT" ) THEN
  !
  ! Read SST from file or set it to input SST
  IF ( .NOT.LAESST ) THEN
    ! Set SST to input
    ZSST(:) = PSST_IN(:)
    !
  ELSE
    ! SST analysed in CANARI 
    ZSST(:) = XUNDEF
    DO JI=1,KI
      IF (PITM(JI)<0.5 .AND. U%XSEA(U%NR_SEA(JI))/=0. ) THEN
       ZSST(JI) = PTS_IN(JI)   ! set SST analysis from CANARI
      ENDIF
    END DO
    !
    ZFMIN = MINVAL(ZSST)
    ZFMAX = MAXVAL(ZSST)
    IF ( KI > 0 ) THEN
      ZFMEAN = SUM(ZSST)/FLOAT(KI)
    ELSE
      ZFMEAN=XUNDEF
    ENDIF
    WRITE(ILUOUT,*) '  SST analysis from CANARI '
    WRITE(ILUOUT,'("  ZSST            - min, mean, max: ",3E13.4)') ZFMIN, ZFMEAN, ZFMAX
  ENDIF

  ! Sum the increments
  CALL ASSIM_GATHER_WRITE_INCREMENTS(HPROGRAM,"SST",S%XSST(:),ZSST(:),XUNDEF,LSTAT=.TRUE.)

  ! Setting modified variables
  S%XSST(:) = ZSST(:)
ENDIF

 
IF ( LEXTRAP_SEA ) THEN
  ZSST0=S%XSST
  WHERE ( ZSST(:) /= XUNDEF )
    ZSST0(:) = ZSST(:)
  ELSEWHERE
    ZSST0(:) = XUNDEF
  ENDWHERE
  ZSST=ZSST0
  CALL ASSIM_EXTRAPOLATE_FIELD(HPROGRAM,PLON_IN,PLAT_IN,ZSST0,OLKEEPEXTZONE,OD_MASKEXT,ZSST,PALT=U%XZS)

  ! Sum the increments
  CALL ASSIM_GATHER_WRITE_INCREMENTS(HPROGRAM,"EXTRAPOLATED SST",S%XSST(:),ZSST(:),XUNDEF,LSTAT=.TRUE.)
ENDIF

! Handle Sea-ice
ZSIC = PSIC_IN
! Consistency check
WHERE(ABS(ZSIC(:)) > 0)
  WHERE( ZSIC(:) < 0.05 ) ZSIC(:) = 0.0
ENDWHERE
CALL S%ICE%ASSIM(HPROGRAM, ZSIC, PLON_IN, PLAT_IN)

IF(S%LHANDLE_SIC .AND. (S%CSEAICE_SCHEME == 'SICE  ')) THEN
  S%XSIC(:) = ZSIC(:)
END IF

IF (LHOOK) CALL DR_HOOK('ASSIM_SEA_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE ASSIM_SEA_n
