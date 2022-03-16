!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################################################
SUBROUTINE ASSIM_INLAND_WATER_n (IM, W, U, HPROGRAM, KI, PTS_IN, PITM, HTEST, &
                                 OLKEEPEXTZONE, OD_MASKEXT, PLON_IN, PLAT_IN  )

!     ###############################################################################
!
!!****  *ASSIM_INLAND_WATER_n * - Chooses the surface assimilation schemes for INLAND_WATER parts  
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
!!      Original    04/2012
!!      Trygve Aspelien, Separating IO  06/2013
!!      HIRLAM (P. Samuelsson), include dependence on PATCH, 04/2018
!!--------------------------------------------------------------------
!
!
USE MODD_SURFEX_n, ONLY : ISBA_MODEL_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_WATFLUX_n, ONLY : WATFLUX_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_ASSIM,          ONLY : NPRINTLEV,LEXTRAP_WATER,LWATERTG2,CASSIM_WATER,LPIO
!
USE YOMHOOK,             ONLY : LHOOK,DR_HOOK
USE PARKIND1,            ONLY : JPRB
!
USE MODI_ABOR1_SFX
USE MODI_OI_HOR_EXTRAPOL_SURF
USE MODI_ASSIM_GATHER_WRITE_INCREMENTS
USE MODI_ASSIM_EXTRAPOLATE_FIELD
USE MODI_GET_LUOUT
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(ISBA_MODEL_t), INTENT(INOUT) :: IM
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(WATFLUX_t), INTENT(INOUT) :: W
!
CHARACTER(LEN=6),   INTENT(IN) :: HPROGRAM  ! program calling surf. schemes
INTEGER,            INTENT(IN) :: KI
REAL,DIMENSION(KI), INTENT(IN) :: PTS_IN
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
REAL, DIMENSION(KI)              :: ZLST
REAL, DIMENSION(KI)              :: ZLST0
REAL, DIMENSION(:), ALLOCATABLE  :: ZLST01, ZLST1, ZLON1, ZLAT1, ZALT1 
!
LOGICAL,DIMENSION(KI) :: GINTERP_LST
LOGICAL, DIMENSION(:), ALLOCATABLE :: GINTERP_LST1
INTEGER  :: IRESP,JI,JJ,IS1,J1,ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('ASSIM_INLAND_WATER_N',0,ZHOOK_HANDLE)

IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('ASSIM_INLAND_WATER_n: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF

CALL GET_LUOUT(HPROGRAM,ILUOUT)
IF (LPIO) WRITE(ILUOUT,*) 'UPDATING LST FOR INLAND_WATER: ',TRIM(U%CWATER)

IF ( U%CWATER=="NONE" ) THEN
  IF (LHOOK) CALL DR_HOOK('ASSIM_INLAND_WATER_N',1,ZHOOK_HANDLE)
  RETURN
ENDIF

ZLST(:)=W%XTS(:)
IF ( TRIM(CASSIM_WATER) == "INPUT" ) THEN

  IF ( U%CWATER=="FLAKE") THEN

    ! DA in vertical for lakes should be called from here.
    ! At the moment, do nothing: FLake runs freely
    IF(U%CWATER=='FLAKE ' .AND. LPIO) WRITE(ILUOUT,*) 'NO UPDATE: FLAKE RUNS FREELY'
        
    IF (LHOOK) CALL DR_HOOK('ASSIM_INLAND_WATER_N',1,ZHOOK_HANDLE)
    RETURN
  ENDIF

  !
  !*     ZLST updated!
  !
  ZLST(:) = XUNDEF
  IF (.NOT.LWATERTG2 ) THEN
    !*     ZLST updated from from CANARI analysis
    DO JI=1,KI
      IF ( PITM(JI)<0.5 ) ZLST(JI) = PTS_IN(JI)
    ENDDO
    !
  ELSE
    ! We do not allow this option if you have more than two patches yet
    IF (IM%O%NPATCH > 2) CALL ABOR1_SFX('ASSIM_INLAND_WATER_n: LWATERTG2 AND NPATCH > 2 IS NOT VALID!')
    ! Set TG2 from global array
    DO JI=1,KI
      IF ( PITM(JI)>0.5 ) THEN
        !*     ZLST updated from LAND values of TG2 from patch 1
        DO JJ=1,IM%NP%AL(1)%NSIZE_P
          IF ( U%NR_WATER(JI)==U%NR_NATURE(JJ) ) THEN
            ! Set ZLST to patch 1 for TG2 if defined, otherwise from patch 2
            IF (IM%NPE%AL(1)%XTG(JJ,2) /=XUNDEF) THEN
              ZLST(JI) = IM%NPE%AL(1)%XTG(JJ,2)
            ELSEIF ( IM%O%NPATCH == 2 .AND. (IM%NPE%AL(2)%XTG(JJ,2) /=XUNDEF)) THEN
              ZLST(JI)=IM%NPE%AL(2)%XTG(JJ,2)
            ELSE
              WRITE(*,*) 'ERROR IN INLAND_WATER XTG: ',IM%NPE%AL(1)%XTG(JJ,2),IM%NPE%AL(2)%XTG(JJ,2)
              CALL ABOR1_SFX('ASSIM_INLAND_WATER_n: THIS SHOULD NEVER HAPPEN:-(')
            ENDIF
            EXIT
          ENDIF
        ENDDO
      ENDIF
    ENDDO
  ENDIF

  ! Sum the increments
  CALL ASSIM_GATHER_WRITE_INCREMENTS(HPROGRAM,"LST",W%XTS(:),ZLST(:),XUNDEF)

  ! Setting modified variables
  WHERE ( ZLST(:)/=XUNDEF)
    W%XTS(:) = ZLST(:)
  ENDWHERE

ELSEIF ( TRIM(CASSIM_WATER) == "NONE" ) THEN
  ! Do nothing
ELSE
  CALL ABOR1_SFX('ASSIM_INLAND_WATER_n:'//TRIM(CASSIM_WATER)//' is not implemented!')
ENDIF

IF ( LEXTRAP_WATER ) THEN
  ZLST0=ZLST
  CALL ASSIM_EXTRAPOLATE_FIELD(HPROGRAM,PLON_IN,PLAT_IN,ZLST0,OLKEEPEXTZONE,OD_MASKEXT,ZLST,PALT=W%XZS)

  ! Sum the increments
  CALL ASSIM_GATHER_WRITE_INCREMENTS(HPROGRAM,"EXTRAPOLATED LST",W%XTS(:),ZLST(:),XUNDEF)

   ! Setting modified variables
   WHERE ( ZLST(:)/=XUNDEF)
     W%XTS(:) = ZLST(:)
   ENDWHERE

ELSEIF ( TRIM(CASSIM_WATER) == "INPUT"  .AND. LWATERTG2 ) THEN
  CALL ABOR1_SFX('ASSIM_INLAND_WATER_n: When running LWATERTG2 you must set LEXTRAP_WATER=.T.')
ENDIF

IF (LHOOK) CALL DR_HOOK('ASSIM_INLAND_WATER_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE ASSIM_INLAND_WATER_n
