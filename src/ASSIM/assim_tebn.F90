!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################################################
SUBROUTINE ASSIM_TEB_n (U, NT, TOP, &
                        HPROGRAM,KI,PT2M_O,HTEST)

!     ###############################################################################
!
!!****  *ASSIM_TOWN_n * - Chooses the surface schemes for TOWN parts  
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
!!--------------------------------------------------------------------
!
!
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_TEB_n, ONLY : TEB_NP_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_ASSIM,          ONLY : CASSIM_TEB
!
USE MODD_CSTS,           ONLY : XPI
USE MODD_ASSIM,          ONLY : NPRINTLEV,XAT2M_TEB,LPIO
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE YOMHOOK,             ONLY : LHOOK,   DR_HOOK
USE PARKIND1,            ONLY : JPRB

USE MODI_GET_LUOUT
USE MODI_ABOR1_SFX
USE MODI_ASSIM_GATHER_WRITE_INCREMENTS
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(TEB_NP_t), INTENT(INOUT) :: NT
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
!
CHARACTER(LEN=6),   INTENT(IN) :: HPROGRAM  ! program calling surf. schemes
INTEGER,            INTENT(IN) :: KI
REAL,DIMENSION(KI), INTENT(IN) :: PT2M_O
CHARACTER(LEN=2),   INTENT(IN) :: HTEST ! must be equal to 'OK'
!
!*      0.2    declarations of local variables
!
!-------------------------------------------------------------------------------------
!
REAL, DIMENSION (KI) :: ZTRD
REAL, DIMENSION (KI) :: ZT2INC
REAL, DIMENSION (KI) :: ZTCLS
CHARACTER            :: CL
INTEGER              :: JI,IROADLAYER,ILUOUT
REAL(KIND=JPRB)      :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('ASSIM_TEB_N',0,ZHOOK_HANDLE)

CALL GET_LUOUT(HPROGRAM,ILUOUT)
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('ASSIM_TEB_n: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF

IF ( TRIM(CASSIM_TEB) == "ROADT" ) THEN
  IROADLAYER=3
  WRITE(CL,'(I1)') IROADLAYER
  IF ( LPIO) WRITE(ILUOUT,*) 'UPDATING TOWN FOR SCHEME: ',TRIM(U%CTOWN)

  IF ( TOP%NROAD_LAYER < IROADLAYER ) CALL ABOR1_SFX('ASSIM_TEB_n: Only imlemented with '//CL//' or more layers')

  ZTRD(:) = NT%AL(1)%XT_ROAD(:,IROADLAYER)  ! T_ROAD
  ZTCLS(:) = XAT2M_TEB(:)  ! T2M (TEB)

  ! Screen-level innovations

  ZT2INC=0.
  WHERE ( PT2M_O(:) /= 999. )
    ZT2INC(:) = PT2M_O(:) - ZTCLS(:)
  END WHERE

  IF ( NPRINTLEV > 0 ) CALL ASSIM_GATHER_WRITE_INCREMENTS(HPROGRAM,"T2M_TEB",ZTCLS(:),PT2M_O(:),999.)

  ! Temperature analysis of TOWN points
  WHERE (ZTRD(:)/=XUNDEF)
    ZTRD(:) = ZTRD(:) + ZT2INC(:)/(2.0*XPI)
  END WHERE

  ! Print increment
  CALL ASSIM_GATHER_WRITE_INCREMENTS(HPROGRAM,"T_ROAD"//CL,NT%AL(1)%XT_ROAD(:,IROADLAYER),ZTRD(:),XUNDEF)

  ! Update modified variables
  NT%AL(1)%XT_ROAD(:,IROADLAYER) = ZTRD  ! T_ROAD
ENDIF

IF (LHOOK) CALL DR_HOOK('ASSIM_TEB_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE ASSIM_TEB_n
