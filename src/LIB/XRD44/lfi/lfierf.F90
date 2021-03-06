! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI

SUBROUTINE LFIERF_FORT                             &
&                     (LFI, KREP, KNUMER, LDERFA )
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        CE SOUS-PROGRAMME PERMET D'ACTIVER OU DE DESACTIVER L'OPTION
!     RENDANT FATALE TOUTE ERREUR DETECTEE SUR UN FICHIER PARTICULIER,
!     OUVERT POUR LE LOGICIEL LFI.
!        CEPENDANT, TANT QUE LE NIVEAU GLOBAL D'ERREUR FATALE *LFI%NERFAG*
!     VAUT 0 OU 2, L'OPTION PROPRE AU FICHIER EST INOPERANTE.
!     *LFI%NERFAG* VAUT PAR DEFAUT 1, ET EST REGLABLE VIA LE S/P "LFINEG".
!**
!     ARGUMENTS : KREP   (SORTIE) ==> CODE-REPONSE DU SOUS-PROGRAMME;
!                 KNUMER (ENTREE) ==> LFI%NUMERO D'UNITE LOGIQUE CONCERNEE;
!                 LDERFA (ENTREE) ==> OPTION D'ERREUR FATALE (VRAI=OUI).
!
!
TYPE(LFICOM) :: LFI
INTEGER (KIND=JPLIKB) KREP, KNUMER, IRANG, IREP, INIMES
!
LOGICAL LDERFA
!
CHARACTER(LEN=LFI%JPLSPX) CLNSPR
CHARACTER(LEN=LFI%JPLMES) CLMESS
CHARACTER(LEN=LFI%JPLFTX) CLACTI
LOGICAL LLFATA

!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('LFIERF_FORT',0,ZHOOK_HANDLE)
CLACTI=''
CALL LFINUM_FORT                    &
&               (LFI, KNUMER,IRANG)
!
IF (IRANG.NE.0) THEN
  LFI%LERFAT(IRANG)=LDERFA
  LFI%NDEROP(IRANG)=4
  IREP=0
ELSE
  IREP=-1
ENDIF
!
LLFATA=LLMOER (IREP,IRANG)
KREP=IREP
!
IF (LLFATA.OR.IXNIMS (IRANG).EQ.2) THEN
  INIMES=2
ELSE
  IF (LHOOK) CALL DR_HOOK('LFIERF_FORT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='LFIERF'
WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KNUMER='',I3, &
&       '', LDERFA= '',L1)') KREP,KNUMER,LDERFA
CALL LFIEMS_FORT                                 &
&               (LFI, KNUMER,INIMES,IREP,LLFATA, &
&                CLMESS,CLNSPR,CLACTI)
!
IF (LHOOK) CALL DR_HOOK('LFIERF_FORT',1,ZHOOK_HANDLE)

CONTAINS

#include "lficom2.ixnims.h"
#include "lficom2.llmoer.h"

END SUBROUTINE LFIERF_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFIERF64              &
&           (KREP, KNUMER, LDERFA)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
LOGICAL                LDERFA                                 ! IN   

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIERF_FORT                       &
&           (LFI, KREP, KNUMER, LDERFA)

END SUBROUTINE LFIERF64

SUBROUTINE LFIERF                &
&           (KREP, KNUMER, LDERFA)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
LOGICAL                LDERFA                                 ! IN   

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIERF_MT                        &
&           (LFI, KREP, KNUMER, LDERFA)

END SUBROUTINE LFIERF

SUBROUTINE LFIERF_MT                  &
&           (LFI, KREP, KNUMER, LDERFA)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
LOGICAL                LDERFA                                 ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)

CALL LFIERF_FORT                       &
&           (LFI, IREP, INUMER, LDERFA)

KREP       = INT (      IREP, JPLIKM)

END SUBROUTINE LFIERF_MT

!INTF KREP            OUT 
!INTF KNUMER        IN    
!INTF LDERFA        IN    
