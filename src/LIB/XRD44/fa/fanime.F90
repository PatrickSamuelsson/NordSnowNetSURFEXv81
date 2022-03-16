! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FANIME_FORT                           &
&                     (FA,  KREP, KNUMER, KNIMES )
USE FA_MOD, ONLY : FA_COM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Ce sous-programme permet d'ajuster le Niveau de Messagerie
!     propre aux actions faites sur un fichier particulier, ouvert pour
!     le Logiciel de Fichiers ARPEGE, de meme que le Niveau correspon-
!     dant du logiciel LFI.
!        Cependant, tant que le Niveau de Messagerie Global *FA%NIMSGA*
!     vaut 0 ou 2, le niveau propre au fichier est inoperant.
!     *FA%NIMSGA* vaut par defaut 1, et est reglable via le s/p "FANMSG".
!**
!     Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                 KNUMER (Entree) ==> Numero d'Unite Logique concernee;
!                 KNIMES (Entree) ==> Niveau de Messagerie souhaite.
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KNUMER, KNIMES
!
INTEGER (KIND=JPLIKB) IREP, IRANG, INIMEX, INIMES
!
LOGICAL LLRLFI
!
CHARACTER(LEN=FA%JPXNOM) CLACTI 
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA

!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FANIME_MT',0,ZHOOK_HANDLE)
CLACTI=''
CALL FANUMU_FORT                 &
&               (FA, KNUMER,IRANG)
INIMEX=0
!
IF (IRANG.EQ.0) THEN
  IREP=-51
ELSEIF (KNIMES.GE.0.AND.KNIMES.LE.2) THEN
  INIMEX=IXNVMS (IRANG)
  FA%FICHIER(IRANG)%NIVOMS=KNIMES
  CALL LFINIM_FORT                            &
&                 (FA%LFI, IREP,KNUMER,KNIMES)
  LLRLFI=IREP.NE.0
ELSE
  IREP=-52
ENDIF
!
KREP=IREP
LLFATA=LLMOER (IREP,IRANG)
!
IF (LLFATA.OR.MAX (IXNVMS (IRANG),INIMEX).EQ.2) THEN
  INIMES=2
ELSE
  IF (LHOOK) CALL DR_HOOK('FANIME_MT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='FANIME'
WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KNUMER='',I3, &
&       '', KNIMES='',I3)') KREP,KNUMER,KNIMES
CALL FAIPAR_FORT                                     &
&               (FA, KNUMER,INIMES,IREP,LLFATA,CLMESS, &
&             CLNSPR,CLACTI,.FALSE.)
!
IF (LHOOK) CALL DR_HOOK('FANIME_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"
#include "facom2.ixnvms.h"

END SUBROUTINE FANIME_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FANIME64              &
&           (KREP, KNUMER, KNIMES)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  KNIMES                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FANIME_FORT                     &
&           (FA, KREP, KNUMER, KNIMES)

END SUBROUTINE FANIME64

SUBROUTINE FANIME                &
&           (KREP, KNUMER, KNIMES)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKM)  KNIMES                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FANIME_MT                       &
&           (FA, KREP, KNUMER, KNIMES)

END SUBROUTINE FANIME

SUBROUTINE FANIME_MT                 &
&           (FA, KREP, KNUMER, KNIMES)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKM)  KNIMES                                 ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  INIMES                                 ! IN   
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)
INIMES     = INT (    KNIMES, JPLIKB)

CALL FANIME_FORT                     &
&           (FA, IREP, INUMER, INIMES)

KREP       = INT (      IREP, JPLIKM)

END SUBROUTINE FANIME_MT

!INTF KREP            OUT 
!INTF KNUMER        IN    
!INTF KNIMES        IN    
