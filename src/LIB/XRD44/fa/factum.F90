! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FACTUM_FORT             &
&                     (FA,  CDNOMC )
USE FA_MOD, ONLY : FA_COM, JPNIIL
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Sous-programme servant a supprimer un cadre.
!     ( Cadre a TUer Methodiquement ? )
!**
!        Argument : CDNOMC (Entree) ==> Nom symbolique du cadre.
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) ILCDNO, IREP, IRANGC, ILNOMC
INTEGER (KIND=JPLIKB) INIMES, INUMER, J
!
LOGICAL LLVERG
!
CHARACTER CDNOMC*(*)
!
!
!
CHARACTER(LEN=FA%JPXNOM) CLACTI 
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA

!**
!     1.  -  INITIALISATIONS ET CONTROLES SOMMAIRES.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FACTUM_MT',0,ZHOOK_HANDLE)
CLACTI=''
IF (FA%FACTUM_LLPREA) THEN
!
!         Initialisation eventuelle des variables globales du logiciel.
!
  CALL FARINE_FORT              &
&                 (FA, 2_JPLIKB )
  FA%FACTUM_LLPREA=.FALSE.
ENDIF
!
LLVERG=.FALSE.
ILCDNO=INT (LEN (CDNOMC), JPLIKB)
!
IF (ILCDNO.LE.0) THEN
  IREP=-65
  GOTO 1001
ELSEIF (CDNOMC.EQ.' ') THEN
  IREP=-68
  GOTO 1001
ENDIF
!
DO J=ILCDNO,1,-1
!
IF (CDNOMC(J:J).NE.' ') THEN
  ILNOMC=J
  GOTO 102
ENDIF
!
ENDDO
!
102 CONTINUE
!
IF (ILNOMC.GT.FA%NCPCAD) THEN
  IREP=-65
  GOTO 1001
ENDIF
!             Verrouillage global, si necessaire.
!
IF (FA%LFAMUL) CALL LFIVER_FORT                        &
&                              (FA%LFI, FA%VRGLAS,'ON')
LLVERG=FA%LFAMUL
!
!          Controle d'existence du cadre specifie.
!
CALL FANUCA_FORT                          &
&               (FA, CDNOMC,IRANGC,.FALSE.)
!
IF (IRANGC.EQ.0) THEN
  IREP=-51
  GOTO 1001
ENDIF
!**
!     2.  -  SUPPRESSION PROPREMENT DITE VIA LE SOUS-PROGRAMME "FACTUI".
!-----------------------------------------------------------------------
!
CALL FACTUI_FORT                &
&               (FA, IREP,IRANGC)
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE, AVEC "ABORT" EVENTUEL,
!            VIA LE sous-programme "FAIPAR" .
!-----------------------------------------------------------------------
!
1001 CONTINUE
!
!          Deverrouillage global eventuel.
!
IF (LLVERG) CALL LFIVER_FORT                         &
&                           (FA%LFI, FA%VRGLAS,'OFF')
!
LLFATA=LLMOER(IREP,0_JPLIKB )
!
IF (.NOT.LLFATA.OR.FA%NIMSGA.NE.2)  THEN 
  IF (LHOOK) CALL DR_HOOK('FACTUM_MT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
INIMES=2
CLNSPR='FACTUM'
!
IF (IREP.EQ.-65.AND.ILCDNO.LE.0) THEN
  ILNOMC=8
  CLACTI(1:ILNOMC)=FA%CHAINC(:ILNOMC)
ELSE
  ILNOMC=MIN (INT (LEN (CLACTI), JPLIKB),ILNOMC)
  CLACTI=CDNOMC(1:ILNOMC)
ENDIF
!
WRITE (UNIT=CLMESS,FMT='(''CDNOMC='''''',A,'''''''')') &
&     CLACTI(1:ILNOMC)
INUMER=JPNIIL
CALL FAIPAR_FORT                                     &
&               (FA, INUMER,INIMES,IREP,LLFATA,CLMESS, &
&                CLNSPR, CLACTI(1:ILNOMC),.FALSE.)
!
IF (LHOOK) CALL DR_HOOK('FACTUM_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"

END SUBROUTINE FACTUM_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FACTUM64           &
&           (CDNOMC)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
CHARACTER (LEN=*)      CDNOMC                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FACTUM_FORT           &
&           (FA, CDNOMC)

END SUBROUTINE FACTUM64

SUBROUTINE FACTUM             &
&           (CDNOMC)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
CHARACTER (LEN=*)      CDNOMC                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FACTUM_MT             &
&           (FA, CDNOMC)

END SUBROUTINE FACTUM

SUBROUTINE FACTUM_MT             &
&           (FA, CDNOMC)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
CHARACTER (LEN=*)      CDNOMC                                 ! IN   
! Local integers
! Convert arguments


CALL FACTUM_FORT           &
&           (FA, CDNOMC)


END SUBROUTINE FACTUM_MT

!INTF CDNOMC        IN    
