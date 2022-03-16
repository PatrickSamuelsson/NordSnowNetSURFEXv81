! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FALSIF_FORT                           &
&                     (FA,  KREP, KNUMER, CDIDEN )
USE FA_MOD, ONLY : FA_COM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Sous-programme renvoyant le NOM de l'Identificateur
!     d'un fichier ARPEGE.
!       ( Lecture Specifique de l'Identificateur de Fichier )
!**
!    Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                KNUMER (Entree) ==> Numero de l'unite logique;
!                CDIDEN (Sortie) ==> Nom de l'identificateur.
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KNUMER
!
INTEGER (KIND=JPLIKB) IREP, ILIDEN, IRANG
INTEGER (KIND=JPLIKB) J, ILONGN, INIMES, ILACTI
!
LOGICAL LLVERF, LLRLFI
!
CHARACTER CDIDEN*(*)
!
CHARACTER(LEN=FA%JPXNOM) CLACTI 
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA

!**
!     1.  -  CONTROLES ET INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FALSIF_MT',0,ZHOOK_HANDLE)
CLACTI=''
LLVERF=.FALSE.
LLRLFI=.FALSE.
ILIDEN=INT (LEN (CDIDEN), JPLIKB)
CALL FANUMU_FORT                 &
&               (FA, KNUMER,IRANG)
!
IF (IRANG.EQ.0) THEN
  IREP=-51
  GOTO 1001
ELSEIF (ILIDEN.LE.0) THEN
  IREP=-65
  GOTO 1001
ELSE
  IREP=0
ENDIF
!
!         Verrouillage eventuel du fichier.
!
IF (FA%LFAMUL) CALL LFIVER_FORT                               &
&                              (FA%LFI, FA%FICHIER(IRANG)%VRFICH,'ON')
LLVERF=FA%LFAMUL
!**
!     2.  -  ON RENVOIE LE NOM D'IDENTIFICATEUR, APRES CONTROLE EVENTUEL
!            D'UNE VARIABLE CARACTERE SUFISAMMENT LONGUE.
!-----------------------------------------------------------------------
!
IF (ILIDEN.GE.FA%NCPCAD) THEN
  CDIDEN=FA%FICHIER(IRANG)%CIDENT
ELSE
!
  DO J=FA%NCPCAD,1,-1
!
  IF (FA%FICHIER(IRANG)%CIDENT(J:J).NE.' ') THEN
    ILONGN=J
    GOTO 202
  ENDIF
!
  ENDDO
!
  IREP=-66
  GOTO 1001
!
202 CONTINUE
!
  IF (ILONGN.GT.ILIDEN) THEN
    IREP=-69
  ELSE
    CDIDEN=FA%FICHIER(IRANG)%CIDENT(1:ILONGN)
  ENDIF
!
ENDIF
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE, AVEC "ABORT" EVENTUEL,
!            VIA LE SOUS-PROGRAMME "FAIPAR" .
!-----------------------------------------------------------------------
!
1001 CONTINUE
KREP=IREP
LLFATA=LLMOER (IREP,IRANG)
!
!        Deverrouillage eventuel du fichier.
!
IF (LLVERF) CALL LFIVER_FORT                                &
&                           (FA%LFI, FA%FICHIER(IRANG)%VRFICH,'OFF')
!
IF (LLFATA) THEN
  INIMES=2
ELSE
  INIMES=IXNVMS(IRANG)
ENDIF
!
IF (.NOT.LLFATA.AND.INIMES.NE.2)  THEN 
  IF (LHOOK) CALL DR_HOOK('FALSIF_MT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='FALSIF'
!
IF (IREP.EQ.0.OR.IREP.EQ.-69) THEN
  ILACTI=FA%NCPCAD
  CLACTI=FA%FICHIER(IRANG)%CIDENT(1:ILACTI)
ELSE
  ILACTI=8
  CLACTI(1:ILACTI)=FA%CHAINC(:ILACTI)
ENDIF
!
WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KNUMER='',I3, &
&       '', CDIDEN='''''',A,'''''''')')                  &
&   KREP,KNUMER,CLACTI(1:ILACTI)
CALL FAIPAR_FORT                                     &
&               (FA, KNUMER,INIMES,IREP,LLFATA,CLMESS, &
&                CLNSPR,CLACTI(1:ILACTI),LLRLFI)
!
IF (LHOOK) CALL DR_HOOK('FALSIF_MT',1,ZHOOK_HANDLE)
RETURN

CONTAINS

#include "facom2.llmoer.h"
#include "facom2.ixnvms.h"

END SUBROUTINE FALSIF_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FALSIF64              &
&           (KREP, KNUMER, CDIDEN)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDIDEN                                 !   OUT

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FALSIF_FORT                     &
&           (FA, KREP, KNUMER, CDIDEN)

END SUBROUTINE FALSIF64

SUBROUTINE FALSIF                &
&           (KREP, KNUMER, CDIDEN)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDIDEN                                 !   OUT

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FALSIF_MT                       &
&           (FA, KREP, KNUMER, CDIDEN)

END SUBROUTINE FALSIF

SUBROUTINE FALSIF_MT                 &
&           (FA, KREP, KNUMER, CDIDEN)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDIDEN                                 !   OUT
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)

CALL FALSIF_FORT                     &
&           (FA, IREP, INUMER, CDIDEN)

KREP       = INT (      IREP, JPLIKM)

END SUBROUTINE FALSIF_MT

!INTF KREP            OUT 
!INTF KNUMER        IN    
!INTF CDIDEN          OUT 
