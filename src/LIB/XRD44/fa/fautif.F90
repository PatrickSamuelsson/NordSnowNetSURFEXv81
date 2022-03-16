! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FAUTIF_FORT                           &
&                     (FA,  KREP, KNUMER, CDIDEN )
USE FA_MOD, ONLY : FA_COM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Sous-programme permettant de donner un NOM a l'Identificateur
!     d'un fichier ARPEGE.
!       ( l'Utilisateur Traite son Identificateur de Fichier )
!**
!    Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                KNUMER (Entree) ==> Numero de l'unite logique;
!                CDIDEN (Entree) ==> Nom de l'identificateur.
!
!       Une messagerie de niveau 1 est emise dans les cas "normaux"
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KNUMER
!
INTEGER (KIND=JPLIKB) IREP, ILIDEN, IRANG, INIMES, ILACTI
!
LOGICAL LLVERF, LLRLFI
!
CHARACTER CDIDEN*(*)
!
CHARACTER(LEN=FA%JPXNOM) CLACTI 
CHARACTER(LEN=FA%JPXNOM) CLNOMA
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA

!**
!     1.  -  CONTROLES ET INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FAUTIF_MT',0,ZHOOK_HANDLE)
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
ENDIF
!
!         Verrouillage eventuel du fichier.
!
IF (FA%LFAMUL) CALL LFIVER_FORT                               &
&                              (FA%LFI, FA%FICHIER(IRANG)%VRFICH,'ON')
LLVERF=FA%LFAMUL
CLNOMA=FA%FICHIER(IRANG)%CIDENT
!
IF (CDIDEN.EQ.FA%CPCACH.OR.CDIDEN.EQ.FA%CPCADI.OR. &
&   CDIDEN.EQ.FA%CPCAFS.OR.CDIDEN.EQ.FA%CPCARP.OR. &
&   CDIDEN.EQ.FA%CPDATE.OR.CDIDEN.EQ.FA%CPDATX) THEN
  IREP=-111
  GOTO 1001
ENDIF
!**
!     2.  -  ON RENOMME L'ARTICLE IDENTIFICATEUR, QUI EXISTE TOUJOURS SI
!            LE FICHIER EST OUVERT, AU MOINS AVEC UN NOM PAR DEFAUT.
!-----------------------------------------------------------------------
!
IF (CDIDEN.NE.FA%FICHIER(IRANG)%CIDENT) THEN
  CALL LFIREN_FORT                                             &
&                 (FA%LFI, IREP,KNUMER,FA%FICHIER(IRANG)%CIDENT,CDIDEN)
  LLRLFI=IREP.NE.0
  IF (.NOT.LLRLFI) FA%FICHIER(IRANG)%CIDENT=CDIDEN
ELSE
  IREP=0
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
IF (.NOT.LLFATA.AND.INIMES.EQ.0)  THEN 
  IF (LHOOK) CALL DR_HOOK('FAUTIF_MT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='FAUTIF'
!
IF (IREP.NE.-65) THEN
  ILACTI=FA%NCPCAD
  CLACTI(1:ILACTI)=CDIDEN(1:MIN (ILIDEN,ILACTI))
ELSE
  ILACTI=8
  CLACTI(1:ILACTI)=FA%CHAINC(:ILACTI)
ENDIF
!
IF (INIMES.EQ.2) THEN
  WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KNUMER='',I3, &
&         '', CDIDEN='''''',A,'''''''')')                  &
&   KREP,KNUMER,CLACTI(1:ILACTI)
  CALL FAIPAR_FORT                              &
&                 (FA, KNUMER,INIMES,IREP,LLFATA, &
&               CLMESS,CLNSPR,                    &
&               CLACTI(1:ILACTI),LLRLFI)
ENDIF
!
!        La messagerie qui suit n'est pas emise en cas d'erreur fatale.
!
IF (INIMES.GE.1.AND.IRANG.NE.0) THEN
  WRITE (UNIT=CLMESS,FMT=                               &
&  '(''Ancien Identificateur de l''''unite logique'',I3, &
&    '' : '''''',A,'''''', Nouveau: '''''',A,'''''''')') &
&  KNUMER,CLNOMA,FA%FICHIER(IRANG)%CIDENT
  CALL FAIPAR_FORT                                      &
&                 (FA, KNUMER,INIMES,IREP,.FALSE.,CLMESS, &
&                  CLNSPR,CLACTI(1:ILACTI),.FALSE.)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('FAUTIF_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"
#include "facom2.ixnvms.h"

END SUBROUTINE FAUTIF_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FAUTIF64              &
&           (KREP, KNUMER, CDIDEN)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDIDEN                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAUTIF_FORT                     &
&           (FA, KREP, KNUMER, CDIDEN)

END SUBROUTINE FAUTIF64

SUBROUTINE FAUTIF                &
&           (KREP, KNUMER, CDIDEN)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDIDEN                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAUTIF_MT                       &
&           (FA, KREP, KNUMER, CDIDEN)

END SUBROUTINE FAUTIF

SUBROUTINE FAUTIF_MT                 &
&           (FA, KREP, KNUMER, CDIDEN)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDIDEN                                 ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)

CALL FAUTIF_FORT                     &
&           (FA, IREP, INUMER, CDIDEN)

KREP       = INT (      IREP, JPLIKM)

END SUBROUTINE FAUTIF_MT

!INTF KREP            OUT 
!INTF KNUMER        IN    
!INTF CDIDEN        IN    
