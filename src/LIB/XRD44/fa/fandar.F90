! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FANDAR_FORT                           &
&                     (FA,  KREP, KNUMER, KDATEF)
USE FA_MOD, ONLY : FA_COM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!      Sous-programme de definition d'une (Nouvelle) Date sur un fichier
!     ARpege.
!**
!    Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                KNUMER (Entree) ==> Numero de l'unite logique;
!     (Tableau)  KDATEF (Entree) ==> Date elle-meme (FA%JPLDAT mots).
!*
!        En cas de modification effective (si le fichier etait deja muni
!     d'une date), il y a messagerie de niveau 1.
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KNUMER
INTEGER (KIND=JPLIKB) KDATEF (FA%JPLDAT)
!
INTEGER (KIND=JPLIKB) IRANG, IREP, INIMES, J
INTEGER (KIND=JPLIKB) ILONG, IPOSEX
INTEGER (KIND=JPLIKB) IDATXF (FA%JPLDAT)
!
LOGICAL LLVERF, LLRLFI, LLMODA
!
CHARACTER(LEN=FA%JPXNOM) CLACTI 
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA

!**
!     1.  -  CONTROLES DES PARAMETRES D'APPEL, PUIS INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FANDAR_MT',0,ZHOOK_HANDLE)
CLACTI=''
LLVERF=.FALSE.
LLRLFI=.FALSE.
LLMODA=.FALSE.
IDATXF=0
CALL FANUMU_FORT                 &
&               (FA, KNUMER,IRANG)
!
IF (IRANG.EQ.0) THEN
  IREP=-51
  GOTO 1001
ENDIF
!
!         Verrouillage eventuel du fichier.
!
IF (FA%LFAMUL) CALL LFIVER_FORT                               &
&                              (FA%LFI, FA%FICHIER(IRANG)%VRFICH,'ON')
LLVERF=FA%LFAMUL
!**
!     2.  -  DEFINITION PROPREMENT DITE VIA LE SOUS-PROGRAMME "FANDAI".
!            ( controles, puis mise a jour de FA%MADATE(.,IRANG) )
!-----------------------------------------------------------------------
!
CALL FANDAI_FORT                             &
&               (FA, IREP,IRANG,KDATEF,IDATXF,LLMODA)
!
IF (IREP.EQ.0) THEN
  IF (FA%FICHIER(IRANG)%LNOMME) THEN
!**
!     3.  -  ECRITURE DE LA DATE SUR LE FICHIER.
!-----------------------------------------------------------------------
!
    CALL LFIECR_FORT                                                 &
&                   (FA%LFI,IREP,KNUMER,FA%CPDATE,KDATEF,FA%JPLDAT)
    LLRLFI=IREP.NE.0
    FA%FICHIER(IRANG)%LCREAF=FA%FICHIER(IRANG)%LCREAF.AND.LLRLFI
    CALL LFINFO_FORT                                                 &
&                   (FA%LFI,IREP,KNUMER,FA%CPDATX,ILONG,IPOSEX)
    IF (ILONG > 0) THEN
      CALL LFISUP_FORT                                               &
&                     (FA%LFI,IREP,KNUMER,FA%CPDATX,ILONG)
      IF (IREP /= 0) GOTO 1001
    ENDIF
  ELSE
    LLRLFI=.FALSE.
    FA%FICHIER(IRANG)%LCREAF=.FALSE.
  ENDIF
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
ELSEIF (IREP.NE.0) THEN
  INIMES=0
ELSE
  INIMES=IXNVMS(IRANG)
ENDIF
!
IF (.NOT.LLFATA.AND.INIMES.EQ.0)  THEN 
  IF (LHOOK) CALL DR_HOOK('FANDAR_MT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='FANDAR'
!
IF (INIMES.GE.1.AND.LLMODA) THEN
  WRITE (UNIT=CLMESS,FMT=                                  &
&         '(''MODIFICATION DE LA DATE, UNITE'',I3)') KNUMER
  CALL FAIPAR_FORT                                      &
&                 (FA, KNUMER,INIMES,IREP,.FALSE.,CLMESS, &
&                  CLNSPR,CLACTI,.FALSE.)
ENDIF
!
IF (INIMES.EQ.2) THEN
!***** FAZZZZ - KREP=iiii, KNUMER=iii, KDATEF(1:5)=iiiii/ii/ii iii:ii, *****
!*****          KDATEF(7:8)=iiiiii-iiiiii                              *****
  WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KNUMER='',I3, &
&       '', KDATEF(1:5)='',I5,2(''/'',I2),I3,'':'',I2.2,   &
&       '', KDATEF(7:8)='',I6,''-'',I6)') KREP,KNUMER,     &
&     (KDATEF(J),J=1,5),(KDATEF(J),J=7,8)
  CALL FAIPAR_FORT                                     &
&                 (FA, KNUMER,INIMES,IREP,LLFATA,CLMESS, &
&               CLNSPR,CLACTI,LLRLFI)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('FANDAR_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"
#include "facom2.ixnvms.h"

END SUBROUTINE FANDAR_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FANDAR64              &
&           (KREP, KNUMER, KDATEF)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  KDATEF     (*)                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FANDAR_FORT                     &
&           (FA, KREP, KNUMER, KDATEF)

END SUBROUTINE FANDAR64

SUBROUTINE FANDAR                &
&           (KREP, KNUMER, KDATEF)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKM)  KDATEF     (*)                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FANDAR_MT                       &
&           (FA, KREP, KNUMER, KDATEF)

END SUBROUTINE FANDAR

SUBROUTINE FANDAR_MT                 &
&           (FA, KREP, KNUMER, KDATEF)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKM)  KDATEF     (FA%JPLDAT)                 ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  IDATEF     (FA%JPLDAT)                 ! IN   
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)
IDATEF     = INT (    KDATEF, JPLIKB)

CALL FANDAR_FORT                     &
&           (FA, IREP, INUMER, IDATEF)

KREP       = INT (      IREP, JPLIKM)

END SUBROUTINE FANDAR_MT

!INTF KREP            OUT                               
!INTF KNUMER        IN                                  
!INTF KDATEF        IN    DIMS=FA%JPLDAT                


