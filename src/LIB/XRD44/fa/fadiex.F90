! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FADIEX_FORT                           &
&                     (FA,  KREP, KNUMER, KDATEF )
USE FA_MOD, ONLY : FA_COM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!      Sous-programme permettant d'obtenir la date d'un fichier ouvert
!     pour le logiciel de Fichiers ARPEGE, et deja muni d'une date.
!     ( "DIES" = jour en latin... )
!**
!    Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                KNUMER (Entree) ==> Numero de l'unite logique;
!     (Tableau)  KDATEF (Sortie) ==> Date elle-meme (FA%JPLDAT mots).
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KNUMER
INTEGER (KIND=JPLIKB) KDATEF (FA%JPLDAT*2)
!
INTEGER (KIND=JPLIKB) IRANG, IREP, INIMES, J
!
LOGICAL LLVERF
!
CHARACTER(LEN=FA%JPXNOM) CLACTI 
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA

!**
!     1.  -  CONTROLES DES PARAMETRES D'APPEL, ET INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FADIEX_MT',0,ZHOOK_HANDLE)
CLACTI=''
LLVERF=.FALSE.
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
!     2.  -  CONTROLE DE DEFINITION PREALABLE DE LA DATE.
!-----------------------------------------------------------------------
!
IF (FA%FICHIER(IRANG)%LCREAF) THEN
  IREP=-85
  GOTO 1001
ENDIF
!**
!     3.  -  TRANSFERT DE LA TABLE "FA%MADATE" DANS LE TABLEAU ARGUMENT.
!-----------------------------------------------------------------------
!
KDATEF(1:FA%JPLDAT)=FA%FICHIER(IRANG)%MADATE(:)
KDATEF(FA%JPLDAT+1:FA%JPLDAT*2)=FA%FICHIER(IRANG)%MADATX(:)
!
IREP=0
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
IF (LLFATA.OR.IXNVMS(IRANG).EQ.2) THEN
  INIMES=2
ELSE
  IF (LHOOK) CALL DR_HOOK('FADIEX_MT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='FADIEX'
!
IF (INIMES.EQ.2) THEN
  WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KNUMER='',I3, &
&       '', KDATEF(1:5)='',I5,2(''/'',I2),I3,'':'',I2.2,   &
&       '', KDATEF(7:8)='',I6,''-'',I6)') KREP,KNUMER,     &
&     (KDATEF(J),J=1,5),(KDATEF(J),J=7,8)
  CALL FAIPAR_FORT                                     &
&                 (FA, KNUMER,INIMES,IREP,LLFATA,CLMESS, &
&               CLNSPR,CLACTI,.FALSE.)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('FADIEX_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"
#include "facom2.ixnvms.h"

END SUBROUTINE FADIEX_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FADIEX64                      &
&           (KREP, KNUMER, KDATEF)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                  FA_COM_DEFAULT_INIT,  &
&                  NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  KDATEF     (*)               !   OUT

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FADIEX_FORT                     &
&           (FA, KREP, KNUMER, KDATEF)

END SUBROUTINE FADIEX64

SUBROUTINE FADIEX                        &
&           (KREP, KNUMER, KDATEF)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                  FA_COM_DEFAULT_INIT,  &
&                  NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKM)  KDATEF     (*)               !   OUT

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FADIEX_MT                       &
&           (FA, KREP, KNUMER, KDATEF)

END SUBROUTINE FADIEX

SUBROUTINE FADIEX_MT                 &
&           (FA, KREP, KNUMER, KDATEF)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKM)  KDATEF     (FA%JPLDAT*2)               !   OUT
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  IDATEF     (FA%JPLDAT*2)               !   OUT
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)

CALL FADIEX_FORT                     &
&           (FA, IREP, INUMER, IDATEF)

KREP       = INT (      IREP, JPLIKM)
KDATEF     = INT (    IDATEF, JPLIKM)

END SUBROUTINE FADIEX_MT

!INTF KREP            OUT                               
!INTF KNUMER        IN                                  
!INTF KDATEF          OUT DIMS=FA%JPLDAT*2              

