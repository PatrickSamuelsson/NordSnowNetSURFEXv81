! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI

SUBROUTINE LFINFO_FORT                                            &
&                     (LFI, KREP, KNUMER, CDNOMA, KLONG, KPOSEX )
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        SOUS-PROGRAMME CHARGE DE RENSEIGNER SUR EXISTENCE ET CARACTERI-
!     STIQUES ( LONGUEUR, POSITION ) D'UN ARTICLE LOGIQUE, POUR UNE
!     UNITE LOGIQUE OUVERTE PAR LE LOGICIEL DE FICHIERS INDEXES *LFI*.
!**
!    ARGUMENTS : KREP   (SORTIE) ==> CODE-REPONSE DU SOUS-PROGRAMME;
!                KNUMER (ENTREE) ==> LFI%NUMERO DE L'UNITE LOGIQUE;
!                CDNOMA (ENTREE) ==> NOM DE L'ARTICLE A CHERCHER;
!                KLONG  (SORTIE) ==> LONGUEUR DE L'ARTICLE;
!                KPOSEX (SORTIE) ==> POSITION ( DANS LE FICHIER, DU PRE-
!                                    MIER MOT ) DE L'ARTICLE SUIVANT.
!
!       Noter que si l'unite logique est ouverte pour le logiciel LFI et
!     que l'article demande n'y est pas trouve, KREP, KLONG et KPOSEX
!     sont retournes a ZERO.
!
!
TYPE(LFICOM) :: LFI
CHARACTER CDNOMA*(*), CLNOMA*(LFI%JPNCPN)
!
INTEGER (KIND=JPLIKB) KREP, KNUMER, KLONG, KPOSEX 
INTEGER (KIND=JPLIKB) IREP, IRANG, ILCLNO, ILCDNO
INTEGER (KIND=JPLIKB) IDECBL, IPOSBL, IARTEX, IRGPIM 
INTEGER (KIND=JPLIKB) INIMES, INBALO, IRETIN
!
LOGICAL LLVERF
!
CHARACTER(LEN=LFI%JPLSPX) CLNSPR
CHARACTER(LEN=LFI%JPLMES) CLMESS
CHARACTER(LEN=LFI%JPLFTX) CLACTI
LOGICAL LLFATA

!**
!     1.  -  CONTROLES DES PARAMETRES D'APPEL, PUIS INITIALISATIONS.
!-----------------------------------------------------------------------
!
!        Appel legerement anticipe a LFINUM, garantissant l'initialisa-
!     tion des variables globales du logiciel a la 1ere utilisation.
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('LFINFO_FORT',0,ZHOOK_HANDLE)

CLACTI=''

CALL LFINUM_FORT                    &
&               (LFI, KNUMER,IRANG)
LLVERF=.FALSE.
ILCDNO=INT (LEN (CDNOMA), JPLIKB)
KLONG=0
KPOSEX=0
!
IF (ILCDNO.LE.0) THEN
  IREP=-15
  CLNOMA=LFI%CHINCO(:LFI%JPNCPN)
  ILCLNO=LFI%JPNCPN
  GOTO 1001
ELSEIF (CDNOMA.EQ.' ') THEN
  IREP=-18
  CLNOMA=' '
  ILCLNO=1
  GOTO 1001
ENDIF
!
!        Recherche de la longueur "utile" du nom d'article specifie.
!        (c'est-a-dire sans tenir compte des blancs terminaux eventuels)
!
IDECBL=0
!
101 CONTINUE
IPOSBL=IDECBL+INT (INDEX (CDNOMA(IDECBL+1:),' '), JPLIKB)
!
IF (IPOSBL.LE.IDECBL) THEN
  ILCLNO=ILCDNO
ELSEIF (CDNOMA(IPOSBL:).EQ.' ') THEN
  ILCLNO=IPOSBL-1
ELSE
  IDECBL=IPOSBL
  GOTO 101
ENDIF
!
IF (ILCLNO.LE.LFI%JPNCPN) THEN
  CLNOMA=CDNOMA(:ILCLNO)
ELSE
  CLNOMA=CDNOMA(:LFI%JPNCPN)
  ILCLNO=LFI%JPNCPN
  IREP=-15
  GOTO 1001
ENDIF
!
IF (IRANG.EQ.0) THEN
  IREP=-1
  GOTO 1001
ENDIF
!
 IF (LFI%LMULTI) CALL LFIVER_FORT                              &
&                                (LFI, LFI%VERRUE(IRANG),'ON')
LLVERF=LFI%LMULTI
!
INBALO=LFI%MDES1D(IXM(LFI%JPNALO,IRANG))
!
IF (INBALO.NE.0) THEN
!**
!     2.  -  EXPLORATION DES (PAIRES DE) PAGES ET ARTICLES D'INDEX,
!            A LA RECHERCHE DE L'ARTICLE LOGIQUE DEMANDE.
!-----------------------------------------------------------------------
!
  CALL LFIRAN_FORT                                  &
&                 (LFI, IREP,IRANG,CLNOMA(:ILCLNO), &
&                  IRGPIM,IARTEX,IRETIN)
!
  IF (IRETIN.EQ.1) THEN
    GOTO 903
  ELSEIF (IRETIN.EQ.2) THEN
    GOTO 904
  ELSEIF (IRETIN.NE.0) THEN
    GOTO 1001
  ENDIF
!
ELSE
  IARTEX=0
  IREP=0
ENDIF
!
IF (IARTEX.EQ.0) THEN
  KLONG=0
  KPOSEX=0
ELSE
!
!        ON COMPLETE LES CARACTERISTIQUES DE L'ARTICLE.
!
  IF (.NOT.LFI%LPHASP(IRGPIM)) THEN
!
    CALL LFIPHA_FORT                                &
&                   (LFI, IREP,IRANG,IRGPIM,IRETIN)
!
    IF (IRETIN.EQ.1) THEN
      GOTO 903
    ELSEIF (IRETIN.EQ.2) THEN
      GOTO 904
    ELSEIF (IRETIN.NE.0) THEN
      GOTO 1001
    ENDIF
!
  ENDIF
!
  KLONG=LFI%MLGPOS(IXM(2*IARTEX-1,IRGPIM))
  KPOSEX=LFI%MLGPOS(IXM(2*IARTEX,IRGPIM))
!
!        On met a jour ce qui a trait aux acces pseudo-sequentiels...
!     ceci surtout pour ne pas faire 2 recherches dans l'index lors
!     d'un appel a LFILEC qui suivrait l'appel a LFINFO.
!
  LFI%NDERGF(IRANG)=LFI%JPNAPP*LFI%MFACTM(IRANG)* &
&                    (LFI%MRGPIF(IRGPIM)-1)+IARTEX
  LFI%CNDERA(IRANG)=CLNOMA(:ILCLNO)
  LFI%NSUIVF(IRANG)=LFI%JPNIL
  LFI%NPRECF(IRANG)=LFI%JPNIL
ENDIF
!
GOTO 1001
!**
!     9.  - CI-DESSOUS, ETIQUETTES DE BRANCHEMENT EN CAS D'ERREUR E/S.
!-----------------------------------------------------------------------
!
903 CONTINUE
CLACTI='WRITE'
GOTO 909
!
904 CONTINUE
CLACTI='READ'
!
909 CONTINUE
!
!      AU CAS OU, ON FORCE LE CODE-REPONSE ENTREE/SORTIE A ETRE POSITIF.
!
IREP=ABS (IREP)
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE, AVEC "ABORT" EVENTUEL,
!            VIA LE SOUS-PROGRAMME "LFIEMS" .
!-----------------------------------------------------------------------
!
1001 CONTINUE
KREP=IREP
LLFATA=LLMOER (IREP,IRANG)
!
IF (IRANG.NE.0) THEN
  LFI%NDEROP(IRANG)=7
  LFI%NDERCO(IRANG)=IREP
   IF (LLVERF) CALL LFIVER_FORT                               &
&                              (LFI, LFI%VERRUE(IRANG),'OFF')
ENDIF
!
IF (LLFATA.OR.IXNIMS (IRANG).EQ.2) THEN
  INIMES=2
ELSE
  IF (LHOOK) CALL DR_HOOK('LFINFO_FORT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='LFINFO'
WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KNUMER='',I3,            &
&       '', CDNOMA='''''',A,'''''', KLONG='',I7,'',KPOSEX='',I10)') &
&     KREP,KNUMER,CLNOMA(:ILCLNO),KLONG,KPOSEX
CALL LFIEMS_FORT                                 &
&               (LFI, KNUMER,INIMES,IREP,LLFATA, &
&                CLMESS,CLNSPR,CLACTI)
!
IF (LHOOK) CALL DR_HOOK('LFINFO_FORT',1,ZHOOK_HANDLE)

CONTAINS

#include "lficom2.ixm.h"
#include "lficom2.ixnims.h"
#include "lficom2.llmoer.h"

END SUBROUTINE LFINFO_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFINFO64                             &
&           (KREP, KNUMER, CDNOMA, KLONG, KPOSEX)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 ! IN   
INTEGER (KIND=JPLIKB)  KLONG                                  !   OUT
INTEGER (KIND=JPLIKB)  KPOSEX                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFINFO_FORT                                      &
&           (LFI, KREP, KNUMER, CDNOMA, KLONG, KPOSEX)

END SUBROUTINE LFINFO64

SUBROUTINE LFINFO                               &
&           (KREP, KNUMER, CDNOMA, KLONG, KPOSEX)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 ! IN   
INTEGER (KIND=JPLIKM)  KLONG                                  !   OUT
INTEGER (KIND=JPLIKM)  KPOSEX                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFINFO_MT                                       &
&           (LFI, KREP, KNUMER, CDNOMA, KLONG, KPOSEX)

END SUBROUTINE LFINFO

SUBROUTINE LFINFO_MT                                 &
&           (LFI, KREP, KNUMER, CDNOMA, KLONG, KPOSEX)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 ! IN   
INTEGER (KIND=JPLIKM)  KLONG                                  !   OUT
INTEGER (KIND=JPLIKM)  KPOSEX                                 !   OUT
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  ILONG                                  !   OUT
INTEGER (KIND=JPLIKB)  IPOSEX                                 !   OUT
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)

CALL LFINFO_FORT                                      &
&           (LFI, IREP, INUMER, CDNOMA, ILONG, IPOSEX)

KREP       = INT (      IREP, JPLIKM)
KLONG      = INT (     ILONG, JPLIKM)
KPOSEX     = INT (    IPOSEX, JPLIKM)

END SUBROUTINE LFINFO_MT

!INTF KREP            OUT 
!INTF KNUMER        IN    
!INTF CDNOMA        IN    
!INTF KLONG           OUT 
!INTF KPOSEX          OUT 
