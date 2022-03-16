! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI

SUBROUTINE LFICAP_FORT                                    &
&                     (LFI, KREP, KNUMER, CDNOMA, KLONG,  &
&                      KPOSEX, LDRECU )
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        SOUS-PROGRAMME DONNANT LES CARACTERISTIQUES ( NOM, LONGUEUR,
!     POSITION ) DE L'ARTICLE LOGIQUE *DE DONNEES* PRECEDENT, SUR UNE
!     UNITE LOGIQUE OUVERTE POUR LE LOGICIEL DE FICHIERS INDEXES *LFI* .
!**
!    ARGUMENTS : KREP   (SORTIE) ==> CODE-REPONSE DU SOUS-PROGRAMME;
!                KNUMER (ENTREE) ==> LFI%NUMERO DE L'UNITE LOGIQUE;
!                CDNOMA (SORTIE) ==> NOM DE L'ARTICLE SUIVANT;
!                KLONG  (SORTIE) ==> LONGUEUR DE L'ARTICLE PRECEDENT;
!                KPOSEX (SORTIE) ==> POSITION ( DANS LE FICHIER, DU PRE-
!                                    MIER MOT ) DE L'ARTICLE PRECEDENT;
!                LDRECU (ENTREE) ==> VRAI SI ON DOIT "RECULER" LE
!                                    POINTEUR DU FICHIER.
!
!     SI L'ON SOUHAITE LIRE ENSUITE L'ARTICLE EN QUESTION (VIA *LFILAP*)
!     IL FAUT PRECISER A L'APPEL LDRECU=.FALSE. ; CET ARGUMENT EXISTE
!     SURTOUT PAR HOMOGENEITE AVEC *LFICAS*.
!
!     SI LE FICHIER EST VIDE OU QUE LE DERNIER ARTICLE LOGIQUE LU ETAIT
!     LE PREMIER, LE SOUS-PROGRAMME "RETOURNE" KLONG=0, ET CDNOMA=' ' .
!
!
TYPE(LFICOM) :: LFI
CHARACTER CDNOMA*(*), CLNOMA*(LFI%JPNCPN)
!
INTEGER (KIND=JPLIKB) KREP, KNUMER, KLONG, KPOSEX, IREP 
INTEGER (KIND=JPLIKB) ILCDNO, IDECBL, IPOSBL
INTEGER (KIND=JPLIKB) ILCLNO, IRANG, IRGPIM, IARTIC 
INTEGER (KIND=JPLIKB) IRGPIF, INIMES, IRETIN
!
LOGICAL LDRECU, LLVERF
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
IF (LHOOK) CALL DR_HOOK('LFICAP_FORT',0,ZHOOK_HANDLE)
CLACTI=''
CALL LFINUM_FORT                    &
&               (LFI, KNUMER,IRANG)
LLVERF=.FALSE.
IREP=0
KLONG=0
KPOSEX=0
ILCDNO=INT (LEN (CDNOMA), JPLIKB)
!
IF (ILCDNO.LE.0) THEN
  IREP=-15
  CLNOMA=LFI%CHINCO(:LFI%JPNCPN)
  ILCLNO=LFI%JPNCPN
  GOTO 1001
ELSE
  CDNOMA=' '
  CLNOMA=' '
  ILCLNO=1
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
!**
!     2.  -  EXPLORATION DES (PAIRES DE) PAGES ET ARTICLES D'INDEX,
!            A LA RECHERCHE DE L'ARTICLE LOGIQUE DEMANDE,
!            DEFINI PAR SON RANG "A PRIORI" DANS LE FICHIER.
!-----------------------------------------------------------------------
!
CALL LFICAQ_FORT                                       &
&               (LFI, IREP,IRANG,IRGPIM,IARTIC,IRETIN)
!
IF (IRETIN.EQ.1) THEN
  GOTO 903
ELSEIF (IRETIN.EQ.2) THEN
  GOTO 904
ELSEIF (IRETIN.NE.0.OR.IARTIC.EQ.0) THEN
  GOTO 1001
ENDIF
!*
!     2.1 -  ARTICLE DE DONNEES TROUVE... APRES CONTROLES SUPPLEMENTAI-
!            RES, ON RETOURNE SES CARACTERISTIQUES.
!-----------------------------------------------------------------------
!
IRGPIF=LFI%MRGPIF(IRGPIM)
!
IF (.NOT.LFI%LPHASP(IRGPIM)) THEN
!
  CALL LFIPHA_FORT                                &
&                 (LFI, IREP,IRANG,IRGPIM,IRETIN)
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
KLONG=LFI%MLGPOS(IXM(2*IARTIC-1,IRGPIM))
KPOSEX=LFI%MLGPOS(IXM(2*IARTIC,IRGPIM))
CLNOMA=LFI%CNOMAR(IXC(IARTIC,IRGPIM))
!
!        Recherche de la longueur "utile" du nom d'article.
!        (c'est-a-dire sans tenir compte des blancs terminaux eventuels)
!
IDECBL=0
!
211 CONTINUE
IPOSBL=IDECBL+INT (INDEX (CLNOMA(IDECBL+1:),' '), JPLIKB)
!
IF (IPOSBL.LE.IDECBL) THEN
  ILCLNO=LFI%JPNCPN
ELSEIF (CLNOMA(IPOSBL:).EQ.' ') THEN
  ILCLNO=IPOSBL-1
ELSE
  IDECBL=IPOSBL
  GOTO 211
ENDIF
!
IF (ILCDNO.GE.ILCLNO) THEN
  CDNOMA=CLNOMA(:ILCLNO)
ELSE
  IREP=-24
  CLACTI=CLNOMA
  GOTO 1001
ENDIF
!
IF (LDRECU) THEN
!
!          ON RECULE LE "POINTEUR" DU FICHIER...
!       ET ON REINITIALISE LES "POINTEURS" SUIVANT ET PRECEDENT.
!
  LFI%NDERGF(IRANG)=LFI%JPNAPP*LFI%MFACTM(IRANG)*(IRGPIF-1)+IARTIC
  LFI%CNDERA(IRANG)=CLNOMA
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
  LFI%NDEROP(IRANG)=17
  LFI%NDERCO(IRANG)=IREP
   IF (LLVERF) CALL LFIVER_FORT                               &
&                              (LFI, LFI%VERRUE(IRANG),'OFF')
ENDIF
!
IF (LLFATA.OR.IXNIMS (IRANG).EQ.2) THEN
  INIMES=2
ELSE
  IF (LHOOK) CALL DR_HOOK('LFICAP_FORT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='LFICAP'
WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KNUMER='',I3,       &
&    '', CDNOMA='''''',A,'''''', KLONG='',I7,'', KPOSEX='',I8, &
&    '', LDRECU= '',L1)')                                      &
&  KREP,KNUMER,CLNOMA(:ILCLNO),KLONG,KPOSEX,LDRECU
CALL LFIEMS_FORT                                 &
&               (LFI, KNUMER,INIMES,IREP,LLFATA, &
&                CLMESS,CLNSPR,CLACTI)
!
IF (LHOOK) CALL DR_HOOK('LFICAP_FORT',1,ZHOOK_HANDLE)

CONTAINS

#include "lficom2.ixc.h"
#include "lficom2.ixm.h"
#include "lficom2.ixnims.h"
#include "lficom2.llmoer.h"

END SUBROUTINE LFICAP_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFICAP64                                     &
&           (KREP, KNUMER, CDNOMA, KLONG, KPOSEX, LDRECU)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 !   OUT
INTEGER (KIND=JPLIKB)  KLONG                                  !   OUT
INTEGER (KIND=JPLIKB)  KPOSEX                                 !   OUT
LOGICAL                LDRECU                                 ! IN   

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFICAP_FORT                                              &
&           (LFI, KREP, KNUMER, CDNOMA, KLONG, KPOSEX, LDRECU)

END SUBROUTINE LFICAP64

SUBROUTINE LFICAP                                       &
&           (KREP, KNUMER, CDNOMA, KLONG, KPOSEX, LDRECU)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 !   OUT
INTEGER (KIND=JPLIKM)  KLONG                                  !   OUT
INTEGER (KIND=JPLIKM)  KPOSEX                                 !   OUT
LOGICAL                LDRECU                                 ! IN   

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFICAP_MT                                               &
&           (LFI, KREP, KNUMER, CDNOMA, KLONG, KPOSEX, LDRECU)

END SUBROUTINE LFICAP

SUBROUTINE LFICAP_MT                                         &
&           (LFI, KREP, KNUMER, CDNOMA, KLONG, KPOSEX, LDRECU)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 !   OUT
INTEGER (KIND=JPLIKM)  KLONG                                  !   OUT
INTEGER (KIND=JPLIKM)  KPOSEX                                 !   OUT
LOGICAL                LDRECU                                 ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  ILONG                                  !   OUT
INTEGER (KIND=JPLIKB)  IPOSEX                                 !   OUT
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)

CALL LFICAP_FORT                                              &
&           (LFI, IREP, INUMER, CDNOMA, ILONG, IPOSEX, LDRECU)

KREP       = INT (      IREP, JPLIKM)
KLONG      = INT (     ILONG, JPLIKM)
KPOSEX     = INT (    IPOSEX, JPLIKM)

END SUBROUTINE LFICAP_MT

!INTF KREP            OUT 
!INTF KNUMER        IN    
!INTF CDNOMA          OUT 
!INTF KLONG           OUT 
!INTF KPOSEX          OUT 
!INTF LDRECU        IN    
