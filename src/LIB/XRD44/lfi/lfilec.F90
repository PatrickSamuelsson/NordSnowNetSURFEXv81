! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI

SUBROUTINE LFILEC_FORT                                          &
&                     (LFI, KREP, KNUMER, CDNOMA, KTAB, KLONG )
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        SOUS-PROGRAMME DE LECTURE D'UN ARTICLE (DE DONNEES) PAR *NOM*
!     SUR UNE UNITE LOGIQUE OUVERTE POUR LE LOGICIEL DE FICHIERS INDEXES
!     *LFI*; L'ARTICLE EN SORTIE EST UN "BLOC" DE DONNEES ADJACENTES.
!**
!    ARGUMENTS : KREP   (SORTIE) ==> CODE-REPONSE DU SOUS-PROGRAMME;
!                KNUMER (ENTREE) ==> LFI%NUMERO DE L'UNITE LOGIQUE;
!                CDNOMA (ENTREE) ==> NOM DE L'ARTICLE A RECHERCHER;
!                KTAB   (ENTREE) ==> PREMIER MOT A LIRE;
!                KLONG  (ENTREE) ==> LONGUEUR DE L'ARTICLE A LIRE.
!
!
TYPE(LFICOM) :: LFI
CHARACTER CDNOMA*(*), CLNOMA*(LFI%JPNCPN)
!
INTEGER (KIND=JPLIKB) KREP, KNUMER, KLONG
INTEGER (KIND=JPLIKB)  KTAB (KLONG)
INTEGER (KIND=JPLIKB) IREP, IRANG, ILCLNO, IREPX, ILCDNO 
INTEGER (KIND=JPLIKB) IDECBL, IPOSBL, IARTEX
INTEGER (KIND=JPLIKB) ILONEX, IRGPIM, IRGPIF, IPOSEX 
INTEGER (KIND=JPLIKB) IRETIN, INIMES, INBALO
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
IF (LHOOK) CALL DR_HOOK('LFILEC_FORT',0,ZHOOK_HANDLE)

CLACTI=''

CALL LFINUM_FORT                    &
&               (LFI, KNUMER,IRANG)
LLVERF=.FALSE.
IREP=0
IREPX=0
ILCDNO=INT (LEN (CDNOMA), JPLIKB)
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
IF (KLONG.LE.0) THEN
  IREP=-14
  GOTO 1001
ELSEIF (IRANG.EQ.0) THEN
  IREP=-1
  GOTO 1001
ENDIF
!
 IF (LFI%LMULTI) CALL LFIVER_FORT                              &
&                                (LFI, LFI%VERRUE(IRANG),'ON')
LLVERF=LFI%LMULTI
!
IARTEX=0
ILONEX=0
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
ENDIF
!
IF (IARTEX.EQ.0) THEN
  IREP=-20
  CLACTI=CLNOMA(:ILCLNO)
  GOTO 1001
ENDIF
!
!        ON COMPLETE LES CARACTERISTIQUES DE L'ARTICLE.
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
ILONEX=LFI%MLGPOS(IXM(2*IARTEX-1,IRGPIM))
IPOSEX=LFI%MLGPOS(IXM(2*IARTEX,IRGPIM))
!
!       CONTROLE CROISE ENTRE LONGUEURS DEMANDEE ET TROUVEE SUR FICHIER.
!
IF (ILONEX.GT.KLONG) THEN
  IREP=-21
  LLFATA=LLMOER (IREP,IRANG)
!
  IF (LLFATA) THEN
    CLACTI=CLNOMA(:ILCLNO)
    GOTO 1001
  ENDIF
!
!        SI L'ERREUR (-21) N'A PAS ETE FATALE, ON VA LIRE SEULEMENT
!       LE DEBUT DE L'ARTICLE ( LECTURE PARTIELLE DE *KLONG* MOTS )
!
ELSEIF (ILONEX.LT.KLONG) THEN
  IREP=-22
  CLACTI=CLNOMA(:ILCLNO)
  GOTO 1001
ENDIF
!
IREPX=IREP
!**
!     3.  -  LECTURE DES DONNEES PROPREMENT DITE.
!-----------------------------------------------------------------------
!
CALL LFILED_FORT                                                  &
&               (LFI, IREP,IRANG,KTAB,KLONG,IRGPIM,IPOSEX,IRETIN)
!
IF (IRETIN.EQ.1) THEN
  GOTO 903
ELSEIF (IRETIN.EQ.2) THEN
  GOTO 904
ELSEIF (IRETIN.NE.0) THEN
  GOTO 1001
ENDIF
!
IREP=IREPX
!**
!     4.  -   MISE A JOUR DE STATISTIQUES ET DE TABLES.
!-----------------------------------------------------------------------
!
LFI%NBLECT(IRANG)=LFI%NBLECT(IRANG)+1
LFI%NBMOLU(IRANG)=LFI%NBMOLU(IRANG)+KLONG
LFI%NDERGF(IRANG)=LFI%JPNAPP*LFI%MFACTM(IRANG)*(IRGPIF-1)+IARTEX
LFI%CNDERA(IRANG)=CLNOMA(:ILCLNO)
LFI%NSUIVF(IRANG)=LFI%JPNIL
LFI%NPRECF(IRANG)=LFI%JPNIL
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
  LFI%NDEROP(IRANG)=2
  LFI%NDERCO(IRANG)=IREP
   IF (LLVERF) CALL LFIVER_FORT                               &
&                              (LFI, LFI%VERRUE(IRANG),'OFF')
ENDIF
!
IF (LLFATA.OR.IXNIMS (IRANG).EQ.2) THEN
  INIMES=2
ELSE
  IF (LHOOK) CALL DR_HOOK('LFILEC_FORT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='LFILEC'
WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KNUMER='',I3, &
&       '', CDNOMA='''''',A,'''''', KLONG='',I7)')       &
&     KREP,KNUMER,CLNOMA(:ILCLNO),KLONG
CALL LFIEMS_FORT                                 &
&               (LFI, KNUMER,INIMES,IREP,LLFATA, &
&                CLMESS,CLNSPR,CLACTI)
!
IF (LHOOK) CALL DR_HOOK('LFILEC_FORT',1,ZHOOK_HANDLE)

CONTAINS

#include "lficom2.ixm.h"
#include "lficom2.ixnims.h"
#include "lficom2.llmoer.h"

END SUBROUTINE LFILEC_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFILEC64                           &
&           (KREP, KNUMER, CDNOMA, KTAB, KLONG)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 ! IN   
INTEGER (KIND=JPLIKB)  KLONG                                  ! IN   
INTEGER (KIND=JPLIKB)  KTAB       (KLONG)                     ! IN   

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFILEC_FORT                                    &
&           (LFI, KREP, KNUMER, CDNOMA, KTAB, KLONG)

END SUBROUTINE LFILEC64

SUBROUTINE LFILEC                             &
&           (KREP, KNUMER, CDNOMA, KTAB, KLONG)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 ! IN   
INTEGER (KIND=JPLIKM)  KLONG                                  ! IN   
INTEGER (KIND=JPLIKB)  KTAB       (KLONG)                     ! IN   

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFILEC_MT                                     &
&           (LFI, KREP, KNUMER, CDNOMA, KTAB, KLONG)

END SUBROUTINE LFILEC

SUBROUTINE LFILEC_MT                               &
&           (LFI, KREP, KNUMER, CDNOMA, KTAB, KLONG)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 ! IN   
INTEGER (KIND=JPLIKM)  KLONG                                  ! IN   
INTEGER (KIND=JPLIKB)  KTAB       (KLONG)                     ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  ILONG                                  ! IN   
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)
ILONG      = INT (     KLONG, JPLIKB)

CALL LFILEC_FORT                                    &
&           (LFI, IREP, INUMER, CDNOMA, KTAB, ILONG)

KREP       = INT (      IREP, JPLIKM)

END SUBROUTINE LFILEC_MT

!INTF KREP            OUT                                                              
!INTF KNUMER        IN                                                                 
!INTF CDNOMA        IN                                                                 
!INTF KTAB          IN    DIMS=KLONG                     KIND=JPLIKB                   
!INTF KLONG         IN                                                                 
