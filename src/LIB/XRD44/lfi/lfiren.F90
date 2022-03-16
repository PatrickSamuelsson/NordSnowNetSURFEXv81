! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI

SUBROUTINE LFIREN_FORT                                     &
&                     (LFI, KREP, KNUMER, CDNOM1, CDNOM2 )
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        SOUS-PROGRAMME PERMETTANT DE RENOMMER UN ARTICLE (DE DONNEES)
!     SUR UNE UNITE LOGIQUE OUVERTE POUR LE LOGICIEL DE FICHIERS INDEXES
!     *LFI*. LE NOUVEAU NOM D'ARTICLE NE DOIT PAS Y ETRE DEJA UTILISE.
!**
!    ARGUMENTS : KREP   (SORTIE) ==> CODE-REPONSE DU SOUS-PROGRAMME;
!                KNUMER (ENTREE) ==> LFI%NUMERO DE L'UNITE LOGIQUE;
!                CDNOM1 (ENTREE) ==> NOM DE L'ARTICLE A RENOMMER;
!                CDNOM2 (ENTREE) ==> NOUVEAU NOM A DONNER A L'ARTICLE.
!
!
TYPE(LFICOM) :: LFI
CHARACTER CDNOM1*(*), CDNOM2*(*), CLNOM1*(LFI%JPNCPN),  &
&          CLNOM2*(LFI%JPNCPN)
!
INTEGER (KIND=JPLIKB) KREP, KNUMER, IRANG, IREP, ILCDN1 
INTEGER (KIND=JPLIKB) ILCLN1, ILCDN2, ILCLN2
INTEGER (KIND=JPLIKB) IDECBL, IPOSBL, IARTEX, INBALO 
INTEGER (KIND=JPLIKB) IRGPIM, IRETIN, INIMES
!
LOGICAL LLECR, LLVERF
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
IF (LHOOK) CALL DR_HOOK('LFIREN_FORT',0,ZHOOK_HANDLE)
CLACTI=''
CALL LFINUM_FORT                    &
&               (LFI, KNUMER,IRANG)
LLVERF=.FALSE.
IREP=0
LLECR=.FALSE.
ILCDN1=INT (LEN (CDNOM1), JPLIKB)
ILCDN2=INT (LEN (CDNOM2), JPLIKB)
!
IF (MIN (ILCDN1,ILCDN2).LE.0) THEN
!
  IREP=-15
!
  IF (ILCDN1.LE.0) THEN
    CLNOM1=LFI%CHINCO(:LFI%JPNCPN)
    ILCLN1=LFI%JPNCPN
  ELSE
    ILCLN1=MIN (ILCDN1,LFI%JPNCPN)
    CLNOM1=CDNOM1(:ILCLN1)
  ENDIF
!
  IF (ILCDN2.LE.0) THEN
    CLNOM2=LFI%CHINCO(:LFI%JPNCPN)
    ILCLN2=LFI%JPNCPN
  ELSE
    ILCLN2=MIN (ILCDN2,LFI%JPNCPN)
    CLNOM2=CDNOM2(:ILCLN2)
  ENDIF
!
  GOTO 1001
!
ELSEIF (CDNOM1.EQ.' '.OR.CDNOM2.EQ.' ') THEN
!
  IREP=-18
!
  IF (CDNOM1.EQ.' ') THEN
    CLNOM1=' '
    ILCLN1=1
  ELSE
    ILCLN1=MIN (ILCDN1,LFI%JPNCPN)
    CLNOM1=CDNOM1(:ILCLN1)
  ENDIF
!
  IF (CDNOM2.EQ.' ') THEN
    CLNOM2=' '
    ILCLN2=1
  ELSE
    ILCLN2=MIN (ILCDN2,LFI%JPNCPN)
    CLNOM2=CDNOM2(:ILCLN2)
  ENDIF
!
  GOTO 1001
!
ENDIF
!
!        Recherche de la longueur "utile" des noms d'article specifies.
!        (c'est-a-dire sans tenir compte des blancs terminaux eventuels)
!
IDECBL=0
!
101 CONTINUE
IPOSBL=IDECBL+INT (INDEX (CDNOM1(IDECBL+1:),' '), JPLIKB)
!
IF (IPOSBL.LE.IDECBL) THEN
  ILCLN1=ILCDN1
ELSEIF (CDNOM1(IPOSBL:).EQ.' ') THEN
  ILCLN1=IPOSBL-1
ELSE
  IDECBL=IPOSBL
  GOTO 101
ENDIF
!
IDECBL=0
!
102 CONTINUE
IPOSBL=IDECBL+INT (INDEX (CDNOM2(IDECBL+1:),' '), JPLIKB)
!
IF (IPOSBL.LE.IDECBL) THEN
  ILCLN2=ILCDN2
ELSEIF (CDNOM2(IPOSBL:).EQ.' ') THEN
  ILCLN2=IPOSBL-1
ELSE
  IDECBL=IPOSBL
  GOTO 102
ENDIF
!
IF (ILCLN1.GT.LFI%JPNCPN) THEN
  ILCLN1=LFI%JPNCPN
  IREP=-15
ENDIF
!
IF (ILCLN2.GT.LFI%JPNCPN) THEN
  ILCLN2=LFI%JPNCPN
  IREP=-15
ENDIF
!
CLNOM1=CDNOM1(:ILCLN1)
CLNOM2=CDNOM2(:ILCLN2)
IF (IREP.NE.0) GOTO 1001
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
IF (LFI%NEXPOR(IRANG).GT.0) THEN
!
!         Fichier en cours d'export... la seule modification acceptee
!         est l'ajout de nouveaux articles.
!
  IREP=-37
  GOTO 1001
ENDIF
!
IARTEX=0
INBALO=LFI%MDES1D(IXM(LFI%JPNALO,IRANG))
!
IF (INBALO.NE.0) THEN
!**
!     2.  -  EXPLORATION DES (PAIRES DE) PAGES ET ARTICLES D'INDEX,
!            A LA RECHERCHE DU NOUVEAU NOM D'ARTICLE, QUI NE DOIT
!            PAS ETRE LE NOM D'UN ARTICLE EXISTANT.
!-----------------------------------------------------------------------
!
  CALL LFIRAN_FORT                                  &
&                 (LFI, IREP,IRANG,CLNOM2(:ILCLN2), &
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
  IF (IARTEX.NE.0) THEN
    IREP=-25
    CLACTI=CLNOM2(:ILCLN2)
    GOTO 1001
  ENDIF
!**
!     3.  -  EXPLORATION DES (PAIRES DE) PAGES ET ARTICLES D'INDEX,
!            A LA RECHERCHE DE L'ARTICLE LOGIQUE A RENOMMER.
!-----------------------------------------------------------------------
!
  CALL LFIRAN_FORT                                  &
&                 (LFI, IREP,IRANG,CLNOM1(:ILCLN1), &
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
  CLACTI=CLNOM1(:ILCLN1)
  GOTO 1001
ENDIF
!**
!     4.  -  TOUT EST OK... ON EFFECTUE LE CHANGEMENT DE NOM.
!-----------------------------------------------------------------------
!
LFI%CNOMAR(IXC(IARTEX,IRGPIM))=CLNOM2(:ILCLN2)
LFI%LECRPI(IRGPIM,1)=.TRUE.
LFI%NBRENO(IRANG)=LFI%NBRENO(IRANG)+1
!
!        On met a jour ce qui a trait aux acces pseudo-sequentiels...
!
LFI%NDERGF(IRANG)=LFI%JPNAPP*LFI%MFACTM(IRANG)* &
&                  (LFI%MRGPIF(IRGPIM)-1)+IARTEX
LFI%CNDERA(IRANG)=CLNOM2(:ILCLN2)
LFI%NSUIVF(IRANG)=LFI%JPNIL
LFI%NPRECF(IRANG)=LFI%JPNIL
!
IF (.NOT.LFI%LMODIF(IRANG)) THEN
!
!         CAS DE LA PREMIERE MODIFICATION DEPUIS L'OUVERTURE DU FICHIER.
!
  LFI%LMODIF(IRANG)=.TRUE.
  CALL LFIMOE_FORT                         &
&                 (LFI, IREP,IRANG,IRETIN)
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
  LFI%NDEROP(IRANG)=13
  LFI%NDERCO(IRANG)=IREP
   IF (LLVERF) CALL LFIVER_FORT                               &
&                              (LFI, LFI%VERRUE(IRANG),'OFF')
ENDIF
!
IF (LLFATA.OR.IXNIMS (IRANG).EQ.2) THEN
  INIMES=2
ELSE
  IF (LHOOK) CALL DR_HOOK('LFIREN_FORT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='LFIREN'
WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KNUMER='',I3,        &
&       '', CDNOM1='''''',A,'''''', CDNOM2='''''',A,'''''''')') &
&     KREP,KNUMER,CLNOM1(:ILCLN1),CLNOM2(:ILCLN2)
CALL LFIEMS_FORT                                 &
&               (LFI, KNUMER,INIMES,IREP,LLFATA, &
&                CLMESS,CLNSPR,CLACTI)
!
IF (LHOOK) CALL DR_HOOK('LFIREN_FORT',1,ZHOOK_HANDLE)

CONTAINS

#include "lficom2.ixc.h"
#include "lficom2.ixm.h"
#include "lficom2.ixnims.h"
#include "lficom2.llmoer.h"

END SUBROUTINE LFIREN_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFIREN64                      &
&           (KREP, KNUMER, CDNOM1, CDNOM2)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDNOM1                                 ! IN   
CHARACTER (LEN=*)      CDNOM2                                 ! IN   

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIREN_FORT                               &
&           (LFI, KREP, KNUMER, CDNOM1, CDNOM2)

END SUBROUTINE LFIREN64

SUBROUTINE LFIREN                        &
&           (KREP, KNUMER, CDNOM1, CDNOM2)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDNOM1                                 ! IN   
CHARACTER (LEN=*)      CDNOM2                                 ! IN   

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIREN_MT                                &
&           (LFI, KREP, KNUMER, CDNOM1, CDNOM2)

END SUBROUTINE LFIREN

SUBROUTINE LFIREN_MT                          &
&           (LFI, KREP, KNUMER, CDNOM1, CDNOM2)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDNOM1                                 ! IN   
CHARACTER (LEN=*)      CDNOM2                                 ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)

CALL LFIREN_FORT                               &
&           (LFI, IREP, INUMER, CDNOM1, CDNOM2)

KREP       = INT (      IREP, JPLIKM)

END SUBROUTINE LFIREN_MT

!INTF KREP            OUT 
!INTF KNUMER        IN    
!INTF CDNOM1        IN    
!INTF CDNOM2        IN    
