! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI
SUBROUTINE LFIPXA_FORT                                            &
&                     (LFI, KREP, KNUMER, CDNOMA, CDSTRU, CDSUIV, &
&                    KLSUIV )
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Sous-programme Preparatoire a l'eXport d'un Article d'un
!     fichier LFI vers un systeme a priori different.
!
!     Il s'agit, en l'occurrence, de decrire la structure interne
!     de cet article en termes de types de variables.  
!**
!    ARGUMENTS : KREP   (Sortie) ==> Code-Reponse du sous-programme;
!                KNUMER (Entree) ==> Numero d'Unite Logique associe;
!                CDNOMA (Entree) ==> Nom de l'article decrit;
!                CDSTRU (Entree) ==> Structure interne de cet article;
!                CDSUIV (Sortie) ==> Nom de l'article suivant sur le
!                                    fichier, s'il en existe;
!                KLSUIV (Sortie) ==> Longueur de cet article.
!
!     (s'il n'y a pas d'article suivant, on retourne CDSUIV=' ' et
!      KLSUIV=0)
!
!     Les syntaxes autorisees pour CDSTRU sont decrites dans le sous-
!     programmes *LFIDST*.
!     
!
TYPE(LFICOM) :: LFI
CHARACTER CDNOMA*(*), CDSUIV*(*), CDSTRU*(*)
CHARACTER*(LFI%JPNCPN) CLNOMA, CLSUIV, CLSTRU
!
INTEGER (KIND=JPLIKB) KREP, KNUMER, KLSUIV
INTEGER (KIND=JPLIKB) ILONEX, ILCLNO, ILCDNO, IRANMX 
INTEGER (KIND=JPLIKB) IDECBL, IPOSBL, ILCDST
INTEGER (KIND=JPLIKB) IRANG, IREP, INBALO, IRANIE 
INTEGER (KIND=JPLIKB) INIMES, IARTEX
INTEGER (KIND=JPLIKB) IRGPIM, IRGPIF, IARTIC, IRETIN 
INTEGER (KIND=JPLIKB) ILCDSU, ILCLSU, ILCLST
INTEGER (KIND=JPLIKB) ILUSTR
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
IF (LHOOK) CALL DR_HOOK('LFIPXA_FORT',0,ZHOOK_HANDLE)
CLACTI=''
CALL LFINUM_FORT                    &
&               (LFI, KNUMER,IRANG)
IREP=0
LLVERF=.FALSE.
ILCDNO=INT (LEN (CDNOMA), JPLIKB)
ILCDSU=INT (LEN (CDSUIV), JPLIKB)
ILCDST=INT (LEN (CDSTRU), JPLIKB)
CLNOMA=' '
ILCLNO=1
CLSTRU=' '
ILCLST=1
CLSUIV=' '
ILCLSU=1
KLSUIV=0
!
IF (ILCDNO.LE.0) THEN
  IREP=-15
  CLNOMA=LFI%CHINCO(:LFI%JPNCPN)
  ILCLNO=LFI%JPNCPN
ELSEIF (CDNOMA.EQ.' ') THEN
  IREP=-18
ENDIF
!
IF (ILCDSU.LE.0) THEN
  IREP=-15
  CLSUIV=LFI%CHINCO(:LFI%JPNCPN)
  ILCLSU=LFI%JPNCPN
ENDIF
!
IF (ILCDST.LE.0) THEN
  IREP=-15
  CLSTRU=LFI%CHINCO(:LFI%JPNCPN)
  ILCLST=LFI%JPNCPN
ELSEIF (CDSTRU.EQ.' ') THEN
  IREP=-39
ENDIF
!
IF (IREP.NE.0) THEN
  GOTO 1001
ELSE
  CDSUIV=' '
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
!        Recherche de la longueur "utile" de la structure specifiee.
!        (c'est-a-dire sans tenir compte des blancs terminaux eventuels)
!
IDECBL=0
!
102 CONTINUE
IPOSBL=IDECBL+INT (INDEX (CDSTRU(IDECBL+1:),' '), JPLIKB)
!
IF (IPOSBL.LE.IDECBL) THEN
  ILUSTR=ILCDST
ELSEIF (CDSTRU(IPOSBL:).EQ.' ') THEN
  ILUSTR=IPOSBL-1
ELSE
  IDECBL=IPOSBL
  GOTO 102
ENDIF
!
ILCLST=MIN (ILCLST,ILUSTR)
!
IF (IRANG.EQ.0) THEN
  IREP=-1
  GOTO 1001
ENDIF
!
 IF (LFI%LMULTI) CALL LFIVER_FORT                              &
&                                (LFI, LFI%VERRUE(IRANG),'ON')
LLVERF=LFI%LMULTI
IRANIE=LFI%NEXPOR(IRANG)
!
IF (IRANIE.LE.0) THEN
  IREP=-38
  CLACTI='EXPORT'
  GOTO 1001
ENDIF
!
IRANMX=LFI%NRCFMX(IRANIE)
IARTEX=0
ILONEX=0
INBALO=LFI%MDES1D(IXM(LFI%JPNALO,IRANG))

IF (INBALO.NE.0) THEN
!**
!     2.  -  EXPLORATION DES (PAIRES DE) PAGES ET ARTICLES D'INDEX,
!            A LA RECHERCHE DE L'ARTICLE LOGIQUE DEMANDE.
!-----------------------------------------------------------------------
!
  CALL LFIRAN_FORT                                         &
&                 (LFI, IREP,IRANG,CLNOMA(:ILCLNO),IRGPIM, &
&                  IARTEX,IRETIN)
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
!**
!     8.  -  RECHERCHE DE L'ARTICLE LOGIQUE DE DONNEES SUIVANT.
!-----------------------------------------------------------------------
!
CALL LFICAX_FORT                                       &
&               (LFI, IREP,IRANG,IRGPIM,IARTIC,IRETIN)
!
IF (IRETIN.EQ.1) THEN
  GOTO 903
ELSEIF (IRETIN.EQ.2) THEN
  GOTO 904
ELSEIF (IRETIN.NE.0.OR.IARTIC.EQ.0) THEN
  GOTO 1001
ENDIF
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
KLSUIV=LFI%MLGPOS(IXM(IARTIC,IRGPIM))
CLSUIV=LFI%CNOMAR(IXC(IARTIC,IRGPIM))
!
!        Recherche de la longueur "utile" du nom d'article.
!        (c'est-a-dire sans tenir compte des blancs terminaux eventuels)
!
IDECBL=0
!
811 CONTINUE
IPOSBL=IDECBL+INT (INDEX (CLSUIV(IDECBL+1:),' '), JPLIKB)
!
IF (IPOSBL.LE.IDECBL) THEN
  ILCLSU=LFI%JPNCPN
ELSEIF (CLSUIV(IPOSBL:).EQ.' ') THEN
  ILCLSU=IPOSBL-1
ELSE
  IDECBL=IPOSBL
  GOTO 811
ENDIF
!
IF (ILCDSU.GE.ILCLSU) THEN
  CDSUIV=CLSUIV(:ILCLNO)
ELSE
  IREP=-24
  CLACTI=CLSUIV
  GOTO 1001
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
  LFI%NDEROP(IRANG)=22
  LFI%NDERCO(IRANG)=IREP
   IF (LLVERF) CALL LFIVER_FORT                               &
&                              (LFI, LFI%VERRUE(IRANG),'OFF')
ENDIF
!
IF (LLFATA.OR.IXNIMS (IRANG).EQ.2) THEN
  INIMES=2
ELSE
  IF (LHOOK) CALL DR_HOOK('LFIPXA_FORT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='LFIPXA'
WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KNUMER='',I3, &
&       '', CDNOMA='''''',A,'''''', CDSTRU='''''',A,     &
&       '''''', CDSUIV='''''',A,'''''', KLSUIV='',I7)')  &
&     KREP,KNUMER,CLNOMA(:ILCLNO),CLSTRU(:ILCLST),       &
&     CLSUIV(:ILCDSU),KLSUIV
CALL LFIEMS_FORT                                        &
&               (LFI, KNUMER,INIMES,IREP,LLFATA,CLMESS, &
&                CLNSPR,CLACTI)
!
IF (LHOOK) CALL DR_HOOK('LFIPXA_FORT',1,ZHOOK_HANDLE)

CONTAINS

#include "lficom2.ixc.h"
#include "lficom2.ixm.h"
#include "lficom2.ixnims.h"
#include "lficom2.llmoer.h"

END SUBROUTINE LFIPXA_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFIPXA64                                      &
&           (KREP, KNUMER, CDNOMA, CDSTRU, CDSUIV, KLSUIV)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 ! IN   
CHARACTER (LEN=*)      CDSTRU                                 ! IN   
CHARACTER (LEN=*)      CDSUIV                                 !   OUT
INTEGER (KIND=JPLIKB)  KLSUIV                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIPXA_FORT                                               &
&           (LFI, KREP, KNUMER, CDNOMA, CDSTRU, CDSUIV, KLSUIV)

END SUBROUTINE LFIPXA64

SUBROUTINE LFIPXA                                        &
&           (KREP, KNUMER, CDNOMA, CDSTRU, CDSUIV, KLSUIV)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 ! IN   
CHARACTER (LEN=*)      CDSTRU                                 ! IN   
CHARACTER (LEN=*)      CDSUIV                                 !   OUT
INTEGER (KIND=JPLIKM)  KLSUIV                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIPXA_MT                                                &
&           (LFI, KREP, KNUMER, CDNOMA, CDSTRU, CDSUIV, KLSUIV)

END SUBROUTINE LFIPXA

SUBROUTINE LFIPXA_MT                                          &
&           (LFI, KREP, KNUMER, CDNOMA, CDSTRU, CDSUIV, KLSUIV)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 ! IN   
CHARACTER (LEN=*)      CDSTRU                                 ! IN   
CHARACTER (LEN=*)      CDSUIV                                 !   OUT
INTEGER (KIND=JPLIKM)  KLSUIV                                 !   OUT
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  ILSUIV                                 !   OUT
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)

CALL LFIPXA_FORT                                               &
&           (LFI, IREP, INUMER, CDNOMA, CDSTRU, CDSUIV, ILSUIV)

KREP       = INT (      IREP, JPLIKM)
KLSUIV     = INT (    ILSUIV, JPLIKM)

END SUBROUTINE LFIPXA_MT

!INTF KREP            OUT 
!INTF KNUMER        IN    
!INTF CDNOMA        IN    
!INTF CDSTRU        IN    
!INTF CDSUIV          OUT 
!INTF KLSUIV          OUT 
