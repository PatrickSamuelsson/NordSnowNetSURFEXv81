! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI

SUBROUTINE LFIPXF_FORT                                     &
&                     (LFI, KREP, KNUMER, KNUMEX, CDCFGX,  &
&                      KLAREX, KXCNEX,                     &
&                      KFACEX, KNUTRA, CDNOMA, KLONG )
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Sous-programme Preparatoire a la realisation d'une
!        "version eXport" d'un Fichier LFI vers un systeme
!        a priori different. La methode utilisee suppose:
!
!     - que les fichiers a acces direct FORTRAN soient implantes ou
!       traitables comme des fichiers non bloques sur le systeme
!       destinataire;
!
!     - que l'on puisse ecrire par WRITE FORTRAN des fichiers
!       non bloques sur le systeme ou est fait la "version export",
!       aussi appelee "fichier export";
!
!     - que la conversion des variables, numeriques voire aussi
!       caracteres, soit faite au niveau des couches d'entrees/sorties
!       FORTRAN sur le systeme ou est fait le fichier export;
!
!     ( dans la pratique, les deux points qui precedent impliquent un
!       parametrage au niveau du langage de controle, a priori )
!
!     - que le programme utilisateur ait ouvert au prealable le fichier
!       LFI dont on veut realiser une "version export", et appelle le
!       sous-programme LFIPXF;
!
!     - que le programme utilisateur specifie le contenu des articles
!       a exporter en termes de types FORTRAN; ceci pouvant se faire
!       de deux manieres, eventuellement combinables:
!
!       1) Si le fichier contient (essentiellement) des donnees
!          utilisateur pouvant se decrire de maniere homogene,
!          par exemple rien que des variables reelles, et doit etre
!          exporte dans la totalite des articles, alors il suffira
!          d'appeler le sous-programme LFIXPH avec la description
!          correspondant a ces articles, que l'on peut aussi voir
!          comme une "description par defaut" (ou implicite);
!
!       2) Si ce n'est pas le cas, ou si une partie des articles ne
!          peut pas etre decrite de la meme maniere que les autres
!          articles, alors il faudra  que le programme utilisateur
!          specifie, pour chacun de ces articles,
!          le contenu en termes de types FORTRAN en appelant le sous-
!          programme LFIXPA: il s'agit la d'une description explicite,
!          ayant precedence sur une eventuelle description implicite;
!
!     - qu'en fin de compte le programme utilisateur appelle le sous-
!       programme LFIXPF qui fabriquera vraiment la version export,
!       a partir des specifications donnees via LFIPXF, LFIXPH, LFIXPA.
!**
!    ARGUMENTS : KREP   (Sortie) ==> Code-Reponse du sous-programme;
!                KNUMER (Entree) ==> Numero d'Unite Logique associe
!                                    au fichier LFI a exporter;
!                KNUMEX (Entree) ==> Numero d'Unite Logique associe
!                                    a la version export a realiser;
!                CDCFGX (Entree) ==> Configuration du systeme
!                                    destinataire du fichier export;
!                KLAREX (Entree) ==> Longueur d'ARticle Elementaire du
!                                    logiciel LFI du systeme destinatai-
!                                    re, exprimee en mots du systeme
!                                    destinataire;
!                                    (LFI%JPLARD du logiciel "distant")
!                KXCNEX (Entree) ==> Nombre maXimum de Caracteres par
!                                    Nom d'article du logiciel LFI du
!                                    systeme destinataire;
!                                    (LFI%JPNCPN du logiciel "distant")
!                KFACEX (Entree) ==> Facteur multiplicatif du fichier
!                                    export;
!                KNUTRA (Entree) ==> Numero d'Unite Logique utilisable
!                                    pour un fichier de travail eventuel
!                                    (si utilisation de LFIXPA), de type
!                                    LFI;
!                CDNOMA (Sortie) ==> Nom du premier article "candidat"
!                                    (potentiel) a l'export;
!                KLONG  (Sortie) ==> Longueur de cet article.
!
!     REMARQUE: Le fichier de travail n'est utilise que si l'on n'a pas
!               assez de place dans les tables pour stocker en memoire
!               les descripteurs. Mais si on en a besoin, il faut penser
!               que ce fichier occupera (temporairement, jusqu'a appel a
!               LFIXPF) une entree dans les tables LFI, et donc ne pas
!               avoir les tables saturees auparavant.
!
!
TYPE(LFICOM) :: LFI
CHARACTER CDNOMA*(*), CLNOMA*(LFI%JPNCPN)
CHARACTER CDCFGX*(*), CLCFGX*(LFI%JPXCCF)
!
INTEGER (KIND=JPLIKB) KREP, KNUMER, KNUMEX, KLAREX 
INTEGER (KIND=JPLIKB) KXCNEX, KFACEX, KNUTRA
INTEGER (KIND=JPLIKB) KLONG, ILCLNO, ILCDNO, IRANMX 
INTEGER (KIND=JPLIKB) ILCFGX, IDECBL, IPOSBL
INTEGER (KIND=JPLIKB) IRANG, IREP, INUMER, INBALO 
INTEGER (KIND=JPLIKB) INTTRU, J, IRANIE, INIMES
INTEGER (KIND=JPLIKB) IRGPIM, IRGPIF, IARTIC, IRETIN, ILCDCF
!
LOGICAL LLVERG, LLVERF, LLEXUL, LLOUVR
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
IF (LHOOK) CALL DR_HOOK('LFIPXF_FORT',0,ZHOOK_HANDLE)
CLACTI=''
CALL LFINUM_FORT                    &
&               (LFI, KNUMER,IRANG)
IREP=0
INUMER=KNUMER
LLVERF=.FALSE.
LLVERG=.FALSE.
ILCDNO=INT (LEN (CDNOMA), JPLIKB)
ILCDCF=INT (LEN (CDCFGX), JPLIKB)
CLNOMA=' '
ILCLNO=1
CLCFGX=' '
ILCFGX=1
KLONG=0
!
IF (MIN (KLAREX,KXCNEX,KFACEX).LE.0) THEN
  IREP=-14
  GOTO 1001
ELSEIF (ILCDNO.LE.0) THEN
  IREP=-15
  CLNOMA=LFI%CHINCO(:LFI%JPNCPN)
  ILCLNO=LFI%JPNCPN
ENDIF
!
IF (ILCDCF.LE.0) THEN
  IREP=-15
  CLCFGX=LFI%CHINCO(:LFI%JPNCPN)
  ILCFGX=LFI%JPNCPN
ENDIF
!
IF (IREP.NE.0) THEN
  GOTO 1001
ELSE
  CDNOMA=' '
ENDIF
!
!        Recherche de la longueur "utile" de la configuration specifiee.
!        (c'est-a-dire sans tenir compte des blancs terminaux eventuels)
!
IDECBL=0
!
101 CONTINUE
IPOSBL=IDECBL+INT (INDEX (CDCFGX(IDECBL+1:),' '), JPLIKB)
!
IF (IPOSBL.LE.IDECBL) THEN
  ILCFGX=ILCDCF
ELSEIF (CDCFGX(IPOSBL:).EQ.' ') THEN
  ILCFGX=IPOSBL-1
ELSE
  IDECBL=IPOSBL
  GOTO 101
ENDIF
!
IF (ILCFGX.LE.LFI%JPXCCF) THEN
  CLCFGX=CDCFGX(:ILCFGX)
ELSE
  CLCFGX=CDCFGX(:LFI%JPXCCF)
  ILCFGX=LFI%JPXCCF
  IREP=-15
  GOTO 1001
ENDIF
!
DO J=0,LFI%JPCFMX
!
IF (CDCFGX.EQ.LFI%CFGMXD(J)) THEN
  IRANMX=J
  GOTO 103
ENDIF      
!
ENDDO
!
!        Configuration du systeme destinataire inconnue ou non prevue.
!
IREP=-32
GOTO 1001
!
103 CONTINUE
!
IF (KXCNEX.GT.LFI%JPXCIE) THEN
  IREP=-33
  GOTO 1001
ENDIF
!
!        Controle de validite FORTRAN et de non ouverture prealable
!        des Numeros d'Unite Logique KNUMEX et KNUTRA.
!
INUMER=KNUMEX
INQUIRE (UNIT=KNUMEX,EXIST=LLEXUL,OPENED=LLOUVR,ERR=901, &
&         IOSTAT=IREP)
CLACTI='EXPORT'
!
IF (.NOT.LLEXUL) THEN
  IREP=-30
  GOTO 1001
ELSEIF (LLOUVR) THEN
  IREP=-34
  GOTO 1001
ENDIF
!
INUMER=KNUTRA
INQUIRE (UNIT=KNUTRA,EXIST=LLEXUL,OPENED=LLOUVR,ERR=901, &
&         IOSTAT=IREP)
!
IF (LFI%LFRANC) THEN
  CLACTI='DE TRAVAIL'
ELSE
  CLACTI='WORK'
ENDIF
!
IF (.NOT.LLEXUL) THEN
  IREP=-30
  GOTO 1001
ELSEIF (LLOUVR) THEN
  IREP=-34
  GOTO 1001
ENDIF
!
INUMER=KNUMER
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
  IREP=-35
  CLACTI='EXPORT'
  GOTO 1001
ELSEIF (LFI%NIMPOR(IRANG).GT.0) THEN
  IREP=-35
  CLACTI='IMPORT'
  GOTO 1001
ENDIF
!
INBALO=LFI%MDES1D(IXM(LFI%JPNALO,IRANG))
INTTRU=LFI%MDES1D(IXM(LFI%JPNTRU,IRANG))+LFI%NBTROU(IRANG)
!
IF (INBALO.EQ.INTTRU) THEN
!
!         Fichier vide de donnees... inexportable.
!
  IREP=-36
  CLACTI='EXPORT'
  GOTO 1001
ENDIF
!
!               Ouverture de l'unite logique KNUMEX.
!
INUMER=KNUMEX
OPEN (UNIT=KNUMEX,STATUS='UNKNOWN',ACCESS='SEQUENTIAL', &
&      FORM='UNFORMATTED',IOSTAT=IREP,ERR=902)
REWIND (UNIT=KNUMEX,IOSTAT=IREP,ERR=906)
INUMER=KNUMER
!**
!     2.  -  RECHERCHE DU PREMIER ARTICLE LOGIQUE DE DONNEES DU FICHIER.
!-----------------------------------------------------------------------
!
!       Reinitialisation des caracteristiques de type "pointeur".
!
LFI%NDERGF(IRANG)=LFI%JPNIL
LFI%CNDERA(IRANG)=' '
LFI%NSUIVF(IRANG)=LFI%JPNIL
LFI%NPRECF(IRANG)=LFI%JPNIL
!
CALL LFICAX_FORT                                       &
&               (LFI, IREP,IRANG,IRGPIM,IARTIC,IRETIN)
!
IF (IRETIN.EQ.1) THEN
  GOTO 903
ELSEIF (IRETIN.EQ.2) THEN
  GOTO 904
ELSEIF (IRETIN.NE.0) THEN
  GOTO 1001
ELSEIF (IARTIC.EQ.0) THEN
  IREP=-16
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
KLONG=LFI%MLGPOS(IXM(IARTIC,IRGPIM))
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
!**
!     3.  -  STOCKAGE DES PARAMETRES D'APPEL DANS LES TABLES.
!-----------------------------------------------------------------------
!
!           VERROUILLAGE GLOBAL EVENTUEL.
!
IF (LFI%LMULTI) CALL LFIVER_FORT                       &
&                                (LFI, LFI%VERGLA,'ON')
LLVERG=LFI%LMULTI
!
IF (LFI%NUIMEX.LT.LFI%JPIMEX) THEN
!
  DO J=1,LFI%JPIMEX
!
  IF (LFI%MNUIEX(J).EQ.LFI%JPNIL) THEN
    IRANIE=J
    LFI%NUIMEX=LFI%NUIMEX+1
    LFI%NINIEX(LFI%NUIMEX)=J
    LFI%MNUIEX(J)=KNUMER
    GOTO 302
  ENDIF
!
  ENDDO
!
  IREP=-16
  GOTO 1001
!
ELSE
!
!        Tables deja pleines...
!
  IREP=-37
  GOTO 1001
ENDIF
!
302 CONTINUE
!
!         Deverrouillage Global eventuel.
!
 IF (LFI%LMULTI) CALL LFIVER_FORT                        &
&                                (LFI, LFI%VERGLA,'OFF')
LLVERG=.FALSE.
!
LFI%NEXPOR(IRANG)=IRANIE
LFI%NAEXPL(IRANIE)=0
LFI%CNIMPL(IRANIE)=' '
LFI%NIMPEX(IRANIE)=KNUMEX
LFI%NUTRAV(IRANIE)=KNUTRA
LFI%NLAPFD(IRANIE)=KLAREX*KFACEX
LFI%NXCNLD(IRANIE)=KXCNEX
LFI%NRCFMX(IRANIE)=IRANMX
!
GOTO 1001
!**
!     9.  - CI-DESSOUS, ETIQUETTES DE BRANCHEMENT EN CAS D'ERREUR E/S.
!-----------------------------------------------------------------------
!
901 CONTINUE
CLACTI='INQUIRE'
GOTO 909
!
902 CONTINUE
CLACTI='OPEN'
GOTO 909
!
903 CONTINUE
CLACTI='WRITE'
GOTO 909
!
904 CONTINUE
CLACTI='READ'
GOTO 909
!
906 CONTINUE
CLACTI='REWIND'
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
 IF (LLVERG) CALL LFIVER_FORT                        &
&                            (LFI, LFI%VERGLA,'OFF')
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
  IF (LHOOK) CALL DR_HOOK('LFIPXF_FORT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='LFIPXF'
  WRITE (UNIT=CLMESS,FMT='(''ARGUMENTS='',I4,2('','',I3),A,   &
&         '','',I5,2('','',I2),'','',I3,A,'','',I6)')          &
&     KREP,KNUMER,KNUMEX,CLCFGX(:ILCFGX),KLAREX,KXCNEX,KFACEX, &
&     KNUTRA,CLNOMA(:ILCLNO),KLONG
CALL LFIEMS_FORT                                 &
&               (LFI, INUMER,INIMES,IREP,LLFATA, &
&                CLMESS,CLNSPR,CLACTI)
!
IF (LHOOK) CALL DR_HOOK('LFIPXF_FORT',1,ZHOOK_HANDLE)

CONTAINS

#include "lficom2.ixc.h"
#include "lficom2.ixm.h"
#include "lficom2.ixnims.h"
#include "lficom2.llmoer.h"

END SUBROUTINE LFIPXF_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFIPXF64                                      &
&           (KREP, KNUMER, KNUMEX, CDCFGX, KLAREX, KXCNEX, &
&           KFACEX, KNUTRA, CDNOMA, KLONG)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  KNUMEX                                 ! IN   
CHARACTER (LEN=*)      CDCFGX                                 ! IN   
INTEGER (KIND=JPLIKB)  KLAREX                                 ! IN   
INTEGER (KIND=JPLIKB)  KXCNEX                                 ! IN   
INTEGER (KIND=JPLIKB)  KFACEX                                 ! IN   
INTEGER (KIND=JPLIKB)  KNUTRA                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 !   OUT
INTEGER (KIND=JPLIKB)  KLONG                                  !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIPXF_FORT                                               &
&           (LFI, KREP, KNUMER, KNUMEX, CDCFGX, KLAREX, KXCNEX, &
&           KFACEX, KNUTRA, CDNOMA, KLONG)

END SUBROUTINE LFIPXF64

SUBROUTINE LFIPXF                                        &
&           (KREP, KNUMER, KNUMEX, CDCFGX, KLAREX, KXCNEX, &
&           KFACEX, KNUTRA, CDNOMA, KLONG)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKM)  KNUMEX                                 ! IN   
CHARACTER (LEN=*)      CDCFGX                                 ! IN   
INTEGER (KIND=JPLIKM)  KLAREX                                 ! IN   
INTEGER (KIND=JPLIKM)  KXCNEX                                 ! IN   
INTEGER (KIND=JPLIKM)  KFACEX                                 ! IN   
INTEGER (KIND=JPLIKM)  KNUTRA                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 !   OUT
INTEGER (KIND=JPLIKM)  KLONG                                  !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIPXF_MT                                                &
&           (LFI, KREP, KNUMER, KNUMEX, CDCFGX, KLAREX, KXCNEX, &
&           KFACEX, KNUTRA, CDNOMA, KLONG)

END SUBROUTINE LFIPXF

SUBROUTINE LFIPXF_MT                                          &
&           (LFI, KREP, KNUMER, KNUMEX, CDCFGX, KLAREX, KXCNEX, &
&           KFACEX, KNUTRA, CDNOMA, KLONG)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKM)  KNUMEX                                 ! IN   
CHARACTER (LEN=*)      CDCFGX                                 ! IN   
INTEGER (KIND=JPLIKM)  KLAREX                                 ! IN   
INTEGER (KIND=JPLIKM)  KXCNEX                                 ! IN   
INTEGER (KIND=JPLIKM)  KFACEX                                 ! IN   
INTEGER (KIND=JPLIKM)  KNUTRA                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 !   OUT
INTEGER (KIND=JPLIKM)  KLONG                                  !   OUT
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  INUMEX                                 ! IN   
INTEGER (KIND=JPLIKB)  ILAREX                                 ! IN   
INTEGER (KIND=JPLIKB)  IXCNEX                                 ! IN   
INTEGER (KIND=JPLIKB)  IFACEX                                 ! IN   
INTEGER (KIND=JPLIKB)  INUTRA                                 ! IN   
INTEGER (KIND=JPLIKB)  ILONG                                  !   OUT
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)
INUMEX     = INT (    KNUMEX, JPLIKB)
ILAREX     = INT (    KLAREX, JPLIKB)
IXCNEX     = INT (    KXCNEX, JPLIKB)
IFACEX     = INT (    KFACEX, JPLIKB)
INUTRA     = INT (    KNUTRA, JPLIKB)

CALL LFIPXF_FORT                                               &
&           (LFI, IREP, INUMER, INUMEX, CDCFGX, ILAREX, IXCNEX, &
&           IFACEX, INUTRA, CDNOMA, ILONG)

KREP       = INT (      IREP, JPLIKM)
KLONG      = INT (     ILONG, JPLIKM)

END SUBROUTINE LFIPXF_MT

!INTF KREP            OUT 
!INTF KNUMER        IN    
!INTF KNUMEX        IN    
!INTF CDCFGX        IN    
!INTF KLAREX        IN    
!INTF KXCNEX        IN    
!INTF KFACEX        IN    
!INTF KNUTRA        IN    
!INTF CDNOMA          OUT 
!INTF KLONG           OUT 
