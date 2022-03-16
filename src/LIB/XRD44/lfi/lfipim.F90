! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI
SUBROUTINE LFIPIM_FORT                                    &
&                     (LFI, KREP ,KRANG, KRANGM, KRGPIM,  &
&                      KRGPIF, KRGFOR, KNPILE, KRETIN )
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        SOUS-PROGRAMME *INTERNE* DU LOGICIEL DE FICHIERS INDEXES LFI
!     GESTION DES REQUETES D'ALLOCATION D'UNE PAIRE DE PAGES D'INDEX
!     SUPPLEMENTAIRE ( APPELS PAR LFIECR, LFILEC... ), ET LECTURE
!     EVENTUELLE SUR FICHIER D'ARTICLE(S) D'INDEX CORRESPONDANT(S) .
!**
!    ARGUMENTS : KREP   (SORTIE) ==> CODE-REPONSE DU SOUS-PROGRAMME;
!                KRANG  (ENTREE) ==> RANG ( DANS LA TABLE *LFI%NUMERO* )
!                                    DE L'UNITE LOGIQUE CONCERNEE;
!                KRANGM (SORTIE) ==> RANG ( DANS LA TABLE *LFI%MRGPIM* )
!                                    DE LA P.P.I. AFFECTEE;
!                                    ( ZERO SI PAS DE P.P.I. ALLOUEE )
!                KRGPIM (SORTIE) ==> RANG ( DANS LES TABLES DECRIVANT
!                                    LES PAIRES DE PAGES D'INDEX ) DE
!                                    LA P.P.I. SUPPLEMENTAIRE ALLOUEE,
!                                    ZERO SI PAS DE P.P.I. ALLOUEE;
!                KRGPIF (ENTREE) ==> RANG ( DANS LE FICHIER ) DE LA
!                                    P.P.I. SUPPLEMENTAIRE;
!                KRGFOR (ENTREE) ==> RANG ( DANS LE FICHIER ) D'UNE
!                                    EVENTUELLE P.P.I. A CONSERVER;
!                KNPILE (ENTREE) ==> NOMBRE D'ARTICLES D'INDEX A LIRE
!                                    ( 0==>RIEN, 1==>NOMS, 2==>LES 2 );
!                KRETIN (SORTIE) ==> CODE-RETOUR INTERNE.
!
!     SI L'ON NE TROUVE PLUS DE P.P.I. LIBRE, ON "RECYCLE" LA P.P.I.
!     ASSOCIEE AU PLUS GRAND RANG DANS LA TABLE *LFI%MRGPIM*,
!         EXCEPTION FAITE DE LA PREMIERE, DE LA DERNIERE, ET DE CELLE
!     DE RANG *KRGFOR* DANS LE FICHIER,
!     CECI POUR ASSURER QUE LORS D'UNE EXPLORATION DES P.P.I.,
!     ON NE REUTILISE PAS L'EMPLACEMENT D'UNE P.P.I. QU'ON A DEJA
!     EXPLOREE, OU QU'ON DOIT GARDER POUR LA LOGIQUE DU TRAITEMENT.
!        LE "FORCAGE" N'EST EFFECTIF QUE SI LE RANG DANS LE FICHIER EST
!     BIEN CELUI D'UNE P.P.I, MAIS NI LA PREMIERE NI LA DERNIERE
!     EN RANG DANS LE FICHIER.
!*
!         LE VERROUILLAGE EVENTUEL DE L'UNITE LOGIQUE DE RANG *KRANG*
!     DOIT AVOIR ETE FAIT EN AMONT, AVANT L'APPEL A CE SOUS-PROGRAMME.
!
!        Noter que la P.P.I. peut etre "multiple", et qu'elle occupe
!     autant de P.P.I. "elementaires" (de longueur LFI%JPLARD par page) ,
!     ces P.P.I. "elementaires" etant necessairement consecutives...
!        Dans ce cas KRGPIM designe le rang de la PREMIERE P.P.I. ele-
!     mentaire concernee.
!
!
TYPE(LFICOM) :: LFI
INTEGER (KIND=JPLIKB) KREP ,KRANG, KRANGM, KRGPIM 
INTEGER (KIND=JPLIKB) KRGPIF, KRGFOR, KNPILE
INTEGER (KIND=JPLIKB) IRANG, INUMER, INPPIM, IFACTM 
INTEGER (KIND=JPLIKB) IRGPIM, IRANGM, ICOMPT, J
INTEGER (KIND=JPLIKB) JR, IREC, INAPHY, IRETOU 
INTEGER (KIND=JPLIKB) INIMES, KRETIN, IRETIN
!
LOGICAL LLAUX1, LLAUX2, LLADON
!
CHARACTER(LEN=LFI%JPLSPX) CLNSPR
CHARACTER(LEN=LFI%JPLMES) CLMESS
CHARACTER(LEN=LFI%JPLFTX) CLACTI
LOGICAL LLFATA

!**
!     1.  -  CONTROLES DES PARAMETRES D'APPEL, PUIS INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('LFIPIM_FORT',0,ZHOOK_HANDLE)
CLACTI=''
KRANGM=0
KRGPIM=0
IRETOU=0
INAPHY=0
LLADON=.FALSE.
!
IF (KRANG.LE.0.OR.KRANG.GT.LFI%JPNXFI.OR.KRGPIF.LE.0.OR. &
&    KNPILE.LT.0.OR.KNPILE.GT.2) THEN
  KREP=-16
  GOTO 1001
ELSE
  IRANG=KRANG
  INUMER=LFI%NUMERO(KRANG)
!
  IF (INUMER.LT.0) THEN
    KREP=-16
    GOTO 1001
  ENDIF
!
ENDIF
!
INPPIM=LFI%NPPIMM(IRANG)
IFACTM=LFI%MFACTM(IRANG)
!**
!     2.  -  RECHERCHE D'UNE P.P.I. SUPPLEMENTAIRE .
!-----------------------------------------------------------------------
!
IF (INPPIM.LE.0) THEN
  KREP=-16
  GOTO 1001
ELSEIF (INPPIM.LT.LFI%JPNPIA) THEN
!*
!     2.1 - CAS OU L'UNE DES P.P.I. PREALLOUEES AU FICHIER EST
!           DISPONIBLE.
!-----------------------------------------------------------------------
!
  IRGPIM=IRANG+INPPIM*LFI%JPNXFI
  INPPIM=INPPIM+1
  IRANGM=INPPIM
  LFI%NPPIMM(IRANG)=INPPIM
  GOTO 300
ELSEIF (LFI%JPNPIS.GT.0) THEN
!*
!     2.2 - PLUS DE P.P.I. PREALLOUEE LIBRE; RECHERCHE DANS
!           LES P.P.I. ALLOUABLES DYNAMIQUEMENT.
!-----------------------------------------------------------------------
!
!           VERROUILLAGE EVENTUEL POUR L'UTILISATION DE *LFI%NPISAF*
!
   IF (LFI%LMULTI) CALL LFIVER_FORT                       &
&                                  (LFI, LFI%VERGLA,'ON')
!
  IF (LFI%NPISAF.LT.LFI%JPNPIS) THEN
    ICOMPT=0
!
    DO J=LFI%JPNPIA*LFI%JPNXFI+1,LFI%JPNXPI
!
    IF (LFI%MCOPIF(J).EQ.LFI%JPNIL) THEN
      ICOMPT=ICOMPT+1
!
      IF (ICOMPT.EQ.IFACTM) THEN
        IRGPIM=J+1-IFACTM
        GOTO 222
      ENDIF
!
    ELSE
      ICOMPT=0
    ENDIF
!
    ENDDO
!
!              Chou blanc... on deverrouille globalement.
!
     IF (LFI%LMULTI) CALL LFIVER_FORT                        &
&                                    (LFI, LFI%VERGLA,'OFF')
    IF (IFACTM.GT.1) GOTO 230
!
!              CAS D'INCOHERENCE DES TABLES DU LOGICIEL !
!
    KREP=-16
    GOTO 1001
!
222 CONTINUE
!
!            UNE P.P.I. "LIBRE", eventuellement Multiple, A ETE TROUVEE.
!
    LFI%NPISAF=LFI%NPISAF+IFACTM
!
    DO JR=IRGPIM,IRGPIM+IFACTM-1
    LFI%MCOPIF(JR)=IRANG
    ENDDO
!
!         ON DEVERROUILLE "GLOBALEMENT", CAR CE QUI SUIT ALORS
!         NE CONCERNE PLUS QUE LE FICHIER.
!
     IF (LFI%LMULTI) CALL LFIVER_FORT                        &
&                                    (LFI, LFI%VERGLA,'OFF')
    INPPIM=INPPIM+1
    IRANGM=INPPIM
    LFI%NPPIMM(IRANG)=INPPIM
    LFI%MRGPIM(INPPIM,IRANG)=IRGPIM
    GOTO 300
!
  ELSE
!
!            CAS OU IL N'Y A PLUS DE P.P.I. "LIBRE" .
!         ON DEVERROUILLE "GLOBALEMENT", CAR CE QUI SUIT ALORS
!         NE CONCERNE PLUS QUE LE FICHIER.
!
     IF (LFI%LMULTI) CALL LFIVER_FORT                        &
&                                    (LFI, LFI%VERGLA,'OFF')
  ENDIF
!
ENDIF
!
230 CONTINUE
!*
!     2.3 - PLUS DE P.P.I. ( PREALLOUEE OU NON ) LIBRE.
!-----------------------------------------------------------------------
!
!         ON VA DONC RECYCLER LA P.P.I. ALLOUEE AU FICHIER
!       QUI SOIT ASSOCIEE AU PLUS GRAND RANG DANS LA TABLE *LFI%MRGPIM*,
!       TOUT EN N'ETANT NI LA PREMIERE, NI LA DERNIERE, NI CELLE DE
!       RANG *KRGFOR* DANS LE FICHIER .
!        C'est pour etre sur de trouver une telle page que l'on a besoin
!     d'avoir LFI%JPNPIA superieur ou egal a 4.
!
IRANGM=INPPIM
!
231 CONTINUE
!
IF (IRANGM.EQ.LFI%NPODPI(IRANG).OR.                      &
&    LFI%MRGPIF(LFI%MRGPIM(IRANGM,IRANG)).EQ.KRGFOR) THEN
  IRANGM=IRANGM-1
  GOTO 231
ENDIF
!
IRGPIM=LFI%MRGPIM(IRANGM,IRANG)
LLAUX1=LFI%LECRPI(IRGPIM,1)
LLAUX2=LFI%LECRPI(IRGPIM,2).AND.LFI%LPHASP(IRGPIM)
!
!          SI NECESSAIRE, ON REECRIT SUR LE FICHIER
!       LA OU LES PAGES D'INDEX qu'on va reutiliser.
!
IF (LLAUX1.OR.LLAUX2) THEN
  CALL LFIREC_FORT                                     &
&                 (LFI, LFI%MRGPIF(IRGPIM),IRANG,IREC)
!
  IF (LLAUX1) THEN
    INAPHY=IREC
    CALL LFIECC_FORT                                      &
&                   (LFI, KREP,INUMER,IREC,             &
&                    LFI%CNOMAR(IXC(1_JPLIKB ,IRGPIM)), &
&                    LFI%NBWRIT(IRANG),IFACTM,          &
&                    LFI%YLFIC (IRANG),IRETIN)
!
    IF (IRETIN.NE.0) THEN
      GOTO 903
    ENDIF
!
    INAPHY=0
  ENDIF
!
  IF (LLAUX2) THEN
    CALL LFIECX_FORT                                      &
&                   (LFI, KREP,IRANG,IREC+1,            &
&                    LFI%MLGPOS(IXM(1_JPLIKB ,IRGPIM)), &
&                    LLADON, IRETIN)
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
ENDIF
!
300 CONTINUE
!**
!     3.  -  MISE A JOUR DE TABLES, NE NECESSITANT PAS LA PROTECTION DU
!            VERROU GLOBAL; A PARTIR DU MOMENT OU *LFI%MCOPIF* A ETE MIS A
!            JOUR, LE SIMPLE VERROUILLAGE DE L'UNITE LOGIQUE SUFFIT.
!             ( ET EST CENSE AVOIR ETE FAIT DANS LE SOUS-PROGRAMME
!               APPELANT, EN MODE MULTI )
!-----------------------------------------------------------------------
!
LFI%LECRPI(IRGPIM,1)=.FALSE.
LFI%LECRPI(IRGPIM,2)=.FALSE.
LFI%LPHASP(IRGPIM)=.FALSE.
LFI%MRGPIF(IRGPIM)=KRGPIF
!**
!     4.  -  MISE EN MEMOIRE EVENTUELLE D'ARTICLE(S) D'INDEX
!            CORRESPONDANT A LA NOUVELLE P.P.I.
!-----------------------------------------------------------------------
!
IF (KNPILE.NE.0) THEN
  CALL LFIREC_FORT                         &
&                 (LFI, KRGPIF,IRANG,IREC)
  INAPHY=IREC
  CALL LFILCC_FORT                                      &
&                 (LFI, KREP,INUMER,IREC,             &
&                  LFI%CNOMAR(IXC(1_JPLIKB ,IRGPIM)), &
&                  LFI%NBREAD(IRANG),IFACTM,          &
&                  LFI%YLFIC (IRANG),IRETIN)
!
  IF (IRETIN.NE.0) THEN
    GOTO 904
  ENDIF
!
  IF (KNPILE.EQ.2) THEN
!
!             PHASAGE DIRECT, SANS APPEL AU SOUS-PROGRAMME "LFIPHA" .
!
    INAPHY=IREC+1
    CALL LFILDO_FORT                                      &
&                   (LFI, KREP,INUMER,IREC+1,           &
&                    LFI%MLGPOS(IXM(1_JPLIKB ,IRGPIM)), &
&                    LFI%NBREAD(IRANG),IFACTM,          &
&                    LFI%YLFIC (IRANG),IRETIN)
!
    IF (IRETIN.NE.0) THEN
      GOTO 904
    ENDIF
!
    LFI%LPHASP(IRGPIM)=.TRUE.
!
  ENDIF
!
ENDIF
!
KREP=0
KRANGM=IRANGM
KRGPIM=IRGPIM
GOTO 1001
!**
!     9.  - CI-DESSOUS, ETIQUETTES DE BRANCHEMENT EN CAS D'ERREUR E/S.
!-----------------------------------------------------------------------
!
903 CONTINUE
IRETOU=1
CLACTI='WRITE'
GOTO 909
!
904 CONTINUE
!
!        "DESALLOCATION" DE LA P.P.I. SUITE A ERREUR EN LECTURE
!     DE L'ARTICLE D'INDEX "NOMS".
!
IF (INPPIM.GT.LFI%JPNPIA) THEN
   IF (LFI%LMULTI) CALL LFIVER_FORT                       &
&                                  (LFI, LFI%VERGLA,'ON')
  LFI%NPISAF=LFI%NPISAF-IFACTM
!
  DO JR=IRGPIM,IRGPIM+IFACTM-1
  LFI%MCOPIF(JR)=LFI%JPNIL
  ENDDO
!
   IF (LFI%LMULTI) CALL LFIVER_FORT                        &
&                                  (LFI, LFI%VERGLA,'OFF')
ENDIF
!
LFI%NPPIMM(IRANG)=INPPIM-1
IRETOU=2
CLACTI='READ'
!
909 CONTINUE
!
!       ON FORCE LE CODE-REPONSE A ETRE POSITIF.
!
KREP=ABS (KREP)
IF (INAPHY.NE.0) LFI%NUMAPH(IRANG)=INAPHY
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE INTERNE EVENTUELLE,
!            VIA LE SOUS-PROGRAMME "LFIEMS", PUIS RETOUR.
!-----------------------------------------------------------------------
!
1001 CONTINUE
LLFATA=LLMOER (KREP,KRANG)
!
IF (KREP.EQ.0) THEN
  KRETIN=0
ELSEIF (KREP.GT.0) THEN
  KRETIN=IRETOU
ELSE
  KRETIN=3
ENDIF
!
IF (LFI%LMISOP.OR.LLFATA) THEN
  INIMES=2
  CLNSPR='LFIPIM'
  WRITE (UNIT=CLMESS,FMT='(''KREP='',I5,'', KRANG='',I3,     &
&       '', KRANGM='',I3,'', KRGPIM='',I3,'', KRGPIF='',I4,   &
&       '', KRGFOR='',I4,'', KNPILE='',I2,'', KRETIN='',I2)') &
&    KREP,KRANG,KRANGM,KRGPIM,KRGPIF,KRGFOR,KNPILE,KRETIN
  CALL LFIEMS_FORT                                  &
&                 (LFI, INUMER,INIMES,KREP,.FALSE., &
&                  CLMESS,CLNSPR,CLACTI)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('LFIPIM_FORT',1,ZHOOK_HANDLE)

CONTAINS

#include "lficom2.ixc.h"
#include "lficom2.ixm.h"
#include "lficom2.llmoer.h"

END SUBROUTINE LFIPIM_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFIPIM64                                     &
&           (KREP, KRANG, KRANGM, KRGPIM, KRGPIF, KRGFOR, &
&           KNPILE, KRETIN)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KRANG                                  ! IN   
INTEGER (KIND=JPLIKB)  KRANGM                                 !   OUT
INTEGER (KIND=JPLIKB)  KRGPIM                                 !   OUT
INTEGER (KIND=JPLIKB)  KRGPIF                                 ! IN   
INTEGER (KIND=JPLIKB)  KRGFOR                                 ! IN   
INTEGER (KIND=JPLIKB)  KNPILE                                 ! IN   
INTEGER (KIND=JPLIKB)  KRETIN                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIPIM_FORT                                             &
&           (LFI, KREP, KRANG, KRANGM, KRGPIM, KRGPIF, KRGFOR, &
&           KNPILE, KRETIN)

END SUBROUTINE LFIPIM64

SUBROUTINE LFIPIM                                       &
&           (KREP, KRANG, KRANGM, KRGPIM, KRGPIF, KRGFOR, &
&           KNPILE, KRETIN)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KRANG                                  ! IN   
INTEGER (KIND=JPLIKM)  KRANGM                                 !   OUT
INTEGER (KIND=JPLIKM)  KRGPIM                                 !   OUT
INTEGER (KIND=JPLIKM)  KRGPIF                                 ! IN   
INTEGER (KIND=JPLIKM)  KRGFOR                                 ! IN   
INTEGER (KIND=JPLIKM)  KNPILE                                 ! IN   
INTEGER (KIND=JPLIKM)  KRETIN                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIPIM_MT                                               &
&           (LFI, KREP, KRANG, KRANGM, KRGPIM, KRGPIF, KRGFOR, &
&           KNPILE, KRETIN)

END SUBROUTINE LFIPIM

SUBROUTINE LFIPIM_MT                                         &
&           (LFI, KREP, KRANG, KRANGM, KRGPIM, KRGPIF, KRGFOR, &
&           KNPILE, KRETIN)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KRANG                                  ! IN   
INTEGER (KIND=JPLIKM)  KRANGM                                 !   OUT
INTEGER (KIND=JPLIKM)  KRGPIM                                 !   OUT
INTEGER (KIND=JPLIKM)  KRGPIF                                 ! IN   
INTEGER (KIND=JPLIKM)  KRGFOR                                 ! IN   
INTEGER (KIND=JPLIKM)  KNPILE                                 ! IN   
INTEGER (KIND=JPLIKM)  KRETIN                                 !   OUT
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  IRANG                                  ! IN   
INTEGER (KIND=JPLIKB)  IRANGM                                 !   OUT
INTEGER (KIND=JPLIKB)  IRGPIM                                 !   OUT
INTEGER (KIND=JPLIKB)  IRGPIF                                 ! IN   
INTEGER (KIND=JPLIKB)  IRGFOR                                 ! IN   
INTEGER (KIND=JPLIKB)  INPILE                                 ! IN   
INTEGER (KIND=JPLIKB)  IRETIN                                 !   OUT
! Convert arguments

IRANG      = INT (     KRANG, JPLIKB)
IRGPIF     = INT (    KRGPIF, JPLIKB)
IRGFOR     = INT (    KRGFOR, JPLIKB)
INPILE     = INT (    KNPILE, JPLIKB)

CALL LFIPIM_FORT                                             &
&           (LFI, IREP, IRANG, IRANGM, IRGPIM, IRGPIF, IRGFOR, &
&           INPILE, IRETIN)

KREP       = INT (      IREP, JPLIKM)
KRANGM     = INT (    IRANGM, JPLIKM)
KRGPIM     = INT (    IRGPIM, JPLIKM)
KRETIN     = INT (    IRETIN, JPLIKM)

END SUBROUTINE LFIPIM_MT

!INTF KREP            OUT 
!INTF KRANG         IN    
!INTF KRANGM          OUT 
!INTF KRGPIM          OUT 
!INTF KRGPIF        IN    
!INTF KRGFOR        IN    
!INTF KNPILE        IN    
!INTF KRETIN          OUT 
