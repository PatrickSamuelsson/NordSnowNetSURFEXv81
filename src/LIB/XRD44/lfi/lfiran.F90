! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI
SUBROUTINE LFIRAN_FORT                                    &
&                     (LFI, KREP, KRANG, CDNOMA, KRGPIM,  &
&                      KARTEX, KRETIN )
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        SOUS-PROGRAMME *INTERNE* DU LOGICIEL DE FICHIERS INDEXES LFI
!     RECHERCHE D'UN ARTICLE LOGIQUE PAR NOM, DANS UNE UNITE LOGIQUE.
!**
!    ARGUMENTS : KREP   (SORTIE) ==> CODE-REPONSE DU SOUS-PROGRAMME;
!                KRANG  (ENTREE) ==> RANG ( DANS LA TABLE *LFI%NUMERO* )
!                                    DE L'UNITE LOGIQUE CONCERNEE;
!                CDNOMA (ENTREE) ==> NOM DE L'ARTICLE A RECHERCHER;
!                KRGPIM (SORTIE) ==> RANG DANS LES TABLES LFI%CNOMAR,LFI%MLGPOS,
!                                    ETC. DE LA P.P.I OU FIGURE
!                                    L'ARTICLE ( 0 SI PAS TROUVE );
!                KARTEX (SORTIE) ==> RANG ( DANS LA PAGE D'INDEX ) DE L'
!                                    ARTICLE S'IL EXISTE ( 0 SINON );
!                KRETIN (SORTIE) ==> CODE-RETOUR INTERNE.
!
!
TYPE(LFICOM) :: LFI
CHARACTER CDNOMA*(*)
!
INTEGER (KIND=JPLIKB) KREP, KRANG, KRGPIM, KARTEX 
INTEGER (KIND=JPLIKB) ILCDNO, IRANG, IFACTM, INALPP
INTEGER (KIND=JPLIKB) INBALO, INTPPI, IRANGF, IRGPIF 
INTEGER (KIND=JPLIKB) J, ILFORC, INPILE, IRANGM
INTEGER (KIND=JPLIKB) IRGPIM, IARTIC, INPIME, IRPIFN 
INTEGER (KIND=JPLIKB) INPPIM, IDEBEX, INUMER
INTEGER (KIND=JPLIKB) JNPAGE, INALPI, IRETOU, INIMES 
INTEGER (KIND=JPLIKB) KRETIN, IRETIN
INTEGER (KIND=JPLIKB) IEXPLO (LFI%JPNPIA+LFI%JPNPIS) 
!
CHARACTER(LEN=LFI%JPLSPX) CLNSPR
CHARACTER(LEN=LFI%JPLMES) CLMESS
CHARACTER(LEN=LFI%JPLFTX) CLACTI
LOGICAL LLFATA

!**
!     1.  -  PREAMBULES.
!-----------------------------------------------------------------------
!*
!     1.1 - CONTROLES DES PARAMETRES D'APPEL ET INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('LFIRAN_FORT',0,ZHOOK_HANDLE)
CLACTI=''
ILCDNO=INT (LEN (CDNOMA), JPLIKB)
!
IF (KRANG.LE.0.OR.KRANG.GT.LFI%JPNXFI.OR.                      &
&    ILCDNO.LE.0.OR.ILCDNO.GT.LFI%JPNCPN.OR.CDNOMA.EQ.' ') THEN
  KREP=-16
  GOTO 1001
ENDIF
!
IRANG=KRANG
KREP=0
IFACTM=LFI%MFACTM(IRANG)
INALPP=LFI%JPNAPP*IFACTM
INBALO=LFI%MDES1D(IXM(LFI%JPNALO,IRANG))
INTPPI=(INBALO-1+INALPP)/INALPP
IF (LFI%LMISOP)                                              &
&    WRITE (UNIT=LFI%NULOUT,FMT=*)'LFIRAN - INBALO= ',INBALO, &
&                                 ', INTPPI= ',INTPPI
!*
!     1.2 - CAS "ELEMENTAIRES" OU CHANCEUX.
!-----------------------------------------------------------------------
!
IF (INBALO.EQ.0) THEN
!
!          Fichier vide ou depourvu d'articles logiques de donnees.
!
  GOTO 300
!
ELSEIF (LFI%NDERGF(IRANG).NE.LFI%JPNIL         &
&        .AND.LFI%CNDERA(IRANG).EQ.CDNOMA) THEN
!
!          Le dernier article demande via LFINFO (cas le plus probable)
!     ou LFILAS/LFILAP/LFICAS/LFICAP etait celui cherche !
!
  IRANGF=LFI%NDERGF(IRANG)
  IRGPIF=1+(IRANGF-1)/INALPP
!
  IF (IRANGF.LE.INALPP) THEN
    IRGPIM=LFI%MRGPIM(1,IRANG)
  ELSEIF (IRANGF.GT.INALPP*(INTPPI-1)) THEN
    IRGPIM=LFI%MRGPIM(LFI%NPODPI(IRANG),IRANG)
  ELSE
!
    DO J=2,LFI%NPPIMM(IRANG)
    IRGPIM=LFI%MRGPIM(J,IRANG)
    IF (LFI%MRGPIF(IRGPIM).EQ.IRGPIF) GOTO 122
    ENDDO
!
!           MISE EN MEMOIRE DE L'ARTICLE D'INDEX "NOMS" CHERCHE.
!
    ILFORC=1
    INPILE=1
    CALL LFIPIM_FORT                                &
&                   (LFI, KREP,IRANG,IRANGM,IRGPIM, &
&                    IRGPIF,ILFORC,INPILE, IRETIN)
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
122 CONTINUE
  IARTIC=IRANGF-INALPP*(IRGPIF-1)
!
  IF (LFI%CNOMAR(IXC(IARTIC,IRGPIM)).EQ.CDNOMA) THEN
    KRGPIM=IRGPIM
    KARTEX=IARTIC
  ELSE
    KREP=-16
  ENDIF
!
  GOTO 1001
!
ENDIF
!
INPIME=0
IRPIFN=1
INPPIM=LFI%NPPIMM(IRANG)
!
IF (LFI%NPODPI(IRANG).EQ.2) THEN
  IDEBEX=3
ELSE
  IDEBEX=2
ENDIF
!**
!     2.  -  EXPLORATION DES PAGES ET ARTICLES D'INDEX "NOMS",
!            A LA RECHERCHE DE L'ARTICLE LOGIQUE. ( ON COMMENCE
!            PAR EXPLORER LES PAGES D'INDEX )
!-----------------------------------------------------------------------
!
DO JNPAGE=1,INTPPI
!
IF (JNPAGE.LE.INPPIM) THEN
!
!           IL S'AGIT D'UNE EXPLORATION EN MEMOIRE ( PAGE D'INDEX ).
!
  IRGPIM=LFI%MRGPIM(JNPAGE,IRANG)
  IRGPIF=LFI%MRGPIF(IRGPIM)
  INPIME=INPIME+1
  IEXPLO(INPIME)=IRGPIF
  IF (IRGPIF.EQ.(IRPIFN+1)) IRPIFN=IRGPIF
ELSE
!
!           IL S'AGIT D'UNE EXPLORATION "HORS MEMOIRE";
!         ON CHERCHE LA PROCHAINE P.A.I. NON EXPLOREE .
!
  IF (JNPAGE.EQ.INPPIM+1) IRGPIF=IRPIFN
!
201 CONTINUE
  IRGPIF=IRGPIF+1
!
  DO J=IDEBEX,INPIME
  IF (IEXPLO(J).EQ.IRGPIF) GOTO 201
  ENDDO
!
  ILFORC=1
  INPILE=1
  CALL LFIPIM_FORT                                       &
&                 (LFI, KREP,IRANG,IRANGM,IRGPIM,IRGPIF, &
&                  ILFORC,INPILE, IRETIN)
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
INALPI=MIN (INALPP,INBALO-(IRGPIF-1)*INALPP)
!
DO J=1,INALPI
!
IF (LFI%CNOMAR(IXC(J,IRGPIM)).EQ.CDNOMA) THEN
  KRGPIM=IRGPIM
  KARTEX=J
  GOTO 1001
ENDIF
!
ENDDO
!
ENDDO
!
300 CONTINUE
!**
!     3.  -  CAS OU L'ARTICLE N'A PAS ETE TROUVE.
!-----------------------------------------------------------------------
!
KRGPIM=0
KARTEX=0
GOTO 1001
!**
!     9.  - CI-DESSOUS, ETIQUETTES DE BRANCHEMENT EN CAS D'ERREUR E/S.
!      AU CAS OU, ON FORCE LE CODE-REPONSE ENTREE/SORTIE A ETRE POSITIF.
!-----------------------------------------------------------------------
!
903 CONTINUE
IRETOU=1
CLACTI='WRITE'
GOTO 909
!
904 CONTINUE
IRETOU=2
CLACTI='READ'
!
909 CONTINUE
KREP=ABS (KREP)
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
  INUMER=LFI%NUMERO(KRANG)
  INIMES=2
  CLNSPR='LFIRAN'
  WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KRANG='',I3,     &
&  '', CDNOMA='''''',A,'''''', KRGPIM='',I3,'', KARTEX='',I5, &
&  '', KRETIN='',I2)')                                        &
&    KREP,KRANG,CDNOMA,KRGPIM,KARTEX,KRETIN
  CALL LFIEMS_FORT                                  &
&                 (LFI, INUMER,INIMES,KREP,.FALSE., &
&                  CLMESS,CLNSPR,CLACTI)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('LFIRAN_FORT',1,ZHOOK_HANDLE)

CONTAINS

#include "lficom2.ixc.h"
#include "lficom2.ixm.h"
#include "lficom2.llmoer.h"

END SUBROUTINE LFIRAN_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFIRAN64                                     &
&           (KREP, KRANG, CDNOMA, KRGPIM, KARTEX, KRETIN)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KRANG                                  ! IN   
CHARACTER (LEN=*)      CDNOMA                                 ! IN   
INTEGER (KIND=JPLIKB)  KRGPIM                                 !   OUT
INTEGER (KIND=JPLIKB)  KARTEX                                 !   OUT
INTEGER (KIND=JPLIKB)  KRETIN                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIRAN_FORT                                             &
&           (LFI, KREP, KRANG, CDNOMA, KRGPIM, KARTEX, KRETIN)

END SUBROUTINE LFIRAN64

SUBROUTINE LFIRAN                                       &
&           (KREP, KRANG, CDNOMA, KRGPIM, KARTEX, KRETIN)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KRANG                                  ! IN   
CHARACTER (LEN=*)      CDNOMA                                 ! IN   
INTEGER (KIND=JPLIKM)  KRGPIM                                 !   OUT
INTEGER (KIND=JPLIKM)  KARTEX                                 !   OUT
INTEGER (KIND=JPLIKM)  KRETIN                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIRAN_MT                                               &
&           (LFI, KREP, KRANG, CDNOMA, KRGPIM, KARTEX, KRETIN)

END SUBROUTINE LFIRAN

SUBROUTINE LFIRAN_MT                                         &
&           (LFI, KREP, KRANG, CDNOMA, KRGPIM, KARTEX, KRETIN)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KRANG                                  ! IN   
CHARACTER (LEN=*)      CDNOMA                                 ! IN   
INTEGER (KIND=JPLIKM)  KRGPIM                                 !   OUT
INTEGER (KIND=JPLIKM)  KARTEX                                 !   OUT
INTEGER (KIND=JPLIKM)  KRETIN                                 !   OUT
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  IRANG                                  ! IN   
INTEGER (KIND=JPLIKB)  IRGPIM                                 !   OUT
INTEGER (KIND=JPLIKB)  IARTEX                                 !   OUT
INTEGER (KIND=JPLIKB)  IRETIN                                 !   OUT
! Convert arguments

IRANG      = INT (     KRANG, JPLIKB)

CALL LFIRAN_FORT                                             &
&           (LFI, IREP, IRANG, CDNOMA, IRGPIM, IARTEX, IRETIN)

KREP       = INT (      IREP, JPLIKM)
KRGPIM     = INT (    IRGPIM, JPLIKM)
KARTEX     = INT (    IARTEX, JPLIKM)
KRETIN     = INT (    IRETIN, JPLIKM)

END SUBROUTINE LFIRAN_MT

!INTF KREP            OUT 
!INTF KRANG         IN    
!INTF CDNOMA        IN    
!INTF KRGPIM          OUT 
!INTF KARTEX          OUT 
!INTF KRETIN          OUT 
