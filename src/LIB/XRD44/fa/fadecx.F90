! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FADECX_FORT                                          &
&                     (FA, KREP, KRANG, CDNOMA, KVALCO, KLONGA, &
&                      PCHAMP, LDCOSP, CDPREF, KNIVAU, CDSUFF,  &
&                      LDUNDF, PUNDF, YDGR1TAB)
USE FA_MOD, ONLY : FA_COM, JPNIIL, FAGR1TAB
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!      Sous-programme INTERNE du logiciel de Fichiers ARPEGE:
!      Controle de coherence et decodage (GRIBEX) d'un CHAMP
!      HORIZONTAL venant d'etre lu sur un fichier ARPEGE/ALADIN.
!       ( DECodage d'un champ gribeX )
!**
!    Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                KRANG  (Entree) ==> Rang de l'unite logique;
!                CDNOMA (Entree) ==> Nom d'article (prefabrique);
!    ( Tableau ) KVALCO (Entree) ==> Donnees issues de la lecture;
!                KLONGA (Entree) ==> Nombre de mots lus;
!    ( Tableau ) PCHAMP (Sortie) ==> Valeurs REELLES du champ lu;
!                LDCOSP (Entree) ==> Vrai si le champ est represente
!                                    par des coefficients spectraux;
!                CDPREF (Entree) ==> Prefixe au sens FA
!                KNIVAU (Entree) ==> Niveau au sens FA
!                CDSUFF (Entree) ==> Suffixe au sens FA
!                LDUNDF (Entree) ==> Si ce champ a des valeurs indefinies
!                                    alors inserer PUNDF sur les points
!                                    manquants
!                PUNDF  (Entree) ==> Dans le cas ou LDUNDF est vrai,
!                                    valeur non definie a inserer dans le champ
!                LDUNDF (Sortie) ==> Vrai si ce champ a des valeurs 
!                                    indefinies
!                PUNDF  (Sortie) ==> Dans le cas ou LDUNDF est vrai (en sortie),
!                                    valeur non definie a inserer dans le champ
!*
!  MODIFICATION :
!         JM AUDOIN  15/05/2007  Partie 3.1  Blindage controle changement unite
!
!
!
TYPE(FA_COM)   :: FA
TYPE(FAGR1TAB) :: YDGR1TAB
INTEGER (KIND=JPLIKB) KREP, KRANG, KLONGA, KNIVAU
!
INTEGER (KIND=JPLIKB), TARGET :: KVALCO(KLONGA)
REAL (KIND=JPDBLR) PCHAMP(*)
INTEGER (KIND=JPLIKB), POINTER :: IVALCO (:)
!
REAL (KIND=JPDBLR) PUNDF
!
LOGICAL LDCOSP, LDUNDF, LLUNDF, LLSWAP
!
CHARACTER CDNOMA*(*), CDPREF*(*), CDSUFF*(*)
!
#include "fagribex.h"
!
REAL (KIND=JPDBLR) ZSEC2(10+2*(FA%JPXNIV+1)), ZSEC3(2)
REAL (KIND=JPDBLR), ALLOCATABLE ::  ZSEC4(:), ZCHAMP(:)
REAL (KIND=JPDBLR) ZUNDF
REAL (KIND=JPDBLR) ZPULAP
!
INTEGER (KIND=JPLIKB) ISEC0(2), ISEC1(FA%JPSEC1)
INTEGER (KIND=JPLIKB) ISEC2(FA%JPSEC2), ISEC3(2)
INTEGER (KIND=JPLIKB) ISEC4(FA%JPSEC4)
INTEGER (KIND=JPLIKB) ILCHAM, ISTRIA, IDECAL
INTEGER (KIND=JPLIKB) IPOFIN, ILONSEC2
INTEGER (KIND=JPLIKB) ITRONC, IIND, ILOW, IHIGH
INTEGER (KIND=JPLIKB) IL, IADD, IRANGC, IILCHAM
INTEGER (KIND=JPLIKB) INIMES
INTEGER (KIND=JPLIKB) IVALC3, IVALC4, IVALC5, IWORD
INTEGER (KIND=JPLIKB) INUMER, ILENG, IRET, IDX
INTEGER (KIND=JPLIKB) JN, JM, JLAT, JLON, J
INTEGER (KIND=JPLIKB) IFAORI, IFAMOD 
INTEGER (KIND=JPLIKB) INIPAR (8)
!
LOGICAL LLMLAM, LLCOSP
!
CHARACTER(LEN=1) CLOPER
CHARACTER(LEN=8) CLGRIB
!
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA
TYPE (FAGR1TAB)          YLGR1TAB

!**
!     1.  -  CONTROLES ET INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FADECX_MT',0,ZHOOK_HANDLE)
KREP=0
IF (KRANG.LE.0.OR.KRANG.GT.FA%JPNXFA) THEN
  KREP=-66
  GOTO 1001
ENDIF
!
INUMER=FA%FICHIER(KRANG)%NULOGI
!
CLOPER='D'
ISTRIA=0
!**
!     2.  -  CONTROLE DES DONNEES DE L'ARTICLE
!-----------------------------------------------------------------------
!
IF (KVALCO(1).NE.3.OR.                         &
&    KVALCO(2).LT.0.OR.KVALCO(2).GT.1.OR.      &
&    (KVALCO(2).EQ.1.AND.KVALCO(4).LT.0)) THEN
  KREP=-91
  GOTO 1001
ELSE
  LLCOSP=KVALCO(2).EQ.1
ENDIF
!
IF ((LLCOSP.AND..NOT.LDCOSP).OR.(.NOT.LLCOSP.AND.LDCOSP)) THEN
  KREP=-92
  GOTO 1001
ENDIF
!
IRANGC=FA%FICHIER(KRANG)%NUCADR
LLMLAM=FA%CADRE(IRANGC)%LIMLAM
ITRONC=FA%CADRE(IRANGC)%MTRONC
!
IF (LDCOSP) THEN
  IF (LLMLAM) THEN
    ILCHAM=FA%CADRE(IRANGC)%NSFLAM
    ILONSEC2=21+ITRONC
  ELSE    
    ILCHAM=(1+ITRONC)*(2+ITRONC)
    ILONSEC2=22
  ENDIF   
ELSE
  ILCHAM=FA%CADRE(IRANGC)%NVAPDG
  IF (LLMLAM) THEN
    ILONSEC2=22
  ELSE
    ILONSEC2=22+FA%CADRE(IRANGC)%NLATIT
  ENDIF
ENDIF
!
ALLOCATE (ZCHAMP(ILCHAM), ZSEC4(ILCHAM))
!
!**
!     3.  -  DECODAGE GRIBEX DES DONNEES DE L'ARTICLE
!-----------------------------------------------------------------------
!
IDECAL=3
IVALC3=KVALCO(3)
IF (LDCOSP) THEN
  IDECAL=IDECAL+2
! IVALC4=ss-tronc non compactee
! IVALC5=puissance de laplacien
  IVALC4=KVALCO(4)
  IVALC5=KVALCO(5)
ENDIF

IILCHAM=ILCHAM
!
! Pour Aladin, le calcul du nb de coeff spectraux qui ont
! ete compactes est plus complexe (certains ont ete retires
! pour ne pas etre compactes: ss-tronc triangulaire).
!
IF (LDCOSP.AND.LLMLAM) THEN
  ISTRIA=4*(1+FA%CADRE(IRANGC)%NOZPAR(1)+FA%CADRE(IRANGC)%NOZPAR(2)+ &
&            IVALC4*(IVALC4-1)/2)
  IILCHAM=ILCHAM-ISTRIA
ENDIF

! C'est un champ GRIB, mais les octets ont peut-etre ete 
! inverses s'il a ete produit sur une architecture differente
! On cherche donc a deviner s'il faut les inverser a nouveau,
! et on inverse le cas echeant

CLGRIB=TRANSFER (KVALCO(IDECAL+1), CLGRIB)
LLSWAP = (CLGRIB (1:4) /= 'GRIB') .AND. (CLGRIB (5:8) == 'BIRG')
IF (LLSWAP) THEN
  ALLOCATE (IVALCO (KLONGA))
  CALL JSWAP (IVALCO(IDECAL+1), KVALCO(IDECAL+1), 8_JPLIKM, INT (KLONGA-IDECAL, JPLIKM))
ELSE
  IVALCO => KVALCO
ENDIF

! ILENG=longueur disponible en entiers declares INTEGER dans KVALCO.
ILENG=2*(KLONGA-IDECAL)
!
!     3.1 -  APPEL A GRIBEX
!
IWORD=0
IRET=-1

CALL FAGRIBEX(ISEC0,ISEC1,ISEC2,ZSEC2,ISEC3,ZSEC3,ISEC4,          &
&             PCHAMP,IILCHAM,IVALCO(IDECAL+1:KLONGA),ILENG,IWORD, &
&             CLOPER,IRET)

IF (LLSWAP) THEN
  DEALLOCATE (IVALCO)
  NULLIFY (IVALCO)
ENDIF

IF (FA%LFAMOP) THEN
  WRITE (UNIT=FA%NULOUT,FMT=*)                          &
&         ' FADECX: KLONGA, IDECAL, ILENG, IILCHAM = ', &
&         KLONGA, IDECAL, ILENG, IILCHAM
  WRITE (UNIT=FA%NULOUT,FMT=*) '       * ISEC0 = ',ISEC0
  WRITE (UNIT=FA%NULOUT,FMT=*) '       * ISEC1 = ',ISEC1
  WRITE (UNIT=FA%NULOUT,FMT=*)                                    &
&                     '       * ILONSEC2 ! ISEC2(1:ILONSEC2) = ', &
&                     ILONSEC2, ' ! ', ISEC2(1:ILONSEC2)
  WRITE (UNIT=FA%NULOUT,FMT=*) '       * ZSEC2(1:2) = ',ZSEC2(1:2)
  IF (ISEC2(12).GT.0) WRITE (UNIT=FA%NULOUT,FMT=*)            &
&          '       * ISEC2(12) ! ZSEC2(11:10+ISEC2(12)) = ',  &
&                    ISEC2(12), ' ! ', ZSEC2(11:10+ISEC2(12))
  WRITE (UNIT=FA%NULOUT,FMT=*) '       * FA%JPSEC4 ! ISEC4 = ', &
&                               FA%JPSEC4,' ! ',ISEC4
ENDIF
!
!
IF (KREP.NE.0) THEN
  GOTO 1001
ENDIF
!
!*
!     3.3 -  CONTROLES DE COHERENCE
!-----------------------------------------------------------------------
!

IF (IRET.GT.0) THEN
! Erreur rapportee par GRIBEX
  KREP=-1000-IRET
  WRITE (UNIT=FA%NULOUT,FMT=*) ' FADECX: IRET, KREP = ',IRET, KREP 
  GOTO 1001
ELSEIF (IRET.LT.0 .AND. ((IRET /= -4) .OR. .NOT. LDUNDF)) THEN ! -4 = "A bitmap was encountered"
! Warning rapporte par GRIBEX
  WRITE (UNIT=FA%NULOUT,FMT=*)
  WRITE (UNIT=FA%NULOUT,FMT=*)                         &
&         '!------------------------------------------'
  WRITE (UNIT=FA%NULOUT,FMT=*)                         &
&         '!           FADECX:   WARNING !!!         !'
  WRITE (UNIT=FA%NULOUT,FMT=*)                         &
&         '!------------------------------------------'
  WRITE (UNIT=FA%NULOUT,FMT=*) ' Code retour de GRIBEX = ', &
&        IRET,' pour le champ: ',CDNOMA
  WRITE (UNIT=FA%NULOUT,FMT=*)
ENDIF
IF (ISEC4(1).LT.IILCHAM) THEN
  KREP=-93
  IF (FA%LFAMOP) THEN
    WRITE (UNIT=FA%NULOUT,FMT=*)                                 &
&         'FADECX: ERREUR !!! Nbre de valeurs decodees = ',      &
&            ISEC4(1),' et nbre de valeurs attendues = ',IILCHAM
  ENDIF
  GOTO 1001
ELSEIF (ISEC4(1).GT.IILCHAM) THEN
  KREP=-94
  IF (FA%LFAMOP) THEN
    WRITE (UNIT=FA%NULOUT,FMT=*)                              &
&         'FADECX: ERREUR !!! Nbre de valeurs decodees = ',   &
&         ISEC4(1),' et nbre de valeurs attendues = ',IILCHAM
  ENDIF
  IF (LLMOER(KREP,KRANG)) GOTO 1001
ENDIF
!
IF (IVALC3.NE.ISEC4(2).AND.FA%LFAMOP) THEN
  WRITE (UNIT=FA%NULOUT,FMT=*)                                    &
&     ' FADECX: WARNING, le nb de bits de codage qui avait',      &
&     ' ete demande ( ',IVALC3,' ) est different de celui qui a', &
&          ' ete finalement retenu ( ',ISEC4(2),' ) par GRIBEX.'
  WRITE (UNIT=FA%NULOUT,FMT=*)                       &
&         ' => Gain de place sans perte de precision'
ENDIF
!
!  Dans le cas d'un champ spectral ARPEGE
!
IF (LDCOSP.AND..NOT.LLMLAM.AND.(ISEC4(18).NE.IVALC4 &
&    .OR.ISEC4(17).NE.IVALC5)) THEN                  
  IF (FA%LFAMOP) THEN
    WRITE (UNIT=FA%NULOUT,FMT=*)                              &
&            'Ss-tronc non compactee dans GRIB = ',ISEC4(18), &
&            ' et on attend: ',IVALC4
    WRITE (UNIT=FA%NULOUT,FMT=*)                              &
&            'Puissance de laplacien dans GRIB = ',ISEC4(17), &
&            ' et on attend: ',IVALC5
  ENDIF
  KREP=-95
  GOTO 1001
ENDIF
!
! Controle de l'adequation entre le nb de mots lus par LFI et le detail:
! ( enrobage FA + message GRIBEX + eventuelles valeurs non-compactees ).
!
IWORD=1+(ISEC0(1)-1)/JPLIKB
IF (FA%LFAMOP) THEN
  WRITE (UNIT=FA%NULOUT,FMT=*) ' FADECX: IWORD = ',IWORD
ENDIF
IPOFIN=IDECAL+IWORD
IF (LDCOSP) THEN
  IF (LLMLAM) THEN
    IPOFIN=IPOFIN+ISTRIA
  ELSE
    IPOFIN=IPOFIN+(1+IVALC4)*(2+IVALC4)
  ENDIF
ENDIF
!
IF (KLONGA.LT.IPOFIN) THEN
  KREP=-93
  GOTO 1001
ELSEIF (KLONGA.GT.IPOFIN) THEN
  KREP=-94
  IF (LLMOER(KREP,KRANG)) GOTO 1001
ENDIF
!*
!     3.2 -  DEMODULATION DES COEFF. SPEC. ALADIN QUI ONT ETE COMPACTES
!-----------------------------------------------------------------------
!
IF (LDCOSP.AND.LLMLAM) THEN
!  Transfert des donnees decodees et modulees entieres en nombres reels
!  pour les demoduler. Comme PCHAMP est a profil implicite, on ne peut
!  s'en servir pour la fonction TRANSFER => il faut passer par ICHAMP!
  ZSEC4(1:IILCHAM) = PCHAMP(1:IILCHAM)
  ZCHAMP=0._JPDBLR
  ZPULAP=REAL(IVALC5,JPDBLR) * (-0.001_JPDBLR)
  IIND=0
  DO JM=1,FA%CADRE(IRANGC)%NOMPAR(2)
    ILOW=2+2*JM+1
    IADD=4*MAX(IVALC4+1-JM,1_JPLIKB )
!
    DO IDX=FA%CADRE(IRANGC)%NOMPAR(ILOW)+IADD,FA%CADRE(IRANGC)%NOMPAR(ILOW+1)
      IIND=IIND+1        
      JN=(IDX-FA%CADRE(IRANGC)%NOMPAR(ILOW))/4
      ZCHAMP(IDX)=ZSEC4(IIND) *                 &
&           ((REAL(JN**2+JM**2,JPDBLR))**ZPULAP)
    ENDDO
  ENDDO
!  Transfert des donnees decodees et demodulees reelles en nombres entiers
!  disposes aux bons endroits du tableau definitif.
  PCHAMP(1:ILCHAM) = ZCHAMP (1:ILCHAM)
ENDIF
!*
!     3.3 -  TRANSFERT DES COEFFICIENTS SPECTRAUX NON COMPACTES.
!-----------------------------------------------------------------------
!        (et non fournis par GRIBEX) stockes en fin d'article.
!
IF (LDCOSP) THEN
  IF (LLMLAM) THEN
    IIND=0
    DO JM=0,FA%CADRE(IRANGC)%NOMPAR(2)
      IL=2+2*JM+1
      ILOW=FA%CADRE(IRANGC)%NOMPAR(IL)
!
      IF (JM.EQ.0) THEN
        IHIGH=FA%CADRE(IRANGC)%NOMPAR(IL+1)
      ELSE
        IHIGH=ILOW+4*(IVALC4+1-JM)-1
        IF (IHIGH.LE.ILOW) IHIGH=ILOW+3
      ENDIF
!
      DO IDX=ILOW,IHIGH
        IIND=IIND+1
        PCHAMP(IDX)=TRANSFER (KVALCO(IDECAL+IWORD+IIND), PCHAMP(IDX))
      ENDDO
    ENDDO
  ELSE
!
! Cas ARPEGE
!
    PCHAMP(1:2*(IVALC4+1))=                              &
&        TRANSFER (KVALCO(IDECAL+IWORD+1:IDECAL+IWORD+2*(IVALC4+1)), PCHAMP(1:2*(IVALC4+1)))
    IIND=2*(IVALC4+1)-1
    IDX=2*(ITRONC+1)-1
    DO JM=1,IVALC4
    DO JN=JM,ITRONC
      IDX=IDX+2
      IF (JN.LE.IVALC4) THEN
        IIND=IIND+2
        PCHAMP(IDX) = TRANSFER (KVALCO(IDECAL+IWORD+IIND), PCHAMP(IDX))
        PCHAMP(IDX+1) = TRANSFER (KVALCO(IDECAL+IWORD+IIND+1), PCHAMP(IDX+1))
      ENDIF
    ENDDO
    ENDDO
!
  ENDIF
ENDIF
!*
!     3.4 - Renversement des valeurs en pts de grille des champs
!            lat-lon afin de les ranger Sud-Nord plutot que Nord-Sud
!            (on conserve le rangt W-E consecutif) a l'image du rangt
!            initial effectue par FullPos.
!-----------------------------------------------------------------------
!
IF ((ISEC2(1)==0.OR.ISEC2(1)==10.OR.ISEC2(1)==20.OR. &
&    ISEC2(1)==30) .AND. .NOT.LDCOSP) THEN
  IF (FA%LFAMOP) THEN
    WRITE (UNIT=FA%NULOUT,FMT=*)                              &
&            ' FADECX: Grille LAT-LON issue BDAP -> ',        &
&            ' renversement des valeurs pour etre rangees SN'
  ENDIF
  DO JLAT=1,FA%CADRE(IRANGC)%NLATIT
  DO JLON=1,FA%CADRE(IRANGC)%NXLOPA
    JN=JLON+FA%CADRE(IRANGC)%NXLOPA*(JLAT-1)
    IDX=JLON+FA%CADRE(IRANGC)%NXLOPA*(FA%CADRE(IRANGC)%NLATIT-JLAT)
    ZCHAMP(IDX) = PCHAMP(JN)
  ENDDO
  ENDDO
  PCHAMP(1:ILCHAM) = ZCHAMP(1:ILCHAM)
ENDIF

!
!            CHANGEMENT D'UNITE DE CERTAINS CHAMPS.
!            Il s'agit de champs dont les valeurs sont comprises
!            entre 0 et 1 dans le modele mais dont l'unite
!            conventionnelle dans le GRIB est le %.
!
CALL FAIPAG_FORT                                                    &
&               (FA,  KREP, INUMER, CDPREF, KNIVAU, CDSUFF, INIPAR, &
&                YLGR1TAB)
!
! Traitement des valeurs indefinies
!
LLUNDF = ISEC1(5) == 192
IF (LLUNDF) THEN
  ZUNDF = ZSEC3(2)
ELSE
  ZUNDF = 0._JPDBLR
ENDIF
!
! Facteur d'echelle eventuel
!
IF (YLGR1TAB%LMULTI) THEN
  PCHAMP (1:ILCHAM) = PCHAMP (1:ILCHAM) / YLGR1TAB%FMULTI
  ZUNDF             = ZUNDF             / YLGR1TAB%FMULTI
ENDIF
IF (LDUNDF .AND. LLUNDF) THEN
  DO J = 1, ILCHAM
    IF (PCHAMP (J) == ZUNDF) THEN
      PCHAMP (J) = PUNDF
    ENDIF
  ENDDO
  ZUNDF = PUNDF
ENDIF
LDUNDF = LLUNDF
PUNDF  = ZUNDF

!**
!    10.  -  PHASE TERMINALE : MESSAGERIE, AVEC "ABORT" EVENTUEL,
!            VIA LE SOUS-PROGRAMME "FAIPAR" .
!-----------------------------------------------------------------------
!
1001 CONTINUE
LLFATA=LLMOER (KREP,KRANG)
!
IF (FA%LFAMOP.OR.LLFATA) THEN
  INIMES=2
  CLNSPR='FADECX'
!
  WRITE (UNIT=CLMESS,FMT='(''KREP='',I5,'', KRANG='',I4,  &
&         '', CDNOMA='''''',A,'''''', KLONGA= '',I8,      &
&         '', LDCOSP='',L1)')                             &
&     KREP, KRANG, CDNOMA, KLONGA, LDCOSP
  CALL FAIPAR_FORT                                        &
&                 (FA, INUMER,INIMES,KREP,.FALSE.,CLMESS, &
&                  CLNSPR,CDNOMA,.FALSE.)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('FADECX_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"

END SUBROUTINE FADECX_FORT

!INTF KREP            OUT                                                              
!INTF KRANG         IN                                                                 
!INTF CDNOMA        IN                                                                 
!INTF KVALCO        IN    DIMS=*                         KIND=JPLIKB                   
!INTF KLONGA        IN                                                                 
!INTF PCHAMP          OUT DIMS=*                                                       
!INTF LDCOSP        IN                                                                 
!INTF LDUNDF          OUT                                                              
!INTF PUNDF           OUT                                                              
!INTF YDGR1TAB        OUT                                                              

