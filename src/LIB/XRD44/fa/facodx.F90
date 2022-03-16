! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FACODX_FORT                                              &
&                     (FA,  KREP,   KRANG,  CDPREF, KNIVAU, CDSUFF, &
&                      PSEC4, LDCOSP, KVALCO, KLONGD,               &
&                      LDUNDF, PUNDF, YDGR1TAB)
USE FA_MOD, ONLY : FA_COM, JPNIIL, NUNDEF, FAGR1TAB
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!      Sous-programme INTERNE du logiciel de Fichiers ARPEGE:
!      PREPARATION (codage GRIBEX) d'un CHAMP HORIZONTAL
!      destine a etre ecrit sur un fichier ARPEGE/ALADIN.
!       ( CODage d'un champ a l'aide de gribeX )
!**
!    Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                KRANG  (Entree) ==> Rang de l'unite logique;
!                CDPREF (Entree) ==> Prefixe eventuel du nom d'article;
!                KNIVAU (Entree) ==> Niveau vertical eventuel;
!                CDSUFF (Entree) ==> Suffixe eventuel du nom d'article;
!    ( Tableau ) PSEC4  (Entree) ==> Valeurs REELLES du champ a ecrire;
!                LDCOSP (Entree) ==> Vrai si le champ est represente
!                                    par des coefficients spectraux;
!    ( Tableau ) KVALCO (Sortie) ==> Donnees destinees a l'ecriture;
!                KLONGD (Sortie) ==> Nombre de mots a ecrire;
!*
!       En mode multi-taches, il doit y avoir verrouillage du fichier
!     concerne avant l'appel au sous-programme.
!
!      Modifications
!      -------------
!         R. El Ouaraini : 03-Oct-06, introduire la nouvelle geometrie pour tester ERPK
!
!         JM AUDOIN  :  15 mai 2007 partie 5 changement unite 
!         R. El Khatib 22-May-2015 : Bypass errror message in case of underflows
!
!
!
TYPE(FA_COM)   :: FA
TYPE(FAGR1TAB) :: YDGR1TAB
INTEGER (KIND=JPLIKB) KREP, KRANG, KNIVAU, KLONGD
!
INTEGER (KIND=JPLIKB) KVALCO(*)
REAL (KIND=JPDBLR) PSEC4(*), PUNDF, ZUNDF
!
LOGICAL LDCOSP, LDUNDF, LLUNDF
!
CHARACTER CDPREF*(*), CDSUFF*(*)
!
#include "fagribex.h"
!
REAL (KIND=JPDBLR), ALLOCATABLE :: ZSEC4(:)
INTEGER (KIND=JPLIKB), ALLOCATABLE :: IVALCO(:)
REAL (KIND=JPDBLR) :: ZMIN, ZA
REAL (KIND=JPDBLR) :: ZSEC2(10+2*(FA%JPXNIV+1)), ZSEC3(2), ZPULAP
!
INTEGER (KIND=JPLIKB) ISEC0(2), ISEC1(FA%JPSEC1)
INTEGER (KIND=JPLIKB) ISEC2(FA%JPSEC2), ISEC3(2)
INTEGER (KIND=JPLIKB) ISEC4(FA%JPSEC4), ILONSEC2
INTEGER (KIND=JPLIKB) ILENG, IWORD, IRET, JM, IPULAP
INTEGER (KIND=JPLIKB) ILCHAM, JN, IDECAL, ICPACK
INTEGER (KIND=JPLIKB) ITRONC, ILOW, IHIGH, IDIMNC, INBITS
INTEGER (KIND=JPLIKB) IL, IADD, IRANGC, IILCHAM, INIMES
INTEGER (KIND=JPLIKB) INUMER,  IDX, JLAT, JLON, IDECOPT
INTEGER (KIND=JPLIKB) IFAORI, IFAMOD, INBIMO
!
LOGICAL LLMLAM
!
CHARACTER(LEN=1) CLOPER
!
INTRINSIC LEN_TRIM
!
CHARACTER(LEN=FA%JPXNOM) CLACTI 
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA
LOGICAL                  LLFACDE

!**
!     1.  -  CONTROLES ET INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FACODX_MT',0,ZHOOK_HANDLE)

ISEC0 = 0
ISEC1 = 0
ISEC2 = 0
ISEC3 = 0
ISEC4 = 0
ZSEC2 = 0
ZSEC3 = 0

LLUNDF = LDUNDF

CLACTI=''
IF (KRANG.LE.0.OR.KRANG.GT.FA%JPNXFA) THEN
  KREP=-66
  GOTO 1001
ENDIF
ICPACK=FA%FICHIER(KRANG)%NSTROF
IRANGC=FA%FICHIER(KRANG)%NUCADR
LLMLAM=FA%CADRE(IRANGC)%LIMLAM
ITRONC=FA%CADRE(IRANGC)%MTRONC
!
IF (LLMLAM) THEN
  IF (LDCOSP) THEN
    ILONSEC2=21+FA%CADRE(IRANGC)%NOMPAR(2)
  ELSE
    ILONSEC2=22
  ENDIF
ELSE
  IF (LDCOSP) THEN
    ILONSEC2=22
  ELSE
    ILONSEC2=22+FA%CADRE(IRANGC)%NLATIT
  ENDIF
ENDIF
!
KVALCO(1)=FA%FICHIER(KRANG)%NFGRIB
IDECAL=3
IF (LDCOSP) THEN
  IF (LLMLAM) THEN
    ILCHAM=FA%CADRE(IRANGC)%NSFLAM
  ELSE
    ILCHAM=(1+ITRONC)*(2+ITRONC)
  ENDIF
  KVALCO(2)=1
  INBITS=FA%FICHIER(KRANG)%NBFCSP 
  IDECAL=IDECAL+2
ELSE
  ILCHAM=FA%CADRE(IRANGC)%NVAPDG
  KVALCO(2)=0
  INBITS=FA%FICHIER(KRANG)%NBFPDG
ENDIF
KVALCO(3)=INBITS
IILCHAM = ILCHAM
IDECOPT = 0
LLFACDE = FA%FICHIER(KRANG)%NCOGRIF(11) /= 0
!**
!     2.  -  PREPARATION DU TABLEAU DE DONNEES A ECRIRE SUR LE FICHIER.
!-----------------------------------------------------------------------
!
ALLOCATE (ZSEC4 (ILCHAM))
!
IF (LDCOSP .AND. LLMLAM) THEN
!
!       Champ ALADIN en coefficients spectraux... traitement particulier,
!     car non prevu dans GRIBEX (il y sera considere comme un champ lat-lon)
!     mais on a la possibilite de compacter une (pseudo-)puissance de
!     laplacien du champ a la place du champ, de maniere a augmenter
!     la precision du champ en "aplanissant" le spectre.
!
!     Determination de la puissance de Laplacien (en 1/1000 ieme)
!
  CALL FAPULA_FORT                                 &
&                 (FA,  KREP, KRANG, PSEC4, IPULAP )
  ZPULAP=REAL(IPULAP,JPDBLR)/1000._JPDBLR
!       ZPULAP=0.
!       IPULAP=0
  IF (FA%LFAMOP) THEN
    PRINT *,'FACODX: puissance de laplacien selectionee ',ZPULAP, &
&          ' pour une sous-tronc de ',ICPACK
  ENDIF
  IF (KREP.NE.0) GOTO 1001
!
! Transfert des coeff spectraux devant etre compactes de PSEC4 a ZSEC4
! avec prise en compte du coefficient (n**2+m**2)**zpulap. Les coefficients
! concernes sont ceux inclus dans le quart de l'ellipse, hors axes (coeff
! nuls), et hors du triangle non-compacte (sous-troncature).
  IILCHAM=0
!
  DO JM=1,FA%CADRE(IRANGC)%NOMPAR(2)
    ILOW=2+2*JM+1
    IADD=4* MAX(ICPACK+1-JM,1_JPLIKB )
!
    DO IDX=FA%CADRE(IRANGC)%NOMPAR(ILOW)+IADD,FA%CADRE(IRANGC)%NOMPAR(ILOW+1)
      IILCHAM=IILCHAM+1
      JN=(IDX-FA%CADRE(IRANGC)%NOMPAR(ILOW))/4
      ZSEC4(IILCHAM)=PSEC4(IDX) *              &
&         ((REAL(JN**2+JM**2, JPDBLR))**ZPULAP)
    ENDDO
  ENDDO

! Number of elements in sub-triangle+axes:IDIMNC
  IDIMNC=ILCHAM-IILCHAM
! Recherche de l'amplitude et du min du champ
  ZMIN = MINVAL(ZSEC4(1:IILCHAM))
  ZA   = MAXVAL(ZSEC4(1:IILCHAM)) - ZMIN
! Recherche du facteur decimal optimal pour utiliser
! au mieux les INBITS dans le codage de ce champ
  IF (FA%LFAMOP) THEN
    WRITE (UNIT=FA%NULOUT,FMT=*)'FACODX: traitement du champ: ', &
&          CDPREF,KNIVAU,CDSUFF
  ENDIF
  KREP = 0
  IF (LLFACDE) CALL FACDEC_FORT (FA, KREP, ZA, ZMIN, INBITS, IDECOPT)
  IF (KREP.NE.0) THEN
    KREP = 0
  ENDIF
ELSEIF(LDCOSP .AND. .NOT.LLMLAM) THEN
!
!          Transfert du tableau d'entree dans un tableau local
!     de maniere a eviter l'ecrasement du tableau d'entree par "GRIBEX".
!
  ZSEC4(1:IILCHAM) = PSEC4(1:IILCHAM)
  IDIMNC=(1+ICPACK)*(2+ICPACK)
ELSE
!
!    CHAMPS NON SPECTRAUX: transfert du tableau d'entree dans un
!    tableau local de maniere a eviter son ecrasement par "GRIBEX".
!
!
  IDIMNC=0
! Tester si Nouvelle ou ancienne geometrie Aladin
IF (FA%CADRE(IRANGC)%SINLAT(1) .GE. 0) THEN
  IF (LLMLAM .AND. FA%CADRE(IRANGC)%SINLAT(10).LT.0) THEN
!  Parametre de projection negatif, donc pas de projection:
!  Il s'agit d'une grille lat-lon reguliere du type Full-Pos
!  (pour champ ARPEGE ou Aladin). Il faut donc renverser
!  le champ afin de ranger Nord-Sud les valeurs plutot que Sud-Nord
!  (on conserve le rangt W-E consecutif).
!  Le but est de satisfaire la BDAP qui attend un rangt NW-->SE.
!
    IF (FA%LFAMOP) THEN
      WRITE (UNIT=FA%NULOUT,FMT=*)                              &
&              ' FACODX: Grille LAT-LON pour BDAP -> ',         &
&              ' renversement des valeurs pour etre rangees NS'
    ENDIF
    DO JLAT=1,FA%CADRE(IRANGC)%NLATIT
    DO JLON=1,FA%CADRE(IRANGC)%NXLOPA
      JN=JLON+FA%CADRE(IRANGC)%NXLOPA*(JLAT-1)
      IDX=JLON+FA%CADRE(IRANGC)%NXLOPA*(FA%CADRE(IRANGC)%NLATIT-JLAT)
      ZSEC4(IDX) = PSEC4(JN)
    ENDDO
    ENDDO
  ELSE
    ZSEC4(1:IILCHAM) = PSEC4(1:IILCHAM)
  ENDIF
ELSE
  IF (LLMLAM .AND. FA%CADRE(IRANGC)%SINLAT(2).LT.0) THEN
    IF (FA%LFAMOP) THEN
      WRITE (UNIT=FA%NULOUT,FMT=*)                              &
&              ' FACODX: Grille LAT-LON pour BDAP -> ',         &
&              ' renversement des valeurs pour etre rangees NS'
    ENDIF
    DO JLAT=1,FA%CADRE(IRANGC)%NLATIT
    DO JLON=1,FA%CADRE(IRANGC)%NXLOPA
      JN=JLON+FA%CADRE(IRANGC)%NXLOPA*(JLAT-1)
      IDX=JLON+FA%CADRE(IRANGC)%NXLOPA*(FA%CADRE(IRANGC)%NLATIT-JLAT)
      ZSEC4(IDX) = PSEC4(JN)
    ENDDO
    ENDDO
  ELSE
    ZSEC4(1:IILCHAM) = PSEC4(1:IILCHAM)
  ENDIF
ENDIF
! Recherche de l'amplitude et du min du champ
  ZMIN=ZSEC4(1)
  ZA=0._JPDBLR
  ZMIN = MINVAL(ZSEC4(1:IILCHAM))
  ZA   = MAXVAL(ZSEC4(1:IILCHAM)) - ZMIN
! Recherche du facteur decimal optimal pour utiliser
! au mieux les INBITS dans le codage de ce champ
  IF (FA%LFAMOP) THEN 
  WRITE (UNIT=FA%NULOUT,FMT=*)'FACODX: traitement du champ: ', &
&          CDPREF,KNIVAU,CDSUFF
  ENDIF
  KREP = 0
  IF (LLFACDE) THEN
    IF (ABS(ZA) <= EPSILON(ZA)) THEN
!     On anticipe le retour d'erreur de facdec dans le cas ou le champs est quasi-constant
!     (cad : son amplitude est inferieur a la precision de la machine).
      IDECOPT = 0
      KREP = 0
    ELSEIF (ZMIN /= 0_JPDBLR .AND. ABS(ZMIN) < EPSILON(ZMIN)) THEN
!     On anticipe le retour d'erreur de facdec dans le cas ou le champ contient un "underflow"
      IDECOPT = 0
      KREP = 0
    ELSE 
      CALL FACDEC_FORT (FA, KREP, ZA, ZMIN, INBITS, IDECOPT)
      IF (KREP.NE.0) THEN
        WRITE (UNIT=FA%NULOUT,FMT=*)'FACODX: field incriminated by FACDEC was ', CDPREF,KNIVAU,CDSUFF
        IDECOPT = 0
        KREP = 0
      ENDIF
    ENDIF
  ENDIF
ENDIF
!*
!     3.  -  INITIALISATION DE L'ENROBAGE GRIB
!-----------------------------------------------------------------------
!
!     3.1 -  Sections 1, 2, 3 et 4 (sf la partie reelle pour 4)
!
CALL FAINIG_FORT                                                   &
&               (FA,  KREP, KRANG, CDPREF, KNIVAU, CDSUFF, LDCOSP, &
&                IILCHAM, ISEC1, ISEC2, ZSEC2, ISEC3, ZSEC3, ISEC4,&
&                YDGR1TAB)

IF (KREP.NE.0) THEN
  GOTO 1001
ENDIF
! Prise en compte du facteur decimal
IF (LLFACDE .AND. ISEC1(23) == 0) THEN
  ISEC1(23) = IDECOPT
ENDIF
!
!     3.2 -  Definition du type de codage
!
CLOPER='C'
IF (FA%FICHIER(KRANG)%NCOGRIF(1)==1) CLOPER='K'
!*
!     4.  -  CHANGEMENT D'UNITE DE CERTAINS CHAMPS.
!            Il s'agit de champs dont les valeurs sont comprises
!            entre 0 et 1 dans le modele mais dont l'unite
!            conventionnelle dans le GRIB est le %.
!---------------------------------------------------------------
!
ZUNDF = PUNDF
IF (YDGR1TAB%LMULTI) THEN
  ZSEC4 = ZSEC4 * YDGR1TAB%FMULTI
  ZUNDF = ZUNDF * YDGR1TAB%FMULTI
ENDIF
!
! Traitement des valeurs indefinies; on verifie d'abord que le champ
! contient de telles valeurs afin d'eviter de polluer le resultat
! final avec un bitmap inutile
!
IF (LLUNDF) THEN
  LLUNDF = ANY (ZSEC4 == ZUNDF)
ENDIF
!
! Ajustement des parametres d'encodage
!
IF (LLUNDF) THEN
  ISEC1(5)=192
  ZSEC3(2)=ZUNDF
  ISEC3(1)=0
  ISEC3(2)=INT (ZUNDF)
ENDIF
!*
!     5.  -  CODAGE GRIB PROPREMENT DIT
!-----------------------------------------------------------------------
!
IRET=-1
! ILENG=longueur disponible en nb d'"entiers declares INTEGER" dans KVALCO.
! On part de l'hypothese ou le dimensionnement de KVALCO se fait
! dans la routine appelante a ILCHAM+2 (cas de l'absence de compactage).
ILENG=(KIND(KVALCO)/4)*(ILCHAM+2-IDECAL)
IWORD=0
!DP
!DP  TEST AVEC UNE PUISSANCE DE LAPLACIEN IMPOSEE
!DP
!DP   CALL GRSMKP(0)
!DP ISEC4(17) = 2000
!DP
IF (FA%LFAMOP) THEN
  WRITE (UNIT=FA%NULOUT,FMT=*)' FACODX: CLOPER = ',CLOPER
  WRITE (UNIT=FA%NULOUT,FMT=*)                                     &
&                    ' FACODX: IILCHAM, ILCHAM, IDECAL, ILENG = ', &
&                    IILCHAM, ILCHAM, IDECAL, ILENG
  WRITE (UNIT=FA%NULOUT,FMT=*)'       * ISEC1 = ',ISEC1
  WRITE (UNIT=FA%NULOUT,FMT=*)                                   &
&                    '       * ILONSEC2 ! ISEC2(1:ILONSEC2) = ', &
&                    ILONSEC2,' ! ', ISEC2(1:ILONSEC2)
  WRITE (UNIT=FA%NULOUT,FMT=*) '       * ZSEC2(1:2) = ',ZSEC2(1:2)
  IF (ISEC2(12).GT.0) WRITE (UNIT=FA%NULOUT,FMT=*)            &
&          '       * ISEC2(12) ! ZSEC2(11:10+ISEC2(12)) = ',  &
&                    ISEC2(12), ' ! ', ZSEC2(11:10+ISEC2(12))
  WRITE (UNIT=FA%NULOUT,FMT=*)'       * FA%JPSEC4 ! ISEC4 = ', &
&                              FA%JPSEC4,' ! ',ISEC4
  WRITE (UNIT=FA%NULOUT,FMT=*)'       * ZSEC4(1:20) = ', &
&                              ZSEC4(1:20)
ENDIF

!     WARNING GRIBEX ENLEVE 
!CALL GRSDBG (0)
!CALL GRSVCK (0)

! Defauts

!CALL GRSX2O (1)
!CALL GRSN2O (1)

! Defaults FA

!IF (FA%IOPTGRSX2O /= NUNDEF) &
!& CALL GRSX2O(INT (FA%IOPTGRSX2O, JPLIKM))
!
!IF (FA%IOPTGRSN2O /= NUNDEF) &
!& CALL GRSN2O(INT (FA%IOPTGRSN2O, JPLIKB))

! Defauts pour cette unite

!IF (FA%FICHIER(KRANG)%IOPTGRSX2O /= NUNDEF) &
!& CALL GRSX2O(INT (FA%FICHIER(KRANG)%IOPTGRSX2O, JPLIKM))
!
!IF (FA%FICHIER(KRANG)%IOPTGRSN2O /= NUNDEF) &
!& CALL GRSN2O(INT (FA%FICHIER(KRANG)%IOPTGRSN2O, JPLIKB))

!  1/ On force GRIBEX a calculer la puissance de laplacien
!CALL GRSMKP(1)
!  2/ On retire l'arrondi du message GRIB a un multiple de 120 octets
!CALL GRSRND(0)

CALL FAGRIBEX(ISEC0,ISEC1,ISEC2,ZSEC2,ISEC3,ZSEC3,ISEC4,  &
&             ZSEC4,IILCHAM,KVALCO(IDECAL+1),ILENG,IWORD, &
&             CLOPER,IRET)
!
IF (IRET.GT.0) THEN
! Erreur rapportee par GRIBEX
  KREP=-1000-IRET
  GOTO 1001
ELSEIF (IRET.LT.0) THEN
! Warning rapporte par GRIBEX
  WRITE (UNIT=FA%NULOUT,FMT=*)
  WRITE (UNIT=FA%NULOUT,FMT=*)                               &
&               '!------------------------------------------'
  WRITE (UNIT=FA%NULOUT,FMT=*)                               &
&               '!           FACODX:   WARNING !!!         !'
  WRITE (UNIT=FA%NULOUT,FMT=*)                               &
&               '!------------------------------------------'
  WRITE (UNIT=FA%NULOUT,FMT=*) ' Code retour de GRIBEX = ', &
&        IRET,' pour le champ: ',CDPREF,KNIVAU,CDSUFF
  WRITE (UNIT=FA%NULOUT,FMT=*)
ENDIF
!
! ISEC0(1) = nb d'octets dans le message GRIB
! IWORD    = nb de mots de JBDBLE octets (64 bits) du message GRIB
IWORD=1+(ISEC0(1)-1)/JPLIKB
KLONGD=IDECAL+IWORD+IDIMNC
IF (FA%LFAMOP) THEN
  WRITE (UNIT=FA%NULOUT,FMT=*)                                    &
&         ' FACODX: longueur du GRIB en nb octets et en mots = ', &
&         ISEC0(1), IWORD
  WRITE (UNIT=FA%NULOUT,FMT=*)                             &
&         ' FACODX: longueur de l''article FA en mots = ', &
&         KLONGD
  IF (ISEC4(4).EQ.64 .AND. ISEC4(3).EQ.128) THEN
    WRITE (UNIT=FA%NULOUT,FMT=*)                          &
&           ' FACODX: complex packing with P=',ISEC4(17), &
&           ' and sub trunc = ',ISEC4(18)
  ENDIF
ENDIF
!
!  CAS D'UN DEPASSEMENT DE LA TAILLE MAX DE L'ARTICLE FINAL
!  On ramene ce cas a celui d'un tableau trop petit dans GRIBEX.
!
IF (KLONGD.GT.ILCHAM+2) THEN
  IF (FA%LFAMOP) THEN
    WRITE (UNIT=FA%NULOUT,FMT=*)                           &
&            ' FACODX: article FA + long avec compactage', &
&            ' que sans => on le supprime'
  ENDIF
  IRET=710
  KREP=-1000-IRET
  GOTO 1001
ENDIF
!
!*
!     6.  -  TRANSFERT DES COEFFICIENTS SPECTRAUX NON COMPACTES.
!-----------------------------------------------------------------------
!        (et non traites par GRIBEX) en fin d'article.
!
IF (LDCOSP) THEN
  KVALCO(4)=ICPACK
  IF (LLMLAM) THEN
    KVALCO(5)=IPULAP
! Copy nonpacked part of PSEC4 (sub-triangle+axes) into KVALCO
    IILCHAM=0
    DO JM=0,FA%CADRE(IRANGC)%NOMPAR(2)
      IL=2+2*JM+1
      ILOW=FA%CADRE(IRANGC)%NOMPAR(IL)
!
      IF (JM.EQ.0) THEN
        IHIGH=FA%CADRE(IRANGC)%NOMPAR(IL+1)
      ELSE
        IHIGH=ILOW+4*(ICPACK+1-JM)-1
        IF (IHIGH.LE.ILOW) IHIGH=ILOW+3
      ENDIF     
!
      DO IDX=ILOW,IHIGH
        IILCHAM=IILCHAM+1
        ZSEC4(IILCHAM)=PSEC4(IDX)
      ENDDO
    ENDDO
    IF (IILCHAM.NE.IDIMNC) THEN
      WRITE (UNIT=FA%NULOUT,FMT='(A35,I10,A11,I10)')        &
&            'FACODX: incoherence entre IILCHAM= ',IILCHAM, &
&            'et IDIMNC= ',IDIMNC
      KREP=-126
      GOTO 1001
    ENDIF
  ELSE
    KVALCO(5)=ISEC4(17)
! Recuperation des coeff spectraux non compactes sachant que le
! rangement est fait par colonnes de JM=cst juxtaposees
    ZSEC4(1:2*(ICPACK+1))=PSEC4(1:2*(ICPACK+1))
    IILCHAM=2*(ICPACK+1)-1
    IDX=2*(ITRONC+1)-1
    DO JM=1,ICPACK
    DO JN=JM,ITRONC
      IDX=IDX+2
      IF (JN.LE.ICPACK) THEN
        IILCHAM=IILCHAM+2
        ZSEC4(IILCHAM) = PSEC4(IDX)
        ZSEC4(IILCHAM+1) = PSEC4(IDX+1)
      ENDIF
    ENDDO
    ENDDO
    IF (IILCHAM+1.NE.IDIMNC) THEN
      WRITE (UNIT=FA%NULOUT,FMT='(A35,I10,A11,I10)')            &
&            'FACODX: incoherence entre IILCHAM+1= ',IILCHAM+1, &
&            'et IDIMNC= ',IDIMNC
      KREP=-126
      GOTO 1001
    ENDIF
  ENDIF
! Les IDIMNC coeff spectraux non compactes doivent etre transferes
! sur le tableau d'entiers KVALCO apres le IDECAL+IWORD ieme elt.
!
!       KVALCO(IDECAL+IWORD+1:KLONGD)=TRANSFER(ZSEC4,KVALCO,IDIMNC)
  ALLOCATE (IVALCO(IDIMNC))
  IVALCO(1:IDIMNC)=TRANSFER(ZSEC4,IVALCO,IDIMNC)
  KVALCO(IDECAL+IWORD+1:KLONGD)=IVALCO(1:IDIMNC)
  DEALLOCATE (IVALCO)
ENDIF
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE, AVEC "ABORT" EVENTUEL,
!            VIA LE SOUS-PROGRAMME "FAIPAR" .
!-----------------------------------------------------------------------
!
1001 CONTINUE
IF (ALLOCATED(ZSEC4)) DEALLOCATE ( ZSEC4 )
!
! Cas particulier de l'erreur GRIBEX num 710: OUTPUT ARRAY TOO SMALL
! On s'en sert pour detecter un probleme de compactage lie a ce que
! le champ compacte+les descripteurs prennent plus de place que le
! champ non compacte...
! On sort donc du compactage (FACODX) pour demander un codage sans
! compactage (FACINE) avec rangement des valeurs selon le modele:
! FA%NFGRIB=-1.
!
IF (IRET==710) THEN
  CLNSPR='FACODX'
  INIMES=2
  INUMER=JPNIIL
!
  WRITE (UNIT=CLMESS,FMT='(''KREP='',I5,'', KRANG='',I4,  &
&         '', CDPREF='''''',A,'''''', KNIVAU='',I6,       &
&         '', CDSUFF='''''',A,'''''', LDCOSP= '',L1,      &
&         '', KLONGD='',I6)')                             &
&     KREP, KRANG, CDPREF(1:LEN_TRIM(CDPREF)), KNIVAU,    &
&     CDSUFF(1:LEN_TRIM(CDSUFF)), LDCOSP, KLONGD
  CALL FAIPAR_FORT                                        &
&                 (FA, INUMER,INIMES,KREP,.FALSE.,CLMESS, &
&                  CLNSPR,CLACTI,.FALSE.)
  CLMESS=                                                          &
& ' CAUTION: this field is not packed or it will occupy more space'
  CALL FAIPAR_FORT                                        &
&                 (FA, INUMER,INIMES,KREP,.FALSE.,CLMESS, &
&                  CLNSPR,CLACTI,.FALSE.)
  IF (LHOOK) CALL DR_HOOK('FACODX_MT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
!
!
LLFATA=LLMOER (KREP,KRANG)
!
IF (FA%LFAMOP.OR.LLFATA) THEN
  INIMES=2
  CLNSPR='FACODX'
  INUMER=JPNIIL
!
  WRITE (UNIT=CLMESS,FMT='(''KREP='',I5,'', KRANG='',I4,  &
&         '', CDPREF='''''',A,'''''', KNIVAU='',I6,       &
&         '', CDSUFF='''''',A,'''''', LDCOSP= '',L1,      &
&         '', KLONGD='',I6)')                             &
&     KREP, KRANG, CDPREF(1:LEN_TRIM(CDPREF)), KNIVAU,    &
&     CDSUFF(1:LEN_TRIM(CDSUFF)), LDCOSP, KLONGD
  CALL FAIPAR_FORT                                        &
&                 (FA, INUMER,INIMES,KREP,.FALSE.,CLMESS, &
&                  CLNSPR,CLACTI,.FALSE.)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('FACODX_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"

END SUBROUTINE FACODX_FORT

!INTF KREP            OUT                                                              
!INTF KRANG         IN                                                                 
!INTF CDPREF        IN                                                                 
!INTF KNIVAU        IN                                                                 
!INTF CDSUFF        IN                                                                 
!INTF PSEC4         IN    DIMS=*                                                       
!INTF LDCOSP        IN                                                                 
!INTF KVALCO          OUT DIMS=*                         KIND=JPLIKB                   
!INTF KLONGD          OUT                                                              
!INTF LDUNDF        IN                                                                 
!INTF PUNDF         IN                                                                 
!INTF YDGR1TAB      IN                                                                 
