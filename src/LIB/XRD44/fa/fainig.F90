! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
! Jun-2015 R. El Khatib Allow an unlimited number of vertical levels
SUBROUTINE FAINIG_FORT                                                &
&                     (FA,  KREP,   KRANG,  CDPREF, KNIVAU, CDSUFF,   &
&                      LDCOSP, KLCHAM, KSEC1, KSEC2, PSEC2, KSEC3,    &
&                      PSEC3, KSEC4, YDGR1TAB)
USE FA_MOD, ONLY : FA_COM, JPNIIL, FAGR1TAB, JD_SET, JD_CE1, JD_DEX, JD_FMT
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!      Sous-programme INTERNE du logiciel de Fichiers ARPEGE:
!      INItialisation de l'entete Gribex d'un champ.
!**
!    Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                KRANG  (Entree) ==> Rang de l'unite logique;
!                CDPREF (Entree) ==> Prefixe eventuel du nom d'article;
!                KNIVAU (Entree) ==> Niveau vertical eventuel;
!                CDSUFF (Entree) ==> Suffixe eventuel du nom d'article;
!                LDCOSP (Entree) ==> Vrai si le champ est represente
!                                    par des coefficients spectraux;
!                KLCHAM (Entree) ==> Longueur totale du champ;
!    ( Tableau ) KSEC1  (Sortie) ==> Image des parametres de la section 1
!                                    de GRIBEX;
!                KSEC2  (Sortie) ==> Image des parametres de la section 2
!                                    de GRIBEX, partie entiere;
!                PSEC2  (Sortie) ==> Image des parametres de la section 2
!                                    de GRIBEX, partie reelle;
!                KSEC3  (Sortie) ==> Image des parametres de la section 3
!                                    de GRIBEX, partie entiere;
!                PSEC3  (Sortie) ==> Image des parametres de la section 3
!                                    de GRIBEX, partie reelle;
!                KSEC4  (Sortie) ==> Image des parametres de la section 4
!                                    de GRIBEX, partie entiere;
!*
!     Modifications
!     -------------
!        R. El Ouaraini: 03-Oct-06 introduction du new EGGX pour tester ERPK
!        R. El Khatib : 11-Aug-2009 Bugfix for non-square geometries
!
!
!
!
!
TYPE(FA_COM) :: FA
REAL (KIND=JPDBLR) PSEC3(*), PSEC2(*)
!
INTEGER (KIND=JPLIKB) KREP, KRANG, KNIVAU, KLCHAM
INTEGER (KIND=JPLIKB) KSEC1(FA%JPSEC1)
INTEGER (KIND=JPLIKB) KSEC2(FA%JPSEC2), KSEC3(2)
INTEGER (KIND=JPLIKB) KSEC4(FA%JPSEC4)
!
CHARACTER CDPREF*(*), CDSUFF*(*)
!
LOGICAL LDCOSP
!
TYPE (FAGR1TAB) :: YDGR1TAB
!
INTEGER (KIND=JPLIKB) IRANGC, INIMES, INUMER
INTEGER (KIND=JPLIKB) INLAT, INIVAU, INBITS
INTEGER (KIND=JPLIKB) INIPAR(8), ICPACK
!
LOGICAL LLMLAM
!
INTRINSIC LEN_TRIM
!
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA
!

!**
!     0.  -  CONTROLES ET INITIALISATIONS PREALABLES
!-----------------------------------------------------------------------
!
!  Controle de la bonne initialisation de la date
!
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('FAINIG_MT',0,ZHOOK_HANDLE)

IF (FA%FICHIER(KRANG)%LCREAF) THEN
  KREP=-85
  GOTO 1001
ENDIF

INUMER=FA%FICHIER(KRANG)%NULOGI

ICPACK=FA%FICHIER(KRANG)%NSTROF
IRANGC=FA%FICHIER(KRANG)%NUCADR
INLAT=FA%CADRE(IRANGC)%NLATIT
INIVAU=FA%CADRE(IRANGC)%NNIVER
LLMLAM=FA%CADRE(IRANGC)%LIMLAM

IF (LDCOSP) THEN
  INBITS=FA%FICHIER(KRANG)%NBFCSP
ELSE
  INBITS=FA%FICHIER(KRANG)%NBFPDG
ENDIF
!
!**
!     1.  -  SECTION 1: the product definition section
!-----------------------------------------------------------------------
!
! Appel a FAISC1 une seule fois pour un fichier: initialisation
! du tableau FA%NSEC1(2:21,KRANG) qui va servir comme base pour KSEC1:
!
IF (FA%FICHIER(KRANG)%LISEC1) THEN
  CALL FAISC1_FORT              &
&                (FA, KREP,KRANG)
  IF (KREP.NE.0) GOTO 1001
  FA%FICHIER(KRANG)%LISEC1=.FALSE.
ENDIF
KSEC1(1:FA%JPSEC1)=0
KSEC1(2:21)=FA%FICHIER(KRANG)%NSEC1(2:21)
!
!  Initialisation de INIPAR (5 elts de KSEC1 (1 et 6:9) et un indicateur
!  de type de champ: 0->RAS; 2->min/max; 4->cumul)
CALL FAIPAG_FORT                                                   &
&               (FA,  KREP, INUMER, CDPREF, KNIVAU, CDSUFF, INIPAR,&
&                YDGR1TAB)
IF (KREP.NE.0) GOTO 1001
!  Element 1: version number of code table 2
KSEC1(1) = INIPAR(1)
!  Element 6: parameter indicator
KSEC1(6) = INIPAR(2)
IF (INIPAR(2).LT.0.OR.INIPAR(2).GT.254.AND.FA%LFAMOP) THEN
  WRITE (UNIT=FA%NULOUT,FMT=*)                                  &
&         '----------------------------------------------------'
  WRITE (UNIT=FA%NULOUT,FMT=*)                                  &
&         '    FAINIG: warning, parameter indicator not defined'
  WRITE (UNIT=FA%NULOUT,FMT=*)                                 &
&         'for: ',CDPREF,'  ',CDSUFF,'. Set to 255, by default'
  WRITE (UNIT=FA%NULOUT,FMT=*)                                  &
&         '----------------------------------------------------'
  KSEC1(6) = 255
ENDIF
!  Element 7: type of level indicator
KSEC1(7) = INIPAR(3)
!  Element 8: height, pressure, etc of level or top of level
KSEC1(8) = INIPAR(4)
!  Element 9: height, pressure, etc of level or bottom of level
KSEC1(9) = INIPAR(5)

IF (FA%FICHIER(KRANG)%MADATX(JD_FMT-11) == 0) THEN

! Cas de la periode de reference
  IF (INIPAR(6)==2) THEN
! Convention dans FA (depuis fin 2000): l'echeance precedente
! est stockee dans FA%MADATE(10,KRANG).
    KSEC1(17)=KSEC1(16)
    KSEC1(16)=FA%FICHIER(KRANG)%MADATE(10)
    KSEC1(18)=2
! Cas du cumul
  ELSEIF (INIPAR(6)==4) THEN
    KSEC1(17)=KSEC1(16)
    KSEC1(16)=FA%FICHIER(KRANG)%MADATE(10)
    KSEC1(18)=4
! Nb de produits inclus dans le cumul: valeur bidon de 1
    KSEC1(19)=1
  ELSEIF (INIPAR(6)==8) THEN
! Cumul depuis le depart
    KSEC1(17)=KSEC1(16)
    KSEC1(18)=4
    KSEC1(16)=0
    KSEC1(19)=0
  ENDIF

ELSEIF ((FA%FICHIER(KRANG)%MADATX(JD_FMT-11) == 1) .AND. &
      & (FA%FICHIER(KRANG)%MADATX(JD_DEX-11) == 1)) THEN
!
! Cas d'une datation en minutes; on descend au quart d'heure
!
  KSEC1(15)=13
  KSEC1(16)=FA%FICHIER(KRANG)%MADATX(JD_SET-11)/(15 * 60)

! Cas de la periode de reference
  IF (INIPAR(6)==2) THEN
! Convention dans FA (depuis fin 2000): l'echeance precedente
! est stockee dans FA%MADATE(10,KRANG).
    KSEC1(17)=KSEC1(16)
    KSEC1(16)=FA%FICHIER(KRANG)%MADATX(JD_CE1-11)/(15 * 60) 
    KSEC1(18)=2
! Cas du cumul
  ELSEIF (INIPAR(6)==4) THEN
    KSEC1(17)=KSEC1(16)
    KSEC1(16)=FA%FICHIER(KRANG)%MADATX(JD_CE1-11)/(15 * 60)
    KSEC1(18)=4
! Nb de produits inclus dans le cumul: valeur bidon de 1
    KSEC1(19)=1
  ELSEIF (INIPAR(6)==8) THEN
! Cumul depuis le depart
    KSEC1(17)=KSEC1(16)
    KSEC1(18)=4
    KSEC1(16)=0
    KSEC1(19)=0
  ENDIF

ENDIF

! Facteur decimal d'echelle
KSEC1(23)=INIPAR(7)
!**
!     2.  -  SECTION 2: the grid definition section
!-----------------------------------------------------------------------
!
! Appel a FAISC2 une seule fois pour un cadre, pour initialiser
! les tableaux NSEC2xxx et FA%XSEC2.
!
IF (FA%CADRE(IRANGC)%LISEC2) THEN
  CALL FAISC2_FORT               &
&                (FA, KREP,IRANGC)
  IF (KREP.NE.0) GOTO 1001
  FA%CADRE(IRANGC)%LISEC2=.FALSE.
ENDIF
!
! Appel a FAIS2F une seule fois pour un fichier Aladin,
! pour initialiser le tableau FA%NSC2ALF (sauf redefinition
! de la ss-tronc dans FAGOTE).
!
IF (LLMLAM.AND.FA%FICHIER(KRANG)%LISC2F) THEN
  CALL FAIS2F_FORT              &
&                (FA, KREP,KRANG)
  IF (KREP.NE.0) GOTO 1001
  FA%FICHIER(KRANG)%LISC2F=.FALSE.
ENDIF

KSEC2(1:FA%JPSEC2)=0
IF (LLMLAM) THEN
  IF (LDCOSP) THEN
!  Le champ spectral que l'on doit coder va etre represente sur une
!  grille lat-lon quasi-reguliere puisque ce type de coeff. spectraux
!  n'est pas pris en compte dans GRIBEX.
    KSEC2(1:22)=FA%CADRE(IRANGC)%NSEC2AL(1:22)
    KSEC2(23:21+FA%CADRE(IRANGC)%NOMPAR(2))=          &
&     FA%FICHIER(KRANG)%NSC2ALF(1:FA%CADRE(IRANGC)%NOMPAR(2)-1)
  ELSE
    IF (FA%CADRE(IRANGC)%SINLAT(1) .GE. 0) THEN
! Old EGGX
      IF (FA%CADRE(IRANGC)%SINLAT(10).LT.0) THEN
!  Parametre de projection negatif, donc pas de projection
!  La grille de ce cadre est une grille lat-lon reguliere
!  du type Full-Pos (pour champ ARPEGE ou Aladin)
        KSEC2(1:22)=FA%CADRE(IRANGC)%NSEC2LL(1:22)
      ELSE
!  La grille de ce cadre est donc du type Lambert conforme
!  (cas general de la grille Aladin)
        KSEC2(1:22)=FA%CADRE(IRANGC)%NSEC2LA(1:22)
      ENDIF
    ELSE
! New EGGX
      IF (FA%CADRE(IRANGC)%SINLAT(2).LT.0) THEN
        KSEC2(1:22)=FA%CADRE(IRANGC)%NSEC2LL(1:22)
      ELSE
        KSEC2(1:22)=FA%CADRE(IRANGC)%NSEC2LA(1:22)
      ENDIF
    ENDIF
  ENDIF
ELSE
  IF (LDCOSP) THEN
    KSEC2(1:22)=FA%CADRE(IRANGC)%NSEC2SP(1:22)
  ELSE
    KSEC2(1:22+INLAT)=FA%CADRE(IRANGC)%NSEC2GG(1:22+INLAT)
  ENDIF
ENDIF
!
! Controle ultime: on regarde le prefixe pour s'assurer de la
! presence ou non d'une coordonnee hybride sur la verticale,
! seul cas qui impose une description dans la section 2 reelle.
!
! Pour encoder un nombre illimite de niveaux, on ne decrit que le niveau courant
! et pas l'integralite de la coordonnee. De toute facon l'en-tete grib n'est pas
! utilisee en relecture. REK
IF (CDPREF=='S') THEN
!REK  KSEC2(12)=2*(INIVAU+1)
  KSEC2(12)=2
  PSEC2(1:10)=FA%CADRE(IRANGC)%XSEC2(1:10)
  PSEC2(11)=FA%CADRE(IRANGC)%XSEC2(10+KNIVAU)
  PSEC2(12)=FA%CADRE(IRANGC)%XSEC2(10+INIVAU+2+KNIVAU)
ELSE
  KSEC2(12)=0
  PSEC2(1:10+KSEC2(12))=FA%CADRE(IRANGC)%XSEC2(1:10+KSEC2(12))
ENDIF

!**
!     3.  -  SECTION 3: the bitmap section
!            As KSEC1(5)=128, the Section 3 is omitted => dummy values
!-----------------------------------------------------------------------
!
!     3.1  - INTEGER PART
!
! Flag: 0->bitmap included in the GRIB message, 1->not included
KSEC3(1)=1
! Value used at missing data points in an INTEGER data field
KSEC3(2)=0
!
!     3.2  - REAL PART
!
! Ignored
PSEC3(1)=0._JPDBLR
! Value used at missing data points in an REAL data field
PSEC3(2)=0._JPDBLR
!**
!     4.  -  SECTION 4: the binary data section (integer part only)
!-----------------------------------------------------------------------
!
! 1: Nb of data values in array PSEC4 to be encoded
KSEC4(1)=KLCHAM
! 2: Nb of bits used for each encoded value
KSEC4(2)=INBITS
! 3: Type of data (0:grid point; 128:spherical harmonic coeff)
KSEC4(3)=0
! 4: Type of packing, only for spectral fields
!    but also to allow 2nd-order packing for grid points fields
!    and for Aladin spectral fields (seen as lat-lon grid points
!    by GRIBEX).
!    (0:simple packing; 64:complex packing and 2nd-order packing) 
IF (FA%FICHIER(KRANG)%NCOGRIF(2)==0) THEN
! If no Additional flags, then 2nd-order packing is not asked!
  KSEC4(4)=0
ELSE
  KSEC4(4)=64
ENDIF
IF (LDCOSP.AND..NOT.LLMLAM) THEN
! For spherical harmonics coeff, complex packing is always done
  KSEC4(3)=128
  KSEC4(4)=64
ENDIF
! 5: Data representation (0:float; 32:integer)
KSEC4(5)=0
! 6: Additional flags indicator (0:no; 16:yes)
KSEC4(6)=FA%FICHIER(KRANG)%NCOGRIF(2)
IF (LDCOSP.AND..NOT.LLMLAM) THEN
! For spherical harmonics coeff, additional flags indicator=0
  KSEC4(6)=0
ENDIF
! 7: Reserved
KSEC4(7)=FA%FICHIER(KRANG)%NCOGRIF(3)
! 8: Nb of values indicator (0:single datum at each grid point; 64:matrix)
KSEC4(8)=0
! 9: Secondary bitmaps indicator (0:no; 32:yes)
KSEC4(9)=FA%FICHIER(KRANG)%NCOGRIF(4)
! 10: Values width indicator (0:2nd order values have constant width; 16:not)
KSEC4(10)=FA%FICHIER(KRANG)%NCOGRIF(5)
! 11: Nb of bits for 2nd order values when these have constant width
KSEC4(11)=FA%FICHIER(KRANG)%NCOGRIF(6)
IF (KSEC4(11).EQ.-99) KSEC4(11)=1-INBITS
! 12: General extended 2nd order packing (0:no; 8:yes)
! 13: Boustrophedonic ordering (0:no; 4:yes)
! 14,15: give the order of spatial differencing; if 0,0 then option rejected
KSEC4(12:15)=FA%FICHIER(KRANG)%NCOGRIF(7:10)
! 16: For complex packing, a pointer to the start of packed data values (octet nb)
KSEC4(16)=0
! 17: For complex packing, the scaling factor factor P, stored as the INTEGER
!     value P*1000 (in the range -10000,+10000): defined later
KSEC4(17)=0
! 18: For complex packing, the pentagonal resolution parameter J specifying
!     the truncation of the subset of the data not packed (32 bits)
KSEC4(18)=0
! 19-20: Idem 18 for resolution parameters K and M
KSEC4(19)=0
KSEC4(20)=0
IF (LDCOSP.AND..NOT.LLMLAM) THEN
! For spherical harmonics coeff (ARPEGE) only
  KSEC4(18)=ICPACK
  KSEC4(19)=ICPACK
  KSEC4(20)=ICPACK
ENDIF
! 21-33: Reserved
! 34-42: 'X' decoding option
KSEC4(21:FA%JPSEC4)=0
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
  CLNSPR='FAINIG'
!
  WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KRANG='',I4,  &
&       '', CDPREF='''''',A,'''''', KNIVAU='',I6,         &
&       '', CDSUFF='''''',A,'''''', LDCOSP= '',L1)')      &
&         KREP,KRANG,CDPREF(1:LEN_TRIM(CDPREF)),KNIVAU,   &
&         CDSUFF(1:LEN_TRIM(CDSUFF)),LDCOSP
  CALL FAIPAR_FORT                                        &
&                 (FA, INUMER,INIMES,KREP,.FALSE.,CLMESS, &
&                  CLNSPR,                                &
&                  CLNSPR,.FALSE.)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('FAINIG_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"

END SUBROUTINE FAINIG_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FAINIG64                                           &
&           (KREP, KRANG, CDPREF, KNIVAU, CDSUFF, LDCOSP,     &
&            KLCHAM, KSEC1, KSEC2, PSEC2, KSEC3, PSEC3, KSEC4,&
&            YDGR1TAB)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                  FA_COM_DEFAULT_INIT,  &
&                  NEW_FA_DEFAULT,       &
&                  FAGR1TAB
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KRANG                                  ! IN   
CHARACTER (LEN=*)      CDPREF                                 ! IN   
INTEGER (KIND=JPLIKB)  KNIVAU                                 ! IN   
CHARACTER (LEN=*)      CDSUFF                                 ! IN   
LOGICAL                LDCOSP                                 ! IN   
INTEGER (KIND=JPLIKB)  KLCHAM                                 ! IN   
INTEGER (KIND=JPLIKB)  KSEC1      (*)                 !   OUT
INTEGER (KIND=JPLIKB)  KSEC2      (*)                 !   OUT
REAL (KIND=JPDBLR)     PSEC2      (*)                         !   OUT
INTEGER (KIND=JPLIKB)  KSEC3      (2)                         !   OUT
REAL (KIND=JPDBLR)     PSEC3      (*)                         !   OUT
INTEGER (KIND=JPLIKB)  KSEC4      (*)                 !   OUT
TYPE (FAGR1TAB)        YDGR1TAB                               !   OUT

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAINIG_FORT                                              &
&           (FA, KREP, KRANG, CDPREF, KNIVAU, CDSUFF, LDCOSP, &
&            KLCHAM, KSEC1, KSEC2, PSEC2, KSEC3, PSEC3, KSEC4,&
&            YDGR1TAB)

END SUBROUTINE FAINIG64

SUBROUTINE FAINIG                                             &
&           (KREP, KRANG, CDPREF, KNIVAU, CDSUFF, LDCOSP,     &
&            KLCHAM, KSEC1, KSEC2, PSEC2, KSEC3, PSEC3, KSEC4,&
&            YDGR1TAB)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                  FA_COM_DEFAULT_INIT,  &
&                  NEW_FA_DEFAULT,       &
&                  FAGR1TAB
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KRANG                                  ! IN   
CHARACTER (LEN=*)      CDPREF                                 ! IN   
INTEGER (KIND=JPLIKM)  KNIVAU                                 ! IN   
CHARACTER (LEN=*)      CDSUFF                                 ! IN   
LOGICAL                LDCOSP                                 ! IN   
INTEGER (KIND=JPLIKM)  KLCHAM                                 ! IN   
INTEGER (KIND=JPLIKM)  KSEC1      (*)                 !   OUT
INTEGER (KIND=JPLIKM)  KSEC2      (*)                 !   OUT
REAL (KIND=JPDBLR)     PSEC2      (*)                         !   OUT
INTEGER (KIND=JPLIKM)  KSEC3      (2)                         !   OUT
REAL (KIND=JPDBLR)     PSEC3      (*)                         !   OUT
INTEGER (KIND=JPLIKM)  KSEC4      (*)                 !   OUT
TYPE (FAGR1TAB)        YDGR1TAB                               !   OUT

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAINIG_MT                                                &
&           (FA, KREP, KRANG, CDPREF, KNIVAU, CDSUFF, LDCOSP, &
&            KLCHAM, KSEC1, KSEC2, PSEC2, KSEC3, PSEC3, KSEC4,&
&            YDGR1TAB)

END SUBROUTINE FAINIG

SUBROUTINE FAINIG_MT                                          &
&           (FA, KREP, KRANG, CDPREF, KNIVAU, CDSUFF, LDCOSP, &
&            KLCHAM, KSEC1, KSEC2, PSEC2, KSEC3, PSEC3, KSEC4,&
&            YDGR1TAB)
USE FA_MOD, ONLY : FA_COM, FAGR1TAB
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KRANG                                  ! IN   
CHARACTER (LEN=*)      CDPREF                                 ! IN   
INTEGER (KIND=JPLIKM)  KNIVAU                                 ! IN   
CHARACTER (LEN=*)      CDSUFF                                 ! IN   
LOGICAL                LDCOSP                                 ! IN   
INTEGER (KIND=JPLIKM)  KLCHAM                                 ! IN   
INTEGER (KIND=JPLIKM)  KSEC1      (FA%JPSEC1)                 !   OUT
INTEGER (KIND=JPLIKM)  KSEC2      (FA%JPSEC2)                 !   OUT
REAL (KIND=JPDBLR)     PSEC2      (*)                         !   OUT
INTEGER (KIND=JPLIKM)  KSEC3      (2)                         !   OUT
REAL (KIND=JPDBLR)     PSEC3      (*)                         !   OUT
INTEGER (KIND=JPLIKM)  KSEC4      (FA%JPSEC4)                 !   OUT
TYPE (FAGR1TAB)        YDGR1TAB                               !   OUT
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  IRANG                                  ! IN   
INTEGER (KIND=JPLIKB)  INIVAU                                 ! IN   
INTEGER (KIND=JPLIKB)  ILCHAM                                 ! IN   
INTEGER (KIND=JPLIKB)  ISEC1      (FA%JPSEC1)                 !   OUT
INTEGER (KIND=JPLIKB)  ISEC2      (FA%JPSEC2)                 !   OUT
INTEGER (KIND=JPLIKB)  ISEC3      (2)                         !   OUT
INTEGER (KIND=JPLIKB)  ISEC4      (FA%JPSEC4)                 !   OUT
! Convert arguments

IRANG      = INT (     KRANG, JPLIKB)
INIVAU     = INT (    KNIVAU, JPLIKB)
ILCHAM     = INT (    KLCHAM, JPLIKB)

CALL FAINIG_FORT                                              &
&           (FA, IREP, IRANG, CDPREF, INIVAU, CDSUFF, LDCOSP, &
&            ILCHAM, ISEC1, ISEC2, PSEC2, ISEC3, PSEC3, ISEC4,&
&            YDGR1TAB)

KREP       = INT (      IREP, JPLIKM)
KSEC1      = INT (     ISEC1, JPLIKM)
KSEC2      = INT (     ISEC2, JPLIKM)
KSEC3      = INT (     ISEC3, JPLIKM)
KSEC4      = INT (     ISEC4, JPLIKM)

END SUBROUTINE FAINIG_MT

!INTF KREP            OUT                               
!INTF KRANG         IN                                  
!INTF CDPREF        IN                                  
!INTF KNIVAU        IN                                  
!INTF CDSUFF        IN                                  
!INTF LDCOSP        IN                                  
!INTF KLCHAM        IN                                  
!INTF KSEC1           OUT DIMS=FA%JPSEC1                
!INTF KSEC2           OUT DIMS=FA%JPSEC2                
!INTF PSEC2           OUT DIMS=*                        
!INTF KSEC3           OUT DIMS=2                        
!INTF PSEC3           OUT DIMS=*                        
!INTF KSEC4           OUT DIMS=FA%JPSEC4                
!INTF YDGR1TAB        OUT
