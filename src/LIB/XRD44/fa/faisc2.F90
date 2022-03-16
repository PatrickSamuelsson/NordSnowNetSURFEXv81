! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FAISC2_FORT                   &
&                     (FA,  KREP, KRANGC )
USE FA_MOD, ONLY : FA_COM, JPNIIL
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!      Ce sous-programme initialise des tableaux "reference" de
!      l'en-tete GRIB, section 2: les differents types de grille
!      sont abordes (routine appelee une seule fois pour un cadre donne)
!**
!    Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                KRANGC (Entree) ==> Rang dans la table des cadres;
!*
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KRANGC
!
REAL (KIND=JPDBLR) :: ZPI, ZRAMDE, ZLATPRE,     &
&                      ZLATDER, ZLONPRE, ZLONDER
REAL (KIND=JPDBLR), PARAMETER :: ONE = 1.0_JPDBLR
!
INTEGER (KIND=JPLIKB) INLAT, INIVAU, INUMER, INIMES
!
LOGICAL LLMLAM
!
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR

!**
!     0.  -  INITIALISATIONS PREALABLES
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FAISC2_MT',0,ZHOOK_HANDLE)
KREP=0
IF (KRANGC.LE.0.OR.KRANGC.GT.FA%JPNXCA) THEN
  KREP=-66
  GOTO 1001
ENDIF
INLAT  = FA%CADRE(KRANGC)%NLATIT
INIVAU = FA%CADRE(KRANGC)%NNIVER
LLMLAM = FA%CADRE(KRANGC)%LIMLAM
ZPI = 2._JPDBLR*ASIN(1._JPDBLR)
! Conversion des radians en 1/1000 de degre
ZRAMDE = 180000._JPDBLR/ZPI
!
IF (LLMLAM) GOTO 300
!**
!     1.  -  KSEC2 POUR LA REPRESENTATION SPECTRALE ARPEGE
!-----------------------------------------------------------------------
!
! Type de representation de donnees
!
! FA%SSLAPO=sinus latitude du pole d'interet
! (si=1, pole=pole N et pas de rotation)
! FA%SCODIL=coeff de dilation (si =1, pas de dilatation)
IF ((1._JPDBLR-FA%CADRE(KRANGC)%SSLAPO).LE.1.E-10_JPDBLR) THEN
  FA%CADRE(KRANGC)%NSEC2SP(1)=70
  IF (ABS(FA%CADRE(KRANGC)%SCODIL-1._JPDBLR).LE.1.E-10_JPDBLR) THEN
    FA%CADRE(KRANGC)%NSEC2SP(1)=50
  ENDIF
ELSE
  FA%CADRE(KRANGC)%NSEC2SP(1)=80
  IF (ABS(FA%CADRE(KRANGC)%SCODIL-1._JPDBLR).LE.1.E-10_JPDBLR) THEN
    FA%CADRE(KRANGC)%NSEC2SP(1)=60
  ENDIF
ENDIF
! Troncature (3 fois la meme si triangulaire)
FA%CADRE(KRANGC)%NSEC2SP(2) =FA%CADRE(KRANGC)%MTRONC
FA%CADRE(KRANGC)%NSEC2SP(3) =FA%CADRE(KRANGC)%MTRONC
FA%CADRE(KRANGC)%NSEC2SP(4) =FA%CADRE(KRANGC)%MTRONC
! Type de representation
FA%CADRE(KRANGC)%NSEC2SP(5) =1
! Mode de representation (2->complex packing)
FA%CADRE(KRANGC)%NSEC2SP(6) =2
! Reserves
FA%CADRE(KRANGC)%NSEC2SP(7:11)=0
! Nb de parametres pour la coord verticale
! On prend ici le cas de la coordonnee hybride
! mais le cas de la coord pression sera aisement
! pris en compte + tard (KSEC2(12)=0).
FA%CADRE(KRANGC)%NSEC2SP(12)=2*(INIVAU+1)
! Latitude du pole sud de rotation
FA%CADRE(KRANGC)%NSEC2SP(13)=0
! Longitude du pole sud de rotation
FA%CADRE(KRANGC)%NSEC2SP(14)=0
! Lat et lon du pole d'etirement
IF (FA%CADRE(KRANGC)%NTYPTR.GE.2) THEN
  FA%CADRE(KRANGC)%NSEC2SP(15)=INT (ZRAMDE*ASIN(FA%CADRE(KRANGC)%SSLAPO),  &
&                             JPLIKB)
  FA%CADRE(KRANGC)%NSEC2SP(16)=INT (ZRAMDE*                       &
&    (SIGN(ONE,FA%CADRE(KRANGC)%SSLOPO)*ACOS(FA%CADRE(KRANGC)%SCLOPO)), &
&    JPLIKB)
ELSE
  FA%CADRE(KRANGC)%NSEC2SP(15)=0
  FA%CADRE(KRANGC)%NSEC2SP(16)=0
ENDIF
! Reserves
FA%CADRE(KRANGC)%NSEC2SP(17:22)=0
!**
!     2.  -  KSEC2 POUR LA GRILLE DE GAUSS (ARPEGE)
!-----------------------------------------------------------------------
!
! Type de representation de donnees
!
! FA%SSLAPO=sinus latitude du pole d'interet
! (si=1, pole=pole N et pas de rotation)
! FA%SCODIL=coeff de dilation (si =1, pas de dilatation)
IF ((1._JPDBLR-FA%CADRE(KRANGC)%SSLAPO).LE.1.E-10_JPDBLR) THEN
  FA%CADRE(KRANGC)%NSEC2GG(1)=24
  IF (ABS(FA%CADRE(KRANGC)%SCODIL-1._JPDBLR).LE.1.E-10_JPDBLR) THEN
    FA%CADRE(KRANGC)%NSEC2GG(1)=4
  ENDIF
ELSE
  FA%CADRE(KRANGC)%NSEC2GG(1)=34
  IF (ABS(FA%CADRE(KRANGC)%SCODIL-1._JPDBLR).LE.1.E-10_JPDBLR) THEN
    FA%CADRE(KRANGC)%NSEC2GG(1)=14
  ENDIF
ENDIF
! Nb de pts sur un parallele
FA%CADRE(KRANGC)%NSEC2GG(2)=FA%CADRE(KRANGC)%NXLOPA
! Nb de pts sur une longitude
FA%CADRE(KRANGC)%NSEC2GG(3)=INLAT
ZLATPRE=ASIN(MAX(-1._JPDBLR,MIN(1._JPDBLR,FA%CADRE(KRANGC)%SINLAT(1))))
! Latitude (1/1000 degre) du premier pt de grille
FA%CADRE(KRANGC)%NSEC2GG(4)=INT (ZRAMDE*ZLATPRE, JPLIKB)
! Longitude (1/1000 degre) du premier pt de grille
FA%CADRE(KRANGC)%NSEC2GG(5)=0
! Flag pour la resolution (0->on ne donne pas l'increment)
FA%CADRE(KRANGC)%NSEC2GG(6)=0
! Latitude (1/1000 degre) du dernier pt de grille
FA%CADRE(KRANGC)%NSEC2GG(7)=-FA%CADRE(KRANGC)%NSEC2GG(4)
! Longitude (1/1000 degre) du dernier pt de grille.
! (FA%NLOPAR(1,KRANGC)=nb de longitudes sur le 1er parallele)
FA%CADRE(KRANGC)%NSEC2GG(8)=-360000/FA%CADRE(KRANGC)%NLOPAR(1)
! Increment zonal (1/1000 degre)
! Pas de sens ici.
FA%CADRE(KRANGC)%NSEC2GG(9)=0
! Nb de paralleles entre le pole et l'equateur
FA%CADRE(KRANGC)%NSEC2GG(10)=(INLAT+1)/2
! Flag pour le mode de balayage
FA%CADRE(KRANGC)%NSEC2GG(11)=0
! Nombre de parametres pour la coord. verticale
FA%CADRE(KRANGC)%NSEC2GG(12)=0
! Latitude du pole sud de rotation
FA%CADRE(KRANGC)%NSEC2GG(13)=0
! Longitude du pole sud de rotation
FA%CADRE(KRANGC)%NSEC2GG(14)=0
! Latitude et longitude du pole d'etirement
IF (FA%CADRE(KRANGC)%NTYPTR.GE.2) THEN
  FA%CADRE(KRANGC)%NSEC2GG(15)=INT (ZRAMDE*ASIN(FA%CADRE(KRANGC)%SSLAPO),  &
&                             JPLIKB)
  FA%CADRE(KRANGC)%NSEC2GG(16)=INT (ZRAMDE*                        &
&     (SIGN(ONE,FA%CADRE(KRANGC)%SSLOPO)*ACOS(FA%CADRE(KRANGC)%SCLOPO)), &
&     JPLIKB)
ELSE
  FA%CADRE(KRANGC)%NSEC2GG(15)=0
  FA%CADRE(KRANGC)%NSEC2GG(16)=0
ENDIF
! Flag:      0 -> grille reguliere,  1 -> grille reduite
IF (FA%CADRE(KRANGC)%NLOPAR(1)==FA%CADRE(KRANGC)%NLOPAR((1+INLAT)/2)) THEN
  FA%CADRE(KRANGC)%NSEC2GG(17)=0
ELSE
  FA%CADRE(KRANGC)%NSEC2GG(17)=1
ENDIF
! Flag:      0 -> Terre ronde     , 64 -> Terre ellipsoide
FA%CADRE(KRANGC)%NSEC2GG(18)=0
! Flag sur les composantes des vecteurs (0->geographique, 8->grille)
FA%CADRE(KRANGC)%NSEC2GG(19)=0
! Reserves
FA%CADRE(KRANGC)%NSEC2GG(20:22)=0
! Pour les grilles reduites, nb de points sur chaque parallele
FA%CADRE(KRANGC)%NSEC2GG(23:22+(1+INLAT)/2)=            &
&                 FA%CADRE(KRANGC)%NLOPAR(1:(1+INLAT)/2)
FA%CADRE(KRANGC)%NSEC2GG(23-MOD(INLAT,2_JPLIKB )+(1+INLAT)/2:22+INLAT)=  &
&                               FA%CADRE(KRANGC)%NLOPAR((1+INLAT)/2:1:-1)
GOTO 600
!**
!     3.  -  KSEC2 POUR LA GRILLE LAT-LON (CAS FULL-POS, ARPEGE OU ALADIN)
!-------------------------------------------------------------------------
!
300 CONTINUE
! TEST POUR NEW EGGX
IF (FA%CADRE(KRANGC)%SINLAT(1) .GE. 0) THEN
! OLD EGGX
ZLATPRE=FA%CADRE(KRANGC)%SINLAT(7)
ZLONPRE=FA%CADRE(KRANGC)%SINLAT(4)
ZLATDER=FA%CADRE(KRANGC)%SINLAT(5)
ZLONDER=FA%CADRE(KRANGC)%SINLAT(6)
! Type de representation de donnees
ELSE
! NEW EGGX
ZLATPRE=FA%CADRE(KRANGC)%SINLAT(16)
ZLONPRE=FA%CADRE(KRANGC)%SINLAT(13)
ZLATDER=FA%CADRE(KRANGC)%SINLAT(14)
ZLONDER=FA%CADRE(KRANGC)%SINLAT(15)
ENDIF
!
! Type de representation de donnees
FA%CADRE(KRANGC)%NSEC2LL(1)=0
! Nb de pts sur un parallele
FA%CADRE(KRANGC)%NSEC2LL(2)=FA%CADRE(KRANGC)%NXLOPA
! Nb de pts sur une longitude
FA%CADRE(KRANGC)%NSEC2LL(3)=INLAT
! Latitude (1/1000 degre) du premier pt de grille
FA%CADRE(KRANGC)%NSEC2LL(4)=NINT(ZRAMDE*ZLATPRE,KIND=JPLIKB)
! Longitude (1/1000 degre) du premier pt de grille
FA%CADRE(KRANGC)%NSEC2LL(5)=NINT(ZRAMDE*ZLONPRE,KIND=JPLIKB)
!
CALL LON360000 (FA%CADRE(KRANGC)%NSEC2LL(5))
! Flag pour la resolution (128->on donne l'increment: grille reguliere)
FA%CADRE(KRANGC)%NSEC2LL(6)=128
! Latitude (1/1000 degre) du dernier pt de grille
FA%CADRE(KRANGC)%NSEC2LL(7)=NINT(ZRAMDE*ZLATDER,KIND=JPLIKB)
! Longitude (1/1000 degre) du dernier pt de grille
FA%CADRE(KRANGC)%NSEC2LL(8)=NINT(ZRAMDE*ZLONDER,KIND=JPLIKB)
CALL LON360000 (FA%CADRE(KRANGC)%NSEC2LL(8))
! Increment zonal (1/1000 degre)
IF (ZLONPRE.GT.ZLONDER) THEN
  FA%CADRE(KRANGC)%NSEC2LL(9)=                         &
&    NINT((ZLONDER+2._JPDBLR*ZPI-ZLONPRE)*ZRAMDE &
&         /(FA%CADRE(KRANGC)%NXLOPA-1),                &
&         KIND=JPLIKB)
ELSE
  FA%CADRE(KRANGC)%NSEC2LL(9)=                                      &
&        NINT((ZLONDER-ZLONPRE)*ZRAMDE/(FA%CADRE(KRANGC)%NXLOPA-1), &
&             KIND=JPLIKB)
ENDIF
! Increment meridien (1/1000 degre)
FA%CADRE(KRANGC)%NSEC2LL(10)=                             &
&          NINT((ZLATPRE-ZLATDER)*ZRAMDE/(INLAT-1), &
&               KIND=JPLIKB)
! Flag pour le mode de balayage: W->E et S->N = 64; W->E et N->S = 0
! Full-Pos produit des champs lat-lon ranges S->N pour ARP et ALD.
! Or la BDAP attend un rangt N->S pour les grilles lat-lon.
! FA renverse donc les champs issus de Full-Pos avant codage GRIBEX.
! 
FA%CADRE(KRANGC)%NSEC2LL(11)=0
! Nombre de parametres pour la coord. verticale
FA%CADRE(KRANGC)%NSEC2LL(12)=0
! Latitude du pole sud de rotation
FA%CADRE(KRANGC)%NSEC2LL(13)=0
! Longitude du pole sud de rotation
FA%CADRE(KRANGC)%NSEC2LL(14)=0
! Latitude et longitude du pole d'etirement
FA%CADRE(KRANGC)%NSEC2LL(15)=0
FA%CADRE(KRANGC)%NSEC2LL(16)=0
! Flag:      0 -> grille reguliere,  1 -> grille reduite
FA%CADRE(KRANGC)%NSEC2LL(17)=0
! Flag:      0 -> Terre ronde     , 64 -> Terre ellipsoide
FA%CADRE(KRANGC)%NSEC2LL(18)=0
! Flag sur les composantes des vecteurs (0->geographique, 8->grille)
FA%CADRE(KRANGC)%NSEC2LL(19)=0
! Reserves
FA%CADRE(KRANGC)%NSEC2LL(20:22)=0
!**
!     4.  -  KSEC2 POUR LA GRILLE LAT-LON QUASI-REGULIERE ALADIN
!            (en fait, tenue de camouflage pour les coeff spectraux
!             que l'on va ranger en balayant le 1/4 de l'ellipse
!             verticalement: axe X=axe n (nb d'onde meridien) et
!             axe Y=axe m (nb d'onde zonal) afin de suivre le rangt
!             dans le modele. Seuls les coeff spectraux qui seront
!             compactes sont stockes sur la grille lat-lon, soit
!             tous sauf ceux des axes et ceux inclus dans le carre
!             delimite par la ss-tronc de non-compactage).
!-----------------------------------------------------------------------
!
! Type de representation de donnees
FA%CADRE(KRANGC)%NSEC2AL(1)=0
! Nb de pts sur un parallele: valeur manquante
FA%CADRE(KRANGC)%NSEC2AL(2)=2**16 -1
! Nb de pts sur une longitude: nombre d'onde zonal max -1
! associe au nombre d'onde meridien 1 (les CSP sur les axes sont
! extraits des champs de CSP puisque non compactes)
FA%CADRE(KRANGC)%NSEC2AL(3)=(FA%CADRE(KRANGC)%NOZPAR(6)-        &
&                      FA%CADRE(KRANGC)%NOZPAR(5)+1)/4 -1
! Latitude (1/1000 degre) du premier pt de grille: valeur bidon
FA%CADRE(KRANGC)%NSEC2AL(4)=0
! Longitude (1/1000 degre) du premier pt de grille: valeur bidon
FA%CADRE(KRANGC)%NSEC2AL(5)=0
! Flag pour la resolution (128->on donne l'increment: grille reguliere)
FA%CADRE(KRANGC)%NSEC2AL(6)=0
! Latitude (1/1000 degre) du dernier pt de grille: valeur bidon
FA%CADRE(KRANGC)%NSEC2AL(7)=40000
! Longitude (1/1000 degre) du dernier pt de grille: valeur bidon
FA%CADRE(KRANGC)%NSEC2AL(8)=40000
! Increment zonal (1/1000 degre)
FA%CADRE(KRANGC)%NSEC2AL(9)=2**16 -1
! Increment meridien (1/1000 degre): deduit des valeurs bidon
FA%CADRE(KRANGC)%NSEC2AL(10)=(FA%CADRE(KRANGC)%NSEC2AL(7)-FA%CADRE(KRANGC)%NSEC2AL(4))/ &
&                   (FA%CADRE(KRANGC)%NSEC2AL(3)-1)
! Flag pour le mode de balayage
FA%CADRE(KRANGC)%NSEC2AL(11)=0
! Nombre de parametres pour la coord. verticale
FA%CADRE(KRANGC)%NSEC2AL(12)=0
! Latitude du pole sud de rotation
FA%CADRE(KRANGC)%NSEC2AL(13)=0
! Longitude du pole sud de rotation
FA%CADRE(KRANGC)%NSEC2AL(14)=0
! Latitude et longitude du pole d'etirement
FA%CADRE(KRANGC)%NSEC2AL(15)=0
FA%CADRE(KRANGC)%NSEC2AL(16)=0
! Flag:      0 -> grille reguliere,  1 -> grille reduite
FA%CADRE(KRANGC)%NSEC2AL(17)=1
! Flag:      0 -> Terre ronde     , 64 -> Terre ellipsoide
FA%CADRE(KRANGC)%NSEC2AL(18)=0
! Flag sur les composantes des vecteurs (0->geographique, 8->grille)
FA%CADRE(KRANGC)%NSEC2AL(19)=0
! Reserves
FA%CADRE(KRANGC)%NSEC2AL(20:22)=0
! Les valeurs (22+1:22+FA%MTRONC(KRANGC)-1) representant les nb de pts
! le long de chaque parallele (ici, le nb de coeff spectraux
! pour un meme n (et -n), excepte le triangle et les axes non
! compactes) dependent de la ss-troncature qui depend du fichier
! et ne seront donc pas stockes dans le tableau FA%NSEC2AL qui
! depend du cadre. Le tableau FA%NSC2ALF(FA%JPXTRO-1,FA%JPNXFA) les
! contiendra.
!
!**
!     5.  -  KSEC2 POUR LA GRILLE LAMBERT CONFORME (CAS GENERAL ALADIN)
!-------------------------------------------------------------------------
!
! Type de representation de donnees
FA%CADRE(KRANGC)%NSEC2LA(1)=3
! Nb de pts sur un parallele
FA%CADRE(KRANGC)%NSEC2LA(2)=FA%CADRE(KRANGC)%NXLOPA
! Nb de pts sur une longitude
FA%CADRE(KRANGC)%NSEC2LA(3)=INLAT
!
! Les parametres communs sont regroupes
! Flag pour la resolution (128->on donne l'increment: grille reguliere)
FA%CADRE(KRANGC)%NSEC2LA(6)=128
! Reserve
FA%CADRE(KRANGC)%NSEC2LA(8)=0
! Flag pour le mode de balayage: W->E et S->N = 64; W->E et N->S = 0
FA%CADRE(KRANGC)%NSEC2LA(11)=64
! Nombre de parametres pour la coord. verticale
FA%CADRE(KRANGC)%NSEC2LA(12)=0
! Latitude (1/1000 degre) du premier pt de grille
FA%CADRE(KRANGC)%NSEC2LA(4)=NINT(ZRAMDE*ZLATPRE,KIND=JPLIKB)
! Longitude (1/1000 degre) du premier pt de grille
FA%CADRE(KRANGC)%NSEC2LA(5)=NINT(ZRAMDE*ZLONPRE,KIND=JPLIKB)
!
CALL LON360000 (FA%CADRE(KRANGC)%NSEC2LA(5))
!
! TEST POUR OLD/NEW EGGX
IF (FA%CADRE(KRANGC)%SINLAT(1) .GE. 0) THEN
! Old EGGX
! Orientation de la grille
FA%CADRE(KRANGC)%NSEC2LA(7)=NINT(ZRAMDE*FA%CADRE(KRANGC)%SINLAT(8), &
&                          KIND=JPLIKB)
!
CALL LON360000 (FA%CADRE(KRANGC)%NSEC2LA(7))
! Dimension de la maille dans la direction X
FA%CADRE(KRANGC)%NSEC2LA(9)=NINT(FA%CADRE(KRANGC)%SINLAT(15),KIND=JPLIKB)
! Dimension de la maille dans la direction Y
FA%CADRE(KRANGC)%NSEC2LA(10)=NINT(FA%CADRE(KRANGC)%SINLAT(16),KIND=JPLIKB)
! Flag pour le centre de projection
! (0: le pole Nord est sur le plan de projection
!  et 1 seul centre de projection est utilise;
!  128: idem sauf que c'est le pole Sud)
IF (FA%CADRE(KRANGC)%SINLAT(9).GE.0) THEN
  FA%CADRE(KRANGC)%NSEC2LA(13)=0
ELSE
  FA%CADRE(KRANGC)%NSEC2LA(13)=128
ENDIF
! Premiere latitude depuis le pole ou le cone coupe la sphere
FA%CADRE(KRANGC)%NSEC2LA(14)=NINT(ZRAMDE*FA%CADRE(KRANGC)%SINLAT(9), &
&                           KIND=JPLIKB)
! Deuxieme latitude depuis le pole ou le cone coupe la sphere
! Dans Aladin, le plan de projection est rarement secant (cela
! releve plus d'un domaine mal defini que d'un choix) et cette
! possibilite va disparaitre bientot. Comme le calcul de cette
! seconde latitude n'est pas aise (pb de convergence), on va
! declarer la grille tangente! mais avec un WARNING...
FA%CADRE(KRANGC)%NSEC2LA(15)=FA%CADRE(KRANGC)%NSEC2LA(14)
IF (ABS(FA%CADRE(KRANGC)%SINLAT(10)-SIN(FA%CADRE(KRANGC)%SINLAT(9))) &
&    .GT.1.E-10_JPDBLR .AND. FA%LFAMOP) THEN
  WRITE (UNIT=FA%NULOUT,FMT=*)                                   &
&          ' FAISC2: WARNING !! La grille Lambert coupe en fait', &
&          ' la sphere, mais sera consideree comme tangente'
ENDIF
ELSE
! NEW EGGX
! Orientation de la grille
FA%CADRE(KRANGC)%NSEC2LA(7)=NINT(ZRAMDE*FA%CADRE(KRANGC)%SINLAT(3), &
&                          KIND=JPLIKB)
!
CALL LON360000 (FA%CADRE(KRANGC)%NSEC2LA(7))
! Dimension de la maille dans la direction X
FA%CADRE(KRANGC)%NSEC2LA(9)=NINT(FA%CADRE(KRANGC)%SINLAT(7),KIND=JPLIKB)
! Dimension de la maille dans la direction Y
FA%CADRE(KRANGC)%NSEC2LA(10)=NINT(FA%CADRE(KRANGC)%SINLAT(8),KIND=JPLIKB)
! Flag pour le centre de projection
! (0: le pole Nord est sur le plan de projection
!  et 1 seul centre de projection est utilise;
!  128: idem sauf que c'est le pole Sud)
IF (FA%CADRE(KRANGC)%SINLAT(4).GE.0) THEN
  FA%CADRE(KRANGC)%NSEC2LA(13)=0
ELSE
  FA%CADRE(KRANGC)%NSEC2LA(13)=128
ENDIF
! Premiere latitude depuis le pole ou le cone coupe la sphere
FA%CADRE(KRANGC)%NSEC2LA(14)=NINT(ZRAMDE*FA%CADRE(KRANGC)%SINLAT(4), &
&                           KIND=JPLIKB)
! NEW EGGX toujours tangent
FA%CADRE(KRANGC)%NSEC2LA(15)=FA%CADRE(KRANGC)%NSEC2LA(14)
ENDIF

! Reserve
FA%CADRE(KRANGC)%NSEC2LA(16)=0
! Flag:      0 -> grille reguliere
FA%CADRE(KRANGC)%NSEC2LA(17)=0
! Flag:      0 -> Terre ronde     , 64 -> Terre ellipsoide
FA%CADRE(KRANGC)%NSEC2LA(18)=0
! Flag sur les composantes des vecteurs (0->geographique, 8->grille)
FA%CADRE(KRANGC)%NSEC2LA(19)=8
! Latitude du pole sud
FA%CADRE(KRANGC)%NSEC2LA(20)=0
! Longitude du pole sud
FA%CADRE(KRANGC)%NSEC2LA(21)=0
! Reserve
FA%CADRE(KRANGC)%NSEC2LA(22)=0
!**
!     6.  -  PARTIE REELLE DE LA SECTION 2 DE GRIBEX
!-----------------------------------------------------------------------
!
600 CONTINUE
! Angle de rotation
FA%CADRE(KRANGC)%XSEC2(1)=0._JPDBLR
! Coefficient d'etirement
FA%CADRE(KRANGC)%XSEC2(2)=FA%CADRE(KRANGC)%SCODIL
! Reserve
FA%CADRE(KRANGC)%XSEC2(3:10)=0._JPDBLR
! Parametres pour la coordonnee verticale
FA%CADRE(KRANGC)%XSEC2(11:11+INIVAU)=FA%CADRE(KRANGC)%SFOHYB(1,0:INIVAU)* &
&                           FA%CADRE(KRANGC)%SPREFE
FA%CADRE(KRANGC)%XSEC2(12+INIVAU:12+2*INIVAU)= &
&     FA%CADRE(KRANGC)%SFOHYB(2,0:INIVAU)
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE, AVEC "ABORT" EVENTUEL,
!            VIA LE SOUS-PROGRAMME "FAIPAR" .
!-----------------------------------------------------------------------
!
1001 CONTINUE
!
IF (FA%LFAMOP.OR.KREP.NE.0) THEN
  INIMES=2
  CLNSPR='FAISC2'
  INUMER=JPNIIL
!
  WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KRANGC='',I4)') &
&     KREP, KRANGC
  CALL FAIPAR_FORT                                      &
&                 (FA, INUMER,INIMES,KREP,.FALSE.,CLMESS, &
&                  CLNSPR,CLNSPR,.FALSE.)
ENDIF

!
IF (LHOOK) CALL DR_HOOK('FAISC2_MT',1,ZHOOK_HANDLE)

CONTAINS

SUBROUTINE LON360000 (KLON)

INTEGER (KIND=JPLIKB) :: KLON

KLON = MODULO (KLON, 360000_JPLIKB)

END SUBROUTINE LON360000

END SUBROUTINE FAISC2_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FAISC264           &
&           (KREP, KRANGC)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KRANGC                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAISC2_FORT             &
&           (FA, KREP, KRANGC)

END SUBROUTINE FAISC264

SUBROUTINE FAISC2             &
&           (KREP, KRANGC)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KRANGC                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAISC2_MT               &
&           (FA, KREP, KRANGC)

END SUBROUTINE FAISC2

SUBROUTINE FAISC2_MT             &
&           (FA, KREP, KRANGC)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KRANGC                                 ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  IRANGC                                 ! IN   
! Convert arguments

IRANGC     = INT (    KRANGC, JPLIKB)

CALL FAISC2_FORT             &
&           (FA, IREP, IRANGC)

KREP       = INT (      IREP, JPLIKM)

END SUBROUTINE FAISC2_MT

!INTF KREP            OUT 
!INTF KRANGC        IN    
