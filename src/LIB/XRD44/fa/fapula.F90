! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FAPULA_FORT                                 &
&                     (FA,  KREP, KRANG, PSPEC, KPULAP)
USE FA_MOD, ONLY : FA_COM, JPNIIL
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Sous-programme INTERNE du logiciel de Fichiers ARPEGE:
!     calcul de la PUissance de LAplacien "optimale" pour aplatir
!     le spectre hors de la sous-troncature.
!
!**
!    Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                KRANG  (Entree) ==> Rang de l'unite logique;
!    ( Tableau ) PSPEC  (Entree) ==> Champ en coef. spectraux en entree;
!                KPULAP (Sortie) ==> Puissance de laplacien calculee.
!*
!
!     Creation
!     --------
!  Avril 1999, D. Paradis, SCEM/TTI/DEV:
!     Ce ss-programme est derive de la routine CALCOP du logiciel GRIBEX
!     du CEPMMT. Il retourne la puissance de laplacien (exprimee en un
!     nombre entier de milliemes) qui aplatit le mieux le spectre des
!     coefficients spectraux (hors sous-troncature).
!     Cette puissance de laplacien, P, sera l'exposant du facteur
!     .(n*(n+1))**P   pour les champs ARPEGE ou n est le nombre d'onde total
!     .(n**2+m**2)**P pour les champs ALADIN ou n est le nombre d'onde meridien
!                                            et m est le nombre d'onde zonal
!     Ce facteur sera applique aux coefficients spectraux hors sous-troncature
!     pour en reduire l'amplitude avant le codage GRIB.
!
!
!     Method.
!     -------
!
!     For ARPEGE case, consider F(n,m) = (n*(n+1))**(-P) * G(n,m),
!     where F(n,m) is the original spectral field and n the total wavenumber.
!     For ALADIN case (LAM), consider F(n,m) = (n**2+m**2)**(-P) * G(n,m),
!     where F(n,m) is the original spectral field, n the meridian
!     wavenumber and m the zonal wavenumber.
!     The aim is to minimize G in a 1 norm with respect to P.  This can only
!     partially be achieved.
!
!     1) For ARPEGE case, what we do is to compute H(n), where
!         H(n) = max(F(n,m))    with respect to m.
!
!        We then perform a least square fit for the equation
!            log(H(n)) = beta0+beta1*log(n*(n+1)).
!
!        To ensure a better fit for the lower end of the spectrum, we
!        apply an (arbitrary) weighting function before fitting
!             W(n) = 1.0 / (n-ISTRON+1)
!
!     2) For ALADIN case, what we do is to compute H(in), where
!         H(in) = max(F(n,m))      where n and m verify: k = n**2 + m**2
!           and "in" is the rank of k among all the reached values by k:
!           we are only interested in k values which verify
!           k = n**2 + m**2, with (n,m) belonging to spectrum without
!           the non-compacted sub-truncation area. We call inmax the number
!           of these k values. "in" belongs to interval 1,inmax.
!
!        We then perform a least square fit for the equation
!            log(H(in)) = beta0+beta1*log(k).
!
!        To ensure a better fit for the lower end of the spectrum, we
!        apply an (arbitrary) weighting function before fitting
!           W(in) = 1.0 / in
!
!     Reference.
!     ----------
!
!     Seber, G.A.F. (1979). Linear Regression Analyses.
!     John Wiley and Sons
!
!     ECMWF Research Department documentation of the IFS
!
!
!     Modifications
!     -------------
!
!
!
!     Subroutine arguments
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KRANG, KPULAP
!
REAL (KIND=JPDBLR) PSPEC(*)
!
!     Local arguments
!
INTEGER (KIND=JPLIKB) IRANGC, ITRONC, IMTRONC, INMAX, INMIN
INTEGER (KIND=JPLIKB) IKMAX, IDEB, IFIN, JN, JM, JK, IK, IN, IM
INTEGER (KIND=JPLIKB) IMLIM, JIND, IOFF, INUMER
INTEGER (KIND=JPLIKB) INIMES, ISTRON
INTEGER (KIND=JPLIKB), DIMENSION(:), ALLOCATABLE :: ITAB1
INTEGER (KIND=JPLIKB), DIMENSION(:), ALLOCATABLE :: ITAB2
!
REAL (KIND=JPDBLR) ZEPS, ZZ, ZXMW, ZYMW, ZWSUM, ZX, ZY
REAL (KIND=JPDBLR) ZP, ZBETA1, ZSUM1, ZSUM2
REAL (KIND=JPDBLR), DIMENSION(:), ALLOCATABLE    :: ZW, ZNORM
!
LOGICAL LLMLAM
!
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA

!**
!     1.  -  CONTROLES DES PARAMETRES D'APPEL, INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FAPULA_MT',0,ZHOOK_HANDLE)
IF (KRANG.LE.0.OR.KRANG.GT.FA%JPNXFA) THEN
  KREP=-66
  GOTO 1001
ENDIF
!
ISTRON=FA%FICHIER(KRANG)%NSTROF
IRANGC=FA%FICHIER(KRANG)%NUCADR
ITRONC=FA%CADRE(IRANGC)%MTRONC
LLMLAM=FA%CADRE(IRANGC)%LIMLAM
!
IF (LLMLAM) IMTRONC=FA%CADRE(IRANGC)%NOZPAR(2)
IF (ITRONC.LE.ISTRON) THEN
  KREP=-88
  GOTO 1001
ELSEIF (LLMLAM.AND.IMTRONC.LE.ISTRON) THEN
  KREP=-88
  GOTO 1001
ELSEIF (LLMLAM.AND.(IMTRONC.GT.3*ITRONC.OR. &
&    ITRONC.GT.3*IMTRONC)) THEN
! Il s'agit d'un garde-fou, modifiable (ne pas oublier FARCIS et FACSIM)
  KREP=-114
  GOTO 1001
ELSE       
  KREP=0
ENDIF
!
ZEPS  = 1.0E-15_JPDBLR
!
IF (LLMLAM) THEN
  IKMAX = ITRONC*ITRONC + IMTRONC*IMTRONC
!
!  Pour le cas ALADIN, il faut creer les tableaux ITAB1 et ITAB2 :
!  ITAB1(JK) = 0 sauf pour JK tel qu'il existe (n,m) appartenant au spectre
!    hors de la sous-troncature non-compactee verifiant JK = n**2 + m**2:
!    on a alors ITAB1(JK) = 1
!  ITAB2 regroupe toutes les valeurs de JK precedentes.
!
!  ITAB1 est necessaire a la creation de ITAB2 et ITAB2 permet de boucler
!  directement sur les valeurs de JK utiles, et de dimensionner au plus
!  juste les tableaux ZW et ZNORM.
!
  ALLOCATE ( ITAB1(IKMAX) )
  ITAB1 = 0
!DP#ifdef SCALAR
  ALLOCATE ( ITAB2((ITRONC-1)*(IMTRONC-1)) )
!DP#else
!DP        ALLOCATE ( ITAB2(0:(ITRONC-1)*(IMTRONC-1)) )
!DP        ITAB2 = 0
!DP#endif
  DO JM = 1,IMTRONC
    IMLIM=ISTRON-JM
    IDEB=MAX(FA%CADRE(IRANGC)%NOMPAR(2*JM+3)+4*(1+IMLIM), &
&            FA%CADRE(IRANGC)%NOMPAR(2*JM+3)+4)
    IFIN=FA%CADRE(IRANGC)%NOMPAR(2*JM+4)
    DO JIND=IDEB,IFIN
      IOFF=JIND-FA%CADRE(IRANGC)%NOMPAR(2*JM+3)
      JN=IOFF/4
      JK=(JN**2 + JM**2)
!  On conserve les valeurs de JK atteintes
      ITAB1(JK) = 1
    ENDDO
  ENDDO
!  On construit le tableau ITAB2 des valeurs de JK atteintes
  IK = 0
  DO JK = 1,IKMAX
!DP#ifdef SCALAR
    IF (ITAB1(JK).GT.0) THEN
      IK = IK+1
      ITAB2(IK) = JK
      ITAB1(JK) = IK
    ENDIF
!DP#else
!DP       IK = IK + ITAB1(JK)
!DP       ITAB2(IK) = ITAB2(IK) + JK*ITAB1(JK)
!DP       ITAB1(JK) = IK
!DP#endif
  ENDDO
  INMIN = 1
  INMAX = IK
!
!  CAS ARPEGE
!
ELSE
  INMIN = ISTRON + 1
  INMAX = ITRONC
ENDIF
!
!**
!     2.  -  CALCUL DES NORMES ET DES POIDS
!-----------------------------------------------------------------------
!
ALLOCATE ( ZW(0:INMAX), ZNORM(0:INMAX) )
ZNORM = 0.0_JPDBLR
!
IF (LLMLAM) THEN
!
!  CAS ALADIN
!
  DO JM = 1,IMTRONC
    IMLIM=ISTRON-JM
    IDEB=MAX(FA%CADRE(IRANGC)%NOMPAR(2*JM+3)+4*(1+IMLIM), &
&            FA%CADRE(IRANGC)%NOMPAR(2*JM+3)+4)
    IFIN=FA%CADRE(IRANGC)%NOMPAR(2*JM+4)
    DO JIND=IDEB,IFIN
      IOFF=JIND-FA%CADRE(IRANGC)%NOMPAR(2*JM+3)
      JN=IOFF/4
      JK=(JN**2 + JM**2)
      ZNORM(ITAB1(JK)) = MAX(ZNORM(ITAB1(JK)), ABS(PSPEC(JIND)))
    ENDDO
  ENDDO
  DEALLOCATE ( ITAB1 )
!
!  CAS ARPEGE
!
ELSE
!
  DO JN=0,INMAX
    DO JM=-JN,JN
      IM=ABS(JM)
      IF (JM < 0) THEN
        JIND=FA%CADRE(IRANGC)%NDIM0GG(IM)+(JN-IM)*2 +1
      ELSE
        JIND=FA%CADRE(IRANGC)%NDIM0GG(IM)+(JN-IM)*2
      ENDIF
      ZNORM(JN) = MAX(ZNORM(JN), ABS(PSPEC(JIND)))
    ENDDO
  ENDDO
!
ENDIF
!
ZZ = REAL(INMAX-INMIN+1,JPDBLR)
DO IN = INMIN, INMAX
  ZW(IN) = ZZ / REAL(IN-INMIN+1,JPDBLR)
!
!     Ensure the norms have a value which is not too small in case of
!     problems with math functions (e.g. LOG).
!
  IF(ZNORM(IN).LT.ZEPS) THEN
    ZNORM(IN) = ZEPS
    ZW(IN) = 100._JPDBLR*ZEPS
  ENDIF
ENDDO
!*
!     3.  -  AJUSTEMENT PAR LES MOINDRES CARRES POUR DETERMINER LA PENTE
!            (ZBETA1).
!-----------------------------------------------------------------------
!
!
!
ZXMW  = 0.0_JPDBLR
ZYMW  = 0.0_JPDBLR
ZWSUM = 0.0_JPDBLR
!
!     Sum the weighted X and Ys.
DO JN = INMIN, INMAX
  IF (LLMLAM) THEN
    ZX =  LOG(REAL(ITAB2(JN),JPDBLR))
  ELSE
    ZX =  LOG(REAL(JN*(JN+1),JPDBLR))
  ENDIF
  ZY    = LOG(ZNORM(JN))
  ZXMW  = ZXMW+ZX*ZW(JN)
  ZYMW  = ZYMW+ZY*ZW(JN)
  ZWSUM = ZWSUM+ZW(JN)
ENDDO
!
!     Form mean weighted X and Y.
ZXMW  = ZXMW / ZWSUM
ZYMW  = ZYMW / ZWSUM
ZSUM1 = 0.0_JPDBLR
ZSUM2 = 0.0_JPDBLR
!
!     Perform a least square fit for the equation
DO JN = INMIN, INMAX
  IF (LLMLAM) THEN
    ZX =  LOG(REAL(ITAB2(JN),JPDBLR))
  ELSE
    ZX =  LOG(REAL(JN*(JN+1),JPDBLR))
  ENDIF
  ZY    = LOG(ZNORM(JN))
  ZSUM1 = ZSUM1+ZW(JN)*(ZY-ZYMW)*(ZX-ZXMW)
  ZSUM2 = ZSUM2+ZW(JN)*(ZX-ZXMW)**2
ENDDO
!
!
DEALLOCATE ( ZW, ZNORM )
IF (LLMLAM) DEALLOCATE ( ITAB2 )
!
!     On peut en tirer la pente recherchee:
ZBETA1 = ZSUM1 / ZSUM2
!
!     Et finalement, la puissance de laplacien, arrondie en un
!     nombre entier de milliemes.
ZP = -ZBETA1
ZP = MAX(-9.999_JPDBLR, MIN(9.999_JPDBLR, ZP))
KPULAP = NINT( ZP * 1000.0_JPDBLR, KIND=JPLIKB )
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE EVENTUELLE,
!            VIA LE SOUS-PROGRAMME "FAIPAR" .
!-----------------------------------------------------------------------
!
1001 CONTINUE
LLFATA=LLMOER (KREP,KRANG)
!
IF (FA%LFAMOP.OR.LLFATA) THEN
  INIMES=2
  CLNSPR='FAPULA'
  INUMER=JPNIIL
  WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KRANG='',I4,  &
&         '', ISTRON='',I4,'', KPULAP='',I6)')            &
&         KREP,KRANG,ISTRON,KPULAP
  CALL FAIPAR_FORT                                        &
&                 (FA, INUMER,INIMES,KREP,.FALSE.,CLMESS, &
&                  CLNSPR,CLNSPR,.FALSE.)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('FAPULA_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"

END SUBROUTINE

!INTF KREP            OUT                               
!INTF KRANG         IN                                  
!INTF PSPEC         IN    DIMS=*                        
!INTF KPULAP          OUT                               

