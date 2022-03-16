! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FAXION_FORT                                           &
&                     (FA,  PCHAME, KPUISS, KDIMNC, KLCHAM, PMIN,  &
&                      PMAX, KNBITS, LDARPE, PECART, LDMLAM,       &
&                      KNOZPA, KSTROF, KTRONC, KXLOPA )
USE FA_MOD, ONLY : FA_COM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Sous-programme INTERNE du logiciel de Fichiers ARPEGE:
!     calcul de l'erreur totale par le codage GRIB
!     sur un champ en coefficients spectraux, en mode "complex packing".
!**
!    Arguments :
!    ( Tableau ) PCHAME (Entree) ==> Champ en coef. spectraux en entree;
!                KPUISS (Entree) ==> Puissance de laplacien a utiliser;
!                KDIMNC (Entree) ==> Taille de la zone non codee;
!                KLCHAM (Entree) ==> Longueur totale du champ en c.spx.;
!                PMIN   (Entree) ==> Minimum precalcule du champ module;
!                PMAX   (Entree) ==> Maximum      "      "    "     "  ;
!                KNBITS (Entree) ==> Nombre de bits par valeur a coder;
!                LDARPE (Entree) ==> Vrai si GRIB non standard;
!                PECART (Sortie) ==> Erreur relative maximum commise,
!                                    en valeur absolue;
!                LDMLAM (Entree) ==> Vrai si fichier aladin
!    (Tableau)   KNOZPA (Entree) ==> Nombre d'onde zonal maxi par parallele;
!                                    (du pole nord vers l'equateur seulement)
!                KSTROF (Entree) ==> Sous-troncature non compactee EFFECTIVE (coef. spectraux)
!                KTRONC (Entree) ==> Troncature (NS pour Aladin)
!                KXLOPA (Entree) ==> Nombre maximum de longitudes par cercle de latitudes
!
!     Modifications
!     -------------
!
!  Juillet 1998, J. Clochard, SCEM/TTI/DAO:
!
!    -Le critere a minimiser est mis sous la forme de fonctions
!    -Pour le cas LDARPE=.TRUE., utilisation des memes gardes-fous
!     que dans CODEGA.
!
!  Septem. 2000, D. Paradis, SCEM/TTI/DEV:
!
!    -Ajustement de la puissance Dolby pour des nb d'onde (JN)
!     uniquement inferieurs a MIN(FA%MTRONC(),(FA%NXLOPA()-1)/3): on
!     s'affranchit de l'influence des grands nb d'onde sur des
!     grilles lineaires pour le choix du Dolby.
!
!  Mars 2003, R. El Khatib CNRM/GMAP:
!
!    -Optimisation : ISAMAX remplace par MAXLOC
!
!  March 2010: J. Masek - fix of precomputed optimal Laplacian power
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KPUISS, KDIMNC, KLCHAM, KNBITS, KSTROF
INTEGER (KIND=JPLIKB) KTRONC, KXLOPA
!
INTEGER (KIND=JPLIKB) KNOZPA (FA%JPXIND)
!
REAL (KIND=JPDBLR) PMIN, PMAX, PECART
REAL (KIND=JPDBLR) PCHAME(KLCHAM)
REAL (KIND=JPDBLR) ZERR  (KLCHAM)
REAL (KIND=JPDBLR) ZECART_LOC
!
LOGICAL LDARPE, LDMLAM
!
INTEGER (KIND=JPLIKB) JN, JIND, ISCALE, J
INTEGER (KIND=JPLIKB) INDICE, IPUISX, IOFF, IM
INTEGER (KIND=JPLIKB) INDLAP, IRAPOR, IPUISR
INTEGER (KIND=JPLIKB) IMLIM, IEXP, IMANT, IPUIS2
INTEGER (KIND=JPLIKB) IDEB, IFIN, ITRDOL, ILCHADO
INTEGER (KIND=JPLIKB) IEXP32, IMANT32
!
REAL (KIND=JPDBLR) ZREFER, ZDIFFR, ZCOMPA, ZAUXI1, ZAUXI2, ZS

REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('FAXION_MT',0,ZHOOK_HANDLE)
!**
!     0.  -  INITIALISATIONS FAITES AU PREMIER APPEL DU SOUS-PROGRAMME.
!-----------------------------------------------------------------------
!
IF (FA%FAXION_LLPREA) THEN
  FA%FAXION_ZEPSIL=TINY(FA%FAXION_ZEPSIL)
  FA%FAXION_ISCALX=99
  FA%FAXION_LLPREA=.FALSE.
ENDIF
!**
!     1.  -  CALCUL DIRECT, EN ESSAYANT DE MINIMISER LES OPERATIONS.
!            ( a l'aide des puissances precalculees dans FA%XLAP2D )
!-----------------------------------------------------------------------
!
IF (LDARPE) THEN
!
  ZDIFFR=PMAX-PMIN
!
  IF ( ZDIFFR .LE. FA%FAXION_ZEPSIL ) THEN
    ZAUXI1=MIN ( ABS (PMIN), ABS (PMAX) )
    IF ( ZAUXI1 .LE. FA%FAXION_ZEPSIL ) ZAUXI1=0._JPDBLR
    PMAX=SIGN (ZAUXI1,PMAX)
    PMIN=PMAX
    ZAUXI1=0._JPDBLR
    ZAUXI2=0._JPDBLR
  ELSE
    ZCOMPA=REAL (2**KNBITS-1, JPDBLR)
    ZAUXI1=ZCOMPA/ZDIFFR
    ZAUXI2=ZDIFFR/ZCOMPA
  ENDIF
!
  ZREFER=PMIN
!
ELSE
!
  CALL CONFI (PMIN,IEXP32,IMANT32,ZREFER)
  IEXP=IEXP32
  IMANT=IMANT32

  ZS = (PMAX-ZREFER)/(2**(KNBITS+1)-1)
  ZAUXI1=1._JPDBLR
  ZAUXI2=2._JPDBLR
  IF (ZS.NE.0._JPDBLR) ZS = LOG(ZS)/LOG(ZAUXI2) + ZAUXI2
  ISCALE = MIN(INT(ZS,JPLIKB),                 &
&               INT(ZS+SIGN(ZAUXI1,ZS),JPLIKB))
  ISCALE = MAX(-FA%FAXION_ISCALX,MIN(FA%FAXION_ISCALX,ISCALE))
  ZAUXI1 = ZAUXI2**(-ISCALE)
  ZAUXI2 = ZAUXI2**ISCALE
!
ENDIF
!
!  Calcul de la borne superieure de la plage des nb d'onde
!  servant pour l'ajustement du Dolby (borne inferieure=KSTROF)
!  valable uniquement dans le cas Arpege pour ne pas prendre
!  en compte les coeff spectraux relatifs aux nb d'onde non
!  quadratiques (grille lineaire).
!
IF (LDARPE.AND..NOT.LDMLAM) THEN
  ITRDOL = MIN ( KTRONC , (KXLOPA-1)/3 )
  ILCHADO = (ITRDOL+1)**2
ENDIF
!
IF (KPUISS.EQ.0) THEN
!
  IF (LDMLAM) THEN 
!$OMP PARALLEL DO PRIVATE(JN,JIND) IF(FA%LOPENMP)
    DO JN=0,KTRONC
    DO JIND=KNOZPA(2*JN+3),KNOZPA(2*JN+4)
    ZERR(JIND)=ZCRIT0(JIND)
    ENDDO
    ENDDO
!$OMP END PARALLEL DO
  ELSE
    DO J=KDIMNC+1,ILCHADO
    ZERR(J)=ZCRIT0(J)
    ENDDO
  ENDIF
!
ELSE
  IPUISX=ABS (KPUISS)
!
  IF (KPUISS.GT.0) THEN
    INDICE=1
  ELSE
    INDICE=0
  ENDIF
!
  IF (IPUISX.LE.FA%JPUILA) THEN
!
!        Dans ce cas precis, on pourrait aussi remplacer la division
!     par ZCOEFF par une multiplication par FA%XLAP2D(J,IPUISX,1-INDICE).
!     Il y aurait gain en calcul, mais reference memoire supplementaire.
!
    IF (LDMLAM) THEN
#ifndef RS6K
!$OMP PARALLEL DO PRIVATE(JN,JIND,IOFF,IM,INDLAP) &
!$OMP&  IF(FA%LOPENMP)
#endif
      DO JN=1,KTRONC
      DO JIND=KNOZPA(2*JN+3)+4,KNOZPA(2*JN+4)
      IOFF=JIND-KNOZPA(2*JN+3)
      IM=IOFF/4 
      INDLAP=((JN-1)*FA%JPXTRO)+IM
      ZERR(JIND)=ZCRITR(JIND,FA%XLAP2DA(INDLAP,IPUISX,INDICE))
      ENDDO
      ENDDO
#ifndef RS6K
!$OMP END PARALLEL DO
#endif
    ELSE
      DO J=KDIMNC+1,ILCHADO
      ZERR(J)=ZCRITR(J,FA%XLAP2D(J,IPUISX,INDICE))
      ENDDO
    ENDIF
!
  ELSEIF (IPUISX.LE.2*FA%JPUILA) THEN
    IPUIS2=IPUISX/2
!
    IF (IPUISX.EQ.2*IPUIS2) THEN
!
      IF (LDMLAM) THEN
#ifndef RS6K
!$OMP PARALLEL DO PRIVATE(JN,JIND,IOFF,IM,INDLAP) &
!$OMP&  IF(FA%LOPENMP)
#endif
        DO JN=1,KTRONC
        DO JIND=KNOZPA(2*JN+3)+4,KNOZPA(2*JN+4)
        IOFF=JIND-KNOZPA(2*JN+3)
        IM=IOFF/4 
        INDLAP=((JN-1)*FA%JPXTRO)+IM
        ZERR(JIND)=ZCRITR(JIND,                         &
&                   FA%XLAP2DA(INDLAP,IPUIS2,INDICE)**2)
        ENDDO
        ENDDO
#ifndef RS6K
!$OMP END PARALLEL DO
#endif
      ELSE
        DO J=KDIMNC+1,ILCHADO
        ZERR(J)=ZCRITR(J,FA%XLAP2D(J,IPUIS2,INDICE)**2)
        ENDDO
      ENDIF
!
    ELSE
!
    IF (LDMLAM) THEN
#ifndef RS6K
!$OMP PARALLEL DO PRIVATE(JN,JIND,IOFF,IM,INDLAP) &
!$OMP&  IF(FA%LOPENMP)
#endif
       DO JN=1,KTRONC
       DO JIND=KNOZPA(2*JN+3)+4,KNOZPA(2*JN+4)
        IOFF=JIND-KNOZPA(2*JN+3)
        IM=IOFF/4 
        INDLAP=((JN-1)*FA%JPXTRO)+IM
        ZERR(JIND)=ZCRITR(JIND,FA%XLAP2DA(INDLAP,FA%JPUILA,INDICE) &
&             *FA%XLAP2DA(INDLAP,IPUISX-FA%JPUILA,INDICE))
       ENDDO
       ENDDO
#ifndef RS6K
!$OMP END PARALLEL DO
#endif
    ELSE
        DO J=KDIMNC+1,ILCHADO
        ZERR(J)=ZCRITR(J,FA%XLAP2D(J,FA%JPUILA,INDICE) &
&             *FA%XLAP2D(J,IPUISX-FA%JPUILA,INDICE))
        ENDDO
    ENDIF
!
    ENDIF
!
  ELSE
    IRAPOR=1+(IPUISX-1)/FA%JPUILA
    IPUISR=IPUISX/IRAPOR
!
    IF (IPUISX.EQ.IRAPOR*IPUISR) THEN
!
      IF (LDMLAM) THEN
#ifndef RS6K
!$OMP PARALLEL DO PRIVATE(JN,JIND,IOFF,IM,INDLAP) &
!$OMP&  IF(FA%LOPENMP)
#endif
        DO JN=1,KTRONC
        DO JIND=KNOZPA(2*JN+3)+4,KNOZPA(2*JN+4)
        IOFF=JIND-KNOZPA(2*JN+3)
        IM=IOFF/4 
        INDLAP=((JN-1)*FA%JPXTRO)+IM
        ZERR(JIND)=ZCRITR(JIND,                          &
&               FA%XLAP2DA(INDLAP,IPUISR,INDICE)**IRAPOR)
        ENDDO
        ENDDO
#ifndef RS6K
!$OMP END PARALLEL DO
#endif
      ELSE
        DO J=KDIMNC+1,ILCHADO
        ZERR(J)=ZCRITR(J,FA%XLAP2D(J,IPUISR,INDICE)**IRAPOR)
        ENDDO
      ENDIF
!
    ELSE
!
     IF (LDMLAM) THEN
#ifndef RS6K
!$OMP PARALLEL DO PRIVATE(JN,JIND,IOFF,IM,INDLAP) &
!$OMP&  IF(FA%LOPENMP)
#endif
        DO JN=1,KTRONC
        DO JIND=KNOZPA(2*JN+3)+4,KNOZPA(2*JN+4)
        IOFF=JIND-KNOZPA(2*JN+3)
        IM=IOFF/4 
        INDLAP=((JN-1)*FA%JPXTRO)+IM
        ZERR(JIND)=ZCRITR(JIND,                                   &
&           FA%XLAP2DA(INDLAP,FA%JPUILA,INDICE)**(IRAPOR-1)        &
&          *FA%XLAP2DA(INDLAP,IPUISX-FA%JPUILA*(IRAPOR-1),INDICE))
        ENDDO
        ENDDO
#ifndef RS6K
!$OMP END PARALLEL DO
#endif
     ELSE
        DO J=KDIMNC+1,ILCHADO
        ZERR(J)=ZCRITR(J,                                      &
&             FA%XLAP2D(J,FA%JPUILA,INDICE)**(IRAPOR-1)        &
&            *FA%XLAP2D(J,IPUISX-FA%JPUILA*(IRAPOR-1),INDICE))
        ENDDO
      ENDIF
!
    ENDIF
!
  ENDIF
!
ENDIF
!
PECART=0._JPDBLR
IF (LDMLAM) THEN
!$OMP PARALLEL IF(FA%LOPENMP)                         &
!$OMP& PRIVATE(JN,IMLIM,IDEB,IFIN,JIND,ZECART_LOC)    &
!$OMP& REDUCTION(+:PECART)
!$OMP DO
  DO JN=1,KTRONC
  IMLIM=KSTROF-JN
  IDEB=MAX(KNOZPA(2*JN+3)+4,KNOZPA(2*JN+3)+4*(1+IMLIM))
  IFIN=KNOZPA(2*JN+4)
  ZECART_LOC=0._JPDBLR
  DO JIND=IDEB,IFIN
    ZECART_LOC=ZECART_LOC+ZERR(JIND)
  ENDDO
  PECART=PECART+ZECART_LOC
!
  ENDDO
!$OMP END DO 
!$OMP END PARALLEL
ELSE
  DO J=KDIMNC+1,ILCHADO
    PECART=PECART+ZERR(J)
  ENDDO
ENDIF
!
IF (FA%LFAMOP) THEN
  IF (LDARPE.AND..NOT.LDMLAM) THEN
    WRITE (UNIT=FA%NULOUT,FMT=*)                                    &
&           'FAXION: KPUISS=', KPUISS,', KDIMNC=',KDIMNC,           &
&           ', KLCHAM=', KLCHAM, ', ITRDOL=', ITRDOL, ', ILCHADO=', &
&           ILCHADO, ', PMIN=', PMIN, ', PMAX=', PMAX
  ELSE
    WRITE (UNIT=FA%NULOUT,FMT=*)                          &
&           'FAXION: KPUISS=', KPUISS,', KDIMNC=',KDIMNC, &
&           ', KLCHAM=', KLCHAM,                          &
&           ', PMIN=', PMIN, ', PMAX=', PMAX
  ENDIF
  WRITE (UNIT=FA%NULOUT,FMT=*)'FAXION: PECART=', PECART
ENDIF
!
IF (LHOOK) CALL DR_HOOK('FAXION_MT',1,ZHOOK_HANDLE)

CONTAINS

REAL (KIND=JPDBLR) FUNCTION ZYPR (IZZZZZ, ZCOEFF)
INTEGER (KIND=JPLIKB) :: IZZZZZ
REAL (KIND=JPDBLR)    :: ZCOEFF
ZYPR = ZREFER + ZAUXI2 * ANINT (ZAUXI1 * (PCHAME(IZZZZZ)/ZCOEFF - ZREFER))
END FUNCTION

REAL (KIND=JPDBLR) FUNCTION ZYP0 (IZZZZZ)
INTEGER (KIND=JPLIKB) :: IZZZZZ
ZYP0 = ZREFER + ZAUXI2 * ANINT (ZAUXI1 * (PCHAME(IZZZZZ) - ZREFER))
END FUNCTION

!
!       Fonction "critere" a minimiser lorsque la modulation de la
!       puissance de laplacien est possible.
!
REAL (KIND=JPDBLR) FUNCTION ZCRITR (IZZZZZ, ZCOEFF)
INTEGER (KIND=JPLIKB) :: IZZZZZ
REAL (KIND=JPDBLR)    :: ZCOEFF
ZCRITR = (ZYPR(IZZZZZ,ZCOEFF)*ZCOEFF - PCHAME(IZZZZZ))**2
END FUNCTION

!
!       Meme chose, mais pour la puissance 0.
!
REAL (KIND=JPDBLR) FUNCTION ZCRIT0 (IZZZZZ)
INTEGER (KIND=JPLIKB) :: IZZZZZ
ZCRIT0 = (ZYP0(IZZZZZ) - PCHAME(IZZZZZ))**2
END FUNCTION

END SUBROUTINE

!INTF PCHAME        IN    DIMS=KLCHAM                   
!INTF KPUISS        IN                                  
!INTF KDIMNC        IN                                  
!INTF KLCHAM        IN                                  
!INTF PMIN          IN                                  
!INTF PMAX          IN                                  
!INTF KNBITS        IN                                  
!INTF LDARPE        IN                                  
!INTF PECART          OUT                               
!INTF LDMLAM        IN                                  
!INTF KNOZPA        IN    DIMS=FA%JPXIND                
!INTF KSTROF        IN                                  
!INTF KTRONC        IN                                  
!INTF KXLOPA        IN                                  
