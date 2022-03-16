! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FACSIM_FORT                                    &
&                     (FA,  KREP, KRANG, PCHAME, PCHAMS,  &
&                      KPULAS, KSTRON)
USE FA_MOD, ONLY : FA_COM, JPNIIL
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Sous-programme INTERNE du logiciel de Fichiers ARPEGE:
!     traitement des champs en coefficients spectraux, preparatoire
!     au codage GRIB.
!              ( Coefficients Spectraux, Integration Methodique ! )
!**
!    Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                KRANG  (Entree) ==> Rang de l'unite logique;
!    ( Tableau ) PCHAME (Entree) ==> Champ en coef. spectraux en entree;
!    ( Tableau ) PCHAMS (Sortie) ==> Champ en sortie, partie a coder;
!                KPULAS (Sortie) ==> Puissance de laplacien utilisee.
!                KSTRON (Entree) ==> Niveau de sous-troncature non
!                                    compactee.
!*
!       En mode multi-taches, il doit y avoir verrouillage du fichier
!     concerne avant l'appel au sous-programme.
!
!     Modifications
!     -------------
!
!  Juillet 1998, J. Clochard, SCEM/TTI/DAO:
!
!    -Reinitialisation de tableaux utilises pour le calcul iteratif
!     au changement de sens de balayage.
!    -Plus de "IF" pour le calcul d'extrema dans le cas ALADIN.
!    -Diagnostic plus precis en mode "mise au point".
!
!  Octobre 1998, J. Clochard, SCEM/TTI/DAO:
!
!    -Ajout de l'argument d'appel KSTRON pour compatibilite avec
!     evaluation dynamique (eventuelle) de la sous-troncature en
!     fonction de la troncature et du nombre de bits par valeur
!     compactee.
!
!  Avril   2004, D. Paradis,  DSI/DEV:
!
!    -Initialisations des tableaux XLAPxDx et FLAP1Dx faites
!     en debut de routine par appel a FAIXLA et FAIFLA.
!
!  April  2009, F. Vana and NEC:
!
!    - OpenMP directives
!
!  March 2010: J. Masek - fix of precomputed optimal Laplacian power
!              F. Vana  - simplification of IFC_SMAX,IFC_SMIN for
!                              better performance
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KRANG, KPULAS, KSTRON
!
REAL (KIND=JPDBLR) PCHAME (*), PCHAMS (*)
!
INTEGER (KIND=JPLIKB) IDIMNC, IRANGC, ITRONC, IPUFLA
INTEGER (KIND=JPLIKB) JN, J
INTEGER (KIND=JPLIKB) IMLIM, IOFF, IM, IMOD, INDLAP
INTEGER (KIND=JPLIKB) INDZ, ILONG, IDECAL, IMINI
INTEGER (KIND=JPLIKB) IMAXI, ILCHAM, INBITS, IMTRONC
INTEGER (KIND=JPLIKB) IMODPL, JIND
INTEGER (KIND=JPLIKB) IMEILL, JSENS, INDICE, IPUISS
INTEGER (KIND=JPLIKB) IPOSEX, JMODPL
INTEGER (KIND=JPLIKB) IPLUS, IMOINS, IPUISX, IPUIS2
INTEGER (KIND=JPLIKB) IRAPOR, IPUISR, INIMES
INTEGER (KIND=JPLIKB) INUMER, IDEB, IFIN, IXLOPA
INTEGER (KIND=JPLIKB) IPULAS (0:1)
!
REAL (KIND=JPDBLR) ZMIN, ZMAX, ZERRXI, ZERRXF, ZBIGVA
REAL (KIND=JPDBLR) ZMINI (FA%JPXTRO,0:2),ZMAXI (FA%JPXTRO,0:2)
REAL (KIND=JPDBLR) Z(4*FA%JPXTRO*FA%JPXTRO,2)
REAL (KIND=JPDBLR) ZECART (2,0:1)
!
LOGICAL LLARPE,LLMLAM
!
INTEGER (KIND=JPLIKB), EXTERNAL :: ISMIN_164 , ISMAX_164 
!
CHARACTER(LEN=FA%JPXNOM) CLACTI 
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA

!**
!     1.  -  CONTROLES DES PARAMETRES D'APPEL, INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FACSIM_MT',0,ZHOOK_HANDLE)
CLACTI=''
IDIMNC=0
ZBIGVA=HUGE(ZBIGVA)
!
IF (KRANG.LE.0.OR.KRANG.GT.FA%JPNXFA) THEN
  KREP=-66
  GOTO 1001
ENDIF
!
! Si ce n'est pas encore fait, initialisation des tableaux XLAP... et FA%FLAP1D.
!
IF (FA%LIXLAP) THEN
  CALL FAIXLA_FORT           &
&                 (FA)
  FA%LIXLAP = .FALSE.
ENDIF
IF (FA%FICHIER(KRANG)%LIFLAP) THEN
  CALL FAIFLA_FORT           &
&                (FA, KRANG)
  FA%FICHIER(KRANG)%LIFLAP = .FALSE.
ENDIF
!
IRANGC=FA%FICHIER(KRANG)%NUCADR
ITRONC=FA%CADRE(IRANGC)%MTRONC
IXLOPA=FA%CADRE(IRANGC)%NXLOPA
LLMLAM=FA%CADRE(IRANGC)%LIMLAM
!
IF (LLMLAM) IMTRONC=FA%CADRE(IRANGC)%NOZPAR(2)
IF (ITRONC.LE.KSTRON) THEN
  KREP=-88
  GOTO 1001
ELSEIF (LLMLAM.AND.IMTRONC.LE.KSTRON) THEN
  KREP=-88
  GOTO 1001
ELSEIF (LLMLAM.AND.(IMTRONC.GT.3*ITRONC.OR. &
&    ITRONC.GT.3*IMTRONC)) THEN
! Il s'agit d'un garde-fou, modifiable (ne pas oublier FARCIS et FAPULA)
  KREP=-114
  GOTO 1001
ELSE       
  KREP=0
ENDIF
!
IPUFLA=FA%FICHIER(KRANG)%NPUFLA
IMODPL=FA%FICHIER(KRANG)%NMFDPL
!
IF (LLMLAM) THEN
   ILCHAM=FA%CADRE(IRANGC)%NSFLAM
   IDIMNC=4*(1+ITRONC+IMTRONC+(KSTRON*(KSTRON-1))/2)
!DP      IDIMNC=FA%NOZPAR(5,IRANGC)+4*KSTRON-1
ELSE
   ILCHAM=(1+ITRONC)**2
   IDIMNC=(1+KSTRON)**2
ENDIF
!**
!     2.  -  DETERMINATION DE LA "MEILLEURE" PUISSANCE DE LAPLACIEN
!            POUR LA PARTIE DU CHAMP QUI SERA COMPACTEE EN "GRIB".
!-----------------------------------------------------------------------
!
IF (IMODPL.EQ.0) THEN
!
!           On elimine le cas ou aucune modulation de la puissance
!         de laplacien n'est possible.
!
  KPULAS=IPUFLA
  GOTO 300
ENDIF
!*
!     2.1 -  AMORCAGE DU PROCESSUS ITERATIF: CALCUL DES EXTREMA DU CHAMP
!            MULTIPLIE PAR LA PUISSANCE DE LAPLACIEN NOMINALE DU FICHIER
!            ( le traitement est decoupe nombre d'onde "n" par "n" )
!-----------------------------------------------------------------------
!
!       Calcul des extrema du champ d'entree (partie a compacter),
!     pour chaque nombre d'onde "n".
!
IF (LLMLAM) THEN
  ZMIN=ZBIGVA
  ZMAX=-ZBIGVA
!$OMP PARALLEL DO IF(FA%LOPENMP)                                &
!$OMP&PRIVATE(JN,IMLIM,IDEB,IFIN,JIND,IOFF,IM,IMOD,INDLAP,INDZ) &
!$OMP&REDUCTION(MAX:ZMAX) REDUCTION(MIN:ZMIN)
  DO JN=1,ITRONC
   IMLIM=KSTRON-JN
   IDEB=MAX(FA%CADRE(IRANGC)%NOZPAR(2*JN+3)+4*(1+IMLIM), &
&          FA%CADRE(IRANGC)%NOZPAR(2*JN+3)+4)
   IFIN=FA%CADRE(IRANGC)%NOZPAR(2*JN+4)
  DO JIND=IDEB,IFIN
    IOFF=JIND-FA%CADRE(IRANGC)%NOZPAR(2*JN+3)
    IM=IOFF/4
    IMOD=MOD(IOFF,4_JPLIKB )
!
    INDLAP=((JN-1)*FA%JPXTRO)+IM
    INDZ=IMOD*FA%JPXTRO*FA%JPXTRO+INDLAP
    Z(INDZ,1)=PCHAME(JIND)*FA%FICHIER(KRANG)%FLAP1DA(INDLAP)
    ZMAX=MAX (ZMAX,Z(INDZ,1))
    ZMIN=MIN (ZMIN,Z(INDZ,1))
!
  ENDDO
  ENDDO
!$OMP END PARALLEL DO
ELSE
  DO JN=KSTRON+1,ITRONC
  ILONG=2*JN+1
  IDECAL=JN**2
  IMAXI=ISMAX_164  (ILONG, PCHAME(IDECAL+1))
  ZMAXI(JN,0)=PCHAME(IDECAL+IMAXI)
  IMINI=ISMIN_164  (ILONG, PCHAME(IDECAL+1))
  ZMINI(JN,0)=PCHAME(IDECAL+IMINI)
  ENDDO
!
!
!
  DO JN=KSTRON+1,ITRONC
  ZMAXI(JN,1)=ZMAXI(JN,0)*FA%FICHIER(KRANG)%FLAP1D(JN)
  ZMINI(JN,1)=ZMINI(JN,0)*FA%FICHIER(KRANG)%FLAP1D(JN)
  ENDDO
!
!
  IMAXI=KSTRON+ISMAX_164  &
&                (ITRONC-KSTRON,ZMAXI(KSTRON+1,1))
  IMINI=KSTRON+ISMIN_164  &
&                (ITRONC-KSTRON,ZMINI(KSTRON+1,1))
  ZMIN=ZMINI(IMINI,1)
  ZMAX=ZMAXI(IMAXI,1)
ENDIF
!
INBITS=FA%FICHIER(KRANG)%NBFCSP
LLARPE=FA%FICHIER(KRANG)%NFGRIB.EQ.2
!
IF (ZMAX.LE.ZMIN) THEN
!
!           On elimine le cas trivial du champ constant,
!         eventuellement apres transformation...
!
  KPULAS=IPUFLA
  GOTO 300
ENDIF
!
!        Calcul de l'erreur de compactage initiale.
!
CALL FAXION_FORT                                                              &
&               (FA, PCHAME,IPUFLA,IDIMNC,ILCHAM,ZMIN,                        &
&                ZMAX,INBITS,LLARPE,ZERRXI,LLMLAM,FA%CADRE(IRANGC)%NOZPAR(1), &
&                KSTRON,ITRONC,IXLOPA)
IMEILL=0
ZECART(2,IMEILL)=ZERRXI
!*
!     2.3 -  BOUCLE SUR LES DEGRES DE MODULATION POSSIBLES,
!            PAR INCREMENTS DE PUISSANCE VALANT +1 (ESSAYE EN PREMIER)
!            PUIS (-1).
!-----------------------------------------------------------------------
!
DO 239 JSENS=1,-1,-2
INDICE=(1-JSENS)/2
IPUISS=IPUFLA
ZECART(1,INDICE)=ZERRXI
IPOSEX=2
!
IF (JSENS.EQ.-1) THEN
!
!       Compte-tenu du caractere "incremental" du calcul des extrema
!       pour des puissances successives, on doit reinitialiser lors du
!       changement de sens de balayage ZMAXI et ZMINI pour le cas ARPEGE
!       et Z pour le cas ALADIN.
!
  IF (LLMLAM) THEN
!
    ZMIN=ZBIGVA
    ZMAX=-ZBIGVA
!$OMP PARALLEL DO IF(FA%LOPENMP)                                 &
!$OMP&PRIVATE(JN,IMLIM,IDEB,IFIN,JIND,IOFF,IM,IMOD,INDLAP,INDZ)
    DO JN=1,ITRONC
    IMLIM=KSTRON-JN
    IDEB=MAX(FA%CADRE(IRANGC)%NOZPAR(2*JN+3)+4*(1+IMLIM), &
&           FA%CADRE(IRANGC)%NOZPAR(2*JN+3)+4)
    IFIN=FA%CADRE(IRANGC)%NOZPAR(2*JN+4)
    DO JIND=IDEB,IFIN
    IOFF=JIND-FA%CADRE(IRANGC)%NOZPAR(2*JN+3)
    IM=IOFF/4
    IMOD=MOD(IOFF,4_JPLIKB )
!
       INDLAP=((JN-1)*FA%JPXTRO)+IM
       INDZ=IMOD*FA%JPXTRO*FA%JPXTRO+INDLAP
       Z(INDZ,1)=PCHAME(JIND)*FA%FICHIER(KRANG)%FLAP1DA(INDLAP)
!
    ENDDO
    ENDDO
!$OMP END PARALLEL DO
!
  ELSE
!
    DO JN=KSTRON+1,ITRONC
    ZMAXI(JN,1)=ZMAXI(JN,0)*FA%FICHIER(KRANG)%FLAP1D(JN)
    ZMINI(JN,1)=ZMINI(JN,0)*FA%FICHIER(KRANG)%FLAP1D(JN)
    ENDDO
!
  ENDIF
!
ENDIF
!
DO JMODPL=1,IMODPL
IPUISS=IPUISS+JSENS
!
IF (LLMLAM) THEN
  ZMIN=ZBIGVA
  ZMAX=-ZBIGVA
!$OMP PARALLEL DO IF(FA%LOPENMP)                                 &
!$OMP&PRIVATE(JN,IMLIM,IDEB,IFIN,JIND,IOFF,IM,IMOD,INDLAP,INDZ)  &
!$OMP&REDUCTION(MAX:ZMAX) REDUCTION(MIN:ZMIN)
  DO JN=1,ITRONC
  IMLIM=KSTRON-JN
  IDEB=MAX(FA%CADRE(IRANGC)%NOZPAR(2*JN+3)+4*(1+IMLIM), &
&         FA%CADRE(IRANGC)%NOZPAR(2*JN+3)+4)
  IFIN=FA%CADRE(IRANGC)%NOZPAR(2*JN+4)
!ocl novrec
  DO JIND=IDEB,IFIN
  IOFF=JIND-FA%CADRE(IRANGC)%NOZPAR(2*JN+3)
  IM=IOFF/4
  IMOD=MOD(IOFF,4_JPLIKB )
!
     INDLAP=((JN-1)*FA%JPXTRO)+IM
     INDZ=IMOD*FA%JPXTRO*FA%JPXTRO+((JN-1)*FA%JPXTRO)+IM
     Z(INDZ,IPOSEX)=Z(INDZ,3-IPOSEX)* &
&     FA%XLAP1DA(INDLAP,INDICE)
     ZMAX=MAX (ZMAX,Z(INDZ,IPOSEX))
     ZMIN=MIN (ZMIN,Z(INDZ,IPOSEX))
!
  ENDDO
  ENDDO
!$OMP END PARALLEL DO
ELSE 
  DO JN=KSTRON+1,ITRONC
  ZMAXI(JN,IPOSEX)=ZMAXI(JN,3-IPOSEX)*FA%XLAP1D(JN,INDICE)
  ZMINI(JN,IPOSEX)=ZMINI(JN,3-IPOSEX)*FA%XLAP1D(JN,INDICE)
  ENDDO
!
  IMAXI=KSTRON+ISMAX_164  &
&               (ITRONC-KSTRON,ZMAXI(KSTRON+1,IPOSEX))
  IMINI=KSTRON+ISMIN_164  &
&               (ITRONC-KSTRON,ZMINI(KSTRON+1,IPOSEX))
  ZMIN=ZMINI(IMINI,IPOSEX)
  ZMAX=ZMAXI(IMAXI,IPOSEX)
ENDIF
!
IF (ZMAX.LE.ZMIN) THEN
!
!           On elimine le cas du champ constant...
!
  KPULAS=IPUISS
  GOTO 240
ENDIF
!
!        Calcul de la nouvelle erreur de compactage.
!
CALL FAXION_FORT                                                      &
&               (FA, PCHAME,IPUISS,IDIMNC,ILCHAM,ZMIN,ZMAX,INBITS,    &
&                LLARPE,ZECART(IPOSEX,INDICE),LLMLAM,                 &
&                FA%CADRE(IRANGC)%NOZPAR(1),KSTRON,ITRONC,IXLOPA)
!
IF (ZECART(IPOSEX,INDICE).GE.ZECART(3-IPOSEX,INDICE)) THEN
!
!        Ecart pas meilleur que celui calcule precedemment, on s'arrete.
!
  IPULAS(INDICE)=IPUISS-JSENS
  GOTO 239
ENDIF
!
IPOSEX=3-IPOSEX
ENDDO
!
!        On a epuise les degres de modulation possibles... on plafonne.
!                    (pour un sens de balayage)
!
IPULAS(INDICE)=IPUISS
239 CONTINUE
!
!        Choix du meilleur resultat obtenu dans les 2 sens de balayage.
!
IPLUS=1+MOD (IPULAS(0)-IPUFLA,2_JPLIKB )
IMOINS=1+MOD (IPUFLA-IPULAS(1),2_JPLIKB )
!
IF (ZECART(IPLUS,0).LE.ZECART(IMOINS,1)) THEN
  IMEILL=0
ELSE
  IMEILL=1
ENDIF
!
KPULAS=IPULAS(IMEILL)
!
240 CONTINUE
!*
!     2.4 -  DIAGNOSTICS EVENTUELS, EN MODE MISE AU POINT SEULEMENT.
!-----------------------------------------------------------------------
!
IF (FA%LFAMOP) THEN
  ZERRXF=MIN (ZECART(1,IMEILL),ZECART(2,IMEILL))
  WRITE (UNIT=FA%NULOUT,FMT=*)                               &
&         'FACSIM - Erreur Initiale (P=',IPUFLA,') ',ZERRXI, &
&         ', Finale (P=',KPULAS,') ', ZERRXF
ENDIF
!**
!     3.  -  TRANSFORMATION DE LA PARTIE A COMPACTER DU CHAMP.
!-----------------------------------------------------------------------
!
300 CONTINUE
!
!        On fait des multiplications plutot que des divisions,
!     et on essaie d'eviter l'exponentiation.
!
IF (KPULAS.EQ.0) THEN
!
  IF (LLMLAM) THEN
!$OMP PARALLEL DO PRIVATE(JN,JIND) IF(FA%LOPENMP)
    DO JN=0,ITRONC
    DO JIND=FA%CADRE(IRANGC)%NOZPAR(2*JN+3),FA%CADRE(IRANGC)%NOZPAR(2*JN+4)
    PCHAMS(JIND)=PCHAME(JIND)
    ENDDO
    ENDDO
!$OMP END PARALLEL DO
  ELSE
    DO J=IDIMNC+1,ILCHAM
    PCHAMS(J)=PCHAME(J)
    ENDDO
  ENDIF
!
ELSE
  IPUISX=ABS (KPULAS)
!
  IF (KPULAS.GT.0) THEN
    INDICE=0
  ELSE
    INDICE=1
  ENDIF
!
  IF (IPUISX.LE.FA%JPUILA) THEN
!
    IF (LLMLAM) THEN
!$OMP PARALLEL DO PRIVATE(JN,JIND,IOFF,IM,INDLAP) IF(FA%LOPENMP)
      DO JN=1,ITRONC
      DO JIND=FA%CADRE(IRANGC)%NOZPAR(2*JN+3)+4, &
&                   FA%CADRE(IRANGC)%NOZPAR(2*JN+4)
      IOFF=JIND-FA%CADRE(IRANGC)%NOZPAR(2*JN+3)
      IM=IOFF/4
      INDLAP=((JN-1)*FA%JPXTRO)+IM
      PCHAMS(JIND)=PCHAME(JIND)*FA%XLAP2DA(INDLAP,IPUISX,INDICE)
      ENDDO
      ENDDO
!$OMP END PARALLEL DO
    ELSE
      DO J=IDIMNC+1,ILCHAM
      PCHAMS(J)=PCHAME(J)*FA%XLAP2D(J,IPUISX,INDICE)
      ENDDO
    ENDIF
!
  ELSEIF (IPUISX.LE.2*FA%JPUILA) THEN
    IPUIS2=IPUISX/2
!
    IF (IPUISX.EQ.2*IPUIS2) THEN
!
      IF (LLMLAM) THEN
!$OMP PARALLEL DO PRIVATE(JN,JIND,IOFF,IM,INDLAP) IF(FA%LOPENMP)
        DO JN=1,ITRONC
        DO JIND=FA%CADRE(IRANGC)%NOZPAR(2*JN+3)+4, &
&                     FA%CADRE(IRANGC)%NOZPAR(2*JN+4)
        IOFF=JIND-FA%CADRE(IRANGC)%NOZPAR(2*JN+3)
        IM=IOFF/4
        INDLAP=((JN-1)*FA%JPXTRO)+IM
        PCHAMS(JIND)=PCHAME(JIND)*                       &
&                     FA%XLAP2DA(INDLAP,IPUIS2,INDICE)**2
        ENDDO
        ENDDO
!$OMP END PARALLEL DO
      ELSE
        DO J=IDIMNC+1,ILCHAM
        PCHAMS(J)=PCHAME(J)*FA%XLAP2D(J,IPUIS2,INDICE)**2
        ENDDO
      ENDIF
!
    ELSE
!
      IF (LLMLAM) THEN
!$OMP PARALLEL DO PRIVATE(JN,JIND,IOFF,IM,INDLAP) IF(FA%LOPENMP)
        DO JN=1,ITRONC
        DO JIND=FA%CADRE(IRANGC)%NOZPAR(2*JN+3)+4, &
&                     FA%CADRE(IRANGC)%NOZPAR(2*JN+4)     
        IOFF=JIND-FA%CADRE(IRANGC)%NOZPAR(2*JN+3)
        IM=IOFF/4
        INDLAP=((JN-1)*FA%JPXTRO)+IM
        PCHAMS(JIND)=PCHAME(JIND)*                          &
&                FA%XLAP2DA(INDLAP,FA%JPUILA,INDICE)         &
&                *FA%XLAP2DA(INDLAP,IPUISX-FA%JPUILA,INDICE)
        ENDDO
        ENDDO
!$OMP END PARALLEL DO

      ELSE
        DO J=IDIMNC+1,ILCHAM
        PCHAMS(J)=PCHAME(J)*FA%XLAP2D(J,FA%JPUILA,INDICE) &
&                *FA%XLAP2D(J,IPUISX-FA%JPUILA,INDICE)
        ENDDO
      ENDIF
    ENDIF
!
  ELSE
    IRAPOR=1+(IPUISX-1)/FA%JPUILA
    IPUISR=IPUISX/IRAPOR
!
    IF (IPUISX.EQ.IRAPOR*IPUISR) THEN
! 
      IF (LLMLAM) THEN
!$OMP PARALLEL DO PRIVATE(JN,JIND,IOFF,IM,INDLAP) IF(FA%LOPENMP)
        DO JN=1,ITRONC
        DO JIND=FA%CADRE(IRANGC)%NOZPAR(2*JN+3)+4, &
&                     FA%CADRE(IRANGC)%NOZPAR(2*JN+4)
        IOFF=JIND-FA%CADRE(IRANGC)%NOZPAR(2*JN+3)
        IM=IOFF/4
        INDLAP=((JN-1)*FA%JPXTRO)+IM
        PCHAMS(JIND)=PCHAME(JIND)*                   &
&            FA%XLAP2DA(INDLAP,IPUISR,INDICE)**IRAPOR
        ENDDO
        ENDDO
!$OMP END PARALLEL DO
      ELSE
        DO J=IDIMNC+1,ILCHAM
        PCHAMS(J)=PCHAME(J)*FA%XLAP2D(J,IPUISR,INDICE)**IRAPOR
        ENDDO
      ENDIF
! 
    ELSE
!
      IF (LLMLAM) THEN
!$OMP PARALLEL DO PRIVATE(JN,JIND,IOFF,IM,INDLAP) IF(FA%LOPENMP)
        DO JN=1,ITRONC
        DO JIND=FA%CADRE(IRANGC)%NOZPAR(2*JN+3)+4, &
&                     FA%CADRE(IRANGC)%NOZPAR(2*JN+4)
        IOFF=JIND-FA%CADRE(IRANGC)%NOZPAR(2*JN+3)
        IM=IOFF/4
        INDLAP=((JN-1)*FA%JPXTRO)+IM
        PCHAMS(JIND)=PCHAME(JIND)*                              &
&          FA%XLAP2DA(INDLAP,FA%JPUILA,INDICE)**(IRAPOR-1)*      &
&          FA%XLAP2DA(INDLAP,IPUISX-FA%JPUILA*(IRAPOR-1),INDICE)
        ENDDO
        ENDDO
!$OMP END PARALLEL DO
      ELSE
        DO J=IDIMNC+1,ILCHAM
        PCHAMS(J)=PCHAME(J)*                                     &
&                  FA%XLAP2D(J,FA%JPUILA,INDICE)**(IRAPOR-1)      &
&                *FA%XLAP2D(J,IPUISX-FA%JPUILA*(IRAPOR-1),INDICE)
        ENDDO
      ENDIF
!
    ENDIF
!
  ENDIF
!
ENDIF
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
  CLNSPR='FACSIM'
  INUMER=JPNIIL
  WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KRANG='',I4,       &
&         '', PCHAME(1)='',G12.5,'', PCHAMS('',I3,'')='',G12.5, &
&         '', KPULAS='',I3)')                                   &
&     KREP,KRANG,PCHAME(1),IDIMNC+1,PCHAMS(IDIMNC+1),KPULAS
  CALL FAIPAR_FORT                                      &
&                 (FA, INUMER,INIMES,KREP,.FALSE.,CLMESS, &
&               CLNSPR,CLACTI,.FALSE.)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('FACSIM_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"

END SUBROUTINE


!INTF KREP            OUT                               
!INTF KRANG         IN                                  
!INTF PCHAME        IN    DIMS=*                        
!INTF PCHAMS          OUT DIMS=*                        
!INTF KPULAS          OUT                               
!INTF KSTRON        IN                                  
