! Jan-2013 P. Marguinaud Use JNGEOM & JNEXPL parameters
! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FACIES_FORT                                            &
&                   (FA,  CDNOMC, KTYPTR, PSLAPO, PCLOPO, PSLOPO, &
&                    PCODIL, KTRONC, KNLATI, KNXLON, KNLOPA,      &
&                    KNOZPA, PSINLA, KNIVER, PREFER, PAHYBR,      &
&                    PBHYBR, LDGARD )
USE FA_MOD, ONLY : FA_COM, JPNIIL, JNGEOM, JNEXPL
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Sous-programme servant a obtenir le contenu d'un Cadre.
!     ( FACIES... par analogie avec FADIES, avec "C" pour Cadre... )
!**
!        Arguments : CDNOMC ==> Nom symbolique du cadre;
!  (tous de Sortie,  KTYPTR ==> Type de transformation horizontale
!   sauf CDNOMC)     PSLAPO ==> Sinus de la latitude du pole d'interet;
!                    PCLOPO ==> Cosinus " " longitude "   "       "   ;
!                    PSLOPO ==> Sinus   " " longitude "   "       "   ;
!                    PCODIL ==> Coefficient de dilatation;
!                    KTRONC ==> Troncature;
!                    KNLATI ==> Nombre de latitudes (de pole a pole);
!                    KNXLON ==> Nombre maxi de longitudes par parallele;
!         (Tableau)  KNLOPA ==> Nombre de longitudes par parallele;
!                               (du pole nord vers l'equateur seulement)
!         (Tableau)  KNOZPA ==> Nombre d'onde zonal maxi par parallele;
!                               (du pole nord vers l'equateur seulement)
!         (Tableau)  PSINLA ==> Sinus des latitudes de l'hemisphere nord
!                               (du pole nord vers l'equateur seulement)
!                    KNIVER ==> Nombre de niveaux verticaux;
!                    PREFER ==> Pression de reference (facteur multipli-
!                               catif de la premiere fonction de la
!                               coordonnee hybride)
!         (Tableau)  PAHYBR ==> Valeurs de la fonction "A" de la coordo-
!                               nnee hybride AUX LIMITES DE COUCHES;
!         (Tableau)  PBHYBR ==> Valeurs de la fonction "B" de la coordo-
!                               nnee hybride AUX LIMITES DE COUCHES;
!                    LDGARD ==> Vrai si le cadre doit etre conserve meme
!                               apres la fermeture du dernier fichier
!                               qui s'y rattache.
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KTYPTR, KTRONC, KNLATI, KNXLON, KNIVER
!
INTEGER (KIND=JPLIKB) KNLOPA (FA%JPXPAH), KNOZPA (FA%JPXIND)
!
REAL (KIND=JPDBLR) PSLAPO, PCLOPO, PSLOPO, PCODIL, PREFER
REAL (KIND=JPDBLR) PSINLA (FA%JPXGEO), PAHYBR (0:FA%JPXNIV)
REAL (KIND=JPDBLR) PBHYBR (0:FA%JPXNIV)
!
CHARACTER CDNOMC*(*)
!
LOGICAL LDGARD
!
INTEGER (KIND=JPLIKB) IREP, IRANGC, ILNOMC
INTEGER (KIND=JPLIKB) INIMES, INUMER, ILCDNO, J
INTEGER (KIND=JPLIKB) INPAHE, ISULEI, INPIND
!
LOGICAL LLVERG, LLMLAM
CHARACTER(LEN=FA%JPXNOM) CLACTI 
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA

!
!
!**
!     0.  -  SI PREMIERE UTILISATION, APPEL AU SOUS-PROGRAMME "FARINE".
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FACIES_MT',0,ZHOOK_HANDLE)
CLACTI=''
IF (FA%FACIES_LLPREA) THEN
  CALL FARINE_FORT              &
&                 (FA, 2_JPLIKB )
  FA%FACIES_LLPREA=.FALSE.
ENDIF
!**
!     1.  -  CONTROLE DE L'ARGUMENT "CDNOMC".
!-----------------------------------------------------------------------
!
LLVERG=.FALSE.
ILCDNO=INT (LEN (CDNOMC), JPLIKB)
ILNOMC=1
!
IF (ILCDNO.LE.0) THEN
  IREP=-65
  GOTO 1001
ELSEIF (CDNOMC.EQ.' ') THEN
  IREP=-68
  GOTO 1001
ENDIF
!
DO J=ILCDNO,1,-1
!
IF (CDNOMC(J:J).NE.' ') THEN
  ILNOMC=J
  GOTO 102
ENDIF
!
ENDDO
!
102 CONTINUE
!
IF (ILNOMC.GT.FA%NCPCAD) THEN
  IREP=-65
  GOTO 1001
ENDIF
!**
!     2.  -  RECHERCHE DU CADRE DANS LES TABLES.
!-----------------------------------------------------------------------
!
!             Verrouillage global prealable, si necessaire.
!
IF (FA%LFAMUL) CALL LFIVER_FORT                        &
&                              (FA%LFI, FA%VRGLAS,'ON')
LLVERG=FA%LFAMUL
!
CALL FANUCA_FORT                          &
&               (FA, CDNOMC,IRANGC,.FALSE.)
!
IF (IRANGC.EQ.0) THEN
  IREP=-51
  GOTO 1001
ENDIF
!**
!     3.  -  TRANSFERT DES TABLES DU LOGICIEL DANS LES ARGUMENTS.
!-----------------------------------------------------------------------
!
LLMLAM=FA%CADRE(IRANGC)%LIMLAM
!
KTYPTR=FA%CADRE(IRANGC)%NTYPTR
KTRONC=FA%CADRE(IRANGC)%MTRONC
KNIVER=FA%CADRE(IRANGC)%NNIVER
KNLATI=FA%CADRE(IRANGC)%NLATIT
!
IF (.NOT.LLMLAM) THEN
   INPAHE=(1+KNLATI)/2
ELSE
   ISULEI=FA%CADRE(IRANGC)%NOZPAR(1)
   INPIND=2*ISULEI+4
ENDIF
!
KNXLON=FA%CADRE(IRANGC)%NXLOPA
PSLAPO=FA%CADRE(IRANGC)%SSLAPO
PCLOPO=FA%CADRE(IRANGC)%SCLOPO
PSLOPO=FA%CADRE(IRANGC)%SSLOPO
PCODIL=FA%CADRE(IRANGC)%SCODIL
PREFER=FA%CADRE(IRANGC)%SPREFE
LDGARD=FA%CADRE(IRANGC)%NGARDE.EQ.2.OR.             &
&       (FA%CADRE(IRANGC)%NGARDE.EQ.1.AND.FA%LIGARD)
!
IF (.NOT.LLMLAM) THEN
!
   DO J=1,INPAHE
   KNLOPA(J)=FA%CADRE(IRANGC)%NLOPAR(J)
   KNOZPA(J)=FA%CADRE(IRANGC)%NOZPAR(J)
   PSINLA(J)=FA%CADRE(IRANGC)%SINLAT(J)
   ENDDO
!
ELSE
!
   DO J=1,JNEXPL
      KNLOPA(J)=FA%CADRE(IRANGC)%NLOPAR(J)
   ENDDO
   DO J=1,INPIND
      KNOZPA(J)=FA%CADRE(IRANGC)%NOZPAR(J)
   ENDDO
   DO J=1,JNGEOM
      PSINLA(J)=FA%CADRE(IRANGC)%SINLAT(J)
   ENDDO
!
ENDIF
!
DO J=0,KNIVER
PAHYBR(J)=FA%CADRE(IRANGC)%SFOHYB(1,J)
PBHYBR(J)=FA%CADRE(IRANGC)%SFOHYB(2,J)
ENDDO
!
IREP=0
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE, AVEC "ABORT" EVENTUEL,
!            VIA LE sous-programme "FAIPAR" .
!-----------------------------------------------------------------------
!
1001 CONTINUE
!
!          Deverrouillage global eventuel.
!
IF (LLVERG) CALL LFIVER_FORT                         &
&                           (FA%LFI, FA%VRGLAS,'OFF')
!
LLFATA=IREP.NE.0.AND.FA%NRFAGA.NE.2
!
IF (LLFATA.OR.FA%NIMSGA.EQ.2) THEN
  INIMES=2
ELSE
  IF (LHOOK) CALL DR_HOOK('FACIES_MT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='FACIES'
!
IF (IREP.EQ.-65.AND.ILCDNO.LE.0) THEN
  ILNOMC=8
  CLACTI(1:ILNOMC)=FA%CHAINC(:ILNOMC)
ELSE
  ILNOMC=MIN (INT (LEN (CLACTI), JPLIKB),ILNOMC)
  CLACTI(1:ILNOMC)=CDNOMC(1:ILNOMC)
ENDIF
!
ILNOMC=MIN (ILNOMC,FA%NCPCAD)
WRITE (UNIT=CLMESS,                                               &
&       FMT='(''ARGUMENTS SIMPLES= '''''',A,'''''','',             &
&   I2,4('','',F7.4),3('','',I6),'','',I5,'','',F11.4,'', '',L1)') &
&     CLACTI(1:ILNOMC),KTYPTR,PSLAPO,PCLOPO,PSLOPO,PCODIL,         &
&     KTRONC,KNLATI,KNXLON,KNIVER,PREFER,LDGARD
INUMER=JPNIIL
CALL FAIPAR_FORT                                     &
&               (FA, INUMER,INIMES,IREP,LLFATA,CLMESS, &
&                CLNSPR,CLACTI(1:ILNOMC),.FALSE.)
!
IF (LHOOK) CALL DR_HOOK('FACIES_MT',1,ZHOOK_HANDLE)
END SUBROUTINE FACIES_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FACIES64                                        &
&           (CDNOMC, KTYPTR, PSLAPO, PCLOPO, PSLOPO, PCODIL, &
&           KTRONC, KNLATI, KNXLON, KNLOPA, KNOZPA, PSINLA,  &
&           KNIVER, PREFER, PAHYBR, PBHYBR, LDGARD)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
CHARACTER (LEN=*)      CDNOMC                                 ! IN   
INTEGER (KIND=JPLIKB)  KTYPTR                                 !   OUT
REAL (KIND=JPDBLR)     PSLAPO                                 !   OUT
REAL (KIND=JPDBLR)     PCLOPO                                 !   OUT
REAL (KIND=JPDBLR)     PSLOPO                                 !   OUT
REAL (KIND=JPDBLR)     PCODIL                                 !   OUT
INTEGER (KIND=JPLIKB)  KTRONC                                 !   OUT
INTEGER (KIND=JPLIKB)  KNLATI                                 !   OUT
INTEGER (KIND=JPLIKB)  KNXLON                                 !   OUT
INTEGER (KIND=JPLIKB)  KNLOPA     (*)                         !   OUT
INTEGER (KIND=JPLIKB)  KNOZPA     (*)                         !   OUT
REAL (KIND=JPDBLR)     PSINLA     (*)                         !   OUT
INTEGER (KIND=JPLIKB)  KNIVER                                 !   OUT
REAL (KIND=JPDBLR)     PREFER                                 !   OUT
REAL (KIND=JPDBLR)     PAHYBR     (*)                         !   OUT
REAL (KIND=JPDBLR)     PBHYBR     (*)                         !   OUT
LOGICAL                LDGARD                                 !   OUT

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FACIES_FORT                                               &
&           (FA, CDNOMC, KTYPTR, PSLAPO, PCLOPO, PSLOPO, PCODIL, &
&           KTRONC, KNLATI, KNXLON, KNLOPA, KNOZPA, PSINLA,      &
&           KNIVER, PREFER, PAHYBR, PBHYBR, LDGARD)

END SUBROUTINE FACIES64

SUBROUTINE FACIES                                          &
&           (CDNOMC, KTYPTR, PSLAPO, PCLOPO, PSLOPO, PCODIL, &
&           KTRONC, KNLATI, KNXLON, KNLOPA, KNOZPA, PSINLA,  &
&           KNIVER, PREFER, PAHYBR, PBHYBR, LDGARD)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
CHARACTER (LEN=*)      CDNOMC                                 ! IN   
INTEGER (KIND=JPLIKM)  KTYPTR                                 !   OUT
REAL (KIND=JPDBLR)     PSLAPO                                 !   OUT
REAL (KIND=JPDBLR)     PCLOPO                                 !   OUT
REAL (KIND=JPDBLR)     PSLOPO                                 !   OUT
REAL (KIND=JPDBLR)     PCODIL                                 !   OUT
INTEGER (KIND=JPLIKM)  KTRONC                                 !   OUT
INTEGER (KIND=JPLIKM)  KNLATI                                 !   OUT
INTEGER (KIND=JPLIKM)  KNXLON                                 !   OUT
INTEGER (KIND=JPLIKM)  KNLOPA     (*)                         !   OUT
INTEGER (KIND=JPLIKM)  KNOZPA     (*)                         !   OUT
REAL (KIND=JPDBLR)     PSINLA     (*)                         !   OUT
INTEGER (KIND=JPLIKM)  KNIVER                                 !   OUT
REAL (KIND=JPDBLR)     PREFER                                 !   OUT
REAL (KIND=JPDBLR)     PAHYBR     (*)                         !   OUT
REAL (KIND=JPDBLR)     PBHYBR     (*)                         !   OUT
LOGICAL                LDGARD                                 !   OUT

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FACIES_MT                                                 &
&           (FA, CDNOMC, KTYPTR, PSLAPO, PCLOPO, PSLOPO, PCODIL, &
&           KTRONC, KNLATI, KNXLON, KNLOPA, KNOZPA, PSINLA,      &
&           KNIVER, PREFER, PAHYBR, PBHYBR, LDGARD)

END SUBROUTINE FACIES

SUBROUTINE FACIES_MT                                           &
&           (FA, CDNOMC, KTYPTR, PSLAPO, PCLOPO, PSLOPO, PCODIL, &
&           KTRONC, KNLATI, KNXLON, KNLOPA, KNOZPA, PSINLA,      &
&           KNIVER, PREFER, PAHYBR, PBHYBR, LDGARD)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
CHARACTER (LEN=*)      CDNOMC                                 ! IN   
INTEGER (KIND=JPLIKM)  KTYPTR                                 !   OUT
REAL (KIND=JPDBLR)     PSLAPO                                 !   OUT
REAL (KIND=JPDBLR)     PCLOPO                                 !   OUT
REAL (KIND=JPDBLR)     PSLOPO                                 !   OUT
REAL (KIND=JPDBLR)     PCODIL                                 !   OUT
INTEGER (KIND=JPLIKM)  KTRONC                                 !   OUT
INTEGER (KIND=JPLIKM)  KNLATI                                 !   OUT
INTEGER (KIND=JPLIKM)  KNXLON                                 !   OUT
INTEGER (KIND=JPLIKM)  KNLOPA     (FA%JPXPAH)                 !   OUT
INTEGER (KIND=JPLIKM)  KNOZPA     (FA%JPXIND)                 !   OUT
REAL (KIND=JPDBLR)     PSINLA     (FA%JPXGEO)                 !   OUT
INTEGER (KIND=JPLIKM)  KNIVER                                 !   OUT
REAL (KIND=JPDBLR)     PREFER                                 !   OUT
REAL (KIND=JPDBLR)     PAHYBR     (0:FA%JPXNIV)               !   OUT
REAL (KIND=JPDBLR)     PBHYBR     (0:FA%JPXNIV)               !   OUT
LOGICAL                LDGARD                                 !   OUT
! Local integers
INTEGER (KIND=JPLIKB)  ITYPTR                                 !   OUT
INTEGER (KIND=JPLIKB)  ITRONC                                 !   OUT
INTEGER (KIND=JPLIKB)  INLATI                                 !   OUT
INTEGER (KIND=JPLIKB)  INXLON                                 !   OUT
INTEGER (KIND=JPLIKB)  INLOPA     (FA%JPXPAH)                 !   OUT
INTEGER (KIND=JPLIKB)  INOZPA     (FA%JPXIND)                 !   OUT
INTEGER (KIND=JPLIKB)  INIVER                                 !   OUT
! Ancillary variables
INTEGER (KIND=JPLIKB)  ISZNLOPA, ISZNOZPA
INTEGER (KIND=JPLIKB)  IRANGC
INTEGER (KIND=JPLIKB)  ISULEI
LOGICAL                LLMLAM
! Convert arguments


CALL FACIES_FORT                                               &
&           (FA, CDNOMC, ITYPTR, PSLAPO, PCLOPO, PSLOPO, PCODIL, &
&           ITRONC, INLATI, INXLON, INLOPA, INOZPA, PSINLA,      &
&           INIVER, PREFER, PAHYBR, PBHYBR, LDGARD)

CALL FANUCA_FORT                     &
&           (FA,CDNOMC,IRANGC,.FALSE.)

IF (IRANGC.NE.0) THEN

  LLMLAM=FA%CADRE(IRANGC)%LIMLAM

  IF (.NOT.LLMLAM) THEN
     ISZNLOPA=(1+INLATI)/2
     ISZNOZPA=(1+INLATI)/2
  ELSE
     ISULEI=FA%CADRE(IRANGC)%NOZPAR(1)
     ISZNLOPA=8
     ISZNOZPA=2*ISULEI+4
  ENDIF

  KNLOPA (1:ISZNLOPA) = INT (INLOPA (1:ISZNLOPA), JPLIKM)
  KNOZPA (1:ISZNOZPA) = INT (INOZPA (1:ISZNOZPA), JPLIKM)

ENDIF

KTYPTR     = INT (    ITYPTR, JPLIKM)
KTRONC     = INT (    ITRONC, JPLIKM)
KNLATI     = INT (    INLATI, JPLIKM)
KNXLON     = INT (    INXLON, JPLIKM)
KNIVER     = INT (    INIVER, JPLIKM)

END SUBROUTINE FACIES_MT

!INTF CDNOMC        IN                                  
!INTF KTYPTR          OUT                               
!INTF PSLAPO          OUT                               
!INTF PCLOPO          OUT                               
!INTF PSLOPO          OUT                               
!INTF PCODIL          OUT                               
!INTF KTRONC          OUT                               
!INTF KNLATI          OUT                               
!INTF KNXLON          OUT                               
!INTF KNLOPA          OUT DIMS=FA%JPXPAH                
!INTF KNOZPA          OUT DIMS=FA%JPXIND                
!INTF PSINLA          OUT DIMS=FA%JPXGEO                
!INTF KNIVER          OUT                               
!INTF PREFER          OUT                               
!INTF PAHYBR          OUT DIMS=0:FA%JPXNIV              
!INTF PBHYBR          OUT DIMS=0:FA%JPXNIV              
!INTF LDGARD          OUT                               
