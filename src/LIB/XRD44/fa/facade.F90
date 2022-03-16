! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FACADE_FORT                                                &
&                     (FA,  CDNOMC, KTYPTR, PSLAPO, PCLOPO, PSLOPO,   &
&                      PCODIL, KTRONC, KNLATI, KNXLON, KNLOPA,        &
&                      KNOZPA, PSINLA, KNIVER, PREFER, PAHYBR,        &
&                      PBHYBR, LDGARD )
USE FA_MOD, ONLY : FA_COM, JPNIIL
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Sous-programme servant a DEfinir un CADre, voire a le
!     redefinir.
!**
!        Arguments : CDNOMC ==> Nom symbolique du cadre;
!  (tous d'Entree)   KTYPTR ==> Type de transformation horizontale;
!                    PSLAPO ==> Sinus de la latitude du pole d'interet;
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
!*
!        La "redefinition" d'un cadre est possible a l'une de ces
!     conditions:
!
!     - le cadre a ete defini, mais n'a aucun fichier qui s'y rattache;
!     - le cadre defini a au moins un fichier qui s'y rattache, et les
!       nouveaux parametres de definition sont identiques a ceux deja
!       definis.
!
!        Toute "redefinition" de cadre donne lieu a une messagerie
!     de niveau 1, donc non masquee par defaut.
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KTYPTR, KTRONC, KNLATI, KNXLON, KNIVER
!
INTEGER (KIND=JPLIKB) KNLOPA (FA%JPXPAH), KNOZPA (FA%JPXIND)
!
REAL (KIND=JPDBLR) PSLAPO, PCLOPO, PSLOPO, PCODIL, PREFER
!
REAL (KIND=JPDBLR) PSINLA ((1+KNLATI)/2), PAHYBR (0:KNIVER), PBHYBR (0:KNIVER)
!
CHARACTER CDNOMC*(*)
!
LOGICAL LDGARD
!
INTEGER (KIND=JPLIKB) IPHASE, IGARDE, IREP, IRANGC
INTEGER (KIND=JPLIKB) ILNOMC, INIMES, INUMER
!
LOGICAL LLREDF, LLMODC
!
!
!
CHARACTER(LEN=FA%JPXNOM) CLACTI 
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA

!**
!     1.  -  SI PREMIERE UTILISATION, APPEL AU SOUS-PROGRAMME "FARINE".
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FACADE_MT',0,ZHOOK_HANDLE)
CLACTI=''
IF (FA%FACADE_LLPREA) THEN
  CALL FARINE_FORT              &
&                 (FA, 2_JPLIKB )
  FA%FACADE_LLPREA=.FALSE.
ENDIF
!**
!     2.  -  LE TRAVAIL EST SOUS-TRAITE AU SOUS-PROGRAMME "FACADI".
!-----------------------------------------------------------------------
!
IPHASE=0
!
IF (LDGARD) THEN
  IGARDE=2
ELSE
  IGARDE=0
ENDIF
!
!             Verrouillage global prealable, si necessaire.
!
IF (FA%LFAMUL) CALL LFIVER_FORT                        &
&                              (FA%LFI, FA%VRGLAS,'ON')
!
CALL FACADI_FORT                                                &
&            (FA, IREP,CDNOMC,KTYPTR,PSLAPO,PCLOPO,             &
&             PSLOPO,PCODIL,                                    &
&             KTRONC,KNLATI,KNXLON,KNLOPA,KNOZPA,PSINLA,KNIVER, &
&             PREFER,PAHYBR,PBHYBR,LLMODC,LLREDF,IPHASE,IRANGC, &
&             ILNOMC,IGARDE)
ILNOMC=MIN (ILNOMC,FA%NCPCAD)
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE, AVEC "ABORT" EVENTUEL,
!            VIA LE sous-programme "FAIPAR" .
!-----------------------------------------------------------------------
!
!
!          Deverrouillage global eventuel.
!
IF (FA%LFAMUL) CALL LFIVER_FORT                         &
&                              (FA%LFI, FA%VRGLAS,'OFF')
!
LLFATA=LLMOER(IREP,0_JPLIKB )
!
IF (LLFATA) THEN
  INIMES=2
ELSEIF (FA%NIMSGA.EQ.0) THEN
  INIMES=0
ELSEIF (LLMODC) THEN
  INIMES=1
  WRITE (UNIT=CLMESS,FMT=                                         &
&  '(''PARAMETRES NUMERIQUES DU CADRE '''''',A,'''''' MODIFIES '', &
&    '' - CONSERVATION A LA FERMETURE DU DERNIER FICHIER= '',L1)') &
&   CDNOMC(1:ILNOMC),LDGARD
ELSEIF (LLREDF) THEN
  INIMES=1
  WRITE (UNIT=CLMESS,FMT='(''CADRE '''''',A,                 &
& '''''' REDEFINI - MEMES PARAMETRES NUMERIQUES - '',         &
& '' CONSERVATION A LA FERMETURE DU DERNIER FICHIER= '',L1)') &
&   CDNOMC(1:ILNOMC),LDGARD
ELSEIF (FA%NIMSGA.EQ.2) THEN
  INIMES=2
ELSE
  INIMES=0
ENDIF
!
IF (INIMES.EQ.0)  THEN 
  IF (LHOOK) CALL DR_HOOK('FACADE_MT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='FACADE'
INUMER=JPNIIL
!
IF (INIMES.EQ.1.AND.FA%NIMSGA.EQ.2) THEN
!
!        Cas ou il faut en fait 2 messages.
!
  CALL FAIPAR_FORT                                      &
&                 (FA, INUMER,INIMES,IREP,.FALSE.,CLMESS, &
&                  CLNSPR,CLACTI,.FALSE.)
  INIMES=2
ENDIF
!
IF (INIMES.EQ.2) THEN
!
  IF (IREP.EQ.-65.AND.ILNOMC.EQ.1) THEN
    ILNOMC=8
    CLACTI(1:ILNOMC)=FA%CHAINC(:ILNOMC)
  ELSE
    ILNOMC=MIN (INT (LEN (CLACTI), JPLIKB),ILNOMC,FA%NCPCAD)
    CLACTI(1:ILNOMC)=CDNOMC(1:ILNOMC)
  ENDIF
!
  WRITE (UNIT=CLMESS,                                             &
&         FMT='(''ARGUMENTS SIMPLES= '''''',A,'''''',''            &
&  ,I2,4('','',F7.4),3('','',I6),'','',I5,'','',F11.4,'', '',L1)') &
&       CLACTI(1:ILNOMC),KTYPTR,PSLAPO,PCLOPO,PSLOPO,PCODIL,       &
&       KTRONC,KNLATI,KNXLON,KNIVER,PREFER,LDGARD
ENDIF
!
CALL FAIPAR_FORT                                     &
&               (FA, INUMER,INIMES,IREP,LLFATA,CLMESS, &
&                CLNSPR,CLACTI(1:ILNOMC),.FALSE.)
!
IF (LHOOK) CALL DR_HOOK('FACADE_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"

END SUBROUTINE FACADE_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FACADE64                                        &
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
INTEGER (KIND=JPLIKB)  KTYPTR                                 ! IN   
REAL (KIND=JPDBLR)     PSLAPO                                 ! IN   
REAL (KIND=JPDBLR)     PCLOPO                                 ! IN   
REAL (KIND=JPDBLR)     PSLOPO                                 ! IN   
REAL (KIND=JPDBLR)     PCODIL                                 ! IN   
INTEGER (KIND=JPLIKB)  KTRONC                                 ! IN   
INTEGER (KIND=JPLIKB)  KNLATI                                 ! IN   
INTEGER (KIND=JPLIKB)  KNXLON                                 ! IN   
INTEGER (KIND=JPLIKB)  KNLOPA     (*)                         ! IN   
INTEGER (KIND=JPLIKB)  KNOZPA     (*)                         ! IN   
REAL (KIND=JPDBLR)     PSINLA     ((1+KNLATI)/2)              ! IN   
INTEGER (KIND=JPLIKB)  KNIVER                                 ! IN   
REAL (KIND=JPDBLR)     PREFER                                 ! IN   
REAL (KIND=JPDBLR)     PAHYBR     (0:KNIVER)                  ! IN   
REAL (KIND=JPDBLR)     PBHYBR     (0:KNIVER)                  ! IN   
LOGICAL                LDGARD                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FACADE_FORT                                               &
&           (FA, CDNOMC, KTYPTR, PSLAPO, PCLOPO, PSLOPO, PCODIL, &
&           KTRONC, KNLATI, KNXLON, KNLOPA, KNOZPA, PSINLA,      &
&           KNIVER, PREFER, PAHYBR, PBHYBR, LDGARD)

END SUBROUTINE FACADE64

SUBROUTINE FACADE                                          &
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
INTEGER (KIND=JPLIKM)  KTYPTR                                 ! IN   
REAL (KIND=JPDBLR)     PSLAPO                                 ! IN   
REAL (KIND=JPDBLR)     PCLOPO                                 ! IN   
REAL (KIND=JPDBLR)     PSLOPO                                 ! IN   
REAL (KIND=JPDBLR)     PCODIL                                 ! IN   
INTEGER (KIND=JPLIKM)  KTRONC                                 ! IN   
INTEGER (KIND=JPLIKM)  KNLATI                                 ! IN   
INTEGER (KIND=JPLIKM)  KNXLON                                 ! IN   
INTEGER (KIND=JPLIKM)  KNLOPA     (*)                         ! IN   
INTEGER (KIND=JPLIKM)  KNOZPA     (*)                         ! IN   
REAL (KIND=JPDBLR)     PSINLA     ((1+KNLATI)/2)              ! IN   
INTEGER (KIND=JPLIKM)  KNIVER                                 ! IN   
REAL (KIND=JPDBLR)     PREFER                                 ! IN   
REAL (KIND=JPDBLR)     PAHYBR     (0:KNIVER)                  ! IN   
REAL (KIND=JPDBLR)     PBHYBR     (0:KNIVER)                  ! IN   
LOGICAL                LDGARD                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FACADE_MT                                                 &
&           (FA, CDNOMC, KTYPTR, PSLAPO, PCLOPO, PSLOPO, PCODIL, &
&           KTRONC, KNLATI, KNXLON, KNLOPA, KNOZPA, PSINLA,      &
&           KNIVER, PREFER, PAHYBR, PBHYBR, LDGARD)

END SUBROUTINE FACADE

SUBROUTINE FACADE_MT                                           &
&           (FA, CDNOMC, KTYPTR, PSLAPO, PCLOPO, PSLOPO, PCODIL, &
&           KTRONC, KNLATI, KNXLON, KNLOPA, KNOZPA, PSINLA,      &
&           KNIVER, PREFER, PAHYBR, PBHYBR, LDGARD)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
CHARACTER (LEN=*)      CDNOMC                                 ! IN   
INTEGER (KIND=JPLIKM)  KTYPTR                                 ! IN   
REAL (KIND=JPDBLR)     PSLAPO                                 ! IN   
REAL (KIND=JPDBLR)     PCLOPO                                 ! IN   
REAL (KIND=JPDBLR)     PSLOPO                                 ! IN   
REAL (KIND=JPDBLR)     PCODIL                                 ! IN   
INTEGER (KIND=JPLIKM)  KTRONC                                 ! IN   
INTEGER (KIND=JPLIKM)  KNLATI                                 ! IN   
INTEGER (KIND=JPLIKM)  KNXLON                                 ! IN   
INTEGER (KIND=JPLIKM)  KNLOPA     (FA%JPXPAH)                 ! IN   
INTEGER (KIND=JPLIKM)  KNOZPA     (FA%JPXIND)                 ! IN   
REAL (KIND=JPDBLR)     PSINLA     ((1+KNLATI)/2)              ! IN   
INTEGER (KIND=JPLIKM)  KNIVER                                 ! IN   
REAL (KIND=JPDBLR)     PREFER                                 ! IN   
REAL (KIND=JPDBLR)     PAHYBR     (0:KNIVER)                  ! IN   
REAL (KIND=JPDBLR)     PBHYBR     (0:KNIVER)                  ! IN   
LOGICAL                LDGARD                                 ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  ITYPTR                                 ! IN   
INTEGER (KIND=JPLIKB)  ITRONC                                 ! IN   
INTEGER (KIND=JPLIKB)  INLATI                                 ! IN   
INTEGER (KIND=JPLIKB)  INXLON                                 ! IN   
INTEGER (KIND=JPLIKB)  INLOPA     (FA%JPXPAH)                 ! IN   
INTEGER (KIND=JPLIKB)  INOZPA     (FA%JPXIND)                 ! IN   
INTEGER (KIND=JPLIKB)  INIVER                                 ! IN   
! Ancillary varibles
LOGICAL                LLMLAM
INTEGER (KIND=JPLIKB)  ISZNLOPA, ISZNOZPA
! Convert arguments

LLMLAM=KTYPTR.LE.0

IF (.NOT.LLMLAM) THEN
  ISZNLOPA=INT ((1+KNLATI)/2, JPLIKB)
  ISZNOZPA=INT ((1+KNLATI)/2, JPLIKB)
ELSE
  ISZNLOPA=8
  ISZNOZPA=0
ENDIF

ITYPTR     = INT (    KTYPTR, JPLIKB)
ITRONC     = INT (    KTRONC, JPLIKB)
INLATI     = INT (    KNLATI, JPLIKB)
INXLON     = INT (    KNXLON, JPLIKB)
INIVER     = INT (    KNIVER, JPLIKB)

INLOPA (1:ISZNLOPA) = INT (KNLOPA (1:ISZNLOPA), JPLIKB)
INOZPA (1:ISZNOZPA) = INT (KNOZPA (1:ISZNOZPA), JPLIKB)

CALL FACADE_FORT                                               &
&           (FA, CDNOMC, ITYPTR, PSLAPO, PCLOPO, PSLOPO, PCODIL, &
&           ITRONC, INLATI, INXLON, INLOPA, INOZPA, PSINLA,      &
&           INIVER, PREFER, PAHYBR, PBHYBR, LDGARD)


END SUBROUTINE FACADE_MT

!INTF CDNOMC        IN                                  
!INTF KTYPTR        IN                                  
!INTF PSLAPO        IN                                  
!INTF PCLOPO        IN                                  
!INTF PSLOPO        IN                                  
!INTF PCODIL        IN                                  
!INTF KTRONC        IN                                  
!INTF KNLATI        IN                                  
!INTF KNXLON        IN                                  
!INTF KNLOPA        IN    DIMS=FA%JPXPAH                
!INTF KNOZPA        IN    DIMS=FA%JPXIND                
!INTF PSINLA        IN    DIMS=(1+KNLATI)/2             
!INTF KNIVER        IN                                  
!INTF PREFER        IN                                  
!INTF PAHYBR        IN    DIMS=0:KNIVER                 
!INTF PBHYBR        IN    DIMS=0:KNIVER                 
!INTF LDGARD        IN                                  

