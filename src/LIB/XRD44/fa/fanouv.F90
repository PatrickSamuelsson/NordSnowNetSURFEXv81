! Feb-2013 P. Marguinaud fix uninitialized output variable
! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
! Sep-2012 P. Marguinaud Remove unused local variables
SUBROUTINE FANOUV_FORT                                             &
&                     (FA, KREP, KNUMER, LDNOMM, CDNOMF, CDSTTU,   &
&                      LDERFA, LDIMST, KNIMES, KNBARP, KNBARI,     &
&                      CDNOMC)
USE FA_MOD, ONLY : FA_COM, NEW_FICHIER, JPNIIL
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Sous-programme d'initialisation SANS OUVERTURE d'une unite logique 
!        "Fichier ARPEGE". Il s'agit d'un fichier indexe, 
!        traite par le logiciel LFI.
!
!      FANOUV est derive de FAITOU, mais ne fait pas l'appel a la couche LFI
!             pour l'ouverture reelle.
!
!      utilise pour la seule compression des donnees par des processeurs
!      qui ne font pas d'ecriture effective sur disque
!
!**
!     ARGUMENTS : Ce sont les memes que pour "LFIOUV", avec CDNOMC comme
!                 argument supplementaire.
!
!                 KREP   (Sortie) ==> Code-reponse du sous-programme;
!                 KNUMER (Entree) ==> Numero de l'unite logique;
!                 LDNOMM (Entree) ==> Vrai si l'unite logique doit etre
!                                     associee a un NOM de Fichier EXP-
!                                     LICITE lors de l'"OPEN" FORTRAN;
!                 CDNOMF (Entree) ==> Nom de fichier explicite, si
!                                     *LDNOMM* est VRAI - Meme si ce
!                                     n'est pas le cas, ce *DOIT* ETRE
!                                     UN OBJET DE TYPE "CHARACTER" .
!                 CDSTTU (Entree) ==> "STATUS" pour l'"OPEN" FORTRAN
!                                     ('OLD','NEW','UNKNOWN','SCRATCH')
!                                     par defaut, mettre 'UNKNOWN';
!                 LDERFA (Entree) ==> Option d'erreur fatale;
!                 LDIMST (Entree) ==> Option impression de Statistiques
!                                     au moment de la fermeture;
!                 KNIMES (Entree) ==> Niveau de la Messagerie (0,1 ou 2)
!                                     ( 0==>Rien, 2==>Tout )
!                 KNBARP (Entree) ==> Nombre d'articles logiques prevus,
!                                     ce qui n'est utilise que lors de
!                                     la Creation du fichier,
!                                     et qui n'empeche quand meme pas
!                                     d'avoir plus d'articles logiques;
!                 KNBARI (Sortie) ==> Nombre d'articles logiques de don-
!                                     nees sur le fichier, initialement.
!                                     (zero si creation)
!                 CDNOMC (Entree) ==> Nom du CADRE associe au fichier.
!*
!     N.B. :  Pour un fichier en mode creation, ce cadre doit avoir ete
!          defini au prealable (via le sous-programme FACADE, ou par
!          l'ouverture d'un fichier preexistant).
!             Pour un fichier ARPEGE preexistant, le cadre est lu sur le
!          fichier; s'il etait deja defini auparavant, il y a controle
!          de coherence entre les deux versions du cadre.
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KNUMER, KNIMES, KNBARP, KNBARI
!
INTEGER (KIND=JPLIKB) IRANG, IRANMS, IREPOU
INTEGER (KIND=JPLIKB) ILNOMC, ILOMIN, IREP, J
INTEGER (KIND=JPLIKB) IRANER, IRANGC
INTEGER (KIND=JPLIKB) ITRONC, ILACTI, INIMES
INTEGER (KIND=JPLIKB) ITYPTR, IPUILA
!
INTEGER (KIND=JPLIKB) IDATEF (FA%JPLDAT)
!
LOGICAL LDNOMM, LDERFA, LDIMST, LLNOUF, LLNOUC, LLRLFI
!
CHARACTER CDNOMF*(*), CDSTTU*(*), CDNOMC*(*)
!
CHARACTER(LEN=FA%JPXNOM) CLACTI 
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA

!**
!     1.  -  CONTROLES DIVERS
!-----------------------------------------------------------------------
!
!     Controle sommaire sur les arguments...
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FANOUV_MT',0,ZHOOK_HANDLE)
KNBARI=0
CLACTI=''
IRANG=0
IRANER=0
IRANMS=0
IREPOU=JPNIIL
LLRLFI=.FALSE.
ILNOMC=INT (LEN (CDNOMC), JPLIKB)
ILOMIN=MIN ( INT (LEN (CDNOMF), JPLIKB),         &
&             INT (LEN (CDSTTU), JPLIKB), ILNOMC)
!
!        L'appel ci-dessous est legerement anticipe, de maniere a
!     initialiser les variables globales du logiciel s'il s'agit
!     du premier appel a un sous-programme de ce logiciel.
!
CALL FANUMU_FORT                 &
&               (FA, KNUMER,IRANG)
!        Si KNUMER est nul, alors le numero d'unite logique est
!     attribué automatiquement
IF (KNUMER == 0) THEN
  CALL FAAUTO_FORT  (FA, KNUMER, .FALSE.)
  IRANG=0
ENDIF
!
IF (ILOMIN.LE.0) THEN
  IREP=-65
  GOTO 1001
ELSEIF (IRANG.NE.0) THEN
!
!            Controle de non-ouverture prealable (au sens du logiciel)
!
  IREP=-55
  IRANMS=IRANG
  GOTO 1001
ENDIF
!
!             Verrouillage global, si necessaire.
!
!        A-t-on deja atteint le nombre limite de fichiers ARPEGE
!     ouverts simultanement ? Si non, on cherche un emplacement libre
!     dans la table FA%NULOGI (logiquement, il devrait en exister un)
!
IF (FA%NFIOUV.GE.FA%JPNXFA) THEN
  IREP=-56
  GOTO 1001
ELSE
!
  DO J=1,FA%JPNXFA
!
  IF (FA%FICHIER(J)%NULOGI.EQ.JPNIIL) THEN
    IRANG=J
    GOTO 102
  ENDIF
!
  ENDDO
!
  IREP=-66
  GOTO 1001
!
102 CONTINUE
!
ENDIF
!
!**
!     2.  -  CONTROLES SPECIFIQUES AU LOGICIEL DE FICHIERS ARPEGE.
!-----------------------------------------------------------------------
!
LLNOUF=KNBARI.EQ.0
CALL FANUCA_FORT                          &
&               (FA, CDNOMC,IRANGC,.FALSE.)
LLNOUC=IRANGC.EQ.0
!
IF (LLNOUF) THEN
!
  IF (LLNOUC) THEN
    IREP=-57
    GOTO 1001
  ELSE
!
!         Fichier en mode creation et cadre predefini... OK a ce niveau.
!
!       On ecrit les articles definissant le cadre sur le fichier,
!     ainsi qu'un article ayant pour nom l'identificateur "par defaut",
!     (en fait, le nom du cadre) de maniere a ce que cet article soit
!     sequentiellement celui qui suit le dernier article du cadre.
!
    ILNOMC=FA%CADRE(IRANGC)%NLCCAD
!
  ENDIF
!
ENDIF

ITRONC=FA%CADRE(IRANGC)%MTRONC
ITYPTR=FA%CADRE(IRANGC)%NTYPTR

CALL NEW_FICHIER (FA, FA%FICHIER(IRANG), FA%JPLDAT, ITRONC, ITYPTR)

!*
!        Controle de la Date fichier, et stockage dans FA%MADATE.
!
!  IDATEF arbitraire pour contenter FACOND
IDATEF(1) = 1993
IDATEF(2) = 9
IDATEF(3) = 2
IDATEF(4) = 0
IDATEF(5) = 0
IDATEF(6) = 1
IDATEF(7) = 0
IDATEF(8) = 0
IDATEF(9) = 1
IDATEF(10) = 0
IDATEF(11) = 0

DO J=1,FA%JPLDAT
  FA%FICHIER(IRANG)%MADATE(J)=IDATEF(J)
ENDDO

FA%FICHIER(IRANG)%MADATX (:) = 0

!**
!     3.  -  ON MET A JOUR LES TABLES RELATIVES AUX FICHIERS.
!-----------------------------------------------------------------------
!
!
IREPOU=0
!
!
FA%NFIOUV=FA%NFIOUV+1
FA%NULIND(FA%NFIOUV)=IRANG
FA%FICHIER(IRANG)%NULOGI=KNUMER
FA%FICHIER(IRANG)%NUCADR=IRANGC
!
FA%FICHIER(IRANG)%LNOMME=LDNOMM
FA%FICHIER(IRANG)%NIVOMS=KNIMES
FA%FICHIER(IRANG)%LERRFA=LDERFA
FA%FICHIER(IRANG)%LCREAF=.FALSE.
FA%FICHIER(IRANG)%NBFPDG=FA%NBIPDG
FA%FICHIER(IRANG)%NBFCSP=FA%NBICSP
FA%FICHIER(IRANG)%NPUFLA=FA%NPUILA
FA%FICHIER(IRANG)%NMFDPL=FA%NMIDPL
FA%FICHIER(IRANG)%NFGRIB=FA%NIGRIB
FA%FICHIER(IRANG)%CIDENT=CDNOMC
!
IF (ITYPTR.LT.0) THEN
  FA%FICHIER(IRANG)%NSTROF=MIN (FA%NSTROI,ITRONC-1,-ITYPTR-1)
ELSE
  FA%FICHIER(IRANG)%NSTROF=MIN (FA%NSTROI,ITRONC-1)
ENDIF
!
! Appel a FAINOC pour interpreter les eventuels defauts
! de -1 pris par FA%NBFPDG, FA%NBFCSP, FA%NSTROF et FA%NPUFLA en
! IRANG-ieme position.
!
CALL FAINOC_FORT            &
&               (FA,  IRANG )
!
IRANER=IRANG
IRANMS=IRANG
IPUILA=FA%FICHIER(IRANG)%NPUFLA
!
FA%FICHIER(IRANG)%NCOGRIF(:)=FA%NCODGRI(:)
FA%FICHIER(IRANG)%NRASHO = 0
FA%FICHIER(IRANG)%NRASVE = 0
!
!   L'initialisation de FLAP1Dx sera faite dans FACSIM
!
FA%FICHIER(IRANG)%LIFLAP=.TRUE.
!
!        On incremente le nombre de fichiers attaches au cadre specifie.
!
FA%CADRE(IRANGC)%NULCAD=FA%CADRE(IRANGC)%NULCAD+1
IREP=IREPOU
GOTO 1001
!**
!     9.  - CI-DESSOUS, ETIQUETTES DE BRANCHEMENT EN CAS D'ERREUR E/S.
!-----------------------------------------------------------------------
!
CLACTI='INQUIRE'
!
!      AU CAS OU, ON FORCE LE CODE-REPONSE ENTREE/SORTIE A ETRE POSITIF.
!
IREP=ABS (IREP)
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE, AVEC "ABORT" EVENTUEL,
!            VIA LE SOUS-PROGRAMME "LFIEMS" .
!-----------------------------------------------------------------------
!
1001 CONTINUE
KREP=IREP
LLFATA=LLMOER (IREP,IRANER)
!
IF (LLFATA) THEN
  INIMES=2
ELSE
  INIMES=IXNVMS (IRANMS)
ENDIF
!
IF (.NOT.LLFATA.AND.INIMES.EQ.0)  THEN 
  IF (LHOOK) CALL DR_HOOK('FANOUV_MT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='FANOUV'
!
IF (INIMES.EQ.2) THEN
!
  IF (ILNOMC.GT.0) THEN
    ILACTI=MIN (INT (LEN (CLACTI), JPLIKB),ILNOMC)
    CLACTI(1:ILACTI)=CDNOMC(1:ILNOMC)
  ELSE
    ILACTI=8
    CLACTI=FA%CHAINC(:ILACTI)
  ENDIF
!
  WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KNUMER='',I3,          &
&     '', LDNOMM= '',L1,'', CDSTTU='''''',A7,'''''', LDERFA= '',L1, &
&     '',  LDIMST= '',L1,                                           &
&         '', KNIMES='',I2,'', KNBARP='',I6,'' KNBARI='',I6)')      &
&   KREP,KNUMER,LDNOMM,CDSTTU,LDERFA,LDIMST,KNIMES,KNBARP,KNBARI
  CALL FAIPAR_FORT                                      &
&                 (FA, KNUMER,INIMES,IREP,.FALSE.,CLMESS, &
&                  CLNSPR,CLACTI(1:ILACTI),LLRLFI)
  CLMESS='CDNOMC='''//CLACTI(1:ILACTI)//''''
  CALL FAIPAR_FORT                                     &
&                 (FA, KNUMER,INIMES,IREP,LLFATA,CLMESS, &
&               CLNSPR,                                  &
&               CLACTI(1:ILACTI),LLRLFI)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('FANOUV_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"
#include "facom2.ixnvms.h"

END SUBROUTINE FANOUV_FORT




! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FANOUV64                                      &
&           (KREP, KNUMER, LDNOMM, CDNOMF, CDSTTU, LDERFA, &
&           LDIMST, KNIMES, KNBARP, KNBARI, CDNOMC)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
LOGICAL                LDNOMM                                 ! IN   
CHARACTER (LEN=*)      CDNOMF                                 ! IN   
CHARACTER (LEN=*)      CDSTTU                                 ! IN   
LOGICAL                LDERFA                                 ! IN   
LOGICAL                LDIMST                                 ! IN   
INTEGER (KIND=JPLIKB)  KNIMES                                 ! IN   
INTEGER (KIND=JPLIKB)  KNBARP                                 ! IN   
INTEGER (KIND=JPLIKB)  KNBARI                                 !   OUT
CHARACTER (LEN=*)      CDNOMC                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FANOUV_FORT                                             &
&           (FA, KREP, KNUMER, LDNOMM, CDNOMF, CDSTTU, LDERFA, &
&           LDIMST, KNIMES, KNBARP, KNBARI, CDNOMC)

END SUBROUTINE FANOUV64

SUBROUTINE FANOUV                                        &
&           (KREP, KNUMER, LDNOMM, CDNOMF, CDSTTU, LDERFA, &
&           LDIMST, KNIMES, KNBARP, KNBARI, CDNOMC)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
LOGICAL                LDNOMM                                 ! IN   
CHARACTER (LEN=*)      CDNOMF                                 ! IN   
CHARACTER (LEN=*)      CDSTTU                                 ! IN   
LOGICAL                LDERFA                                 ! IN   
LOGICAL                LDIMST                                 ! IN   
INTEGER (KIND=JPLIKM)  KNIMES                                 ! IN   
INTEGER (KIND=JPLIKM)  KNBARP                                 ! IN   
INTEGER (KIND=JPLIKM)  KNBARI                                 !   OUT
CHARACTER (LEN=*)      CDNOMC                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FANOUV_MT                                               &
&           (FA, KREP, KNUMER, LDNOMM, CDNOMF, CDSTTU, LDERFA, &
&           LDIMST, KNIMES, KNBARP, KNBARI, CDNOMC)

END SUBROUTINE FANOUV

SUBROUTINE FANOUV_MT                                         &
&           (FA, KREP, KNUMER, LDNOMM, CDNOMF, CDSTTU, LDERFA, &
&           LDIMST, KNIMES, KNBARP, KNBARI, CDNOMC)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
LOGICAL                LDNOMM                                 ! IN   
CHARACTER (LEN=*)      CDNOMF                                 ! IN   
CHARACTER (LEN=*)      CDSTTU                                 ! IN   
LOGICAL                LDERFA                                 ! IN   
LOGICAL                LDIMST                                 ! IN   
INTEGER (KIND=JPLIKM)  KNIMES                                 ! IN   
INTEGER (KIND=JPLIKM)  KNBARP                                 ! IN   
INTEGER (KIND=JPLIKM)  KNBARI                                 !   OUT
CHARACTER (LEN=*)      CDNOMC                                 ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  INIMES                                 ! IN   
INTEGER (KIND=JPLIKB)  INBARP                                 ! IN   
INTEGER (KIND=JPLIKB)  INBARI                                 !   OUT
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)
INIMES     = INT (    KNIMES, JPLIKB)
INBARP     = INT (    KNBARP, JPLIKB)

CALL FANOUV_FORT                                             &
&           (FA, IREP, INUMER, LDNOMM, CDNOMF, CDSTTU, LDERFA, &
&           LDIMST, INIMES, INBARP, INBARI, CDNOMC)

KREP       = INT (      IREP, JPLIKM)
KNBARI     = INT (    INBARI, JPLIKM)

IF (KNUMER == 0) THEN
  KNUMER = INT (    INUMER, JPLIKM)
ENDIF

END SUBROUTINE FANOUV_MT



!INTF KREP            OUT 
!INTF KNUMER        IN    
!INTF LDNOMM        IN    
!INTF CDNOMF        IN    
!INTF CDSTTU        IN    
!INTF LDERFA        IN    
!INTF LDIMST        IN    
!INTF KNIMES        IN    
!INTF KNBARP        IN    
!INTF KNBARI          OUT 
!INTF CDNOMC        IN    
