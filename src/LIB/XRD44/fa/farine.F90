! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FARINE_FORT             &
&                     (FA,  KOPTIO )
USE FA_MOD, ONLY : FA_COM, JPNIIL
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Ce sous-programme est charge des INITIALISATIONS du logiciel
!     de Fichiers ARPEGE FA ( Routine d'INitialisation )
!**
!        Argument : KOPTIO  ==> OPTION concernant le mode d'utilisation.
!                  (Entree)     (MULTI-TACHE ou NON)
!     VALEURS POSSIBLES : 0 ==> Mode MONO-Tache prescrit;
!                         1 ==> Mode MULTI-Taches prescrit;
!                         2 ==> Utilisation du mode par defaut si c'est
!                               le premier appel; sinon on garde le mode
!                               prescrit anterieurement .
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KOPTIO
!
INTEGER (KIND=JPLIKB) IREP, J
INTEGER (KIND=JPLIKB) INIMES, INUMER
!
LOGICAL LLNMUL, LLASGN, LLREL
!
!
CHARACTER(LEN=FA%JPXNOM) CLACTI 
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA

!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FARINE_MT',0,ZHOOK_HANDLE)
CLACTI=''
IF (KOPTIO.LT.0.OR.KOPTIO.GT.2) THEN
  IREP=-52
  GOTO 1001
ENDIF
!
IF (FA%FARINE_LLPREA) THEN
!
!         C'EST LE PREMIER APPEL AU sous-programme - INITIALISATIONS .
!
  FA%NFIOUV=0
  FA%NCADEF=0
  FA%NRFAGA=1
  FA%NIMSGA=1
  FA%NBIPDG=24
  FA%NBICSP=24
  FA%NPUILA=1
  FA%NSTROI=10
  FA%NMIDPL=5
  FA%NIGRIB=2
  FA%LFAMOP=.FALSE.
  FA%FICHIER(0)%LERRFA=.TRUE.
  FA%FICHIER(0)%NIVOMS=0
  FA%NCPCAD=LEN (FA%CADRE(1)%CNOMCA)
  FA%SPSMIN=60000._JPDBLR
  FA%SPSMAX=110000._JPDBLR
  FA%MPRESX=100000
  FA%NBIMAC=64
  FA%NBIMAX=31
  FA%LIGARD=.FALSE.
  FA%NTYPTX=2
  FA%NXNIVV=FA%JPXNIV
  FA%NXTRON=FA%JPXTRO
  FA%NXLATI=FA%JPXLAT
  FA%NXLONG=FA%JPXLON
!
  DO J=1,FA%JPNXFA
    FA%FICHIER(J)%NULOGI=JPNIIL
    FA%FICHIER(J)%NRASHO=0
    FA%FICHIER(J)%NRASVE=0
  ENDDO
!
  DO J=1,FA%JPNXCA
    FA%CADRE(J)%CNOMCA=' '
  ENDDO
!
  DO J=1,FA%JPXNOM
    FA%CHAINC(J:J)='?'
  ENDDO
!
!        Descripteurs lies aux types de niveaux verticaux:
!        prefixes reconnus, extrema possibles du niveau dans le cas
!        d'une coordonnee verticale, premiers elements de xB1PAR.
!
  DO J=0,FA%JPTNIV
!
!       Initialisation par defaut pour la serie d'affectations qui suit.
!
    FA%NIVDSC(0,J)=0
    FA%NIVDSC(1,J)=0
    FA%NIVDSC(2,J)=0
!
!        Tous les cas sont reputes "niveau vrai" (pas des couches).
!
    FA%NIVDSC(4,J)=0
  ENDDO
!
!           Cas du type non reconnu.
!
  FA%NIVDSC(0,0)=0
  FA%NIVDSC(3,0)=200
!
!           Niveau hybride.
!
  FA%CTNPRF(1)='S'
  FA%NIVDSC(0,1)=3
  FA%NIVDSC(2,1)=FA%JPXNIV
  FA%NIVDSC(3,1)=109
!
!           Niveau isobare.
!
  FA%CTNPRF(2)='P'
  FA%NIVDSC(0,2)=5
  FA%NIVDSC(1,2)=1
  FA%NIVDSC(2,2)=10**5
  FA%NIVDSC(3,2)=100
!
!           Niveau iso-hauteur (au-dessus d'un relief de reference).
!
  FA%CTNPRF(3)='H'
  FA%NIVDSC(0,3)=5
  FA%NIVDSC(2,3)=10**5-1
  FA%NIVDSC(3,3)=105
!
!           Niveau iso-tourbillon_potentiel.
!
  FA%CTNPRF(4)='V'
  FA%NIVDSC(0,4)=3
  FA%NIVDSC(2,4)=10**3-1
  FA%NIVDSC(3,4)=117
!
!           Niveau iso-temperature_potentielle.
!
  FA%CTNPRF(5)='T'
  FA%NIVDSC(0,5)=3
  FA%NIVDSC(2,5)=10**3-1
  FA%NIVDSC(3,5)=113
!
!           Niveau surface.
!
  FA%CTNPRF(6)='SURF'
  FA%NIVDSC(3,6)=1
!
!           Niveau de vent max (jet).
!
  FA%CTNPRF(7)='JET'
  FA%NIVDSC(3,7)=6
!
!           Niveau surface.
!
  FA%CTNPRF(8)='TROPO'
  FA%NIVDSC(3,8)=7
!
!           Niveau moyen de la mer.
!
  FA%CTNPRF(9)='MER'
  FA%NIVDSC(3,9)=102
!
!           Niveau hybride bis pour MOCAGE
!
  FA%CTNPRF(10)='L'
  FA%NIVDSC(0,10)=3
  FA%NIVDSC(2,10)=FA%JPXNIV
!  
!           Niveau iso-temperature.
!
  FA%CTNPRF(11)='KB'
  FA%NIVDSC(0,11)=3
  FA%NIVDSC(2,11)=10**3-1
  FA%NIVDSC(3,11)=113
!
  FA%CTNPRF(12)='KT'
  FA%NIVDSC(0,12)=3
  FA%NIVDSC(2,12)=10**3-1
  FA%NIVDSC(3,12)=113
!
!           Niveau iso-hauteur (au-dessus du niveau moyen de la mer).
!
  FA%CTNPRF(13)='F'
  FA%NIVDSC(0,13)=4
  FA%NIVDSC(2,13)=10**4-1
  FA%NIVDSC(3,13)=103
!
!           Niveau SURFEX
!
  FA%CTNPRF(14)='X'
  FA%NIVDSC(0,14)=3
  FA%NIVDSC(2,14)=10**3-1
  FA%NIVDSC(3,14)=113

!
!  Initialisations pour la mise en oeuvre de GRIBEX
!
!  1/ On force GRIBEX a calculer la puissance de laplacien
!  CALL GRSMKP(1)
!  2/ On retire l'arrondi du message GRIB a un multiple de 120 octets
!  CALL GRSRND(0)
!
!  3/ Creation de la correspondance "nom article FA" et
!                                   "descripteurs GRIBEX"
  IF (.NOT. ASSOCIATED (FA%YGR1TAB)) THEN
    CALL FAICOR_FORT (FA)
  ENDIF
!
!
!  4/ Definition du codage GRIBEX par defaut:
!
!  Il s'agit d'une compression "APAC1" (meilleure solution entre
!  l'absence de compression et la compression ligne a ligne)
!  associee a la compression "general extended", a la differentiation
!  spatiale (-1: calcul dynamique de l'ordre) et au rearrangement
!  boustrophedonique.
!
!  "APAC1"
  FA%NCODGRI(1) = 1
  FA%NCODGRI(2) = 16
  FA%NCODGRI(3) = 0
  FA%NCODGRI(4) = 0
  FA%NCODGRI(5) = 16
  FA%NCODGRI(6) = 0
!  compression "general extended"
  FA%NCODGRI(7) = 8
!  arrangement boustrophedonique
  FA%NCODGRI(8) = 4
!  differentiation spatiale
  FA%NCODGRI( 9)= 0
  FA%NCODGRI(10)= -1
! Calcul automatique du facteur d'echelle decimal (KSEC1 (23))
  FA%NCODGRI(11)= 1
! Ecriture des champs GRIB1 dans un fichier externe
  FA%NCODGRI(12)= 0
!
!  5/ Initialisation de logiques pilotant des initialisations ulterieures
!
!  Il faudra initialiser XLAPxDx
  FA%LIXLAP=.TRUE.
!  Il ne faut pas initialiser FLAP1Dx(): on attend l'ouverture du fichier
  FA%FICHIER(:)%LIFLAP=.FALSE.
!  Il faudra initialiser le tableau FA%NSEC1 (section 1 GRIBEX) via FAISC1
  FA%FICHIER(:)%LISEC1=.TRUE.
!  Il faudra initialiser les tableaux NSEC2xxx et FA%XSEC2
!  (section 2 GRIBEX) via FAISC2
  FA%CADRE(:)%LISEC2=.TRUE.
!  Il faudra initialiser le tableau FA%NSC2ALF
!  (section 2 GRIBEX) via FAIS2F
  FA%FICHIER(:)%LISC2F=.TRUE.
!
  FA%FARINE_LLPREA=.FALSE.
  LLNMUL=(KOPTIO.EQ.1).OR.(KOPTIO.EQ.2.AND.FA%FARINE_LLDEFM)
  LLASGN=LLNMUL
  LLREL=.FALSE.
  CALL LFIINI_FORT               &
&                (FA%LFI, KOPTIO)
!
ELSEIF (KOPTIO.EQ.2) THEN
!
!         CE N'EST PAS LE PREMIER APPEL, MAIS COMME L'ARGUMENT VAUT 2,
!         ON LAISSE LES CHOSES EN PLACE .
!
  LLNMUL=FA%LFAMUL
  LLASGN=.FALSE.
  LLREL =.FALSE.
!
ELSE
  LLNMUL=KOPTIO.EQ.1
  LLASGN=LLNMUL.AND.(.NOT.FA%LFAMUL)
  LLREL =(.NOT.LLNMUL).AND.FA%LFAMUL
!
!     CE N'EST PAS LE PREMIER APPEL ET LE MODE EST PASSE 'EXPLICITEMENT'
!
  IF ((LLASGN.OR.LLREL).AND.FA%NFIOUV.NE.0) THEN
    IREP=-54
    GOTO 1001
  ENDIF
!
  CALL LFIINI_FORT               &
&                (FA%LFI, KOPTIO)
!
ENDIF
!
FA%LFAMUL=LLNMUL
IREP=0
!
IF (LLASGN) THEN
  CALL LFIVER_FORT                          &
&                 (FA%LFI, FA%VRGLAS,'ASGN')
ELSEIF (LLREL) THEN
  CALL LFIVER_FORT                         &
&                 (FA%LFI, FA%VRGLAS,'REL')
ENDIF
!
1001 CONTINUE
!
!        MESSAGERIE EVENTUELLE, AVEC ABORT SI NECESSAIRE .
!
LLFATA=IREP.NE.0.AND.FA%NRFAGA.NE.2
!
IF (LLFATA.OR.FA%LFAMOP) THEN
  INIMES=2
ELSEIF (IREP.NE.0) THEN
  INIMES=0
ELSEIF (FA%NIMSGA.EQ.2) THEN
  INIMES=2
ELSE
  IF (LHOOK) CALL DR_HOOK('FARINE_MT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='FARINE'
INUMER=JPNIIL
!
IF (MAX (INIMES,FA%NIMSGA).EQ.2) THEN
  WRITE (UNIT=CLMESS,                                  &
&         FMT='(''KOPTIO='',I5,'', CODE INTERNE='',I4)' &
&         ) KOPTIO,IREP
  IF (INIMES.NE.2) CALL FAIPAR_FORT                          &
&                                  (FA, INUMER,FA%NIMSGA,IREP, &
&                                .FALSE.,CLMESS,               &
&                                CLNSPR,CLACTI,.FALSE.)
ENDIF
!
CALL FAIPAR_FORT                                     &
&               (FA, INUMER,INIMES,IREP,LLFATA,CLMESS, &
&             CLNSPR,CLACTI,.FALSE.)
!
IF (LHOOK) CALL DR_HOOK('FARINE_MT',1,ZHOOK_HANDLE)
END SUBROUTINE FARINE_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FARINE64           &
&           (KOPTIO)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KOPTIO                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FARINE_FORT           &
&           (FA, KOPTIO)

END SUBROUTINE FARINE64

SUBROUTINE FARINE             &
&           (KOPTIO)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KOPTIO                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FARINE_MT             &
&           (FA, KOPTIO)

END SUBROUTINE FARINE

SUBROUTINE FARINE_MT             &
&           (FA, KOPTIO)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KOPTIO                                 ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IOPTIO                                 ! IN   
! Convert arguments

IOPTIO     = INT (    KOPTIO, JPLIKB)

CALL FARINE_FORT           &
&           (FA, IOPTIO)


END SUBROUTINE FARINE_MT

!INTF KOPTIO        IN    
