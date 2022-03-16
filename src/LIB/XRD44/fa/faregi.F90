! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FAREGI_FORT                         &
&                     (FA,  CDCLEF, KVAL, KOPT)
USE FA_MOD, ONLY : FA_COM, JPNIIL
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!      Ce sous-programme controle (lecture/ecriture) les options
!      de compression de GRIBEX implicites.
!      (REGlage des options Implicites de codage de gribex)
!**
!    Arguments : CDCLEF (Entree) ==> Mot clef precisant l'action a faire;
!                KVAL   (Sortie  ==> Valeur lue ou ecrite;
!                      ou Entree)
!                KOPT   (Entree) ==> Flag: 0->lecture 1->ecriture;
!*
!     Signification des divers elements du tableau FA%NCODGRI
!
!     NCODGRI(1) = type de codage (0->option HOPER='C'; 1->option HOPER='K')
!     NCODGRI(2) = KSEC4(6), indicateur de la presence de flags
!                            additionnels (0->non; 16->oui)
!     NCODGRI(3) = KSEC4(7)
!     NCODGRI(4) = KSEC4(9), indicateur de la presence de bitmaps
!                          secondaires (0->non; 32->oui)
!     NCODGRI(5) = KSEC4(10), indicateur pour le nb de bits des
!                     groupes de pts de grille (0->const.; 16->different)
!     NCODGRI(6) = KSEC4(11), nb de bits pour les groupes de pts de
!                   grille, quand il est constant.
!                     Si negatif, le logiciel calcule un nb optimal a partir
!                     de -KSEC4(11).
!     NCODGRI(7) = KSEC4(12), indicateur pour les extensions generales de la
!                     compression (0->non; 8->oui)
!     NCODGRI(8) = KSEC4(13), indicateur pour le rearrangement boustrophedo
!                     nique (0->non; 4->oui)
!     NCODGRI(9) = KSEC4(14) (valeurs possibles: -1, 0 et 2)
!     NCODGRI(10) = KSEC4(15) (valeurs possibles: -1, 0 et 1), sert avec
!                      KSEC4(14) a definir la technique de la difference
!                      spatiale.Si l'un des 2 est negatif, l'ordre de
!                      differentiation est estime dynamiquement, sinon
!                      l'ordre vaut KSEC4(14)+KSEC4(15)
!     NCODGRI(11) = 1->Calcul automatique de KSEC1 (23), 0->On laisse faire
!     NCODGRI(12) = Ecriture des champs GRIB1 dans un fichier externe
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KVAL, KOPT
!
CHARACTER(LEN=*) CDCLEF
!
INTEGER (KIND=JPLIKB) INUMER, INIMES, IREP, INBITSMAX
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA

!
!
!
!**
!     0.  -  INITIALISATIONS ET ALLOCATIONS PREALABLES
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FAREGI_MT',0,ZHOOK_HANDLE)
IREP=0
IF (FA%FAREGI_LLPREA) THEN
!
!          A la premiere utilisation, appel au sous-programme "FARINE".
!
  CALL FARINE_FORT             &
&                (FA, 2_JPLIKB )
  FA%FAREGI_LLPREA=.FALSE.
ENDIF
INBITSMAX=MAX(FA%NBIPDG,FA%NBICSP)
!
!**
!     1.  -  SPECIFICATION D'UN NOUVEAU CODAGE
!-----------------------------------------------------------------------
!
IF (KOPT==1) THEN
!
!   Pas de compression (sous-tronc et puissance laplacien
!   ajoutees systematiquement + tard pour les coeff spectraux)
!
  IF (CDCLEF=='BASIC'.OR.CDCLEF=='basic') THEN
    FA%NCODGRI(1)=0
    FA%NCODGRI(2)=0
    FA%NCODGRI(3)=0
    FA%NCODGRI(4)=0
    FA%NCODGRI(5)=0
    FA%NCODGRI(6)=0
  ELSEIF (CDCLEF=='IDCEN'.OR.CDCLEF=='idcen') THEN
    FA%NIDCEN = KVAL
!
!   Comme le "BASIC" avec une compression
!   ligne a ligne pour les points de grille
!
  ELSEIF (CDCLEF=='PACK1'.OR.CDCLEF=='pack1') THEN
    FA%NCODGRI(1)=0
    FA%NCODGRI(2)=16
    FA%NCODGRI(3)=0
    FA%NCODGRI(4)=0
    FA%NCODGRI(5)=16
    FA%NCODGRI(6)=0
!
!   Comme le "BASIC" avec une compression
!   pour les points de grille ou le nb de bits est le meme
!   dans chaque groupe de points de grille
!
  ELSEIF (CDCLEF=='PACK2'.OR.CDCLEF=='pack2') THEN
    FA%NCODGRI(1)=0
    FA%NCODGRI(2)=16
    FA%NCODGRI(3)=0
    FA%NCODGRI(4)=32
    FA%NCODGRI(5)=0
! Un nb de bits optimal sera recherche par le logiciel
    FA%NCODGRI(6)=-99
!
!   Comme le "BASIC" avec une compression
!   "OMM general" pour les points de grille
!
  ELSEIF (CDCLEF=='PACK3'.OR.CDCLEF=='pack3') THEN
    FA%NCODGRI(1)=0
    FA%NCODGRI(2)=16
    FA%NCODGRI(3)=0
    FA%NCODGRI(4)=32
    FA%NCODGRI(5)=16
    FA%NCODGRI(6)=0
!
!   Compression "aggressive": le logiciel va
!   tenter la compression ligne a ligne et comparer la
!   longueur de message obtenue avec celle en l'absence de
!   compression et au final retenir la meilleure solution.
!
  ELSEIF (CDCLEF=='APAC1'.OR.CDCLEF=='apac1') THEN
    FA%NCODGRI(1)=1
    FA%NCODGRI(2)=16
    FA%NCODGRI(3)=0
    FA%NCODGRI(4)=0
    FA%NCODGRI(5)=16
    FA%NCODGRI(6)=0
!
!   Compression "aggressive": le logiciel va
!   suivre la demarche "APAC1" en testant en plus le nb de bits
!   constant par groupe de pts de grille et retenir la meilleure
!   compression
!
  ELSEIF (CDCLEF=='APAC2'.OR.CDCLEF=='apac2') THEN
    FA%NCODGRI(1)=1
    FA%NCODGRI(2)=16
    FA%NCODGRI(3)=0
    FA%NCODGRI(4)=0
    FA%NCODGRI(5)=0
! Un nb de bits optimal sera recherche par le logiciel
    FA%NCODGRI(6)=-99
!
!   Compression "aggressive": le logiciel va
!   suivre la demarche "APAC1" en testant en plus la compression
!   "general OMM" et retenir la meilleure compression.
!
  ELSEIF (CDCLEF=='APAC3'.OR.CDCLEF=='apac3') THEN
    FA%NCODGRI(1)=1
    FA%NCODGRI(2)=16
    FA%NCODGRI(3)=0
    FA%NCODGRI(4)=32
    FA%NCODGRI(5)=16
    FA%NCODGRI(6)=0
!
!   Compression "aggressive": le logiciel va
!   suivre la demarche "APAC3" en testant en plus le nb de bits
!   constant par groupe de pts de grille et retenir la meilleure
!   compression.
!
  ELSEIF (CDCLEF=='APAC4'.OR.CDCLEF=='apac4') THEN
    FA%NCODGRI(1)=1
    FA%NCODGRI(2)=16
    FA%NCODGRI(3)=0
    FA%NCODGRI(4)=32
    FA%NCODGRI(5)=0
! Un nb de bits optimal sera recherche par le logiciel
    FA%NCODGRI(6)=-99
!
!   Specification du nb de bits a utiliser dans le cadre
!   de la compression avec nb de bits constant
!   par groupe de pts de grille
!   
  ELSEIF (CDCLEF=='WIDPA'.OR.CDCLEF=='widpa') THEN
    IF (KVAL.LT.1-INBITSMAX.OR.KVAL.GT.INBITSMAX-1) THEN
      IREP=-97
      GOTO 1001
    ENDIF
    FA%NCODGRI(6)=KVAL
!
!   Demande supplementaire de l'extension generale de la compression
!   (si KVAL=1, sinon c'est le retrait de cette option)
!
  ELSEIF (CDCLEF=='GEXTE'.OR.CDCLEF=='gexte') THEN
    IF (KVAL.EQ.1) THEN
      FA%IOPTGRSX2O=1
      FA%NCODGRI(7)=8
    ELSE
      FA%NCODGRI(7)=0
    ENDIF
!
!   Demande supplementaire du rearrangement boustrophedonique dans la
!   compression (si KVAL=1, sinon c'est le retrait de cette option)
!
  ELSEIF (CDCLEF=='BOUST'.OR.CDCLEF=='boust') THEN
    IF (KVAL.EQ.1) THEN
      FA%IOPTGRSX2O=1
      FA%NCODGRI(8)=4
    ELSE
      FA%NCODGRI(8)=0
    ENDIF
!
!   Demande supplementaire de la difference spatiale dans la compression.
!   KVAL donne l'ordre de differentiation:
!   -1-> calcul dynamique par GRIBEX; 1 a 3->ordre; 0->desactiv; autre->err
!
  ELSEIF (CDCLEF=='DIFFE'.OR.CDCLEF=='diffe') THEN
    IF (KVAL.EQ.-1) THEN
      FA%IOPTGRSX2O=1
      FA%IOPTGRSN2O=1
      FA%NCODGRI( 9)=0
      FA%NCODGRI(10)=-1
    ELSEIF (KVAL.EQ.1) THEN
      FA%IOPTGRSX2O=1
      FA%IOPTGRSN2O=1
      FA%NCODGRI( 9)=0
      FA%NCODGRI(10)=1
    ELSEIF (KVAL.EQ.2) THEN
      FA%IOPTGRSX2O=1
      FA%IOPTGRSN2O=1
      FA%NCODGRI( 9)=2
      FA%NCODGRI(10)=0
    ELSEIF (KVAL.EQ.3) THEN
      FA%IOPTGRSX2O=1
      FA%IOPTGRSN2O=1
      FA%NCODGRI( 9)=2
      FA%NCODGRI(10)=1
    ELSEIF (KVAL.EQ.0) THEN
      FA%IOPTGRSX2O=0
      FA%NCODGRI( 9)=0
      FA%NCODGRI(10)=0
    ELSE
      IREP=-125
      GOTO 1001
    ENDIF
!
!   Facteur decimal; calcul automatique
!
  ELSEIF (CDCLEF=='FACDEC'.OR.CDCLEF=='facdec') THEN
    FA%NCODGRI(11)=KVAL
!
!  Ecriture dans un fichier externe
!
  ELSEIF (CDCLEF=='EXTERN'.OR.CDCLEF=='extern') THEN
    FA%NCODGRI(12)=KVAL
  ENDIF
!**
!     2.  -  DEMANDE D'INFORMATION
!-----------------------------------------------------------------------
!
ELSEIF (KOPT==0) THEN
!
!   Obtention des mots-clef disponibles
!
  IF (CDCLEF=='CLEFS'.OR.CDCLEF=='clefs') THEN
    KVAL=0
    WRITE (UNIT=FA%NULOUT,FMT=*)
    WRITE (UNIT=FA%NULOUT,FMT=*) 'Mots clef disponibles pour FAREGI:'
    WRITE (UNIT=FA%NULOUT,FMT=*)
    WRITE (UNIT=FA%NULOUT,FMT=*) 'BASIC: pas de compression'
    WRITE (UNIT=FA%NULOUT,FMT=*) 'PACK1: BASIC avec une compression'
    WRITE (UNIT=FA%NULOUT,FMT=*) '       ligne a ligne pour les pts de grille'
    WRITE (UNIT=FA%NULOUT,FMT=*) 'PACK2: BASIC avec une compression'
    WRITE (UNIT=FA%NULOUT,FMT=*) '       ou le nb de bits est cst pour les groupes'
    WRITE (UNIT=FA%NULOUT,FMT=*) 'PACK3: BASIC avec une compression'
    WRITE (UNIT=FA%NULOUT,FMT=*) '       OMM general pour les points de grille'
    WRITE (UNIT=FA%NULOUT,FMT=*) 'APAC1: compression agressive'
    WRITE (UNIT=FA%NULOUT,FMT=*) '       BASIC et PACK1 sont testes'
    WRITE (UNIT=FA%NULOUT,FMT=*) 'APAC2: compression agressive'
    WRITE (UNIT=FA%NULOUT,FMT=*) '       BASIC, PACK1 et PACK2 sont testes'
    WRITE (UNIT=FA%NULOUT,FMT=*) 'APAC3: compression agressive'
    WRITE (UNIT=FA%NULOUT,FMT=*) '       BASIC, PACK1 et PACK3 sont testes'
    WRITE (UNIT=FA%NULOUT,FMT=*) 'APAC4: compression agressive'
    WRITE (UNIT=FA%NULOUT,FMT=*) '       BASIC, PACK1, PACK2 et PACK3 testes'
    WRITE (UNIT=FA%NULOUT,FMT=*) 'WIDPA: lecture/ecriture du nb de bits'
    WRITE (UNIT=FA%NULOUT,FMT=*) '       a utiliser pour les groupes de points'
    WRITE (UNIT=FA%NULOUT,FMT=*) '       de grille dans le cas PACK2'
    WRITE (UNIT=FA%NULOUT,FMT=*) 'GEXTE: les extensions generales de la compression'
    WRITE (UNIT=FA%NULOUT,FMT=*) '       sont activees (KVAL=1) ou desactiv. (KVAL=0)'
    WRITE (UNIT=FA%NULOUT,FMT=*) 'BOUST: le rearrangement boustrophedonique est'
    WRITE (UNIT=FA%NULOUT,FMT=*) '       active (KVAL=1) ou desactive (KVAL=0)'
    WRITE (UNIT=FA%NULOUT,FMT=*) 'DIFFE: la differenciation spatiale est'
    WRITE (UNIT=FA%NULOUT,FMT=*) '       activee (KVAL=ordre de differ. (1 a 3)'
    WRITE (UNIT=FA%NULOUT,FMT=*) '       ou -1 (calcul dyn)) ou desactivee (0)'
    WRITE (UNIT=FA%NULOUT,FMT=*) 'FACDEC: calcul automatique du facteur decimal'
    WRITE (UNIT=FA%NULOUT,FMT=*) 'EXTERN: ecriture dans un fichier externe'
    WRITE (UNIT=FA%NULOUT,FMT=*)
  ELSEIF (CDCLEF=='IDCEN'.OR.CDCLEF=='idcen') THEN
    KVAL = FA%NIDCEN 
!
!   Lecture du nb de bits a utiliser dans le cadre
!   de la compression avec nb de bits constant
!   par groupe de pts de grille
!   
  ELSEIF (CDCLEF=='WIDPA'.OR.CDCLEF=='widpa') THEN
    KVAL=FA%NCODGRI(6)
!
!   Lecture de la presence ou non de la compression
!   "general extended"
!
  ELSEIF (CDCLEF=='GEXTE'.OR.CDCLEF=='gexte') THEN
    KVAL = FA%NCODGRI(7) / 8
!
!   Lecture de la presence ou non du rearrangement
!   boustrophedonique.
!
  ELSEIF (CDCLEF=='BOUST'.OR.CDCLEF=='boust') THEN
    KVAL = FA%NCODGRI(8) / 4
!
!   Lecture de la presence ou non de la differentiation spatiale
!
  ELSEIF (CDCLEF=='DIFFE'.OR.CDCLEF=='diffe') THEN
    KVAL=FA%NCODGRI( 9)+FA%NCODGRI(10)
!
!   Facteur decimal; calcul automatique
!
  ELSEIF (CDCLEF=='FACDEC'.OR.CDCLEF=='facdec') THEN
    KVAL=FA%NCODGRI(11)
!
!   Ecriture dans un fichier externe
!
  ELSEIF (CDCLEF=='EXTERN'.OR.CDCLEF=='extern') THEN
    KVAL=FA%NCODGRI(12)
  ELSE
    IREP=-125
    GOTO 1001
  ENDIF
!**
!     3.  -  OPTION INCONNUE
!-----------------------------------------------------------------------
!
ELSE
  IREP=-125
  GOTO 1001
ENDIF
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE, AVEC "ABORT" EVENTUEL,
!            VIA LE SOUS-PROGRAMME "FAIPAR" .
!-----------------------------------------------------------------------
!
1001 CONTINUE
LLFATA=LLMOER (IREP,0_JPLIKB )
!
IF (LLFATA) THEN
  INIMES=2
ELSE
  INIMES=FA%NIMSGA
ENDIF
!
IF (INIMES.EQ.0)  THEN 
  IF (LHOOK) CALL DR_HOOK('FAREGI_MT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='FAREGI'
INUMER=JPNIIL
!
WRITE (UNIT=CLMESS,FMT='(''IREP='',I2,            &
&        '', CDCLEF='''''',A,'''''', KVAL='',I12, &
&        '', KOPT='',I4)')                        &
&        IREP,CDCLEF,KVAL,KOPT
INUMER=JPNIIL
CALL FAIPAR_FORT                                       &
&               (FA, INUMER,INIMES,IREP,LLFATA,CLMESS, &
&                CLNSPR,CLNSPR,.FALSE.)
!
IF (LHOOK) CALL DR_HOOK('FAREGI_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"

END SUBROUTINE FAREGI_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FAREGI64            &
&           (CDCLEF, KVAL, KOPT)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                  FA_COM_DEFAULT_INIT,  &
&                  NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
CHARACTER (LEN=*)      CDCLEF                                 ! IN   
INTEGER (KIND=JPLIKB)  KVAL                                   ! INOUT
INTEGER (KIND=JPLIKB)  KOPT                                   ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAREGI_FORT                   &
&           (FA, CDCLEF, KVAL, KOPT)

END SUBROUTINE FAREGI64

SUBROUTINE FAREGI              &
&           (CDCLEF, KVAL, KOPT)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                  FA_COM_DEFAULT_INIT,  &
&                  NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
CHARACTER (LEN=*)      CDCLEF                                 ! IN   
INTEGER (KIND=JPLIKM)  KVAL                                   ! INOUT
INTEGER (KIND=JPLIKM)  KOPT                                   ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAREGI_MT                     &
&           (FA, CDCLEF, KVAL, KOPT)

END SUBROUTINE FAREGI

SUBROUTINE FAREGI_MT               &
&           (FA, CDCLEF, KVAL, KOPT)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
CHARACTER (LEN=*)      CDCLEF                                 ! IN   
INTEGER (KIND=JPLIKM)  KVAL                                   ! INOUT
INTEGER (KIND=JPLIKM)  KOPT                                   ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IVAL                                   ! INOUT
INTEGER (KIND=JPLIKB)  IOPT                                   ! IN   
! Convert arguments

IF (KOPT==1) THEN
  IVAL       = INT (      KVAL, JPLIKB)
ENDIF
IOPT       = INT (      KOPT, JPLIKB)

CALL FAREGI_FORT                   &
&           (FA, CDCLEF, IVAL, IOPT)

IF (KOPT==0) THEN
  KVAL       = INT (      IVAL, JPLIKM)
ENDIF

END SUBROUTINE FAREGI_MT

!INTF CDCLEF        IN                                                                 
!INTF KVAL          INOUT IN_IF=KOPT==1                  OUT_IF=KOPT==0                
!INTF KOPT          IN                                                                 
