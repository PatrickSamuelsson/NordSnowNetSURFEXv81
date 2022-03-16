! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FAREGU_FORT                                 &
&                     (FA,  KNUMER, CDCLEF, KVAL, KOPT)
USE FA_MOD, ONLY : FA_COM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!      Ce sous-programme controle (lecture/ecriture) les options
!      de compression de GRIBEX, pour chacune des unites logiques et
!      certains descripteurs GRIB communs a l'unite logique.
!      (REGLAGE des options de codage de gribex pour une Unite)
!**
!    Arguments : KNUMER (Entree) ==> Numero de l'unite logique;
!                CDCLEF (Entree) ==> Mot clef precisant l'action a faire;
!                KVAL   (Sortie  ==> Valeur lue ou a ecrire;
!                      ou Entree)
!                KOPT   (Entree) ==> Flag: 0->lecture 1->ecriture;
!*
!     Signification des divers elements du tableau NCOGRIF
!
!     NCOGRIF(1) = type de codage (0->option HOPER='C'
!                               1->option HOPER='K')
!     NCOGRIF(2) = KSEC4(6), indicateur de la presence de flags
!               additionnels (0->non; 16->oui)
!     NCOGRIF(3) = KSEC4(7)
!     NCOGRIF(4) = KSEC4(9), indicateur de la presence de bitmaps
!               secondaires (0->non; 32->oui)
!     NCOGRIF(5) = KSEC4(10), indicateur pour le nb de bits des
!               groupes de pts de grille (0->const.; 16->different)
!     NCOGRIF(6) = KSEC4(11), nb de bits pour les groupes de pts de grille
!               quand il est constant.
!               Si negatif, le logiciel calcule un nb optimal a partir
!               de -KSEC4(11).
!     NCOGRIF(7) = KSEC4(12), indicateur pour les extensions generales de
!               la compression (0->non; 8->oui)
!     NCOGRIF(8) = KSEC4(13), indicateur pour le rearrangement boustrophedo
!               nique (0->non; 4->oui)
!     NCOGRIF(9) = KSEC4(14) (valeurs possibles: -1, 0 et 2)
!     NCOGRIF(10) = KSEC4(15) (valeurs possibles: -1, 0 et 1), sert avec
!                KSEC4(14) a definir la technique de la difference
!                spatiale. Si l'un des 2 est negatif, l'ordre de
!                differentiation est estime dynamiquement, sinon
!                l'ordre = KSEC4(14)+KSEC4(15)
!     NCOGRIF(11) = 1->Calcul automatique de KSEC1 (23), 0->On laisse faire
!     NCODGRI(12) = Ecriture des champs GRIB1 dans un fichier externe
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KNUMER, KVAL, KOPT
!
CHARACTER(LEN=*) CDCLEF
!
INTEGER (KIND=JPLIKB) IRANG, INIMES, IREP, INBITSMAX
!
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA

!
!**
!     0.  -  INITIALISATIONS ET ALLOCATIONS PREALABLES
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FAREGU_MT',0,ZHOOK_HANDLE)
IREP=0
!
CALL FANUMU_FORT                 &
&               (FA, KNUMER,IRANG)
!
IF (IRANG.EQ.0) THEN
  IREP=-51
  GOTO 1001
ENDIF
!
! Appel prealable a FAISC1 pour initialiser FA%NSEC1(2:21,IRANG).
! On le fait ici plutot que dans FAINIG pour ne pas ecraser
! les eventuelles modifs apportees par IDCEN et/ou IDMOD
!
IF (FA%FICHIER(IRANG)%LISEC1) THEN
  CALL FAISC1_FORT              &
&                (FA, IREP,IRANG)
  IF (IREP.NE.0) THEN
    WRITE (UNIT=FA%NULOUT,FMT=*) &
&           'FAREGU: ERROR ',IREP,' dans appel a FAISC1 !!'
    GOTO 1001
  ENDIF
  FA%FICHIER(IRANG)%LISEC1=.FALSE.
ENDIF
!
INBITSMAX=MAX(FA%FICHIER(IRANG)%NBFPDG,FA%FICHIER(IRANG)%NBFCSP)
!**
!     1.  -  SPECIFICATION D'UN NOUVEAU CODAGE
!-----------------------------------------------------------------------
!
IF (KOPT==1) THEN
!
!   Pas de compression (sous-tronc et puissance laplacien ajoutees
!   systematiquement + tard pour les coeff spectraux)
!
  IF (CDCLEF=='BASIC'.OR.CDCLEF=='basic') THEN
    FA%FICHIER(IRANG)%NCOGRIF(1)=0
    FA%FICHIER(IRANG)%NCOGRIF(2)=0
    FA%FICHIER(IRANG)%NCOGRIF(3)=0
    FA%FICHIER(IRANG)%NCOGRIF(4)=0
    FA%FICHIER(IRANG)%NCOGRIF(5)=0
    FA%FICHIER(IRANG)%NCOGRIF(6)=0
!
!   Comme le "BASIC" avec une compression
!   ligne a ligne pour les points de grille
!
  ELSEIF (CDCLEF=='PACK1'.OR.CDCLEF=='pack1') THEN
    FA%FICHIER(IRANG)%NCOGRIF(1)=0
    FA%FICHIER(IRANG)%NCOGRIF(2)=16
    FA%FICHIER(IRANG)%NCOGRIF(3)=0
    FA%FICHIER(IRANG)%NCOGRIF(4)=0
    FA%FICHIER(IRANG)%NCOGRIF(5)=16
    FA%FICHIER(IRANG)%NCOGRIF(6)=0
!
!   Comme le "BASIC" avec une compression
!   pour les points de grille ou le nb de bits est le meme
!   dans chaque groupe de points de grille
!
  ELSEIF (CDCLEF=='PACK2'.OR.CDCLEF=='pack2') THEN
    FA%FICHIER(IRANG)%NCOGRIF(1)=0
    FA%FICHIER(IRANG)%NCOGRIF(2)=16
    FA%FICHIER(IRANG)%NCOGRIF(3)=0
    FA%FICHIER(IRANG)%NCOGRIF(4)=32
    FA%FICHIER(IRANG)%NCOGRIF(5)=0
! Un nb de bits optimal sera recherche par le logiciel
    FA%FICHIER(IRANG)%NCOGRIF(6)=-99
!
!   Comme le "BASIC" avec une compression general OMM
!   pour les points de grille
!
  ELSEIF (CDCLEF=='PACK3'.OR.CDCLEF=='pack3') THEN
    FA%FICHIER(IRANG)%NCOGRIF(1)=0
    FA%FICHIER(IRANG)%NCOGRIF(2)=16
    FA%FICHIER(IRANG)%NCOGRIF(3)=0
    FA%FICHIER(IRANG)%NCOGRIF(4)=32
    FA%FICHIER(IRANG)%NCOGRIF(5)=16
    FA%FICHIER(IRANG)%NCOGRIF(6)=0
!
!   Compression "aggressive": le logiciel va
!   tenter la compression ligne a ligne puis l'absence
!   de compression et retenir la meilleure methode
!
  ELSEIF (CDCLEF=='APAC1'.OR.CDCLEF=='apac1') THEN
    FA%FICHIER(IRANG)%NCOGRIF(1)=1
    FA%FICHIER(IRANG)%NCOGRIF(2)=16
    FA%FICHIER(IRANG)%NCOGRIF(3)=0
    FA%FICHIER(IRANG)%NCOGRIF(4)=0
    FA%FICHIER(IRANG)%NCOGRIF(5)=16
    FA%FICHIER(IRANG)%NCOGRIF(6)=0
!
!   Compression "aggressive": le logiciel va
!   tenter la compression type "APAC1" puis celle avec le nb de bits
!   constant par groupe de pts de grille et retenir la meilleure
!
  ELSEIF (CDCLEF=='APAC2'.OR.CDCLEF=='apac2') THEN
    FA%FICHIER(IRANG)%NCOGRIF(1)=1
    FA%FICHIER(IRANG)%NCOGRIF(2)=16
    FA%FICHIER(IRANG)%NCOGRIF(3)=0
    FA%FICHIER(IRANG)%NCOGRIF(4)=0
    FA%FICHIER(IRANG)%NCOGRIF(5)=0
! Un nb de bits optimal sera recherche par le logiciel
    FA%FICHIER(IRANG)%NCOGRIF(6)=-99
!
!   Compression "aggressive": le logiciel va tenter
!   la compression type "APAC1" puis la compression generale
!   OMM et retenir la meilleure
!
  ELSEIF (CDCLEF=='APAC3'.OR.CDCLEF=='apac3') THEN
    FA%FICHIER(IRANG)%NCOGRIF(1)=1
    FA%FICHIER(IRANG)%NCOGRIF(2)=16
    FA%FICHIER(IRANG)%NCOGRIF(3)=0
    FA%FICHIER(IRANG)%NCOGRIF(4)=32
    FA%FICHIER(IRANG)%NCOGRIF(5)=16
    FA%FICHIER(IRANG)%NCOGRIF(6)=0
!
!   Compression "aggressive": le logiciel va
!   tenter la compression type "APAC3" puis celle avec le nb de bits
!   constant par groupe de pts de grille et retenir la meilleure
!
  ELSEIF (CDCLEF=='APAC4'.OR.CDCLEF=='apac4') THEN
    FA%FICHIER(IRANG)%NCOGRIF(1)=1
    FA%FICHIER(IRANG)%NCOGRIF(2)=16
    FA%FICHIER(IRANG)%NCOGRIF(3)=0
    FA%FICHIER(IRANG)%NCOGRIF(4)=32
    FA%FICHIER(IRANG)%NCOGRIF(5)=0
! Un nb de bits optimal sera recherche par le logiciel
    FA%FICHIER(IRANG)%NCOGRIF(6)=-99
!
!   Specification du nb de bits a utiliser dans le cadre
!   de la compression avec nb de bits constant
!   par groupe de pts de grille
!   
  ELSEIF (CDCLEF=='WIDPA'.OR.CDCLEF=='widpa') THEN
    IF (KVAL.LT.1-INBITSMAX.OR.KVAL.GT.INBITSMAX-1) THEN
      IREP=-97
      WRITE (UNIT=FA%NULOUT,FMT='(A)')'Dans FAREGU, action WIDPA:'
      WRITE (UNIT=FA%NULOUT,FMT='(A57,I8)')                       &
&   '!!  ERREUR  !! Valeur incorrecte, non prise en compte: ',KVAL
      GOTO 1001
    ENDIF
    FA%FICHIER(IRANG)%NCOGRIF(6)=KVAL
!
!   Demande supplementaire de la compression avec extension
!   generale a la norme OMM (si KVAL=1, sinon c'est le retrait de l'option)
!
  ELSEIF (CDCLEF=='GEXTE'.OR.CDCLEF=='gexte') THEN
    IF (KVAL.EQ.1) THEN
      FA%FICHIER(IRANG)%IOPTGRSX2O=1
      FA%FICHIER(IRANG)%NCOGRIF(7)=8
    ELSE
      FA%FICHIER(IRANG)%NCOGRIF(7)=0
    ENDIF
!
!   Demande supplementaire du rearrangement boustrophedonique dans la compression
!   (si KVAL=1, sinon c'est le retrait de cette option)
!
  ELSEIF (CDCLEF=='BOUST'.OR.CDCLEF=='boust') THEN
    IF (KVAL.EQ.1) THEN
      FA%FICHIER(IRANG)%IOPTGRSX2O=1
      FA%FICHIER(IRANG)%NCOGRIF(8)=4
    ELSE
      FA%FICHIER(IRANG)%NCOGRIF(8)=0
    ENDIF
!
!   Demande supplementaire de la difference spatiale dans la
!   compression. KVAL donne l'ordre de differentiation
!   (-1-> calcul dynamique par GRIBEX; 1 a 3->ordre; 0->desactiv; autre->err)
!
  ELSEIF (CDCLEF=='DIFFE'.OR.CDCLEF=='diffe') THEN
    IF (KVAL.EQ.-1) THEN
      FA%FICHIER(IRANG)%IOPTGRSX2O=1
      FA%FICHIER(IRANG)%IOPTGRSN2O=1
      FA%FICHIER(IRANG)%NCOGRIF( 9)=0
      FA%FICHIER(IRANG)%NCOGRIF(10)=-1
    ELSEIF (KVAL.EQ.1) THEN
      FA%FICHIER(IRANG)%IOPTGRSX2O=1
      FA%FICHIER(IRANG)%IOPTGRSN2O=1
      FA%FICHIER(IRANG)%NCOGRIF( 9)=0
      FA%FICHIER(IRANG)%NCOGRIF(10)=1
    ELSEIF (KVAL.EQ.2) THEN
      FA%FICHIER(IRANG)%IOPTGRSX2O=1
      FA%FICHIER(IRANG)%IOPTGRSN2O=1
      FA%FICHIER(IRANG)%NCOGRIF( 9)=2
      FA%FICHIER(IRANG)%NCOGRIF(10)=0
    ELSEIF (KVAL.EQ.3) THEN
      FA%FICHIER(IRANG)%IOPTGRSX2O=1
      FA%FICHIER(IRANG)%IOPTGRSN2O=1
      FA%FICHIER(IRANG)%NCOGRIF( 9)=2
      FA%FICHIER(IRANG)%NCOGRIF(10)=1
    ELSEIF (KVAL.EQ.0) THEN
      FA%FICHIER(IRANG)%IOPTGRSX2O=0
      FA%FICHIER(IRANG)%NCOGRIF( 9)=0
      FA%FICHIER(IRANG)%NCOGRIF(10)=0
    ELSE
      IREP=-125
      WRITE (UNIT=FA%NULOUT,FMT='(A)')    &
&             'Dans FAREGU, action DIFFE:'
      WRITE (UNIT=FA%NULOUT,FMT='(A57,I8)')                       &
&   '!!  ERREUR  !! Valeur incorrecte, non prise en compte: ',KVAL
      GOTO 1001
    ENDIF
!
!   Specification de l'identificateur du centre meteo (defaut=85 pour
!   Toulouse; pour Reading, il vaut 98). Sera utilise pour initialiser
!   KSEC1(2), le 2ieme elt de la section 1 de GRIBEX
!
  ELSEIF (CDCLEF=='IDCEN'.OR.CDCLEF=='idcen') THEN
    IF (KVAL.LT.7.OR.KVAL.GT.99) THEN
      IREP=-125
      WRITE (UNIT=FA%NULOUT,FMT='(A)')    &
&             'Dans FAREGU, action IDCEN:'
      WRITE (UNIT=FA%NULOUT,FMT='(A57,I8)') &
&   '!!  ERREUR  !! Valeur incorrecte, non prise en compte: ',KVAL
      GOTO 1001
    ENDIF
    FA%FICHIER(IRANG)%NSEC1(2) = KVAL
    FA%FICHIER(IRANG)%NIDCEN   = KVAL

!
!   Specification de l'identificateur de modele.
!   FAISC1 initialise automatiquement a
!      177 pour ALADIN
!      211 pour les previsions ARPEGE
!      201 pour les analyses ARPEGE
!   Sera utilise pour initialiser KSEC1(3).
!
  ELSEIF (CDCLEF=='IDMOD'.OR.CDCLEF=='idmod') THEN
    IF (KVAL.LT.0.OR.KVAL.GT.255) THEN
      IREP=-125
      WRITE (UNIT=FA%NULOUT,FMT='(A)')    &
&             'Dans FAREGU, action IDMOD:'
      WRITE (UNIT=FA%NULOUT,FMT='(A57,I8)') &
&   '!!  ERREUR  !! Valeur incorrecte, non prise en compte: ',KVAL
      GOTO 1001
    ENDIF
    FA%FICHIER(IRANG)%NSEC1(3)=KVAL
  ELSEIF (CDCLEF (1:MIN (7, LEN (CDCLEF)))=='CMODEL=') THEN
    FA%FICHIER(IRANG)%CMODEL = CDCLEF (8:)
!
!   Facteur decimal; calcul automatique
!
  ELSEIF (CDCLEF=='FACDEC'.OR.CDCLEF=='facdec') THEN
    FA%FICHIER(IRANG)%NCOGRIF(11)=KVAL
!
!   Ecriture dans un fichier externe
!
  ELSEIF (CDCLEF=='EXTERN'.OR.CDCLEF=='extern') THEN
    FA%FICHIER(IRANG)%NCOGRIF(12)=KVAL
  ELSE
    IREP=-125
    WRITE (UNIT=FA%NULOUT,FMT='(A)') &
&     '!!  ERREUR  !! Dans FAREGU, action inconnue: '//CDCLEF
    GOTO 1001
  ENDIF
!**
!     2.  -  DEMANDE D'INFORMATION
!-----------------------------------------------------------------------
!
ELSEIF (KOPT==0) THEN
!
!   Obtention des mots-clef disponibles
!
  IF (CDCLEF=='CLEFS'.OR. CDCLEF=='clefs' .OR. &
&      CDCLEF=='HELP' .OR. CDCLEF=='help') THEN
    KVAL=0
    WRITE (UNIT=FA%NULOUT,FMT=*)
    WRITE (UNIT=FA%NULOUT,FMT=*) 'Mots clef disponibles pour FAREGU:'
    WRITE (UNIT=FA%NULOUT,FMT=*)
    WRITE (UNIT=FA%NULOUT,FMT=*) 'BASIC: pas de compression'
    WRITE (UNIT=FA%NULOUT,FMT=*) 'PACK1: BASIC avec une compression'
    WRITE (UNIT=FA%NULOUT,FMT=*) '       ligne a ligne pour les pts de grille'
    WRITE (UNIT=FA%NULOUT,FMT=*) 'PACK2: BASIC avec une compression avec'
    WRITE (UNIT=FA%NULOUT,FMT=*) '       nb de bits cst pour les groupes'
    WRITE (UNIT=FA%NULOUT,FMT=*) 'PACK3: BASIC avec une compression generale'
    WRITE (UNIT=FA%NULOUT,FMT=*) '       OMM pour les points de grille'
    WRITE (UNIT=FA%NULOUT,FMT=*) 'APAC1: compression agressive:'
    WRITE (UNIT=FA%NULOUT,FMT=*) '       BASIC et PACK1 sont testes'
    WRITE (UNIT=FA%NULOUT,FMT=*) 'APAC2: compression agressive:'
    WRITE (UNIT=FA%NULOUT,FMT=*) '       BASIC, PACK1 et PACK2 sont testes'
    WRITE (UNIT=FA%NULOUT,FMT=*) 'APAC3: compression agressive:'
    WRITE (UNIT=FA%NULOUT,FMT=*) '       BASIC, PACK1 et PACK3 sont testes'
    WRITE (UNIT=FA%NULOUT,FMT=*) 'APAC4: compression agressive:'
    WRITE (UNIT=FA%NULOUT,FMT=*) '       BASIC, PACK1, PACK2 et PACK3 testes'
    WRITE (UNIT=FA%NULOUT,FMT=*) 'WIDPA: lecture/ecriture du nb de bits'
    WRITE (UNIT=FA%NULOUT,FMT=*) '       a utiliser pour les groupes de points'
    WRITE (UNIT=FA%NULOUT,FMT=*) '       de grille dans le cas PACK2'
    WRITE (UNIT=FA%NULOUT,FMT=*) 'GEXTE: la compression avec extensions generales'
    WRITE (UNIT=FA%NULOUT,FMT=*) '       activees (KVAL=1) ou desactivees (KVAL=0)'
    WRITE (UNIT=FA%NULOUT,FMT=*) 'BOUST: le rearrangement boustrophedonique est'
    WRITE (UNIT=FA%NULOUT,FMT=*) '       active (KVAL=1) ou desactive (KVAL=0)'
    WRITE (UNIT=FA%NULOUT,FMT=*) 'DIFFE: la differenciation spatiale est'
    WRITE (UNIT=FA%NULOUT,FMT=*) '       activee (KVAL=ordre de differ. (1 a 3)'
    WRITE (UNIT=FA%NULOUT,FMT=*) '       ou -1 (calcul dyn)) ou desactivee (0)'
    WRITE (UNIT=FA%NULOUT,FMT=*) 'IDCEN: lect/ecriture de l''identificateur du'
    WRITE (UNIT=FA%NULOUT,FMT=*) '       centre meteo'
    WRITE (UNIT=FA%NULOUT,FMT=*) 'IDMOD: lect/ecriture de l''identificateur du'
    WRITE (UNIT=FA%NULOUT,FMT=*) '       modele'
    WRITE (UNIT=FA%NULOUT,FMT=*) 'CMODEL: lect/ecriture de l''identificateur du'
    WRITE (UNIT=FA%NULOUT,FMT=*) '        modele'
    WRITE (UNIT=FA%NULOUT,FMT=*) 'FACDEC: calcul automatique du facteur decimal'
    WRITE (UNIT=FA%NULOUT,FMT=*) 'EXTERN: ecriture dans un fichier externe'
    WRITE (UNIT=FA%NULOUT,FMT=*)
!
!   Lecture du nb de bits a utiliser dans le cadre
!   de la compression avec nb de bits
!   constant par groupe de pts de grille
!   
  ELSEIF (CDCLEF=='WIDPA'.OR.CDCLEF=='widpa') THEN
    KVAL=FA%FICHIER(IRANG)%NCOGRIF(6)
!
!   Lecture de la presence ou non de la compression
!   "general extended"
!
  ELSEIF (CDCLEF=='GEXTE'.OR.CDCLEF=='gexte') THEN
    KVAL = FA%FICHIER(IRANG)%NCOGRIF(7)/8
!
!   Lecture de la presence ou non du rearrangement
!   boustrophedonique.
!
  ELSEIF (CDCLEF=='BOUST'.OR.CDCLEF=='boust') THEN
    KVAL = FA%FICHIER(IRANG)%NCOGRIF(8)/4
!
!   Lecture de la presence ou non de la differentiation spatiale
!
  ELSEIF (CDCLEF=='DIFFE'.OR.CDCLEF=='diffe') THEN
    KVAL=FA%FICHIER(IRANG)%NCOGRIF( 9)+FA%FICHIER(IRANG)%NCOGRIF(10)
!
!   Lecture de l'identificateur du centre meteo (defaut=85 pour
!   Toulouse; pour Reading, il vaut 98). Sera utilise pour initialiser
!   KSEC1(2), le 2ieme elt de la section 1 de GRIBEX
!
  ELSEIF (CDCLEF=='IDCEN'.OR.CDCLEF=='idcen') THEN
    KVAL=FA%FICHIER(IRANG)%NIDCEN
!
!   Lecture de l'identificateur du modele
!
  ELSEIF (CDCLEF=='IDMOD'.OR.CDCLEF=='idmod') THEN
    KVAL=FA%FICHIER(IRANG)%NSEC1(3)
  ELSEIF (CDCLEF (1:MIN (7, LEN (CDCLEF)))=='CMODEL=') THEN
    CDCLEF (8:) = FA%FICHIER(IRANG)%CMODEL
!
!   Facteur decimal; calcul automatique
!
  ELSEIF (CDCLEF=='FACDEC'.OR.CDCLEF=='facdec') THEN
    KVAL=FA%FICHIER(IRANG)%NCOGRIF(11)
!
!   Ecriture dans un fichier externe
!
  ELSEIF (CDCLEF=='EXTERN'.OR.CDCLEF=='extern') THEN
    KVAL=FA%FICHIER(IRANG)%NCOGRIF(12)
  ELSE
    IREP=-125
    WRITE (UNIT=FA%NULOUT,FMT='(A)')                          &
&      '!!  ERREUR  !! Dans FAREGU, action inconnue: '//CDCLEF
    GOTO 1001
  ENDIF
!**
!     3.  -  OPTION INCONNUE
!-----------------------------------------------------------------------
!
ELSE
  IREP=-125
  WRITE (UNIT=FA%NULOUT,FMT='(A57,I8)')                        &
&    '!!  ERREUR  !! Dans FAREGU, option inconnue: KOPT= ',KOPT
  GOTO 1001
ENDIF
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE, AVEC "ABORT" EVENTUEL,
!            VIA LE SOUS-PROGRAMME "FAIPAR" .
!-----------------------------------------------------------------------
!
1001 CONTINUE
LLFATA=LLMOER (IREP,IRANG)
!
IF (LLFATA) THEN
  INIMES=2
ELSE
  INIMES=IXNVMS(IRANG)
ENDIF
!
IF (.NOT.LLFATA.AND.INIMES.NE.2)  THEN
  IF (LHOOK) CALL DR_HOOK('FAREGU_MT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='FAREGU'
!
WRITE (UNIT=CLMESS,FMT='(''IREP='',I4,'', KNUMER='',I3,  &
&         '', CDCLEF='''''',A,'''''', KVAL='',I12,       &
&        '', KOPT='',I4)')                               &
&              IREP,KNUMER,CDCLEF,KVAL,KOPT
CALL FAIPAR_FORT                                       &
&               (FA, KNUMER,INIMES,IREP,LLFATA,CLMESS, &
&                CLNSPR,CLNSPR,.FALSE.)
!
IF (LHOOK) CALL DR_HOOK('FAREGU_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"
#include "facom2.ixnvms.h"

END SUBROUTINE FAREGU_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FAREGU64                    &
&           (KNUMER, CDCLEF, KVAL, KOPT)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                  FA_COM_DEFAULT_INIT,  &
&                  NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDCLEF                                 ! IN   
INTEGER (KIND=JPLIKB)  KVAL                                   ! INOUT
INTEGER (KIND=JPLIKB)  KOPT                                   ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAREGU_FORT                           &
&           (FA, KNUMER, CDCLEF, KVAL, KOPT)

END SUBROUTINE FAREGU64

SUBROUTINE FAREGU                      &
&           (KNUMER, CDCLEF, KVAL, KOPT)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                  FA_COM_DEFAULT_INIT,  &
&                  NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDCLEF                                 ! IN   
INTEGER (KIND=JPLIKM)  KVAL                                   ! INOUT
INTEGER (KIND=JPLIKM)  KOPT                                   ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAREGU_MT                             &
&           (FA, KNUMER, CDCLEF, KVAL, KOPT)

END SUBROUTINE FAREGU

SUBROUTINE FAREGU_MT                       &
&           (FA, KNUMER, CDCLEF, KVAL, KOPT)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDCLEF                                 ! IN   
INTEGER (KIND=JPLIKM)  KVAL                                   ! INOUT
INTEGER (KIND=JPLIKM)  KOPT                                   ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  IVAL                                   ! INOUT
INTEGER (KIND=JPLIKB)  IOPT                                   ! IN   
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)
IF (KOPT==1) THEN
  IVAL       = INT (      KVAL, JPLIKB)
ENDIF
IOPT       = INT (      KOPT, JPLIKB)

CALL FAREGU_FORT                           &
&           (FA, INUMER, CDCLEF, IVAL, IOPT)

IF (KOPT==0) THEN
  KVAL       = INT (      IVAL, JPLIKM)
ENDIF

END SUBROUTINE FAREGU_MT

!INTF KNUMER        IN                                                                 
!INTF CDCLEF        IN                                                                 
!INTF KVAL          INOUT IN_IF=KOPT==1                  OUT_IF=KOPT==0                
!INTF KOPT          IN                                                                 

