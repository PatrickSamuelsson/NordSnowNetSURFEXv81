! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FAGOTE_FORT                                             &
&                     (FA,  KREP, KNUMER, KNGRIB, KNARG1, KNARG2,  &
&                      KNARG3, KNARG4, KNARG5)
USE FA_MOD, ONLY : FA_COM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Ce sous-programme permet d'ajuster, pour un fichier ARPEGE
!     ouvert, les options liees au codage GRIB des champs.
!     CES OPTIONS NE SONT UTILISEES QUE POUR (RE)ECRIRE DES CHAMPS
!     codes en GRIB.
!       ( Grib, Options Techniques Effectives )
!**
!     Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                 KNUMER (Entree) ==> Numero d'Unite Logique concernee;
!                 KNGRIB (Entree) ==> Niveau de codage GRIB (-1,0,1,2,3,4);
!
!               * Pour KNGRIB compris entre -1 et 3, les arguments 
!                 d'entree ont la signification suivante:
!                 KNARG1 (Entree) ==> Nombre de bits par valeur point-
!                                     de-grille; 
!                 KNARG2 (Entree) ==> Nombre de bits par partie reelle/
!                                     imaginaire de coeff. spectral;
!                 KNARG3 (Entree) ==> Sous-troncature non compactee;
!                 KNARG4 (Entree) ==> Puissance de laplacien;
!                 KNARG5 (Entree) ==> Degre de modulation de KNARG4.
!
!     Remarque:  KNARG3 egal a -1 est accepte; dans ce cas on indexera
!                (pour chaque champ spectral ecrit) la sous-troncature
!                effective sur la troncature et sur le nombre de bits
!                par valeur compactee.
!                 
!               * Pour KNGRIB==4, les arguments d'entree ont la 
!                 signification suivante:
!                 
!                 KNARG1 (Entree) ==> Taille de la couronne a conserver
!                 KNARG2 (Entree) ==> Nombre de bits pour la mantisse
!                 KNARG3 (Entree) ==> Inutilise
!                 KNARG4 (Entree) ==> Inutilise
!                 KNARG5 (Entree) ==> Inutilise
!
!     MODIF : 30/03/2007 JM AUDOIN  FA%LFAMOP Pour limiter Impression
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KNUMER, KNGRIB
INTEGER (KIND=JPLIKB) KNARG1, KNARG2, KNARG3, KNARG4
INTEGER (KIND=JPLIKB) KNARG5
!
INTEGER (KIND=JPLIKB) IMINIM, IREP, IRANGC
INTEGER (KIND=JPLIKB) ITRONC, INIMES, IRANG, ITYPTR
!
LOGICAL LLVERF, LLMLAM
!
CHARACTER(LEN=FA%JPXNOM) CLACTI 
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA

!**
!     1.  -  CONTROLES ET INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FAGOTE_MT',0,ZHOOK_HANDLE)
CLACTI=''
LLVERF=.FALSE.
CALL FANUMU_FORT                 &
&               (FA, KNUMER,IRANG)
!

IF (IRANG.EQ.0) THEN
  IREP=-51
  GOTO 1001
ENDIF

IF (((KNGRIB >= -1) .AND. (KNGRIB <= 3)) .OR. FALGRA (KNGRIB)) THEN

  IMINIM=MIN (2+KNGRIB,2+KNARG1,2+KNARG2,2+KNARG3,1+KNARG5)
  
  IF (IMINIM.LE.0) THEN
    IREP=-64
    GOTO 1001
  ELSEIF (KNARG1*KNARG2.EQ.0 .AND. KNGRIB.GT.0) THEN
    IREP=-124
    GOTO 1001
  ELSEIF ((MAX (KNARG1,KNARG2).GT.FA%NBIMAX) .AND. (.NOT. FALGRA (KNGRIB)) .AND. (KNGRIB /= 0)) THEN
    IREP=-97
    GOTO 1001
  ELSEIF (ABS (KNARG4).GT.2**15-1) THEN
    IREP=-98
    GOTO 1001
  ENDIF

ELSEIF (KNGRIB == 4) THEN

  IF ((KNARG1 < 0) .OR. (KNARG2 < 0)) THEN
    IREP=-64
    GOTO 1001
  ENDIF

ELSE
  IREP=-96
  GOTO 1001
ENDIF
!
!         Verrouillage eventuel du fichier.
!
IF (FA%LFAMUL) CALL LFIVER_FORT                               &
&                              (FA%LFI, FA%FICHIER(IRANG)%VRFICH,'ON')
LLVERF=FA%LFAMUL
!
IRANGC=FA%FICHIER(IRANG)%NUCADR
ITRONC=FA%CADRE(IRANGC)%MTRONC
ITYPTR=FA%CADRE(IRANGC)%NTYPTR
LLMLAM=FA%CADRE(IRANGC)%LIMLAM
!

IF (KNGRIB /= 4) THEN

  IF (KNARG3.GE.ITRONC) THEN
    IREP=-99
    GOTO 1001
  ELSEIF (ITYPTR.LT.0.AND.KNARG3.GE.(-ITYPTR)) THEN
    IREP=-99
    GOTO 1001
  ENDIF
!**
!     2.  -  STOCKAGE DES NOUVEAUX PARAMETRES.
!-----------------------------------------------------------------------
!
  IF (KNARG4.NE.FA%FICHIER(IRANG)%NPUFLA) THEN
    FA%FICHIER(IRANG)%NPUFLA=KNARG4
    FA%FICHIER(IRANG)%LIFLAP=.TRUE.
  ENDIF
  IF (KNARG3.NE.FA%FICHIER(IRANG)%NSTROF) THEN
    FA%FICHIER(IRANG)%NSTROF=KNARG3
    FA%FICHIER(IRANG)%LISC2F=.TRUE.
  ENDIF
!
  IF (FA%LFAMOP.AND.(FA%FICHIER(IRANG)%NFGRIB.EQ.3              &
  &  .OR.FA%FICHIER(IRANG)%NFGRIB.EQ.-1)                        &
  &  .AND.(KNGRIB.LT.3.AND.KNGRIB.GT.-1))           THEN
    WRITE (UNIT=FA%NULOUT,FMT=*)'-----------------'
    WRITE (UNIT=FA%NULOUT,FMT=*)                                    &
  &        'FAGOTE: WARNING!! Les champs spectraux NE devront',     &
  &        ' PAS etre ranges comme dans le modele (rangt horiz.)',  &
  &        ' pour l''unite logique ',KNUMER
    WRITE (UNIT=FA%NULOUT,FMT=*)'-----------------'
  ENDIF
  IF (FA%LFAMOP.AND.(FA%FICHIER(IRANG)%NFGRIB.LT.3              &
  &  .AND.FA%FICHIER(IRANG)%NFGRIB.GT.-1)                       &
  &  .AND.(KNGRIB.EQ.3.OR.KNGRIB.EQ.-1))           THEN
    WRITE (UNIT=FA%NULOUT,FMT=*)'-----------------'
    WRITE (UNIT=FA%NULOUT,FMT=*)                                    &
  &        'FAGOTE: WARNING!! Les champs spectraux devront',        &
  &        ' etre ranges comme dans le modele (rangt verti.) pour', &
  &        ' l''unite logique ',KNUMER
    WRITE (UNIT=FA%NULOUT,FMT=*)'-----------------'
  ENDIF

  FA%FICHIER(IRANG)%NBFPDG=KNARG1
  FA%FICHIER(IRANG)%NBFCSP=KNARG2
  FA%FICHIER(IRANG)%NMFDPL=KNARG5

ELSE

  IF (.NOT. LLMLAM) THEN
    IREP=-96
    GOTO 1001
  ENDIF

  FA%FICHIER(IRANG)%NCPLSIZE=KNARG1
  FA%FICHIER(IRANG)%NCPLBITS=KNARG2

ENDIF

FA%FICHIER(IRANG)%NFGRIB=KNGRIB
IREP=0

IF (KNGRIB /= 4) THEN
!
! Appel a FAINOC pour interpreter les eventuels defauts
! de -1 pris par FA%NBFPDG, FA%NBFCSP, FA%NSTROF et FA%NPUFLA en
! IRANG-ieme position.
!
  CALL FAINOC_FORT (FA, IRANG)
ENDIF
!
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE, AVEC "ABORT" EVENTUEL,
!            VIA LE SOUS-PROGRAMME "FAIPAR" .
!-----------------------------------------------------------------------
!
1001 CONTINUE
KREP=IREP
LLFATA=LLMOER (IREP,IRANG)
!
!        Deverrouillage eventuel du fichier.
!
IF (LLVERF) CALL LFIVER_FORT                                &
&                           (FA%LFI, FA%FICHIER(IRANG)%VRFICH,'OFF')
!
IF (LLFATA) THEN
  INIMES=2
ELSE
  INIMES=IXNVMS(IRANG)
ENDIF
!
IF (.NOT.LLFATA.AND.INIMES.NE.2)  THEN 
  IF (LHOOK) CALL DR_HOOK('FAGOTE_MT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='FAGOTE'
!
!***** FAZZZZ - KREP=iiii, KNUMER=iii, KNGRIB=ii, KNARG1=iii,   *****
!*****          KNARG2=iii, KNARG3=ii, KNARG4=iii, KNARG5=iii  *****
WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KNUMER='',I3,       &
&       '', KNGRIB='',I2,'', KNARG1='',I3,'', KNARG2='',I3,   &
&       '', KNARG3='',I2,'', KNARG4='',I3,'', KNARG5='',I3)') &
&   KREP,KNUMER,KNGRIB,KNARG1,KNARG2,KNARG3,KNARG4,KNARG5
CALL FAIPAR_FORT                                       &
&               (FA, KNUMER,INIMES,IREP,LLFATA,CLMESS, &
&                CLNSPR, CLACTI,.FALSE.)
!
IF (LHOOK) CALL DR_HOOK('FAGOTE_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"
#include "facom2.ixnvms.h"
#include "falgra.h"

END SUBROUTINE FAGOTE_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FAGOTE64                                        &
&           (KREP, KNUMER, KNGRIB, KNARG1, KNARG2, KNARG3, &
&           KNARG4, KNARG5)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                  FA_COM_DEFAULT_INIT,  &
&                  NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  KNGRIB                                 ! IN   
INTEGER (KIND=JPLIKB)  KNARG1                                 ! IN   
INTEGER (KIND=JPLIKB)  KNARG2                                 ! IN   
INTEGER (KIND=JPLIKB)  KNARG3                                 ! IN   
INTEGER (KIND=JPLIKB)  KNARG4                                 ! IN   
INTEGER (KIND=JPLIKB)  KNARG5                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAGOTE_FORT                                               &
&           (FA, KREP, KNUMER, KNGRIB, KNARG1, KNARG2, KNARG3, &
&            KNARG4, KNARG5)

END SUBROUTINE FAGOTE64

SUBROUTINE FAGOTE                                          &
&           (KREP, KNUMER, KNGRIB, KNARG1, KNARG2, KNARG3, &
&            KNARG4, KNARG5)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                  FA_COM_DEFAULT_INIT,  &
&                  NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKM)  KNGRIB                                 ! IN   
INTEGER (KIND=JPLIKM)  KNARG1                                 ! IN   
INTEGER (KIND=JPLIKM)  KNARG2                                 ! IN   
INTEGER (KIND=JPLIKM)  KNARG3                                 ! IN   
INTEGER (KIND=JPLIKM)  KNARG4                                 ! IN   
INTEGER (KIND=JPLIKM)  KNARG5                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAGOTE_MT                                                 &
&           (FA, KREP, KNUMER, KNGRIB, KNARG1, KNARG2, KNARG3, &
&            KNARG4, KNARG5)

END SUBROUTINE FAGOTE

SUBROUTINE FAGOTE_MT                                           &
&           (FA, KREP, KNUMER, KNGRIB, KNARG1, KNARG2, KNARG3, &
&            KNARG4, KNARG5)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKM)  KNGRIB                                 ! IN   
INTEGER (KIND=JPLIKM)  KNARG1                                 ! IN   
INTEGER (KIND=JPLIKM)  KNARG2                                 ! IN   
INTEGER (KIND=JPLIKM)  KNARG3                                 ! IN   
INTEGER (KIND=JPLIKM)  KNARG4                                 ! IN   
INTEGER (KIND=JPLIKM)  KNARG5                                 ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  INGRIB                                 ! IN   
INTEGER (KIND=JPLIKB)  INBPDG                                 ! IN   
INTEGER (KIND=JPLIKB)  INBCSP                                 ! IN   
INTEGER (KIND=JPLIKB)  ISTRON                                 ! IN   
INTEGER (KIND=JPLIKB)  IPUILA                                 ! IN   
INTEGER (KIND=JPLIKB)  IDMOPL                                 ! IN   
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)
INGRIB     = INT (    KNGRIB, JPLIKB)
INBPDG     = INT (    KNARG1, JPLIKB)
INBCSP     = INT (    KNARG2, JPLIKB)
ISTRON     = INT (    KNARG3, JPLIKB)
IPUILA     = INT (    KNARG4, JPLIKB)
IDMOPL     = INT (    KNARG5, JPLIKB)

CALL FAGOTE_FORT                                               &
&           (FA, IREP, INUMER, INGRIB, INBPDG, INBCSP, ISTRON, &
&            IPUILA, IDMOPL)

KREP       = INT (      IREP, JPLIKM)

END SUBROUTINE FAGOTE_MT

!INTF KREP            OUT 
!INTF KNUMER        IN    
!INTF KNGRIB        IN    
!INTF KNARG1        IN    
!INTF KNARG2        IN    
!INTF KNARG3        IN    
!INTF KNARG4        IN    
!INTF KNARG5        IN    
