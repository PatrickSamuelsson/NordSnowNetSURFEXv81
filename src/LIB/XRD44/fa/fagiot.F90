! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FAGIOT_FORT                                              &
&                     (FA,  KNGRIB, KNARG1, KNARG2, KNARG3, KNARG4, &
&                      KNARG5)
USE FA_MOD, ONLY : FA_COM, JPNIIL
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Ce sous-programme permet de modifier les options implicites
!     liees au codage GRIB des champs.
!     CES OPTIONS NE SONT UTILISEES QUE POUR (RE)ECRIRE DES CHAMPS
!     codes en GRIB, et les nouvelles valeurs implicites ne serviront
!     que LORS d'une OUVERTURE de FICHIER ULTERIEURE.
!       ( Grib, Implicites Options Techniques )
!**
!     Arguments : KNGRIB (Entree) ==> Niveau de codage GRIB (-1,0,1,2,3);
!                 KNARG1 (Entree) ==> Nombre de bits par valeur point-
!                                     de-grille;
!                 KNARG2 (Entree) ==> Nombre de bits par partie reelle/
!                                     imaginaire de coeff. spectral;
!                 KNARG3 (Entree) ==> Sous-troncature non compactee;
!                 KNARG4 (Entree) ==> Puissance de laplacien;
!                 KNARG5 (Entree) ==> Degre de modulation de KNARG4.
!
!     N.B.: Il doit y avoir coherence vis-a-vis des cadres deja definis
!           et vis-a-vis des limites usagers.
!           ( ce qui en pratique, ne concerne que KNARG3 )
!
!     Remarque:  KNARG3 egal a -1 est accepte, et dans ce cas
!                on indexera (a chaque ouverture de fichier) la sous-
!                troncature effective sur la troncature.
!
!      MODIF 30/03/2007 JM AUDOIN FA%LFAMOP pour limiter IMPRESSION
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KNGRIB, KNARG1, KNARG2
INTEGER (KIND=JPLIKB) KNARG3, KNARG4, KNARG5
!
INTEGER (KIND=JPLIKB) IMINIM, IREP, INIMES
INTEGER (KIND=JPLIKB) INUMER, J, IRANGC
!
LOGICAL LLVERG
!
!
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
IF (LHOOK) CALL DR_HOOK('FAGIOT_MT',0,ZHOOK_HANDLE)
CLACTI=''
IF (FA%FAGIOT_LLPREA) THEN
!
!          A la premiere utilisation, appel au sous-programme "FARINE".
!
  CALL FARINE_FORT             &
&                (FA, 2_JPLIKB )
  FA%FAGIOT_LLPREA=.FALSE.
ENDIF
!
LLVERG=.FALSE.
IMINIM=MIN (2+KNGRIB,2+KNARG1,2+KNARG2,2+KNARG3,1+KNARG5)
!
IF (IMINIM.LE.0) THEN
  IREP=-64
  GOTO 1001
ELSEIF (KNARG1*KNARG2.EQ.0) THEN
  IREP=-64
  GOTO 1001
ELSEIF (KNGRIB.GT.3 .AND. .NOT. FALGRA (KNGRIB)) THEN
  IREP=-96
  GOTO 1001
ELSEIF (MAX (KNARG1,KNARG2).GT.FA%NBIMAX) THEN
  IREP=-97
  GOTO 1001
ELSEIF (ABS (KNARG4).GT.2**15-1) THEN
  IREP=-98
  GOTO 1001
ENDIF
!
!         Verrouillage global eventuel.
!
IF (FA%LFAMUL) CALL LFIVER_FORT                        &
&                              (FA%LFI, FA%VRGLAS,'ON')
LLVERG=FA%LFAMUL
!
IF (KNARG3.GE.FA%NXTRON) THEN
  IREP=-113
  GOTO 1001
ENDIF
!
!        Coherence de "KNARG3" vis-a-vis de la troncature des cadres
!     deja definis.
!
DO J=1,FA%NCADEF
IRANGC=FA%NCAIND(J)
!
IF (KNARG3.GE.FA%CADRE(IRANGC)%MTRONC) THEN
  IREP=-99
  GOTO 1001
ENDIF
!
ENDDO
!**
!     2.  -  STOCKAGE DES NOUVEAUX PARAMETRES.
!-----------------------------------------------------------------------
!
IF (FA%LFAMOP.AND.(FA%NIGRIB.EQ.-1.OR.FA%NIGRIB.EQ.3).AND. &
&    (KNGRIB.GT.-1.AND.KNGRIB.LT.3))     THEN
  WRITE (UNIT=FA%NULOUT,FMT=*)'-----------------'
  WRITE (UNIT=FA%NULOUT,FMT=*)                                   &
&          'FAGIOT: CAUTION!! Les champs spectraux ARPEGE ne',    &
&          ' devront pas etre ranges comme dans le modele ARPEGE'
  WRITE (UNIT=FA%NULOUT,FMT=*)'-----------------'
ENDIF
IF (FA%LFAMOP.AND.(KNGRIB.EQ.-1.OR.KNGRIB.EQ.3).AND. &
&    (FA%NIGRIB.GT.-1.AND.FA%NIGRIB.LT.3))     THEN
  WRITE (UNIT=FA%NULOUT,FMT=*)'-----------------'
  WRITE (UNIT=FA%NULOUT,FMT=*)                                     &
&          'FAGIOT: CAUTION!! Les champs spectraux ARPEGE devront', &
&          ' etre ranges comme dans le modele ARPEGE'
  WRITE (UNIT=FA%NULOUT,FMT=*)'-----------------'
ENDIF
FA%NIGRIB=KNGRIB
FA%NBIPDG=KNARG1
FA%NBICSP=KNARG2
FA%NSTROI=KNARG3
FA%NPUILA=KNARG4
FA%NMIDPL=KNARG5
IREP=0
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE, AVEC "ABORT" EVENTUEL,
!            VIA LE SOUS-PROGRAMME "FAIPAR" .
!-----------------------------------------------------------------------
!
1001 CONTINUE
LLFATA=LLMOER (IREP,0_JPLIKB )
!
!        Deverrouillage global eventuel.
!
IF (LLVERG) CALL LFIVER_FORT                         &
&                           (FA%LFI, FA%VRGLAS,'OFF')
!
IF (LLFATA) THEN
  INIMES=2
ELSE
  INIMES=FA%NIMSGA
ENDIF
!
IF (.NOT.LLFATA.AND.INIMES.NE.2)  THEN 
  IF (LHOOK) CALL DR_HOOK('FAGIOT_MT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='FAGIOT'
!
WRITE (UNIT=CLMESS,FMT='(''KNGRIB='',I2,'', KNARG1='',I3,  &
&       '', KNARG2='',I3,'', KNARG3='',I2,'', KNARG4='',I3, &
&       '', KNARG5='',I3)')                                 &
&   KNGRIB,KNARG1,KNARG2,KNARG3,KNARG4,KNARG5
INUMER=JPNIIL
CALL FAIPAR_FORT                                     &
&               (FA, INUMER,INIMES,IREP,LLFATA,CLMESS, &
&                CLNSPR,CLACTI,.FALSE.)
!
IF (LHOOK) CALL DR_HOOK('FAGIOT_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"
#include "falgra.h"

END SUBROUTINE FAGIOT_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FAGIOT64                                        &
&           (KNGRIB, KNARG1, KNARG2, KNARG3, KNARG4, KNARG5)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KNGRIB                                 ! IN   
INTEGER (KIND=JPLIKB)  KNARG1                                 ! IN   
INTEGER (KIND=JPLIKB)  KNARG2                                 ! IN   
INTEGER (KIND=JPLIKB)  KNARG3                                 ! IN   
INTEGER (KIND=JPLIKB)  KNARG4                                 ! IN   
INTEGER (KIND=JPLIKB)  KNARG5                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAGIOT_FORT                                               &
&           (FA, KNGRIB, KNARG1, KNARG2, KNARG3, KNARG4, KNARG5)

END SUBROUTINE FAGIOT64

SUBROUTINE FAGIOT                                          &
&           (KNGRIB, KNARG1, KNARG2, KNARG3, KNARG4, KNARG5)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KNGRIB                                 ! IN   
INTEGER (KIND=JPLIKM)  KNARG1                                 ! IN   
INTEGER (KIND=JPLIKM)  KNARG2                                 ! IN   
INTEGER (KIND=JPLIKM)  KNARG3                                 ! IN   
INTEGER (KIND=JPLIKM)  KNARG4                                 ! IN   
INTEGER (KIND=JPLIKM)  KNARG5                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAGIOT_MT                                                 &
&           (FA, KNGRIB, KNARG1, KNARG2, KNARG3, KNARG4, KNARG5)

END SUBROUTINE FAGIOT

SUBROUTINE FAGIOT_MT                                           &
&           (FA, KNGRIB, KNARG1, KNARG2, KNARG3, KNARG4, KNARG5)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KNGRIB                                 ! IN   
INTEGER (KIND=JPLIKM)  KNARG1                                 ! IN   
INTEGER (KIND=JPLIKM)  KNARG2                                 ! IN   
INTEGER (KIND=JPLIKM)  KNARG3                                 ! IN   
INTEGER (KIND=JPLIKM)  KNARG4                                 ! IN   
INTEGER (KIND=JPLIKM)  KNARG5                                 ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  INGRIB                                 ! IN   
INTEGER (KIND=JPLIKB)  INARG1                                 ! IN   
INTEGER (KIND=JPLIKB)  INARG2                                 ! IN   
INTEGER (KIND=JPLIKB)  INARG3                                 ! IN   
INTEGER (KIND=JPLIKB)  INARG4                                 ! IN   
INTEGER (KIND=JPLIKB)  INARG5                                 ! IN   
! Convert arguments

INGRIB     = INT (    KNGRIB, JPLIKB)
INARG1     = INT (    KNARG1, JPLIKB)
INARG2     = INT (    KNARG2, JPLIKB)
INARG3     = INT (    KNARG3, JPLIKB)
INARG4     = INT (    KNARG4, JPLIKB)
INARG5     = INT (    KNARG5, JPLIKB)

CALL FAGIOT_FORT                                               &
&           (FA, INGRIB, INARG1, INARG2, INARG3, INARG4, INARG5)


END SUBROUTINE FAGIOT_MT

!INTF KNGRIB        IN    
!INTF KNARG1        IN    
!INTF KNARG2        IN    
!INTF KNARG3        IN    
!INTF KNARG4        IN    
!INTF KNARG5        IN    
