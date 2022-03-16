! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FAVEUR_FORT                                             &
&                     (FA,  KREP, KNUMER, KNGRIB, KNARG1, KNARG2,  &
&                      KNARG3, KNARG4, KNARG5)
USE FA_MOD, ONLY : FA_COM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Ce sous-programme permet d'obtenir, pour un fichier ARPEGE
!     ouvert, les options courantes liees au codage GRIB des champs.
!     CES OPTIONS NE SONT UTILISEES QUE POUR (RE)ECRIRE DES CHAMPS
!     codes en GRIB.
!       ( Visualisation (?) options Effectives pour l'UtilisateuR )
!**
!     Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                 KNUMER (Entree) ==> Numero d'Unite Logique concernee;
!                 KNGRIB (Sortie) ==> Niveau de codage GRIB (-1,0,1,2,3,4);
!
!               * Pour KNGRIB compris entre -1 et 3, les arguments 
!                 de sortie ont la signification suivante:
!                 KNARG1 (Sortie) ==> Nombre de bits par valeur point-de-grille;
!                 KNARG2 (Sortie) ==> Nombre de bits par partie reelle/imaginaire
!                                     de coefficient spectral;
!                 KNARG3 (Sortie) ==> Sous-troncature non compactee;
!                 KNARG4 (Sortie) ==> Puissance de laplacien;
!                 KNARG5 (Sortie) ==> Degre de modulation de KNARG4.
!
!               * Pour KNGRIB==4, les arguments de sortie ont la 
!                 signification suivante:
!                 
!                 KNARG1 (Sortie) ==> Taille de la couronne a conserver
!                 KNARG2 (Sortie) ==> Nombre de bits utilises pour le codage
!                 KNARG3 (Sortie) ==> Inutilise
!                 KNARG4 (Sortie) ==> Inutilise
!                 KNARG5 (Sortie) ==> Inutilise
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KNUMER, KNGRIB
INTEGER (KIND=JPLIKB) KNARG1, KNARG2, KNARG3, KNARG4
INTEGER (KIND=JPLIKB) KNARG5
!
INTEGER (KIND=JPLIKB) IREP, IRANG, INIMES
!
LOGICAL LLVERF
!
CHARACTER(LEN=FA%JPXNOM) CLACTI 
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA

!**
!     1.  -  INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FAVEUR_MT',0,ZHOOK_HANDLE)
CLACTI=''
LLVERF=.FALSE.
CALL FANUMU_FORT                 &
&               (FA, KNUMER,IRANG)
!
IF (IRANG.EQ.0) THEN
  IREP=-51
  GOTO 1001
ENDIF
!
!         Verrouillage eventuel du fichier.
!
IF (FA%LFAMUL) CALL LFIVER_FORT                               &
&                              (FA%LFI, FA%FICHIER(IRANG)%VRFICH,'ON')
LLVERF=FA%LFAMUL
!**
!     2.  -  RECOPIE DES VALEURS EN COMMON DANS LES ARGUMENTS.
!-----------------------------------------------------------------------
!
KNGRIB=FA%FICHIER(IRANG)%NFGRIB

IF (KNGRIB /= 4) THEN
  KNARG1=FA%FICHIER(IRANG)%NBFPDG
  KNARG2=FA%FICHIER(IRANG)%NBFCSP
  KNARG3=FA%FICHIER(IRANG)%NSTROF
  KNARG4=FA%FICHIER(IRANG)%NPUFLA
  KNARG5=FA%FICHIER(IRANG)%NMFDPL
ELSE
  KNARG1=FA%FICHIER(IRANG)%NCPLSIZE
  KNARG2=FA%FICHIER(IRANG)%NCPLBITS
  KNARG3=0
  KNARG4=0
  KNARG5=0
ENDIF
IREP=0
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
  IF (LHOOK) CALL DR_HOOK('FAVEUR_MT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='FAVEUR'
!
WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KNUMER='',I3,      &
&       '', KNGRIB='',I2,'', KNARG1='',I3,'', KNARG2='',I3,   &
&       '', KNARG3='',I2,'', KNARG4='',I3,'', KNARG5='',I3)') &
&   KREP,KNUMER,KNGRIB,KNARG1,KNARG2,KNARG3,KNARG4,KNARG5
CALL FAIPAR_FORT                                     &
&               (FA, KNUMER,INIMES,IREP,LLFATA,CLMESS, &
&                CLNSPR,CLACTI,.FALSE.)
!
IF (LHOOK) CALL DR_HOOK('FAVEUR_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"
#include "facom2.ixnvms.h"

END SUBROUTINE FAVEUR_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FAVEUR64                                      &
&           (KREP, KNUMER, KNGRIB, KNARG1, KNARG2, KNARG3, &
&           KNARG4, KNARG5)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  KNGRIB                                 !   OUT
INTEGER (KIND=JPLIKB)  KNARG1                                 !   OUT
INTEGER (KIND=JPLIKB)  KNARG2                                 !   OUT
INTEGER (KIND=JPLIKB)  KNARG3                                 !   OUT
INTEGER (KIND=JPLIKB)  KNARG4                                 !   OUT
INTEGER (KIND=JPLIKB)  KNARG5                                 !   OUT

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAVEUR_FORT                                               &
&           (FA, KREP, KNUMER, KNGRIB, KNARG1, KNARG2, KNARG3, &
&           KNARG4, KNARG5)

END SUBROUTINE FAVEUR64

SUBROUTINE FAVEUR                                          &
&           (KREP, KNUMER, KNGRIB, KNARG1, KNARG2, KNARG3, &
&           KNARG4, KNARG5)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT,  &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKM)  KNGRIB                                 !   OUT
INTEGER (KIND=JPLIKM)  KNARG1                                 !   OUT
INTEGER (KIND=JPLIKM)  KNARG2                                 !   OUT
INTEGER (KIND=JPLIKM)  KNARG3                                 !   OUT
INTEGER (KIND=JPLIKM)  KNARG4                                 !   OUT
INTEGER (KIND=JPLIKM)  KNARG5                                 !   OUT

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAVEUR_MT                                                 &
&           (FA, KREP, KNUMER, KNGRIB, KNARG1, KNARG2, KNARG3, &
&           KNARG4, KNARG5)

END SUBROUTINE FAVEUR

SUBROUTINE FAVEUR_MT                                           &
&           (FA, KREP, KNUMER, KNGRIB, KNARG1, KNARG2, KNARG3, &
&           KNARG4, KNARG5)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKM)  KNGRIB                                 !   OUT
INTEGER (KIND=JPLIKM)  KNARG1                                 !   OUT
INTEGER (KIND=JPLIKM)  KNARG2                                 !   OUT
INTEGER (KIND=JPLIKM)  KNARG3                                 !   OUT
INTEGER (KIND=JPLIKM)  KNARG4                                 !   OUT
INTEGER (KIND=JPLIKM)  KNARG5                                 !   OUT
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  INGRIB                                 !   OUT
INTEGER (KIND=JPLIKB)  INARG1                                 !   OUT
INTEGER (KIND=JPLIKB)  INARG2                                 !   OUT
INTEGER (KIND=JPLIKB)  INARG3                                 !   OUT
INTEGER (KIND=JPLIKB)  INARG4                                 !   OUT
INTEGER (KIND=JPLIKB)  INARG5                                 !   OUT
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)

CALL FAVEUR_FORT                                               &
&           (FA, IREP, INUMER, INGRIB, INARG1, INARG2, INARG3, &
&           INARG4, INARG5)

KREP       = INT (      IREP, JPLIKM)
KNGRIB     = INT (    INGRIB, JPLIKM)
KNARG1     = INT (    INARG1, JPLIKM)
KNARG2     = INT (    INARG2, JPLIKM)
KNARG3     = INT (    INARG3, JPLIKM)
KNARG4     = INT (    INARG4, JPLIKM)
KNARG5     = INT (    INARG5, JPLIKM)

END SUBROUTINE FAVEUR_MT

!INTF KREP            OUT 
!INTF KNUMER        IN    
!INTF KNGRIB          OUT 
!INTF KNARG1          OUT 
!INTF KNARG2          OUT 
!INTF KNARG3          OUT 
!INTF KNARG4          OUT 
!INTF KNARG5          OUT 

