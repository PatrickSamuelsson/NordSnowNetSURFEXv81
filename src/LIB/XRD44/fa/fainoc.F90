! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FAINOC_FORT            &
&                     (FA,  KRANG )
USE FA_MOD, ONLY : FA_COM, JPNIIL
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Ce sous-programme permet d'INterpreter, pour un fichier ARPEGE
!     ouvert, les Options par defaut (-1) du Codage GRIB des champs:
!     FA%NBFPDG(KRANG), FA%NBFCSP(KRANG), FA%NSTROF(KRANG), FA%NPUFLA(KRANG).
!     Cette routine doit etre appelee par FAITOU ou FANOUV ou FAGOTE
!     pour ne pas laisser le defaut -1 lors du decodage ou du codage
!     GRIB.
!
!**
!     Arguments : KRANG  (Entree) ==> Rang de l'unite logique;
!
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KRANG
!
INTEGER (KIND=JPLIKB) IRANGC, ITRONC, INBITS
INTEGER (KIND=JPLIKB) ITYPTR, IAUXIL, IREP, INIMES
INTEGER (KIND=JPLIKB) INUMER
!
LOGICAL LLVERF, LLMLAM
!
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR

!**
!     1.  -  INITIALISATIONS PREALABLES.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FAINOC_MT',0,ZHOOK_HANDLE)
LLVERF=.FALSE.
!
!         Verrouillage eventuel du fichier.
!
IF (FA%LFAMUL) CALL LFIVER_FORT                               &
&                              (FA%LFI, FA%FICHIER(KRANG)%VRFICH,'ON')
LLVERF=FA%LFAMUL
!
IRANGC=FA%FICHIER(KRANG)%NUCADR
LLMLAM=FA%CADRE(IRANGC)%LIMLAM
ITRONC=FA%CADRE(IRANGC)%MTRONC
ITYPTR=FA%CADRE(IRANGC)%NTYPTR
!
!**
!     2.  -  INTERPRETATION DES OPTIONS PAR DEFAUT.
!-----------------------------------------------------------------------
!
! On distingue le cas ARPEGE du cas ALADIN (LLMLAM=.T.).
!
!
! Evaluation du nombre de bits par valeur point-de-grille
!
IF (FA%FICHIER(KRANG)%NBFPDG.LT.0) THEN
  IF (LLMLAM) THEN
    FA%FICHIER(KRANG)%NBFPDG=16
  ELSE
    FA%FICHIER(KRANG)%NBFPDG=16
  ENDIF
ENDIF
!
! Evaluation du nombre de bits par partie reelle/imagin. de coeff. spectral
!
IF (FA%FICHIER(KRANG)%NBFCSP.LT.0) THEN
  IF (LLMLAM) THEN
    FA%FICHIER(KRANG)%NBFCSP=18
  ELSE
    FA%FICHIER(KRANG)%NBFCSP=16
  ENDIF
ENDIF
!
! Evaluation de la sous-troncature non compactee
!
IF (FA%FICHIER(KRANG)%NSTROF.LT.0) THEN
  INBITS=FA%FICHIER(KRANG)%NBFCSP
  IF (LLMLAM) THEN
    IAUXIL=MAX ( ITRONC, -ITYPTR )
    IAUXIL=MAX ( 10_JPLIKB , ((1+IAUXIL)*25)/(10*INBITS),  &
&               (1+IAUXIL)/10 )
    IAUXIL=MIN ( IAUXIL, ITRONC-1, -ITYPTR-1 )
    FA%FICHIER(KRANG)%NSTROF=IAUXIL
  ELSE
    IAUXIL=MAX ( 10_JPLIKB , 480/INBITS-10, (1+ITRONC)/10 )
    IAUXIL=MIN ( IAUXIL, ITRONC-1 )
    FA%FICHIER(KRANG)%NSTROF=IAUXIL
  ENDIF
ENDIF
!
! Evaluation de la puissance de laplacien
!
IF (FA%FICHIER(KRANG)%NPUFLA.LT.0) THEN
  IF (LLMLAM) THEN
    FA%FICHIER(KRANG)%NPUFLA=2
  ELSE
    FA%FICHIER(KRANG)%NPUFLA=1
  ENDIF
ENDIF
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE,
!            VIA LE SOUS-PROGRAMME "FAIPAR" .
!-----------------------------------------------------------------------
!
!
!        Deverrouillage eventuel du fichier.
!
IF (LLVERF) CALL LFIVER_FORT                                &
&                           (FA%LFI, FA%FICHIER(KRANG)%VRFICH,'OFF')
!
IF (IXNVMS(KRANG).NE.2)  THEN 
  IF (LHOOK) CALL DR_HOOK('FAINOC_MT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
INUMER=JPNIIL
INIMES=IXNVMS (KRANG)
IREP=0
CLNSPR='FAINOC'
WRITE (UNIT=CLMESS,FMT='(''KRANG='',I4)') KRANG
CALL FAIPAR_FORT                                      &
&               (FA, INUMER,INIMES,IREP,.FALSE.,CLMESS, &
&                CLNSPR,' ',.FALSE.)
!
IF (LHOOK) CALL DR_HOOK('FAINOC_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.ixnvms.h"

END SUBROUTINE FAINOC_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FAINOC64           &
&           (KRANG)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KRANG                                  ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAINOC_FORT           &
&           (FA, KRANG)

END SUBROUTINE FAINOC64

SUBROUTINE FAINOC             &
&           (KRANG)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KRANG                                  ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAINOC_MT             &
&           (FA, KRANG)

END SUBROUTINE FAINOC

SUBROUTINE FAINOC_MT             &
&           (FA, KRANG)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KRANG                                  ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IRANG                                  ! IN   
! Convert arguments

IRANG      = INT (     KRANG, JPLIKB)

CALL FAINOC_FORT           &
&           (FA, IRANG)


END SUBROUTINE FAINOC_MT

!INTF KRANG         IN    
