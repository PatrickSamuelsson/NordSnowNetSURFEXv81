! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FAISC1_FORT                  &
&                     (FA,  KREP, KRANG )
USE FA_MOD, ONLY : FA_COM, JPNIIL
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!      Ce sous-programme initialise un tableau "reference" de
!      l'en-tete GRIB, section 1.
!      (routine appelee une seule fois pour un fichier donne)
!**
!    Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                KRANG  (Entree) ==> Rang de l'unite logique;
!*
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KRANG
!
INTEGER (KIND=JPLIKB) I, INUMER
INTEGER (KIND=JPLIKB) INIMES, IRANGC
!
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA

!**
!     0.  -  INITIALISATIONS ET CONTROLES
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FAISC1_MT',0,ZHOOK_HANDLE)
KREP=0
IF (KRANG.LE.0.OR.KRANG.GT.FA%JPNXFA) THEN
  KREP=-66
  GOTO 1001
ENDIF
IRANGC=FA%FICHIER(KRANG)%NUCADR
!**
!     1.  -  INIT. DU TAB. FA%NSEC1 REPRESENTANT LA SECTION 1 DE GRIBEX
!-----------------------------------------------------------------------
!
!  2: identification of centre
!
! (defaut=85 pour Toulouse; pour en changer, utiliser FAREGU)
FA%FICHIER(KRANG)%NSEC1(2) = 85
!  3: generating process identification number, alloc by the orig. centre
IF (FA%CADRE(IRANGC)%LIMLAM) THEN
! Il s'agit du modele Aladin
  FA%FICHIER(KRANG)%NSEC1(3) = 177
ELSE
! Il s'agit du modele Arpege
  IF (FA%FICHIER(KRANG)%MADATE(7).GT.0) THEN
!     prevision
    FA%FICHIER(KRANG)%NSEC1(3) = 211
  ELSE
!     analyse
    FA%FICHIER(KRANG)%NSEC1(3) = 201
!     analyse initialisee -> prevision
    IF (FA%FICHIER(KRANG)%MADATE(9).EQ.1) FA%FICHIER(KRANG)%NSEC1(3) = 211
  ENDIF
ENDIF

FA%FICHIER(KRANG)%NIDCEN = FA%FICHIER(KRANG)%NSEC1(2)

!  4: grid definition
!     =255 for a non-catalogued grid (description follows in KSEC2)
FA%FICHIER(KRANG)%NSEC1(4) = 255
!  5: flag showing whether sections 2 and 3 are present
!     128 --> Section 2 is included, Section 3 is omitted (no bitmap)
FA%FICHIER(KRANG)%NSEC1(5) = 128
!  6 a 9: to be initialized later (specific to each field)
FA%FICHIER(KRANG)%NSEC1(6:9) = 0
! 10 a 21: valeurs deduites de FA%MADATE(1:11,KRANG)
!
! rappel :   2000 -->  an 100 (=FA%NSEC1(10))  siecle 20 (=FA%NSEC1(21))
!            2001 -->  an   1  "        "   siecle 21  "        "
!
FA%FICHIER(KRANG)%NSEC1(10) = 1 + MOD(FA%FICHIER(KRANG)%MADATE(1) - 1 , 100_JPLIKB )
FA%FICHIER(KRANG)%NSEC1(21) = 1 + (FA%FICHIER(KRANG)%MADATE(1) - 1) / 100
DO I=1,10
  FA%FICHIER(KRANG)%NSEC1(10+I) = FA%FICHIER(KRANG)%MADATE(1+I)
ENDDO

! FA%NSEC1(18,KRANG)=10 signifie un codage sur 2 octets de l'echeance
! Ce n'est pas le cas, donc on revient a 0 pour GRIBEX
IF (FA%FICHIER(KRANG)%NSEC1(18)==10) FA%FICHIER(KRANG)%NSEC1(18)=0
! FA%MADATE(10,KRANG) peut contenir l'echeance precedente dans le cas
! d'un calcul sur une periode (min, max par exemple): c'est une
! convention dans FA (depuis fin 2000) qui est incompatible avec
! GRIBEX. On retire donc cette valeur ici dans FA%NSEC1, sachant
! qu'elle sera utilisee plus tard.
FA%FICHIER(KRANG)%NSEC1(19) = 0
!
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE, AVEC "ABORT" EVENTUEL,
!            VIA LE SOUS-PROGRAMME "FAIPAR" .
!-----------------------------------------------------------------------
!
1001 CONTINUE
LLFATA=LLMOER (KREP,KRANG)
!
IF (FA%LFAMOP.OR.LLFATA) THEN
  INIMES=2
  CLNSPR='FAISC1'
  INUMER=JPNIIL
!
  WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KRANG='',I4)') &
&     KREP, KRANG
  CALL FAIPAR_FORT                                      &
&                 (FA, INUMER,INIMES,KREP,.FALSE.,CLMESS, &
&                  CLNSPR, CLNSPR,.FALSE.)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('FAISC1_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"

END SUBROUTINE FAISC1_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FAISC164           &
&           (KREP, KRANG)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KRANG                                  ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAISC1_FORT            &
&           (FA, KREP, KRANG)

END SUBROUTINE FAISC164

SUBROUTINE FAISC1             &
&           (KREP, KRANG)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KRANG                                  ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAISC1_MT              &
&           (FA, KREP, KRANG)

END SUBROUTINE FAISC1

SUBROUTINE FAISC1_MT             &
&           (FA, KREP, KRANG)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KRANG                                  ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  IRANG                                  ! IN   
! Convert arguments

IRANG      = INT (     KRANG, JPLIKB)

CALL FAISC1_FORT            &
&           (FA, IREP, IRANG)

KREP       = INT (      IREP, JPLIKM)

END SUBROUTINE FAISC1_MT

!INTF KREP            OUT 
!INTF KRANG         IN    
