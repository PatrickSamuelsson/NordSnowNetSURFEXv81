! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FAIS2F_FORT                  &
&                     (FA,  KREP, KRANG )
USE FA_MOD, ONLY : FA_COM, JPNIIL
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!      Ce sous-programme initialise un tableau "reference" de
!      l'en-tete GRIB, section 2.
!      (routine appelee une seule fois pour un fichier Aladin donne)
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
INTEGER (KIND=JPLIKB) IRANGC, JM, JMAX, ILOW
INTEGER (KIND=JPLIKB) IADD, INUMER, INIMES
!
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA

!**
!     0.  -  INITIALISATIONS ET CONTROLES
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FAIS2F_MT',0,ZHOOK_HANDLE)
KREP=0
IF (KRANG.LE.0.OR.KRANG.GT.FA%JPNXFA) THEN
  KREP=-66
  GOTO 1001
ENDIF
IRANGC=FA%FICHIER(KRANG)%NUCADR
!**
!     1.  -  INITIALISATION DU TABLEAU FA%NSC2ALF
!-----------------------------------------------------------------------
!
!  Les valeurs de ce tableau representent les nb de pts
! le long de chaque "parallele" (ici, le nb de coeff spectraux
! pour un meme m (nb d'onde zonal), excepte le triangle et les axes non
! compactes). Il s'agit en effet de deguiser un champ spectral 
! Aladin en champ pts de grille (grille lat-lon) pour profiter
! du compactage, voire de la compression, GRIBEX.
! Le rangt des CSP est fait verticalement (par colonne de m=cst)
! et pour chaque couple (m,n) correspond 4 CSP.
!
JMAX = (FA%CADRE(IRANGC)%NOZPAR(6)-FA%CADRE(IRANGC)%NOZPAR(5)+1)/4 -1
DO JM=1,JMAX
  ILOW=2+2*JM+1
  IADD=4* MAX(FA%FICHIER(KRANG)%NSTROF+1-JM,1_JPLIKB )
!
  FA%FICHIER(KRANG)%NSC2ALF(JM)=FA%CADRE(IRANGC)%NOMPAR(ILOW+1)-        &
&                        (FA%CADRE(IRANGC)%NOMPAR(ILOW)+IADD)+1
ENDDO
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
  CLNSPR='FAIS2F'
  INUMER=JPNIIL
!
  WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KRANG='',I4)') &
&     KREP, KRANG
  CALL FAIPAR_FORT                                      &
&                 (FA, INUMER,INIMES,KREP,.FALSE.,CLMESS, &
&                  CLNSPR,CLNSPR,.FALSE.)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('FAIS2F_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"

END SUBROUTINE FAIS2F_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FAIS2F64           &
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

CALL FAIS2F_FORT            &
&           (FA, KREP, KRANG)

END SUBROUTINE FAIS2F64

SUBROUTINE FAIS2F             &
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

CALL FAIS2F_MT              &
&           (FA, KREP, KRANG)

END SUBROUTINE FAIS2F

SUBROUTINE FAIS2F_MT             &
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

CALL FAIS2F_FORT            &
&           (FA, IREP, IRANG)

KREP       = INT (      IREP, JPLIKM)

END SUBROUTINE FAIS2F_MT

!INTF KREP            OUT 
!INTF KRANG         IN    
