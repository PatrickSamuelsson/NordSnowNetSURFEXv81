! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FATCHA_FORT                                              &
&                     (FA,  KREP, CDNOMC,  LDCOSP, KLCHAM)
USE FA_MOD, ONLY : FA_COM, JPNIIL
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!      Sous-programme du logiciel de Fichiers ARPEGE:
!      recuperation de la taille d'un champ horizontal
!**
!    Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                CDNOMC (Entree) ==> Nom du cadre
!                LDCOSP (Entree) ==> Vrai si le champ est represente
!                                    par des coefficients spectraux;
!                KLCHAM (Sortie) ==> Taille du champ
!*
!       En mode multi-taches, il doit y avoir verrouillage du fichier
!     concerne avant l'appel au sous-programme.
!
!
TYPE(FA_COM)   :: FA
INTEGER (KIND=JPLIKB) KREP, KLCHAM
CHARACTER(LEN=*) CDNOMC
LOGICAL LDCOSP
!
INTEGER (KIND=JPLIKB) ITRONC
INTEGER (KIND=JPLIKB) IRANGC, INIMES, INUMER
!
LOGICAL LLMLAM
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
IF (LHOOK) CALL DR_HOOK('FATCHA_MT',0,ZHOOK_HANDLE)

KREP=0

CALL FANUCA_FORT (FA, CDNOMC, IRANGC, .FALSE.)

IF (IRANGC.EQ.0) THEN
  KREP=-51
  GOTO 1001
ENDIF

CLACTI=''

LLMLAM=FA%CADRE(IRANGC)%LIMLAM
ITRONC=FA%CADRE(IRANGC)%MTRONC
!
IF (LDCOSP) THEN
  IF (LLMLAM) THEN
    KLCHAM=FA%CADRE(IRANGC)%NSFLAM
  ELSE
    KLCHAM=(1+ITRONC)*(2+ITRONC)
  ENDIF
ELSE
  KLCHAM=FA%CADRE(IRANGC)%NVAPDG
ENDIF

1001 CONTINUE

LLFATA = .TRUE.

IF (FA%LFAMOP.OR.LLFATA) THEN
  INIMES=2
  CLNSPR='FATCHA'
  INUMER=JPNIIL
!
  WRITE (UNIT=CLMESS,FMT='(''KREP='',I5,                  &
&        '', LDCOSP= '',L1, '', KLCHAM='',I6)')           &
&        KREP, LDCOSP, KLCHAM
  CALL FAIPAR_FORT                                        &
&                 (FA, INUMER,INIMES,KREP,.FALSE.,CLMESS, &
&                  CLNSPR,CLACTI,.FALSE.)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('FATCHA_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"

END SUBROUTINE

SUBROUTINE FATCHAT64 (KREP, CDNOMC, LDCOSP, KLCHAM)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                  FA_COM_DEFAULT_INIT,  &
&                  NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
INTEGER (KIND=JPLIKB) KREP, KLCHAM
CHARACTER(LEN=*) CDNOMC
LOGICAL LDCOSP

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FATCHA_FORT (FA,  KREP, CDNOMC,  LDCOSP, KLCHAM)

END SUBROUTINE

SUBROUTINE FATCHA (KREP, CDNOMC, LDCOSP, KLCHAM)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                  FA_COM_DEFAULT_INIT,  &
&                  NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
INTEGER (KIND=JPLIKM) KREP, KLCHAM
CHARACTER(LEN=*) CDNOMC
LOGICAL LDCOSP

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FATCHA_MT (FA,  KREP, CDNOMC,  LDCOSP, KLCHAM)

END SUBROUTINE

SUBROUTINE FATCHA_MT (FA, KREP, CDNOMC, LDCOSP, KLCHAM)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
TYPE (FA_COM)         FA
INTEGER (KIND=JPLIKM) KREP, KLCHAM
CHARACTER(LEN=*)      CDNOMC
LOGICAL               LDCOSP

INTEGER (KIND=JPLIKB) IREP, ILCHAM


CALL FATCHA_FORT (FA, IREP, CDNOMC,  LDCOSP, ILCHAM)

KREP   = INT (  IREP, JPLIKM)
KLCHAM = INT (ILCHAM, JPLIKM)

END SUBROUTINE


!INTF KREP            OUT                                                              
!INTF CDNOMC        IN                                                                 
!INTF LDCOSP        IN                                                                 
!INTF KLCHAM          OUT                                                              

