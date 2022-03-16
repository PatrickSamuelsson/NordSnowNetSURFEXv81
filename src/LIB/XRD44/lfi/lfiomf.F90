! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI

SUBROUTINE LFIOMF_FORT                             &
&                     (LFI, KREP, KNUMER, KNIMES )
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Ce sous-programme permet d'obtenir la valeur courante du niveau
!     de messagerie propre aux actions faites via le logiciel LFI,
!     sur un fichier particulier ouvert pour ce logiciel.
!
!        Noter que si le niveau global de messagerie *LFI%NIMESG*
!     vaut 0 ou 2, le niveau propre au fichier est inoperant.
!     *LFI%NIMESG* vaut par defaut 1, est reglable via le s/p "LFINMG",
!     et sa valeur courante peut etre obtenue par le s/p "LFIOMG".
!**
!     ARGUMENTS : KREP   (Sortie) ==> Code-REPonse du sous-programme;
!                 KNUMER (Entree) ==> NUMERo d'unite logique concernee;
!                 KNIMES (Sortie) ==> NIveau courant de MESsagerie.
!
!
TYPE(LFICOM) :: LFI
INTEGER (KIND=JPLIKB) KREP, KNUMER, KNIMES, IRANG, IREP, INIMES
!
CHARACTER(LEN=LFI%JPLSPX) CLNSPR
CHARACTER(LEN=LFI%JPLMES) CLMESS
CHARACTER(LEN=LFI%JPLFTX) CLACTI
LOGICAL LLFATA

!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('LFIOMF_FORT',0,ZHOOK_HANDLE)
CLACTI=''
CALL LFINUM_FORT                    &
&               (LFI, KNUMER,IRANG)
!
IF (IRANG.EQ.0) THEN
  IREP=-1
ELSE
  KNIMES=LFI%NIVMES(IRANG)
  LFI%NDEROP(IRANG)=21
  IREP=0
ENDIF
!
LLFATA=LLMOER (IREP,IRANG)
KREP=IREP
!
IF (LLFATA.OR.IXNIMS (IRANG).EQ.2) THEN
  INIMES=2
ELSE
  IF (LHOOK) CALL DR_HOOK('LFIOMF_FORT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='LFIOMF'
WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KNUMER='',I3, &
&       '', KNIMES='',I3)') KREP,KNUMER,KNIMES
CALL LFIEMS_FORT                                 &
&               (LFI, KNUMER,INIMES,IREP,LLFATA, &
&                CLMESS,CLNSPR,CLACTI)
!
IF (LHOOK) CALL DR_HOOK('LFIOMF_FORT',1,ZHOOK_HANDLE)

CONTAINS

#include "lficom2.ixnims.h"
#include "lficom2.llmoer.h"

END SUBROUTINE LFIOMF_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFIOMF64              &
&           (KREP, KNUMER, KNIMES)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  KNIMES                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIOMF_FORT                       &
&           (LFI, KREP, KNUMER, KNIMES)

END SUBROUTINE LFIOMF64

SUBROUTINE LFIOMF                &
&           (KREP, KNUMER, KNIMES)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKM)  KNIMES                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIOMF_MT                        &
&           (LFI, KREP, KNUMER, KNIMES)

END SUBROUTINE LFIOMF

SUBROUTINE LFIOMF_MT                  &
&           (LFI, KREP, KNUMER, KNIMES)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKM)  KNIMES                                 !   OUT
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  INIMES                                 !   OUT
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)

CALL LFIOMF_FORT                       &
&           (LFI, IREP, INUMER, INIMES)

KREP       = INT (      IREP, JPLIKM)
KNIMES     = INT (    INIMES, JPLIKM)

END SUBROUTINE LFIOMF_MT

!INTF KREP            OUT 
!INTF KNUMER        IN    
!INTF KNIMES          OUT 
