! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI

SUBROUTINE LFIOSF_FORT                             &
&                     (LFI, KREP, KNUMER, LDIMST )
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Ce sous-programme permet d'obtenir l'option courante gouvernant
!     l'impression de STATISTIQUES a la fermeture d'un fichier
!     particulier, ouvert pour le logiciel LFI.
!
!        Noter que si le niveau global d'impression des statistiques
!     *LFI%NISTAG* vaut 0 ou 2, l'option propre au fichier est inoperante.
!     *LFI%NISTAG* vaut par defaut 1, est reglable via le s/p "LFINSG",
!     et sa valeur peut etre obtenue par le s/p "LFIOSG".
!**
!     ARGUMENTS : KREP   (Sortie) ==> Code-REPonse du sous-programme;
!                 KNUMER (Entree) ==> NUMERo d'unite logique concernee;
!                 LDIMST (Sortie) ==> Option d'IMpression des STatisti-
!                                     ques a la fermeture (vrai=oui).
!
!
TYPE(LFICOM) :: LFI
INTEGER (KIND=JPLIKB) KREP, KNUMER, IRANG, IREP, INIMES
!
LOGICAL LDIMST
!
CHARACTER(LEN=LFI%JPLSPX) CLNSPR
CHARACTER(LEN=LFI%JPLMES) CLMESS
CHARACTER(LEN=LFI%JPLFTX) CLACTI
LOGICAL LLFATA

!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('LFIOSF_FORT',0,ZHOOK_HANDLE)
CLACTI=''
CALL LFINUM_FORT                    &
&               (LFI, KNUMER,IRANG)
!
IF (IRANG.NE.0) THEN
   IF (LFI%LMULTI) CALL LFIVER_FORT                              &
&                                  (LFI, LFI%VERRUE(IRANG),'ON')
  LDIMST=LFI%LISTAT(IRANG)
  LFI%NDEROP(IRANG)=20
   IF (LFI%LMULTI) CALL LFIVER_FORT                               &
&                                  (LFI, LFI%VERRUE(IRANG),'OFF')
  IREP=0
ELSE
  IREP=-1
ENDIF
!
KREP=IREP
LLFATA=LLMOER (IREP,IRANG)
!
IF (LLFATA.OR.IXNIMS (IRANG).EQ.2) THEN
  INIMES=2
ELSE
  IF (LHOOK) CALL DR_HOOK('LFIOSF_FORT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='LFIOSF'
WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KNUMER='',I3, &
&       '', LDIMST= '',L1)') KREP,KNUMER,LDIMST
CALL LFIEMS_FORT                                 &
&               (LFI, KNUMER,INIMES,IREP,LLFATA, &
&                CLMESS,CLNSPR,CLACTI)
!
IF (LHOOK) CALL DR_HOOK('LFIOSF_FORT',1,ZHOOK_HANDLE)

CONTAINS

#include "lficom2.ixnims.h"
#include "lficom2.llmoer.h"

END SUBROUTINE LFIOSF_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFIOSF64              &
&           (KREP, KNUMER, LDIMST)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
LOGICAL                LDIMST                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIOSF_FORT                       &
&           (LFI, KREP, KNUMER, LDIMST)

END SUBROUTINE LFIOSF64

SUBROUTINE LFIOSF                &
&           (KREP, KNUMER, LDIMST)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
LOGICAL                LDIMST                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIOSF_MT                        &
&           (LFI, KREP, KNUMER, LDIMST)

END SUBROUTINE LFIOSF

SUBROUTINE LFIOSF_MT                  &
&           (LFI, KREP, KNUMER, LDIMST)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
LOGICAL                LDIMST                                 !   OUT
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)

CALL LFIOSF_FORT                       &
&           (LFI, IREP, INUMER, LDIMST)

KREP       = INT (      IREP, JPLIKM)

END SUBROUTINE LFIOSF_MT

!INTF KREP            OUT 
!INTF KNUMER        IN    
!INTF LDIMST          OUT 
