! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FATALE_FORT                           &
&                     (FA,  KREP, KNUMER, LDERFA )
USE FA_MOD, ONLY : FA_COM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Ce sous-programme permet d'activer ou de desactiver l'option
!     rendant fatale toute erreur detectee sur un fichier particulier,
!     ouvert pour le Logiciel de Fichiers ARPEGE, de meme pour l'option
!     correspondante du logiciel LFI.
!        Cependant, tant que le niveau global d'erreur fatale *FA%NRFAGA*
!     vaut 0 ou 2, l'option propre au fichier est inoperante.
!     *FA%NRFAGA* vaut par defaut 1, et est reglable via le s/p "FANERG".
!**
!     Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                 KNUMER (Entree) ==> Numero d'Unite Logique concernee;
!                 LDERFA (Entree) ==> Option d'Erreur Fatale (Vrai=oui).
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KNUMER
!
INTEGER (KIND=JPLIKB) IRANG, IREP, INIMES
!
LOGICAL LDERFA, LLRLFI
!
CHARACTER(LEN=FA%JPXNOM) CLACTI 
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA

!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FATALE_MT',0,ZHOOK_HANDLE)
CLACTI=''
CALL FANUMU_FORT                 &
&               (FA, KNUMER,IRANG)
!
IF (IRANG.NE.0) THEN
  FA%FICHIER(IRANG)%LERRFA=LDERFA
  CALL LFIERF_FORT                            &
&                 (FA%LFI, IREP,KNUMER,LDERFA)
  LLRLFI=IREP.NE.0
ELSE
  IREP=-51
  LLRLFI=.FALSE.
ENDIF
!
LLFATA=LLMOER (IREP,IRANG)
KREP=IREP
!
IF (LLFATA.OR.IXNVMS (IRANG).EQ.2) THEN
  INIMES=2
ELSE
  IF (LHOOK) CALL DR_HOOK('FATALE_MT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='FATALE'
WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KNUMER='',I3, &
&       '', LDERFA= '',L1)') KREP,KNUMER,LDERFA
CALL FAIPAR_FORT                                     &
&               (FA, KNUMER,INIMES,IREP,LLFATA,CLMESS, &
&             CLNSPR,CLACTI,LLRLFI)
!
IF (LHOOK) CALL DR_HOOK('FATALE_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"
#include "facom2.ixnvms.h"

END SUBROUTINE FATALE_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FATALE64              &
&           (KREP, KNUMER, LDERFA)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
LOGICAL                LDERFA                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FATALE_FORT                     &
&           (FA, KREP, KNUMER, LDERFA)

END SUBROUTINE FATALE64

SUBROUTINE FATALE                &
&           (KREP, KNUMER, LDERFA)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
LOGICAL                LDERFA                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FATALE_MT                       &
&           (FA, KREP, KNUMER, LDERFA)

END SUBROUTINE FATALE

SUBROUTINE FATALE_MT                 &
&           (FA, KREP, KNUMER, LDERFA)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
LOGICAL                LDERFA                                 ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)

CALL FATALE_FORT                     &
&           (FA, IREP, INUMER, LDERFA)

KREP       = INT (      IREP, JPLIKM)

END SUBROUTINE FATALE_MT

!INTF KREP            OUT 
!INTF KNUMER        IN    
!INTF LDERFA        IN    
