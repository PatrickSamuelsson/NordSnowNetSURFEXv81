! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FANUCA_FORT                             &
&                     (FA,  CDNOMC, KRANGC, LDVERR )
USE FA_MOD, ONLY : FA_COM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Ce sous-programme calcule le RANG du Cadre de NOM
!     *CDNOMC* dans la table des noms de cadres *FA%CNOMCA*;
!     S'IL N'Y EST PAS TROUVE, LE RESULTAT EST ZERO.
!        Ce sous-programme, appele par plusieurs sous-programmes
!     du Logiciel de Fichiers ARPEGE, se charge lors de son
!     premier appel d'appeler le sous-programme preparatoire FARINE.
!**
!       Arguments : CDNOMC (Entree) ==> Nom de cadre cherche;
!                   KRANGC (Sortie) ==> Rang dans la table des cadres
!                                       du logiciel FA (0 si absent);
!                   LDVERR (Entree) ==> Sert, en mode multi-taches
!                                       seulement, a savoir si l'on doit
!                                       verrouiller globalement ou pas.
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KRANGC
!
INTEGER (KIND=JPLIKB) J, IRESUL
!
CHARACTER CDNOMC*(*)
!
LOGICAL LDVERR, LLVERG
!
!

!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FANUCA_MT',0,ZHOOK_HANDLE)
IF (FA%FANUCA_LLPREA) THEN
  CALL FARINE_FORT              &
&                 (FA, 2_JPLIKB )
  FA%FANUCA_LLPREA=.FALSE.
ENDIF
!
!          VERROUILLAGE GLOBAL (A CAUSE DE L'UTILISATION DE FA%NFIOUV )
!
LLVERG=FA%LFAMUL.AND.LDVERR
IF (LLVERG) CALL LFIVER_FORT                        &
&                           (FA%LFI, FA%VRGLAS,'ON')
!
DO J=1,FA%NCADEF
!
IF (CDNOMC.EQ.FA%CADRE(FA%NCAIND(J))%CNOMCA) THEN
  IRESUL=FA%NCAIND(J)
  GOTO 20
ENDIF
!
ENDDO
!
IRESUL=0
!
20 CONTINUE
!
!          DEVERROUILLAGE GLOBAL
!
IF (LLVERG) CALL LFIVER_FORT                         &
&                           (FA%LFI, FA%VRGLAS,'OFF')
KRANGC=IRESUL
!
IF (LHOOK) CALL DR_HOOK('FANUCA_MT',1,ZHOOK_HANDLE)
END SUBROUTINE FANUCA_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FANUCA64                &
&           (CDNOMC, KRANGC, LDVERR)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
CHARACTER (LEN=*)      CDNOMC                                 ! IN   
INTEGER (KIND=JPLIKB)  KRANGC                                 !   OUT
LOGICAL                LDVERR                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FANUCA_FORT                       &
&           (FA, CDNOMC, KRANGC, LDVERR)

END SUBROUTINE FANUCA64

SUBROUTINE FANUCA                  &
&           (CDNOMC, KRANGC, LDVERR)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
CHARACTER (LEN=*)      CDNOMC                                 ! IN   
INTEGER (KIND=JPLIKM)  KRANGC                                 !   OUT
LOGICAL                LDVERR                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FANUCA_MT                         &
&           (FA, CDNOMC, KRANGC, LDVERR)

END SUBROUTINE FANUCA

SUBROUTINE FANUCA_MT                   &
&           (FA, CDNOMC, KRANGC, LDVERR)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
CHARACTER (LEN=*)      CDNOMC                                 ! IN   
INTEGER (KIND=JPLIKM)  KRANGC                                 !   OUT
LOGICAL                LDVERR                                 ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IRANGC                                 !   OUT
! Convert arguments


CALL FANUCA_FORT                       &
&           (FA, CDNOMC, IRANGC, LDVERR)

KRANGC     = INT (    IRANGC, JPLIKM)

END SUBROUTINE FANUCA_MT

!INTF CDNOMC        IN    
!INTF KRANGC          OUT 
!INTF LDVERR        IN    
