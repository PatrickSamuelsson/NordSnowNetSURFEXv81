! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FALIMU_FORT                                     &
&                     (FA,  KXNIVV, KXTRON, KXLATI, KXLONG )
USE FA_MOD, ONLY : FA_COM, JPNIIL
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Sous-programme servant a obtenir les valeurs courantes
!     des LIMites Utilisateur en termes de Resolutions horizontale
!     et verticale, valides globalement pour tous les Fichiers et Cadres
!     ARPEGE manipules par le programme utilisateur.
!**
!        Arguments : KXNIVV ==> Nombre maximum de niveaux verticaux;
!  (tous de Sortie)  KXTRON ==> Troncature maximum;
!                    KXLATI ==> Nombre maximum de latitudes pole a pole;
!                    KXLONG ==> Nombre maxi de longitudes par parallele.
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KXNIVV, KXTRON, KXLATI, KXLONG
!
INTEGER (KIND=JPLIKB) INUMER, INIMES, IREP
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
!     1.  -  SI PREMIERE UTILISATION, APPEL AU SOUS-PROGRAMME "FARINE".
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FALIMU_MT',0,ZHOOK_HANDLE)
CLACTI=''
IF (FA%FALIMU_LLPREA) THEN
  CALL FARINE_FORT              &
&                 (FA, 2_JPLIKB )
  FA%FALIMU_LLPREA=.FALSE.
ENDIF
!
!             Verrouillage global, si necessaire.
!
IF (FA%LFAMUL) CALL LFIVER_FORT                        &
&                              (FA%LFI, FA%VRGLAS,'ON')
LLVERG=FA%LFAMUL
!**
!     2.  -  RECOPIE DES VALEURS EN COMMON DANS LES ARGUMENTS.
!-----------------------------------------------------------------------
!
KXNIVV=FA%NXNIVV
KXTRON=FA%NXTRON
KXLATI=FA%NXLATI
KXLONG=FA%NXLONG
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE, AVEC "ABORT" EVENTUEL,
!            VIA LE sous-programme "FAIPAR" .
!-----------------------------------------------------------------------
!
!
!          Deverrouillage global eventuel.
!
IF (LLVERG) CALL LFIVER_FORT                         &
&                           (FA%LFI, FA%VRGLAS,'OFF')
!
IF (FA%NIMSGA.EQ.2) THEN
  IREP=0
  INIMES=2
  CLNSPR='FALIMU'
  INUMER=JPNIIL
  WRITE (UNIT=CLMESS,FMT='(''KXNIVV='',I4,'', KXTRON='',I4, &
&         '', KXLATI='',I4,'', KXLONG='',I4)')               &
&     KXNIVV,KXTRON,KXLATI,KXLONG
  CALL FAIPAR_FORT                                     &
&                 (FA, INUMER,INIMES,IREP,LLFATA,CLMESS, &
&               CLNSPR,CLACTI,.FALSE.)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('FALIMU_MT',1,ZHOOK_HANDLE)
END SUBROUTINE FALIMU_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FALIMU64                        &
&           (KXNIVV, KXTRON, KXLATI, KXLONG)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KXNIVV                                 !   OUT
INTEGER (KIND=JPLIKB)  KXTRON                                 !   OUT
INTEGER (KIND=JPLIKB)  KXLATI                                 !   OUT
INTEGER (KIND=JPLIKB)  KXLONG                                 !   OUT

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FALIMU_FORT                               &
&           (FA, KXNIVV, KXTRON, KXLATI, KXLONG)

END SUBROUTINE FALIMU64

SUBROUTINE FALIMU                          &
&           (KXNIVV, KXTRON, KXLATI, KXLONG)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KXNIVV                                 !   OUT
INTEGER (KIND=JPLIKM)  KXTRON                                 !   OUT
INTEGER (KIND=JPLIKM)  KXLATI                                 !   OUT
INTEGER (KIND=JPLIKM)  KXLONG                                 !   OUT

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FALIMU_MT                                 &
&           (FA, KXNIVV, KXTRON, KXLATI, KXLONG)

END SUBROUTINE FALIMU

SUBROUTINE FALIMU_MT                           &
&           (FA, KXNIVV, KXTRON, KXLATI, KXLONG)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KXNIVV                                 !   OUT
INTEGER (KIND=JPLIKM)  KXTRON                                 !   OUT
INTEGER (KIND=JPLIKM)  KXLATI                                 !   OUT
INTEGER (KIND=JPLIKM)  KXLONG                                 !   OUT
! Local integers
INTEGER (KIND=JPLIKB)  IXNIVV                                 !   OUT
INTEGER (KIND=JPLIKB)  IXTRON                                 !   OUT
INTEGER (KIND=JPLIKB)  IXLATI                                 !   OUT
INTEGER (KIND=JPLIKB)  IXLONG                                 !   OUT
! Convert arguments


CALL FALIMU_FORT                               &
&           (FA, IXNIVV, IXTRON, IXLATI, IXLONG)

KXNIVV     = INT (    IXNIVV, JPLIKM)
KXTRON     = INT (    IXTRON, JPLIKM)
KXLATI     = INT (    IXLATI, JPLIKM)
KXLONG     = INT (    IXLONG, JPLIKM)

END SUBROUTINE FALIMU_MT

!INTF KXNIVV          OUT 
!INTF KXTRON          OUT 
!INTF KXLATI          OUT 
!INTF KXLONG          OUT 
