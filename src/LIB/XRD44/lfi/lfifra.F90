! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI

SUBROUTINE LFIFRA_FORT               &
&                     (LFI, LDFRAN )
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Ce Sous-Programme permet aux messages (ulterieurs) emis par le
!     logiciel LFI d'etre rediges en FRANCAIS ou en ANGLAIS.
!        L'option par defaut est definie dans le sous-programme *LFIINI*
!
!        This Subroutine enables (further) messages from LFI software
!     to be written in FRENCH or ENGLISH. Default mode is defined in
!     routine *LFIINI* .
!**
!        ARGUMENT : LDFRAN (Entree) ==> Vrai/Faux pour francais/anglais.
!                           Input       True/False for french/english
!
TYPE(LFICOM) :: LFI
INTEGER (KIND=JPLIKB) INIMES, IREP, INUMER
!
LOGICAL LDFRAN
CHARACTER(LEN=LFI%JPLSPX) CLNSPR
CHARACTER(LEN=LFI%JPLMES) CLMESS
CHARACTER(LEN=LFI%JPLFTX) CLACTI

!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('LFIFRA_FORT',0,ZHOOK_HANDLE)
CLACTI=''
IF (LFI%LFIFRA_LLPREA) THEN
!
!        Au premier appel du sous-programme, on initialise les variables
!     globales du logiciel (si cela n'a pas deja ete fait) .
!
!        At first routine call, initialisation of software global
!     variables (if not already done) .
!
  CALL LFIINI_FORT                 &
&                 (LFI, 2_JPLIKB )
  LFI%LFIFRA_LLPREA=.FALSE.
ENDIF
!
LFI%LFRANC=LDFRAN
!
!        MESSAGERIE EVENTUELLE . MESSAGE, IF NECESSARY .
!
IF (LFI%NIMESG.NE.2)  THEN 
  IF (LHOOK) CALL DR_HOOK('LFIFRA_FORT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
INUMER=LFI%JPNIL
INIMES=2
IREP=0
CLNSPR='LFIFRA'
WRITE (UNIT=CLMESS,FMT='(''LDFRAN= '',L1)') LDFRAN
CALL LFIEMS_FORT                                         &
&               (LFI, INUMER,INIMES,IREP,.FALSE.,CLMESS, &
&                CLNSPR,CLACTI)
!
IF (LHOOK) CALL DR_HOOK('LFIFRA_FORT',1,ZHOOK_HANDLE)
END SUBROUTINE LFIFRA_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFIFRA64           &
&           (LDFRAN)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
LOGICAL                LDFRAN                                 ! IN   

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIFRA_FORT            &
&           (LFI, LDFRAN)

END SUBROUTINE LFIFRA64

SUBROUTINE LFIFRA             &
&           (LDFRAN)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
LOGICAL                LDFRAN                                 ! IN   

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIFRA_MT             &
&           (LFI, LDFRAN)

END SUBROUTINE LFIFRA

SUBROUTINE LFIFRA_MT             &
&           (LFI, LDFRAN)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
LOGICAL                LDFRAN                                 ! IN   
! Local integers
! Convert arguments


CALL LFIFRA_FORT            &
&           (LFI, LDFRAN)


END SUBROUTINE LFIFRA_MT

!INTF LDFRAN        IN    
