! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI

SUBROUTINE LFIDAH_FORT                      &
&                     (LFI, KDATE, KHEURE )
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!     SOUS-PROGRAMME CHARGE DE DONNER LA DATE ET L'HEURE MACHINE
!     SOUS LA FORME D'ENTIERS .
!**
!     ARGUMENTS (SORTIE) KDATE  ==> DATE SOUS LA FORME AAMMJJ;
!                        KHEURE ==> HEURE SOUS LA FORME HHMMSS .
!
TYPE(LFICOM) :: LFI
CHARACTER (LEN=8) CLAUXI
!
INTEGER (KIND=JPLIKB) KDATE, KHEURE
CHARACTER (LEN=10) CLAUXIT
CHARACTER (LEN=5)  CLDIFF
INTEGER (KIND=JPLIKB) KTIME(8)
INTEGER (KIND=JPLIKB) IMOIS, IJOUR, IANNEE
INTEGER (KIND=JPLIKB) IHEURE, IMINUT, ISECON
INTEGER (KIND=JPLIKM) ITIME(8)
!

REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('LFIDAH_FORT',0,ZHOOK_HANDLE)
CALL DATE_AND_TIME (CLAUXI,CLAUXIT,CLDIFF,ITIME)
KTIME(1:8)=INT (ITIME(1:8), JPLIKB)
IANNEE=KTIME(1)
IMOIS=KTIME(2)
IJOUR=KTIME(3)
IHEURE=KTIME(5)
IMINUT=KTIME(6)
ISECON=KTIME(7)
!
KDATE=100*(100*IANNEE+IMOIS)+IJOUR
KHEURE=100*(100*IHEURE+IMINUT)+ISECON
!
IF (LHOOK) CALL DR_HOOK('LFIDAH_FORT',1,ZHOOK_HANDLE)
END SUBROUTINE LFIDAH_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFIDAH64           &
&           (KDATE, KHEURE)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KDATE                                  !   OUT
INTEGER (KIND=JPLIKB)  KHEURE                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIDAH_FORT               &
&           (LFI, KDATE, KHEURE)

END SUBROUTINE LFIDAH64

SUBROUTINE LFIDAH             &
&           (KDATE, KHEURE)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KDATE                                  !   OUT
INTEGER (KIND=JPLIKM)  KHEURE                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIDAH_MT                 &
&           (LFI, KDATE, KHEURE)

END SUBROUTINE LFIDAH

SUBROUTINE LFIDAH_MT             &
&           (LFI, KDATE, KHEURE)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
INTEGER (KIND=JPLIKM)  KDATE                                  !   OUT
INTEGER (KIND=JPLIKM)  KHEURE                                 !   OUT
! Local integers
INTEGER (KIND=JPLIKB)  IDATE                                  !   OUT
INTEGER (KIND=JPLIKB)  IHEURE                                 !   OUT
! Convert arguments


CALL LFIDAH_FORT               &
&           (LFI, IDATE, IHEURE)

KDATE      = INT (     IDATE, JPLIKM)
KHEURE     = INT (    IHEURE, JPLIKM)

END SUBROUTINE LFIDAH_MT

!INTF KDATE           OUT 
!INTF KHEURE          OUT 
