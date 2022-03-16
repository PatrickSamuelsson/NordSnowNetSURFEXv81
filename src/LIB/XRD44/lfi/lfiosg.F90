! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI

SUBROUTINE LFIOSG_FORT               &
&                     (LFI, KNIVAU )
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Ce sous-programme permet d'obtenir la valeur courante du NIVEAU
!     GLOBAL d'IMPRESSION des STATISTIQUES concernant les fichiers
!     manipules par le logiciel LFI, lors de leur fermeture.
!**
!        ARGUMENT : KNIVAU (Sortie) ==> Niveau global d'impression des
!                                       statistiques a la fermeture.
!
!     Valeurs possibles (par defaut, 1):
!
!     0 : Dans ce cas, on n'imprime pas de statistiques lors de la fer-
!         meture d'un fichier, meme si l'option individuelle du fichier
!         concerne est a .TRUE. .
!
!     1 : Respect de l'option individuelle du fichier.
!
!     2 : Dans ce cas, force l'impression des statistiques a la ferme-
!         ture d'un fichier, meme si l'option individuelle du fichier
!         est a .FALSE. .
!
!
TYPE(LFICOM) :: LFI
INTEGER (KIND=JPLIKB) KNIVAU, INIMES, IREP, INUMER
CHARACTER(LEN=LFI%JPLSPX) CLNSPR
CHARACTER(LEN=LFI%JPLMES) CLMESS
CHARACTER(LEN=LFI%JPLFTX) CLACTI

!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('LFIOSG_FORT',0,ZHOOK_HANDLE)
CLACTI=''
IF (LFI%LFIOSG_LLPREA) THEN
  CALL LFIINI_FORT                 &
&                 (LFI, 2_JPLIKB )
  LFI%LFIOSG_LLPREA=.FALSE.
ENDIF
!
KNIVAU=LFI%NISTAG
INIMES=LFI%NIMESG
!
IF (INIMES.EQ.2) THEN
  IREP=0
  INUMER=LFI%JPNIL
  CLNSPR='LFIOSG'
  WRITE (UNIT=CLMESS,FMT='(''KNIVAU='',I2)') KNIVAU
  CALL LFIEMS_FORT                                  &
&                 (LFI, INUMER,INIMES,IREP,.FALSE., &
&                  CLMESS,CLNSPR,CLACTI)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('LFIOSG_FORT',1,ZHOOK_HANDLE)
END SUBROUTINE LFIOSG_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFIOSG64           &
&           (KNIVAU)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KNIVAU                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIOSG_FORT            &
&           (LFI, KNIVAU)

END SUBROUTINE LFIOSG64

SUBROUTINE LFIOSG             &
&           (KNIVAU)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KNIVAU                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIOSG_MT             &
&           (LFI, KNIVAU)

END SUBROUTINE LFIOSG

SUBROUTINE LFIOSG_MT             &
&           (LFI, KNIVAU)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
INTEGER (KIND=JPLIKM)  KNIVAU                                 !   OUT
! Local integers
INTEGER (KIND=JPLIKB)  INIVAU                                 !   OUT
! Convert arguments


CALL LFIOSG_FORT            &
&           (LFI, INIVAU)

KNIVAU     = INT (    INIVAU, JPLIKM)

END SUBROUTINE LFIOSG_MT

!INTF KNIVAU          OUT 
