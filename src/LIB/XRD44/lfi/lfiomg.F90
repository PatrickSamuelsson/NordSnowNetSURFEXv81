! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI

SUBROUTINE LFIOMG_FORT               &
&                     (LFI, KNIVAU, KULOUT)
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Ce sous-programme permet d'obtenir la valeur courante du NIVEAU
!      GLOBAL d'IMPRESSION des MESSAGES emis par le logiciel LFI.
!**
!        ARGUMENT : KNIVAU (Sortie) ==> Niveau global de messagerie.
!
!     Valeurs possibles (par defaut, 1):
!
!     0 : N'emettre que les messages d'erreurs reellement importants,
!         en pratique ceux relies a une erreur fatale. Le niveau indivi-
!         duel de messagerie des fichiers est alors inoperant.
!
!     1 : Seuls quelques messages "globaux" (c'est-a-dire non reliables
!         a un fichier deja ouvert) et les messages lies a un fichier
!         ouvert, messages dont le niveau est au plus egal au niveau de
!         messagerie individuelle du fichier.
!
!     2 : Emettre tous les messages possibles (donc jusqu'au niveau 2,
!         mais pas ceux lies au mode "mise au point"), meme si
!         ces messages concernent un fichier dont le niveau individuel
!         de messagerie est inferieur a 2.
!
!
TYPE(LFICOM) :: LFI
INTEGER (KIND=JPLIKB) KNIVAU, KULOUT, INIMES, IREP, INUMER
CHARACTER(LEN=LFI%JPLSPX) CLNSPR
CHARACTER(LEN=LFI%JPLMES) CLMESS
CHARACTER(LEN=LFI%JPLFTX) CLACTI

!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('LFIOMG_FORT',0,ZHOOK_HANDLE)
CLACTI=''
IF (LFI%LFIOMG_LLPREA) THEN
  CALL LFIINI_FORT                 &
&                 (LFI, 2_JPLIKB )
  LFI%LFIOMG_LLPREA=.FALSE.
ENDIF
!
KNIVAU=LFI%NIMESG
INIMES=LFI%NIMESG
KULOUT=LFI%NULOUT
!
IF (INIMES.EQ.2) THEN
  IREP=0
  INUMER=LFI%JPNIL
  CLNSPR='LFIOMG'
  WRITE (UNIT=CLMESS,FMT='(''KNIVAU='',I2)') KNIVAU
  CALL LFIEMS_FORT                                         &
&                 (LFI, INUMER,INIMES,IREP,.FALSE.,CLMESS, &
&                  CLNSPR,CLACTI)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('LFIOMG_FORT',1,ZHOOK_HANDLE)
END SUBROUTINE LFIOMG_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFIOMG64           &
&           (KNIVAU, KULOUT)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KNIVAU                                 !   OUT
INTEGER (KIND=JPLIKB)  KULOUT                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIOMG_FORT            &
&           (LFI, KNIVAU, KULOUT)

END SUBROUTINE LFIOMG64

SUBROUTINE LFIOMG             &
&           (KNIVAU, KULOUT)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KNIVAU                                 !   OUT
INTEGER (KIND=JPLIKM)  KULOUT                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIOMG_MT             &
&           (LFI, KNIVAU, KULOUT)

END SUBROUTINE LFIOMG

SUBROUTINE LFIOMG_MT             &
&           (LFI, KNIVAU, KULOUT)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
INTEGER (KIND=JPLIKM)  KNIVAU                                 !   OUT
INTEGER (KIND=JPLIKM)  KULOUT                                 !   OUT
! Local integers
INTEGER (KIND=JPLIKB)  INIVAU                                 !   OUT
INTEGER (KIND=JPLIKB)  IULOUT                                 !   OUT
! Convert arguments


CALL LFIOMG_FORT            &
&           (LFI, INIVAU, IULOUT)

KNIVAU     = INT (    INIVAU, JPLIKM)
KULOUT     = INT (    IULOUT, JPLIKM)

END SUBROUTINE LFIOMG_MT

!INTF KNIVAU          OUT 
!INTF KULOUT          OUT 
