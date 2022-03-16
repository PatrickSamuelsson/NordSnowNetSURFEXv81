! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI

SUBROUTINE LFIOEG_FORT               &
&                     (LFI, KNIVAU )
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Ce sous-programme permet d'obtenir la valeur courante du NIVEAU
!      GLOBAL de traitement des ERREURS detectees par le logiciel LFI.
!**
!        ARGUMENT : KNIVAU (Sortie) ==> Niveau global de traitement des
!                                       erreurs detectees.
!
!     Valeurs possibles (par defaut, 1):
!
!     0 : Dans ce cas, toute erreur detectee sera fatale, meme si
!         l'erreur joue sur un fichier dont l'option individuelle de
!         traitement des erreurs est a .FALSE. .
!
!     1 : Seules les erreurs "globales" (c'est-a-dire non reliables a un
!         fichier deja ouvert) et les erreurs reliees a un fichier dont
!         l'option individuelle de traitement est .TRUE. seront fatales.
!
!     2 : Passer outre toute erreur detectee, meme si elle correspond
!         a un fichier dont l'option individuelle de traitement est
!         .TRUE. ; noter que dans ce dernier cas le code-reponse ne
!         sera pas nul. Par ailleurs, le code-reponse (-16) echappe a
!         ce mode de controle et est toujours fatal.
!
!
TYPE(LFICOM) :: LFI
INTEGER (KIND=JPLIKB) KNIVAU, INIMES, IREP, INUMER
CHARACTER(LEN=LFI%JPLSPX) CLNSPR
CHARACTER(LEN=LFI%JPLMES) CLMESS
CHARACTER(LEN=LFI%JPLFTX) CLACTI

!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('LFIOEG_FORT',0,ZHOOK_HANDLE)
CLACTI=''
IF (LFI%LFIOEG_LLPREA) THEN
  CALL LFIINI_FORT                 &
&                 (LFI, 2_JPLIKB )
  LFI%LFIOEG_LLPREA=.FALSE.
ENDIF
!
KNIVAU=LFI%NERFAG
INIMES=LFI%NIMESG
!
IF (INIMES.EQ.2) THEN
  IREP=0
  INUMER=LFI%JPNIL
  CLNSPR='LFIOEG'
  WRITE (UNIT=CLMESS,FMT='(''KNIVAU='',I2)') KNIVAU
  CALL LFIEMS_FORT                                  &
&                 (LFI, INUMER,INIMES,IREP,.FALSE., &
&                  CLMESS,CLNSPR,CLACTI)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('LFIOEG_FORT',1,ZHOOK_HANDLE)
END SUBROUTINE LFIOEG_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFIOEG64           &
&           (KNIVAU)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KNIVAU                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIOEG_FORT            &
&           (LFI, KNIVAU)

END SUBROUTINE LFIOEG64

SUBROUTINE LFIOEG             &
&           (KNIVAU)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KNIVAU                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIOEG_MT             &
&           (LFI, KNIVAU)

END SUBROUTINE LFIOEG

SUBROUTINE LFIOEG_MT             &
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


CALL LFIOEG_FORT            &
&           (LFI, INIVAU)

KNIVAU     = INT (    INIVAU, JPLIKM)

END SUBROUTINE LFIOEG_MT

!INTF KNIVAU          OUT 
