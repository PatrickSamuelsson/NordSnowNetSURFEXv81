! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI

SUBROUTINE LFIFMD_FORT               &
&                     (LFI, KFACMD )
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Ce sous-programme permet de changer le Facteur Multiplicatif
!     par Defaut du logiciel de fichiers indexes LFI.
!        Apres appel reussi a ce sous-programme, toute ouverture d'unite
!     logique LFI pour laquelle il n'y a pas de facteur multiplicatif
!     predefini (via *LFIAFM*) se fera en traitant le fichier avec une
!     longueur PHYSIQUE d'article de LFI%JPLARD*KFACMD mots.
!
!        La valeur implicite de ce Facteur Multiplicatif par Defaut est
!     definie dans *LFIINI* ( en l'occurrence, il s'agit de 1 ) .
!**
!       ARGUMENT  : KFACMD (Entree) ==> Facteur Multiplicatif par Defaut
!
!
TYPE(LFICOM) :: LFI
INTEGER (KIND=JPLIKB) KFACMD, INIMES, IREP, INUMER
CHARACTER(LEN=LFI%JPLSPX) CLNSPR
CHARACTER(LEN=LFI%JPLMES) CLMESS
CHARACTER(LEN=LFI%JPLFTX) CLACTI
LOGICAL LLFATA

!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('LFIFMD_FORT',0,ZHOOK_HANDLE)
CLACTI=''
IF (LFI%LFIFMD_LLPREA) THEN
  CALL LFIINI_FORT                 &
&                 (LFI, 2_JPLIKB )
  LFI%LFIFMD_LLPREA=.FALSE.
ENDIF
!
IF (KFACMD.LE.0) THEN
  IREP=-14
ELSEIF (KFACMD.GT.LFI%JPFACX) THEN
  IREP=-28
ELSE
  IREP=0
!
!          Modification, sous Verrouillage Global eventuel.
!
  IF (LFI%LMULTI) CALL LFIVER_FORT                       &
&                                 (LFI, LFI%VERGLA,'ON')
!
  LFI%MFACTU(0)=KFACMD
!
  IF (LFI%LMULTI) CALL LFIVER_FORT                        &
&                                 (LFI, LFI%VERGLA,'OFF')
ENDIF
!
!        MESSAGERIE EVENTUELLE, AVEC ABORT SI NECESSAIRE .
!
LLFATA=IREP.NE.0.AND.LFI%NERFAG.NE.2
!
IF (LLFATA) THEN
  INIMES=2
ELSEIF (IREP.NE.0) THEN
  INIMES=0
ELSEIF (LFI%NIMESG.EQ.0) THEN
  IF (LHOOK) CALL DR_HOOK('LFIFMD_FORT',1,ZHOOK_HANDLE)
  RETURN
ELSE
  INIMES=LFI%NIMESG
ENDIF
!
CLNSPR='LFIFMD'
INUMER=LFI%JPNIL
!
IF (INIMES.EQ.2) THEN
!
  IF (LFI%LFRANC) THEN
    WRITE (UNIT=CLMESS,                              &
&           FMT='(''KFACMD='',I5,'', CODE INTERNE='', &
&           I4)') KFACMD,IREP
  ELSE
    WRITE (UNIT=CLMESS,                               &
&           FMT='(''KFACMD='',I5,'', INTERNAL CODE='', &
&           I4)') KFACMD,IREP
  ENDIF
!
  CALL LFIEMS_FORT                                 &
&                 (LFI, INUMER,INIMES,IREP,LLFATA, &
&                  CLMESS,CLNSPR,CLACTI)
ENDIF
!
IF (INIMES.GE.1) THEN
!
  IF (LFI%LFRANC) THEN
    WRITE (UNIT=CLMESS,FMT=                                        &
&         '(''Reglage du Facteur Multiplicatif par Defaut a'',I3)') &
&         KFACMD
  ELSE
    WRITE (UNIT=CLMESS,FMT=                         &
&         '(''Default Multiply Factor set to'',I3)') &
&         KFACMD
  ENDIF
!
ENDIF
!
CALL LFIEMS_FORT                                 &
&               (LFI, INUMER,INIMES,IREP,LLFATA, &
&                CLMESS,CLNSPR,CLACTI)
!
IF (LHOOK) CALL DR_HOOK('LFIFMD_FORT',1,ZHOOK_HANDLE)
END SUBROUTINE LFIFMD_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFIFMD64           &
&           (KFACMD)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KFACMD                                 ! IN   

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIFMD_FORT            &
&           (LFI, KFACMD)

END SUBROUTINE LFIFMD64

SUBROUTINE LFIFMD             &
&           (KFACMD)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KFACMD                                 ! IN   

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIFMD_MT             &
&           (LFI, KFACMD)

END SUBROUTINE LFIFMD

SUBROUTINE LFIFMD_MT             &
&           (LFI, KFACMD)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
INTEGER (KIND=JPLIKM)  KFACMD                                 ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IFACMD                                 ! IN   
! Convert arguments

IFACMD     = INT (    KFACMD, JPLIKB)

CALL LFIFMD_FORT            &
&           (LFI, IFACMD)


END SUBROUTINE LFIFMD_MT

!INTF KFACMD        IN    
