! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FAMISO_FORT             &
&                     (FA,  LDEBUG )
USE FA_MOD, ONLY : FA_COM, JPNIIL
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Ce sous-programme permet d'activer ou de desactiver le mode
!     "Mise au point du logiciel". ( par defaut, inactif )
!        A noter que le mode "mise au point" du logiciel LFI n'est pas
!     modifie.
!**
!        Argument : LDEBUG (Entree) ==> Vrai si on doit activer ce mode.
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) IREP, INUMER, INIMES
!
LOGICAL LDEBUG
CHARACTER(LEN=FA%JPXNOM) CLACTI 
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR

!
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FAMISO_MT',0,ZHOOK_HANDLE)
CLACTI=''
IF (FA%FAMISO_LLPREA) THEN
  CALL FARINE_FORT              &
&                 (FA, 2_JPLIKB )
  FA%FAMISO_LLPREA=.FALSE.
ENDIF
!
FA%LFAMOP=LDEBUG
!
!  Prise en compte du niveau de messagerie dans GRIBEX
!
!IF (FA%LFAMOP) THEN
!  CALL GRSDBG(1)
!ELSE
!  CALL GRSDBG(0)
!ENDIF
!
!        MESSAGERIE EVENTUELLE .
!
IF (FA%NIMSGA.NE.2)  THEN 
  IF (LHOOK) CALL DR_HOOK('FAMISO_MT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
INUMER=JPNIIL
INIMES=2
IREP=0
CLNSPR='FAMISO'
WRITE (UNIT=CLMESS,FMT='(''LDEBUG= '',L1)') LDEBUG
CALL FAIPAR_FORT                                      &
&               (FA, INUMER,INIMES,IREP,.FALSE.,CLMESS, &
&                CLNSPR,CLACTI,.FALSE.)
!
IF (LHOOK) CALL DR_HOOK('FAMISO_MT',1,ZHOOK_HANDLE)
END SUBROUTINE FAMISO_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FAMISO64           &
&           (LDEBUG)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
LOGICAL                LDEBUG                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAMISO_FORT           &
&           (FA, LDEBUG)

END SUBROUTINE FAMISO64

SUBROUTINE FAMISO             &
&           (LDEBUG)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
LOGICAL                LDEBUG                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAMISO_MT             &
&           (FA, LDEBUG)

END SUBROUTINE FAMISO

SUBROUTINE FAMISO_MT             &
&           (FA, LDEBUG)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
LOGICAL                LDEBUG                                 ! IN   
! Local integers
! Convert arguments


CALL FAMISO_FORT           &
&           (FA, LDEBUG)


END SUBROUTINE FAMISO_MT

!INTF LDEBUG        IN    
