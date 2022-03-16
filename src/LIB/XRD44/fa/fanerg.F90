! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FANERG_FORT             &
&                     (FA,  KNIVAU )
USE FA_MOD, ONLY : FA_COM, JPNIIL
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Ce sous-programme se charge de mettre le Niveau Global d'Erreur
!     Fatale du logiciel de Fichiers ARPEGE (*FA%NRFAGA*) a la valeur
!     KNIVAU, de meme que la variable correspondante du logiciel LFI.
!     Par defaut, FA%NRFAGA vaut 1.
!**
!        Argument : KNIVAU (Entree) ==> Niveau global d'erreur fatale.
!                                       Valeurs possibles:
!
!     0 : Rendre fatale toute erreur detectee, meme si elle correspond
!         a un fichier ouvert avec l'option "pas d'erreur fatale".
!     1 : Ne rend fatales que certaines erreurs "globales", c'est-a-dire
!         non reliables a un fichier ouvert, et les erreurs sur les fi-
!         chiers ouverts avec l'option "erreur fatale" (Mode par defaut)
!     2 : Passer outre toute erreur detectee, meme si elle correspond
!         a un fichier ouvert avec l'option "erreur fatale".
!         Neanmoins, le code-reponse eventuel ne sera pas zero.
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KNIVAU
!
INTEGER (KIND=JPLIKB) IREP, INIMES, INUMER
CHARACTER(LEN=FA%JPXNOM) CLACTI 
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA

!
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FANERG_MT',0,ZHOOK_HANDLE)
CLACTI=''
IF (FA%FANERG_LLPREA) THEN
  CALL FARINE_FORT              &
&                 (FA, 2_JPLIKB )
  FA%FANERG_LLPREA=.FALSE.
ENDIF
!
IF (KNIVAU.GE.0.AND.KNIVAU.LE.2) THEN
  FA%NRFAGA=KNIVAU
  CALL LFINEG_FORT                &
&                 (FA%LFI, KNIVAU)
  IREP=0
ELSE
  IREP=-52
ENDIF
!
LLFATA=IREP.NE.0.AND.FA%NRFAGA.NE.2
!
IF (LLFATA) THEN
  INIMES=2
ELSEIF (IREP.NE.0) THEN
  INIMES=0
ELSEIF (FA%NIMSGA.EQ.2) THEN
  INIMES=2
ELSE
  IF (LHOOK) CALL DR_HOOK('FANERG_MT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
INUMER=JPNIIL
CLNSPR='FANERG'
!
IF (MAX (INIMES,FA%NIMSGA).EQ.2) THEN
  WRITE (UNIT=CLMESS,                                  &
&         FMT='(''KNIVAU='',I5,'', CODE INTERNE='',I4)' &
&         ) KNIVAU,IREP
ENDIF
!
CALL FAIPAR_FORT                                     &
&               (FA, INUMER,INIMES,IREP,LLFATA,CLMESS, &
&             CLNSPR,CLACTI,.FALSE.)
!
IF (LHOOK) CALL DR_HOOK('FANERG_MT',1,ZHOOK_HANDLE)
END SUBROUTINE FANERG_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FANERG64           &
&           (KNIVAU)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KNIVAU                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FANERG_FORT           &
&           (FA, KNIVAU)

END SUBROUTINE FANERG64

SUBROUTINE FANERG             &
&           (KNIVAU)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KNIVAU                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FANERG_MT             &
&           (FA, KNIVAU)

END SUBROUTINE FANERG

SUBROUTINE FANERG_MT             &
&           (FA, KNIVAU)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KNIVAU                                 ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  INIVAU                                 ! IN   
! Convert arguments

INIVAU     = INT (    KNIVAU, JPLIKB)

CALL FANERG_FORT           &
&           (FA, INIVAU)


END SUBROUTINE FANERG_MT

!INTF KNIVAU        IN    
