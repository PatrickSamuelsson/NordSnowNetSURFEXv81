! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FACTUI_FORT                   &
&                     (FA,  KREP, KRANGC )
USE FA_MOD, ONLY : FA_COM, FREE_CADRE, JPNIIL
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Sous-programme A USAGE INTERNE AU LOGICIEL. Fait la suppression
!     d'un cadre ( vis-a-vis des tables du logiciel ) .
!        En mode multi-taches, il doit y avoir verrouillage global
!     de la zone d'appel au sous-programme.
!**
!        Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                    KRANGC (Entree) ==> Rang du cadre dans les tables.
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KRANGC
!
INTEGER (KIND=JPLIKB) J, IPOSCA, INIMES, INUMER
CHARACTER(LEN=FA%JPXNOM) CLACTI 
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA

!**
!     1.  -  CONTROLE PREALABLE DE COHERENCE.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FACTUI_MT',0,ZHOOK_HANDLE)
CLACTI=''
IF (KRANGC.LE.0.OR.KRANGC.GT.FA%JPNXCA) THEN
  KREP=-66
  GOTO 1001
ENDIF
!**
!     2.  -  RECHERCHE DU CADRE DANS LA TABLE "FA%NCAIND".
!-----------------------------------------------------------------------
!
DO J=1,FA%NCADEF
!
IF (FA%NCAIND(J).EQ.KRANGC) THEN
  IPOSCA=J
  GOTO 202
ENDIF
!
ENDDO
!
KREP=-66
GOTO 1001
!
202 CONTINUE
!
IF (FA%CADRE(KRANGC)%NULCAD.NE.0) THEN
  KREP=-67
  GOTO 1001
ENDIF
!**
!     3.  -  MISE A JOUR DES TABLES.
!-----------------------------------------------------------------------
!
CALL FREE_CADRE (FA%CADRE(KRANGC))

FA%CADRE(KRANGC)%CNOMCA=' '
FA%NCADEF=FA%NCADEF-1
!
DO J=IPOSCA,FA%NCADEF
FA%NCAIND(J)=FA%NCAIND(J+1)
ENDDO

!
KREP=0
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE EVENTUELLE,
!            VIA LE sous-programme "FAIPAR" .
!-----------------------------------------------------------------------
!
1001 CONTINUE
!
LLFATA=KREP.EQ.-66.OR.(KREP.NE.0.AND.FA%NRFAGA.NE.2)
!
IF (FA%LFAMOP.OR.LLFATA) THEN
  INIMES=2
  CLNSPR='FACTUI'
  INUMER=JPNIIL
  WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KRANGC='',I4)') &
&     KREP,KRANGC
  CALL FAIPAR_FORT                                      &
&                 (FA, INUMER,INIMES,KREP,.FALSE.,CLMESS, &
&               CLNSPR,CLACTI,.FALSE.)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('FACTUI_MT',1,ZHOOK_HANDLE)
END SUBROUTINE FACTUI_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FACTUI64           &
&           (KREP, KRANGC)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KRANGC                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FACTUI_FORT             &
&           (FA, KREP, KRANGC)

END SUBROUTINE FACTUI64

SUBROUTINE FACTUI             &
&           (KREP, KRANGC)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KRANGC                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FACTUI_MT               &
&           (FA, KREP, KRANGC)

END SUBROUTINE FACTUI

SUBROUTINE FACTUI_MT             &
&           (FA, KREP, KRANGC)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KRANGC                                 ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  IRANGC                                 ! IN   
! Convert arguments

IRANGC     = INT (    KRANGC, JPLIKB)

CALL FACTUI_FORT             &
&           (FA, IREP, IRANGC)

KREP       = INT (      IREP, JPLIKM)

END SUBROUTINE FACTUI_MT

!INTF KREP            OUT 
!INTF KRANGC        IN    
