! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FARFLU_FORT                                     &
&                     (FA,  KXNIVV, KXTRON, KXLATI, KXLONG )
USE FA_MOD, ONLY : FA_COM, JPNIIL
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Sous-programme servant a specifier des Limites Utilisateur
!     en termes de Resolutions horizontale et verticale, valides
!     globalement pour tous les Fichiers et Cadres ARPEGE.
!            ( Resolution Fichiers - Limites Utilisateur )
!**
!        Arguments : KXNIVV ==> Nombre maximum de niveaux verticaux;
!  (tous d'Entree)   KXTRON ==> Troncature maximum;
!                    KXLATI ==> Nombre maximum de latitudes pole a pole;
!                    KXLONG ==> Nombre maxi de longitudes par parallele.
!*
!        S'il y a des cadres deja definis (dynamiquement ou non) avec
!     des valeurs correspondantes plus grandes, cela provoque une erreur
!     globale.
!        Si les valeurs donnees en argument depassent les limites cor-
!     repondantes du logiciel, elles sont ecretees, avec un message.
!        Une messagerie de niveau 1 est emise dans le cas normal ou
!     ci-dessus.
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KXNIVV, KXTRON, KXLATI, KXLONG
!
INTEGER (KIND=JPLIKB) INUMER, INIMES, IREP, IMINIM
INTEGER (KIND=JPLIKB) IXNIVV, IXTRON, IXLATI, J
INTEGER (KIND=JPLIKB) IXLONG, IRANGC
!
LOGICAL LLDEPA, LLVERG
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
IF (LHOOK) CALL DR_HOOK('FARFLU_MT',0,ZHOOK_HANDLE)
CLACTI=''
IF (FA%FARFLU_LLPREA) THEN
  CALL FARINE_FORT              &
&                 (FA, 2_JPLIKB )
  FA%FARFLU_LLPREA=.FALSE.
ENDIF
!**
!     2.  -  CONTROLES.
!-----------------------------------------------------------------------
!
IMINIM=MIN (KXNIVV,KXTRON,KXLATI,KXLONG)
!
IF (IMINIM.LE.0) THEN
  LLVERG=.FALSE.
  LLDEPA=.FALSE.
  IREP=-64
  GOTO 1001
ELSE
  LLDEPA=KXNIVV.GT.FA%JPXNIV.OR.KXTRON.GT.FA%JPXTRO.OR. &
&         KXLATI.GT.FA%JPXLAT.OR.KXLONG.GT.FA%JPXLON
  IXNIVV=MIN (KXNIVV,FA%JPXNIV)
  IXTRON=MIN (KXTRON,FA%JPXTRO)
  IXLATI=MIN (KXLATI,FA%JPXLAT)
  IXLONG=MIN (KXLONG,FA%JPXLON)
ENDIF
!
!             Verrouillage global, si necessaire.
!
IF (FA%LFAMUL) CALL LFIVER_FORT                        &
&                              (FA%LFI, FA%VRGLAS,'ON')
LLVERG=FA%LFAMUL
!
IF (IXTRON.LE.FA%NSTROI) THEN
  IREP=-99
  GOTO 1001
ENDIF
!
!        Controles vis-a-vis des cadres deja definis, s'il y en a.
!
DO J=1,FA%NCADEF
IRANGC=FA%NCAIND(J)
!
IF (FA%CADRE(IRANGC)%MTRONC.GT.IXTRON) THEN
  IREP=-104
  GOTO 1001
ELSEIF (FA%CADRE(IRANGC)%NNIVER.GT.IXNIVV) THEN
  IREP=-105
  GOTO 1001
ELSEIF (FA%CADRE(IRANGC)%NLATIT.GT.IXLATI) THEN
  IREP=-106
  GOTO 1001
ELSEIF (FA%CADRE(IRANGC)%NXLOPA.GT.IXLONG) THEN
  IREP=-107
  GOTO 1001
ENDIF
!
ENDDO
!**
!     3.  -  MODIFICATION DES LIMITES USAGER.
!-----------------------------------------------------------------------
!
FA%NXNIVV=IXNIVV
FA%NXTRON=IXTRON
FA%NXLATI=IXLATI
FA%NXLONG=IXLONG
IREP=0
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE, AVEC "ABORT" EVENTUEL,
!            VIA LE sous-programme "FAIPAR" .
!-----------------------------------------------------------------------
!
1001 CONTINUE
!
!          Deverrouillage global eventuel.
!
IF (LLVERG) CALL LFIVER_FORT                         &
&                           (FA%LFI, FA%VRGLAS,'OFF')
!
LLFATA=IREP.NE.0.AND.FA%NRFAGA.NE.2
!
IF (LLFATA) THEN
  INIMES=2
ELSEIF (FA%NIMSGA.EQ.0) THEN
  IF (LHOOK) CALL DR_HOOK('FARFLU_MT',1,ZHOOK_HANDLE)
  RETURN
ELSE
  INIMES=FA%NIMSGA
ENDIF
!
CLNSPR='FARFLU'
INUMER=JPNIIL
!
IF (INIMES.EQ.2) THEN
  WRITE (UNIT=CLMESS,FMT='(''KXNIVV='',I4,'', KXTRON='',I4, &
&         '', KXLATI='',I4,'', KXLONG='',I4)')               &
&     KXNIVV,KXTRON,KXLATI,KXLONG
  CALL FAIPAR_FORT                                     &
&                 (FA, INUMER,INIMES,IREP,LLFATA,CLMESS, &
&               CLNSPR,CLACTI,.FALSE.)
ENDIF
!
IF (INIMES.GE.1) THEN
!
  IF (LLDEPA) THEN
    WRITE (UNIT=CLMESS,FMT=                                       &
&   '(''LIMITES USAGER (***ECRETEES***): FA%NXNIVV='',I4,          &
&   '', FA%NXTRON='',I4,'', FA%NXLATI='',I4,'', FA%NXLONG='',I4)') &
&     FA%NXNIVV,FA%NXTRON,FA%NXLATI,FA%NXLONG
  ELSE
    WRITE (UNIT=CLMESS,FMT=                                        &
&    '(''LIMITES USAGER EFFECTIVES: FA%NXNIVV='',I4,                &
&    '', FA%NXTRON='',I4,'', FA%NXLATI='',I4,'', FA%NXLONG='',I4)') &
&     FA%NXNIVV,FA%NXTRON,FA%NXLATI,FA%NXLONG
  ENDIF
!
  CALL FAIPAR_FORT                                      &
&                 (FA, INUMER,INIMES,IREP,.FALSE.,CLMESS, &
&               CLNSPR,CLACTI,.FALSE.)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('FARFLU_MT',1,ZHOOK_HANDLE)
END SUBROUTINE FARFLU_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FARFLU64                        &
&           (KXNIVV, KXTRON, KXLATI, KXLONG)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KXNIVV                                 ! IN   
INTEGER (KIND=JPLIKB)  KXTRON                                 ! IN   
INTEGER (KIND=JPLIKB)  KXLATI                                 ! IN   
INTEGER (KIND=JPLIKB)  KXLONG                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FARFLU_FORT                               &
&           (FA, KXNIVV, KXTRON, KXLATI, KXLONG)

END SUBROUTINE FARFLU64

SUBROUTINE FARFLU                          &
&           (KXNIVV, KXTRON, KXLATI, KXLONG)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KXNIVV                                 ! IN   
INTEGER (KIND=JPLIKM)  KXTRON                                 ! IN   
INTEGER (KIND=JPLIKM)  KXLATI                                 ! IN   
INTEGER (KIND=JPLIKM)  KXLONG                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FARFLU_MT                                 &
&           (FA, KXNIVV, KXTRON, KXLATI, KXLONG)

END SUBROUTINE FARFLU

SUBROUTINE FARFLU_MT                           &
&           (FA, KXNIVV, KXTRON, KXLATI, KXLONG)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KXNIVV                                 ! IN   
INTEGER (KIND=JPLIKM)  KXTRON                                 ! IN   
INTEGER (KIND=JPLIKM)  KXLATI                                 ! IN   
INTEGER (KIND=JPLIKM)  KXLONG                                 ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IXNIVV                                 ! IN   
INTEGER (KIND=JPLIKB)  IXTRON                                 ! IN   
INTEGER (KIND=JPLIKB)  IXLATI                                 ! IN   
INTEGER (KIND=JPLIKB)  IXLONG                                 ! IN   
! Convert arguments

IXNIVV     = INT (    KXNIVV, JPLIKB)
IXTRON     = INT (    KXTRON, JPLIKB)
IXLATI     = INT (    KXLATI, JPLIKB)
IXLONG     = INT (    KXLONG, JPLIKB)

CALL FARFLU_FORT                               &
&           (FA, IXNIVV, IXTRON, IXLATI, IXLONG)


END SUBROUTINE FARFLU_MT

!INTF KXNIVV        IN    
!INTF KXTRON        IN    
!INTF KXLATI        IN    
!INTF KXLONG        IN    
