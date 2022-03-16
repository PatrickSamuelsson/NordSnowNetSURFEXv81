! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FAIRME_FORT                           &
&                     (FA,  KREP, KNUMER, CDSTTU )
USE FA_MOD, ONLY : FA_COM, FREE_FICHIER, JPNIIL
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!       Sous-programme de FERMETURE d'une unite logique "Fichier ARPEGE"
!**
!    Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                KNUMER (Entree) ==> Numero de l'unite logique;
!                CDSTTU (Entree) ==> "STATUS" eventuel pour "CLOSE".
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KNUMER
!
INTEGER (KIND=JPLIKB) IREP, IRANG, J, IPOSNU
INTEGER (KIND=JPLIKB) IRANGC, INIMES, ILNOMC
!
CHARACTER(LEN=*) CDSTTU
CHARACTER(LEN=7) CLSTTU
!
LOGICAL LLSTTU, LLVERF, LLRLFI
!
CHARACTER(LEN=FA%JPXNOM) CLACTI 
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA
INTEGER (KIND=JPLIKM)    IREP4

!**
!     1.  -  CONTROLES DES PARAMETRES D'APPEL, PUIS INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FAIRME_MT',0,ZHOOK_HANDLE)
CLACTI=''
IREP=0
LLVERF=.FALSE.
LLRLFI=.FALSE.
CALL FANUMU_FORT                 &
&               (FA, KNUMER,IRANG)
!
!         Verrouillage global eventuel.
!
IF (FA%LFAMUL) CALL LFIVER_FORT                        &
&                              (FA%LFI, FA%VRGLAS,'ON')
!
IF (IRANG.EQ.0) THEN
  IREP=-51
  GOTO 1001
ELSEIF (LEN (CDSTTU).LE.0) THEN
  IREP=-65
  GOTO 1001
ELSE
  LLSTTU=CDSTTU.EQ.'KEEP'.OR.CDSTTU.EQ.'DELETE'
!
  IF (LLSTTU) THEN
    CLSTTU=CDSTTU(1:MIN (LEN (CDSTTU),LEN (CLSTTU)))
  ELSE
    CLSTTU='DEFAUT'
  ENDIF
!
ENDIF
!
IF (FA%LFAMUL) CALL LFIVER_FORT                               &
&                              (FA%LFI, FA%FICHIER(IRANG)%VRFICH,'ON')
LLVERF=FA%LFAMUL
!
IF (FA%FICHIER(IRANG)%LCREAF.AND..NOT.LLSTTU) THEN
!
!         On force le relachement d'un fichier "parasite".
!
  LLSTTU=.TRUE.
  CLSTTU='DELETE'
ENDIF
!**
!     2.  -  FERMETURE DU FICHIER, AU SENS DU LOGICIEL LFI.
!-----------------------------------------------------------------------
!
CALL LFIFER_FORT                            &
&               (FA%LFI, IREP,KNUMER,CLSTTU)
!
IF (IREP.NE.0) THEN
  LLRLFI=.TRUE.
  GOTO 1001
ENDIF
IF (FA%FICHIER(IRANG)%NFILEP /= 0) THEN
  CALL FI_FCLOSE (IREP4, FA%FICHIER(IRANG)%NFILEP)
  FA%FICHIER(IRANG)%NFILEP = 0
  IREP = IREP4
  IF (IREP /= 0) GOTO 1001
ENDIF
!**
!     3.  -  "NETTOYAGE" DES TABLES AYANT PERMIS DE GERER LE FICHIER.
!            ( au moins celles ayant un caractere "global" )
!-----------------------------------------------------------------------
!
FA%FICHIER(IRANG)%NULOGI=JPNIIL
!
DO J=1,FA%NFIOUV
!
IF (FA%NULIND(J).EQ.IRANG) THEN
  IPOSNU=J
  GOTO 302
ENDIF
!
ENDDO
!
IREP=-66
GOTO 1001
!
302 CONTINUE
!
FA%NFIOUV=FA%NFIOUV-1
!
DO J=IPOSNU,FA%NFIOUV
FA%NULIND(J)=FA%NULIND(J+1)
ENDDO
!
IF (FA%LFAMUL) THEN
  CALL LFIVER_FORT                                &
&                 (FA%LFI, FA%FICHIER(IRANG)%VRFICH,'OFF')
  CALL LFIVER_FORT                                &
&                 (FA%LFI, FA%FICHIER(IRANG)%VRFICH,'REL')
ENDIF
!
LLVERF=.FALSE.
IRANGC=FA%FICHIER(IRANG)%NUCADR
FA%CADRE(IRANGC)%NULCAD=FA%CADRE(IRANGC)%NULCAD-1
!
!        Si le cadre auquel etait rattache le fichier n'a plus d'autre
!     fichier rattache, et qu'on ne devait pas conserver ce cadre,
!     on le supprime.
!
IF (FA%CADRE(IRANGC)%NULCAD.LE.0.AND. &
&   (FA%CADRE(IRANGC)%NGARDE.EQ.0.OR.  &
&   (FA%CADRE(IRANGC)%NGARDE.EQ.1.AND. &
&   .NOT.FA%LIGARD)))                  &
&      CALL FACTUI_FORT                &
&                     (FA, IREP,IRANGC)
CALL FREE_FICHIER (FA%FICHIER(IRANG))
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE, AVEC "ABORT" EVENTUEL,
!            VIA LE SOUS-PROGRAMME "FAIPAR" .
!-----------------------------------------------------------------------
!
1001 CONTINUE
KREP=IREP
LLFATA=LLMOER (IREP,IRANG)
!
!        Deverrouillage(s) eventuel(s).
!
IF (LLVERF) CALL LFIVER_FORT                                &
&                           (FA%LFI, FA%FICHIER(IRANG)%VRFICH,'OFF')
IF (FA%LFAMUL) CALL LFIVER_FORT                         &
&                              (FA%LFI, FA%VRGLAS,'OFF')
!
IF (LLFATA) THEN
  INIMES=2
ELSE
  INIMES=IXNVMS(IRANG)
ENDIF
!
IF (INIMES.EQ.0)  THEN 
  IF (LHOOK) CALL DR_HOOK('FAIRME_MT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='FAIRME'
!
IF (IREP.EQ.-65) THEN
  ILNOMC=8
  CLACTI(1:ILNOMC)=FA%CHAINC(:ILNOMC)
 ELSE
  ILNOMC=MIN ( INT (LEN (CDSTTU), JPLIKB),  &
&               INT (LEN (CLACTI), JPLIKB) )
  CLACTI(1:ILNOMC)=CDSTTU(1:ILNOMC)
ENDIF
!
IF (INIMES.EQ.2) THEN
!
  ILNOMC=MIN (ILNOMC,FA%NCPCAD)
  WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KNUMER='',I3, &
&         '', CDSTTU='''''',A,'''''''')') KREP,KNUMER,     &
&       CLACTI(1:ILNOMC)
  CALL FAIPAR_FORT                              &
&                 (FA, KNUMER,INIMES,IREP,LLFATA, &
&               CLMESS,CLNSPR,                    &
&               CLACTI(1:ILNOMC),LLRLFI)
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('FAIRME_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"
#include "facom2.ixnvms.h"

END SUBROUTINE FAIRME_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FAIRME64              &
&           (KREP, KNUMER, CDSTTU)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDSTTU                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAIRME_FORT                     &
&           (FA, KREP, KNUMER, CDSTTU)

END SUBROUTINE FAIRME64

SUBROUTINE FAIRME                &
&           (KREP, KNUMER, CDSTTU)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDSTTU                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAIRME_MT                       &
&           (FA, KREP, KNUMER, CDSTTU)

END SUBROUTINE FAIRME

SUBROUTINE FAIRME_MT                 &
&           (FA, KREP, KNUMER, CDSTTU)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDSTTU                                 ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)

CALL FAIRME_FORT                     &
&           (FA, IREP, INUMER, CDSTTU)

KREP       = INT (      IREP, JPLIKM)

END SUBROUTINE FAIRME_MT

!INTF KREP            OUT 
!INTF KNUMER        IN    
!INTF CDSTTU        IN    
