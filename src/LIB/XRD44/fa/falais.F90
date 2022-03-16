! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FALAIS_FORT                                           &
&                     (FA,  KREP, KNUMER, CDNOMA, KDONNE, KLONGD )
USE FA_MOD, ONLY : FA_COM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!      Sous-programme de lecture d'un article de donnees non assimila-
!     bles a un champ horizontal sur un fichier ARPEGE.
!       ( Lecture d'un Article Integre Simplement, non code )
!**
!    Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                KNUMER (Entree) ==> Numero de l'unite logique;
!                CDNOMA (Entree) ==> Nom de l'article;
!    ( Tableau ) KDONNE (Sortie) ==> Donnees a ecrire;
!                KLONGD (Entree) ==> Nombre de mots a ecrire.
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KNUMER, KLONGD
!
INTEGER (KIND=JPLIKB) ILCDNO, IREP, IRANG
INTEGER (KIND=JPLIKB) ILNOMA, INIMES, ILACTI
!
INTEGER (KIND=JPLIKB) KDONNE (KLONGD)
!
LOGICAL LLVERF, LLRLFI
!
CHARACTER CDNOMA*(*)
!
CHARACTER(LEN=FA%JPXNOM) CLACTI 
CHARACTER(LEN=FA%JPXNOM) CLNOMA
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA

!**
!     1.  -  CONTROLES ET INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FALAIS_MT',0,ZHOOK_HANDLE)
CLACTI=''
LLVERF=.FALSE.
LLRLFI=.FALSE.
ILCDNO=INT (LEN (CDNOMA), JPLIKB)
CALL FANUMU_FORT                 &
&               (FA, KNUMER,IRANG)
!
IF (IRANG.EQ.0) THEN
  IREP=-51
  GOTO 1001
ELSEIF (KLONGD.LE.0) THEN
  IREP=-64
  GOTO 1001
ELSEIF (ILCDNO.LE.0) THEN
  IREP=-65
  GOTO 1001
ENDIF
!
!         Verrouillage eventuel du fichier.
!
IF (FA%LFAMUL) CALL LFIVER_FORT                               &
&                              (FA%LFI, FA%FICHIER(IRANG)%VRFICH,'ON')
LLVERF=FA%LFAMUL
!
IF (FA%FICHIER(IRANG)%LCREAF) THEN
  IREP=-85
  GOTO 1001
ELSEIF (CDNOMA.EQ.FA%CPCACH.OR.CDNOMA.EQ.FA%CPCADI.OR. &
&    CDNOMA.EQ.FA%CPCAFS.OR.CDNOMA.EQ.FA%CPCARP.OR.     &
&    CDNOMA.EQ.FA%CPDATE.OR.CDNOMA.EQ.FA%CPDATX) THEN
  IREP=-111
  GOTO 1001
ENDIF
!**
!     2.  -  LECTURE DE L'ARTICLE DE DONNEES SUR LE FICHIER.
!-----------------------------------------------------------------------
!
ILNOMA=MIN ( FA%NCPCAD, INT (LEN (CDNOMA), JPLIKB) )
CLNOMA(1:ILNOMA)=CDNOMA(1:ILNOMA)
!
CALL LFILEC_FORT                                      &
&               (FA%LFI, IREP,KNUMER,CLNOMA(1:ILNOMA), &
&                KDONNE,KLONGD)
LLRLFI=IREP.NE.0
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE, AVEC "ABORT" EVENTUEL,
!            VIA LE SOUS-PROGRAMME "FAIPAR" .
!-----------------------------------------------------------------------
!
1001 CONTINUE
KREP=IREP
LLFATA=LLMOER (IREP,IRANG)
!
!        Deverrouillage eventuel du fichier.
!
IF (LLVERF) CALL LFIVER_FORT                                &
&                           (FA%LFI, FA%FICHIER(IRANG)%VRFICH,'OFF')
!
IF (LLFATA) THEN
  INIMES=2
ELSE
  INIMES=IXNVMS(IRANG)
ENDIF
!
IF (.NOT.LLFATA.AND.INIMES.NE.2)  THEN 
  IF (LHOOK) CALL DR_HOOK('FALAIS_MT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='FALAIS'
!
IF (IREP.NE.-65) THEN
  ILACTI=MIN (ILCDNO,FA%NCPCAD)
  CLACTI(1:ILACTI)=CDNOMA(:ILACTI)
ELSE
  ILACTI=8
  CLACTI(1:ILACTI)=FA%CHAINC(:ILACTI)
ENDIF
!
WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KNUMER='',I3, &
&       '', CDNOMA='''''',A,'''''', KLONGD='',I8)')      &
&   KREP,KNUMER,CLACTI(1:ILACTI),KLONGD
CALL FAIPAR_FORT                                     &
&               (FA, KNUMER,INIMES,IREP,LLFATA,CLMESS, &
&                CLNSPR,CLACTI(1:ILACTI),LLRLFI)
!
IF (LHOOK) CALL DR_HOOK('FALAIS_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"
#include "facom2.ixnvms.h"

END SUBROUTINE FALAIS_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FALAIS64                              &
&           (KREP, KNUMER, CDNOMA, KDONNE, KLONGD)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 ! IN   
INTEGER (KIND=JPLIKB)  KLONGD                                 ! IN   
INTEGER (KIND=JPLIKB)  KDONNE     (KLONGD)                    ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FALAIS_FORT                                     &
&           (FA, KREP, KNUMER, CDNOMA, KDONNE, KLONGD)

END SUBROUTINE FALAIS64

SUBROUTINE FALAIS                                &
&           (KREP, KNUMER, CDNOMA, KDONNE, KLONGD)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 ! IN   
INTEGER (KIND=JPLIKM)  KLONGD                                 ! IN   
REAL (KIND=JPLIKB)     KDONNE     (KLONGD)                    ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FALAIS_MT                                       &
&           (FA, KREP, KNUMER, CDNOMA, KDONNE, KLONGD)

END SUBROUTINE FALAIS

SUBROUTINE FALAIS_MT                                 &
&           (FA, KREP, KNUMER, CDNOMA, KDONNE, KLONGD)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 ! IN   
INTEGER (KIND=JPLIKM)  KLONGD                                 ! IN   
INTEGER (KIND=JPLIKB)  KDONNE     (KLONGD)                    ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  ILONGD                                 ! IN   
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)
ILONGD     = INT (    KLONGD, JPLIKB)

CALL FALAIS_FORT                                     &
&           (FA, IREP, INUMER, CDNOMA, KDONNE, ILONGD)

KREP       = INT (      IREP, JPLIKM)

END SUBROUTINE FALAIS_MT

!INTF KREP            OUT                               
!INTF KNUMER        IN                                  
!INTF CDNOMA        IN                                  
!INTF KDONNE        IN    DIMS=KLONGD                   
!INTF KLONGD        IN                                  
