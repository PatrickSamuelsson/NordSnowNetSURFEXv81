! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI
SUBROUTINE LFIPHA_FORT                                    &
&                     (LFI, KREP, KRANG, KRGPIM, KRETIN )
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        SOUS-PROGRAMME *INTERNE* DU LOGICIEL DE FICHIERS INDEXES LFI
!     PHASAGE D'UNE PAGE D'INDEX "LONGUEUR/POSITION"
!     AVEC LA PAGE D'INDEX "NOMS" CORRESPONDANTE.
!        IL EST ABSOLUMENT NECESSAIRE QUE LA PAGE D'INDEX "NOMS" SOIT
!     EFFECTIVEMENT ALIMENTEE AVANT L'APPEL DE CE SOUS-PROGRAMME...
!**
!    ARGUMENTS : KREP   (SORTIE) ==> CODE-REPONSE DE LA LECTURE DE PAGE;
!                KRANG  (ENTREE) ==> RANG DE L'UNITE LOGIQUE CONCERNEE;
!                KRGPIM (ENTREE) ==> LFI%NUMERO DE LA PAGE CONCERNEE;
!                KRETIN (SORTIE) ==> CODE-RETOUR INTERNE.
!
!
TYPE(LFICOM) :: LFI
INTEGER (KIND=JPLIKB) KREP, KRANG, KRGPIM, KRETIN, INUMER 
INTEGER (KIND=JPLIKB) IREC, INAPHY, INIMES
INTEGER (KIND=JPLIKB) IRETOU, IRETIN
!
CHARACTER(LEN=LFI%JPLSPX) CLNSPR
CHARACTER(LEN=LFI%JPLMES) CLMESS
CHARACTER(LEN=LFI%JPLFTX) CLACTI
LOGICAL LLFATA

!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('LFIPHA_FORT',0,ZHOOK_HANDLE)
CLACTI=''
IRETOU=0
INUMER=LFI%NUMERO(KRANG)
CALL LFIREC_FORT                                     &
&               (LFI, LFI%MRGPIF(KRGPIM),KRANG,IREC)
INAPHY=IREC+1
CALL LFILDO_FORT                                         &
&               (LFI, KREP,INUMER,IREC+1,              &
&                LFI%MLGPOS(IXM(1_JPLIKB ,KRGPIM)),    &
&                LFI%NBREAD(KRANG),LFI%MFACTM(KRANG),  &
&                LFI%YLFIC (KRANG),IRETIN)
!
IF (IRETIN.NE.0) THEN
  GOTO 904
ENDIF
!
LFI%LPHASP(KRGPIM)=.TRUE.
GOTO 1001
!
904 CONTINUE
IRETOU=2
CLACTI='READ'
!
!      AU CAS OU, ON FORCE LE CODE-REPONSE ENTREE/SORTIE A ETRE POSITIF.
!
KREP=ABS (KREP)
LFI%NUMAPH(KRANG)=INAPHY
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE INTERNE EVENTUELLE,
!            VIA LE SOUS-PROGRAMME "LFIEMS", PUIS RETOUR.
!-----------------------------------------------------------------------
!
1001 CONTINUE
LLFATA=LLMOER (KREP,KRANG)
!
IF (KREP.EQ.0) THEN
  KRETIN=0
ELSEIF (KREP.GT.0) THEN
  KRETIN=IRETOU
ELSE
  KRETIN=3
ENDIF
!
IF (LFI%LMISOP.OR.LLFATA) THEN
  INIMES=2
  CLNSPR='LFIPHA'
  WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KRANG='',I3, &
&         '', KRGPIM='',I3,'', KRETIN='',I2)')            &
&    KREP,KRANG,KRGPIM,KRETIN
  CALL LFIEMS_FORT                                  &
&                 (LFI, INUMER,INIMES,KREP,.FALSE., &
&                  CLMESS,CLNSPR,CLACTI)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('LFIPHA_FORT',1,ZHOOK_HANDLE)

CONTAINS

#include "lficom2.ixm.h"
#include "lficom2.llmoer.h"

END SUBROUTINE LFIPHA_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFIPHA64                     &
&           (KREP, KRANG, KRGPIM, KRETIN)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KRANG                                  ! IN   
INTEGER (KIND=JPLIKB)  KRGPIM                                 ! IN   
INTEGER (KIND=JPLIKB)  KRETIN                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIPHA_FORT                             &
&           (LFI, KREP, KRANG, KRGPIM, KRETIN)

END SUBROUTINE LFIPHA64

SUBROUTINE LFIPHA                       &
&           (KREP, KRANG, KRGPIM, KRETIN)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KRANG                                  ! IN   
INTEGER (KIND=JPLIKM)  KRGPIM                                 ! IN   
INTEGER (KIND=JPLIKM)  KRETIN                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIPHA_MT                               &
&           (LFI, KREP, KRANG, KRGPIM, KRETIN)

END SUBROUTINE LFIPHA

SUBROUTINE LFIPHA_MT                         &
&           (LFI, KREP, KRANG, KRGPIM, KRETIN)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KRANG                                  ! IN   
INTEGER (KIND=JPLIKM)  KRGPIM                                 ! IN   
INTEGER (KIND=JPLIKM)  KRETIN                                 !   OUT
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  IRANG                                  ! IN   
INTEGER (KIND=JPLIKB)  IRGPIM                                 ! IN   
INTEGER (KIND=JPLIKB)  IRETIN                                 !   OUT
! Convert arguments

IRANG      = INT (     KRANG, JPLIKB)
IRGPIM     = INT (    KRGPIM, JPLIKB)

CALL LFIPHA_FORT                             &
&           (LFI, IREP, IRANG, IRGPIM, IRETIN)

KREP       = INT (      IREP, JPLIKM)
KRETIN     = INT (    IRETIN, JPLIKM)

END SUBROUTINE LFIPHA_MT

!INTF KREP            OUT 
!INTF KRANG         IN    
!INTF KRGPIM        IN    
!INTF KRETIN          OUT 
