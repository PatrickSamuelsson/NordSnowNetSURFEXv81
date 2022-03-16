! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI

SUBROUTINE LFISFM_FORT                     &
&                     (LFI, KREP, KNUMER )
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Sous-Programme Suprimant un Facteur Multiplicatif
!     d'une Unite Logique FORTRAN, qui a ete fermee PRECEDEMMENT
!     par le Logiciel de Fichiers Indexes *LFI* .
!     (ou du moins, n'est pas ouverte pour ce logiciel)
!
!        Ce sous-programme permet de faire de la place dans les tables
!     decrivant les associations Unite Logique/facteur Multiplicatif.
!**
!    ARGUMENTS : KREP   (Sortie) ==> Code-REPonse du sous-programme;
!                KNUMER (Entree) ==> NUMERo de l'unite logique.
!
!
TYPE(LFICOM) :: LFI
INTEGER (KIND=JPLIKB) KREP, KNUMER, IRANG, IREP 
INTEGER (KIND=JPLIKB) IRANFM, INIMES, IFACTM, J
!
LOGICAL LLVERG, LLEXUL
!
CHARACTER(LEN=LFI%JPLSPX) CLNSPR
CHARACTER(LEN=LFI%JPLMES) CLMESS
CHARACTER(LEN=LFI%JPLFTX) CLACTI
LOGICAL LLFATA

!**
!     1.  -  CONTROLES DES PARAMETRES D'APPEL, INITIALISATIONS.
!-----------------------------------------------------------------------
!
!        Appel a LFINUM, permettant (le cas echeant) l'initialisation
!     variables globales du logiciel a la 1ere utilisation.
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('LFISFM_FORT',0,ZHOOK_HANDLE)
CLACTI=''
IFACTM=0
LLVERG=.FALSE.
CALL LFINUM_FORT                    &
&               (LFI, KNUMER,IRANG)
!
IF (IRANG.NE.0) THEN
  IREP=-5
  GOTO 1001
ENDIF
!
!        Controle de validite FORTRAN du Numero d'Unite Logique.
!
IF (KNUMER > 0) THEN
  INQUIRE (UNIT=KNUMER,EXIST=LLEXUL,ERR=901,IOSTAT=IREP)
ELSE
  LLEXUL=.TRUE.
ENDIF
!
IF (.NOT.LLEXUL) THEN
  IREP=-30
  GOTO 1001
ENDIF
!
!              Verrouillage Global eventuel.
!
 IF (LFI%LMULTI) CALL LFIVER_FORT                       &
&                                (LFI, LFI%VERGLA,'ON')
LLVERG=LFI%LMULTI
!**
!     2.  -  TRAVAIL EFFECTIF SUR LES TABLES DECRIVANT LES ASSOCIATIONS
!            UNITES LOGIQUES/FACTEURS.
!-----------------------------------------------------------------------
!
CALL LFIFMP_FORT                     &
&               (LFI, KNUMER,IRANFM)
!
IF (IRANFM.EQ.0) THEN
!
!          Unite logique non trouvee dans la table *LFI%MULOFM*.
!
  IREP=-31
  GOTO 1001
ENDIF
!
IFACTM=LFI%MFACTU(IRANFM)
LFI%NULOFM=LFI%NULOFM-1
!
DO J=IRANFM,LFI%NULOFM
LFI%MFACTU(J)=LFI%MFACTU(J+1)
LFI%MULOFM(J)=LFI%MULOFM(J+1)
ENDDO
!
IREP=0
GOTO 1001
!**
!     9.  - CI-DESSOUS, ETIQUETTE DE BRANCHEMENT EN CAS D'ERREUR INQUIRE
!-----------------------------------------------------------------------
!
901 CONTINUE
CLACTI='INQUIRE'
!
!      AU CAS OU, ON FORCE LE CODE-REPONSE ENTREE/SORTIE A ETRE POSITIF.
!
IREP=ABS (IREP)
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE, AVEC "ABORT" EVENTUEL,
!            VIA LE SOUS-PROGRAMME "LFIEMS" .
!-----------------------------------------------------------------------
!
1001 CONTINUE
KREP=IREP
LLFATA=LLMOER (IREP,IRANG)
!
IF (LLVERG) CALL LFIVER_FORT                        &
&                           (LFI, LFI%VERGLA,'OFF')
!
IF (LLFATA) THEN
  INIMES=2
ELSEIF (IRANG.EQ.0) THEN
  INIMES=LFI%NIMESG
ELSE
  INIMES=IXNIMS (IRANG)
ENDIF
!
IF (INIMES.EQ.0)  THEN 
  IF (LHOOK) CALL DR_HOOK('LFISFM_FORT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
CLNSPR='LFISFM'
!
IF (INIMES.EQ.2) THEN
  WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KNUMER='',I3)') &
&       KREP,KNUMER
  CALL LFIEMS_FORT                                 &
&                 (LFI, KNUMER,INIMES,IREP,LLFATA, &
&                  CLMESS,CLNSPR,CLACTI)
ENDIF
!
IF (LFI%LFRANC) THEN
  WRITE (UNIT=CLMESS,FMT=                               &
&         '(''Suppression du Facteur Multiplicatif'',I3, &
&           '', Unite Logique'',I3)') IFACTM,KNUMER
ELSE
  WRITE (UNIT=CLMESS,FMT=                                    &
&         '(''Multiply Factor'',I3,                           &
&           '' suppressed, Logical Unit'',I3)') IFACTM,KNUMER
ENDIF
!
CALL LFIEMS_FORT                                  &
&               (LFI, KNUMER,INIMES,IREP,.FALSE., &
&                CLMESS,CLNSPR,CLACTI)
!
IF (LHOOK) CALL DR_HOOK('LFISFM_FORT',1,ZHOOK_HANDLE)

CONTAINS

#include "lficom2.ixnims.h"
#include "lficom2.llmoer.h"

END SUBROUTINE LFISFM_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFISFM64           &
&           (KREP, KNUMER)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFISFM_FORT               &
&           (LFI, KREP, KNUMER)

END SUBROUTINE LFISFM64

SUBROUTINE LFISFM             &
&           (KREP, KNUMER)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFISFM_MT                &
&           (LFI, KREP, KNUMER)

END SUBROUTINE LFISFM

SUBROUTINE LFISFM_MT             &
&           (LFI, KREP, KNUMER)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)

CALL LFISFM_FORT               &
&           (LFI, IREP, INUMER)

KREP       = INT (      IREP, JPLIKM)

END SUBROUTINE LFISFM_MT

!INTF KREP            OUT 
!INTF KNUMER        IN    
