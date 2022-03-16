! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI

SUBROUTINE LFIAFM_FORT                             &
&                     (LFI, KREP, KNUMER, KFACTM)
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Sous-Programme permettant d'Attribuer un Facteur Multiplicatif
!     a une Unite Logique FORTRAN, destinee a etre ouverte
!     ULTERIEUREMENT par le Logiciel de Fichiers Indexes *LFI* .
!        Lors de cette ouverture ulterieure, LFIOUV essaiera de traiter
!     l'unite logique consideree comme un fichier a acces direct
!     non formatte de longueur d'article "Physique" LFI%JPLARD*KFACTM mots.
!**
!    ARGUMENTS : KREP   (Sortie) ==> Code-REPonse du sous-programme;
!                KNUMER (Entree) ==> NUMero de l'unite logique;
!                KFACTM (Entree) ==> FACteur Multiplicatif a attribuer.
!
!
TYPE(LFICOM) :: LFI
INTEGER (KIND=JPLIKB) KREP, KNUMER, KFACTM, IRANG 
INTEGER (KIND=JPLIKB) IREP, IRANFM, INIMES, IFACTM
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
!        Appel legerement anticipe a LFINUM, permettant une initialisa-
!     tion des variables globales du logiciel a la 1ere utilisation.
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('LFIAFM_FORT',0,ZHOOK_HANDLE)
CLACTI=''
IFACTM=KFACTM
LLVERG=.FALSE.
CALL LFINUM_FORT                    &
&               (LFI, KNUMER,IRANG)
!
IF (KFACTM.LE.0) THEN
  IREP=-14
  GOTO 1001
ELSEIF (KFACTM.GT.LFI%JPFACX) THEN
  IREP=-28
  GOTO 1001
ELSEIF (IRANG.NE.0) THEN
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
IF (IRANFM.NE.0) THEN
!
!          Redefinition du facteur multiplicatif.
!
  IFACTM=LFI%MFACTU(IRANFM)
ELSEIF (LFI%NULOFM.GE.LFI%JPXUFM) THEN
!
!          Tables pleines...
!
  IREP=-29
  GOTO 1001
ELSE
!
!          Cas standard.
!
  LFI%NULOFM=LFI%NULOFM+1
  IRANFM=LFI%NULOFM
  LFI%MULOFM(IRANFM)=KNUMER
  IFACTM=KFACTM
ENDIF
!
LFI%MFACTU(IRANFM)=KFACTM
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
  IF (LHOOK) CALL DR_HOOK('LFIAFM_FORT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
CLNSPR='LFIAFM'
!
IF (INIMES.EQ.2) THEN
  WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KNUMER='',I3, &
&         '', KFACTM='',I4)') KREP,KNUMER,KFACTM
  CALL LFIEMS_FORT                                 &
&                 (LFI, KNUMER,INIMES,IREP,LLFATA, &
&                  CLMESS,CLNSPR,CLACTI)
ENDIF
!
IF (IFACTM.EQ.KFACTM) THEN
!
  IF (LFI%LFRANC) THEN
    WRITE (UNIT=CLMESS,FMT=                                &
&           '(''Attribution du Facteur Multiplicatif'',I3,  &
&             '' a l''''Unite Logique'',I3)') KFACTM,KNUMER
  ELSE
    WRITE (UNIT=CLMESS,FMT='(''Multiply Factor'',I3, &
&           '' specified for Logical Unit'',          &
&             I3)') KFACTM,KNUMER
  ENDIF
!
  CALL LFIEMS_FORT                                         &
&                 (LFI, KNUMER,INIMES,IREP,.FALSE.,CLMESS, &
&                  CLNSPR,CLACTI)
ELSE
!
  IF (LFI%LFRANC) THEN
    WRITE (UNIT=CLMESS,FMT='(''Unite Logique'',I3,               &
&           '': *NOUVEAU* Facteur Multiplicatif attribue='',I3)') &
&    KNUMER,KFACTM
  ELSE
    WRITE (UNIT=CLMESS,FMT='(''Logical Unit'',I3,       &
&           '': *NEW* Multiply Factor specified='',I3)') &
&    KNUMER,KFACTM
  ENDIF
!
  CALL LFIEMS_FORT                                  &
&                 (LFI, KNUMER,INIMES,IREP,.FALSE., &
&                  CLMESS,CLNSPR,CLACTI)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('LFIAFM_FORT',1,ZHOOK_HANDLE)

CONTAINS

#include "lficom2.ixnims.h"
#include "lficom2.llmoer.h"

END SUBROUTINE LFIAFM_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFIAFM64              &
&           (KREP, KNUMER, KFACTM)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  KFACTM                                 ! IN   

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIAFM_FORT                       &
&           (LFI, KREP, KNUMER, KFACTM)

END SUBROUTINE LFIAFM64

SUBROUTINE LFIAFM                &
&           (KREP, KNUMER, KFACTM)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKM)  KFACTM                                 ! IN   

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIAFM_MT                        &
&           (LFI, KREP, KNUMER, KFACTM)

END SUBROUTINE LFIAFM

SUBROUTINE LFIAFM_MT                  &
&           (LFI, KREP, KNUMER, KFACTM)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKM)  KFACTM                                 ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  IFACTM                                 ! IN   
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)
IFACTM     = INT (    KFACTM, JPLIKB)

CALL LFIAFM_FORT                       &
&           (LFI, IREP, INUMER, IFACTM)

KREP       = INT (      IREP, JPLIKM)

END SUBROUTINE LFIAFM_MT

!INTF KREP            OUT 
!INTF KNUMER        IN    
!INTF KFACTM        IN    
