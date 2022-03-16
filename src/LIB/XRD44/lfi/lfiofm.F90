! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI

SUBROUTINE LFIOFM_FORT                                     &
&                     (LFI, KREP, KNUMER, KFACTM, LDOUVR )
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Sous-Programme permettant d'obtenir le Facteur Multiplicatif:
!
!     - effectif d'une Unite Logique FORTRAN deja ouverte pour le
!       logiciel de Fichiers Indexes *LFI*;
!     - prevu pour une Unite Logique FORTRAN, destinee a etre ouverte
!       ULTERIEUREMENT par le Logiciel de Fichiers Indexes *LFI*, en
!       supposant que l'on n'appelle pas ensuite LFIAFM ou LFIFMD
!       avant LFIOUV.
!
!       L'argument de sortie LDOUVR permet de savoir dans quel cas on se
!     trouve.
!**
!    ARGUMENTS : KREP   (Sortie) ==> Code-REPonse du sous-programme;
!                KNUMER (Entree) ==> NUMERo de l'unite logique;
!                KFACTM (Sortie) ==> FACteur Multiplicatif;
!                LDOUVR (Sortie) ==> Vrai si l'unite logique est deja
!                                    ouverte pour le logiciel LFI.
!
TYPE(LFICOM) :: LFI
INTEGER (KIND=JPLIKB) KREP, KNUMER, KFACTM
INTEGER (KIND=JPLIKB) IRANG, IREP, IRANFM, INIMES
!
LOGICAL LLEXUL, LDOUVR
!
CHARACTER(LEN=LFI%JPLSPX) CLNSPR
CHARACTER(LEN=LFI%JPLMES) CLMESS
CHARACTER(LEN=LFI%JPLFTX) CLACTI
LOGICAL LLFATA

!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('LFIOFM_FORT',0,ZHOOK_HANDLE)
CLACTI=''
CALL LFINUM_FORT                    &
&               (LFI, KNUMER,IRANG)
LDOUVR=IRANG.NE.0
!
IF (LDOUVR) THEN
!
!        Unite logique deja ouverte pour le logiciel, on renvoie le
!     facteur multiplicatif effectif sous verrouillage eventuel.
!
  IF (LFI%LMULTI) CALL LFIVER_FORT                              &
&                                 (LFI, LFI%VERRUE(IRANG),'ON')
  KFACTM=LFI%MFACTM(IRANG)
  IF (LFI%LMULTI) CALL LFIVER_FORT                               &
&                                 (LFI, LFI%VERRUE(IRANG),'OFF')
ELSE
!
!        Unite logique non (encore) ouverte pour le logiciel.
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
!          On renvoie le facteur multiplicatif prevu,
!          sous verrouillage Global eventuel.
!
  IF (LFI%LMULTI) CALL LFIVER_FORT                       &
&                                 (LFI, LFI%VERGLA,'ON')
  CALL LFIFMP_FORT                     &
&                 (LFI, KNUMER,IRANFM)
  KFACTM=LFI%MFACTU(IRANFM)
  IF (LFI%LMULTI) CALL LFIVER_FORT                        &
&                                 (LFI, LFI%VERGLA,'OFF')
ENDIF
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
IF (LLFATA) THEN
  INIMES=2
ELSEIF (IRANG.EQ.0) THEN
  INIMES=LFI%NIMESG
ELSE
  INIMES=IXNIMS (IRANG)
ENDIF
!
IF (INIMES.EQ.2) THEN
  CLNSPR='LFIOFM'
  WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KNUMER='',I3, &
&         '', KFACTM='',I4,'', LDOUVR= '',L1)')            &
&    KREP,KNUMER,KFACTM,LDOUVR
  CALL LFIEMS_FORT                                 &
&                 (LFI, KNUMER,INIMES,IREP,LLFATA, &
&                  CLMESS,CLNSPR,CLACTI)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('LFIOFM_FORT',1,ZHOOK_HANDLE)

CONTAINS

#include "lficom2.ixnims.h"
#include "lficom2.llmoer.h"

END SUBROUTINE LFIOFM_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFIOFM64                      &
&           (KREP, KNUMER, KFACTM, LDOUVR)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  KFACTM                                 !   OUT
LOGICAL                LDOUVR                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIOFM_FORT                               &
&           (LFI, KREP, KNUMER, KFACTM, LDOUVR)

END SUBROUTINE LFIOFM64

SUBROUTINE LFIOFM                        &
&           (KREP, KNUMER, KFACTM, LDOUVR)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKM)  KFACTM                                 !   OUT
LOGICAL                LDOUVR                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIOFM_MT                                &
&           (LFI, KREP, KNUMER, KFACTM, LDOUVR)

END SUBROUTINE LFIOFM

SUBROUTINE LFIOFM_MT                          &
&           (LFI, KREP, KNUMER, KFACTM, LDOUVR)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKM)  KFACTM                                 !   OUT
LOGICAL                LDOUVR                                 !   OUT
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  IFACTM                                 !   OUT
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)

CALL LFIOFM_FORT                               &
&           (LFI, IREP, INUMER, IFACTM, LDOUVR)

KREP       = INT (      IREP, JPLIKM)
KFACTM     = INT (    IFACTM, JPLIKM)

END SUBROUTINE LFIOFM_MT

!INTF KREP            OUT 
!INTF KNUMER        IN    
!INTF KFACTM          OUT 
!INTF LDOUVR          OUT 
