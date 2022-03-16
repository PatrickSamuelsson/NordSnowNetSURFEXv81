! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI

SUBROUTINE LFIMST_FORT                             &
&                     (LFI, KREP, KNUMER, LDIMST )
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        CE SOUS-PROGRAMME PERMET D'ACTIVER OU DE DESACTIVER L'OPTION
!     D'IMPRESSION DE STATISTIQUES A LA FERMETURE D'UN FICHIER
!     PARTICULIER, OUVERT POUR LE LOGICIEL LFI.
!        CEPENDANT, TANT QUE LE NIVEAU GLOBAL D'IMPRESSION DES STAT.
!     *LFI%NISTAG* VAUT 0 OU 2, L'OPTION PROPRE AU FICHIER EST INOPERANTE.
!     *LFI%NISTAG* VAUT PAR DEFAUT 1, ET EST REGLABLE VIA LE S/P "LFINSG".
!**
!     ARGUMENTS : KREP   (SORTIE) ==> CODE-REPONSE DU SOUS-PROGRAMME;
!                 KNUMER (ENTREE) ==> LFI%NUMERO D'UNITE LOGIQUE CONCERNEE;
!                 LDIMST (ENTREE) ==> OPTION D'IMPRESSION (VRAI=OUI)
!
!
TYPE(LFICOM) :: LFI
INTEGER (KIND=JPLIKB) KREP, KNUMER, IRANG, IREP, INIMES
!
LOGICAL LDIMST
!
CHARACTER(LEN=LFI%JPLSPX) CLNSPR
CHARACTER(LEN=LFI%JPLMES) CLMESS
CHARACTER(LEN=LFI%JPLFTX) CLACTI
LOGICAL LLFATA

!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('LFIMST_FORT',0,ZHOOK_HANDLE)
CLACTI=''
CALL LFINUM_FORT                    &
&               (LFI, KNUMER,IRANG)
!
IF (IRANG.NE.0) THEN
   IF (LFI%LMULTI) CALL LFIVER_FORT                              &
&                                  (LFI, LFI%VERRUE(IRANG),'ON')
  LFI%LISTAT(IRANG)=LDIMST
  LFI%NDEROP(IRANG)=3
   IF (LFI%LMULTI) CALL LFIVER_FORT                               &
&                                  (LFI, LFI%VERRUE(IRANG),'OFF')
  IREP=0
ELSE
  IREP=-1
ENDIF
!
KREP=IREP
LLFATA=LLMOER (IREP,IRANG)
!
IF (LLFATA.OR.IXNIMS (IRANG).EQ.2) THEN
  INIMES=2
ELSE
  IF (LHOOK) CALL DR_HOOK('LFIMST_FORT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='LFIMST'
WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KNUMER='',I3, &
&       '', LDIMST= '',L1)') KREP,KNUMER,LDIMST
CALL LFIEMS_FORT                                 &
&               (LFI, KNUMER,INIMES,IREP,LLFATA, &
&                CLMESS,CLNSPR,CLACTI)
!
IF (LHOOK) CALL DR_HOOK('LFIMST_FORT',1,ZHOOK_HANDLE)

CONTAINS

#include "lficom2.ixnims.h"
#include "lficom2.llmoer.h"

END SUBROUTINE LFIMST_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFIMST64              &
&           (KREP, KNUMER, LDIMST)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
LOGICAL                LDIMST                                 ! IN   

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIMST_FORT                       &
&           (LFI, KREP, KNUMER, LDIMST)

END SUBROUTINE LFIMST64

SUBROUTINE LFIMST                &
&           (KREP, KNUMER, LDIMST)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
LOGICAL                LDIMST                                 ! IN   

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIMST_MT                        &
&           (LFI, KREP, KNUMER, LDIMST)

END SUBROUTINE LFIMST

SUBROUTINE LFIMST_MT                  &
&           (LFI, KREP, KNUMER, LDIMST)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
LOGICAL                LDIMST                                 ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)

CALL LFIMST_FORT                       &
&           (LFI, IREP, INUMER, LDIMST)

KREP       = INT (      IREP, JPLIKM)

END SUBROUTINE LFIMST_MT

!INTF KREP            OUT 
!INTF KNUMER        IN    
!INTF LDIMST        IN    
