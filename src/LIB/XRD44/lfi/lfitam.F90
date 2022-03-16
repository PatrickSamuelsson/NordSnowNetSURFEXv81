! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI
SUBROUTINE LFITAM_FORT                                     &
&                     (LFI, KREP, KNUMER, LDTAML, LDTAME )
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        CE SOUS-PROGRAMME PERMET D'ACTIVER OU DE DESACTIVER LES OPTIONS
!     D'UTILISATION MAXIMUM DE LA MEMOIRE TAMPON SUR UN FICHIER
!     PARTICULIER, OUVERT POUR LE LOGICIEL LFI.
!        LES OPTIONS INITIALES SONT DEFINIES A L'OUVERTURE PAR LES
!     VARIABLES GLOBALES *LFI%LTAMLG* ET *LFI%LTAMEG*, RESPECTIVEMENT POUR LES
!     OPERATIONS DE LECTURE ET D'ECRITURE .
!     ( CES VARIABLES GLOBALES SONT DEFINIES DANS LE S/P *LFIINI* )
!**
!     ARGUMENTS : KREP   (SORTIE) ==> CODE-REPONSE DU SOUS-PROGRAMME;
!                 KNUMER (ENTREE) ==> LFI%NUMERO D'UNITE LOGIQUE CONCERNEE;
!                 LDTAML (ENTREE) ==> OPTION D'UTILISATION MAXIMUM DE LA
!                                     LA MEMOIRE TAMPON EN LECTURE;
!                 LDTAME (ENTREE) ==> CF. CI-DESSUS, EN ECRITURE.
!
!
TYPE(LFICOM) :: LFI
INTEGER (KIND=JPLIKB) KREP, KNUMER, IRANG, IREP, INIMES
!
LOGICAL LDTAML, LDTAME
!
CHARACTER(LEN=LFI%JPLSPX) CLNSPR
CHARACTER(LEN=LFI%JPLMES) CLMESS
CHARACTER(LEN=LFI%JPLFTX) CLACTI
LOGICAL LLFATA

!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('LFITAM_FORT',0,ZHOOK_HANDLE)
CLACTI=''
CALL LFINUM_FORT                    &
&               (LFI, KNUMER,IRANG)
!
IF (IRANG.NE.0) THEN
   IF (LFI%LMULTI) CALL LFIVER_FORT                              &
&                                  (LFI, LFI%VERRUE(IRANG),'ON')
  LFI%LTAMPL(IRANG)=LDTAML
  LFI%LTAMPE(IRANG)=LDTAME
  LFI%NDEROP(IRANG)=6
   IF (LFI%LMULTI) CALL LFIVER_FORT                               &
&                                  (LFI, LFI%VERRUE(IRANG),'OFF')
  IREP=0
ELSE
  IREP=-1
ENDIF
!
LLFATA=LLMOER (IREP,IRANG)
KREP=IREP
!
IF (LLFATA.OR.IXNIMS (IRANG).EQ.2) THEN
  INIMES=2
ELSE
  IF (LHOOK) CALL DR_HOOK('LFITAM_FORT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='LFITAM'
WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KNUMER='',I3,            &
&  '', LDTAML= '',L1,'', LDTAME= '',L1)') KREP,KNUMER,LDTAML,LDTAME
CALL LFIEMS_FORT                                 &
&               (LFI, KNUMER,INIMES,IREP,LLFATA, &
&                CLMESS,CLNSPR,CLACTI)
!
IF (LHOOK) CALL DR_HOOK('LFITAM_FORT',1,ZHOOK_HANDLE)

CONTAINS

#include "lficom2.ixnims.h"
#include "lficom2.llmoer.h"

END SUBROUTINE LFITAM_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFITAM64                      &
&           (KREP, KNUMER, LDTAML, LDTAME)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
LOGICAL                LDTAML                                 ! IN   
LOGICAL                LDTAME                                 ! IN   

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFITAM_FORT                               &
&           (LFI, KREP, KNUMER, LDTAML, LDTAME)

END SUBROUTINE LFITAM64

SUBROUTINE LFITAM                        &
&           (KREP, KNUMER, LDTAML, LDTAME)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
LOGICAL                LDTAML                                 ! IN   
LOGICAL                LDTAME                                 ! IN   

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFITAM_MT                                &
&           (LFI, KREP, KNUMER, LDTAML, LDTAME)

END SUBROUTINE LFITAM

SUBROUTINE LFITAM_MT                          &
&           (LFI, KREP, KNUMER, LDTAML, LDTAME)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
LOGICAL                LDTAML                                 ! IN   
LOGICAL                LDTAME                                 ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)

CALL LFITAM_FORT                               &
&           (LFI, IREP, INUMER, LDTAML, LDTAME)

KREP       = INT (      IREP, JPLIKM)

END SUBROUTINE LFITAM_MT

!INTF KREP            OUT 
!INTF KNUMER        IN    
!INTF LDTAML        IN    
!INTF LDTAME        IN    
