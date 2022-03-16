! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI
SUBROUTINE LFIVID_FORT                                            &
&                     (LFI, KREP, KRANG, KNUMPD, KTAMPO, KRETIN )
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        SOUS-PROGRAMME *INTERNE* DU LOGICIEL DE FICHIERS INDEXES LFI
!     "VIDAGE" SUR FICHIER D'UNE PAGE DE DONNEES, APRES L'AVOIR DUMENT
!     COMPLETEE SI NECESSAIRE ( AVEC LES DONNEES DEJA PRESENTES SUR
!     FICHIER, OU AVEC DES ZEROS DANS LE CAS DU DERNIER ARTICLE ).
!**
!    ARGUMENTS : KREP   (SORTIE) ==> CODE-REPONSE DE L'ECRITURE FORTRAN;
!                KRANG  (ENTREE) ==> RANG EN MEMOIRE DE L'UNITE LOGIQUE;
!                KNUMPD (ENTREE) ==> LFI%NUMERO DE LA PAGE DE DONNEES;
!                KTAMPO (ENTREE) ==> ZONE SERVANT A RELIRE L'ARTICLE
!                                    PHYSIQUE CORRESPONDANT SUR FICHIER,
!                                    SI NECESSAIRE. (LONGUEUR: LFI%JPLARX)
!                KRETIN (SORTIE) ==> CODE-RETOUR INTERNE.
!
!
TYPE(LFICOM) :: LFI
INTEGER (KIND=JPLIKB) KREP, KRANG, KNUMPD, KRETIN
INTEGER (KIND=JPLIKB)  KTAMPO (LFI%JPLARX)
INTEGER (KIND=JPLIKB) INUMER, ILONPD, INUMAE, IFACTM 
INTEGER (KIND=JPLIKB) ILARPH, JD, INAPHY, IRETOU
INTEGER (KIND=JPLIKB) INIMES, IRETIN
!
LOGICAL LLADON
!
CHARACTER(LEN=LFI%JPLSPX) CLNSPR
CHARACTER(LEN=LFI%JPLMES) CLMESS
CHARACTER(LEN=LFI%JPLFTX) CLACTI
LOGICAL LLFATA

!**
!     1.  -  CONTROLES ET INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('LFIVID_FORT',0,ZHOOK_HANDLE)
CLACTI=''
IF (KRANG.LE.0.OR.KRANG.GT.LFI%JPNXFI) THEN
  INUMER=LFI%JPNIL
ELSE
  INUMER=LFI%NUMERO(KRANG)
  KREP=0
ENDIF
!
IRETOU=0
!
IF (INUMER.EQ.LFI%JPNIL) THEN
  KREP=-14
  GOTO 1001
ENDIF
!
ILONPD=LFI%NLONPD(KNUMPD,KRANG)
INUMAE=LFI%NUMAPD(KNUMPD,KRANG)
IFACTM=LFI%MFACTM(KRANG)
ILARPH=LFI%JPLARD*IFACTM
!**
!     2.  -  COMPLEMENT EVENTUEL DE LA PAGE DE DONNEES A TRAITER.
!-----------------------------------------------------------------------
!
IF (ILONPD.NE.ILARPH) THEN
!
!             PAGE DE DONNEES INSUFFISAMMENT REMPLIE.
!
  IF (INUMAE.GT.LFI%MDES1D(IXM(LFI%JPAXPD,KRANG))) THEN
!*
!     2.1 -  PAS D'ARTICLE PHYSIQUE ASSOCIE SUR FICHIER,
!            ON LA COMPLETE AVEC DES ZEROS.
!-----------------------------------------------------------------------
!
    DO JD=ILONPD+1,ILARPH
    LFI%MTAMPD(IXT(JD,KNUMPD,KRANG))=0
    ENDDO
!
  ELSE
!*
!     2.2 -  NECESSITE D'ALLER RELIRE L'ARTICLE PHYSIQUE DE DONNEES
!            SUR FICHIER, ET DE "RECOLLER LES MORCEAUX".
!-----------------------------------------------------------------------
!
    INAPHY=INUMAE
    CALL LFILDO_FORT                                  &
&                   (LFI, KREP,INUMER,INUMAE,KTAMPO,&
&                    LFI%NBREAD(KRANG),IFACTM,      &
&                    LFI%YLFIC (KRANG),IRETIN)
!
    IF (IRETIN.NE.0) THEN
      GOTO 904
    ENDIF
!
    DO JD=ILONPD+1,ILARPH
    LFI%MTAMPD(IXT(JD,KNUMPD,KRANG))=KTAMPO(JD)
    ENDDO
!
  ENDIF
!
ENDIF
!**
!     3.  -  ECRITURE OU REECRITURE DE LA PAGE DE DONNEES COMPLETE
!            OU COMPLETEE SUR FICHIER.
!-----------------------------------------------------------------------
!
LLADON=.TRUE.
INAPHY=0
CALL LFIECX_FORT                                          &
&               (LFI,KREP,KRANG,INUMAE,                   &
&                LFI%MTAMPD(IXT(1_JPLIKB ,KNUMPD,KRANG)), &
&                LLADON,IRETIN)
!
IF (IRETIN.EQ.1) THEN
  GOTO 903
ELSEIF (IRETIN.EQ.2) THEN
  GOTO 904
ELSEIF (IRETIN.NE.0) THEN
  GOTO 1001
ENDIF
!
LFI%LECRPD(KNUMPD,KRANG)=.FALSE.
GOTO 1001
!**
!     9.  - CI-DESSOUS, ETIQUETTES DE BRANCHEMENT EN CAS D'ERREUR E/S.
!      AU CAS OU, ON FORCE LE CODE-REPONSE ENTREE/SORTIE A ETRE POSITIF.
!-----------------------------------------------------------------------
!
903 CONTINUE
IRETOU=1
CLACTI='WRITE'
GOTO 909
!
904 CONTINUE
IRETOU=2
CLACTI='READ'
!
909 CONTINUE
KREP=ABS (KREP)
IF (INAPHY.NE.0) LFI%NUMAPH(KRANG)=INAPHY
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
  CLNSPR='LFIVID'
  WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KRANG='',I3, &
&         '', KNUMPD='',I3,'', KRETIN='',I2)')            &
&     KREP,KRANG,KNUMPD,KRETIN
  CALL LFIEMS_FORT                                  &
&                 (LFI, INUMER,INIMES,KREP,.FALSE., &
&                  CLMESS,CLNSPR,CLACTI)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('LFIVID_FORT',1,ZHOOK_HANDLE)

CONTAINS

#include "lficom2.ixm.h"
#include "lficom2.ixt.h"
#include "lficom2.llmoer.h"

END SUBROUTINE LFIVID_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFIVID64                             &
&           (KREP, KRANG, KNUMPD, KTAMPO, KRETIN)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KRANG                                  ! IN   
INTEGER (KIND=JPLIKB)  KNUMPD                                 ! IN   
INTEGER (KIND=JPLIKB)  KTAMPO     (*)                ! IN   
INTEGER (KIND=JPLIKB)  KRETIN                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIVID_FORT                                     &
&           (LFI, KREP, KRANG, KNUMPD, KTAMPO, KRETIN)

END SUBROUTINE LFIVID64

SUBROUTINE LFIVID                               &
&           (KREP, KRANG, KNUMPD, KTAMPO, KRETIN)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KRANG                                  ! IN   
INTEGER (KIND=JPLIKM)  KNUMPD                                 ! IN   
INTEGER (KIND=JPLIKB)  KTAMPO     (*)                ! IN   
INTEGER (KIND=JPLIKM)  KRETIN                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIVID_MT                                       &
&           (LFI, KREP, KRANG, KNUMPD, KTAMPO, KRETIN)

END SUBROUTINE LFIVID

SUBROUTINE LFIVID_MT                                 &
&           (LFI, KREP, KRANG, KNUMPD, KTAMPO, KRETIN)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KRANG                                  ! IN   
INTEGER (KIND=JPLIKM)  KNUMPD                                 ! IN   
INTEGER (KIND=JPLIKB)  KTAMPO     (LFI%JPLARX)                ! IN   
INTEGER (KIND=JPLIKM)  KRETIN                                 !   OUT
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  IRANG                                  ! IN   
INTEGER (KIND=JPLIKB)  INUMPD                                 ! IN   
INTEGER (KIND=JPLIKB)  IRETIN                                 !   OUT
! Convert arguments

IRANG      = INT (     KRANG, JPLIKB)
INUMPD     = INT (    KNUMPD, JPLIKB)

CALL LFIVID_FORT                                     &
&           (LFI, IREP, IRANG, INUMPD, KTAMPO, IRETIN)

KREP       = INT (      IREP, JPLIKM)
KRETIN     = INT (    IRETIN, JPLIKM)

END SUBROUTINE LFIVID_MT

!INTF KREP            OUT                                                              
!INTF KRANG         IN                                                                 
!INTF KNUMPD        IN                                                                 
!INTF KTAMPO        IN    DIMS=LFI%JPLARX                KIND=JPLIKB                   
!INTF KRETIN          OUT                                                              
