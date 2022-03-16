! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI
SUBROUTINE LFIDST_FORT                                           &
&                     (LFI, KREP, KRANG, KRANIE, CDSTRU, KLONG )
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        SOUS-PROGRAMME *INTERNE* DU LOGICIEL DE FICHIERS INDEXES LFI;
!     Decorticage de la STructure decrivant un article logique
!     de donnees, en vue de son import ou export ulterieur.
!**
!    ARGUMENTS : KREP   (SORTIE) ==> CODE-REPONSE DU SOUS-PROGRAMME;
!                KRANG  (ENTREE) ==> RANG ( DANS LA TABLE *LFI%NUMERO* )
!                                    DE L'UNITE LOGIQUE CONCERNEE;
!                KRANIE (Entree) ==> Rang dans les tables d'import/exp;
!                CDSTRU (Entree) ==> Structure interne de cet article;
!                KLONG  (Entree) ==> Longueur (mots) de l'article.
!
!     Les syntaxes autorisees pour CDSTRU sont decrites ci-dessous.
!     La presence de crochets [ ] indique le cote optionnel, mais dans
!     tous les cas ce cote optionnel est reserve a la toute derniere
!     partie de la description. Si l'argument optionnel est present,
!     il doit etre coherent avec la longueur effective de l'article. 
!
!     'type [nbre]' ==> article homogene,ex: 'i', 'r 20'
!
!     'type_1 nbre_1 ... type_n [nbre_n]' ==> juxtaposition de types,ex:
!                                             'i 2 r', 'i 3 r 2 c 80000'
!
!     '(type_1 nbre_1 ... type_n nbre_n) [nbre]' ==> boucle,ex:
!                                                    '(i 1 r 1)'
!
!     ou une juxtaposition des possibilites ci-dessus.
!
!     Les blancs ne sont pas obligatoires, ils sont neutres et utilises
!     ci-dessus pour des questions de clarte.
!
!
TYPE(LFICOM) :: LFI
CHARACTER CDSTRU*(*), CLSTRU*(LFI%JPNCPN)
!
INTEGER (KIND=JPLIKB) KREP, KRANG, KRANIE, KLONG 
INTEGER (KIND=JPLIKB) IRANG, ILCLST, ILUSTR
INTEGER (KIND=JPLIKB) J, INMOTS, INIPAR, IDEXPL, INGROU 
INTEGER (KIND=JPLIKB) IJ, IDECAL, IPOSTY
INTEGER (KIND=JPLIKB) INIMES, INUMER, IDECGR, INTYPE
!
CHARACTER(LEN=LFI%JPLSPX) CLNSPR
CHARACTER(LEN=LFI%JPLMES) CLMESS
CHARACTER(LEN=LFI%JPLFTX) CLACTI
LOGICAL LLFATA

!**
!     1.  -  CONTROLES DES PARAMETRES D'APPEL, PUIS INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('LFIDST_FORT',0,ZHOOK_HANDLE)
CLACTI=''
ILUSTR=INT (LEN (CDSTRU), JPLIKB)
CLSTRU=' '
ILCLST=1
!
IF (KRANG.LE.0.OR.KRANG.GT.LFI%JPNXFI.OR.ILUSTR.LE.0.OR. &
&    KRANIE.LE.0.OR.KRANIE.GT.LFI%JPIMEX) THEN
  KREP=-16
  ILCLST=MIN (LFI%JPNCPN,INT (LEN (CLSTRU), JPLIKB))
  CLSTRU=LFI%CHINCO(:ILCLST)
  GOTO 1001
ENDIF
!
IRANG=KRANG
KREP=0
ILCLST=MIN (ILUSTR,INT (LEN (CLSTRU), JPLIKB))
CLSTRU=CDSTRU(:ILCLST)
INMOTS=0
INIPAR=0
INGROU=0
IDECGR=0
IDEXPL=LFI%NREXPL(LFI%NAEXPL(KRANG),KRANIE)
IJ=0
!**
!     2.  -  DECORTICAGE PROPREMENT DIT.
!-----------------------------------------------------------------------
!*
!     2.1 -  DEBUT "BOUCLE" SUR LES GROUPES.
!-----------------------------------------------------------------------
!
211 CONTINUE
!
IDECAL=IJ
!
IF (IDECAL.GE.ILUSTR) GOTO 301
!
DO J=IDECAL+1,ILUSTR
!
IF (CDSTRU(J:J).EQ.'(') THEN
!
  INGROU=INGROU+1
  INIPAR=INIPAR+1
!
  IF (INIPAR.GT.1) THEN
    KREP=-40
    GOTO 1001
  ENDIF
!
  IJ=J
!
ELSEIF (CDSTRU(J:J).NE.' ') THEN
!
  IPOSTY=INT (INDEX (LFI%CTYPMX,CDSTRU(J:J)), JPLIKB)
!
  IF (IPOSTY.EQ.0) THEN
    KREP=-40
    GOTO 1001
  ENDIF
!
  INGROU=INGROU+1
  IJ=J-1
!
ENDIF
!      
ENDDO
!*
!     2.2 -  DEBUT "BOUCLE" SUR LES TYPES.
!-----------------------------------------------------------------------
!
INTYPE=0
!
IDECAL=IJ
!
DO J=IDECAL+1,ILUSTR
!
IF (CDSTRU(J:J).EQ.')') THEN
!
  INIPAR=INIPAR-1
!
  IF (INTYPE.EQ.0.OR.INIPAR.NE.0) THEN
    KREP=-40
    GOTO 1001
  ENDIF
!
  IJ=J
  
  GOTO 211
!
ELSEIF (CDSTRU(J:J).NE.' ') THEN
!
  IPOSTY=INT (INDEX (LFI%CTYPMX,CDSTRU(J:J)), JPLIKB)
!
  IF (IPOSTY.EQ.0) THEN
    KREP=-40
    GOTO 1001
  ENDIF
!
  INTYPE=INTYPE+1
!
  IF ((IDEXPL+2).GT.LFI%JPDEXP) THEN
    KREP=-42
    GOTO 1001
  ENDIF
!
  LFI%NDEXPL(IDEXPL+1,KRANIE)=IPOSTY
!
  IF (J.EQ.ILUSTR) THEN
!
    IF (INIPAR.EQ.0) THEN
      LFI%NDEXPL(IDEXPL+2,KRANIE)=LFI%JPNIL
      IDEXPL=IDEXPL+2
      GOTO 301
    ELSE
      KREP=-40
      GOTO 1001
    ENDIF
!
  ELSE
!
    CALL LFICHI_FORT                               &
&                   (LFI, KREP,CDSTRU(J+1:ILUSTR), &
&                    LFI%NDEXPL(IDEXPL+2,KRANIE),  &
&                 IJ)
    IF (KREP.NE.0) GOTO 1001
!
    IDEXPL=IDEXPL+2
!
  ENDIF
! 
ENDIF
! 
ENDDO
!


!
301 CONTINUE
!


!**
!    10.  -  PHASE TERMINALE : MESSAGERIE INTERNE EVENTUELLE,
!            VIA LE SOUS-PROGRAMME "LFIEMS", PUIS RETOUR.
!-----------------------------------------------------------------------
!
1001 CONTINUE
LLFATA=LLMOER (KREP,KRANG)
!
IF (LFI%LMISOP.OR.LLFATA) THEN
  INUMER=LFI%NUMERO(KRANG)
  INIMES=2
  CLNSPR='LFIDST'
  WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KRANG='',I3, &
&         '''''', CDSTRU='''''',A,'''''', KLONG='',I7)')  &
&    KREP,KRANG,CLSTRU(:ILCLST),KLONG
  CALL LFIEMS_FORT                                  &
&                 (LFI, INUMER,INIMES,KREP,.FALSE., &
&                  CLMESS,CLNSPR,CLACTI)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('LFIDST_FORT',1,ZHOOK_HANDLE)

CONTAINS

#include "lficom2.llmoer.h"

END SUBROUTINE LFIDST_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFIDST64                            &
&           (KREP, KRANG, KRANIE, CDSTRU, KLONG)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KRANG                                  ! IN   
INTEGER (KIND=JPLIKB)  KRANIE                                 ! IN   
CHARACTER (LEN=*)      CDSTRU                                 ! IN   
INTEGER (KIND=JPLIKB)  KLONG                                  ! IN   

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIDST_FORT                                    &
&           (LFI, KREP, KRANG, KRANIE, CDSTRU, KLONG)

END SUBROUTINE LFIDST64

SUBROUTINE LFIDST                              &
&           (KREP, KRANG, KRANIE, CDSTRU, KLONG)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KRANG                                  ! IN   
INTEGER (KIND=JPLIKM)  KRANIE                                 ! IN   
CHARACTER (LEN=*)      CDSTRU                                 ! IN   
INTEGER (KIND=JPLIKM)  KLONG                                  ! IN   

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIDST_MT                                      &
&           (LFI, KREP, KRANG, KRANIE, CDSTRU, KLONG)

END SUBROUTINE LFIDST

SUBROUTINE LFIDST_MT                                &
&           (LFI, KREP, KRANG, KRANIE, CDSTRU, KLONG)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KRANG                                  ! IN   
INTEGER (KIND=JPLIKM)  KRANIE                                 ! IN   
CHARACTER (LEN=*)      CDSTRU                                 ! IN   
INTEGER (KIND=JPLIKM)  KLONG                                  ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  IRANG                                  ! IN   
INTEGER (KIND=JPLIKB)  IRANIE                                 ! IN   
INTEGER (KIND=JPLIKB)  ILONG                                  ! IN   
! Convert arguments

IRANG      = INT (     KRANG, JPLIKB)
IRANIE     = INT (    KRANIE, JPLIKB)
ILONG      = INT (     KLONG, JPLIKB)

CALL LFIDST_FORT                                    &
&           (LFI, IREP, IRANG, IRANIE, CDSTRU, ILONG)

KREP       = INT (      IREP, JPLIKM)

END SUBROUTINE LFIDST_MT

!INTF KREP            OUT 
!INTF KRANG         IN    
!INTF KRANIE        IN    
!INTF CDSTRU        IN    
!INTF KLONG         IN    
