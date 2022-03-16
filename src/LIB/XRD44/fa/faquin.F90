! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FAQUIN_FORT                                             &
&                     (FA,  KREP, KNUMER, CDPREF, KNIVAU, CDSUFF,  &
&                      CDNOMA, KLNOMA)
USE FA_MOD, ONLY : FA_COM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!      Sous-programme de decomposition du nom d'un article associe a un 
!     champ.
!**
!    Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                KNUMER (Entree) ==> Numero de l'unite logique;
!                CDPREF (Sortie) ==> Prefixe eventuel du nom d'article;
!                KNIVAU (Sortie) ==> Niveau vertical eventuel;
!                CDSUFF (Sortie) ==> Suffixe eventuel du nom d'article;
!                CDNOMA (Entree) ==> Nom de l'article LFI
!                KLNOMA (Entree) ==> Longueur du nom de l'article LFI
!     
!    P MARGUINAUD 18/03/2013 CREATION
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KNUMER, KNIVAU, KLNOMA
!
INTEGER (KIND=JPLIKB) IREP, ILPRFU, ILSUFU, IRANG, INIMES, INIVAU
INTEGER (KIND=JPLIKB) IB1PAR (FA%JPLB1P), ILNOMA
!
LOGICAL LLVERF, LLRLFI
!
CHARACTER CDPREF*(*), CDSUFF*(*), CDNOMA*(*)
!
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
CHARACTER(LEN=FA%JPXNOM) CLNOMA
LOGICAL                  LLFATA

!**
!     1.  -  CONTROLES ET INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FAQUIN_MT',0,ZHOOK_HANDLE)

IREP=0

LLVERF=.FALSE.
LLRLFI=.FALSE.
ILPRFU=INT (LEN (CDPREF), JPLIKB)
ILSUFU=INT (LEN (CDSUFF), JPLIKB)

CALL FANUMU_FORT (FA, KNUMER,IRANG)
!
IF (IRANG.EQ.0) THEN
  IREP=-51
  GOTO 1001
ENDIF
!
!         Verrouillage eventuel du fichier.
!
IF (FA%LFAMUL) CALL LFIVER_FORT (FA%LFI, FA%FICHIER(IRANG)%VRFICH,'ON')
LLVERF=FA%LFAMUL
!
IF (FA%FICHIER(IRANG)%LCREAF) THEN
  IREP=-85
  GOTO 1001
ENDIF

CDPREF = ''
CDSUFF = ''
KNIVAU = 0

IF (CDNOMA (1:8) == 'SPECSURF') THEN
  CDPREF = CDNOMA (1:8)
  CDSUFF = CDNOMA (9:KLNOMA)
ELSEIF (CDNOMA (1:4) == 'PROF') THEN
  CDPREF = CDNOMA (1:4)
  CDSUFF = CDNOMA (5:KLNOMA)
ELSEIF (CDNOMA (1:4) == 'SURF') THEN
  CDPREF = CDNOMA (1:4)
  CDSUFF = CDNOMA (5:KLNOMA)
ELSEIF (CDNOMA (1:4) == 'SOMM') THEN
  CDPREF = CDNOMA (1:4)
  CDSUFF = CDNOMA (5:KLNOMA)
ELSEIF (CDNOMA (1:4) == 'ICAO') THEN
  CDPREF = CDNOMA (1:4)
  CDSUFF = CDNOMA (5:KLNOMA)
ELSEIF (CDNOMA (1:4) == 'SFX.') THEN
  CDPREF = CDNOMA (1:4)
  CDSUFF = CDNOMA (5:KLNOMA)
ELSEIF (CDNOMA (1:3) == 'CLS') THEN
  CDPREF = CDNOMA (1:3)
  CDSUFF = CDNOMA (4:KLNOMA)
ELSEIF (CDNOMA (1:3) == 'MSL') THEN
  CDPREF = CDNOMA (1:3)
  CDSUFF = CDNOMA (4:KLNOMA)
ELSEIF (CDNOMA (1:3) == 'CLP') THEN
  CDPREF = CDNOMA (1:3)
  CDSUFF = CDNOMA (4:KLNOMA)
ELSEIF (CDNOMA (1:3) == 'JET') THEN
  CDPREF = CDNOMA (1:3)
  CDSUFF = CDNOMA (4:KLNOMA)
ELSEIF (CDNOMA (1:3) == 'INT') THEN
  CDPREF = CDNOMA (1:3)
  CDSUFF = CDNOMA (4:KLNOMA)
ELSEIF (CDNOMA (1:2) == 'KT' .AND. LNUM (CDNOMA(3:KLNOMA), INIVAU, '(I3.3)')) THEN
  CDPREF = CDNOMA (1:2)
  CDSUFF = CDNOMA (6:KLNOMA)
  KNIVAU = INIVAU
ELSEIF (CDNOMA (1:2) == 'KB' .AND. LNUM (CDNOMA(3:KLNOMA), INIVAU, '(I3.3)')) THEN
  CDPREF = CDNOMA (1:2)
  CDSUFF = CDNOMA (6:KLNOMA)
  KNIVAU = INIVAU
ELSEIF (CDNOMA (1:1) == 'P' .AND. LNUM (CDNOMA(2:KLNOMA), INIVAU, '(I5.5)')) THEN
  CDPREF = CDNOMA (1:1)
  CDSUFF = CDNOMA (7:KLNOMA)
  KNIVAU = INIVAU
  IF (KNIVAU == 0) KNIVAU = 100000
ELSEIF (CDNOMA (1:1) == 'H' .AND. LNUM (CDNOMA(2:KLNOMA), INIVAU, '(I5.5)')) THEN
  CDPREF = CDNOMA (1:1)
  CDSUFF = CDNOMA (7:KLNOMA)
  KNIVAU = INIVAU
ELSEIF (CDNOMA (1:1) == 'V' .AND. LNUM (CDNOMA(2:KLNOMA), INIVAU, '(I3.3)')) THEN
  CDPREF = CDNOMA (1:1)
  CDSUFF = CDNOMA (5:KLNOMA)
  KNIVAU = INIVAU
ELSEIF (CDNOMA (1:1) == 'T' .AND. LNUM (CDNOMA(2:KLNOMA), INIVAU, '(I3.3)')) THEN
  CDPREF = CDNOMA (1:1)
  CDSUFF = CDNOMA (5:KLNOMA)
  KNIVAU = INIVAU
ELSEIF (CDNOMA (1:1) == 'S' .AND. LNUM (CDNOMA(2:KLNOMA), INIVAU, '(I3.3)')) THEN
  CDPREF = CDNOMA (1:1)
  CDSUFF = CDNOMA (5:KLNOMA)
  KNIVAU = INIVAU
ELSEIF (CDNOMA (1:1) == 'X' .AND. LNUM (CDNOMA(2:KLNOMA), INIVAU, '(I3.3)')) THEN
  CDPREF = CDNOMA (1:1)
  CDSUFF = CDNOMA (5:KLNOMA)
  KNIVAU = INIVAU
ELSE
  CDPREF = CDNOMA (1:4)
  CDSUFF = CDNOMA (5:)
ENDIF

ILNOMA=FA%JPXNOM
CALL FANFAN_FORT                                          &
&            (FA,  IREP, KNUMER, CDPREF, KNIVAU, CDSUFF,  &
&             CLNOMA, ILNOMA)

IF (IREP.NE.0) GOTO 1001

IF (CLNOMA (1:ILNOMA) /= CDNOMA (1:KLNOMA)) THEN
  IREP=-65
ENDIF

1001 CONTINUE
KREP=IREP
LLFATA=LLMOER (IREP,IRANG)
!
!        Deverrouillage eventuel du fichier.
!
IF (LLVERF) CALL LFIVER_FORT (FA%LFI, FA%FICHIER(IRANG)%VRFICH,'OFF')

IF (LLFATA) THEN
  INIMES=2
ELSE
  INIMES=IXNVMS(IRANG)
ENDIF
!
IF (.NOT.LLFATA.AND.INIMES.NE.2)  THEN 
  IF (LHOOK) CALL DR_HOOK('FAQUIN_MT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='FAQUIN'
!
WRITE (UNIT=CLMESS,FMT='(''KREP='',I5,'', KNUMER='',I3, &
&       '', CDPREF='''''',A,'''''', KNIVAU='',I6,       &
&       '', CDSUFF='''''',A,'''')')                     &
&   KREP,KNUMER,CDPREF(1:ILPRFU),KNIVAU,CDSUFF(1:ILSUFU)
CALL FAIPAR_FORT                                       &
&               (FA, KNUMER,INIMES,IREP,LLFATA,CLMESS, &
&                CLNSPR, CDNOMA(1:KLNOMA),LLRLFI)
!
IF (LHOOK) CALL DR_HOOK('FAQUIN_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"
#include "facom2.ixnvms.h"

LOGICAL FUNCTION LNUM (CDST, KNUM, CDFMT)

CHARACTER (LEN=*) :: CDST, CDFMT
INTEGER (KIND=JPLIKB) :: KNUM
INTEGER :: IREP

IREP = 0
READ (UNIT=CDST, FMT=CDFMT, IOSTAT=IREP) KNUM

IF (IREP == 0) THEN
  LNUM = .TRUE.
ELSE
  LNUM = .FALSE.
  KNUM = 0
ENDIF

END FUNCTION LNUM

END SUBROUTINE FAQUIN_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FAQUIN64                                        &
&           (KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, CDNOMA, &
&            KLNOMA)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                  FA_COM_DEFAULT_INIT,  &
&                  NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDPREF                                 !   OUT
INTEGER (KIND=JPLIKB)  KNIVAU                                 !   OUT
CHARACTER (LEN=*)      CDSUFF                                 !   OUT
CHARACTER (LEN=*)      CDNOMA                                 ! IN   
INTEGER (KIND=JPLIKB)  KLNOMA                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAQUIN_FORT                                               &
&           (FA, KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, CDNOMA, &
&            KLNOMA)

END SUBROUTINE FAQUIN64

SUBROUTINE FAQUIN                                          &
&           (KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, CDNOMA, &
&            KLNOMA)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                  FA_COM_DEFAULT_INIT,  &
&                  NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDPREF                                 !   OUT
INTEGER (KIND=JPLIKM)  KNIVAU                                 !   OUT
CHARACTER (LEN=*)      CDSUFF                                 !   OUT
CHARACTER (LEN=*)      CDNOMA                                 ! IN   
INTEGER (KIND=JPLIKM)  KLNOMA                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAQUIN_MT                                                 &
&           (FA, KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, CDNOMA, &
&            KLNOMA)

END SUBROUTINE FAQUIN

SUBROUTINE FAQUIN_MT                                           &
&           (FA, KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, CDNOMA, &
&            KLNOMA)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDPREF                                 !   OUT
INTEGER (KIND=JPLIKM)  KNIVAU                                 !   OUT
CHARACTER (LEN=*)      CDSUFF                                 !   OUT
CHARACTER (LEN=*)      CDNOMA                                 ! IN   
INTEGER (KIND=JPLIKM)  KLNOMA                                 ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  INIVAU                                 !   OUT
INTEGER (KIND=JPLIKB)  ILNOMA                                 ! IN   
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)
ILNOMA     = INT (    KLNOMA, JPLIKM)

CALL FAQUIN_FORT                                               &
&           (FA, IREP, INUMER, CDPREF, INIVAU, CDSUFF, CDNOMA, &
&            ILNOMA)

KREP       = INT (      IREP, JPLIKM)
KNIVAU     = INT (    INIVAU, JPLIKB)

END SUBROUTINE FAQUIN_MT

!INTF KREP            OUT 
!INTF KNUMER        IN    
!INTF CDPREF          OUT 
!INTF KNIVAU          OUT 
!INTF CDSUFF          OUT 
!INTF CDNOMA        IN    
!INTF KLNOMA        IN    

