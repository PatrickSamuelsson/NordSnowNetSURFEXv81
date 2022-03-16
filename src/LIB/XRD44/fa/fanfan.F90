! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FANFAN_FORT                                             &
&                     (FA,  KREP, KNUMER, CDPREF, KNIVAU, CDSUFF,  &
&                      CDNOMA, KLNOMA)
USE FA_MOD, ONLY : FA_COM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!      Sous-programme de construction du nom d'un article associe a un 
!     champ.
!**
!    Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                KNUMER (Entree) ==> Numero de l'unite logique;
!                CDPREF (Entree) ==> Prefixe eventuel du nom d'article;
!                KNIVAU (Entree) ==> Niveau vertical eventuel;
!                CDSUFF (Entree) ==> Suffixe eventuel du nom d'article;
!                CDNOMA (Sortie) ==> Nom de l'article LFI
!                KLNOMA (Sortie) ==> Longueur du nom de l'article LFI
!     
!    P MARGUINAUD 30/04/2012 CREATION
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KNUMER, KNIVAU, KLNOMA
!
INTEGER (KIND=JPLIKB) IREP, ILPRFU, ILSUFU, IRANG, INIMES
INTEGER (KIND=JPLIKB) IB1PAR (FA%JPLB1P)
!
LOGICAL LLVERF, LLRLFI
!
CHARACTER CDPREF*(*), CDSUFF*(*), CDNOMA*(*)
!
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA

!**
!     1.  -  CONTROLES ET INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FANFAN_MT',0,ZHOOK_HANDLE)
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
!**
!     2.  -  FABRICATION DU NOM D'ARTICLE VIA LE SOUS-PROGRAMME "FANFAR"
!            ( controles de CDPREF, KNIVAU, CDSUFF inclus )
!-----------------------------------------------------------------------
!
CALL FANFAR_FORT                                             &
&               (FA, IREP,IRANG,CDPREF,KNIVAU,CDSUFF,CDNOMA, &
&                IB1PAR(6),ILPRFU,ILSUFU,KLNOMA)

IF (IREP.NE.0) GOTO 1001
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
  IF (LHOOK) CALL DR_HOOK('FANFAN_MT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='FANFAN'
!
WRITE (UNIT=CLMESS,FMT='(''KREP='',I5,'', KNUMER='',I3, &
&       '', CDPREF='''''',A,'''''', KNIVAU='',I6,       &
&       '', CDSUFF='''''',A,'''')')                     &
&   KREP,KNUMER,CDPREF(1:ILPRFU),KNIVAU,CDSUFF(1:ILSUFU)
CALL FAIPAR_FORT                                       &
&               (FA, KNUMER,INIMES,IREP,LLFATA,CLMESS, &
&                CLNSPR, CDNOMA(1:KLNOMA),LLRLFI)
!
IF (LHOOK) CALL DR_HOOK('FANFAN_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"
#include "facom2.ixnvms.h"

END SUBROUTINE FANFAN_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FANFAN64                                        &
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
CHARACTER (LEN=*)      CDPREF                                 ! IN   
INTEGER (KIND=JPLIKB)  KNIVAU                                 ! IN   
CHARACTER (LEN=*)      CDSUFF                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 !   OUT
INTEGER (KIND=JPLIKB)  KLNOMA                                 !   OUT

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FANFAN_FORT                                               &
&           (FA, KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, CDNOMA, &
&            KLNOMA)

END SUBROUTINE FANFAN64

SUBROUTINE FANFAN                                          &
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
CHARACTER (LEN=*)      CDPREF                                 ! IN   
INTEGER (KIND=JPLIKM)  KNIVAU                                 ! IN   
CHARACTER (LEN=*)      CDSUFF                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 !   OUT
INTEGER (KIND=JPLIKM)  KLNOMA                                 !   OUT

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FANFAN_MT                                                 &
&           (FA, KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, CDNOMA, &
&            KLNOMA)

END SUBROUTINE FANFAN

SUBROUTINE FANFAN_MT                                           &
&           (FA, KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, CDNOMA, &
&            KLNOMA)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDPREF                                 ! IN   
INTEGER (KIND=JPLIKM)  KNIVAU                                 ! IN   
CHARACTER (LEN=*)      CDSUFF                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 !   OUT
INTEGER (KIND=JPLIKM)  KLNOMA                                 !   OUT
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  INIVAU                                 ! IN   
INTEGER (KIND=JPLIKB)  ILNOMA                                 !   OUT
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)
INIVAU     = INT (    KNIVAU, JPLIKB)

CALL FANFAN_FORT                                               &
&           (FA, IREP, INUMER, CDPREF, INIVAU, CDSUFF, CDNOMA, &
&            ILNOMA)

KREP       = INT (      IREP, JPLIKM)
KLNOMA     = INT (    ILNOMA, JPLIKM)

END SUBROUTINE FANFAN_MT

!INTF KREP            OUT 
!INTF KNUMER        IN    
!INTF CDPREF        IN    
!INTF KNIVAU        IN    
!INTF CDSUFF        IN    
!INTF CDNOMA          OUT 
!INTF KLNOMA          OUT 
