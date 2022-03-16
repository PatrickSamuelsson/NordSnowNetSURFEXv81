! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FADOCO_FORT                                              &
&                     (FA,  KREP,   KNUMER, CDPREF, KNIVAU, CDSUFF, &
&                      LDCOSP, CDNOMA, KLNOMA, PVALCO, KLONGD,      &
&                      PCHAMP, LDUNDF, PUNDF)
USE FA_MOD, ONLY : FA_COM, FAGR1TAB, JPPRCM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!
!****
!      Sous-programme de controle et de DECODAGE d'un CHAMP HORIZONTAL
!      venant d'etre lu sur un fichier ARPEGE/ALADIN, avec reordonnement
!      des coefficients spectraux, le cas echeant.
!       ( DECOdage de donnees )
!**
!    Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                KNUMER (Entree) ==> Numero de l'unite logique;
!                CDPREF (Entree) ==> Prefixe eventuel du nom d'article;
!                KNIVAU (Entree) ==> Niveau vertical eventuel;
!                CDSUFF (Entree) ==> Suffixe eventuel du nom d'article;
!                LDCOSP (Entree) ==> Vrai si le champ est represente
!                                    par des coefficients spectraux;
!                CDNOMA (Sortie) ==> Nom de l'article-champ lu;
!                KLNOMA (Sortie) ==> Nombre de caracteres utiles dans
!                                    CDNOMA;
!    ( Tableau ) PVALCO (Entree) ==> Donnees issues de la lecture;
!                KLONGD (Entree) ==> Nombre de valeurs (mots de 64 bits
!                                    en principe) lues;
!    ( Tableau ) PCHAMP (Sortie) ==> Valeurs REELLES du champ lu (ordre du
!                                    modele).
!                LDUNDF (Sortie) ==> Vrai si ce champ a des valeurs 
!                                    indefinies
!                PUNDF  (Sortie) ==> Dans le cas ou LDUNDF est vrai,
!                                    valeur non definie
!
!    Remarques:
!
!    - PVALCO est type entier, et doit avoir une longueur
!      suffisante pour stocker les donnees codees. Le dimensionnement
!      "tous terrains" est (2+ILCHAM), qui permet le cas echeant de
!      stocker un champ a pleine resolution sans codage effectif.
!      (ILCHAM est le nombre de valeurs du champ a decoder)
!
!    - CDNOMA doit avoir au moins FA%JPXNOM caracteres.
!
!
TYPE(FA_COM)           FA
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDPREF                                 ! IN   
INTEGER (KIND=JPLIKB)  KNIVAU                                 ! IN   
CHARACTER (LEN=*)      CDSUFF                                 ! IN   
LOGICAL                LDCOSP                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 !   OUT
INTEGER (KIND=JPLIKB)  KLNOMA                                 !   OUT
REAL (KIND=JPDBLR)     PVALCO     (*)                         ! IN   
INTEGER (KIND=JPLIKB)  KLONGD                                 ! IN   
REAL (KIND=JPDBLR)     PCHAMP     (*)                         !   OUT
LOGICAL,               OPTIONAL :: LDUNDF                     !   OUT
REAL (KIND=JPDBLR),    OPTIONAL :: PUNDF                      !   OUT
!
REAL (KIND=JPDBLR), ALLOCATABLE :: ZCHAMP (:)
INTEGER (KIND=JPLIKB) IRANG, IRANGC, INIMES
INTEGER (KIND=JPLIKB) ISMAX, IMSMAX
INTEGER (KIND=JPLIKB) INGRIB
INTEGER (KIND=JPLIKB) IREP
LOGICAL LLREORD
!
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA
LOGICAL                  LLRLFI
LOGICAL               :: LLUNDF
REAL (KIND=JPDBLR)    :: ZUNDF 
TYPE (FAGR1TAB)       :: YLGR1TAB

!**
!     1.  -  CONTROLES ET INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FADOCO_MT',0,ZHOOK_HANDLE)

LLUNDF = .FALSE.
IF (PRESENT (LDUNDF   )) LLUNDF   = LDUNDF 
ZUNDF  = 0._JPDBLR
IF (PRESENT (PUNDF    )) ZUNDF    = PUNDF  

IREP=0
LLRLFI=.FALSE.
KLNOMA=0

CALL FANUMU_FORT                 &
&               (FA, KNUMER,IRANG)
!
IF (IRANG.EQ.0) THEN
  IREP=-51
  GOTO 1001
  CDNOMA=' '
ENDIF

INGRIB=TRANSFER (PVALCO (1:JPPRCM), INGRIB)
IRANGC=FA%FICHIER(IRANG)%NUCADR
LLREORD = LDCOSP .AND. (.NOT.(INGRIB==-1 .OR. INGRIB==3 .OR. FALGRA (INGRIB)))

IF (LLREORD) THEN
  ISMAX  = FA%CADRE(IRANGC)%NSMAX     
  IMSMAX = FA%CADRE(IRANGC)%NMSMAX     
  ALLOCATE (ZCHAMP (4 * (IMSMAX+1) * (ISMAX+1))) ! Assez grand

  CALL FADEC1_FORT (FA, IREP, KNUMER, CDPREF, KNIVAU, CDSUFF, &
                  & LDCOSP, CDNOMA, KLNOMA, PVALCO, KLONGD,   &
                  & ZCHAMP, LLUNDF, ZUNDF, YLGR1TAB)

  IF (IREP /= 0) GOTO 1001
  CALL FAREOR_FORT (FA, IREP, KNUMER, PCHAMP, ZCHAMP, .TRUE.)
  IF (IREP /= 0) GOTO 1001
  DEALLOCATE (ZCHAMP)
ELSE
  CALL FADEC1_FORT (FA, IREP, KNUMER, CDPREF, KNIVAU, CDSUFF, &
                  & LDCOSP, CDNOMA, KLNOMA, PVALCO, KLONGD,   &
                  & PCHAMP, LLUNDF, ZUNDF, YLGR1TAB)
ENDIF

1001 CONTINUE
KREP=IREP
LLFATA=LLMOER (IREP,IRANG)
!
IF (LLFATA) THEN
  INIMES=2
ELSE
  INIMES=IXNVMS(IRANG)
ENDIF
!

IF (PRESENT (LDUNDF  )) LDUNDF   = LLUNDF 
IF (PRESENT (PUNDF   )) PUNDF    = ZUNDF  

IF (.NOT.LLFATA.AND.INIMES.NE.2)  THEN 
  IF (LHOOK) CALL DR_HOOK('FADOCO_MT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='FADOCO'
!
WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KNUMER='',I3,         &
&       '', CDPREF='''''',A,'''''', KNIVAU='',I6,               &
&       '', CDSUFF='''''',A,'''''', LDCOSP= '',L1)')            &
&   KREP,KNUMER,TRIM (CDPREF),KNIVAU,TRIM (CDSUFF),LDCOSP
CALL FAIPAR_FORT                                       &
&               (FA, KNUMER,INIMES,IREP,LLFATA,CLMESS, &
&                CLNSPR,TRIM (CDNOMA))
!
IF (LHOOK) CALL DR_HOOK('FADOCO_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"
#include "facom2.ixnvms.h"
#include "falgra.h"

END SUBROUTINE FADOCO_FORT

! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FADOCO64                                        &
&           (KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, LDCOSP, &
&            CDNOMA, KLNOMA, PVALCO, KLONGD, PCHAMP,       &
&            LDUNDF, PUNDF)
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
LOGICAL                LDCOSP                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 !   OUT
INTEGER (KIND=JPLIKB)  KLNOMA                                 !   OUT
REAL (KIND=JPDBLR)     PVALCO     (*)                         ! IN   
INTEGER (KIND=JPLIKB)  KLONGD                                 ! IN   
REAL (KIND=JPDBLR)     PCHAMP     (*)                         !   OUT
LOGICAL,               OPTIONAL :: LDUNDF                     !   OUT
REAL (KIND=JPDBLR),    OPTIONAL :: PUNDF                      !   OUT

#include "fadoco_mt64.h"

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FADOCO_FORT                                               &
&           (FA, KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, LDCOSP, &
&            CDNOMA, KLNOMA, PVALCO, KLONGD, PCHAMP,           &
&            LDUNDF, PUNDF)

END SUBROUTINE FADOCO64

SUBROUTINE FADOCO                                          &
&           (KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, LDCOSP, &
&            CDNOMA, KLNOMA, PVALCO, KLONGD, PCHAMP,       &
&            LDUNDF, PUNDF)
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
LOGICAL                LDCOSP                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 !   OUT
INTEGER (KIND=JPLIKM)  KLNOMA                                 !   OUT
REAL (KIND=JPDBLR)     PVALCO     (*)                         ! IN   
INTEGER (KIND=JPLIKM)  KLONGD                                 ! IN   
REAL (KIND=JPDBLR)     PCHAMP     (*)                         !   OUT
LOGICAL,               OPTIONAL :: LDUNDF                     !   OUT
REAL (KIND=JPDBLR),    OPTIONAL :: PUNDF                      !   OUT

#include "fadoco_mt.h"

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FADOCO_MT                                                 &
&           (FA, KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, LDCOSP, &
&            CDNOMA, KLNOMA, PVALCO, KLONGD, PCHAMP,           &
&            LDUNDF, PUNDF)

END SUBROUTINE FADOCO

SUBROUTINE FADOCO_MT                                           &
&           (FA, KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, LDCOSP, &
&            CDNOMA, KLNOMA, PVALCO, KLONGD, PCHAMP,           &
&            LDUNDF, PUNDF)
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
LOGICAL                LDCOSP                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 !   OUT
INTEGER (KIND=JPLIKM)  KLNOMA                                 !   OUT
REAL (KIND=JPDBLR)     PVALCO     (*)                         ! IN   
INTEGER (KIND=JPLIKM)  KLONGD                                 ! IN   
REAL (KIND=JPDBLR)     PCHAMP     (*)                         !   OUT
LOGICAL,               OPTIONAL :: LDUNDF                     !   OUT
REAL (KIND=JPDBLR),    OPTIONAL :: PUNDF                      !   OUT

#include "fadoco_mt64.h"

! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  INIVAU                                 ! IN   
INTEGER (KIND=JPLIKB)  ILNOMA                                 !   OUT
INTEGER (KIND=JPLIKB)  ILONGD                                 ! IN   
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)
INIVAU     = INT (    KNIVAU, JPLIKB)
ILONGD     = INT (    KLONGD, JPLIKB)

CALL FADOCO_FORT                                               &
&           (FA, IREP, INUMER, CDPREF, INIVAU, CDSUFF, LDCOSP, &
&            CDNOMA, ILNOMA, PVALCO, ILONGD, PCHAMP,           &
&            LDUNDF, PUNDF)

KREP       = INT (      IREP, JPLIKM)
KLNOMA     = INT (    ILNOMA, JPLIKM)


END SUBROUTINE FADOCO_MT

!INTF KREP            OUT                                                              
!INTF KNUMER        IN                                                                 
!INTF CDPREF        IN                                                                 
!INTF KNIVAU        IN                                                                 
!INTF CDSUFF        IN                                                                 
!INTF LDCOSP        IN                                                                 
!INTF CDNOMA          OUT                                                              
!INTF KLNOMA          OUT                                                              
!INTF PVALCO        IN    DIMS=*                         KIND=JPDBLR                   
!INTF KLONGD        IN                                                                 
!INTF PCHAMP          OUT DIMS=*                                                       
!INTF LDUNDF        INOUT                                                              
!INTF PUNDF         INOUT                                                              

