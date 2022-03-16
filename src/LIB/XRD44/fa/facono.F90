! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FACONO_FORT                                             &
&                     (FA, KREP, KNUMER, CDPREF, KNIVAU, CDSUFF,   &
&                      PCHAMP, LDCOSP, CDNOMA, KLNOMA, PVALCO,     &
&                      KLONGD, LDUNDF, PUNDF)
USE FA_MOD, ONLY : FA_COM, FAGR1TAB
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!      Sous-programme de CODAGE d'un CHAMP HORIZONTAL destine a etre
!      ecrit sur un fichier ARPEGE/ALADIN, avec reordonnement des 
!      coefficients spectraux, le cas echeant (LDCOSP=.TRUE.)
!       ( COdage de (Nouvelles ?) Donnees )
!**
!    Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                KNUMER (Entree) ==> Numero de l'unite logique;
!                CDPREF (Entree) ==> Prefixe eventuel du nom d'article;
!                KNIVAU (Entree) ==> Niveau vertical eventuel;
!                CDSUFF (Entree) ==> Suffixe eventuel du nom d'article;
!    ( Tableau ) PCHAMP (Entree) ==> Valeurs REELLES du champ a ecrire;
!                                    rangement modele
!                LDCOSP (Entree) ==> Vrai si le champ est represente
!                                    par des coefficients spectraux;
!                CDNOMA (Sortie) ==> Nom de l'article-champ a ecrire;
!                KLNOMA (Sortie) ==> Nombre de caracteres utiles dans
!                                    CDNOMA;
!    ( Tableau ) PVALCO (Sortie) ==> Donnees destinees a l'ecriture;
!                KLONGD (Entree/Sortie) 
!                                ==> Nombre de valeurs (mots de 64 bits
!                                    en principe) a ecrire.
!                LDUNDF (Entree) ==> Vrai si ce champ a des valeurs 
!                                    indefinies
!                PUNDF  (Entree) ==> Dans le cas ou LDUNDF est vrai,
!                                    valeur non definie
!
TYPE(FA_COM)           FA
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDPREF                                 ! IN   
INTEGER (KIND=JPLIKB)  KNIVAU                                 ! IN   
CHARACTER (LEN=*)      CDSUFF                                 ! IN   
REAL (KIND=JPDBLR)     PCHAMP     (*)                         ! IN   
LOGICAL                LDCOSP                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 !   OUT
INTEGER (KIND=JPLIKB)  KLNOMA                                 !   OUT
REAL (KIND=JPDBLR)     PVALCO     (*)                         !   OUT
INTEGER (KIND=JPLIKB)  KLONGD                                 !   OUT
LOGICAL,               OPTIONAL :: LDUNDF                     ! IN
REAL (KIND=JPDBLR),    OPTIONAL :: PUNDF                      ! IN
!
INTEGER (KIND=JPLIKB) IREP
INTEGER (KIND=JPLIKB) IRANG, INIMES
INTEGER (KIND=JPLIKB) IRANGC
INTEGER (KIND=JPLIKB) ISMAX, IMSMAX
INTEGER (KIND=JPLIKB) INGRIB
!
LOGICAL LLVERF, LLRLFI
!
REAL (KIND=JPDBLR), ALLOCATABLE :: ZCHAMP (:)
LOGICAL               :: LLUNDF                   
REAL (KIND=JPDBLR)    :: ZUNDF                    
!
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA
LOGICAL                  LLREORD
TYPE (FAGR1TAB)          YLGR1TAB

!**
!     1.  -  CONTROLES ET INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FACONO_MT',0,ZHOOK_HANDLE)

LLUNDF = .FALSE.
IF (PRESENT (LDUNDF  )) LLUNDF   = LDUNDF
ZUNDF  = 0._JPDBLR
IF (PRESENT (PUNDF   )) ZUNDF    = PUNDF 

IREP=0
LLRLFI=.FALSE.
CDNOMA=' '
CALL FANUMU_FORT                 &
&               (FA, KNUMER,IRANG)
!
IF (IRANG.EQ.0) THEN
  IREP=-51
  GOTO 1001
ENDIF
!
!         Verrouillage eventuel du fichier.
!
IF (FA%LFAMUL) CALL LFIVER_FORT                               &
&                              (FA%LFI, FA%FICHIER(IRANG)%VRFICH,'ON')
LLVERF=FA%LFAMUL
!
IRANGC=FA%FICHIER(IRANG)%NUCADR
INGRIB=FA%FICHIER(IRANG)%NFGRIB

LLREORD = LDCOSP .AND. (.NOT.(INGRIB==-1 .OR. INGRIB==3 .OR. FALGRA (INGRIB)))

IF (LLREORD) THEN
  ISMAX  = FA%CADRE(IRANGC)%NSMAX     
  IMSMAX = FA%CADRE(IRANGC)%NMSMAX     
  ALLOCATE (ZCHAMP (4 * (IMSMAX+1) * (ISMAX+1))) ! Assez grand
  CALL FAREOR_FORT (FA, IREP, KNUMER, PCHAMP, ZCHAMP, .FALSE.)
  IF (IREP /= 0) GOTO 1001
  CALL FACON1_FORT (FA, IREP, KNUMER, CDPREF, KNIVAU, CDSUFF, ZCHAMP, &
                  & LDCOSP, CDNOMA, KLNOMA, PVALCO, KLONGD,           &
                  & LLUNDF, ZUNDF, YLGR1TAB)
  IF (IREP /= 0) GOTO 1001
  DEALLOCATE (ZCHAMP)
ELSE
  CALL FACON1_FORT (FA, IREP, KNUMER, CDPREF, KNIVAU, CDSUFF, PCHAMP, &
                  & LDCOSP, CDNOMA, KLNOMA, PVALCO, KLONGD,           &
                  & LLUNDF, ZUNDF, YLGR1TAB)
ENDIF


1001 CONTINUE
KREP=IREP
LLFATA=LLMOER (IREP,IRANG)
!
!        Deverrouillage eventuel du fichier.
!
IF (LLVERF) CALL LFIVER_FORT                                &
&                           (FA%LFI, FA%FICHIER(IRANG)%VRFICH,'OFF')
!
IF (LLFATA) THEN
  INIMES=2
ELSE
  INIMES=IXNVMS(IRANG)
ENDIF
!
IF (.NOT.LLFATA.AND.INIMES.NE.2)  THEN 
  IF (LHOOK) CALL DR_HOOK('FACONO_MT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='FACONO'
!
WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KNUMER='',I3,        &
&       '', CDPREF='''''',A,'''''', KNIVAU='',I6,              &
&       '', CDSUFF='''''',A,'''''', LDCOSP= '',L1)')           &
&   KREP,KNUMER,TRIM (CDPREF),KNIVAU,TRIM (CDSUFF),LDCOSP
CALL FAIPAR_FORT                                     &
&               (FA, KNUMER,INIMES,IREP,LLFATA,CLMESS, &
&                CLNSPR,TRIM (CDNOMA),LLRLFI)
!
IF (LHOOK) CALL DR_HOOK('FACONO_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"
#include "facom2.ixnvms.h"
#include "falgra.h"

END SUBROUTINE FACONO_FORT

! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FACONO64                                        &
&           (KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, PCHAMP, &
&            LDCOSP, CDNOMA, KLNOMA, PVALCO, KLONGD,       &
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
REAL (KIND=JPDBLR)     PCHAMP     (*)                         ! IN   
LOGICAL                LDCOSP                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 !   OUT
INTEGER (KIND=JPLIKB)  KLNOMA                                 !   OUT
REAL (KIND=JPDBLR)     PVALCO     (*)                         !   OUT
INTEGER (KIND=JPLIKB)  KLONGD                                 !   OUT
LOGICAL,               OPTIONAL :: LDUNDF                     ! IN
REAL (KIND=JPDBLR),    OPTIONAL :: PUNDF                      ! IN

#include "facono_mt64.h"

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FACONO_FORT                                               &
&           (FA, KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, PCHAMP, &
&            LDCOSP, CDNOMA, KLNOMA, PVALCO, KLONGD,           &
&            LDUNDF, PUNDF)

END SUBROUTINE FACONO64

SUBROUTINE FACONO                                          &
&           (KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, PCHAMP, &
&            LDCOSP, CDNOMA, KLNOMA, PVALCO, KLONGD,       &
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
REAL (KIND=JPDBLR)     PCHAMP     (*)                         ! IN   
LOGICAL                LDCOSP                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 !   OUT
INTEGER (KIND=JPLIKM)  KLNOMA                                 !   OUT
REAL (KIND=JPDBLR)     PVALCO     (*)                         !   OUT
INTEGER (KIND=JPLIKM)  KLONGD                                 !   OUT
LOGICAL,               OPTIONAL :: LDUNDF                     ! IN
REAL (KIND=JPDBLR),    OPTIONAL :: PUNDF                      ! IN

#include "facono_mt.h"

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FACONO_MT                                                 &
&           (FA, KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, PCHAMP, &
&            LDCOSP, CDNOMA, KLNOMA, PVALCO, KLONGD,           &
&            LDUNDF, PUNDF)

END SUBROUTINE FACONO

SUBROUTINE FACONO_MT                                           &
&           (FA, KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, PCHAMP, &
&            LDCOSP, CDNOMA, KLNOMA, PVALCO, KLONGD,           &
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
REAL (KIND=JPDBLR)     PCHAMP     (*)                         ! IN   
LOGICAL                LDCOSP                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 !   OUT
INTEGER (KIND=JPLIKM)  KLNOMA                                 !   OUT
REAL (KIND=JPDBLR)     PVALCO     (*)                         !   OUT
INTEGER (KIND=JPLIKM)  KLONGD                                 !   OUT
LOGICAL,               OPTIONAL :: LDUNDF                     ! IN
REAL (KIND=JPDBLR),    OPTIONAL :: PUNDF                      ! IN

#include "facono_mt64.h"

! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  INIVAU                                 ! IN   
INTEGER (KIND=JPLIKB)  ILNOMA                                 !   OUT
INTEGER (KIND=JPLIKB)  ILONGD                                 !   OUT
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)
INIVAU     = INT (    KNIVAU, JPLIKB)
ILONGD     = INT (    KLONGD, JPLIKB)

CALL FACONO_FORT                                               &
&           (FA, IREP, INUMER, CDPREF, INIVAU, CDSUFF, PCHAMP, &
&            LDCOSP, CDNOMA, ILNOMA, PVALCO, ILONGD,           &
&            LDUNDF, PUNDF)

KREP       = INT (      IREP, JPLIKM)
KLNOMA     = INT (    ILNOMA, JPLIKM)
KLONGD     = INT (    ILONGD, JPLIKM)

END SUBROUTINE FACONO_MT

!INTF KREP            OUT                               
!INTF KNUMER        IN                                  
!INTF CDPREF        IN                                  
!INTF KNIVAU        IN                                  
!INTF CDSUFF        IN                                  
!INTF PCHAMP        IN    DIMS=*                        
!INTF LDCOSP        IN                                  
!INTF CDNOMA          OUT                               
!INTF KLNOMA          OUT                               
!INTF PVALCO          OUT DIMS=*                        
!INTF KLONGD        INOUT                               
!INTF LDUNDF        IN                                  
!INTF PUNDF         IN                                  

