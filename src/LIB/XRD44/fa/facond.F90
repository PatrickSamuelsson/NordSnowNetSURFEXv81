! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FACOND_FORT                                              &
&                     (FA,  KREP,   KNUMER, CDPREF, KNIVAU, CDSUFF, &
&                      PCHAMP, LDCOSP, CDNOMA, KLNOMA, PVALCO,      &
&                      KLONGD)
USE FA_MOD, ONLY : FA_COM, FAGR1TAB
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!      Sous-programme de CODAGE d'un CHAMP HORIZONTAL destine a etre
!      ecrit sur un fichier ARPEGE/ALADIN.
!       ( COdage de (Nouvelles ?) Donnees )
!**
!    Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                KNUMER (Entree) ==> Numero de l'unite logique;
!                CDPREF (Entree) ==> Prefixe eventuel du nom d'article;
!                KNIVAU (Entree) ==> Niveau vertical eventuel;
!                CDSUFF (Entree) ==> Suffixe eventuel du nom d'article;
!    ( Tableau ) PCHAMP (Entree) ==> Valeurs REELLES du champ a ecrire;
!                LDCOSP (Entree) ==> Vrai si le champ est represente
!                                    par des coefficients spectraux;
!                CDNOMA (Sortie) ==> Nom de l'article-champ a ecrire;
!                KLNOMA (Sortie) ==> Nombre de caracteres utiles dans
!                                    CDNOMA;
!    ( Tableau ) PVALCO (Sortie) ==> Donnees destinees a l'ecriture;
!                KLONGD (Sortie) ==> Nombre de valeurs (mots de 64 bits
!                                    en principe) a ecrire.
!
!    Remarques:
!
!    - PVALCO est type reel par commodite, et doit avoir une longueur
!      suffisante pour stocker les donnees codees. Le dimensionnement
!      "tous terrains" est (2+ILCHAM), qui permet le cas echeant de
!      stocker un champ a pleine resolution sans codage effectif.
!      (ILCHAM est le nombre de valeurs du champ a ecrire)
!
!    - CDNOMA doit avoir au moins FA%JPXNOM caracteres.
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KNUMER, KNIVAU, KLNOMA, KLONGD
!
REAL (KIND=JPDBLR) PCHAMP (*), PVALCO (*), ZUNDF
!
CHARACTER CDPREF*(*), CDSUFF*(*), CDNOMA*(*)
!
INTEGER (KIND=JPLIKB) IREP, ILPRFU, ILSUFU, ILNOMU
INTEGER (KIND=JPLIKB) IRANG, INIMES
INTEGER (KIND=JPLIKB) ILPREF, ILSUFF, ILCDNO, IRANGC
INTEGER (KIND=JPLIKB) IB1PAR (FA%JPLB1P)
!
LOGICAL LLVERF, LLRLFI, LDCOSP, LLNOMU, LLNOPA, LLUNDF
!
CHARACTER CLPREF*(FA%JPXNOM), CLSUFF*(FA%JPXSUF)
!
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA
TYPE (FAGR1TAB)          YLGR1TAB

!**
!     1.  -  CONTROLES ET INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FACOND_MT',0,ZHOOK_HANDLE)

LLUNDF = .FALSE.
ZUNDF  = 0._JPDBLR
KLONGD = 0
CALL FACON1_FORT (FA,  KREP,   KNUMER, CDPREF, KNIVAU, CDSUFF, &
&                 PCHAMP, LDCOSP, CDNOMA, KLNOMA, PVALCO,      &
&                 KLONGD, LLUNDF, ZUNDF, YLGR1TAB)

IF (LHOOK) CALL DR_HOOK('FACOND_MT',1,ZHOOK_HANDLE)

END SUBROUTINE FACOND_FORT

! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FACOND64                                       &
&           (KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, PCHAMP, &
&            LDCOSP, CDNOMA, KLNOMA, PVALCO, KLONGD)
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

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FACOND_FORT                                               &
&           (FA, KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, PCHAMP, &
&           LDCOSP, CDNOMA, KLNOMA, PVALCO, KLONGD)

END SUBROUTINE FACOND64

SUBROUTINE FACOND                                          &
&           (KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, PCHAMP, &
&           LDCOSP, CDNOMA, KLNOMA, PVALCO, KLONGD)
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

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FACOND_MT                                                 &
&           (FA, KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, PCHAMP, &
&            LDCOSP, CDNOMA, KLNOMA, PVALCO, KLONGD)

END SUBROUTINE FACOND

SUBROUTINE FACOND_MT                                           &
&           (FA, KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, PCHAMP, &
&            LDCOSP, CDNOMA, KLNOMA, PVALCO, KLONGD)
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
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  INIVAU                                 ! IN   
INTEGER (KIND=JPLIKB)  ILNOMA                                 !   OUT
INTEGER (KIND=JPLIKB)  ILONGD                                 !   OUT
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)
INIVAU     = INT (    KNIVAU, JPLIKB)

CALL FACOND_FORT                                               &
&           (FA, IREP, INUMER, CDPREF, INIVAU, CDSUFF, PCHAMP, &
&            LDCOSP, CDNOMA, ILNOMA, PVALCO, ILONGD)

KREP       = INT (      IREP, JPLIKM)
KLNOMA     = INT (    ILNOMA, JPLIKM)
KLONGD     = INT (    ILONGD, JPLIKM)

END SUBROUTINE FACOND_MT

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
!INTF KLONGD          OUT                               

