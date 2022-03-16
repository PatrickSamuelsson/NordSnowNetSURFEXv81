! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FADECO_FORT                                                &
&                     (FA,  KREP,   KNUMER, CDPREF, KNIVAU, CDSUFF,   &
&                      LDCOSP, CDNOMA, KLNOMA, KVALCO, KLONGD,        &
&                      PCHAMP )
USE FA_MOD, ONLY : FA_COM, FAGR1TAB
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!      Sous-programme de controle et de DECODAGE d'un CHAMP HORIZONTAL
!      venant d'etre lu sur un fichier ARPEGE/ALADIN.
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
!    ( Tableau ) KVALCO (Entree) ==> Donnees issues de la lecture;
!                KLONGD (Entree) ==> Nombre de valeurs (mots de 64 bits
!                                    en principe) lues;
!    ( Tableau ) PCHAMP (Sortie) ==> Valeurs REELLES du champ lu.
!
!    Remarques:
!
!    - KVALCO est type entier, et doit avoir une longueur
!      suffisante pour stocker les donnees codees. Le dimensionnement
!      "tous terrains" est (2+ILCHAM), qui permet le cas echeant de
!      stocker un champ a pleine resolution sans codage effectif.
!      (ILCHAM est le nombre de valeurs du champ a decoder)
!
!    - CDNOMA doit avoir au moins FA%JPXNOM caracteres.
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KNUMER, KNIVAU, KLNOMA, KLONGD
!
!
REAL (KIND=JPDBLR) PCHAMP (*), ZUNDF
INTEGER (KIND=JPLIKB) KVALCO(*)
!
LOGICAL LDCOSP, LLUNDF
!
CHARACTER CDPREF*(*), CDSUFF*(*), CDNOMA*(*)
TYPE (FAGR1TAB) YLGR1TAB
!

!**
!     1.  -  CONTROLES ET INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FADECO_MT',0,ZHOOK_HANDLE)

CALL FADEC1_FORT (FA, KREP,   KNUMER, CDPREF, KNIVAU, CDSUFF,  &
                & LDCOSP, CDNOMA, KLNOMA, KVALCO, KLONGD,      & 
                & PCHAMP, LLUNDF, ZUNDF, YLGR1TAB)

IF (LHOOK) CALL DR_HOOK('FADECO_MT',1,ZHOOK_HANDLE)

END SUBROUTINE FADECO_FORT

! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FADECO64                                        &
&           (KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, LDCOSP, &
&            CDNOMA, KLNOMA, KVALCO, KLONGD, PCHAMP)
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
INTEGER (KIND=JPLIKB)  KVALCO     (*)                         ! IN   
INTEGER (KIND=JPLIKB)  KLONGD                                 ! IN   
REAL (KIND=JPDBLR)     PCHAMP     (*)                         !   OUT

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FADECO_FORT                                               &
&           (FA, KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, LDCOSP, &
&            CDNOMA, KLNOMA, KVALCO, KLONGD, PCHAMP)

END SUBROUTINE FADECO64

SUBROUTINE FADECO                                          &
&           (KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, LDCOSP, &
&            CDNOMA, KLNOMA, KVALCO, KLONGD, PCHAMP)
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
INTEGER (KIND=JPLIKB)  KVALCO     (*)                         ! IN   
INTEGER (KIND=JPLIKM)  KLONGD                                 ! IN   
REAL (KIND=JPDBLR)     PCHAMP     (*)                         !   OUT

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FADECO_MT                                                 &
&           (FA, KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, LDCOSP, &
&            CDNOMA, KLNOMA, KVALCO, KLONGD, PCHAMP)

END SUBROUTINE FADECO

SUBROUTINE FADECO_MT                                           &
&           (FA, KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, LDCOSP, &
&            CDNOMA, KLNOMA, KVALCO, KLONGD, PCHAMP)
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
INTEGER (KIND=JPLIKB)  KVALCO     (*)                         ! IN   
INTEGER (KIND=JPLIKM)  KLONGD                                 ! IN   
REAL (KIND=JPDBLR)     PCHAMP     (*)                         !   OUT
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

CALL FADECO_FORT                                               &
&           (FA, IREP, INUMER, CDPREF, INIVAU, CDSUFF, LDCOSP, &
&            CDNOMA, ILNOMA, KVALCO, ILONGD, PCHAMP)

KREP       = INT (      IREP, JPLIKM)
KLNOMA     = INT (    ILNOMA, JPLIKM)

END SUBROUTINE FADECO_MT

!INTF KREP            OUT                                                              
!INTF KNUMER        IN                                                                 
!INTF CDPREF        IN                                                                 
!INTF KNIVAU        IN                                                                 
!INTF CDSUFF        IN                                                                 
!INTF LDCOSP        IN                                                                 
!INTF CDNOMA          OUT                                                              
!INTF KLNOMA          OUT                                                              
!INTF KVALCO        IN    DIMS=*                         KIND=JPLIKB                   
!INTF KLONGD        IN                                                                 
!INTF PCHAMP          OUT DIMS=*                                                       
