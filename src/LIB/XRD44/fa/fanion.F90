! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FANION_FORT                                                &
&                     (FA,  KREP, KNUMER, CDPREF, KNIVAU, CDSUFF,     &
&                      LDEXIS, LDCOSP, KNGRIB, KNARG1, KNARG2, KNARG3)
USE FA_MOD, ONLY : FA_COM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!      Sous-programme renseignant sur l'EXISTENCE et les CARACTERISTI-
!     QUES eventuelles d'un Article de type CHAMP dans un Fichier ARPEGE
!       ( LDEXIS est le "fanion" leve si l'article existe )
!**
!    Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                KNUMER (Entree) ==> Numero de l'unite logique;
!                CDPREF (Entree) ==> Prefixe eventuel du nom d'article;
!                KNIVAU (Entree) ==> Niveau vertical eventuel;
!                CDSUFF (Entree) ==> Suffixe eventuel du nom d'article;
!                LDEXIS (Sortie) ==> Vrai si l'article de type CHAMP
!                                    existe bien dans le Fichier;
!                LDCOSP (Sortie) ==> Vrai si le champ est represente
!                                    par des coefficients spectraux;
!                KNGRIB (Sortie) ==> Niveau de codage GRIB;
!    Si KNGRIB vaut -1,0,1,2,3, alors les arguments de sortie ont la 
!    signification suivante:
!                KNARG1 (Sortie) ==> Nombre de bits de codage eventuel;
!                KNARG2 (Sortie) ==> Sous-troncature non codee   " -le;
!                KNARG3 (Sortie) ==> Puissance de laplacien eventuelle.
!    Si KNGRIB vaut 4, alors les arguments de sortie ont la signification
!    suivante:
!                KNARG1 (Sortie) ==> Taille de la couronne a conserver
!                KNARG2 (Sortie) ==> Nombre de bits utilises pour le codage
!                KNARG3 (Sortie) ==> Inutilise
!             
!
!        KNARG1 n'a de sens que si l'article existe et a ete code;
!     de meme pour KNARG2 et KNARG3, qui ne sont applicables qu'a un
!     champ represente en coefficients spectraux.
!        Les arguments de sortie n'ayant pas de sens sont mis a
!     0 pour les entiers, .FALSE. pour les logiques.
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KNUMER, KNIVAU, KNGRIB
INTEGER (KIND=JPLIKB) KNARG1, KNARG2, KNARG3
!
INTEGER (KIND=JPLIKB) IREP, ILPRFU, ILSUFU, ILNOMU
INTEGER (KIND=JPLIKB) ILONGA, IRANG, INIMES
INTEGER (KIND=JPLIKB) ILPREF, ILSUFF, IPOSEX
INTEGER (KIND=JPLIKB) IRANGC, ILCHAM
!
INTEGER (KIND=JPLIKB) IVALCO (5)
INTEGER (KIND=JPLIKB)  IB1PAR (3)
!
LOGICAL LLVERF, LLRLFI, LDCOSP, LDEXIS, LLTEMP, LLNOMU, LLMLAM
!
CHARACTER CDPREF*(*), CDSUFF*(*)
CHARACTER CLPREF*(FA%JPXNOM), CLSUFF*(FA%JPXSUF)
!
CHARACTER(LEN=FA%JPXNOM) CLNOMA
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA

!**
!     1.  -  CONTROLES ET INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FANION_MT',0,ZHOOK_HANDLE)
IREP=0
LLVERF=.FALSE.
LLTEMP=.FALSE.
LLRLFI=.FALSE.
LLNOMU=.FALSE.
ILPRFU=INT (LEN (CDPREF), JPLIKB)
ILSUFU=INT (LEN (CDSUFF), JPLIKB)
LDEXIS=.FALSE.
LDCOSP=.FALSE.
KNGRIB=0
KNARG1=0
KNARG2=0
KNARG3=0
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
IF (FA%FICHIER(IRANG)%LCREAF) GOTO 1001
!**
!     2.  -  FABRICATION DU NOM D'ARTICLE VIA LE SOUS-PROGRAMME "FANFAR"
!            ( controles de CDPREF, KNIVAU, CDSUFF inclus )
!-----------------------------------------------------------------------
!
CALL FANFAR_FORT                                             &
&               (FA, IREP,IRANG,CDPREF,KNIVAU,CDSUFF,CLNOMA, &
&                IB1PAR,ILPRFU,ILSUFU,ILNOMU)
IF (IREP.NE.0) GOTO 1001
LLNOMU=.TRUE.
!**
!     3.  -  RECHERCHE DE L'ARTICLE SUR LE FICHIER, LECTURE PARTIELLE.
!-----------------------------------------------------------------------
!
CALL LFINFO_FORT                                       &
&               (FA%LFI, IREP,KNUMER,CLNOMA(1:ILNOMU), &
&             ILONGA,IPOSEX)
LLRLFI=IREP.NE.0
IF (LLRLFI.OR.ILONGA.EQ.0) GOTO 1001
LDEXIS=.TRUE.
!
IF (ILONGA.GT.FA%JPXCHA+2) THEN
  IREP=-90
  GOTO 1001
ENDIF
!
IF (FA%FICHIER(IRANG)%LERRFA) THEN
!
!        Le fichier est gere en mode "toute erreur est fatale".
!     Ce mode etant normalement couple au mode correspondant du logiciel
!     LFI, on va temporairement annuler l'option LFI afin de pouvoir
!     faire une lecture partielle de l'entete de l'article Champ.
!
  CALL LFIERF_FORT                             &
&                 (FA%LFI, IREP,KNUMER,.FALSE.)
  LLRLFI=IREP.NE.0
  IF (LLRLFI) GOTO 1001
  LLTEMP=.TRUE.
ENDIF
!
CALL LFILEC_FORT                                       &
&               (FA%LFI, IREP,KNUMER,CLNOMA(1:ILNOMU), &
&               IVALCO,5_JPLIKB )
!
IF (IREP.EQ.0) THEN
  IREP=-93
  GOTO 1001
ELSEIF (IREP.NE.-21) THEN
  LLRLFI=.TRUE.
  GOTO 1001
ELSEIF ((IVALCO(1).LT.-2 .OR. IVALCO(1).GT.4 .OR.                   &
&       (IVALCO(2).LT.0  .OR. IVALCO(2).GT.1 .OR.                   &
&       (IVALCO(1).GT.0 .AND. IVALCO(2).EQ.1 .AND. IVALCO(4).LT.0)))&
& .AND. (.NOT. FALGRA (IVALCO(1)))) THEN
  IREP=-91
  GOTO 1001
ELSE
  IREP=0
  KNGRIB=IVALCO(1)
  LDCOSP=IVALCO(2).EQ.1
ENDIF
!
IRANGC=FA%FICHIER(IRANG)%NUCADR
LLMLAM=FA%CADRE(IRANGC)%LIMLAM
!
IF (LDCOSP) THEN
  IF (LLMLAM) THEN
     ILCHAM=FA%CADRE(IRANGC)%NSFLAM
  ELSE
     IF (KNGRIB.EQ.3 .OR. KNGRIB.EQ.-1) THEN
       ILCHAM=(1+FA%CADRE(IRANGC)%MTRONC)*(2+FA%CADRE(IRANGC)%MTRONC)
     ELSE
       ILCHAM=(1+FA%CADRE(IRANGC)%MTRONC)**2
     ENDIF
  ENDIF
ELSE
  ILCHAM=FA%CADRE(IRANGC)%NVAPDG
ENDIF
!
IF (KNGRIB.EQ.-1 .OR. KNGRIB.EQ.0) THEN
!
!          Cas ou il n'y a aucun codage... controle longueur d'article
!
  IF (ILONGA.LT.(ILCHAM+2)) THEN
    IREP=-93
    GOTO 1001
  ELSEIF (ILONGA.GT.(ILCHAM+2)) THEN
    IREP=-94
    IF (LLMOER(IREP,IRANG)) GOTO 1001
  ENDIF
!
ELSEIF (KNGRIB.EQ.-2) THEN
  IF (ILONGA .LT. ((ILCHAM+1)/2+2)) THEN
    IREP=-93
    GOTO 1001
  ELSEIF (ILONGA .GT. ((ILCHAM+1)/2+2)) THEN
    IREP=-94
    IF (LLMOER(IREP,IRANG)) GOTO 1001
  ENDIF
ELSEIF (KNGRIB.EQ.4) THEN
  KNARG1=IVALCO(3)
  KNARG2=IVALCO(4)
ELSEIF (FALGRA (KNGRIB)) THEN
  LDCOSP=IVALCO(2).EQ.1
  KNARG1=IVALCO(3)
ELSE
!
!        Cas avec codage GRIB (standard ou non).
!
  KNARG1=IVALCO(3)
!
  IF (LDCOSP) THEN
    KNARG2=IVALCO(4)
    KNARG3=IVALCO(5)
!
    IF (KNGRIB.EQ.2.AND.ILONGA.LT.(5+(1+KNARG2)**2)) THEN
      IREP=-93
      GOTO 1001
    ENDIF
!
  ENDIF
!
ENDIF
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE, AVEC "ABORT" EVENTUEL,
!            VIA LE SOUS-PROGRAMME "FAIPAR" .
!-----------------------------------------------------------------------
!
1001 CONTINUE
!
IF (LLTEMP) THEN
!
!         On remet le fichier en mode "toute erreur fatale" au niveau
!     du logiciel LFI.
!
  CALL LFIERF_FORT                            &
&                 (FA%LFI, IREP,KNUMER,.TRUE.)
  LLRLFI=IREP.NE.0
ENDIF
!
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
  IF (LHOOK) CALL DR_HOOK('FANION_MT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='FANION'
!
IF (ILPRFU.GE.1) THEN
  ILPREF=MIN (ILPRFU,INT (LEN (CLPREF), JPLIKB))
  CLPREF(1:ILPREF)=CDPREF(1:ILPREF)
ELSE
  ILPREF=8
  CLPREF(1:ILPREF)=FA%CHAINC(:ILPREF)
ENDIF
!
IF (ILSUFU.GE.1) THEN
  ILSUFF=MIN (ILSUFU,INT (LEN (CLSUFF), JPLIKB))
  CLSUFF(1:ILSUFF)=CDSUFF(1:ILSUFF)
ELSE
  ILSUFF=8
  CLSUFF(1:ILSUFF)=FA%CHAINC(:ILSUFF)
ENDIF
!
IF (.NOT.LLNOMU) THEN
  ILNOMU=MIN (ILPREF,FA%NCPCAD)
  CLNOMA(1:ILNOMU)=CLPREF(1:ILPREF)
ENDIF
!
WRITE (UNIT=CLMESS,                                              &
&       FMT='(''ARGUMENTS:'',I4,'','',I3,'','''''',A,            &
&       '''''','',I6,'','''''',A,'''''', LDEXIS= '',L1,          &
&       '', LDCOSP= '',L1,'', KNGRIB='',I2,'', KNARG1='',I3,     &
&       '',KNARG2='',I3,'',KNARG3='',I6)')                       &
&   KREP,KNUMER,CLPREF(1:ILPREF),KNIVAU,CLSUFF(1:ILSUFF),LDEXIS, &
&   LDCOSP,KNGRIB,KNARG1,KNARG2,KNARG3
CALL FAIPAR_FORT                                       &
&               (FA, KNUMER,INIMES,IREP,LLFATA,CLMESS, &
&                CLNSPR,CLNOMA(1:ILNOMU),LLRLFI)
!
IF (LHOOK) CALL DR_HOOK('FANION_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"
#include "facom2.ixnvms.h"
#include "falgra.h"

END SUBROUTINE FANION_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FANION64                                        &
&           (KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, LDEXIS, &
&           LDCOSP, KNGRIB, KNARG1, KNARG2, KNARG3)
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
LOGICAL                LDEXIS                                 !   OUT
LOGICAL                LDCOSP                                 !   OUT
INTEGER (KIND=JPLIKB)  KNGRIB                                 !   OUT
INTEGER (KIND=JPLIKB)  KNARG1                                 !   OUT
INTEGER (KIND=JPLIKB)  KNARG2                                 !   OUT
INTEGER (KIND=JPLIKB)  KNARG3                                 !   OUT

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FANION_FORT                                               &
&           (FA, KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, LDEXIS, &
&           LDCOSP, KNGRIB, KNARG1, KNARG2, KNARG3)

END SUBROUTINE FANION64

SUBROUTINE FANION                                          &
&           (KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, LDEXIS, &
&           LDCOSP, KNGRIB, KNARG1, KNARG2, KNARG3)
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
LOGICAL                LDEXIS                                 !   OUT
LOGICAL                LDCOSP                                 !   OUT
INTEGER (KIND=JPLIKM)  KNGRIB                                 !   OUT
INTEGER (KIND=JPLIKM)  KNARG1                                 !   OUT
INTEGER (KIND=JPLIKM)  KNARG2                                 !   OUT
INTEGER (KIND=JPLIKM)  KNARG3                                 !   OUT

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FANION_MT                                                 &
&           (FA, KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, LDEXIS, &
&           LDCOSP, KNGRIB, KNARG1, KNARG2, KNARG3)

END SUBROUTINE FANION

SUBROUTINE FANION_MT                                           &
&           (FA, KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, LDEXIS, &
&           LDCOSP, KNGRIB, KNARG1, KNARG2, KNARG3)
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
LOGICAL                LDEXIS                                 !   OUT
LOGICAL                LDCOSP                                 !   OUT
INTEGER (KIND=JPLIKM)  KNGRIB                                 !   OUT
INTEGER (KIND=JPLIKM)  KNARG1                                 !   OUT
INTEGER (KIND=JPLIKM)  KNARG2                                 !   OUT
INTEGER (KIND=JPLIKM)  KNARG3                                 !   OUT
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  INIVAU                                 ! IN   
INTEGER (KIND=JPLIKB)  INGRIB                                 !   OUT
INTEGER (KIND=JPLIKB)  INARG1                                 !   OUT
INTEGER (KIND=JPLIKB)  INARG2                                 !   OUT
INTEGER (KIND=JPLIKB)  INARG3                                 !   OUT
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)
INIVAU     = INT (    KNIVAU, JPLIKB)

CALL FANION_FORT                                               &
&           (FA, IREP, INUMER, CDPREF, INIVAU, CDSUFF, LDEXIS, &
&           LDCOSP, INGRIB, INARG1, INARG2, INARG3)

KREP       = INT (      IREP, JPLIKM)
KNGRIB     = INT (    INGRIB, JPLIKM)
KNARG1     = INT (    INARG1, JPLIKM)
KNARG2     = INT (    INARG2, JPLIKM)
KNARG3     = INT (    INARG3, JPLIKM)

END SUBROUTINE FANION_MT

!INTF KREP            OUT 
!INTF KNUMER        IN    
!INTF CDPREF        IN    
!INTF KNIVAU        IN    
!INTF CDSUFF        IN    
!INTF LDEXIS          OUT 
!INTF LDCOSP          OUT 
!INTF KNGRIB          OUT 
!INTF KNARG1          OUT 
!INTF KNARG2          OUT 
!INTF KNARG3          OUT 
