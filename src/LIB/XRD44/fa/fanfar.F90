! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FANFAR_FORT                                            &
&                     (FA,  KREP, KRANG, CDPREF, KNIVAU, CDSUFF,  &
&                      CDNOMA, KB1PAR, KLPRFU, KLSUFU, KLNOMU )
USE FA_MOD, ONLY : FA_COM, JPNIIL
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Sous-programme INTERNE du logiciel de Fichiers ARPEGE:
!     fabrication d'un nom article devant contenir un champ horizontal.
!              (Nom a Fabriquer pour un ARticle)
!**
!    Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                KRANG  (Entree) ==> Rang de l'unite logique;
!                CDPREF (Entree) ==> Prefixe eventuel du nom d'article;
!                KNIVAU (Entree) ==> Niveau vertical eventuel;
!                CDSUFF (Entree) ==> Suffixe eventuel du nom d'article;
!                CDNOMA (Sortie) ==> Nom d'article;
!    ( Tableau ) KB1PAR (Sortie) ==> 3 elements du "Bloc 1" des
!                                    interfaces GRIB concernes par la
!                                    coordonnee et le niveau verticaux;
!                KLPRFU (Sortie) ==> Longueur utile du prefixe;
!                KLSUFU (Sortie) ==> Longueur utile du suffixe;
!                KLNOMU (Sortie) ==> Longueur utile du nom d'article.
!*
!       En mode multi-taches, il doit y avoir verrouillage du fichier
!     concerne avant l'appel au sous-programme.
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KRANG, KNIVAU
INTEGER (KIND=JPLIKB) KLPRFU, KLSUFU, KLNOMU
INTEGER (KIND=JPLIKB) KB1PAR (3)
!
CHARACTER CDPREF*(*), CDSUFF*(*), CDNOMA*(*), CLAUXI*(FA%JPXNOM)
!
INTEGER (KIND=JPLIKB) ILPREF, ILSUFF, ILCDNO, ILPRFU
INTEGER (KIND=JPLIKB) ILSUFU, J, INCHIF, INIMES
INTEGER (KIND=JPLIKB) INUMER, ILACTI, ILNOMA, ILAUXI
INTEGER (KIND=JPLIKB) ITYNIV, INIVAU
!
CHARACTER(LEN=FA%JPXNOM) CLACTI 
CHARACTER(LEN=FA%JPXNOM) CLNOMA
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA

!**
!     1.  -  CONTROLES DES PARAMETRES D'APPEL, INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FANFAR_MT',0,ZHOOK_HANDLE)
CLACTI=''
ILPREF=INT (LEN (CDPREF), JPLIKB)
ILSUFF=INT (LEN (CDSUFF), JPLIKB)
ILCDNO=INT (LEN (CDNOMA), JPLIKB)
ILPRFU=MAX (0_JPLIKB , ILPREF)
ILSUFU=MAX (0_JPLIKB , ILSUFF)
KLNOMU=MAX (0_JPLIKB , ILCDNO)
!
IF (KRANG.LE.0.OR.KRANG.GT.FA%JPNXFA) THEN
  KREP=-66
  GOTO 1001
ENDIF
!
IF (ILCDNO.LE.0.OR.ILCDNO.GT.FA%NCPCAD) THEN
  KREP=-66
  GOTO 1001
ELSEIF (MIN (ILPREF,ILSUFF).LE.0) THEN
  KREP=-65
  GOTO 1001
ELSEIF (CDPREF.EQ.' '.OR.CDSUFF.EQ.' ') THEN
  KREP=-86
  GOTO 1001
ENDIF
!
!       Decompte du nombre de caracteres utiles dans prefixe et suffixe.
!
DO J=ILPREF,1,-1
!
IF (CDPREF(J:J).NE.' ') THEN
  ILPRFU=J
  GOTO 104
ENDIF
!
ENDDO
!
KREP=-66
GOTO 1001
!
104 CONTINUE
!
IF (ILPRFU.GT.FA%JPXPRF) THEN
  KREP=-87
  GOTO 1001
ENDIF
!
DO J=ILSUFF,1,-1
!
IF (CDSUFF(J:J).NE.' ') THEN
  ILSUFU=J
  GOTO 106
ENDIF
!
ENDDO
!
KREP=-66
GOTO 1001
!
106 CONTINUE
!**
!     2.  -  DIFFERENTS CAS, SELON LE PREFIXE.
!-----------------------------------------------------------------------
!
DO J=1,FA%JPTNIV
!
IF (CDPREF.EQ.FA%CTNPRF(J)) THEN
  ITYNIV=J
  GOTO 202
ENDIF
!
ENDDO
!
ITYNIV=0
!
202 CONTINUE
!
INCHIF=FA%NIVDSC(0,ITYNIV)
!
IF (INCHIF.EQ.0) THEN
!
  INIVAU=0
!
ELSEIF (KNIVAU.LT.FA%NIVDSC(1,ITYNIV).OR.   &
&       KNIVAU.GT.FA%NIVDSC(2,ITYNIV)) THEN
!
  KREP=-64
  GOTO 1001
!
ELSEIF (CDPREF.EQ.'S'.AND.KNIVAU.GT.      &
&        FA%CADRE(FA%FICHIER(KRANG)%NUCADR)%NNIVER) THEN
!
  KREP=-64
  GOTO 1001
!
ELSEIF (CDPREF.EQ.'L'.AND.KNIVAU.GT.      &
&        FA%CADRE(FA%FICHIER(KRANG)%NUCADR)%NNIVER) THEN
!
  KREP=-64
  GOTO 1001
!
ELSE
!
  INIVAU=KNIVAU
!
ENDIF
!
KB1PAR(1)=FA%NIVDSC(3,ITYNIV)
KB1PAR(2)=FA%NIVDSC(4,ITYNIV)
KB1PAR(3)=INIVAU
!
ILSUFU=MIN (ILCDNO-ILPRFU-INCHIF,ILSUFU)
KLNOMU=ILPRFU+INCHIF+ILSUFU
!
IF (INCHIF.NE.0) THEN
  WRITE (UNIT=CLNOMA,FMT='(I8.8)') KNIVAU
  CDNOMA=CDPREF(1:ILPRFU)//CLNOMA(9-INCHIF:8)//CDSUFF(1:ILSUFU)
ELSE
  CDNOMA=CDPREF(1:ILPRFU)//CDSUFF(1:ILSUFU)
ENDIF
!
IF (CDNOMA.EQ.FA%CPCACH.OR.CDNOMA.EQ.FA%CPCADI.OR.CDNOMA.EQ. &
&    FA%CPCAFS.OR.                                           &
&    CDNOMA.EQ.FA%CPCARP.OR.CDNOMA.EQ.FA%CPDATE.OR.          &
&    CDNOMA.EQ.FA%CPDATX.OR.                                 &
&    CDNOMA.EQ.FA%FICHIER(KRANG)%CIDENT) THEN
  KREP=-111
  GOTO 1001
ENDIF
!
KREP=0
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE EVENTUELLE,
!            VIA LE SOUS-PROGRAMME "FAIPAR" .
!-----------------------------------------------------------------------
!
1001 CONTINUE
KLPRFU=ILPRFU
KLSUFU=ILSUFU
LLFATA=LLMOER (KREP,KRANG)
!
IF (FA%LFAMOP.OR.LLFATA) THEN
  INIMES=2
  CLNSPR='FANFAR'
  INUMER=JPNIIL
!
  IF (ILPRFU.GE.1) THEN
    ILACTI=MIN (ILPRFU,FA%NCPCAD)
    CLACTI(1:ILACTI)=CDPREF(1:ILACTI)
  ELSE
    ILACTI=8
    CLACTI(1:ILACTI)=FA%CHAINC(:ILACTI)
  ENDIF
!
  IF (ILSUFU.GE.1) THEN
    ILNOMA=MIN (ILSUFU,FA%NCPCAD)
    CLNOMA(1:ILNOMA)=CDSUFF(1:ILNOMA)
  ELSE
    ILNOMA=8
    CLNOMA(1:ILNOMA)=FA%CHAINC(:ILNOMA)
  ENDIF
!
  IF (KLNOMU.GE.1) THEN
    ILAUXI=MIN (KLNOMU,INT (LEN(CLAUXI), JPLIKB))
    CLAUXI(1:ILAUXI)=CDNOMA(1:ILAUXI)
  ELSE
    ILAUXI=8
    CLAUXI(1:ILAUXI)=FA%CHAINC(:ILAUXI)
  ENDIF
!
  WRITE (UNIT=CLMESS,                                           &
&         FMT='(''ARGUMENTS='',2(I4,'', ''),'''''''',A,         &
&         '''''','',I6,'', '''''',A,'''''',  '''''',A,'''''''', &
&         2('','',I4),'','',I6,3('','',I3))')                   &
&     KREP,KRANG,CLACTI(1:ILACTI),KNIVAU,CLNOMA(1:ILNOMA),      &
&     CLAUXI(1:ILAUXI),KB1PAR,KLPRFU,KLSUFU,KLNOMU
  CALL FAIPAR_FORT                                        &
&                 (FA, INUMER,INIMES,KREP,.FALSE.,CLMESS, &
&                  CLNSPR, CLACTI(1:ILACTI),.FALSE.)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('FANFAR_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"

END SUBROUTINE FANFAR_FORT








! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FANFAR64                                       &
&           (KREP, KRANG, CDPREF, KNIVAU, CDSUFF, CDNOMA, &
&           KB1PAR, KLPRFU, KLSUFU, KLNOMU)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                  FA_COM_DEFAULT_INIT,  &
&                  NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KRANG                                  ! IN   
CHARACTER (LEN=*)      CDPREF                                 ! IN   
INTEGER (KIND=JPLIKB)  KNIVAU                                 ! IN   
CHARACTER (LEN=*)      CDSUFF                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 !   OUT
INTEGER (KIND=JPLIKB)  KB1PAR     (3)                         !   OUT
INTEGER (KIND=JPLIKB)  KLPRFU                                 !   OUT
INTEGER (KIND=JPLIKB)  KLSUFU                                 !   OUT
INTEGER (KIND=JPLIKB)  KLNOMU                                 !   OUT

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FANFAR_FORT                                              &
&           (FA, KREP, KRANG, CDPREF, KNIVAU, CDSUFF, CDNOMA, &
&           KB1PAR, KLPRFU, KLSUFU, KLNOMU)

END SUBROUTINE FANFAR64

SUBROUTINE FANFAR                                         &
&           (KREP, KRANG, CDPREF, KNIVAU, CDSUFF, CDNOMA, &
&           KB1PAR, KLPRFU, KLSUFU, KLNOMU)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                  FA_COM_DEFAULT_INIT,  &
&                  NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KRANG                                  ! IN   
CHARACTER (LEN=*)      CDPREF                                 ! IN   
INTEGER (KIND=JPLIKM)  KNIVAU                                 ! IN   
CHARACTER (LEN=*)      CDSUFF                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 !   OUT
INTEGER (KIND=JPLIKM)  KB1PAR     (3)                         !   OUT
INTEGER (KIND=JPLIKM)  KLPRFU                                 !   OUT
INTEGER (KIND=JPLIKM)  KLSUFU                                 !   OUT
INTEGER (KIND=JPLIKM)  KLNOMU                                 !   OUT

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FANFAR_MT                                                &
&           (FA, KREP, KRANG, CDPREF, KNIVAU, CDSUFF, CDNOMA, &
&            KB1PAR, KLPRFU, KLSUFU, KLNOMU)

END SUBROUTINE FANFAR

SUBROUTINE FANFAR_MT                                          &
&           (FA, KREP, KRANG, CDPREF, KNIVAU, CDSUFF, CDNOMA, &
&            KB1PAR, KLPRFU, KLSUFU, KLNOMU)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KRANG                                  ! IN   
CHARACTER (LEN=*)      CDPREF                                 ! IN   
INTEGER (KIND=JPLIKM)  KNIVAU                                 ! IN   
CHARACTER (LEN=*)      CDSUFF                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 !   OUT
INTEGER (KIND=JPLIKM)  KB1PAR     (3)                         !   OUT
INTEGER (KIND=JPLIKM)  KLPRFU                                 !   OUT
INTEGER (KIND=JPLIKM)  KLSUFU                                 !   OUT
INTEGER (KIND=JPLIKM)  KLNOMU                                 !   OUT
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  IRANG                                  ! IN   
INTEGER (KIND=JPLIKB)  INIVAU                                 ! IN   
INTEGER (KIND=JPLIKB)  IB1PAR     (3)                         !   OUT
INTEGER (KIND=JPLIKB)  ILPRFU                                 !   OUT
INTEGER (KIND=JPLIKB)  ILSUFU                                 !   OUT
INTEGER (KIND=JPLIKB)  ILNOMU                                 !   OUT
! Convert arguments

IRANG      = INT (     KRANG, JPLIKB)
INIVAU     = INT (    KNIVAU, JPLIKB)

CALL FANFAR_FORT                                              &
&           (FA, IREP, IRANG, CDPREF, INIVAU, CDSUFF, CDNOMA, &
&            IB1PAR, ILPRFU, ILSUFU, ILNOMU)

KREP       = INT (      IREP, JPLIKM)
KB1PAR     = INT (    IB1PAR, JPLIKM)
KLPRFU     = INT (    ILPRFU, JPLIKM)
KLSUFU     = INT (    ILSUFU, JPLIKM)
KLNOMU     = INT (    ILNOMU, JPLIKM)

END SUBROUTINE FANFAR_MT

!INTF KREP            OUT                               
!INTF KRANG         IN                                  
!INTF CDPREF        IN                                  
!INTF KNIVAU        IN                                  
!INTF CDSUFF        IN                                  
!INTF CDNOMA          OUT                               
!INTF KB1PAR          OUT DIMS=3                        
!INTF KLPRFU          OUT                               
!INTF KLSUFU          OUT                               
!INTF KLNOMU          OUT                               
