! Oct-2012 P. Marguinaud Use JNGEOM & JNEXPL parameters
! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FACOCH_FORT                           &
&                     (FA,  KREP, KNUME1, KNUME2,  &
&                      CDPREF, KNIVAU, CDSUFF )
USE FA_MOD, ONLY : FA_COM, JPNIIL, JNGEOM, JNEXPL
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!      Sous-programme de reCOpie d'un Champ Horizontal d'un fichier
!     ARPEGE sur un autre.
!**
!    Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                KNUME1 (Entree) ==> Numero d'unite logique en entree;
!                KNUME2 (Entree) ==> Numero d'unite logique en sortie;
!                CDPREF (Entree) ==> Prefixe eventuel du nom d'article;
!                KNIVAU (Entree) ==> Niveau vertical eventuel;
!                CDSUFF (Entree) ==> Suffixe eventuel du nom d'article.
!
!     Modifications
!     -------------
!
!  Avril 2004, D. Paradis, DSI/DEV:
!    -Declaration IVALCO en ALLOCATABLE (gain memoire)
!  Juin  2004, D. Paradis, DSI/DEV:
!    -Prise en compte des codages type -1 et 3
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KNUME1, KNUME2, KNIVAU
!
INTEGER (KIND=JPLIKB) IREP, ILPRFU, ILSUFU, ILNOMU
INTEGER (KIND=JPLIKB) ILONGA, IRANC1, IRANC2
INTEGER (KIND=JPLIKB) INIMES, J, INUMFI, IPOSEX, INPAHE
INTEGER (KIND=JPLIKB) INPAHEL, JLAT, IZPAHEL
INTEGER (KIND=JPLIKB) ISPAHEL, JNIV, ILPREF, ILSUFF
INTEGER (KIND=JPLIKB) INUMRO, IRANG2, IGRIB
!
INTEGER (KIND=JPLIKB), ALLOCATABLE :: IVALCO(:)
INTEGER (KIND=JPLIKB) IRANG (2), INUMER (2), IB1PAR (3)
!
LOGICAL LLVERF (2), LLRLFI, LLCOSP, LLMESS, LLNOMU
LOGICAL LLMLAM1, LLMLAM2
!
CHARACTER CDPREF*(*), CDSUFF*(*)
CHARACTER CLPREF*(FA%JPXNOM),  &
&          CLSUFF*(FA%JPXSUF)
!
CHARACTER(LEN=FA%JPXNOM) CLACTI 
CHARACTER(LEN=FA%JPXNOM) CLNOMA
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA

!**
!     1.  -  CONTROLES ET INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FACOCH_MT',0,ZHOOK_HANDLE)
CLACTI=''
LLRLFI=.FALSE.
LLMESS=.FALSE.
LLNOMU=.FALSE.
ILPRFU=INT (LEN (CDPREF), JPLIKB)
ILSUFU=INT (LEN (CDSUFF), JPLIKB)
IRANC1=0
IRANC2=0
INUMER(1)=KNUME1
INUMER(2)=KNUME2
LLVERF(1)=.FALSE.
LLVERF(2)=.FALSE.
IRANG(2)=0
!
DO J=1,2
INUMFI=J
CALL FANUMU_FORT                       &
&               (FA, INUMER(J),IRANG(J))
!
IF (IRANG(J).EQ.0) THEN
  IREP=-51
  GOTO 1001
ENDIF
!
!         Verrouillage eventuel du fichier.
!
IF (FA%LFAMUL) CALL LFIVER_FORT                                  &
&                              (FA%LFI, FA%FICHIER(IRANG(J))%VRFICH,'ON')
LLVERF(J)=FA%LFAMUL
!
IF (FA%FICHIER(IRANG(J))%LCREAF) THEN
  IREP=-85
  GOTO 1001
ENDIF
!*
!       FABRICATION DU NOM D'ARTICLE VIA LE SOUS-PROGRAMME "FANFAR"
!            ( controles de CDPREF, KNIVAU, CDSUFF inclus )
!
CALL FANFAR_FORT                                &
&               (FA, IREP,IRANG(J),CDPREF,KNIVAU, &
&             CDSUFF,CLNOMA,IB1PAR,               &
&             ILPRFU,ILSUFU,ILNOMU)
IF (IREP.NE.0) GOTO 1001
ENDDO
!
LLNOMU=.TRUE.
!**
!     2.  -  LECTURE DE L'ARTICLE SUR LE FICHIER, CONTROLES.
!-----------------------------------------------------------------------
!
CALL LFINFO_FORT                                      &
&               (FA%LFI, IREP,KNUME1,CLNOMA(1:ILNOMU), &
&             ILONGA,IPOSEX)
!
IF (IREP.NE.0) THEN
  LLRLFI=.TRUE.
  GOTO 1001
ELSEIF (ILONGA.EQ.0) THEN
  IREP=-89
  GOTO 1001
ELSEIF (ILONGA.GT.FA%JPXCHA+2) THEN
  IREP=-90
  GOTO 1001
ENDIF
!
ALLOCATE (IVALCO (ILONGA))
CALL LFILEC_FORT                             &
&               (FA%LFI, IREP,KNUME1,         &
&             CLNOMA(1:ILNOMU),IVALCO,ILONGA)
LLRLFI=IREP.NE.0
IF (LLRLFI) GOTO 1001
!
IF (IVALCO(1).LT.-1.OR.IVALCO(1).GT.3.OR.                        &
&    IVALCO(2).LT.0 .OR.IVALCO(2).GT.1.OR.                        &
&    (IVALCO(1).GT.0.AND.IVALCO(2).EQ.1.AND.IVALCO(4).LT.0)) THEN
  IREP=-91
  GOTO 1001
ELSE
  LLCOSP=IVALCO(2).EQ.1
ENDIF
!**
!     3.  -  CONTROLE DE COHERENCE ENTRE LES FICHIERS, VIS-A-VIS DU TYPE
!            DE DONNEES LUES (points de grille/coefficients spectraux).
!-----------------------------------------------------------------------
!
IRANC1=FA%FICHIER(IRANG(1))%NUCADR
IRANC2=FA%FICHIER(IRANG(2))%NUCADR
INPAHE=(1+FA%CADRE(IRANC1)%NLATIT)/2
LLMLAM1=FA%CADRE(IRANC1)%NTYPTR.LE. -1
LLMLAM2=FA%CADRE(IRANC2)%NTYPTR.LE. -1
!
IF (IRANC1.NE.IRANC2) THEN
!
!         On a pris ici une optique souple: n'est fatale qu'une erreur
!     vraiment grossiere. Toute autre discordance est signalee par un
!     message global de niveau 1.
!
  IF ( (LLMLAM1.AND..NOT.LLMLAM2).OR.                             &
&       (LLMLAM2.AND..NOT.LLMLAM1).OR.                             &
&       (LLCOSP.AND.((.NOT.LLMLAM1.AND..NOT.LLMLAM2.AND.           &
&                     FA%CADRE(IRANC1)%MTRONC.NE.FA%CADRE(IRANC2)%MTRONC) .OR. &
&                    (LLMLAM1.AND.LLMLAM2.AND.                     &
&                     FA%CADRE(IRANC1)%MTRONC.NE.FA%CADRE(IRANC2)%MTRONC.AND.  &
&                     FA%CADRE(IRANC1)%NTYPTR.NE.FA%CADRE(IRANC2)%NTYPTR ))    &
&                    ).OR.                                         &
&       (.NOT.LLCOSP.AND.(FA%CADRE(IRANC1)%NLATIT.NE.                    &
&                         FA%CADRE(IRANC2)%NLATIT.OR.                    &
&                         FA%CADRE(IRANC1)%NVAPDG.NE.FA%CADRE(IRANC2)%NVAPDG)) &
&     ) THEN
    IREP=-112
    GOTO 1001
!
  ELSEIF (.NOT.LLCOSP) THEN
!
    IF (.NOT.LLMLAM1.AND..NOT.LLMLAM2) THEN
       INPAHEL=INPAHE
    ELSE
       INPAHEL=8
    ENDIF
    DO JLAT=1,INPAHEL
    LLMESS=LLMESS.OR.FA%CADRE(IRANC1)%NLOPAR(JLAT).NE. &
&           FA%CADRE(IRANC2)%NLOPAR(JLAT)
    ENDDO
!
    IF (LLMESS) THEN
      IREP=-112
      GOTO 1001
    ENDIF
!
  ENDIF
!
  LLMESS=FA%CADRE(IRANC1)%MTRONC.NE.FA%CADRE(IRANC2)%MTRONC.OR. &
&         FA%CADRE(IRANC1)%NTYPTR.NE.FA%CADRE(IRANC2)%NTYPTR.OR. &
&       (KNIVAU.GT.0.AND.(FA%CADRE(IRANC1)%NNIVER.NE.      &
&                         FA%CADRE(IRANC2)%NNIVER).OR.     &
&                        (FA%CADRE(IRANC1)%SPREFE.NE.      &
&                         FA%CADRE(IRANC2)%SPREFE)).OR.    &
&         FA%CADRE(IRANC1)%NLATIT.NE.FA%CADRE(IRANC2)%NLATIT.OR. &
&         FA%CADRE(IRANC1)%SSLAPO.NE.FA%CADRE(IRANC2)%SSLAPO.OR. &
&         FA%CADRE(IRANC1)%SCLOPO.NE.FA%CADRE(IRANC2)%SCLOPO.OR. &
&         FA%CADRE(IRANC1)%SSLOPO.NE.FA%CADRE(IRANC2)%SSLOPO.OR. &
&         FA%CADRE(IRANC1)%SCODIL.NE.FA%CADRE(IRANC2)%SCODIL
!
  IF (.NOT.LLMESS) THEN
!
    IF (.NOT.LLMLAM1.AND..NOT.LLMLAM2) THEN
       INPAHEL=INPAHE
       IZPAHEL=INPAHE
       ISPAHEL=INPAHE
    ELSE
       INPAHEL=JNEXPL
       IZPAHEL=0
       ISPAHEL=JNGEOM
    ENDIF
    DO JLAT=1,INPAHEL
    LLMESS=FA%CADRE(IRANC1)%NLOPAR(JLAT).NE.FA%CADRE(IRANC2)%NLOPAR(JLAT) &
&           .OR.LLMESS
    ENDDO
    DO JLAT=1,IZPAHEL
    LLMESS=FA%CADRE(IRANC1)%NOZPAR(JLAT).NE.FA%CADRE(IRANC2)%NOZPAR(JLAT) &
&           .OR.LLMESS
    ENDDO
    DO JLAT=1,ISPAHEL
    LLMESS=FA%CADRE(IRANC1)%SINLAT(JLAT).NE.FA%CADRE(IRANC2)%SINLAT(JLAT) &
&           .OR.LLMESS
    ENDDO
!
    IF (.NOT.LLMESS.AND.KNIVAU.GT.0) THEN
!
      DO JNIV=0,FA%CADRE(IRANC1)%NNIVER
      LLMESS=FA%CADRE(IRANC1)%SFOHYB(1,JNIV).NE.    &
&             FA%CADRE(IRANC2)%SFOHYB(1,JNIV).OR.    &
&      LLMESS.OR.FA%CADRE(IRANC1)%SFOHYB(2,JNIV).NE. &
&             FA%CADRE(IRANC2)%SFOHYB(2,JNIV)
      ENDDO
!
    ENDIF
!
  ENDIF
!
ENDIF
!**
!     4.  -  ECRITURE DE L'ARTICLE "CHAMP" SUR LE FICHIER.
!-----------------------------------------------------------------------
!
!        Deverrouillage eventuel de l'unite logique d'entree.
!
IF (LLVERF(1)) CALL LFIVER_FORT                                   &
&                              (FA%LFI, FA%FICHIER(IRANG(1))%VRFICH,'OFF')
LLVERF(1)=.FALSE.
!
CALL LFIECR_FORT                                      &
&               (FA%LFI, IREP,KNUME2,CLNOMA(1:ILNOMU), &
&             IVALCO,ILONGA)
INUMFI=2
LLRLFI=IREP.NE.0
IF (LLRLFI) GOTO 1001
!
!  Controle de l'homogeneite du type de rangement de coeff. spectraux
!  parmi les champs lus/ecrits: ces champs compactes avec
!  IGRIB=-1 ou 3 doivent etre ranges comme dans le modele ("verticalement"
!  soit selon des colonnes JM=cst consecutives) et contrairement si compactes
!  avec IGRIB= 0,1 ou 2.
!
IRANG2 = IRANG(2)
IGRIB = IVALCO(1)
IF (LLCOSP) THEN
  IF (IGRIB.EQ.-1 .OR. IGRIB.EQ.3) THEN
    FA%FICHIER(IRANG2)%NRASVE=FA%FICHIER(IRANG2)%NRASVE+1
    IF (FA%FICHIER(IRANG2)%NRASVE.EQ.1 .AND. FA%FICHIER(IRANG2)%NRASHO.GT.0) THEN
      WRITE(FA%NULOUT,*)                                 &
&      '------------------------------------------------'
      WRITE(FA%NULOUT,*)' FACOCH : WARNING !!!!!           '
      WRITE(FA%NULOUT,*)                                 &
&      ' Un champ de coef. spect. avec rangt type modele'
      WRITE(FA%NULOUT,*)' va etre ecrit sur l''unite ',KNUME2, &
&                ' alors que'
      WRITE(FA%NULOUT,*)                                &
&      ' d''autres champs y ont un rangement different.'
      WRITE(FA%NULOUT,*)                                 &
&      '------------------------------------------------'
    ENDIF
  ELSEIF (IGRIB.GE.0 .AND. IGRIB.LE.2) THEN
    FA%FICHIER(IRANG2)%NRASHO=FA%FICHIER(IRANG2)%NRASHO+1
    IF (FA%FICHIER(IRANG2)%NRASHO.EQ.1 .AND. FA%FICHIER(IRANG2)%NRASVE.GT.0) THEN
      WRITE(FA%NULOUT,*)                                 &
&      '------------------------------------------------'
      WRITE(FA%NULOUT,*)' FACOCH : WARNING !!!!!           '
      WRITE(FA%NULOUT,*)                               &
&      ' Un champ de coef. spect. avec rangt autre que'
      WRITE(FA%NULOUT,*)                                     &
&      ' celui du modele va etre ecrit sur l''unite ', KNUME2
      WRITE(FA%NULOUT,*)                                  &
&      ' alors que d''autres champs y ont le rangt modele'
      WRITE(FA%NULOUT,*)                                 &
&      '------------------------------------------------'
    ENDIF
  ENDIF
ENDIF
!
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE, AVEC "ABORT" EVENTUEL,
!            VIA LE SOUS-PROGRAMME "FAIPAR" .
!-----------------------------------------------------------------------
!
1001 CONTINUE
IF (ALLOCATED( IVALCO )) DEALLOCATE ( IVALCO )
KREP=IREP
LLFATA=LLMOER (IREP,IRANG(INUMFI))
!
!        Deverrouillage eventuel des fichiers.
!
IF (LLVERF(1)) CALL LFIVER_FORT                                   &
&                              (FA%LFI, FA%FICHIER(IRANG(1))%VRFICH,'OFF')
IF (LLVERF(2)) CALL LFIVER_FORT                                   &
&                              (FA%LFI, FA%FICHIER(IRANG(2))%VRFICH,'OFF')
!
CLNSPR='FACOCH'
!
!        Messages d'avertissement eventuels.
!
IF (FA%NIMSGA.NE.0.AND.IREP.EQ.0) THEN
!
  IF (LLMESS) THEN
    INIMES=1
    WRITE (UNIT=CLMESS,FMT='(''*ATTENTION* - LES UNITES'',I3,      &
&   '' ET'',I3,'' ONT DES CARACTERISTIQUES "CADRE" DIFFERENTES'')') &
&    KNUME1,KNUME2
    CALL FAIPAR_FORT                                         &
&                   (FA, JPNIIL,INIMES,IREP,.FALSE.,CLMESS, &
&                 CLNSPR,CLACTI,.FALSE.)
  ELSEIF (IRANC1.NE.IRANC2) THEN
    INIMES=1
    WRITE (UNIT=CLMESS,FMT='(''REMARQUE: CADRES '''''',A,        &
&           '''''' ET '''''',A,                                   &
&           '''''' DISTINCTS MAIS DE CONTENU IDENTIQUE (UNITES'', &
&           I3,'' ET'',I3,'' )'')')                               &
&      FA%CADRE(IRANC1)%CNOMCA(1:FA%CADRE(IRANC1)%NLCCAD),                    &
&      FA%CADRE(IRANC2)%CNOMCA(1:FA%CADRE(IRANC2)%NLCCAD),KNUME1,KNUME2
    CALL FAIPAR_FORT                                         &
&                   (FA, JPNIIL,INIMES,IREP,.FALSE.,CLMESS, &
&                 CLNSPR,CLACTI,.FALSE.)
  ENDIF
!
ENDIF
!
IF (LLFATA) THEN
  INIMES=2
ELSE
  INIMES=IXNVMS(IRANG(INUMFI))
ENDIF
!
IF (.NOT.LLFATA.AND.INIMES.NE.2)  THEN 
  IF (LHOOK) CALL DR_HOOK('FACOCH_MT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
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
WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KNUME1='',I3,           &
&       '', KNUME2='',I3,'', CDPREF='''''',A,'''''', KNIVAU='',I6, &
&       '', CDSUFF='''''',A,'''''''')') KREP,KNUME1,KNUME2,        &
&   CLPREF(1:ILPREF),KNIVAU,CLSUFF(1:ILSUFF)
!
IF (IREP.EQ.-112) THEN
  INUMRO=1000*KNUME1+KNUME2
ELSE
  INUMRO=INUMER(INUMFI)
ENDIF
!
CALL FAIPAR_FORT                                     &
&               (FA, INUMRO,INIMES,IREP,LLFATA,CLMESS, &
&                CLNSPR, CLNOMA(1:ILNOMU),LLRLFI)
!
IF (LHOOK) CALL DR_HOOK('FACOCH_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"
#include "facom2.ixnvms.h"

END SUBROUTINE FACOCH_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FACOCH64                                      &
&           (KREP, KNUME1, KNUME2, CDPREF, KNIVAU, CDSUFF)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUME1                                 ! IN   
INTEGER (KIND=JPLIKB)  KNUME2                                 ! IN   
CHARACTER (LEN=*)      CDPREF                                 ! IN   
INTEGER (KIND=JPLIKB)  KNIVAU                                 ! IN   
CHARACTER (LEN=*)      CDSUFF                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FACOCH_FORT                                             &
&           (FA, KREP, KNUME1, KNUME2, CDPREF, KNIVAU, CDSUFF)

END SUBROUTINE FACOCH64

SUBROUTINE FACOCH                                        &
&           (KREP, KNUME1, KNUME2, CDPREF, KNIVAU, CDSUFF)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUME1                                 ! IN   
INTEGER (KIND=JPLIKM)  KNUME2                                 ! IN   
CHARACTER (LEN=*)      CDPREF                                 ! IN   
INTEGER (KIND=JPLIKM)  KNIVAU                                 ! IN   
CHARACTER (LEN=*)      CDSUFF                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FACOCH_MT                                               &
&           (FA, KREP, KNUME1, KNUME2, CDPREF, KNIVAU, CDSUFF)

END SUBROUTINE FACOCH

SUBROUTINE FACOCH_MT                                         &
&           (FA, KREP, KNUME1, KNUME2, CDPREF, KNIVAU, CDSUFF)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUME1                                 ! IN   
INTEGER (KIND=JPLIKM)  KNUME2                                 ! IN   
CHARACTER (LEN=*)      CDPREF                                 ! IN   
INTEGER (KIND=JPLIKM)  KNIVAU                                 ! IN   
CHARACTER (LEN=*)      CDSUFF                                 ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUME1                                 ! IN   
INTEGER (KIND=JPLIKB)  INUME2                                 ! IN   
INTEGER (KIND=JPLIKB)  INIVAU                                 ! IN   
! Convert arguments

INUME1     = INT (    KNUME1, JPLIKB)
INUME2     = INT (    KNUME2, JPLIKB)
INIVAU     = INT (    KNIVAU, JPLIKB)

CALL FACOCH_FORT                                             &
&           (FA, IREP, INUME1, INUME2, CDPREF, INIVAU, CDSUFF)

KREP       = INT (      IREP, JPLIKM)

END SUBROUTINE FACOCH_MT

!INTF KREP            OUT 
!INTF KNUME1        IN    
!INTF KNUME2        IN    
!INTF CDPREF        IN    
!INTF KNIVAU        IN    
!INTF CDSUFF        IN    
