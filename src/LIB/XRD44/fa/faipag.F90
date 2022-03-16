! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FAIPAG_FORT                                              &
&                     (FA,  KREP,  KNUMER,  CDPREF, KNIVAU, CDSUFF, &
&                      KNIPAR, YDGR1TAB)
USE FA_MOD, ONLY : FA_COM, JPNIIL, FAGR1TAB, NUNDEF
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!      Sous-programme du logiciel de Fichiers ARPEGE:
!      Initialisation de quelques descripteurs de l'entete Gribex
!      (section 1) relatifs au parametre, a partir du nom FA du champ.
!**
!    Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                KNUMER (Entree) ==> Numero de l'unite logique;
!                CDPREF (Entree) ==> Prefixe eventuel du nom d'article;
!                KNIVAU (Entree) ==> Niveau vertical eventuel;
!                CDSUFF (Entree) ==> Suffixe eventuel du nom d'article;
!    ( Tableau ) KNIPAR (Sortie) ==> quelques descripteurs de la section 1 de
!                                    GRIBEX (KNIPAR(1)  =KSEC1(1),
!                                            KNIPAR(2:5)=KSEC1(6:9),
!                                            KNIPAR(7)=KSEC1(23) ) et un
!                                    indicateur de type de champ (KNIPAR(6)=
!                                    KSEC1(18)):0->RAS; 2->min/max; 4->cumul,
!                                    8->cumul depuis le debut
!                                    
!
!     Original  : 06 juillet 2004, Denis Paradis DSI/DEV
!     --------
!
!     Modifications
!     -------------
!       R. El Ouaraini: 03-Oct-2006, enlever le commentaire de l'initialisation de KNIPAR(3) pour
!                  les types de niveaux :  hauteur, iso-tourb potent, isentrope et modele.
!       R. El Khatib 24-Jul-2015 No use of the correspondence table if no external grib file
!
!*
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KNUMER, KNIVAU, KNIPAR(8)
!
CHARACTER (LEN=*) CDPREF, CDSUFF
!
TYPE (FAGR1TAB) :: YDGR1TAB
!
INTEGER (KIND=JPLIKB) IRANG, INIMES, J, JMEM, ILENMIN
!
INTRINSIC LEN_TRIM
!
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA
LOGICAL                  LLNIVA
TYPE (FAGR1TAB)          YDGR1DUM
!

!**
!     0.  -  INITIALISATIONS PREALABLES
!-----------------------------------------------------------------------
!
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FAIPAG_MT',0,ZHOOK_HANDLE)
KREP=0

CALL FANUMU_FORT                 &
&               (FA, KNUMER,IRANG)
!
IF (IRANG.LE.0.OR.IRANG.GT.FA%JPNXFA) THEN
  KREP=-66
  GOTO 1001
ENDIF
!
!  DEFAUTS:
!
! Numero de version de la table de code parametres
KNIPAR(1)=1
! Indicateur de parametre (255=> valeur manquante)
KNIPAR(2)=255
! Indicateur de type de niveau (1=> surface)
KNIPAR(3)=1
! Niveau 1, Niveau 2 et type de champs
KNIPAR(4:6)=0
!
KNIPAR(7)=0
!**
!     1.  -  UTILISATION DE LA TABLE DE CORRESPONDANCE (SEULEMENT POUR UN FICHIER EXTERNE AU FORMAT GRIB)
!--------------------------------------------------------------------------------------------------------
!
!
IF ((ANY (YDGR1TAB%NCODPA == YDGR1DUM%NCODPA)).AND.FA%FICHIER(IRANG)%NCOGRIF(12)==0) THEN
  JMEM = 0
  DO J = 1,FA%NBPARC
    ILENMIN=MIN(LEN_TRIM(CDSUFF),LEN_TRIM(FA%YGR1TAB(J)%CISUFF))
    IF (CDPREF(1:LEN_TRIM(CDPREF)).EQ.FA%YGR1TAB(J)%CIPREF(1:LEN_TRIM(FA%YGR1TAB(J)%CIPREF)) .AND. &
&       CDSUFF(1:ILENMIN).EQ.FA%YGR1TAB(J)%CISUFF(1:ILENMIN)) THEN
      JMEM = J
      EXIT
    ELSEIF (CDPREF(1:LEN_TRIM(CDPREF))//CDSUFF(1:LEN_TRIM(CDSUFF)).EQ. &
&     FA%YGR1TAB(J)%CIPREF(1:LEN_TRIM(FA%YGR1TAB(J)%CIPREF))// &
&     FA%YGR1TAB(J)%CISUFF(1:LEN_TRIM(FA%YGR1TAB(J)%CISUFF))) THEN
      JMEM = J
      EXIT
    ENDIF
  ENDDO
  IF (FA%LFAMOP.AND.JMEM.EQ.0) THEN
    WRITE (UNIT=FA%NULOUT,FMT=*)                                  &
&           'FAIPAG: WARNING, pas de reference GRIB pour ',        &
&           CDPREF(1:LEN_TRIM(CDPREF))//CDSUFF(1:LEN_TRIM(CDSUFF))
    WRITE (UNIT=FA%NULOUT,FMT=*)'       Les defauts seront utilises'
    GOTO 1001
  ENDIF
  IF (JMEM /= 0) THEN
    YDGR1TAB = FA%YGR1TAB(JMEM)
  ELSE
    YDGR1TAB = YDGR1DUM
  ENDIF
ENDIF

IF (ALL (YDGR1TAB%NCODPA /= YDGR1DUM%NCODPA)) THEN
  KNIPAR   = YDGR1TAB%NCODPA(1:8)
  LLNIVA   = YDGR1TAB%LFNIVA
ELSE  
  LLNIVA=.FALSE.
ENDIF

!**
!     2.  -  INITIALISATION DU NIVEAU (AUTRE QUE 0)
!--------------------------------------------------
!
!     2.1 -  Champs sur un niveau isobare
!
IF (CDPREF(1:LEN_TRIM(CDPREF)).EQ.'P') THEN
!  La pression est sur 5 chiffres: on la ramene a l'hPa
!     et on recree le niveau 1000 hPa
!
!  Si KNIVAU < 100, la pression fait moins d'un hPa et
!  on utilise une extension du GRIB introduite par le CEP:
!       KSEC1(7) = 210 (au lieu de 100)
!    et KSEC1(8) = pression en Pa
!
  IF (KNIVAU .GE. 100) THEN
    IF (KNIPAR (2) == 255) KNIPAR(3)=100
    IF (.NOT. LLNIVA)      KNIPAR(4)=KNIVAU/100
  ELSEIF (KNIVAU==0) THEN
    IF (KNIPAR (2) == 255) KNIPAR(3)=100
    IF (.NOT. LLNIVA)      KNIPAR(4)=1000
  ELSE
    IF (KNIPAR (2) == 255) KNIPAR(3)=210
    IF (.NOT. LLNIVA)      KNIPAR(4)=KNIVAU
  ENDIF
!
!     2.2 -  Champs sur un niveau hauteur
!
ELSEIF (CDPREF(1:LEN_TRIM(CDPREF)).EQ.'H') THEN
  IF (KNIPAR (2) == 255) KNIPAR(3)=105
  IF (.NOT. LLNIVA)      KNIPAR(4)=KNIVAU
!
!     2.3 -  Champs sur un niveau iso-tourbillon-potentiel
!
!            ( unite SI = K m2 s-1 kg-1 = 10+6 PVU
!              mais l'unite retenu est le milliPVU: 10-9 SI)
ELSEIF (CDPREF(1:LEN_TRIM(CDPREF)).EQ.'V') THEN
  IF (KNIPAR (2) == 255) KNIPAR(3)=117
! KNIVAU est exprime en 1/10 PVU
  IF (.NOT. LLNIVA) THEN
    KNIPAR(4)=KNIVAU*100
    IF (KNIVAU==0) KNIPAR(4)=1000
  ENDIF
!
!     2.4 -  Champs sur un niveau isentrope
!
ELSEIF (CDPREF(1:LEN_TRIM(CDPREF)).EQ.'T') THEN
  IF (KNIPAR (2) == 255) KNIPAR(3)=107
  IF (.NOT. LLNIVA)      KNIPAR(4)=KNIVAU
!
!     2.5 -  Champs sur un niveau modele
!
ELSEIF (CDPREF(1:LEN_TRIM(CDPREF)).EQ.'S') THEN
  IF (KNIPAR (2) == 255) KNIPAR(3)=109
  IF (.NOT. LLNIVA)      KNIPAR(4)=KNIVAU
ELSEIF (CDPREF(1:LEN_TRIM(CDPREF)).EQ.'KT') THEN
  IF (KNIPAR (2) == 255) KNIPAR(3)=115
  IF (.NOT. LLNIVA) THEN
    SELECT CASE (KNIVAU)
      CASE (273)
        KNIPAR(4)=27315
      CASE (263)
        KNIPAR(4)=26315
      CASE DEFAULT
        KNIPAR(4)=KNIVAU*100
    END SELECT
  ENDIF
ENDIF
!
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE, AVEC "ABORT" EVENTUEL,
!            VIA LE SOUS-PROGRAMME "FAIPAR" .
!-----------------------------------------------------------------------
!
1001 CONTINUE
LLFATA=LLMOER (KREP,IRANG)
!
IF (FA%LFAMOP.OR.LLFATA) THEN
  INIMES=2
  CLNSPR='FAIPAG'
!
  WRITE (UNIT=FA%NULOUT,FMT=*)'FAIPAG: KNIPAR(1:7) = ',KNIPAR(1:7)
  WRITE (UNIT=FA%NULOUT,FMT=*)
  WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', IRANG='',I4,  &
&       '', CDPREF='''''',A,'''''', KNIVAU='',I6,         &
&       '', CDSUFF='''''',A,'''''''')')                   &
&     KREP, IRANG, CDPREF(1:LEN_TRIM(CDPREF)), KNIVAU,    &
&     CDSUFF(1:LEN_TRIM(CDSUFF))
  CALL FAIPAR_FORT                                        &
&                 (FA, KNUMER,INIMES,KREP,.FALSE.,CLMESS, &
&                  CLNSPR,CDPREF,.FALSE.)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('FAIPAG_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"

END SUBROUTINE FAIPAG_FORT

! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FAIPAG64                                        &
&           (KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, KNIPAR, &
&            YDGR1TAB)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                  FA_COM_DEFAULT_INIT,  &
&                  NEW_FA_DEFAULT,       &
&                  FAGR1TAB
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDPREF                                 ! IN   
INTEGER (KIND=JPLIKB)  KNIVAU                                 ! IN   
CHARACTER (LEN=*)      CDSUFF                                 ! IN   
INTEGER (KIND=JPLIKB)  KNIPAR     (8)                         !   OUT
TYPE (FAGR1TAB)        YDGR1TAB                               ! INOUT

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAIPAG_FORT                                               &
&           (FA, KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, KNIPAR, &
&            YDGR1TAB)

END SUBROUTINE FAIPAG64

SUBROUTINE FAIPAG                                          &
&           (KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, KNIPAR, &
&            YDGR1TAB)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                  FA_COM_DEFAULT_INIT,  &
&                  NEW_FA_DEFAULT,       &
&                  FAGR1TAB
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDPREF                                 ! IN   
INTEGER (KIND=JPLIKM)  KNIVAU                                 ! IN   
CHARACTER (LEN=*)      CDSUFF                                 ! IN   
INTEGER (KIND=JPLIKM)  KNIPAR     (8)                         !   OUT
TYPE (FAGR1TAB)        YDGR1TAB                               ! INOUT

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAIPAG_MT                                              &
&           (FA, KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, KNIPAR, YDGR1TAB)

END SUBROUTINE FAIPAG

SUBROUTINE FAIPAG_MT                                           &
&           (FA, KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, KNIPAR, &
&            YDGR1TAB)
USE FA_MOD, ONLY : FA_COM, FAGR1TAB
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDPREF                                 ! IN   
INTEGER (KIND=JPLIKM)  KNIVAU                                 ! IN   
CHARACTER (LEN=*)      CDSUFF                                 ! IN   
INTEGER (KIND=JPLIKM)  KNIPAR     (8)                         !   OUT
TYPE (FAGR1TAB)        YDGR1TAB                               ! INOUT
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  INIVAU                                 ! IN   
INTEGER (KIND=JPLIKB)  INIPAR     (8)                         !   OUT
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)
INIVAU     = INT (    KNIVAU, JPLIKB)

CALL FAIPAG_FORT                                            &
&           (FA, IREP, INUMER, CDPREF, INIVAU, CDSUFF, INIPAR, YDGR1TAB)

KREP       = INT (      IREP, JPLIKM)
KNIPAR     = INT (    INIPAR, JPLIKM)

END SUBROUTINE FAIPAG_MT

!INTF KREP            OUT                               
!INTF KNUMER        IN                                  
!INTF CDPREF        IN                                  
!INTF KNIVAU        IN                                  
!INTF CDSUFF        IN                                  
!INTF KNIPAR          OUT DIMS=8                        
!INTF YDGR1TAB      INOUT                               
