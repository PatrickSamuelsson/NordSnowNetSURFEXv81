! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FAISAN_FORT                                           &
&                     (FA,  KREP, KNUMER, CDNOMA, KDONNE, KLONGD)
USE FA_MOD, ONLY : FA_COM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!      Sous-programme d'ecriture d'un article de donnees non assimila-
!     bles a un champ horizontal sur un fichier ARPEGE.
!       ( Integration Simple d'un Article Non code )
!**
!    Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                KNUMER (Entree) ==> Numero de l'unite logique;
!                CDNOMA (Entree) ==> Nom de l'article;
!    ( Tableau ) KDONNE (Entree) ==> Donnees a ecrire;
!                KLONGD (Entree) ==> Nombre de mots a ecrire.
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KNUMER, KLONGD
!
INTEGER (KIND=JPLIKB) ILCDNO, IRANG, IREP
INTEGER (KIND=JPLIKB) ILNOMA, INIMES, ILACTI
!
INTEGER (KIND=JPLIKB) KDONNE (KLONGD)
!
LOGICAL LLVERF, LLRLFI
!
CHARACTER CDNOMA*(*)
!
CHARACTER(LEN=FA%JPXNOM) CLACTI 
CHARACTER(LEN=FA%JPXNOM) CLNOMA
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA
LOGICAL                  LLECRI

!**
!     1.  -  CONTROLES ET INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FAISAN_MT',0,ZHOOK_HANDLE)
CLACTI=''
LLVERF=.FALSE.
LLRLFI=.FALSE.
ILCDNO=INT (LEN (CDNOMA), JPLIKB)
CALL FANUMU_FORT                 &
&               (FA, KNUMER,IRANG)

!
IF (IRANG.EQ.0) THEN
  IREP=-51
  GOTO 1001
ELSEIF (KLONGD.LE.0) THEN
  IREP=-64
  GOTO 1001
ELSEIF (ILCDNO.LE.0) THEN
  IREP=-65
  GOTO 1001
ENDIF
!
!         Verrouillage eventuel du fichier.
!
IF (FA%LFAMUL) CALL LFIVER_FORT                               &
&                              (FA%LFI, FA%FICHIER(IRANG)%VRFICH,'ON')
LLVERF=FA%LFAMUL
!
IF (FA%FICHIER(IRANG)%LCREAF) THEN
  IREP=-85
  GOTO 1001
ELSEIF (CDNOMA.EQ.FA%CPCACH.OR.CDNOMA.EQ.FA%CPCADI.OR.  &
&       CDNOMA.EQ.FA%CPCAFS.OR.CDNOMA.EQ.FA%CPCARP.OR.  &
&       CDNOMA.EQ.FA%CPDATE.OR.CDNOMA.EQ.FA%CPDATX.OR.  &
&       CDNOMA.EQ.FA%FICHIER(IRANG)%CIDENT) THEN
  IREP=-111
  GOTO 1001
ENDIF
!**
!     2.  -  ECRITURE DE L'ARTICLE DE DONNEES SUR LE FICHIER.
!-----------------------------------------------------------------------
!
ILNOMA=MIN ( FA%NCPCAD, INT (LEN (CDNOMA), JPLIKB) )
CLNOMA(1:ILNOMA)=CDNOMA(1:ILNOMA)
!

LLECRI = .FALSE.
IF (FA%FICHIER(IRANG)%NCOGRIF (12) > 0) THEN
  CALL WGRIB1 (LLECRI)
ENDIF
IF (.NOT. LLECRI) THEN
  CALL LFIECR_FORT                                       &
  &               (FA%LFI, IREP,KNUMER,CLNOMA(1:ILNOMA), &
  &                KDONNE,KLONGD)
ENDIF
LLRLFI=IREP.NE.0
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE, AVEC "ABORT" EVENTUEL,
!            VIA LE SOUS-PROGRAMME "FAIPAR" .
!-----------------------------------------------------------------------
!
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
  IF (LHOOK) CALL DR_HOOK('FAISAN_MT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='FAISAN'
!
IF (IREP.NE.-65) THEN
  ILACTI=MIN (ILCDNO,FA%NCPCAD)
  CLACTI(1:ILACTI)=CDNOMA(:ILACTI)
ELSE
  ILACTI=8
  CLACTI(1:ILACTI)=FA%CHAINC(:ILACTI)
ENDIF
!
WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KNUMER='',I3, &
&       '', CDNOMA='''''',A,'''''', KLONGD='',I8)')     &
&   KREP,KNUMER,CLACTI(1:ILACTI),KLONGD
CALL FAIPAR_FORT                                       &
&               (FA, KNUMER,INIMES,IREP,LLFATA,CLMESS, &
&                CLNSPR, CLACTI(1:ILACTI),LLRLFI)
!
IF (LHOOK) CALL DR_HOOK('FAISAN_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"
#include "facom2.ixnvms.h"
#include "falgra.h"

SUBROUTINE WGRIB1 (LDECRI)

LOGICAL :: LDECRI

INTEGER (KIND=JPLIKB), PARAMETER :: ILONGD = 5_JPLIKB

INTEGER (KIND=JPLIKB)  IDONNE (ILONGD)
CHARACTER(LEN=16)      CLGRIB, CL7777
INTEGER (KIND=JPLIKB)  J, IL7777, INGRIB, IREP8, ILGRIBA, ILGRIBB, IGRIBED
LOGICAL                LLNOMM, LLERFA, LLIMST
CHARACTER (LEN=256)    CLNOMF, CLNOMD, CLNOMB
CHARACTER (LEN=16)     CLSTTU
INTEGER (KIND=JPLIKB)  INIMES
INTEGER (KIND=JPLIKM)  IREP4

LDECRI = .FALSE.

IF (KLONGD < 10) RETURN ! Trop petit pour etre un GRIB1

INGRIB = KDONNE (1)                        ! Methode de codage
CLGRIB = TRANSFER (KDONNE (4:5), CLGRIB)   ! Debut de l'entete GRIB1

IF ((INGRIB /= 3) .AND. (.NOT. FALGRA (INGRIB))) RETURN

IF (CLGRIB (1:4) /= 'GRIB') RETURN

IGRIBED = MOD (ICHAR (CLGRIB (8:8)), 256)

IF ((IGRIBED /= 1) .AND. (IGRIBED /= 2)) RETURN

! Recuperation de la longueur du message GRIB

ILGRIBA = 0

IF (IGRIBED == 1) THEN
  DO J = 5, 7
    ILGRIBA = 256 * ILGRIBA + MOD (ICHAR (CLGRIB (J:J)), 256)
  ENDDO
ELSE
  DO J = 9, 16
    ILGRIBA = 256 * ILGRIBA + MOD (ICHAR (CLGRIB (J:J)), 256)
  ENDDO
ENDIF

! On cherche maintenant la fin du message GRIB1

CL7777 = TRANSFER (KDONNE (KLONGD-1:KLONGD), CL7777)
IL7777 = LEN (CL7777)

DO J = 0, IL7777-4
  IF (CL7777 (IL7777-J-3:IL7777-J) == '7777') EXIT
ENDDO

IF (J == -1) RETURN

! Calcul de la longueur du message GRIB en octets

ILGRIBB = (KLONGD-3)*8 - J

! On verifie que les deux longueurs correspondent

IF (ILGRIBA /= ILGRIBB) RETURN

! Ouverture du fichier externe

IF (FA%FICHIER(IRANG)%NFILEP == 0) THEN
  CALL LFIOPT_FORT                                        &
&                 (FA%LFI, IREP, KNUMER, LLNOMM, CLNOMF,  &
&                  CLSTTU, LLERFA, LLIMST, INIMES)
  IF (IREP /= 0) RETURN
  CALL FILEPARSE (CLNOMF, CLNOMD, CLNOMB)
  CLNOMF = TRIM (CLNOMD)//'GRIB'//TRIM (CLNOMB)
  CALL FI_FOPEN (FA%FICHIER(IRANG)%NFILEP, CLNOMF, "a")
  IF (FA%FICHIER(IRANG)%NFILEP == 0) THEN
    CALL FI_ERRNO (IREP4)
    IREP = IREP4
    RETURN
  ENDIF
ENDIF

! Ecriture de l'article GRIB

CALL FI_FWRITE (IREP8, KDONNE (4), ILGRIBA, 1_JPLIKB, &
              & FA%FICHIER(IRANG)%NFILEP)
IF (IREP8 /= 1) THEN
  CALL FI_ERRNO (IREP4)
  IREP = IREP4
ELSE
  IREP = 0
ENDIF

FA%FICHIER(IRANG)%NOFFST = FA%FICHIER(IRANG)%NOFFST + ILGRIBA

! Ecriture d'un article referencant le champ GRIB

IDONNE (1:3) = KDONNE (1:3)
IDONNE (4) = FA%FICHIER(IRANG)%NOFFST
IDONNE (5) = ILONGD

CALL LFIECR_FORT                                         &
&               (FA%LFI, IREP, KNUMER, CLNOMA(1:ILNOMA), &
&                IDONNE, ILONGD)

IF (IREP /= 0) RETURN

LDECRI = .TRUE.

END SUBROUTINE WGRIB1

SUBROUTINE FILEPARSE (CDNOMF, CDNOMD, CDNOMB)

CHARACTER (LEN=*) :: CDNOMF, CDNOMD, CDNOMB

INTEGER (KIND=JPLIKB) :: I

I = INDEX (CDNOMF, "/", .TRUE.)

IF (I == 0) THEN
  CDNOMD = ''
  CDNOMB = CDNOMF
ELSE
  CDNOMD = CDNOMF (1:I)
  CDNOMB = CDNOMF (I+1:)
ENDIF

END SUBROUTINE FILEPARSE

END SUBROUTINE FAISAN_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FAISAN64                              &
&           (KREP, KNUMER, CDNOMA, KDONNE, KLONGD)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 ! IN   
INTEGER (KIND=JPLIKB)  KLONGD                                 ! IN   
INTEGER (KIND=JPLIKB)  KDONNE (KLONGD)                        ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAISAN_FORT                                     &
&           (FA, KREP, KNUMER, CDNOMA, KDONNE, KLONGD)

END SUBROUTINE FAISAN64

SUBROUTINE FAISAN                                &
&           (KREP, KNUMER, CDNOMA, KDONNE, KLONGD)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 ! IN   
INTEGER (KIND=JPLIKM)  KLONGD                                 ! IN   
INTEGER (KIND=JPLIKB)  KDONNE (KLONGD)                        ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAISAN_MT                                       &
&           (FA, KREP, KNUMER, CDNOMA, KDONNE, KLONGD)

END SUBROUTINE FAISAN

SUBROUTINE FAISAN_MT                                 &
&           (FA, KREP, KNUMER, CDNOMA, KDONNE, KLONGD)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 ! IN   
INTEGER (KIND=JPLIKM)  KLONGD                                 ! IN   
INTEGER (KIND=JPLIKB)  KDONNE (KLONGD)                        ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  ILONGD                                 ! IN   
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)
ILONGD     = INT (    KLONGD, JPLIKB)

CALL FAISAN_FORT                                     &
&           (FA, IREP, INUMER, CDNOMA, KDONNE, ILONGD)

KREP       = INT (      IREP, JPLIKM)

END SUBROUTINE FAISAN_MT

!INTF KREP            OUT                               
!INTF KNUMER        IN                                  
!INTF CDNOMA        IN                                  
!INTF KDONNE        IN    KDIMS=KLONGD                  
!INTF KLONGD        IN                                  

