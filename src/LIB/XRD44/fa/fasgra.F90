SUBROUTINE FASGRA_FORT                                             &
&                     (FA,  KREP, CDNOMC, KLONGD)

USE FA_MOD, ONLY : FA_COM, FACADR, LGRIB2_LAM_BF
USE PARKIND1, ONLY : JPRB, JPIM
USE LFI_PRECISION
USE YOMHOOK , ONLY : LHOOK, DR_HOOK

IMPLICIT NONE
!****
!      Sous-programme de calcul de la taille maximale de l'entete GRIB pour
!     un champ horizontal.
!**
!    Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                CDNOMC (Entree) ==> Nom du cadre
!                KLONGD (Sortie) ==> Taille max. de l'entete
!
!

TYPE(FA_COM)   :: FA
INTEGER (KIND=JPLIKB) KREP, KLONGD
!
CHARACTER CDNOMC*(*)
!
INTEGER (KIND=JPLIKB) :: J
TYPE (FACADR), POINTER :: YLCADR
INTEGER (KIND=JPLIKB) :: IRANGC, IRANGC2, INUMER2, IRANG2
INTEGER (KIND=JPLIKB) :: ITYPTR, ISTROI
!
CHARACTER (LEN=*), PARAMETER :: CLNOM2 = '.dummy'
!
INTEGER (KIND=JPLIKB), PARAMETER :: IFLEVG = 1, ITRONC = 2, ILATIT = 4, IXLOPA = 5, INPAHE=(1+ILATIT)/2
INTEGER (KIND=JPLIKB), ALLOCATABLE :: IOZPAR (:), INLOPA (:)
REAL (KIND=JPDBLR),    ALLOCATABLE :: ZSINLA (:)

CHARACTER (LEN=16) :: CLNOMA, CLPREF, CLSUFF
INTEGER (KIND=JPLIKB) :: ILGRSP, ILGRGP, ILCHSP, ILCHGP
INTEGER (KIND=JPLIKB) :: ILNOMA, INBARI, INBARP, INIVAU
INTEGER (KIND=JPLIKB) :: INGRIB, INBPDG, INBCSP, ISTRON, IPUILA, IDMOPL, ILNOMC

REAL (KIND=JPDBLR) :: ZCHAMP (1000)
INTEGER (KIND=JPLIKB) :: IVALCO (1000)
REAL (KIND=JPDBLR) :: ZFOHYB (2,IFLEVG+1)
REAL (KIND=JPDBLR) :: ZUNDF
LOGICAL :: LLMLAM, LLLTLN, LLUNDF, LLMODC, LLREDF

REAL (KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('FASGRA_MT',0,ZHOOK_HANDLE)

#ifndef BUG_FAGRIB2
!#define FAGRIB2
#endif
#ifndef FAGRIB2

KLONGD = 2
GOTO 1001

#else

CALL FANUCA_FORT (FA, CDNOMC,IRANGC, .FALSE.)

YLCADR => FA%CADRE(IRANGC)

IF (YLCADR%IADDPK > 0) THEN
  KLONGD = YLCADR%IADDPK
  GOTO 1001
ENDIF

ISTROI = FA%NSTROI
FA%NSTROI = 1


LLMLAM = YLCADR%LIMLAM
LLLTLN = YLCADR%SINLAT(2) < 0 .AND. LLMLAM

! Taille d'un champ

IF (LLMLAM) THEN
  ILCHSP = YLCADR%NSFLAM
ELSE
  ILCHSP = (1+YLCADR%MTRONC)*(2+YLCADR%MTRONC)
ENDIF

ILCHGP = YLCADR%NVAPDG

! Geometrie minimale

ALLOCATE (IOZPAR (FA%JPXIND), INLOPA (FA%JPXPAH), ZSINLA (FA%JPXGEO))

ZFOHYB (1,:) = 1._JPDBLR
ZFOHYB (2,:) = 0._JPDBLR

IOZPAR = 1

IF (LLMLAM) THEN
  ITYPTR = - ITRONC
  ZSINLA (1:18) = YLCADR%SINLAT (1:18)
  INLOPA (1:8)  = (/ 1_JPLIKB, 1_JPLIKB, &
                 &   1_JPLIKB, ILATIT-2, &
                 &   1_JPLIKB, IXLOPA-2, &
                 &   0_JPLIKB, 0_JPLIKB /)
ELSE
  ZSINLA (1:INPAHE) = (/ (1._JPDBLR/REAL (J, JPDBLR), J = 1, INPAHE) /)
  ITYPTR = YLCADR%NTYPTR
  INLOPA = IXLOPA
ENDIF


! Definition d'un cadre sur la geometrie minimale

LLMODC = .FALSE.
LLREDF = .FALSE.
ILNOMC = INT (LEN (CLNOM2), JPLIKB)

CALL FACADI_FORT                                                                        &
&           (FA, KREP, CLNOM2, ITYPTR, YLCADR%SSLAPO, YLCADR%SCLOPO, YLCADR%SSLOPO,     &
&            YLCADR%SCODIL, ITRONC,  ILATIT, IXLOPA, INLOPA, IOZPAR, ZSINLA, IFLEVG,    &
&            YLCADR%SPREFE, ZFOHYB (1,:), ZFOHYB (2,:), LLMODC, LLREDF, 0_JPLIKB,       &
&            IRANGC2, ILNOMC, 1_JPLIKB)


! Ouverture d'un fichier

INUMER2 = 0
INBARP=0
INBARI=0

CALL FANOUV_FORT (FA, KREP, INUMER2, .FALSE., CLNOM2, 'UNKNOWN', .TRUE., &
                & .TRUE., 0_JPLIKB, INBARP, INBARI, CLNOM2)

CALL FANUMU_FORT (FA, INUMER2, IRANG2)

! Read grib_api templates

CALL FAIGRA_FORT (FA)


! Compactage et extrapolation de la taille d'un champ compacte

CALL FAGOTE_FORT (FA, KREP, INUMER2, 121_JPLIKB, 64_JPLIKB, 64_JPLIKB, 1_JPLIKB, 0_JPLIKB, 0_JPLIKB)

ZUNDF  = 0._JPDBLR
LLUNDF = .FALSE.
ZCHAMP = 0._JPDBLR
IVALCO = 0_JPLIKB
IF (LLLTLN) THEN
  CLPREF = 'H'
  INIVAU = 2
  CLSUFF = 'TEMPERATURE'
  ILGRGP = SIZE (IVALCO)
  CALL FACGRA_FORT (FA, KREP, IRANG2, CLPREF, INIVAU, CLSUFF, ZCHAMP, &
                  & .FALSE., IVALCO, ILGRGP, LLUNDF, ZUNDF)
  KLONGD = MAX (ILGRGP, 2)
ELSE
  CLPREF = 'S'
  INIVAU = 1
  CLSUFF = 'TEMPERATURE'
  ILGRGP = SIZE (IVALCO)
  CALL FACGRA_FORT (FA, KREP, IRANG2, CLPREF, INIVAU, CLSUFF, ZCHAMP, &
                  & .FALSE., IVALCO, ILGRGP, LLUNDF, ZUNDF)
  IF ((.NOT. LLMLAM) .OR. LGRIB2_LAM_BF) THEN
    ILGRSP = SIZE (IVALCO)
    CALL FACGRA_FORT (FA, KREP, IRANG2, CLPREF, INIVAU, CLSUFF, ZCHAMP, &
                    & .TRUE.,  IVALCO, ILGRSP, LLUNDF, ZUNDF)
  ELSE
    ILGRSP = 0
  ENDIF
  KLONGD=MAX (                                                        &
        &         2, ILGRGP + 2*(YLCADR%NNIVER+1) + YLCADR%NLATIT,    &
        &         2, ILGRSP + 2*(YLCADR%NNIVER+1)                     &
        &    )
ENDIF

KLONGD = KLONGD + 100

CALL FAIRNO_FORT (FA, KREP, INUMER2, 'KEEP')

YLCADR%IADDPK = KLONGD

FA%NSTROI = ISTROI

#endif

1001 CONTINUE

IF (LHOOK) CALL DR_HOOK('FASGRA_MT',1,ZHOOK_HANDLE)

END SUBROUTINE FASGRA_FORT

SUBROUTINE FASGRA64                                        &
&           (KREP, CDNOMC, KLONGD)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                  FA_COM_DEFAULT_INIT,  &
&                  NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
CHARACTER (LEN=*)      CDNOMC                                 ! IN   
INTEGER (KIND=JPLIKB)  KLONGD                                 !   OUT

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FASGRA_FORT                                               &
&           (FA, KREP, CDNOMC, KLONGD)

END SUBROUTINE

SUBROUTINE FASGRA                                          &
&           (KREP, CDNOMC, KLONGD)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                  FA_COM_DEFAULT_INIT,  &
&                  NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
CHARACTER (LEN=*)      CDNOMC                                 ! IN   
INTEGER (KIND=JPLIKM)  KLONGD                                 !   OUT

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FASGRA_MT                                                 &
&           (FA, KREP, CDNOMC, KLONGD)

END SUBROUTINE

SUBROUTINE FASGRA_MT                                           &
&           (FA, KREP, CDNOMC, KLONGD)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
CHARACTER (LEN=*)      CDNOMC                                 ! IN   
INTEGER (KIND=JPLIKM)  KLONGD                                 !   OUT
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  ILONGD                                 !   OUT
! Convert arguments


CALL FASGRA_FORT                                               &
&           (FA, IREP, CDNOMC, ILONGD)

KREP       = INT (      IREP, JPLIKM)
KLONGD     = INT (    ILONGD, JPLIKM)

END SUBROUTINE

