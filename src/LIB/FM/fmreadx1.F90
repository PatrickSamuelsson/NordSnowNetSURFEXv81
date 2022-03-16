!     ######spl
      SUBROUTINE FMREADX1(HFILEM,HRECFM,HFIPRI,KLENG,PFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
      USE PARKIND1, ONLY : JPRB
      USE YOMHOOK , ONLY : LHOOK, DR_HOOK
!     #############################################################
!
!!****  *FMREADX1* - routine to read a real 1D array into a "FM"-file
!!
!!    PURPOSE
!!    -------
!
!       The purpose of FMREADX0 is to convert the real into integer(kind=8)
!     by calling FM_READ without interface module
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!      FM_READ
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      V. MASSON      *METEO-FRANCE*
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      original                                                     06/08/97
!----------------------------------------------------------------------------
!
!*      0.    DECLARATIONS
!             ------------
!
USE MODD_FMDECLAR
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=*)          ,INTENT(IN) ::HFILEM   ! file name
CHARACTER(LEN=*)          ,INTENT(IN) ::HRECFM   ! name of the article to be written

CHARACTER(LEN=*)          ,INTENT(IN) ::HFIPRI   ! file for prints in FM

INTEGER,                   INTENT(IN) ::KLENG  ! length of the data field
REAL, DIMENSION(:), &
                           INTENT(OUT)::PFIELD ! array containing the data field
INTEGER,                   INTENT(OUT)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(OUT)::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(OUT)::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
!*      0.2   Declarations of local variables
!
CHARACTER(LEN=JPXKRK) ::YCOMMENT 
INTEGER :: ILENG
REAL(KIND=8),DIMENSION(SIZE(PFIELD)) :: ZFIELD
!-------------------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FMREADX1',0,ZHOOK_HANDLE)
ILENG=SIZE(PFIELD)
CALL FM_READ(HFILEM,HRECFM,HFIPRI,ILENG,ZFIELD,KGRID,KLENCH,YCOMMENT,KRESP)
IF(KRESP==0) PFIELD = ZFIELD
IF(KRESP==0) HCOMMENT = YCOMMENT
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('FMREADX1',1,ZHOOK_HANDLE)
END SUBROUTINE FMREADX1
