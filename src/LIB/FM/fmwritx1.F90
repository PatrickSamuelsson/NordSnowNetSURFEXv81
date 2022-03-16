!     ######spl
      SUBROUTINE FMWRITX1(HFILEM,HRECFM,HFIPRI,KLENG,PFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
      USE PARKIND1, ONLY : JPRB
      USE YOMHOOK , ONLY : LHOOK, DR_HOOK
!     #############################################################
!
!!****  *FMWRITX1* - routine to write a real 1D array into a "FM"-file
!!
!!    PURPOSE
!!    -------
!
!       The purpose of FMWRITX0 is to convert the real into integer(kind=8)
!     by calling FM_WRIT without interface module
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!      FM_WRIT
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
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=*)          ,INTENT(IN) ::HFILEM   ! file name
CHARACTER(LEN=*)          ,INTENT(IN) ::HRECFM   ! name of the article to be written

CHARACTER(LEN=*)          ,INTENT(IN) ::HFIPRI   ! file for prints in FM

INTEGER,                   INTENT(IN) ::KLENG  ! length of the data field
REAL, DIMENSION(:), &
                           INTENT(IN) ::PFIELD ! array containing the data field
INTEGER,                   INTENT(IN) ::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(IN) ::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(IN) ::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
!*      0.2   Declarations of local variables
!
INTEGER :: ILENG
REAL(KIND=8), DIMENSION(SIZE(PFIELD)) :: ZFIELD
!-------------------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FMWRITX1',0,ZHOOK_HANDLE)
ILENG=SIZE(PFIELD)
ZFIELD=PFIELD
CALL FM_WRIT(HFILEM,HRECFM,HFIPRI,ILENG,ZFIELD,KGRID,KLENCH,HCOMMENT,KRESP)
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('FMWRITX1',1,ZHOOK_HANDLE)
END SUBROUTINE FMWRITX1
