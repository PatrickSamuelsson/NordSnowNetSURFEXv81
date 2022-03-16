!     ######spl
      SUBROUTINE FMWRITN2(HFILEM,HRECFM,HFIPRI,KLENG,KFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
      USE PARKIND1, ONLY : JPRB
      USE YOMHOOK , ONLY : LHOOK, DR_HOOK
!     #############################################################
!
!!****  *FMWRITN2* - routine to write a integer 2D array into a "FM"-file
!!
!!    PURPOSE
!!    -------
!
!       The purpose of FMWRITN1 is to convert the integer into integer(kind=8)
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
!!      Modification 15/10/97 (V.Masson)    1D and 2D cases
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
INTEGER, DIMENSION(:,:), &
                           INTENT(IN) ::KFIELD ! array containing the data field
INTEGER,                   INTENT(IN) ::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(IN) ::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(IN) ::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
!*      0.2   Declarations of local variables
!
INTEGER(KIND=8), DIMENSION(SIZE(KFIELD,1),SIZE(KFIELD,2)) :: IFIELD
INTEGER                                                   :: ILENG
!-------------------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FMWRITN2',0,ZHOOK_HANDLE)
IFIELD(:,:)=KFIELD(:,:)
!
ILENG=SIZE(KFIELD)
CALL FM_WRIT(HFILEM,HRECFM,HFIPRI,ILENG,IFIELD,KGRID,KLENCH,HCOMMENT,KRESP)
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('FMWRITN2',1,ZHOOK_HANDLE)
END SUBROUTINE FMWRITN2
