!     ######spl
      SUBROUTINE FMWRITC0(HFILEM,HRECFM,HFIPRI,KLENG,HFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
      USE PARKIND1, ONLY : JPRB
      USE YOMHOOK , ONLY : LHOOK, DR_HOOK
!     #############################################################
!
!!****  *FMWRITC0* - routine to write a string scalar into a "FM"-file
!!
!!    PURPOSE
!!    -------
!
!       The purpose of FMWRITL0 is to convert the string into arrayr of
!      integer(kind=8) and to call FM_WRIT without interface module
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
CHARACTER(LEN=*), &
                           INTENT(IN) ::HFIELD ! array containing the data field
INTEGER,                   INTENT(IN) ::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(IN) ::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(IN) ::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
!*      0.2   Declarations of local variables
!
INTEGER                                               :: JLOOP
INTEGER(KIND=8), DIMENSION(:), ALLOCATABLE            :: IFIELD
INTEGER                                               :: ILENG
!-------------------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FMWRITC0',0,ZHOOK_HANDLE)
ILENG=LEN(HFIELD)
ALLOCATE(IFIELD(ILENG))
DO JLOOP=1,ILENG
 IFIELD(JLOOP)=IACHAR(HFIELD(JLOOP:JLOOP))
END DO
!
CALL FM_WRIT(HFILEM,HRECFM,HFIPRI,ILENG,IFIELD,KGRID,KLENCH,HCOMMENT,KRESP)
!
DEALLOCATE(IFIELD)
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('FMWRITC0',1,ZHOOK_HANDLE)
END SUBROUTINE FMWRITC0
