!     ######spl
      SUBROUTINE FMWRITL0(HFILEM,HRECFM,HFIPRI,KLENG,OFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
      USE PARKIND1, ONLY : JPRB
      USE YOMHOOK , ONLY : LHOOK, DR_HOOK
!     #############################################################
!
!!****  *FMWRITL0* - routine to write a logical scalar into a "FM"-file
!!
!!    PURPOSE
!!    -------
!
!       The purpose of FMWRITN0 is to convert the integer into integer(kind=8)
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
LOGICAL, &
                           INTENT(IN) ::OFIELD ! array containing the data field
INTEGER,                   INTENT(IN) ::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(IN) ::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(IN) ::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
!*      0.2   Declarations of local variables
!
INTEGER(KIND=8) :: IFIELD
!-------------------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FMWRITL0',0,ZHOOK_HANDLE)
IF (OFIELD) THEN
  IFIELD=1
ELSE
  IFIELD=0
END IF
!
CALL FM_WRIT(HFILEM,HRECFM,HFIPRI,1,IFIELD,KGRID,KLENCH,HCOMMENT,KRESP)
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('FMWRITL0',1,ZHOOK_HANDLE)
END SUBROUTINE FMWRITL0
