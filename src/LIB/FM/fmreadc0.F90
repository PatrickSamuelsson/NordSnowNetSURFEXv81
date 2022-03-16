!     ######spl
      SUBROUTINE FMREADC0(HFILEM,HRECFM,HFIPRI,KLENG,HFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
      USE PARKIND1, ONLY : JPRB
      USE YOMHOOK , ONLY : LHOOK, DR_HOOK
!     #############################################################
!
!!****  *FMREADL1* - routine to read a logical scalar into a "FM"-file
!!
!!    PURPOSE
!!    -------
!
!       The purpose of FMREADL0 is to convert the string into arrayr of
!      integer(kind=8) and to call FM_READ without interface module
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
CHARACTER(LEN=*), &
                           INTENT(OUT)::HFIELD ! array containing the data field
INTEGER,                   INTENT(OUT)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(OUT)::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(OUT)::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
!*      0.2   Declarations of local variables
!
INTEGER                                      :: JLOOP
CHARACTER(LEN=JPXKRK)                        ::YCOMMENT 
INTEGER(KIND=8), DIMENSION(:), ALLOCATABLE   :: IFIELD
INTEGER                                      :: ILENG
!-------------------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FMREADC0',0,ZHOOK_HANDLE)
ILENG=LEN(HFIELD)
ALLOCATE(IFIELD(ILENG))
!
CALL FM_READ(HFILEM,HRECFM,HFIPRI,ILENG,IFIELD,KGRID,KLENCH,YCOMMENT,KRESP)
!
IF(KRESP==0) THEN
  DO JLOOP=1,ILENG
   HFIELD(JLOOP:JLOOP)=ACHAR(IFIELD(JLOOP))
  END DO
  HCOMMENT = YCOMMENT
END IF
!
DEALLOCATE(IFIELD)
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('FMREADC0',1,ZHOOK_HANDLE)
END SUBROUTINE FMREADC0
