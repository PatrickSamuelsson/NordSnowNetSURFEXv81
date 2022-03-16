      SUBROUTINE PACKGB ( PFDATA, KPACKD, PREFER, PSCALE, KLENG )
      USE PARKIND1, ONLY : JPIM, JPRB
      USE YOMHOOK , ONLY : LHOOK, DR_HOOK
      USE LFI_PRECISION
!
!********************************************************************
!*
!*    NAME      : PACKGB
!*
!*    FUNCTION  : COMPUTES INDIVIDUAL "PACKED" VALUES (FIELD FOR GRIB),
!*                THE RESULT CONSISTS OF ONE DATA READY TO PACK
!*                WITHIN A BIT STRING PER COMPUTER WORD.
!*                   This subroutine has been designed to avoid explicit
!*                mixed use of REAL and INTEGER type values within the
!*                dummy-argument array PFDATA of CODEGA, this explicit
!*                use leading to non-standard code. The following code
!*                enables use of the same actual argument for the 2
!*                dummy-argument arrays.
!*
!*    INPUT     : PFDATA - FLOATING-POINT VALUES TO BE PACKED.
!*                PREFER - REFERENCE VALUE OF THE FIELD: SHOULD BE THE
!*                         MINIMUM VALUE, OR AN "UNDER-APPROXIMATION"
!*                         OF THE MINIMUM VALUE).
!*                PSCALE - SCALING FACTOR.
!*                KLENG  - NUMBER OF VALUES TO TREAT.
!*
!*    OUTPUT    : KPACKD - (POSITIVE) INTEGER VALUES "READY TO PACK"
!*
!*    AUTHOR    : J.CLOCHARD, FRENCH WEATHER SERVICE, 01/03/90.
!*
!********************************************************************
!*
      IMPLICIT NONE
!
!     JP_STRIDE= pas permettant la correspondance entre les elements
!                d'un tableau de reels (KIND=JPDBLR): PFDATA(J) et
!                les elements d'un tableau d'entiers KPACKD(JP_STRIDE*J)
!                defini comme un tableau d'entiers representes sur
!                autant de bits que les reels.
!
      INTEGER(KIND=JPIM),PARAMETER :: JP_STRIDE=JPDBLD/JP_SIMPLE_ENTIER
!
! If integers are on 32 bits, don't be afraid by the number of bits of the
! real argument which is real and on 64 bits ... it's a trick : we pack
! PFDATA to KPACKD in the same area, but with half bit and we are kind
! enough to have a stride of 2 to access KPACKD ... ! (see also unpagb.F)
!
      INTEGER(KIND=JPIM) :: KLENG
!
      INTEGER(KIND=JPIM) :: KPACKD (JP_STRIDE*KLENG)
!
      REAL (KIND=JPDBLD) :: PREFER
      REAL (KIND=JPDBLD) :: PSCALE
!
      REAL (KIND=JPDBLD) :: PFDATA (KLENG)
!
      INTEGER(KIND=JPIM) :: J, II
      REAL(KIND=JPRB) :: ZHOOK_HANDLE
!**
!     1.  -  STRAIGHT-FORWARD COMPUTING.
!
      IF (LHOOK) CALL DR_HOOK('PACKGB',0,ZHOOK_HANDLE)
!$OMP PARALLEL DO PRIVATE(J,II) SCHEDULE(STATIC,4096)
      DO 101 J=1,KLENG
#if defined(LITTLE)
        II=JP_STRIDE*J -1
#else
        II=JP_STRIDE*J
#endif
        KPACKD(II)=NINT ( ( PFDATA(J) - PREFER ) * PSCALE )
  101 CONTINUE
!$OMP END PARALLEL DO
!
      IF (LHOOK) CALL DR_HOOK('PACKGB',1,ZHOOK_HANDLE)
      ENDSUBROUTINE PACKGB
