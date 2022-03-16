      SUBROUTINE DECFP_MF (PFVAL,KEXP,KMANT)
      USE PARKIND1, ONLY : JPRB
      USE YOMHOOK , ONLY : LHOOK, DR_HOOK
      USE LFI_PRECISION
!
!********************************************************************
!*
!*    NAME      : DECFP
!*
!*    FUNCTION  : DECODE GRIB CODE REPRESENTATION TO
!*                FLOATING POINT NUMBER.
!*
!*    INPUT     : KEXP  - 8 BIT SIGNED EXPONENT
!*                KMANT - 24 BIT MANTISSA
!*
!*    OUTPUT    : PFVAL - FLOATING POINT NUMBER
!*                KEXP  - UNCHANGED
!*                KMANT - UNCHANGED
!*
!*    JOHN HENNESSY , ECMWF , 15TH APRIL 1985
!*
!*    Modified by Jean CLOCHARD, February 1990, to comply with "DOCTOR",
!*    and to get a more straight-forward code.
!*
!********************************************************************
!
      IMPLICIT NONE
!
      INTEGER (KIND=JPLIKM) :: KEXP
      INTEGER (KIND=JPLIKM) :: KMANT
!
      REAL (KIND=JPDBLD) :: PFVAL
!
      REAL (KIND=JPDBLD) :: ZC16
!
      SAVE ZC16
      DATA ZC16 / 16.0_JPDBLD /
!
!      Direct decoding, depending on sign bit, included in KEXP.
!
!
      REAL(KIND=JPRB) :: ZHOOK_HANDLE
      IF (LHOOK) CALL DR_HOOK('DECFP_MF',0,ZHOOK_HANDLE)
      IF (KEXP.LT.128) THEN
        PFVAL = KMANT*(ZC16**(KEXP-70))
      ELSE
        PFVAL = -KMANT*(ZC16**(KEXP-198))
      ENDIF
!
!
      IF (LHOOK) CALL DR_HOOK('DECFP_MF',1,ZHOOK_HANDLE)
      ENDSUBROUTINE DECFP_MF
