      SUBROUTINE CONFP_MF (PFVAL,KEXP,KMANT)
      USE PARKIND1, ONLY : JPRB
      USE YOMHOOK , ONLY : LHOOK, DR_HOOK
      USE LFI_PRECISION
!
!
!
!
!
!********************************************************************
!*
!*    NAME      : CONFP
!*
!*    FUNCTION  : CONVERT FLOATING POINT NUMBER FROM MACHINE
!*                REPRESENTATION TO GRIB REPRESENTATION.
!*
!*    INPUT     : PFVAL - FLOATING POINT NUMBER TO BE CONVERTED.
!*
!*    OUTPUT    : KEXP  - 8 BIT SIGNED EXPONENT
!*                KMANT - 24 BIT MANTISSA
!*                PFVAL - UNCHANGED.
!*
!*    JOHN HENNESSY , ECMWF , 15TH APRIL 1985
!*
!*    Modified by Jean CLOCHARD, February 1990, to comply with "DOCTOR",
!*    and to get a better precision on the truncation made:
!*       according to FM92 GRIB specification and recommandation,
!*    replacement of "1.0/ALOG(2.0)" by "1.+ZEPS", where ZEPS
!*    is a small positive value avoiding rounding errors to lead to
!*    a mantissa greater than (or equal to) 2**24.
!*
!*    (recommandation: ZEPS=1.E-12 for machines with word length of 60
!*                     bits or more, ZEPS=1.E-8 for 32 bits word length)
!*
!*       Tested with 10**5 pseudo-random values through the RANF
!*    function on Cyber 960 under NOS/VE 1.4.2, the modification gives
!*    an enhancement of more than 10, both on standard deviation and
!*    maximum error. Similar results are obtained when changing test
!*    interval from (0,1) to (-256,256).
!*
!*    Overflowing values are truncated, with a message on the listing.
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
      INTEGER (KIND=JPLIKM) :: IEXP
!
      REAL (KIND=JPDBLD) :: ZEPS, ZREF, ZC16
!
!RJ       INTRINSIC ABS, LOG, MAX, MIN
!
      SAVE ZEPS, ZC16
      DATA ZEPS / 1.E-12_JPDBLD /
      DATA ZC16 / 16.0_JPDBLD /
!
!
!
!           Elimination of sign.
!
!
      REAL(KIND=JPRB) :: ZHOOK_HANDLE
      IF (LHOOK) CALL DR_HOOK('CONFP_MF',0,ZHOOK_HANDLE)
      ZREF = ABS (PFVAL)
!
!
!
!     EXPONENT
!
      IF (ZREF.EQ.0.0_JPDBLD) THEN
        KEXP = 0
      ELSE
        IEXP = INT ( ( LOG(ZREF)/LOG(ZC16) + 65.0_JPDBLD ) +ZEPS )
        KEXP= MAX (0, MIN (127,IEXP))
      ENDIF
!
!
!
!
!
!
!
!     MANTISSA
!
  201 CONTINUE
!
      KMANT = NINT ( ZREF/(ZC16**(KEXP-70)) )
!
      IF (KMANT.GE.2**24) THEN
!
        IF (KEXP.LT.127) THEN
!
!     Some rounding error ocurred in the computation of KEXP, and could
!     not be compensated by ZEPS. Incrementation of KEXP, and new value
!     of KMANT computed.
!
          KEXP=KEXP+1
          GOTO 201
        ELSE
          PRINT *,                                                      &
     &'*/*/* OVERFLOW OF GRIB FLOATING-POINT REPRESENTATION WITH ',PFVAL
          KMANT=2**24-1
        ENDIF
!
      ENDIF
!
!     ADD SIGN BIT TO EXPONENT.
!
      IF (PFVAL.LT.0.0_JPDBLD) KEXP = KEXP + 128
!
!
!
!
      IF (LHOOK) CALL DR_HOOK('CONFP_MF',1,ZHOOK_HANDLE)
      ENDSUBROUTINE CONFP_MF
