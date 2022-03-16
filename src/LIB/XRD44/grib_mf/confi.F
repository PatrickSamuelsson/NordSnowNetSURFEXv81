      SUBROUTINE CONFI (PFVAL,KEXP,KMANT,PNFVAL)
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
!*    NAME      : CONFI
!*
!*    FUNCTION  : CONVERT FLOATING POINT NUMBER FROM MACHINE
!*                REPRESENTATION TO GRIB REPRESENTATION,
!*                THE RESULT NUMBER NOT EXCEEDING THE INPUT NUMBER.
!*    INPUT     : PFVAL  - FLOATING POINT NUMBER TO BE CONVERTED.
!*
!*    OUTPUT    : KEXP   - 8 BIT SIGNED EXPONENT
!*                KMANT  - 24 BIT MANTISSA
!*                PFVAL  - UNCHANGED
!*                PNFVAL - "EXACT" VALUE REPRESENTED BY (KEXP,KMANT).
!*
!*    Jean CLOCHARD , French DMN, January 1990.
!*    Nearly rewritten from CONFP subroutine from John HENNESSY, ECMWF.
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
      REAL (KIND=JPDBLD) :: PNFVAL
!
      INTEGER (KIND=JPLIKM) :: IAUXIL, IEXP
!
      REAL (KIND=JPDBLD) :: ZEPS, ZREF, ZC16, ZMANT
!
      LOGICAL :: LLPOSI
!
!RJ       INTRINSIC LOG, ABS, MIN, MAX, INT
!
      SAVE ZEPS, ZC16
      DATA ZEPS / 1.E-12_JPDBLD /
      DATA ZC16 / 16.0_JPDBLD /
!
!
!
!           Elimination of sign.
!
      REAL(KIND=JPRB) :: ZHOOK_HANDLE
      IF (LHOOK) CALL DR_HOOK('CONFI',0,ZHOOK_HANDLE)
      LLPOSI=PFVAL.GE.0.0_JPDBLD
      ZREF = ABS (PFVAL)
!
!
!
!
!     EXPONENT
!
      IF (ZREF.EQ.0.0_JPDBLD) THEN
        KEXP = 0
      ELSE
        IEXP = INT ((LOG(ZREF)/LOG(16.0_JPDBLD) + 65.0_JPDBLD) +ZEPS)
        KEXP= MAX (0, MIN (127,IEXP))
      ENDIF
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
      ZMANT = ZREF/(ZC16**(KEXP-70))
!
      IF (LLPOSI) THEN
        KMANT=INT (ZMANT)
      ELSE
!
!        Special case for negative values... because the "INT" function
!     is not equivalent to the "integer part" mathematical function
!     for this range of values.
!
        IAUXIL=2+INT (ZMANT)
        KMANT=IAUXIL - INT ( -ZMANT + REAL (IAUXIL,JPDBLD) )
      ENDIF
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
!     COMPUTE "EXACT" VALUE REPRESENTED, AND ADD SIGN BIT TO EXPONENT.
!
      IF (LLPOSI) THEN
        PNFVAL =  KMANT*(ZC16**(KEXP-70))
      ELSE
        PNFVAL = -KMANT*(ZC16**(KEXP-70))
        KEXP=KEXP+128
      ENDIF
!
!
      IF (LHOOK) CALL DR_HOOK('CONFI',1,ZHOOK_HANDLE)
      ENDSUBROUTINE CONFI
