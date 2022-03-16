      SUBROUTINE PRTBIN_MF (KIN,KNBIT,KOUT,KERR)
      USE PARKIND1, ONLY : JPRB
      USE YOMHOOK , ONLY : LHOOK, DR_HOOK
      USE LFI_PRECISION
!
!**** PRTBIN - Binary to decimal conversion.
!
!     Purpose.
!     --------
!
!           Produces a decimal number with ones and zeroes
!           corresponding to the ones and zeroes of the input
!           binary number.
!           eg input number 1011 binary, output number 1011 decimal.
!
!**   Interface.
!     ----------
!
!           CALL PRTBIN_MF (KIN,KNBIT,KOUT,KERR)
!
!           Integer    K.
!           Real       P.
!           Logical    O.
!           Character  H.
!
!               Input Parameters.
!               -----------------
!
!               KIN   - Integer variable containing binary number.
!
!               KNBIT - Number of bits in binary number.
!
!               Output Parameters.
!               -----------------
!
!               KOUT  - Integer variable containing decimal value
!                       with ones and zeroes corresponding to those of
!                       the input binary number.
!
!               KERR  - 0, If no error.
!                       1, Number of bits in binary number exceeds
!                          maximum allowed.
!
!     Method.
!     -------
!
!           Masking expression used is not ANSI standard.
!
!     Externals.
!     ----------
!
!           None.
!
!     Reference.
!     ----------
!
!           None.
!
!     Comments.
!     ---------
!
!           Routine contains sections 0, 1 and section 9.
!
!     Author.
!     -------
!
!           John Hennessy     ECMWF  October 1985
!
!     Modifications.
!     --------------
!
!           John Hennessy     ECMWF  March 1991
!           Made to conform to current programming standards.
!
!     ---------------------------------------------------------------
!
!
      IMPLICIT NONE
!
      INTEGER (KIND=JPLIKM) :: KIN
      INTEGER (KIND=JPLIKM) :: KNBIT
      INTEGER (KIND=JPLIKM) :: KOUT
      INTEGER (KIND=JPLIKM) :: KERR
!
      INTEGER (KIND=JPLIKM) :: J101, IK, IMASC, ITEMP
!
!
!
!
!
!
!
!
!*    Section 0. Definition of variables. Check on parameters.
!     -----------------------------------------------------------------
!
!     Check length of binary number.
!
      REAL(KIND=JPRB) :: ZHOOK_HANDLE
      IF (LHOOK) CALL DR_HOOK('PRTBIN_MF',0,ZHOOK_HANDLE)
      IF (KNBIT.LT.0.OR.KNBIT.GT.15)                                    &
     &   THEN
             KERR = 1
             WRITE (*,9000) KNBIT
             GO TO 900
         ELSE
             KERR = 0
         ENDIF
!
!     -----------------------------------------------------------------
!
!
!
!
!
!
!
!
!
!
!*    Section 1. Generate required number.
!     -----------------------------------------------------------------
!
  100 CONTINUE
!
      KOUT = 0
!
      DO 101 J101=1,KNBIT
         IK    = J101 - 1
         IMASC = 2**IK
         ITEMP = IAND(KIN,IMASC)
         IF (ITEMP.NE.0) KOUT = KOUT + 10**IK
  101 CONTINUE
!
!     -----------------------------------------------------------------
!
!
!
!
!
!
!
!
!
!
!*    Section 9. Format statements. Return to calling routine.
!     -----------------------------------------------------------------
!
  900 CONTINUE
!
 9000 FORMAT (1H ,'PRTBIN : Binary number too long - ',I3,' bits.')
!
      IF (LHOOK) CALL DR_HOOK('PRTBIN_MF',1,ZHOOK_HANDLE)
      ENDSUBROUTINE PRTBIN_MF
