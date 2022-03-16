      SUBROUTINE OFFSET_MF  (KOFF,KVAL,KWORD,KBYTE,KNBIT,KLEN,KERR)
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
!*    NAME      : OFFSET
!*
!*    FUNCTION  : CALCULATES THE WORD AND BIT OFFSET OF THE START
!*                OF THE NEXT BIT-FIELD IN AN ARRAY OF BINARY
!*                DATA, FOLLOWING THE INSERTION/EXTRACTION OF ONE
!*                OR MORE CONSECUTIVE BIT-FIELDS.
!*
!*    INPUT     : KOFF  - BIT OFFSET AT WHICH LAST INSERTION OR
!*                        EXTRACTION STARTED.
!*                KVAL - NUMBER OF FIELDS INSERTED OR EXTRACTED IN
!*                        LAST OPERATION.
!*                KWORD - WORD NUMBER OF ARRAY AT WHICH LAST OPERATION
!*                        STARTED.
!*                KBYTE - LENGTH, IN BITS, OF LAST FIELD(S) INSERTED
!*                        OR EXTRACTED.
!*                KNBIT - NUMBER OF BITS IN COMPUTER WORD.
!*                KLEN  - NUMBER OF WORDS IN ARRAY.
!*
!*    OUTPUT    : KOFF  - BIT OFFSET AT WHICH NEXT INSERTION OR
!*                        EXTRACTION STARTS.
!*                KVAL - UNCHANGED.
!*                KWORD - WORD NUMBER OF ARRAY AT WHICH NEXT OPERATION
!*                        STARTS.
!*                KBYTE - UNCHANGED.
!*                KNBIT - UNCHANGED.
!*                KLEN  - UNCHANGED.
!*
!*                KERR  - EQUAL 0 IF NO ERROR.
!*                        EQUAL -2 IF NEXT OPERATION IS OUTSIDE ARRAY
!*                        BOUNDS.
!*
!*    GENERAL   : OFFSET CALLS -----
!*
!*    AUTHOR    : J.HENNESSY  15.4.85
!*
!*    MODIFIED  : J.HENNESSY  28.11.85
!*    Modified by Jean CLOCHARD, February 1990, to comply with "DOCTOR".
!*
!********************************************************************
!
      IMPLICIT NONE
!
      INTEGER (KIND=JPLIKM) :: KOFF
      INTEGER (KIND=JPLIKM) :: KVAL
      INTEGER (KIND=JPLIKM) :: KWORD
      INTEGER (KIND=JPLIKM) :: KBYTE
      INTEGER (KIND=JPLIKM) :: KNBIT
      INTEGER (KIND=JPLIKM) :: KLEN
      INTEGER (KIND=JPLIKM) :: KERR
!
      INTEGER (KIND=JPLIKM) :: IBITL, INTER, IOFFS
!
!
!     CLEAR ERROR INDICATOR
!
      REAL(KIND=JPRB) :: ZHOOK_HANDLE
      IF (LHOOK) CALL DR_HOOK('OFFSET_MF',0,ZHOOK_HANDLE)
      KERR = 0
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
!     CALCULATE NEXT WORD AND BIT POSITIONS.
!
      IBITL = KVAL * KBYTE
      INTER = IBITL / KNBIT
      IOFFS = IBITL - INTER * KNBIT
      KWORD = KWORD + INTER
      KOFF  = KOFF  + IOFFS
!
      IF (KOFF.GE.KNBIT)                                                &
     &   THEN
             KWORD = KWORD + 1
             KOFF  = KOFF  - KNBIT
         END IF
!
!
!
!
!
!
!
!
!     CHECK THAT NEXT WORD TO BE ACCESSED LIES WITHIN THE ARRAY BOUNDS.
!
      IF (KWORD.GT.KLEN)                                                &
     &   THEN
             KERR = -2
             WRITE (*,9001) KWORD,KLEN
 9001        FORMAT (1H ,'WORD ',I8,' IS OUTSIDE ARRAY BOUNDS ',I8)
         ENDIF
!
!
      IF (LHOOK) CALL DR_HOOK('OFFSET_MF',1,ZHOOK_HANDLE)
      ENDSUBROUTINE OFFSET_MF

