      FUNCTION ISRCHFLE(N,ARRAY,INC,TARGET)
      USE PARKIND1, ONLY : JPIM, JPRB
      IMPLICIT NONE
      INTEGER(KIND=JPIM) :: ISRCHFLE
      INTEGER(KIND=JPIM) :: N
      INTEGER(KIND=JPIM) :: INC
      REAL(KIND=JPRB) :: TARGET
      REAL(KIND=JPRB) :: ARRAY(1+INC*(N-1))
      INTEGER(KIND=JPIM) :: I
!
      IF (N.LE.0) THEN
        ISRCHFLE=0
        RETURN
      ELSE
        DO 1 I=1,N
          IF ( ARRAY(1+INC*(I-1)).LE.TARGET ) THEN
            ISRCHFLE=I
            RETURN
          ENDIF
 1      CONTINUE
        ISRCHFLE=N+1
      ENDIF
      ENDFUNCTION ISRCHFLE
