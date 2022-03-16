! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FACODEGA                                            &
&          (PFDATA, KLENF, KBITS, KNBIT, KB1PAR, KB2PAR,       &
&           PVERT, KLENV, KGRIB, KLENG, KWORD, KROUND, KCPACK, &
&           KSCALP, KERR, PMIN, PMAX, LDARPE)
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KLENF                                  ! IN   
REAL (KIND=JPDBLR)     PFDATA     (KLENF)                     ! IN   
INTEGER (KIND=JPLIKB)  KBITS                                  ! IN   
INTEGER (KIND=JPLIKB)  KNBIT                                  ! IN   
INTEGER (KIND=JPLIKB)  KB1PAR     (19)                        ! INOUT
INTEGER (KIND=JPLIKB)  KB2PAR     (17)                        ! INOUT
INTEGER (KIND=JPLIKB)  KLENV                                  ! IN   
REAL (KIND=JPDBLR)     PVERT      (KLENV)                     ! IN   
INTEGER (KIND=JPLIKB)  KLENG                                  ! IN   
INTEGER (KIND=JPLIKB)  KGRIB      (KLENG)                     !   OUT
INTEGER (KIND=JPLIKB)  KWORD                                  !   OUT
INTEGER (KIND=JPLIKB)  KROUND                                 ! IN   
INTEGER (KIND=JPLIKB)  KCPACK                                 ! IN   
INTEGER (KIND=JPLIKB)  KSCALP                                 ! IN   
INTEGER (KIND=JPLIKB)  KERR                                   !   OUT
REAL (KIND=JPDBLD)     PMIN                                   !   OUT
REAL (KIND=JPDBLD)     PMAX                                   !   OUT
LOGICAL                LDARPE                                 ! IN   
! Local integers
INTEGER (KIND=JPLIKM)  ILENF                                  ! IN   
INTEGER (KIND=JPLIKM)  IBITS                                  ! IN   
INTEGER (KIND=JPLIKM)  INBIT                                  ! IN   
INTEGER (KIND=JPLIKM)  IB1PAR     (19)                        ! INOUT
INTEGER (KIND=JPLIKM)  IB2PAR     (17)                        ! INOUT
INTEGER (KIND=JPLIKM)  ILENV                                  ! IN   
INTEGER (KIND=JPLIKM)  ILENG                                  ! IN   
INTEGER (KIND=JPLIKM)  IWORD                                  !   OUT
INTEGER (KIND=JPLIKM)  IROUND                                 ! IN   
INTEGER (KIND=JPLIKM)  ICPACK                                 ! IN   
INTEGER (KIND=JPLIKM)  ISCALP                                 ! IN   
INTEGER (KIND=JPLIKM)  IERR                                   !   OUT
! Local reals
REAL (KIND=JPDBLD)     ZFDATA     (KLENF)                     ! IN   
REAL (KIND=JPDBLD)     ZVERT      (KLENV)                     ! IN   
! Convert arguments

ILENF      = INT (     KLENF, JPLIKM)
IBITS      = INT (     KBITS, JPLIKM)
INBIT      = INT (     KNBIT, JPLIKM)
IB1PAR     = INT (    KB1PAR, JPLIKM)
IB2PAR     = INT (    KB2PAR, JPLIKM)
ILENV      = INT (     KLENV, JPLIKM)
ILENG      = INT (     KLENG, JPLIKM)
IROUND     = INT (    KROUND, JPLIKM)
ICPACK     = INT (    KCPACK, JPLIKM)
ISCALP     = INT (    KSCALP, JPLIKM)

IF (JPDBLR == JPDBLD) THEN

  CALL CODEGA                                                     &
  &           (PFDATA, ILENF, IBITS, INBIT, IB1PAR, IB2PAR,       &
  &            PVERT, ILENV, KGRIB, ILENG, IWORD, IROUND, ICPACK, &
  &            ISCALP, IERR, PMIN, PMAX, LDARPE)

ELSE

  ZFDATA = REAL (PFDATA, JPDBLD)
  ZVERT  = REAL ( PVERT, JPDBLD)

  CALL CODEGA                                                     &
  &           (ZFDATA, ILENF, IBITS, INBIT, IB1PAR, IB2PAR,       &
  &            ZVERT, ILENV, KGRIB, ILENG, IWORD, IROUND, ICPACK, &
  &            ISCALP, IERR, PMIN, PMAX, LDARPE)

ENDIF

KB1PAR     = INT (    IB1PAR, JPLIKB)
KB2PAR     = INT (    IB2PAR, JPLIKB)
KWORD      = INT (     IWORD, JPLIKB)
KERR       = INT (      IERR, JPLIKB)

END SUBROUTINE FACODEGA
