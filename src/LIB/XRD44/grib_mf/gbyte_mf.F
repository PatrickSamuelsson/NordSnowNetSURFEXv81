      SUBROUTINE GBYTE_MF(KSOURC,KDEST,KOFSET,KBYTSZ)
      USE PARKIND1, ONLY : JPRB
      USE YOMHOOK , ONLY : LHOOK, DR_HOOK
      USE LFI_PRECISION
!*****
!*
!*    FUNCTION: GET A SINGLE BIT FIELD FROM KSOURC INTO KDEST
!*
!*    INPUT   : KSOURC(1)= WORD CONTAINING START OF BIT FIELD
!*              KDEST    = TARGET WORD
!*              KOFSET   = OFFSET IN BITS FOR START OF THE FIELD
!*              KBYTSZ   = LENGTH OF FIELD IN BITS
!*
!*    OUTPUT  : KSOURC,KOFSET,KBYTSZ UNCHANGED
!*              KDEST CONTAINS FIELD RIGHT JUSTIFIED
!*
!*    AUTHOR  : M.MIQUEU   08/1981 (REWRITTEN FROM J.MARTELLET'S)
!*
!*****
!
      IMPLICIT NONE
!
      INTEGER (KIND=JPLIKM) :: KOFSET
      INTEGER (KIND=JPLIKM) :: KBYTSZ
      INTEGER (KIND=JPLIKB) :: KDEST
!
      INTEGER (KIND=JPLIKB) :: KSOURC (2)
!
      INTEGER (KIND=JPLIKM) :: INBPW, ISH1
!
      REAL(KIND=JPRB) :: ZHOOK_HANDLE
      IF (LHOOK) CALL DR_HOOK('GBYTE_MF',0,ZHOOK_HANDLE)
      INBPW=64
      ISH1=KOFSET+KBYTSZ-INBPW
!
      IF(ISH1.LE.0) THEN
!
!
!     BYTES DO NOT SPAN WORDS
!
!
        ISH1=-ISH1
!
!
        KDEST=IBITS (KSOURC(1),ISH1,KBYTSZ)
!
      ELSE
!
!     BYTE SPANS WORDS
!
!
        KDEST=IOR (                                                     &
     &              ISHFT (                                             &
     &                      IBITS (KSOURC(1),0,INBPW-KOFSET)            &
     &                     , ISH1 )                                     &
     &             ,                                                    &
     &              IBITS (KSOURC(2),INBPW-ISH1,ISH1)                   &
     &            )
!
!
!
      ENDIF
!
      IF (LHOOK) CALL DR_HOOK('GBYTE_MF',1,ZHOOK_HANDLE)
      ENDSUBROUTINE GBYTE_MF
