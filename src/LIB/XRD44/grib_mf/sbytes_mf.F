      SUBROUTINE SBYTES_MF(KD,KS,KSKIP1,KBSIZ,KSKIP2,KBYTES)
      USE PARKIND1, ONLY : JPRB
      USE YOMHOOK , ONLY : LHOOK, DR_HOOK
      USE LFI_PRECISION
!
! REVERSES THE ACTION OF GBYTES, TAKING FIELDS FROM KS AND
! INSERTING THEM INTO A BIT STRING IN KD. SEE GBYTES.
! AUTHOR D. ROBERTSON  AUG,1981
!*
!*    Modified by Mats HAMRUD, ECMWF, 1988, to have a constant number
!*    number af arguments , as within the GRIB package calls,
!*    and to make some cleanings.
!*
!*    Modifications by Jean CLOCHARD, French DMN, January 1990,
!*    essentially to get a vectorising code on CRAY (no recurrences),
!*    and to make some cleanings.
!
      IMPLICIT NONE
!
      INTEGER (KIND=JPLIKM) :: KSKIP1
      INTEGER (KIND=JPLIKM) :: KBSIZ
      INTEGER (KIND=JPLIKM) :: KSKIP2
      INTEGER (KIND=JPLIKM) :: KBYTES
!
      INTEGER (KIND=JPLIKB) :: KD(*)
      INTEGER (KIND=JPLIKB) :: KS(KBYTES)
!
      INTEGER (KIND=JPLIKM) :: INBPW, ISTEP, JBYTE, IAUXIL
      INTEGER (KIND=JPLIKM) :: ID, ISKIP, ISH1, ISH2, ISH3
!
!     LOGICAL LLSTOP
!
      REAL(KIND=JPRB) :: ZHOOK_HANDLE
      IF (LHOOK) CALL DR_HOOK('SBYTES_MF',0,ZHOOK_HANDLE)
      INBPW=64
      ISTEP = KSKIP2+KBSIZ
!
      DO 75 JBYTE = 1 , KBYTES
!
! WITH THE STARTING WORD AND BIT POSITION KNOWN, THE
! DESIRED INSERTION CAN BE DONE BY
!**      CALL SBYTE(KD(ID),KS(JBYTE),ISKIP,KBSIZ)
! BUT THE CODE IS SHORT ENOUGH TO GO IN-LINE.
!
      IAUXIL=KSKIP1+(JBYTE-1)*ISTEP
      ID=1+IAUXIL/INBPW
      ISKIP=IAUXIL-(ID-1)*INBPW
      ISH1=ISKIP+KBSIZ-INBPW
!     LLSTOP=ISH1.GT.0
!
      IF(ISH1.LE.0) THEN
!*
! BYTE GOES INTO 1 WORD OF KD.
!
!        PRINT '(''a'',2(TR1,B64))', KD(ID),KS(JBYTE)
         KD(ID) = ISHFTC(IOR(ISHFT(ISHFTC(KD(ID),ISKIP,BIT_SIZE(KD(ID)))&
     &            ,KBSIZ),IBITS (KS(JBYTE),0,KBSIZ)),-ISH1,             &
     &            BIT_SIZE(IOR(ISHFT( ISHFTC(KD(ID),ISKIP,              &
     &            BIT_SIZE(KD(ID))),KBSIZ),IBITS (KS(JBYTE),0,KBSIZ))))
!        PRINT '(''a'',TR1,B64))', KD(ID)
      ELSE
!*
! BYTE GOES INTO 2 WORDS OF KD.
!
!        PRINT *, JBYTE, INBPW, KSKIP1, KBSIZ, ISTEP, ID, ISKIP, ISH1
!        PRINT '(''b1 KD(ID  ) ='',B64.64))', KD(ID)
!        PRINT '(''b1 KD(ID+1) ='',B64.64))', KD(ID+1)
!        PRINT '(''b1 KS(JBYTE)='',B64.64))', KS(JBYTE)
         KD(ID)=IOR(ISHFTC(ISHFT(KD(ID),ISKIP-INBPW), INBPW-ISKIP,      &
     &          BIT_SIZE(ISHFT(KD(ID),ISKIP-INBPW))),                   &
     &          ISHFT(IBITS(KS(JBYTE),0,KBSIZ),-ISH1))
         KD(ID+1)=ISHFTC(IOR(ISHFT(KD(ID+1),ISH1),                      &
     &            IBITS(KS(JBYTE),0,ISH1)),-ISH1,                       &
     &            BIT_SIZE(IOR(ISHFT(KD(ID+1),ISH1),                    &
     &            IBITS(KS(JBYTE),0,ISH1))))
!        PRINT '(''b2 KD(ID  ) ='',B64.64))', KD(ID)
!        PRINT '(''b2 KD(ID+1) ='',B64.64))', KD(ID+1)
!        IF (JBYTE.GT.10) STOP 'provisoire b'
      ENDIF
!
   75 CONTINUE
!
!     IF (LLSTOP) STOP 'provisoire'
!
      IF (LHOOK) CALL DR_HOOK('SBYTES_MF',1,ZHOOK_HANDLE)
      ENDSUBROUTINE SBYTES_MF
