      SUBROUTINE DECOGA (PFDATA,KLENF,KBITS,KNBIT,KB1PAR,               &
     &                   KB2PAR,PVERT,KLENV,KGRIB,KLENG,KWORD,          &
     &                   KJLENV,KJLENF,KCPACK,KSCALP,KERR,               &
     &                   PMIN,PMAX,LDARPE)
      USE PARKIND1, ONLY : JPRB
      USE YOMHOOK , ONLY : LHOOK, DR_HOOK
      USE LFI_PRECISION

!
!
!
!
!
!
!********************************************************************
!*
!*     NAME     : DECOGA
!*
!*     FUNCTION : DECODE WMO GRIB CODED DATA.
!*
!*     INPUT    : KNBIT   - NUMBER OF BITS IN COMPUTER WORD
!*
!*                KGRIB   - INTEGER ARRAY CONTAINING DATA IN GRIB CODE.
!*                KLENG   - LENGTH OF ARRAY KGRIB.
!*
!*                PVERT   - REAL ARRAY TO RECEIVE VERTICAL COORDINATE
!*                          PARAMETERS.
!*                KLENV   - LENGTH OF ARRAY PVERT.
!*
!*                PFDATA  - REAL ARRAY TO RECEIVE DECODED DATA VALUES.
!*                KLENF   - LENGTH OF ARRAY PFDATA.
!*
!*                          *****  IF THIS IS 1 ONLY
!*                          THE PRODUCT AND GRID DEFINITION BLOCKS
!*                          ARE DECODED. *****
!*
!*                KB1PAR  - INTEGER ARRAY (DIMENSION 19) TO RECEIVE
!*                          PRODUCT DEFINITION INFORMATION.
!*
!*                KB2PAR  - INTEGER ARRAY (DIMENSION 11) TO RECEIVE GRID
!*                          DEFINITION INFORMATION.
!*
!*                PMIN    - MINIMUM VALUE OF THE FIELD DATA.
!*
!*                PMAX    - MAXIMUM VALUE OF THE FIELD DATA.
!*
!*                  *****   These 2 last values have only to be supplied
!*                          if next argument is .TRUE. .   *****
!*
!*                LDARPE  - .TRUE., modifications for ARPEGE coding
!*                                  have been included when coding data;
!*                          .FALSE., no such modifications.
!*
!*     OUTPUT   : PARAMETERS FROM BLOCK 1 , PRODUCT DEFINITION BLOCK.
!*                --------------------------------------------------
!*                KB1PAR  - INTEGER ARRAY (DIMENSION 19) OF PRODUCT
!*                          DEFINITION INFORMATION.
!*
!*                WORD      CONTENTS
!*                ----      --------
!*                  1       ORIGINATING CENTRE IDENTIFIER.
!*                  2       MODEL IDENTIFICATION.
!*                  3       GRID DEFINITION.
!*                  4       FLAG ( CODE TABLE 1)
!*                  5       PARAMETER IDENTIFIER ( CODE TABLE 2 ).
!*                  6       TYPE OF LEVEL (CODE TABLE 3).
!*                 7-8      VALUE(S) OF LEVELS (CODE TABLE 3).
!*                  9       YEAR OF DATA
!*                 10       MONTH OF DATA
!*                 11       DAY OF DATA
!*                 12       HOUR OF DATA
!*                 13       MINUTE OF DATA
!*                 14       TIME UNIT (CODE TABLE 4).
!*                 15       TIME RANGE ONE
!*                 16       TIME RANGE TWO
!*                 17       TIME RANGE FLAG (CODE TABLE 5).
!*                 18       NUMBER AVERAGED OR ACCUMULATED.
!*                 19       NUMBER MISSING FROM AVERAGES/ACCUMULATIONS.
!*
!*                          VALUE(S) OF LEVEL CAN OCCUPY 2 WORDS..
!*                          FOR A LAYER THE FIRST WORD DEFINES THE TOP
!*                          AND THE SECOND THE BOTTOM OF THE LAYER.
!*                          FOR A SINGLE LEVEL, ONLY THE FIRST WORD IS
!*                          USED.
!*
!*                PARAMETERS FROM BLOCK 2 , GRID DEFINITION BLOCK.
!*                -----------------------------------------------
!*                KB2PAR - INTEGER ARRAY (DIMENSION 17) CONTAINING GRID
!*                         DEFINITION INFORMATION.
!*                         USE VARIES WITH DATA REPRESENTATION TYPE.
!*
!*                WORD      LAT/LONG GRID
!*                ----      -------------
!*                 1        DATA REPRESENTATION TYPE (CODE TABLE 6).
!*                 2        NO. OF POINTS ALONG A LATITUDE
!*                 3        NO. OF POINTS ALONG A MERIDIAN.
!*                 4        LATITUDE OF ORIGIN (SOUTH - IVE).
!*                 5        LONGITUDE OF ORIGIN (WEST - IVE).
!*                 6        RESOLUTION FLAG. (CODE TABLE 7)
!*                 7        LATITUDE OF EXTREME POINT (SOUTH - IVE).
!*                 8        LONGITUDE OF EXTREME POINT (WEST - IVE).
!*                 9        LATITUDE INCREMENT
!*                10        LONGITUDE INCREMENT
!*                11        SCANNING MODE FLAGS (CODE TABLE 8)
!*
!*                WORD      GAUSSIAN GRID
!*                ----      -------------
!*                1-9       AS FOR LAT/LONGITUDE GRID.
!*                10        THE NUMBER OF LATITUDE LINES BETWEEN A POLE
!*                          AND THE EQUATOR.
!*                11        SCANNING MODE FLAGS (CODE TABLE 8)
!*
!*                WORD      SPHERICAL HARMONICS
!*                ----      -------------------
!*                  1       DATA REPRESENTATION TYPE (CODE TABLE 6)
!*                  2       J - PENTAGONAL RESOLUTION PARAMETER
!*                  3       K - PENTAGONAL RESOLUTION PARAMETER
!*                  4       M - PENTAGONAL RESOLUTION PARAMETER
!*                  5       REPRESENTATION TYPE (CODE TABLE 9)
!*                  6       REPRESENTATION MODE (CODE TABLE 10)
!*                7-11      NOT USED
!*
!*
!*                USE FOR OTHER DATA REPRESENTATION TYPES IS
!*
!*                PVERT  - REAL ARRAY OF VERTICAL COORDINATE PARAMETERS
!*                KJLENV - NUMBER OF VALUES IN THIS ARRAY.
!*
!*                USED FOR HYBRID LEVELS ONLY.
!*
!*                PARAMETERS FROM BLOCK 3 , BIT-MAP DEFINITION BLOCK.
!*                --------------------------------------------------
!*
!*                TO BE DEFINED LATER.
!*
!*                PARAMETERS FROM BLOCK 4 , BINARY DATA BLOCK.
!*                -------------------------------------------
!*                PFDATA  - ARRAY OF FLOATING POINT VALUES.
!*                KJLENF  - NUMBER OF VALUES IN THIS ARRAY.
!*                          KJLENF IS NEGATIVE IF MISSING DATA CODED
!*
!*                KBITS   - NUMBER OF BITS FOR CODED DATA VALUES.
!*
!*                KWORD   - NUMBER OF WORDS DECODED.
!*
!*                KCPACK  - KCPACK GREATER THAN ZERO INDICATES COMPLEX
!*                          PACKING, WITH A *TRIANGULAR* SUB-TRUNCATION
!*                          "UNPACKED" OF ORDER KCPACK.
!
!*                KSCALP  - FOR COMPLEX PACKING OF SPECTRAL COEFFICIENTS
!*                          ONLY, LAPLACIAN SCALING FACTOR.
!*
!*                 *****   THE 2 NEXT VALUES ARE SUPPLIED BY THE ROUTINE
!*                         ONLY IF ARGUMENT "LDARPE" IS .FALSE. .  *****
!*
!*                PMIN    - MINIMUM VALUE OF THE FIELD DATA, OR A SLIGHT
!*                          UNDER-APPROXIMATION OF THE "TRUE" MINIMUM OF
!*                          DECODED FIELD DATA.
!*
!*                PMAX    - OVER-ESTIMATION OF THE MAXIMUM VALUE OF THE
!*                          FIELD DATA (COMPUTED WITH PMIN, THE SCALE
!*                          FACTOR "ISCALE" OF GRIB, AND KBITS).
!*
!*
!*                KERR    - ERROR INDICATOR.
!*
!*                          0, NO ERROR.
!*
!*                         -1, NUMBER OF BITS PER DATA VALUE EXCEEDS
!*                             WORD LENGTH, OR MAXIMUM ALLOWED.
!*                         -2, INPUT ARRAY HOLDS FEWER CODED VALUES
!*                             THAN EXPECTED.
!*                         -3, OUTPUT ARRAY TOO SMALL TO HOLD DECODED
!*                             DATA VALUES.
!*                         -4, CURRENTLY UNDEFINED OPTION SELECTED.
!*
!*                          1, NO BLOCK 5 FOUND.
!                           2, ERROR IN CONVERTING OLD TO NEW FORM OF
!                              BLOCK 1 OR 2.
!*                          3, NO BLOCK 0 FOUND.
!*
!*    GENERAL  : DECOGA CALLS GBYTE
!*                            GSBYTE
!*                            OFFSET
!*                            DECFP
!*
!*
!*    MODIFIED : J. HENNESSY 17.06.87
!*               CONVERT OLD FORMAT BLOCK 1 AND 2 TO NEW FORMAT.
!*               ( DISTINCTION BETWEEN ANALYSIS AND INITIALISED
!*               NOT POSSIBLE ).
!*
!*    Modifications by Mats HAMRUD, ECMWF, to handle complex unpacking
!*    of spectral harmonics data, 1988.
!*
!*    Modifications by Jean CLOCHARD, French DMN, February 1990:
!*
!*    -  to remove automatic arrays introduced by the last modification;
!*    -  to comply with the "DOCTOR" norm;
!*    -  to allow modifications of the unpacking for ARPEGE files use,
!*       in an optional way.
!*
!********************************************************************
!
!
!
!
!
!
!
!
!

      USE SDL_MOD   , ONLY : SDL_SRLABORT

      IMPLICIT NONE
!
      INTEGER (KIND=JPLIKM) :: KLENF
      INTEGER (KIND=JPLIKM) :: KBITS
      INTEGER (KIND=JPLIKM) :: KNBIT
      INTEGER (KIND=JPLIKM) :: KLENV
      INTEGER (KIND=JPLIKM) :: KLENG
      INTEGER (KIND=JPLIKM) :: KWORD
      INTEGER (KIND=JPLIKM) :: KJLENV
      INTEGER (KIND=JPLIKM) :: KJLENF
      INTEGER (KIND=JPLIKM) :: KCPACK
      INTEGER (KIND=JPLIKM) :: KSCALP
      INTEGER (KIND=JPLIKM) :: KERR
!
      INTEGER (KIND=JPLIKM) :: KB1PAR(19)
      INTEGER (KIND=JPLIKM) :: KB2PAR(17)
      INTEGER (KIND=JPLIKB) :: KGRIB(KLENG)
!
      REAL (KIND=JPDBLD) :: PMIN
      REAL (KIND=JPDBLD) :: PMAX
!
      REAL (KIND=JPDBLD) :: PFDATA(KLENF)
      REAL (KIND=JPDBLD) :: PVERT(KLENV)
!
      LOGICAL :: LDARPE
!
      INTEGER (KIND=JPLIKM) :: IMAX, IOFF, IBYTE, INVAL, ISNEW
      INTEGER (KIND=JPLIKM) :: J, ITEMP, INC, ILBLK
      INTEGER (KIND=JPLIKM) :: ILNIL, IEXP, IMANT, ILEN
      INTEGER (KIND=JPLIKM) :: IPW, IPB, ILBIN, IFLAG, IREP
      INTEGER (KIND=JPLIKM) :: ISCALX, ISCALE, IMISS, ISPDA
      INTEGER (KIND=JPLIKM) :: IPREMC, ISSUIV, IL, ILBVAL
      INTEGER (KIND=JPLIKB) :: ILEXP, ILMANT, ILFLAG, ILSCALX, ILLNIL
      INTEGER (KIND=JPLIKB) :: ILLBLK, ILLBIN, ILBITS, ILSPDA, ILSCALP
      INTEGER (KIND=JPLIKB) :: ILCPACK
!
!
      INTEGER (KIND=JPLIKM) :: IBLOCK(24), ILAT(2)
      INTEGER (KIND=JPLIKB) :: ILBLOCK(24), ILB2PAR(17), ILLAT(2)
!
      REAL (KIND=JPDBLD) :: ZSCALE
!
!
!     SET MAXIMUM NUMBER OF BITS PER DATA FIELD.
!
      SAVE IMAX
      DATA IMAX /60/
!
!     CLEAR ERROR INDICATOR.
!
      REAL(KIND=JPRB) :: ZHOOK_HANDLE
      IF (LHOOK) CALL DR_HOOK('DECOGA',0,ZHOOK_HANDLE)
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
!
!
!
!
!
!
!
!********************************************************************
!*
!*    BLOCK 0 - INDICATOR BLOCK.
!*
!********************************************************************
!
!
!     EXTRACT 4 OCTETS CONTAINING ASCII G R I B .
!
      KWORD = 1
      IOFF  = 0
      IBYTE = 8
      INVAL  = 4
!
      CALL GSBYTE_MF (KGRIB(1),ILBLOCK(1),IOFF,IBYTE,0,INVAL,           &
     &             KNBIT,'D',KLENG,KERR,KWORD,.TRUE.)
      IBLOCK=ILBLOCK
!
      IF (KERR.NE.0) THEN
        IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
        RETURN
      ELSEIF (IBLOCK(1).NE.71.AND.IBLOCK(2).NE.82.AND.                  &
     &        IBLOCK(3).NE.73.AND.IBLOCK(4).NE.66) THEN
        KERR=3
        WRITE (UNIT=*,FMT=*) 'NO ''GRIB'' GROUP (BLOCK 0) FOUND'
        IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
        RETURN
      ENDIF
!
!
!
!
!
!
!
!********************************************************************
!*
!*    BLOCK 1 - PRODUCT DEFINITION BLOCK.
!*
!********************************************************************
!
!
!     EXTRACT FIRST 4 OCTETS OF BLOCK 1.
!
      INVAL  = 4
!
      CALL GSBYTE_MF (KGRIB(KWORD),ILBLOCK(1),IOFF,IBYTE,0,INVAL,       &
     &             KNBIT,'D',KLENG,KERR,KWORD,.TRUE.)
      IBLOCK=ILBLOCK
      IF (KERR.NE.0)  THEN
        IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
        RETURN
      ENDIF
!
!     IF THESE OCTETS DO NOT CONTAIN 0, 0, 24 AND 0 RESPECTIVELY
!     THEN THE DATA IS IN THE OLD VERSION OF THE CODE.
!
      IF (IBLOCK(1).EQ.0.AND.IBLOCK(2).EQ.0.AND.IBLOCK(3).EQ.24         &
     &    .AND.IBLOCK(4).EQ.0)                                          &
     &      THEN
                ISNEW = 1
                INVAL = 20
            ELSE
                ISNEW = 0
                INVAL = 16
            ENDIF
!
!     EXTRACT NEXT 16 OR 20 OCTETS OF BLOCK 1 - NUMBER DEPENDS
!     ON VERSION OF CODE.
!
      CALL GSBYTE_MF (KGRIB(KWORD),ILBLOCK(5),IOFF,IBYTE,0,INVAL,       &
     &             KNBIT,'D',KLENG,KERR,KWORD,.TRUE.)
      IBLOCK=ILBLOCK
      IF (KERR.NE.0)  THEN
        IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
        RETURN
      ENDIF
!
!     TRANSFER PRODUCT DEFINITION INFORMATION TO OUTPUT ARRAY.
!     EXCEPT FOR NUMBER AVERAGED/ACCUMULATED AND MISSING FROM AVER/ACCUM
!
      DO 100 J=1,17
         KB1PAR(J) = IBLOCK(J+4*ISNEW)
  100 CONTINUE
!
!
!
!
!     NUMBER AVERAGED OCCUPIES 2 OCTETS.
!
      KB1PAR(18) = IBLOCK(18+4*ISNEW) * 256 + IBLOCK(19+4*ISNEW)
!
!     NUMBER MISSING FROM AVERAGES/ACCUMULATIONS.
!
      KB1PAR(19)=IBLOCK(20+4*ISNEW)
!
!
!***
!*    DESCRIPTION OF LEVEL OR LAYER ( CODE TABLE 3 ).
!***
!
!     CERTAIN LEVEL TYPES REQUIRE THAT THE DESCRIPTION OCCUPY
!     BOTH 8 BIT FIELDS. PUT SINGLE VALUE IN FIRST LEVEL WORD.
!
      ITEMP = 32 + (ISNEW*224)
!
      IF( (KB1PAR(6).EQ. 20).OR.                                        &
     &    (KB1PAR(6).EQ.100).OR.                                        &
     &    (KB1PAR(6).EQ.103).OR.                                        &
     &    (KB1PAR(6).EQ.105).OR.                                        &
     &    (KB1PAR(6).EQ.107).OR.                                        &
     &    (KB1PAR(6).EQ.109).OR.                                        &
     &    (KB1PAR(6).EQ.111).OR.                                        &
     &    (KB1PAR(6).EQ.113).OR.                                        &
     &    (KB1PAR(6).EQ.115).OR.                                        &
     &    (KB1PAR(6).EQ.117).OR.                                        &
     &    (KB1PAR(6).EQ.125).OR.                                        &
     &    (KB1PAR(6).EQ.127).OR.                                        &
     &    (KB1PAR(6).EQ.160).OR.                                        &
     &    (KB1PAR(6).EQ.210) ) THEN
!
              KB1PAR(7) = KB1PAR(7) * ITEMP + KB1PAR(8)
              KB1PAR(8) = 0
          ENDIF
!
!
!
!
!
!
!
!
!
!***
!*    TIME RANGE.
!***
!
!     ONE TIME RANGE CAN OCCUPY TWO OCTETS.
!
      IF (KB1PAR(17).EQ.10)                                             &
     &   THEN
             KB1PAR(15) = KB1PAR(15) * ITEMP + KB1PAR(16)
             KB1PAR(16) = 0
         ENDIF
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
!
!********************************************************************
!*
!*    BLOCK 2 - GRID DESCRIPTION BLOCK.
!*
!********************************************************************
!
!***
!*    CHECK FLAG INDICATING IF BLOCK 2 IS INCLUDED IN CODE.
!***
!
!     INC IS SET TO 1 , IF BLOCK 2 IS INCLUDED.
!
      INC = 0
!
!
      IF (ISNEW.EQ.0)                                                   &
     &   THEN
!            OLD VERSION OF CODE
!
!            BLOCKS INCLUDED    BINARY VALUE      DECIMAL VALUE
!
!                NONE             00000000               0
!                 2               00000001               1
!                 3               00000010               2
!             2 AND 3             00000011               3
!
             IF (KB1PAR(4).EQ.1.OR.KB1PAR(4).EQ.3) INC = 1
         ELSE
!            NEW VERSION
!
!            BLOCKS INCLUDED    BINARY VALUE      DECIMAL VALUE
!
!                NONE             00000000               0
!                 2               10000000             128
!                 3               01000000              64
!             2 AND 3             11000000             192
!
             IF (KB1PAR(4).EQ.128.OR.KB1PAR(4).EQ.192) INC = 1
         ENDIF
!
      IF (INC.EQ.1)                                                     &
     &   THEN
!
!
!***
!*           LENGTH OF GRID DESCRIPTION BLOCK.
!***
!
             CALL GBYTE_MF (KGRIB(KWORD),ILLBLK,IOFF,24)
             ILBLK=ILLBLK
             CALL OFFSET_MF (IOFF,1,KWORD,24,KNBIT,KLENG,KERR)
             IF (KERR.NE.0)  THEN
               IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
               RETURN
             ENDIF
!
!
!
!***
!*           NUMBER OF UNUSED BITS AT END OF BLOCK.
!***
!
             CALL GBYTE_MF (KGRIB(KWORD),ILLNIL,IOFF,8)
             ILNIL=ILLNIL
             CALL OFFSET_MF (IOFF,1,KWORD,8,KNBIT,KLENG,KERR)
             IF (KERR.NE.0)  THEN
               IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
               RETURN
             ENDIF
!
!
!
!
!***
!*           NEXT OCTET IS RESERVED.
!***
!
             CALL OFFSET_MF (IOFF,1,KWORD,8,KNBIT,KLENG,KERR)
             IF (KERR.NE.0)  THEN
               IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
               RETURN
             ENDIF
!
!
!
!
!
!
!
!***
!*           DATA REPRESENTATION TYPE.
!***
!
             CALL GBYTE_MF (KGRIB(KWORD),ILB2PAR(1),IOFF,8)
             KB2PAR=ILB2PAR
             CALL OFFSET_MF (IOFF,1,KWORD,8,KNBIT,KLENG,KERR)
             IF (KERR.NE.0)  THEN
               IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
               RETURN
             ENDIF
!
!            LAT/LONGITUDE GRID, GAUSSIAN GRID AND SPHERICAL HARMONICS
!            ARE THE ONLY DATA REPRESENTATIONS HANDLED.
!
             IF (KB2PAR(1).NE.0.AND.KB2PAR(1).NE.4.AND.KB2PAR(1).NE.50  &
     &           .AND.KB2PAR(1).NE.80)                                  &
     &          THEN
                     WRITE (*,*)'GRID DESCRIPTION BLOCK NOT YET DEFINED'
                     KERR = -4
                     IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
                     RETURN
                ENDIF
!
!
!
!
!***
!*           LAT/LONG OR GAUSSIAN GRID.
!***
!
             IF (KB2PAR(1).EQ.0.OR.KB2PAR(1).EQ.4)                      &
     &         THEN
!
!                  NUMBER OF LAT/LONG POINTS.
!
                   CALL GSBYTE_MF (KGRIB(KWORD),ILB2PAR(2),IOFF,16,0,2, &
     &                          KNBIT,'D',KLENG,KERR,KWORD,.TRUE.)
                   KB2PAR=ILB2PAR
                   IF (KERR.NE.0)  THEN
                     IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
                     RETURN
                   ENDIF
!
!                  LAT/LONG OF ORIGIN.
!
                   CALL GSBYTE_MF (KGRIB(KWORD),ILLAT(1),IOFF,24,0,2,   &
     &                          KNBIT,'D',KLENG,KERR,KWORD,.TRUE.)
                   ILAT=ILLAT
                   IF (KERR.NE.0)  THEN
                     IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
                     RETURN
                   ENDIF
!
!                  IF SIGN BIT SET TO 1 , VALUES ARE NEGATIVE.
!
                  DO 200 J=1,2
!
                  IF (ILAT(J).LE.2**23) THEN
                    KB2PAR(J+3) = ILAT(J)
                  ELSE
                    KB2PAR(J+3) = 2**23 - ILAT(J)
                  ENDIF
!
  200             CONTINUE
!
!
!                  RESOLUTION FLAG.
!
                   CALL GBYTE_MF (KGRIB(KWORD),ILB2PAR(6),IOFF,8)
                   KB2PAR=ILB2PAR
                   CALL OFFSET_MF (IOFF,1,KWORD,8,KNBIT,KLENG,KERR)
                   IF (KERR.NE.0)  THEN
                     IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
                     RETURN
                   ENDIF
!
!
!                  LAT/LONG OF EXTREME POINTS.
!
                   CALL GSBYTE_MF (KGRIB(KWORD),ILLAT(1),IOFF,24,0,2,   &
     &                          KNBIT,'D',KLENG,KERR,KWORD,.TRUE.)
                   ILAT=ILLAT
                   IF (KERR.NE.0)  THEN
                     IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
                     RETURN
                   ENDIF
!
!                  IF SIGN BIT SET TO 1 , VALUES ARE NEGATIVE.
!
                  DO 300 J=1,2
!
                  IF (ILAT(J).LE.2**23) THEN
                    KB2PAR(J+6) = ILAT(J)
                  ELSE
                    KB2PAR(J+6) = 2**23 - ILAT(J)
                  ENDIF
!
  300             CONTINUE
!
!                 DIRECTION INCREMENTS / NUMBER OF LATITUDE LINES.
!
                  CALL GSBYTE_MF (KGRIB(KWORD),ILB2PAR(9),IOFF,16,0,2,  &
     &                         KNBIT,'D',KLENG,KERR,KWORD,.TRUE.)
                  KB2PAR=ILB2PAR
                  IF (KERR.NE.0)  THEN
                    IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
                    RETURN
                  ENDIF
!
!                 SCANNING MODE FLAGS.
!
                  CALL GBYTE_MF (KGRIB(KWORD),ILB2PAR(11),IOFF,8)
                  KB2PAR=ILB2PAR
                  CALL OFFSET_MF (IOFF,1,KWORD,8,KNBIT,KLENG,KERR)
                  IF (KERR.NE.0)  THEN
                    IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
                    RETURN
                  ENDIF
!
!                 4 RESERVED OCTETS.
!
                  CALL OFFSET_MF (IOFF,4,KWORD,8,KNBIT,KLENG,KERR)
                  IF (KERR.NE.0)  THEN
                    IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
                    RETURN
                  ENDIF
!
              ENDIF
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
!
!
!
!
!***
!*            SPHERICAL HARMONIC DATA.
!***
!
              IF (KB2PAR(1).EQ.50.OR.KB2PAR(1).EQ.80)                   &
     &           THEN
!
!                    PENTAGONAL RESOLUTION PARAMETERS.
!
                     CALL GSBYTE_MF (KGRIB(KWORD),ILB2PAR(2),IOFF,16,0, &
     &                            3,KNBIT,'D',KLENG,KERR,KWORD,.TRUE.)
                     KB2PAR=ILB2PAR
                     IF (KERR.NE.0)  THEN
                       IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
                       RETURN
                     ENDIF
!
!                    REPRESENTATION TYPE AND MODE.
!
                     CALL GSBYTE_MF (KGRIB(KWORD),ILB2PAR(5),IOFF,8,0,2,&
     &                            KNBIT,'D',KLENG,KERR,KWORD,.TRUE.)
                     KB2PAR=ILB2PAR
                     IF (KERR.NE.0)  THEN
                       IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
                       RETURN
                     ENDIF
!
!                    18 RESERVED OCTETS.
!
                     CALL OFFSET_MF (IOFF,18,KWORD,8,KNBIT,KLENG,KERR)
                     IF (KERR.NE.0)  THEN
                       IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
                       RETURN
                     ENDIF
!
                 ENDIF
!
!**
!*    STRETCHED AND ROTATED SHPERICAL HARMONICS.
!**
         IF(KB2PAR(1).EQ.80) THEN
!
!*    SOUTHERN POLE OF ROTATED GRID
!
                   CALL GSBYTE_MF (KGRIB(KWORD),ILLAT(1),IOFF,24,0,2,   &
     &                          KNBIT,'D',KLENG,KERR,KWORD,.TRUE.)
                   ILAT=ILLAT
                   IF (KERR.NE.0)  THEN
                     IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
                     RETURN
                   ENDIF
!
!                  IF SIGN BIT SET TO 1 , VALUES ARE NEGATIVE.
!
                  DO 410 J=1,2
!
                  IF (ILAT(J).LE.2**23) THEN
                    KB2PAR(J+11) = ILAT(J)
                  ELSE
                    KB2PAR(J+11) = 2**23 - ILAT(J)
                  ENDIF
!
  410             CONTINUE
!
!*    ANGLE OF ROTATION
!
      CALL GBYTE_MF (KGRIB(KWORD),ILEXP,IOFF,8)
      IEXP=ILEXP
      CALL OFFSET_MF (IOFF,1,KWORD,8,KNBIT,KLENG,KERR)
      IF (KERR.NE.0)  THEN
        IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
        RETURN
      ENDIF
      CALL GBYTE_MF (KGRIB(KWORD),ILMANT,IOFF,24)
      IMANT=ILMANT
      CALL OFFSET_MF (IOFF,1,KWORD,24,KNBIT,KLENG,KERR)
      IF (KERR.NE.0)  THEN
        IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
        RETURN
      ENDIF
!
             CALL DECFP_MF (KB2PAR(14),IEXP,IMANT)
!
!*    POLE OF STRETCHING
!
                   CALL GSBYTE_MF (KGRIB(KWORD),ILLAT(1),IOFF,24,0,2,   &
     &                          KNBIT,'D',KLENG,KERR,KWORD,.TRUE.)
                   ILAT=ILLAT
                   IF (KERR.NE.0)  THEN
                     IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
                     RETURN
                   ENDIF
!
!                  IF SIGN BIT SET TO 1 , VALUES ARE NEGATIVE.
!
                  DO 420 J=1,2
!
                  IF (ILAT(J).LE.2**23) THEN
                    KB2PAR(J+14) = ILAT(J)
                  ELSE
                    KB2PAR(J+14) = 2**23 - ILAT(J)
                  ENDIF
!
  420             CONTINUE
!
!*    STRETCHING FACTOR
!
      CALL GBYTE_MF (KGRIB(KWORD),ILEXP,IOFF,8)
      IEXP=ILEXP
      CALL OFFSET_MF (IOFF,1,KWORD,8,KNBIT,KLENG,KERR)
      IF (KERR.NE.0)  THEN
        IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
        RETURN
      ENDIF
      CALL GBYTE_MF (KGRIB(KWORD),ILMANT,IOFF,24)
      IMANT=ILMANT
      CALL OFFSET_MF (IOFF,1,KWORD,24,KNBIT,KLENG,KERR)
      IF (KERR.NE.0)  THEN
        IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
        RETURN
      ENDIF
!
      CALL DECFP_MF (KB2PAR(17),IEXP,IMANT)
!
      ENDIF
!
!
!
!            LENGTH IS 32 OCTETS FOR LAT/LONG, GAUSSIAN AND SPHERICAL
!            HARMONICS, 52 OCTETS FOR STRETCHED AND ROTATED SPHERICAL
!            HARMONICS.FOR ANY DATA  ON HYBRID LEVELS THE
!            VERTICAL COORDINATES ARE ADDED.
!            GET NUMBER OF VERTICAL COORDINATE PARAMETERS, IF ANY.
!
         IF(KB2PAR(1).EQ.80) THEN
            ILEN=52
         ELSE
            ILEN=32
         ENDIF
!
             KJLENV = (ILBLK - ILEN) / 4
!
             IF(KLENV.LT.KJLENV) THEN
                    KERR = -3
                    WRITE (*,9001) KJLENV,KLENV
 9001               FORMAT (1H ,'NUMBER OF VERTICAL COORDINATES - ',I4, &
     &                          ', ARRAY SIZE IS - ',I4)
                    IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
                    RETURN
                ENDIF
!
!
!
!***
!*               VERTICAL COORDINATE PARAMETERS FOR HYBRID LEVELS.
!***
!
                 IF (KJLENV.NE.0) THEN
!
                    DO 400 J=1,KJLENV
                    CALL GBYTE_MF (KGRIB(KWORD),ILEXP,IOFF,8)
                    IEXP=ILEXP
                    CALL OFFSET_MF (IOFF,1,KWORD,8,KNBIT,KLENG,KERR)
                    IF (KERR.NE.0)  THEN
                      IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
                      RETURN
                    ENDIF
                    CALL GBYTE_MF (KGRIB(KWORD),ILMANT,IOFF,24)
                    IMANT=ILMANT
                    CALL OFFSET_MF (IOFF,1,KWORD,24,KNBIT,KLENG,KERR)
                    IF (KERR.NE.0)  THEN
                      IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
                      RETURN
                    ENDIF
                    CALL DECFP_MF (PVERT(J),IEXP,IMANT)
  400               CONTINUE
!
                 ENDIF
!
         ENDIF
!
!
!
!     RETURN IF ONLY PRODUCT AND GRID DEFINITION BLOCKS REQUIRED.
!
      IF (KLENF.EQ.1)  THEN
        IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
        RETURN
      ENDIF
!
!
!
!
!
!
!
!
!********************************************************************
!*
!*    BLOCK 3 - BIT MAP BLOCK.
!*
!********************************************************************
!
!     INC IS SET TO 1 , IF BLOCK 3 IS INCLUDED.
!
      INC = 0
!
!
      IF (ISNEW.EQ.0)                                                   &
     &   THEN
!            OLD VERSION OF CODE
!
!            BLOCKS INCLUDED    BINARY VALUE      DECIMAL VALUE
!
!                NONE             00000000               0
!                 2               00000001               1
!                 3               00000010               2
!             2 AND 3             00000011               3
!
             IF (KB1PAR(4).EQ.2.OR.KB1PAR(4).EQ.3) INC = 1
         ELSE
!            NEW VERSION
!
!            BLOCKS INCLUDED    BINARY VALUE      DECIMAL VALUE
!
!                NONE             00000000               0
!                 2               10000000             128
!                 3               01000000              64
!             2 AND 3             11000000             192
!
             IF (KB1PAR(4).EQ.64.OR.KB1PAR(4).EQ.192) INC = 1
         ENDIF
!
      IF (INC.EQ.1)                                                     &
     &     THEN
               WRITE (*,*)'BIT MAP BLOCK NOT YET DEFINED'
               KERR = -4
               IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
               RETURN
           ENDIF
!
!
!
!
!**********************************************************************
!*
!*    CONVERT VALUES IN BLOCKS 1 AND 2 TO NEW CODE VALUES.
!*
!**********************************************************************
!
!
      IF (ISNEW.EQ.0)                                                   &
     &   THEN
!
!            CHANGE CODE FOR FLAG INDICATING INCLUSION OR OMISSION
!            OF BLOCKS 2 AND 3.
!
!            BINARY VALUE OLD CODE = 00000001
!            BINARY VALUE NEW CODE = 10000000 (128 DECIMAL)
!
             IF (KB1PAR(4).EQ.1)                                        &
     &          THEN
                    KB1PAR(4) = 128
                ELSE
                    WRITE (*,*) 'DECOGA : BLOCK FLAG ERROR.'
                    KERR = 2
                    IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
                    RETURN
                ENDIF
!
!            CHANGE CODE FOR TIME UNIT.
!
             IF (KB1PAR(14).EQ.40)                                      &
     &          THEN
                    KB1PAR(14) = 1
                ELSE
                    WRITE (*,*) 'DECOGA : TIME UNIT ERROR.'
                    KERR = 2
                    IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
                    RETURN
                ENDIF
!
!            CONVERT FLAGS FOR LAT/LONG AND GAUSSIAN GRID DATA.
!
             IF (KB2PAR(1).EQ.0.OR.KB2PAR(1).EQ.4)                      &
     &          THEN
!
!                   CONVERT SCANNING MODE FLAG.
!
                    IF (KB2PAR(11).EQ.1)                                &
     &                 THEN
                           KB2PAR(11) = 0
                       ELSE
                           WRITE (*,*) 'DECOGA : SCAN MODE FLAG ERROR.'
                           KERR = 2
                           IF (LHOOK)                                   &
     &                       CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
                           RETURN
                       ENDIF
!
!                   CONVERT RESOLUTION FLAG.
!
                    IF (KB2PAR(6).EQ.3)                                 &
     &                 THEN
                           KB2PAR(6) = 128
                       ELSE
                           WRITE (*,*) 'DECOGA : RESOLUTION FLAG ERROR.'
                           KERR = 2
                           IF (LHOOK)                                   &
     &                       CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
                           RETURN
                       ENDIF
                ENDIF
         ENDIF
!
!
!
!
!
!
!********************************************************************
!*
!*    BLOCK 4 - BINARY DATA BLOCK.
!*
!********************************************************************
!
!
!
!***
!*    GET LENGTH OF BINARY DATA BLOCK.
!***
!
!
      IPW = KWORD
      IPB = IOFF
!
      CALL GBYTE_MF (KGRIB(KWORD),ILLBIN,IOFF,24)
      ILBIN=ILLBIN
      CALL OFFSET_MF (IOFF,1,KWORD,24,KNBIT,KLENG,KERR)
      IF (KERR.NE.0)  THEN
        IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
        RETURN
      ENDIF
!
!
!
!
!***
!*    4 BIT FLAG / 4 BIT COUNT OF UNUSED BITS AT END OF BLOCK OCTET.
!***
!
      CALL GBYTE_MF (KGRIB(KWORD),ILFLAG,IOFF,8)
      IFLAG=ILFLAG
      CALL OFFSET_MF (IOFF,1,KWORD,8,KNBIT,KLENG,KERR)
      IF (KERR.NE.0)  THEN
        IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
        RETURN
      ENDIF
!
      IF (ISNEW.EQ.0)                                                   &
     &    THEN
!             OLD VERSION OF CODE
!
!             0000---- GRID POINT
!             0001---- SPHERICAL HARMONICS
!
              IREP = IFLAG / 16
              ILNIL = IFLAG - IREP * 16
!
          ELSE
!             NEW VERSION OF CODE
!
!             0000---- GRID POINT
!             1000---- SPHERICAL HARMONICS
!             1100----       -''-           COMPLEX PACKING
!
              IREP = IFLAG / 128
              ILNIL = MOD(IFLAG,16)
!
          ENDIF
!
!
!
!
!
!
!
!
!***
!*    GET SCALE FACTOR
!***
!
      CALL GBYTE_MF (KGRIB(KWORD),ILSCALX,IOFF,16)
      ISCALX=ILSCALX
      CALL OFFSET_MF (IOFF,1,KWORD,16,KNBIT,KLENG,KERR)
      IF (KERR.NE.0)  THEN
        IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
        RETURN
      ENDIF
!
      IF (ISCALX.LE.2**15) THEN
        ISCALE = ISCALX
      ELSE
        ISCALE = 2**15 - ISCALX
      ENDIF
!
!
!
!
!***
!*    GET REFERENCE VALUE (PMIN) IN GRIB FORMAT (IEXP,IMANT)
!***
!
!
      CALL GBYTE_MF (KGRIB(KWORD),ILEXP,IOFF,8)
      IEXP=ILEXP
      CALL OFFSET_MF (IOFF,1,KWORD,8,KNBIT,KLENG,KERR)
      IF (KERR.NE.0)  THEN
        IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
        RETURN
      ENDIF
      CALL GBYTE_MF (KGRIB(KWORD),ILMANT,IOFF,24)
      IMANT=ILMANT
      CALL OFFSET_MF (IOFF,1,KWORD,24,KNBIT,KLENG,KERR)
      IF (KERR.NE.0)  THEN
        IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
        RETURN
      ENDIF
!
!
!     CHECK FOR MISSING DATA INDICATORS.
!
      IMISS = 0
      IF (ISCALX.EQ.65535.AND.IEXP.EQ.255.AND.IMANT.EQ.16777215)        &
     &   IMISS = 1
!
!
!
!***
!*    GET NUMBER OF BITS IN EACH DATA VALUE.
!***
!
      CALL GBYTE_MF (KGRIB(KWORD),ILBITS,IOFF,8)
      KBITS=ILBITS
      CALL OFFSET_MF (IOFF,1,KWORD,8,KNBIT,KLENG,KERR)
      IF (KERR.NE.0)  THEN
        IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
        RETURN
      ENDIF
!
!
!     CHECK NUMBER OF BITS PER DATA FIELD.
!
      IF (KBITS.GT.KNBIT.OR.KBITS.GT.IMAX)                              &
     &    THEN
              KERR = -1
              WRITE (*,9002) KBITS,KNBIT,IMAX
 9002         FORMAT (1H ,'NUMBER OF BITS PER DATA VALUE, ',I3,         &
     &                'EXCEEDS WORD LENGTH, ',I3,' OR MAXIMUM ',        &
     &                ' PERMITTED VALUE, ',I3)
              CALL SDL_SRLABORT
!
      ELSEIF (LDARPE) THEN
!
!     Minimum value given as input argument... it has not been decoded
!     before as stored in GRIB field (should be zero).
!        But the scale factor may be computed, now.
!
        ZSCALE= ( PMAX - PMIN ) / REAL(2**KBITS-1,KIND=JPDBLD)
!
      ELSEIF (IMISS.EQ.0) THEN
!
!
!     CONVERT REFERENCE VALUE AND SCALE FACTOR.
!
!
        CALL DECFP_MF (PMIN,IEXP,IMANT)
        ZSCALE = 2.0_JPDBLD**ISCALE
        PMAX=PMIN+REAL(2**(KBITS+1)-3,KIND=JPDBLD) *                    &
     &                 2.0_JPDBLD ** (ISCALE-1)
!
      ENDIF
!
!**
!*    IF COMPLEX PACKING GET ADDITIONAL PARAMETERS
!**
      IF(IFLAG.GE.192) THEN
!
!     GET START OCTET OF PACKED DATA
!
      CALL GBYTE_MF (KGRIB(KWORD),ILSPDA,IOFF,16)
      ISPDA=ILSPDA
      CALL OFFSET_MF (IOFF,1,KWORD,16,KNBIT,KLENG,KERR)
      IF (KERR.NE.0)  THEN
        IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
        RETURN
      ENDIF
      IPREMC=1+(8*ISPDA-1)/KNBIT
!
      IF (IPREMC.GT.KLENF) THEN
        KERR=-3
              WRITE (*,9004) KLENF,ISPDA
 9004         FORMAT (' OUTPUT ARRAY LENGTH (',I7,                      &
     &                ' WORDS) TOO SHORT, START OCTET OF PACKED DATA =',&
     &                I9)
              CALL SDL_SRLABORT
      ENDIF
!
!     GET LAPLACIAN SCALING FACTOR
!
      CALL GBYTE_MF (KGRIB(KWORD),ILSCALP,IOFF,16)
      KSCALP=ILSCALP
      CALL OFFSET_MF (IOFF,1,KWORD,16,KNBIT,KLENG,KERR)
!
      IF (KERR.NE.0) THEN
        IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
        RETURN
      ELSEIF (KSCALP.GT.2**15) THEN
        KSCALP=2**15-KSCALP
      ENDIF
!
!     GET TRUNCATION OF UNPACKED COEFFS.
!
      DO 430 J=1,3
      CALL GBYTE_MF (KGRIB(KWORD),ILCPACK,IOFF,8)
      KCPACK=ILCPACK
      CALL OFFSET_MF (IOFF,1,KWORD,8,KNBIT,KLENG,KERR)
      IF (KERR.NE.0)  THEN
        IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
        RETURN
      ENDIF
 430  CONTINUE
!
!     COMPUTE NUMBER OF UNPACKED DATA ITEMS
!
      IREP=(KCPACK+1)**2
      ENDIF
!
!***
!*    IF DATA IS IN SPHERICAL HARMONIC FORM, NEXT OCTETS
!*    CONTAIN UNPACKED COEFFICIENT(S).
!***
!
      IF (IREP.NE.0)                                                    &
     &    THEN
!
!            EVENTUALLY,
!            GET UNPACKED COEFFICIENTS IN GRIB FORMAT AND
!            CONVERT TO FLOATING POINT.
!
        IF (IMISS.EQ.1) THEN
!
          DO 440 J=1,IREP
          PFDATA(J)=0.0_JPDBLD
  440     CONTINUE
!
        ELSEIF (LDARPE) THEN
!
!       The "unpacked" values should then contain zeroes,
!     and are skipped: the corresponding values of PFDATA array
!     are then not provided. A positioning in KGRIB is made.
!
          CALL OFFSET_MF (IOFF,IREP,KWORD,32,KNBIT,KLENG,KERR)
          IF (KERR.NE.0)  THEN
            IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
            RETURN
          ENDIF
!
        ELSE
!
         DO 450 J=1,IREP
             CALL GBYTE_MF (KGRIB(KWORD),ILEXP,IOFF,8)
             IEXP=ILEXP
             CALL OFFSET_MF (IOFF,1,KWORD,8,KNBIT,KLENG,KERR)
             IF (KERR.NE.0)  THEN
               IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
               RETURN
             ENDIF
             CALL GBYTE_MF (KGRIB(KWORD),ILMANT,IOFF,24)
             IMANT=ILMANT
             CALL OFFSET_MF (IOFF,1,KWORD,24,KNBIT,KLENG,KERR)
             IF (KERR.NE.0)  THEN
               IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
               RETURN
             ENDIF
                    CALL DECFP_MF (PFDATA(J),IEXP,IMANT)
 450     CONTINUE
!
        ENDIF
!
        IF (IFLAG.GE.192) THEN
!
!            Proper positioning on start octet of packed data, because
!        it may be not the first octet following the "unpacked" data.
!
          ISSUIV=19+IREP*4
!
          IF (ISSUIV.LT.ISPDA) THEN
            CALL OFFSET_MF (IOFF,ISPDA-ISSUIV,KWORD,8,KNBIT,KLENG,KERR)
            IF (KERR.NE.0)  THEN
              IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
              RETURN
            ENDIF
          ENDIF
!
        ENDIF
!
      ENDIF
!
!
!
!
!*****
!*    DECODE DATA VALUES TO FLOATING POINT AND STORE IN PFDATA.
!*****
!
!     FIRST CALCULATE THE NUMBER OF DATA VALUES.
!
      KJLENF = ILBIN - 11 - IREP * 4
      IF(IFLAG.GE.192) KJLENF=KJLENF-7-(ISPDA-ISSUIV)
      KJLENF = (KJLENF * 8 - ILNIL) / KBITS
!
!     CHECK LENGTH OF OUTPUT ARRAY.
!
      IF (KJLENF+IREP.GT.KLENF)                                         &
     &   THEN
             KERR = -3
             WRITE (*,9003) KJLENF,KLENF
 9003        FORMAT (1H ,'NUMBER OF VALUES TO BE DECODED IS - ',I7,     &
     &                    ', ARRAY SIZE - ',I7)
             CALL SDL_SRLABORT
         ENDIF
!
      IF (IMISS.EQ.0.AND.ZSCALE.GT.0.0_JPDBLD) THEN
        CALL GSBYTE_MF (KGRIB(KWORD),PFDATA(IREP+1),IOFF,KBITS,0,KJLENF,&
     &               KNBIT,'D',KLENG,KERR,KWORD,.TRUE.)
        IF (KERR.NE.0)  THEN
          IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
          RETURN
        ENDIF
        CALL UNPAGB (PFDATA(IREP+1),PFDATA(IREP+1),PMIN,PMAX,KBITS,     &
     &               ZSCALE,KJLENF,LDARPE)
      ELSE
!
        IF (ZSCALE.LE.0.) THEN
!
          DO 460 J= IREP+1,IREP+KJLENF
          PFDATA(J) = PMIN
  460     CONTINUE
!
        ELSE
!
          DO 470 J= IREP+1,IREP+KJLENF
          PFDATA(J) = 0.0_JPDBLD
  470     CONTINUE
!
        ENDIF
!
        CALL OFFSET_MF (IOFF,KJLENF,KWORD,KBITS,KNBIT,KLENG,KERR)
        IF (KERR.NE.0)  THEN
          IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
          RETURN
        ENDIF
      ENDIF
!
!
!     INCLUDE UNPACKED COEFFICIENT IN COUNT, IF SPHERICAL HARMONIC DATA.
!
      KJLENF = IREP + KJLENF
!
!
!
!
!***
!*    SKIP ANY ZERO FILL AT END OF DATA BLOCK.
!***
!
      IL = (KWORD-IPW) * KNBIT + IOFF -IPB
      IL = ILBIN * 8 - IL
!
      IF (IL.NE.0) THEN
              CALL OFFSET_MF (IOFF,1,KWORD,IL,KNBIT,KLENG,KERR)
              IF (KERR.NE.0)  THEN
                IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
                RETURN
              ENDIF
      ENDIF
!
!
!
!
!********************************************************************
!*
!*    BLOCK 5 - END BLOCK.
!*
!********************************************************************
!
!     CHECK 7 7 7 7 AT END OF CODED DATA.
!
!
      INVAL=4
      ILBVAL=8
      CALL GSBYTE_MF (KGRIB(KWORD),ILBLOCK(1),IOFF,ILBVAL,0,INVAL,      &
     &             KNBIT,'D',KLENG,KERR,KWORD,.FALSE.)
      IBLOCK=ILBLOCK
      IF (KERR.NE.0)  THEN
        IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
        RETURN
      ENDIF
!
!     55 IS THE VALUE OF ASCII CHARACTER '7'
!
      DO 600 J=1,INVAL
         IF (IBLOCK(J).NE.55) KERR = 1
  600 CONTINUE
!
      IF (KERR.EQ.1) WRITE (*,*) ' NO 7777 GROUP FOUND '
!
!
!
!     SET NUMBER OF VALUES NEGATIVE, IF MISSING DATA VALUES.
!
      IF (IMISS.EQ.1) KJLENF = - KJLENF
!
      IF (LHOOK) CALL DR_HOOK('DECOGA',1,ZHOOK_HANDLE)
      ENDSUBROUTINE DECOGA
