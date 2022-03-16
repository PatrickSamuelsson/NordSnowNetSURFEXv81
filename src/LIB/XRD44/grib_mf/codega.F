      SUBROUTINE CODEGA (PFDATA,KLENF,KBITS,KNBIT,KB1PAR,               &
     &                   KB2PAR,PVERT,KLENV,KGRIB,KLENG,KWORD,          &
     &                   KROUND,KCPACK,KSCALP,KERR,PMIN,PMAX,LDARPE)
      USE PARKIND1, ONLY : JPRB
      USE YOMHOOK , ONLY : LHOOK, DR_HOOK
      USE LFI_PRECISION
!
!
!********************************************************************
!*
!*     NAME     : CODEGA
!*
!*     FUNCTION : CODE ARRAY OF FLOATING POINT VALUES
!*                IN WMO GRIB CODE.
!*
!*     INPUT    : PARAMETERS FOR BLOCK 1 , PRODUCT DEFINITION BLOCK.
!*                --------------------------------------------------
!*                KB1PAR - INTEGER ARRAY (DIMENSION 19) OF PRODUCT
!*                         DEFINITION INFORMATION.
!*
!*                WORD      CONTENTS
!*                ----      --------
!*                  1       ORIGINATING CENTRE IDENTIFIER.
!*                  2       MODEL IDENTIFICATION.
!*                  3       GRID DEFINITION.
!*                  4       FLAG ( CODE TABLE 1 ).
!*                  5       PARAMETER IDENTIFIER (CODE TABLE 2).
!*                  6       TYPE OF LEVEL (CODE TABLE 3).
!*                 7-8      VALUE(S) OF LEVEL (CODE TABLE 3).
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
!*                          VALUE(S) OF LEVEL CAN OCCUPY 2 WORDS.
!*                          FOR A LAYER THE FIRST WORD DEFINES THE TOP
!*                          AND THE SECOND THE BOTTOM OF THE LAYER.
!*                          FOR A SINGLE LEVEL, ONLY THE FIRST WORD IS
!*                          USED.
!*
!*                PARAMETERS FOR BLOCK 2 , GRID DEFINITION BLOCK.
!*                -----------------------------------------------
!*                KB2PAR - INTEGER ARRAY (DIMENSION 17) CONTAINING GRID
!*                         DEFINITION INFORMATION.
!*                         USE VARIES WITH REPRESENTATION TYPE.
!*
!*                WORD      LAT/LONG GRID
!*                ----      -------------
!*                 1        DATA REPRESENTATION TYPE (CODE TABLE 6)
!*                 2        NO. OF POINTS ALONG A LATITUDE
!*                 3        NO. OF POINTS ALONG A MERIDIAN
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
!*                TO BE DEFINED LATER.
!*
!*                PVERT  - REAL ARRAY OF VERTICAL COORDINATE PARAMETERS
!*                KLENV  - LENGTH OF THIS ARRAY (NUMBER OF COORDINATE
!*                         PARAMETERS).
!*
!*                USED FOR HYBRID LEVELS ONLY.
!*
!*
!*                PARAMETERS FOR BLOCK 3 , BIT-MAP DEFINITION BLOCK.
!*                --------------------------------------------------
!*
!*                TO BE DEFINED LATER.
!*
!*                PARAMETERS FOR BLOCK 4 , BINARY DATA BLOCK.
!*                -------------------------------------------
!*                PFDATA  - ARRAY OF FLOATING POINT VALUES.
!*
!*                          ****
!*                              VALUES IN THIS ARRAY ARE OVERWRITTEN
!*                              BY THE SUBROUTINE.
!*                                                              ****
!*
!*                KLENF   - LENGTH OF THIS ARRAY (NUMBER OF DATA
!*                          VALUES TO BE CODED)
!*                KBITS   - NUMBER OF BITS FOR CODED DATA VALUES.
!*
!*                KCPACK  - KCPACK GREATER THAN ZERO INDICATES COMPLEX
!*                          PACKING, WITH A *TRIANGULAR* SUB-TRUNCATION
!*                          "UNPACKED" OF ORDER KCPACK.
!
!*                KSCALP  - FOR COMPLEX PACKING OF SPECTRAL COEFFICIENTS
!*                          ONLY, LAPLACIAN SCALING FACTOR.
!*
!*                          TO INDICATE MISSING DATA VALUES, KLENF IS
!*                          MADE NEGATIVE AND ALL DATA VALUES SET TO 0.
!*
!*                OTHER PARAMETERS
!*                ----------------
!*
!*                KNBIT    - NUMBER OF BITS IN COMPUTER WORD.
!*
!*                KGRIB   - ARRAY TO RECEIVE DATA IN GRIB CODE.
!*                KLENG   - LENGTH OF THIS ARRAY.
!*
!*                KROUND  - 0 , NO ROUNDING.
!*                          1 , ROUND TO NEAREST 120 OCTETS.
!*
!*                LDARPE  - .TRUE., modifications for ARPEGE coding.
!*                          .FALSE., no such modifications.
!*
!*    OUTPUT    : KGRIB   - DATA CODED IN GRIB CODE.
!*                KWORD   - NUMBER OF WORDS OCCUPIED BY CODED DATA.
!*
!*                KERR    - ERROR INDICATOR.
!*
!*                           0, NO ERROR.
!*
!*                          -1, NUMBER OF BITS PER DATA VALUE EXCEEDS
!*                              WORD LENGTH, OR MAXIMUM ALLOWED.
!*                          -2, OUTPUT ARRAY TOO SMALL TO HOLD CODED
!*                              DATA VALUES.
!*                          -3, CURRENTLY UNDEFINED OPTION SELECTED.
!*                          -4, MISSING DATA FIELD CONTAINS NON-ZERO.
!*                          -5, INVALID COMPLEX PACKING MODE.
!*                          -6, INVALID LAPLACIAN SCALING FACTOR.
!*
!*                           1, INVALID ORIGINATING CENTRE.
!*                           2, INVALID MODEL IDENTIFICATION.
!*                           3, INVALID GRID DEFINITION.
!*                           4, INVALID BLOCK INDICATOR FLAG
!*                           5, INVALID PARAMETER IDENTIFIER.
!*                           6, INVALID LEVEL TYPE INDICATOR.
!*                           7, ERROR IN DESCRIPTION OF LEVELS.
!*                           8, DATE/TIME ERROR.
!*                           9, INVALID TIME UNIT OR RANGE.
!*                          10, INVALID DATA REPRESENTATION TYPE.
!*                          11, INVALID EXTREME POINT/INCREMENT FLAG.
!*                          12, INVALID SCANNING MODE FLAGS.
!*                          13, INVALID COMPLEX PACKING MODE.
!*
!*                PMIN    - MINIMUM VALUE OF THE FIELD DATA.
!*
!*                PMAX    - MAXIMUM VALUE OF THE FIELD DATA.
!*
!*    JOHN HENNESSY , ECMWF , NOVEMBER 1985
!*
!*    Modifications by Mats HAMRUD, ECMWF, to handle complex packing of
!*    spectral harmonics data, 1988.
!*
!*    Modifications by Jean CLOCHARD, French DMN, January 1990:
!*
!*    -  to remove automatic arrays introduced by the last modification
!*     ( *** BUT INPUT ARRAY PFDATA IS OVERWRITTEN AGAIN *** );
!*    -  to comply with the "DOCTOR" norm;
!*    -  to use (standard case) as reference value the highest 32-bit
!*       value not exceeding the minimum of the field, this reference
!*       value being used to compute departures from this value;
!*    -  to allow modifications of the packing for ARPEGE files use,
!*       in an optional way.
!*
!********************************************************************
!
!
!
!
!
!
      IMPLICIT NONE
!
      INTEGER (KIND=JPLIKM) :: KLENF
      INTEGER (KIND=JPLIKM) :: KBITS
      INTEGER (KIND=JPLIKM) :: KNBIT
      INTEGER (KIND=JPLIKM) :: KLENG
      INTEGER (KIND=JPLIKM) :: KWORD
      INTEGER (KIND=JPLIKM) :: KROUND
      INTEGER (KIND=JPLIKM) :: KCPACK
      INTEGER (KIND=JPLIKM) :: KSCALP
      INTEGER (KIND=JPLIKM) :: KERR
      INTEGER (KIND=JPLIKM) :: KLENV
!
      REAL (KIND=JPDBLD) :: PFDATA(*)
      REAL (KIND=JPDBLD) :: PVERT(KLENV)
!
      REAL (KIND=JPDBLD) :: PMIN
      REAL (KIND=JPDBLD) :: PMAX
!
      LOGICAL :: LDARPE
!
      INTEGER (KIND=JPLIKB) :: KGRIB(KLENG)
      INTEGER (KIND=JPLIKM) :: KB1PAR(19)
      INTEGER (KIND=JPLIKM) :: KB2PAR(17)
!
      INTEGER (KIND=JPLIKM) :: IMAX, ILENF, IMISS
      INTEGER (KIND=JPLIKM) :: J, IBYTE, INVAL, IOFF, ITEMP, IERR
      INTEGER (KIND=JPLIKM) :: IERY, IERD, IERM, IERH, IERN
      INTEGER (KIND=JPLIKM) :: I, IEXP, IMANT, IPW, IPB
      INTEGER (KIND=JPLIKM) :: IREP, IFLAG, ICPACK, ILEN
      INTEGER (KIND=JPLIKM) :: ISCALE, ISCALX, ISTPA, IRESTE
      INTEGER (KIND=JPLIKM) :: ISCALP, IAUXIL, ILENFM, ILBIN
      INTEGER (KIND=JPLIKM) :: IL, ILNIL, IBITS, INUMBI
      INTEGER (KIND=JPLIKB) :: IL8, ILEXP, ILMANT, ILFLAG, ILSCALX
      INTEGER (KIND=JPLIKB) :: ILAUXIL, ILLBIN, ILBITS, ILSTPA, ILSCALP
!
      INTEGER (KIND=JPLIKM) :: IBLOCK(24)
      INTEGER (KIND=JPLIKM) :: ILAT(2)
      INTEGER (KIND=JPLIKB) :: ILBLOCK(24), ILB2PAR(17), ILLAT(2)
!
      REAL (KIND=JPDBLD) :: ZCOEFF, ZEPSIL, ZAUXIL, ZSCALE, ZS, ZREFER
      REAL (KIND=JPDBLD) :: ZAUXI2
!
!     SET MAXIMUM NUMBER OF BITS PER DATA FIELD.
!
      SAVE IMAX
      DATA IMAX /60/
!
!     CLEAR ERROR INDICATOR.
!
      REAL(KIND=JPRB) :: ZHOOK_HANDLE
      IF (LHOOK) CALL DR_HOOK('CODEGA',0,ZHOOK_HANDLE)
      KERR = 0
!
!
!
!
!
!********************************************************************
!*
!*    CHECK FOR MISSING DATA INDICATORS.
!*
!********************************************************************
!
!
      IF (KLENF.LT.0)                                                   &
     &   THEN
             ILENF = - KLENF
             IMISS = 1
!
             DO 50 J=1,ILENF
!
                IF (PFDATA(J).NE.0.0_JPDBLD) THEN
                       KERR = -4
                       WRITE (*,9012)
 9012                  FORMAT (TR1,'NON-ZERO VALUE IN MISSING DATA',    &
     &                          ' FIELD.')
                       IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
                       RETURN
                   ENDIF
   50        CONTINUE
!
         ELSE
             ILENF = KLENF
             IMISS = 0
         ENDIF
!
!
!
!
!********************************************************************
!*
!*    CHECK NUMBER OF BITS PER DATA FIELD.
!*
!********************************************************************
!
!
      IF (KBITS.GT.KNBIT.OR.KBITS.GT.IMAX)                              &
     &    THEN
              KERR = -1
              WRITE (*,9000) KBITS,KNBIT,IMAX
 9000         FORMAT (TR1,'NUMBER OF BITS PER DATA VALUE, ',I3,         &
     &                'EXCEEDS WORD LENGTH, ',I3,' OR MAXIMUM ',        &
     &                ' PERMITTED VALUE, ',I3)
              IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
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
!
!***
!*    THE LETTERS G R I B ARE INSERTED IN THE
!*    FIRST 4 ELEMENTS OF 'IBLOCK'.
!***
!
      IBLOCK(1) = 71
      IBLOCK(2) = 82
      IBLOCK(3) = 73
      IBLOCK(4) = 66
!
      KWORD = 1
      IBYTE = 8
      INVAL  = 4
      IOFF  = 0
!
!     PACK 4 8-BIT FIELDS IN BLOCK 0 OF CODED ARRAY.
!
      ILBLOCK=IBLOCK
      CALL GSBYTE_MF (KGRIB(KWORD),ILBLOCK(1),IOFF,IBYTE,0,INVAL,       &
     &             KNBIT,'C',KLENG,KERR,KWORD,.TRUE.)
      IF (KERR.NE.0)  THEN
        IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
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
!*    BLOCK 1 - PRODUCT DEFINITION BLOCK.
!*
!********************************************************************
!
!
!
!***
!*    LENGTH OF BLOCK 1 (IN OCTETS) IN FIRST 3 OCTETS.
!***
!
      IBLOCK(1) = 0
      IBLOCK(2) = 0
      IBLOCK(3) = 24
!
!
!
!
!***
!*    FOURTH OCTET RESERVED AND SET TO 0.
!***
!
      IBLOCK(4) = 0
!
!
!
!
!
!
!***
!*    IDENTIFICATION OF CENTRE
!***
      IBLOCK(5) = KB1PAR(1)
      IF (KB1PAR(1).LT.1.OR.KB1PAR(1).GT.98)                            &
     &   THEN
             KERR = 1
             WRITE (*,9001) KB1PAR(1)
 9001        FORMAT (TR1,'INVALID ORIGINATING CENTRE ',I3)
             IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
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
!*    MODEL IDENTIFICATION
!***
!
      IBLOCK(6) = KB1PAR(2)
      IF (KB1PAR(2).LT.1.OR.KB1PAR(2).GT.255)                           &
     &   THEN
             KERR = 2
             WRITE (*,9002) KB1PAR(2)
 9002        FORMAT (TR1,'INVALID MODEL IDENTIFICATION ',I4)
             IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
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
!***
!*    GRID DEFINITION
!***
!
      IBLOCK(7) = KB1PAR(3)
      IF (KB1PAR(3).LT.1.OR.KB1PAR(3).GT.255)                           &
     &   THEN
             KERR = 3
             WRITE (*,9003) KB1PAR(3)
 9003        FORMAT (TR1,'INVALID GRID IDENTIFICATION ',I4)
             IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
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
!***
!*    OPTIONAL BLOCK INDICATOR FLAGS ( CODE TABLE 1 ).
!***
!
!     BLOCKS INCLUDED    BINARY VALUE      DECIMAL VALUE
!
!         NONE             00000000               0
!          2               10000000             128
!          3               01000000              64
!         2 AND 3          11000000             192
!
      IBLOCK(8) = KB1PAR(4)
!
!     SHIFT 2-BIT FLAG FIELD RIGHT BY 6 BITS TO FACILITATE CHECKING
!
      ITEMP = KB1PAR(4) / 64
!
      IF (ITEMP.LT.0.OR.ITEMP.GT.3)    KERR = 4
!
!     CHECK IF GRID SPECIFIED IS UNCATALOGUED.
!
      IF (KB1PAR(3).EQ.255.AND.KB1PAR(4).EQ.0)  KERR = 4
      IF (KB1PAR(3).EQ.255.AND.KB1PAR(4).EQ.64) KERR = 4
!
      IF (KERR.NE.0) THEN
           CALL PRTBIN_MF (KB1PAR(4),8,ITEMP,IERR)
           WRITE (*,9004) ITEMP
 9004      FORMAT (TR1,'INVALID BLOCK INDICATOR FLAG ',I8.8)
           IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
           RETURN
      ENDIF
!
!
!
!
!***
!*    PARAMETER IDENTIFIER ( CODE TABLE 2 ).
!***
!
      IBLOCK(9) = KB1PAR(5)
      IF (KB1PAR(5).LT.0.OR.KB1PAR(5).GT.255)                           &
     &   THEN
             KERR = 5
             WRITE (*,9005) KB1PAR(5)
 9005        FORMAT (TR1,'INVALID PARAMETER ',I4)
             IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
             RETURN
         ENDIF
!
!
!
!
!***
!*    INDICATOR OF LEVEL TYPE ( CODE TABLE 3 ).
!***
!
      IBLOCK(10) = KB1PAR(6)
!
!*    CODE FIGURES FOR LEVEL TYPE RANGE FROM 0 - 210, WITH 200
!     BEING USED FOR PSEUDO-LEVELS.
!
      IF (KB1PAR(6).LT.0.OR.KB1PAR(6).GT.210) KERR = 6
      IF (KERR.EQ.6)                                                    &
     &   THEN
             WRITE (*,9006) KB1PAR(6)
 9006        FORMAT (TR1,'INVALID LEVEL TYPE ',I4)
             IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
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
!*    DESCRIPTION OF LEVEL OR LAYER ( CODE TABLE 3 ).
!***
!
!
!
!     FOR CERTAIN SPECIAL LEVELS NO FURTHER DESCRIPTION IS
!     NECESSARY, AND THE FIELDS ARE SET TO 0.
!
      IF (KB1PAR(6).LT.100.OR.KB1PAR(6).EQ.102)                         &
     &    THEN
              KB1PAR(7) = 0
              KB1PAR(8) = 0
          ENDIF
!
      IBLOCK(11) = KB1PAR(7)
      IBLOCK(12) = KB1PAR(8)
!
!     CERTAIN LEVEL TYPES REQUIRE THAT THE DESCRIPTION OCCUPY
!     BOTH 8 BIT FIELDS.
!
      IF( (KB1PAR(6).NE. 20).AND.                                       &
     &    (KB1PAR(6).NE.100).AND.                                       &
     &    (KB1PAR(6).NE.103).AND.                                       &
     &    (KB1PAR(6).NE.105).AND.                                       &
     &    (KB1PAR(6).NE.107).AND.                                       &
     &    (KB1PAR(6).NE.109).AND.                                       &
     &    (KB1PAR(6).NE.111).AND.                                       &
     &    (KB1PAR(6).NE.113).AND.                                       &
     &    (KB1PAR(6).NE.115).AND.                                       &
     &    (KB1PAR(6).NE.117).AND.                                       &
     &    (KB1PAR(6).NE.125).AND.                                       &
     &    (KB1PAR(6).NE.127).AND.                                       &
     &    (KB1PAR(6).NE.160).AND.                                       &
     &    (KB1PAR(6).NE.210) ) THEN
!
              IF (KB1PAR(7).GT.255.OR.KB1PAR(8).GT.255) KERR = 7
          ELSE
!
!             8 LOW ORDER BITS IN IBLOCK(12)
!             HIGH ORDER BITS IN IBLOCK(11)
!
              IBLOCK(12) = IBLOCK(11)
              IBLOCK(11) = IBLOCK(11) / 256
              IBLOCK(12) = IBLOCK(12) - IBLOCK(11) * 256
!
!             CHECK MAXIMUM POSSIBLE IN 16 BITS.
!
              IF (KB1PAR(7).GT.65535) KERR = 7
          ENDIF
!
      IF (KERR.NE.0)                                                    &
     &    THEN
              WRITE (*,9007) KB1PAR(7),KB1PAR(8)
 9007         FORMAT (TR1,'LEVEL DESCRIPTION ERROR ',I8,3X,I8)
              IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
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
!*    DATE AND TIME. CHECK VALID RANGES AND MISSING DATA VALUES.
!***
!
      IBLOCK(13) = KB1PAR(9)
      IERY = 0
      IF (KB1PAR(9).LT.0.OR.KB1PAR(9).GT.99) IERY = 1
      IF (KB1PAR(9).EQ.255) IERY = 0
      IBLOCK(14) = KB1PAR(10)
      IERM = 0
      IF (KB1PAR(10).LT.1.OR.KB1PAR(10).GT.12) IERM = 1
      IF (KB1PAR(10).EQ.255) IERM = 0
      IBLOCK(15) = KB1PAR(11)
      IERD = 0
      IF (KB1PAR(11).LT.1.OR.KB1PAR(11).GT.31) IERD = 1
      IF (KB1PAR(11).EQ.255) IERD = 0
      IBLOCK(16) = KB1PAR(12)
      IERH = 0
      IF (KB1PAR(12).LT.0.OR.KB1PAR(12).GT.23) IERH = 1
      IF (KB1PAR(12).EQ.255) IERH = 0
      IBLOCK(17) = KB1PAR(13)
      IERN = 0
      IF (KB1PAR(13).LT.0.OR.KB1PAR(13).GT.59) IERN = 1
      IF (KB1PAR(13).EQ.255) IERN = 0
!
      KERR = IERY + IERM + IERD + IERH + IERN
!
      IF (KERR.NE.0)                                                    &
     &    THEN
              KERR = 8
              WRITE (*,9008) KB1PAR(9),KB1PAR(10),KB1PAR(11),KB1PAR(12),&
     &                       KB1PAR(13)
 9008         FORMAT (TR1,'INVALID DATE/TIME ',3I2,' / ',2I2)
              IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
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
!***
!*    UNIT OF TIME - ( CODE TABLE 4 ).
!*    TIME RANGE(S) AND TIME RANGE INDICATOR ( CODE TABLE 5 ).
!***
!
!     TIME UNIT.
!
      IBLOCK(18) = KB1PAR(14)
      IF (KB1PAR(14).LT.0.OR.KB1PAR(14).GT.7) KERR = 9
!
!*    TIME UNIT CODES ARE IN THE RANGE 0 - 7, WITH 254 USED FOR SECONDS.
!
      IF (KB1PAR(14).EQ.254) KERR = 0
!
!     THERE CAN BE ONE OR TWO TIME RANGES, EACH IN 1 OCTECT OR
!     ONE TIME RANGE IN 2 OCTECTS.
!
      IF (KB1PAR(17).EQ.0.OR.KB1PAR(17).EQ.1                            &
     & .OR.KB1PAR(17).EQ.10)   KB1PAR(16)  = 0
!
!     CROSS CHECK RANGE VALUES AND FLAG VALUES.
!
      IF (KB1PAR(15).GT.65535.OR.KB1PAR(15).LT.0)   KERR = 9
      IF (KB1PAR(15).GT.255.AND.KB1PAR(17).NE.10)   KERR = 9
      IF (KB1PAR(16).GT.255.OR.KB1PAR(16).LT.0)     KERR = 9
      IF (KB1PAR(17).LT.0.OR.KB1PAR(17).GT.10)      KERR = 9
      IF (KERR.NE.0)                                                    &
     &    THEN
              WRITE (*,9009) KB1PAR(14),KB1PAR(15),KB1PAR(16),KB1PAR(17)
 9009         FORMAT (TR1,'TIME UNIT/TIME 1/TIME 2/INDICATOR ERROR - ', &
     &                I8,2X,'/',I8,2X,'/',I8,2X,'/',I8)
              IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
              RETURN
          ENDIF
!
      IBLOCK(19) = KB1PAR(15)
      IBLOCK(20) = KB1PAR(16)
!
!     ONE TIME RANGE OCCUPYING BOTH OCTETS.
!
      IF (KB1PAR(17).EQ.10)                                             &
     &   THEN
!            8 LOW ORDER BITS IN IBLOCK(20)
!            HIGH ORDER BITS IN IBLOCK(19)
!
             IBLOCK(20) = IBLOCK(19)
             IBLOCK(19) = IBLOCK(19) / 256
             IBLOCK(20) = IBLOCK(20) - IBLOCK(19) * 256
         ENDIF
!
!     TIME RANGE INDICATOR
!
      IBLOCK(21) = KB1PAR(17)
!
!
!
!
!
!
!
!***
!*    NUMBER AVERAGED OR ACCUMULATED.
!***
!
      IBLOCK(22) = 0
      IBLOCK(23) = 0
!
!
!     CHECK CONSISTENCY INDICATOR AND NUMBER FIELD.
!
      IF (KB1PAR(17).EQ.3.AND.KB1PAR(18).EQ.0)                          &
     &   THEN
             KERR = 12
             WRITE (*,9013) KB1PAR(17),KB1PAR(18)
 9013        FORMAT (TR1,'INDICATOR/NUMBER AVERAGED ERROR - ',          &
     &                I8,2X,'/',I8)
             IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
             RETURN
         ENDIF
!
!     VALUE IN TWO OCTETS.
!
      IBLOCK(22) = KB1PAR(18)
!
!     8 LOW ORDER BITS IN IBLOCK(23)
!     HIGH ORDER BITS IN IBLOCK(22)
!
      IBLOCK(23) = IBLOCK(22)
      IBLOCK(22) = IBLOCK(22) / 256
      IBLOCK(23) = IBLOCK(23) - IBLOCK(22) * 256
!
!
!
!
!
!
!***
!*    NUMBER MISSING FROM AVERAGES/ACCUMULATIONS.
!***
!
      IBLOCK(24) = KB1PAR(19)
!
!
!
!
!
!
!
!***
!*
!*    PACK 24 8-BIT FIELDS OF BLOCK 1 IN CODED ARRAY.
!*
!***
!
      IBYTE = 8
      INVAL = 24
!
      ILBLOCK=IBLOCK
      CALL GSBYTE_MF (KGRIB(KWORD),ILBLOCK(1),IOFF,IBYTE,0,INVAL,       &
     &             KNBIT,'C',KLENG,KERR,KWORD,.TRUE.)
      IF (KERR.NE.0)  THEN
        IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
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
!
      IF (KB1PAR(4).LT.128) GOTO 333
!
!
!***
!*    SET ARRAY IBLOCK TO 0 . IT IS USED TO ZERO FILL RESERVED OCTETS.
!***
!
      DO 100 J=1,24
         IBLOCK(J) = 0
  100 CONTINUE
!
!
!
!
!***
!*    LAT/LONGITUDE GRID, GAUSSIAN GRID AND SPHERICAL HARMONICS
!*    ARE THE ONLY DATA REPRESENTATIONS HANDLED.
!***
!
      IF (KB1PAR(4).EQ.128.OR.KB1PAR(4).EQ.192)                         &
     &   THEN
             IF (KB2PAR(1).NE.0.AND.KB2PAR(1).NE.4.AND.KB2PAR(1).NE.50  &
     &           .AND.KB2PAR(1).NE.80)                                  &
     &          THEN
                     WRITE (*,*)'GRID DESCRIPTION BLOCK NOT YET DEFINED'
                     KERR = -3
                     IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
                     RETURN
                ENDIF
!
!
!
!
!***
!*       LENGTH OF GRID DESCRIPTION BLOCK.
!***
!
!        LENGTH IS 32 OCTETS FOR LAT/LONG, GAUSSIAN AND SPHERICAL
!        HARMONICS . FOR ANY DATA  ON HYBRID LEVELS THE
!        VERTICAL COORDINATES ARE ADDED.
!
         IF(KB2PAR(1).EQ.80) THEN
            I=52
         ELSE
            I = 32
         ENDIF
         IF (KB1PAR(6).GT.108) I = I + KLENV * 4
!
         IL8=I
         CALL SBYTE_MF (KGRIB(KWORD),IL8,IOFF,24)
         CALL OFFSET_MF (IOFF,1,KWORD,24,KNBIT,KLENG,KERR)
         IF (KERR.NE.0)  THEN
           IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
           RETURN
         ENDIF
!
!
!
!
!***
!*       NUMBER OF UNUSED BITS AT END OF BLOCK.
!*       CURRENT DEFINITION IMPLIES 0.
!***
!
         ILBLOCK=IBLOCK
         CALL SBYTE_MF (KGRIB(KWORD),ILBLOCK(1),IOFF,8)
         CALL OFFSET_MF (IOFF,1,KWORD,8,KNBIT,KLENG,KERR)
         IF (KERR.NE.0)  THEN
           IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
           RETURN
         ENDIF
!
!
!
!
!
!***
!*       NEXT OCTET IS RESERVED.
!***
!
         ILBLOCK=IBLOCK
         CALL SBYTE_MF (KGRIB(KWORD),ILBLOCK(1),IOFF,8)
         CALL OFFSET_MF (IOFF,1,KWORD,8,KNBIT,KLENG,KERR)
         IF (KERR.NE.0)  THEN
           IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
           RETURN
         ENDIF
!
!
!
!
!
!
!***
!*       DATA REPRESENTATION TYPE.
!***
!
         ILB2PAR=KB2PAR
         CALL SBYTE_MF (KGRIB(KWORD),ILB2PAR(1),IOFF,8)
         CALL OFFSET_MF (IOFF,1,KWORD,8,KNBIT,KLENG,KERR)
         IF (KERR.NE.0)  THEN
           IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
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
!*       LAT/LONG OR GAUSSIAN GRID.
!***
!
         IF (KB2PAR(1).EQ.0.OR.KB2PAR(1).EQ.4)                          &
     &    THEN
!
!            NUMBER OF LAT/LONG POINTS.
!
             ILB2PAR=KB2PAR
             CALL GSBYTE_MF (KGRIB(KWORD),ILB2PAR(2),IOFF,16,0,2,       &
     &                    KNBIT,'C',KLENG,KERR,KWORD,.TRUE.)
             IF (KERR.NE.0)  THEN
               IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
               RETURN
             ENDIF
!
!            LAT/LONG OF ORIGIN.
!            SIGN BIT SET TO 1 IF VALUES ARE NEGATIVE.
!
             DO 200 J=1,2
!
             IF (KB2PAR(J+3).GE.0) THEN
                ILAT(J) = KB2PAR(J+3)
             ELSE
                ILAT(J) = 2**23 - KB2PAR(J+3)
             ENDIF
!
  200        CONTINUE
!
             ILBLOCK=IBLOCK
             CALL GSBYTE_MF (KGRIB(KWORD),ILBLOCK(1),IOFF,24,0,2,       &
     &                    KNBIT,'C',KLENG,KERR,KWORD,.TRUE.)
             IF (KERR.NE.0)  THEN
               IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
               RETURN
             ENDIF
!
!            RESOLUTION FLAG.
!
!            INCREMENTS       BINARY CODE     DECIMAL VALUE
!
!             NOT GIVEN        00000000              0
!              GIVEN           10000000            128
!
!
!            SHIFT 1 BIT FLAG FIELD TO LOW ORDER BIT
!
             ITEMP = KB2PAR(6) / 128
!
             IF (ITEMP.LE.0.OR.ITEMP.GT.1)                              &
     &          THEN
                    CALL PRTBIN_MF (KB2PAR(6),8,ITEMP,IERR)
                    WRITE (*,9011) ITEMP
 9011               FORMAT (TR1,'INVALID RESOLUTION FLAG ',I8.8)
                    KERR = 11
                    IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
                    RETURN
                ENDIF
!
!            SET TO ALL 1-BITS UNUSED INCREMENT FIELDS.
!
             IF (KB2PAR(6).EQ.0)                                        &
     &          THEN
!                   1111111111111111 BINARY = 65535 DECIMAL
                    KB2PAR(9)  = 65535
                    KB2PAR(10) = 65535
                ENDIF
!
             ILB2PAR=KB2PAR
             CALL SBYTE_MF (KGRIB(KWORD),ILB2PAR(6),IOFF,8)
             CALL OFFSET_MF (IOFF,1,KWORD,8,KNBIT,KLENG,KERR)
             IF (KERR.NE.0)  THEN
               IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
               RETURN
             ENDIF
!
!            LAT/LONG OF EXTREME POINT.
!
!            SIGN BIT SET TO 1 IF VALUES ARE NEGATIVE.
!
             DO 300 J=1,2
!
             IF (KB2PAR(J+6).GE.0) THEN
                ILAT(J) = KB2PAR(J+6)
             ELSE
                ILAT(J) = 2**23 - KB2PAR(J+6)
             ENDIF
!
  300        CONTINUE
!
             ILLAT=ILAT
             CALL GSBYTE_MF (KGRIB(KWORD),ILLAT(1),IOFF,24,0,2,         &
     &                    KNBIT,'C',KLENG,KERR,KWORD,.TRUE.)
             IF (KERR.NE.0)  THEN
               IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
               RETURN
             ENDIF
!
!
!
!            DIRECTION INCREMENTS / NUMBER OF LATITUDE LINES.
!
             ILB2PAR=KB2PAR
             CALL GSBYTE_MF (KGRIB(KWORD),ILB2PAR(9),IOFF,16,0,2,       &
     &                    KNBIT,'C',KLENG,KERR,KWORD,.TRUE.)
             IF (KERR.NE.0)  THEN
               IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
               RETURN
             ENDIF
!
!            SCANNING MODE FLAGS.
!
!            VALID VALUES       VALUE / 32
!             BINARY             DECIMAL
!
!              00000000               0
!              10000000               4
!              01000000               2
!              11000000               6
!              00100000               1
!              10100000               5
!              01100000               3
!              11100000               7
!
!            SHIFT 3 BIT FLAG FIELD TO LOW ORDER BITS.
!
             ITEMP = KB2PAR(11) / 32
!
             IF (ITEMP.LT.0.OR.ITEMP.GT.7)                              &
     &          THEN
                    CALL PRTBIN_MF (KB2PAR(11),8,ITEMP,IERR)
                    WRITE (*,9014) ITEMP
 9014               FORMAT (TR1,'INVALID SCANNING MODE FLAGS ',I8.8)
                    KERR = 12
                    IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
                    RETURN
                ENDIF
!
             ILB2PAR=KB2PAR
             CALL SBYTE_MF (KGRIB(KWORD),ILB2PAR(11),IOFF,8)
             CALL OFFSET_MF (IOFF,1,KWORD,8,KNBIT,KLENG,KERR)
             IF (KERR.NE.0)  THEN
               IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
               RETURN
             ENDIF
!
!            4 RESERVED OCTETS.
!
             ILBLOCK=IBLOCK
             CALL GSBYTE_MF (KGRIB(KWORD),ILBLOCK(1),IOFF,8,0,4,        &
     &                    KNBIT,'C',KLENG,KERR,KWORD,.TRUE.)
             IF (KERR.NE.0)  THEN
               IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
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
!***
!*       SPHERICAL HARMONIC DATA.
!***
!
         IF (KB2PAR(1).EQ.50.OR.KB2PAR(1).EQ.80)                        &
     &    THEN
!
!            PENTAGONAL RESOLUTION PARAMETERS.
!
             ILB2PAR=KB2PAR
             CALL GSBYTE_MF (KGRIB(KWORD),ILB2PAR(2),IOFF,16,0,3,       &
     &                    KNBIT,'C',KLENG,KERR,KWORD,.TRUE.)
             IF (KERR.NE.0)  THEN
               IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
               RETURN
             ENDIF
!
!            REPRESENTATION TYPE AND MODE.
!
             ILB2PAR=KB2PAR
             CALL GSBYTE_MF (KGRIB(KWORD),ILB2PAR(5),IOFF,8,0,2,        &
     &                    KNBIT,'C',KLENG,KERR,KWORD,.TRUE.)
             IF (KERR.NE.0)  THEN
               IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
               RETURN
             ENDIF
!
!            18 RESERVED OCTETS.
!
             ILBLOCK=IBLOCK
             CALL GSBYTE_MF (KGRIB(KWORD),ILBLOCK(1),IOFF,8,0,18,       &
     &                    KNBIT,'C',KLENG,KERR,KWORD,.TRUE.)
             IF (KERR.NE.0)  THEN
               IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
               RETURN
             ENDIF
!
          ENDIF
!
!
!***
!*    ADD PARAMETERS NEEDED TO DESCRIBE STRETCHED AND ROTATED
!*    SPHERICAL HARMONICS DATA
!***
!
      IF(KB2PAR(1).EQ.80) THEN
!
!*    POLE OF ROTATION
!
             DO 310 J=1,2
!
             IF (KB2PAR(J+11).GE.0) THEN
                ILAT(J) = KB2PAR(J+11)
             ELSE
                ILAT(J) = 2**23 - KB2PAR(J+11)
             ENDIF
!
  310        CONTINUE
!
             ILLAT=ILAT
             CALL GSBYTE_MF (KGRIB(KWORD),ILLAT(1),IOFF,24,0,2,         &
     &                    KNBIT,'C',KLENG,KERR,KWORD,.TRUE.)
             IF (KERR.NE.0)  THEN
               IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
               RETURN
             ENDIF
!
!*    ANGLE OF ROTATION
             ILB2PAR=KB2PAR
             CALL CONFP_MF (ILB2PAR(14),IEXP,IMANT)
             ILEXP=IEXP
             CALL SBYTE_MF (KGRIB(KWORD),ILEXP,IOFF,8)
             CALL OFFSET_MF (IOFF,1,KWORD,8,KNBIT,KLENG,KERR)
             IF (KERR.NE.0)  THEN
               IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
               RETURN
             ENDIF
             ILMANT=IMANT
             CALL SBYTE_MF (KGRIB(KWORD),ILMANT,IOFF,24)
             CALL OFFSET_MF (IOFF,1,KWORD,24,KNBIT,KLENG,KERR)
             IF (KERR.NE.0)  THEN
               IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
               RETURN
             ENDIF
!
!*    POLE OF STRETCHING
!
             DO 320 J=1,2
!
             IF (KB2PAR(J+14).GE.0) THEN
                ILAT(J) = KB2PAR(J+14)
             ELSE
                ILAT(J) = 2**23 - KB2PAR(J+14)
             ENDIF
!
  320        CONTINUE
             ILLAT=ILAT
             CALL GSBYTE_MF (KGRIB(KWORD),ILLAT(1),IOFF,24,0,2,         &
     &                    KNBIT,'C',KLENG,KERR,KWORD,.TRUE.)
             IF (KERR.NE.0)  THEN
               IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
               RETURN
             ENDIF
!
!*    STRETCHING  FACTOR
             ILB2PAR=KB2PAR
             CALL CONFP_MF (ILB2PAR(17),IEXP,IMANT)
             ILEXP=IEXP
             CALL SBYTE_MF (KGRIB(KWORD),IEXP,IOFF,8)
             CALL OFFSET_MF (IOFF,1,KWORD,8,KNBIT,KLENG,KERR)
             IF (KERR.NE.0)  THEN
               IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
               RETURN
             ENDIF
             ILMANT=IMANT
             CALL SBYTE_MF (KGRIB(KWORD),ILMANT,IOFF,24)
             CALL OFFSET_MF (IOFF,1,KWORD,24,KNBIT,KLENG,KERR)
             IF (KERR.NE.0)  THEN
               IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
               RETURN
             ENDIF
         ENDIF
!
!
!
!***
!*       ADD VERTICAL COORDINATE PARAMETERS FOR HYBRID LEVELS.
!***
!
         IF (KB1PAR(6).GT.108) THEN
!
             DO 400 J=1,KLENV
                 CALL CONFP_MF (PVERT(J),IEXP,IMANT)
                 ILEXP=IEXP
                 CALL SBYTE_MF (KGRIB(KWORD),ILEXP,IOFF,8)
                 CALL OFFSET_MF (IOFF,1,KWORD,8,KNBIT,KLENG,KERR)
                 IF (KERR.NE.0)  THEN
                   IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
                   RETURN
                 ENDIF
                 ILMANT=IMANT
                 CALL SBYTE_MF (KGRIB(KWORD),ILMANT,IOFF,24)
                 CALL OFFSET_MF (IOFF,1,KWORD,24,KNBIT,KLENG,KERR)
                 IF (KERR.NE.0)  THEN
                   IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
                   RETURN
                 ENDIF
  400        CONTINUE
!
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
  333 CONTINUE
!
!
!********************************************************************
!*
!*    BLOCK 3 (IF REQUIRED) - BIT MAP BLOCK.
!*
!********************************************************************
!
      IF (KB1PAR(4).EQ.64.OR.KB1PAR(4).EQ.192)                          &
     &     THEN
               WRITE (*,*)'BIT MAP BLOCK NOT YET DEFINED'
               KERR = -3
               IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
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
!*    BLOCK 4 - BINARY DATA BLOCK.
!*
!********************************************************************
!
!
!***
!*    RETAIN POINTERS TO WORD AND BIT POSITION OF BINARY DATA
!*    BLOCK LENGTH FIELD. ENTER LENGTH AS 0.
!***
!
      IPW = KWORD
      IPB = IOFF
!
      ILBLOCK=IBLOCK
      CALL SBYTE_MF (KGRIB(KWORD),ILBLOCK(1),IOFF,24)
      CALL OFFSET_MF (IOFF,1,KWORD,24,KNBIT,KLENG,KERR)
      IF (KERR.NE.0)  THEN
        IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
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
!
!
!
!***
!*    4 BIT FLAG / 4 BIT COUNT OF UNUSED BITS AT END OF BLOCK OCTET.
!***
!
!     FLAG IS 1000 FOR SPHERICAL HARMONICS, 0000 FOR LAT/LONG
!     OR GAUSSIAN GRID, 1100 FOR SPHERICAL HARM. COMPLEX PACKING
!
      IREP = 0
      IFLAG=0
      ICPACK=KCPACK
!
      IF (KB2PAR(1).EQ.50.OR.KB2PAR(1).EQ.80) THEN
         IFLAG=128
         IREP=1
!
         IF (ICPACK.LT.0) THEN
           WRITE (*,*) 'CODEGA : COMPLEX PACKING CODE ERROR'
           KERR = -5
           IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
           RETURN
         ELSEIF (IABS (KSCALP).GE.2**15) THEN
           WRITE (*,*) 'CODEGA : LAPLACIAN SCALING FACTOR ERROR'
           KERR = -6
           IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
           RETURN
         ENDIF
!
         IF(ICPACK.NE.0.AND.KB2PAR(6).EQ.2) THEN
            IFLAG=IFLAG+64
            IREP=(ICPACK+1)**2
         ELSE
            ICPACK=0
         ENDIF
!
      ENDIF
!
      ILFLAG=IFLAG
      CALL SBYTE_MF (KGRIB(KWORD),ILFLAG,IOFF,8)
      CALL OFFSET_MF (IOFF,1,KWORD,8,KNBIT,KLENG,KERR)
      IF (KERR.NE.0)  THEN
        IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
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
!
!
!
!***
!*    FIND MAXIMUM AND MINIMUM VALUES IN DATA ARRAY. FOR
!*    DATA IN SPHERICAL HARMONIC FORM THE FIRST WORD CONTAINS
!*    THE REAL (0,0)COEFFICIENT, WHICH IS TREATED SEPARATELY.
!*    FOR COMPLEX PACKING AND SPECTRAL DATA REPRESENTATION MODE=2
!*    THE FIRST (ICPACK+1)**2 COEFFICENTS ARE NOT PACKED
!***
!
!
      ILEN = ILENF - IREP
      CALL MXMN_MF (PFDATA(IREP+1),ILEN,PMAX,PMIN)
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
!***
!*    COMPUTE REFERENCE VALUE AND SCALE FACTOR.
!***
!
      IF (LDARPE) THEN
!
        ZCOEFF = PMAX - PMIN
!RJ: unsafe and can underflow
        ZEPSIL=1.E-290_JPDBLD
!RJ        ZEPSIL=EPSILON(ZEPSIL)*2**N
!
        IF ( ZCOEFF .LE. ZEPSIL ) THEN
          ZAUXIL=MIN ( ABS (PMIN), ABS (PMAX) )
          IF ( ZAUXIL .LE. ZEPSIL ) ZAUXIL=0.0_JPDBLD
!
          PMAX=SIGN (ZAUXIL,PMAX)
          PMIN=PMAX
          ZSCALE=0.0_JPDBLD
        ELSE
          ZSCALE=REAL(2**KBITS-1,KIND=JPDBLD) / ZCOEFF
        ENDIF
!
!             Scale factor and reference value forced to zero.
!
        ISCALE=0
        ZREFER=PMIN
        IEXP=0
        IMANT=0
!
      ELSEIF (IMISS.EQ.1) THEN
!
!           Scaling factor,
!           EXPONENT AND MANTISSA SET TO ALL 1-BITS FOR MISSING DATA.
!
        ISCALE = 65535
        IEXP   = 255
        IMANT  = 16777215
        ZREFER = PMIN
!
      ELSE
!
!     CONVERT MINIMUM VALUE (PMIN) TO GRIB FORMAT (IEXP,IMANT).
!
!     Its image decoded back is being used as reference value to compute
!     (in PACKGB) normalized integer field values that will be
!     chained into the binary string.
!
        CALL CONFI (PMIN,IEXP,IMANT,ZREFER)
!
        ZS = (PMAX-ZREFER)/REAL(2**(KBITS+1)-1,KIND=JPDBLD)
        ZAUXIL=1.0_JPDBLD
        ZAUXI2=2.0_JPDBLD
!
!     CONFI may return ZREFER > PMAX if the range
!     of PMAX-PMIN is smaller than the GRIB accuracy
!
        IF (ZS.GT.0.0_JPDBLD) THEN
           ZS = LOG(ZS)/LOG(ZAUXI2) + ZAUXI2
        ELSE
           ZS=0.0_JPDBLD
        ENDIF
        ISCALE = MIN(INT(ZS),INT(ZS+SIGN(ZAUXIL,ZS)))
!
!      Absolute value of ISCALE is limited, to avoid problems due to
!      fields constant except on a few points.
!
        ISCALE = MAX(-99,MIN(99,ISCALE))
        ZSCALE = ZAUXI2** (-ISCALE)
!
      ENDIF
!
!
!
!
!     SET SIGN BIT (BIT 16) AND PUT SCALE FACTOR INTO REMAINING
!     15 BITS OF CODED 16 BIT FIELD.
!
      IF (ISCALE.GE.0) THEN
        ISCALX = ISCALE
      ELSE
        ISCALX= 2**15 - ISCALE
      ENDIF
!
      ILSCALX=ISCALX
      CALL SBYTE_MF (KGRIB(KWORD),ILSCALX,IOFF,16)
      CALL OFFSET_MF (IOFF,1,KWORD,16,KNBIT,KLENG,KERR)
      IF (KERR.NE.0)  THEN
        IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
        RETURN
      ENDIF
!
!
!
!
!
      IF (IEXP.EQ.0.AND.IMANT.EQ.0) THEN
        ILMANT=IMANT
        CALL SBYTE_MF (KGRIB(KWORD),ILMANT,IOFF,32)
        CALL OFFSET_MF (IOFF,1,KWORD,32,KNBIT,KLENG,KERR)
      ELSE
        ILEXP=IEXP
        CALL SBYTE_MF (KGRIB(KWORD),ILEXP,IOFF,8)
        CALL OFFSET_MF (IOFF,1,KWORD,8,KNBIT,KLENG,KERR)
        IF (KERR.NE.0)  THEN
          IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
          RETURN
        ENDIF
        ILMANT=IMANT
        CALL SBYTE_MF (KGRIB(KWORD),ILMANT,IOFF,24)
        CALL OFFSET_MF (IOFF,1,KWORD,24,KNBIT,KLENG,KERR)
      ENDIF
!
      IF (KERR.NE.0)  THEN
        IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
        RETURN
      ENDIF
!
!
!
!
!***
!*    NUMBER OF BITS IN EACH DATA VALUE.
!***
!
      ILBITS=KBITS
      CALL SBYTE_MF (KGRIB(KWORD),ILBITS,IOFF,8)
      CALL OFFSET_MF (IOFF,1,KWORD,8,KNBIT,KLENG,KERR)
      IF (KERR.NE.0)  THEN
        IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
        RETURN
      ENDIF
!
!
!
!
!***
!*    IF COMPLEX PACKING DESCRIBE PACKING
!***
!
      IF (IREP.NE.0.AND.ICPACK.NE.0) THEN
!
!     START OF PACKED DATA
!
      ISTPA=19+IREP*4
!
! Removing of the padding (06 sept 2000, not correct and useless)
!
!      IRESTE=MOD (IOFF+40+IREP*32,KNBIT)
!C
!      IF (IRESTE.NE.0) THEN
!C
!        IF ( MOD (KNBIT,KBITS).EQ.0 .AND. MOD (IRESTE,8).EQ.0 .AND.
!     S       MOD (KBITS,IRESTE).NE.0 ) THEN
!C
!C        Packed data will start on a word boundary,
!C     and each packed data will be into a single word.
!C
!          ISTPA=ISTPA+IRESTE/8
!        ENDIF
!C
!      ENDIF
!
      ILSTPA=ISTPA
      CALL SBYTE_MF (KGRIB(KWORD),ILSTPA,IOFF,16)
      CALL OFFSET_MF (IOFF,1,KWORD,16,KNBIT,KLENG,KERR)
      IF (KERR.NE.0)  THEN
        IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
        RETURN
      ENDIF
!
!     SCALING FACTOR.
!
      IF (KSCALP.GE.0) THEN
        ISCALP=KSCALP
      ELSE
        ISCALP=2**15-KSCALP
      ENDIF
!
      ILSCALP=ISCALP
      CALL SBYTE_MF (KGRIB(KWORD),ILSCALP,IOFF,16)
      CALL OFFSET_MF (IOFF,1,KWORD,16,KNBIT,KLENG,KERR)
      IF (KERR.NE.0)  THEN
        IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
        RETURN
      ENDIF
!
!     TRUNCATION FOR UNPACKED PART OF SPECTRAL DATA, CURRENTLY
!     ONLY TRIANGULAR TRUNCATION SUPPORTED
!
      IAUXIL=ICPACK*(1+2**8*(1+2**8))
      ILAUXIL=IAUXIL
      CALL SBYTE_MF (KGRIB(KWORD),ILAUXIL,IOFF,24)
      CALL OFFSET_MF (IOFF,1,KWORD,24,KNBIT,KLENG,KERR)
      IF (KERR.NE.0)  THEN
        IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
        RETURN
      ENDIF
!
      ENDIF
!***
!*    IF SPHERICAL HARMONICS DATA, NEXT 4 OCTETS ARE DIFFERENT FORMAT.
!***
!
      IF (IREP.NE.0)                                                    &
     &    THEN
!
!            STORE IREP COEFFICIENTS IN FLOATING
!            POINT FORM.
!
        IF (LDARPE) THEN
!
!                THE "UNPACKED" PART IS FILLED WITH ZEROES.
!
          IEXP=0
          IMANT=0
!
          DO 510 J=1,IREP
          ILMANT=IMANT
          CALL SBYTE_MF (KGRIB(KWORD),ILMANT,IOFF,32)
          CALL OFFSET_MF (IOFF,1,KWORD,32,KNBIT,KLENG,KERR)
          IF (KERR.NE.0)  THEN
            IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
            RETURN
          ENDIF
  510     CONTINUE
!
        ELSE
!
!                STANDARD CASE.
!
             DO 520 J=1,IREP
             CALL CONFP_MF (PFDATA(J),IEXP,IMANT)
!
             ILEXP=IEXP
             CALL SBYTE_MF (KGRIB(KWORD),ILEXP,IOFF,8)
             CALL OFFSET_MF (IOFF,1,KWORD,8,KNBIT,KLENG,KERR)
             IF (KERR.NE.0)  THEN
               IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
               RETURN
             ENDIF
             ILMANT=IMANT
             CALL SBYTE_MF (KGRIB(KWORD),ILMANT,IOFF,24)
             CALL OFFSET_MF (IOFF,1,KWORD,24,KNBIT,KLENG,KERR)
             IF (KERR.NE.0)  THEN
               IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
               RETURN
             ENDIF
 520         CONTINUE
!
          ENDIF
!
        ENDIF
!
!
!
!
!***
!*    SCALE AND STORE DATA VALUES.
!***
!
!     DO 600 J = IREP+1 , ILENF
!        IPDATA    = NINT ( (PFDATA(J)-ZREFER) * ZSCALE )
!        PFDATA(J)=OR (IPDATA,0)
! 600 CONTINUE
!
      ILENFM = ILENF - IREP
      CALL PACKGB (PFDATA(IREP+1),PFDATA(IREP+1),ZREFER,ZSCALE,ILENFM)
      CALL GSBYTE_MF (KGRIB(KWORD),PFDATA(IREP+1),IOFF,KBITS,0,ILENFM,  &
     &             KNBIT,'C',KLENG,KERR,KWORD,.TRUE.)
      IF (KERR.NE.0)  THEN
        IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
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
!
!
!
!***
!*    ENTER LENGTH OF BINARY DATA BLOCK, HAVING ENSURED THAT
!*    THE LENGTH IS AN EVEN NUMBER OF OCTETS.
!***
!
!     LENGTH OF BINARY DATA BLOCK IN BITS.
!
      ILBIN = (KWORD-IPW) * KNBIT + IOFF - IPB
!
      IL = MOD (ILBIN,16)
!
!     FILL UNUSED PORTION OF LAST 2 OCTETS WITH BINARY ZEROES.
!
      ILNIL = 0
!
      IF (IL.NE.0) THEN
              ILNIL = 16 - IL
              ILBLOCK=IBLOCK
              CALL SBYTE_MF (KGRIB(KWORD),ILBLOCK(1),IOFF,ILNIL)
              CALL OFFSET_MF (IOFF,1,KWORD,ILNIL,KNBIT,KLENG,KERR)
              IF (KERR.NE.0)  THEN
                IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
                RETURN
              ENDIF
          ENDIF
!
      ILBIN = (KWORD-IPW) * KNBIT + IOFF - IPB
!
!     ENTER LENGTH - IN OCTETS - OF DATA BLOCK.
!
      ILBIN = ILBIN / 8
      ILLBIN=ILBIN
      CALL SBYTE_MF (KGRIB(IPW),ILLBIN,IPB,24)
      CALL OFFSET_MF (IPB,1,IPW,24,KNBIT,KLENG,KERR)
!
!     ENTER NUMBER OF UNUSED BITS IN FLAG/BIT COUNT FIELD.
!
      IFLAG = IFLAG + ILNIL
      ILFLAG=IFLAG
      CALL SBYTE_MF (KGRIB(IPW),ILFLAG,IPB,8)
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
!
!
!
!********************************************************************
!*
!*    BLOCK 5 - END BLOCK.
!*
!********************************************************************
!
!
!***
!*    ADD 7 7 7 7 TO CODED DATA.
!***
!
      IBLOCK(1) = 55
      IBLOCK(2) = 55
      IBLOCK(3) = 55
      IBLOCK(4) = 55
!
      ILBLOCK=IBLOCK
      CALL GSBYTE_MF (KGRIB(KWORD),ILBLOCK(1),IOFF,8,0,4,               &
     &             KNBIT,'C',KLENG,KERR,KWORD,.FALSE.)
      IF (KERR.NE.0)  THEN
        IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
        RETURN
      ENDIF
!
!
!***
!*    SET ANY UNUSED PART OF LAST WORD TO BINARY ZEROES.
!***
!
      IF (IOFF.NE.KNBIT)                                                &
     &   THEN
             IBITS = KNBIT - IOFF
             ILBLOCK=IBLOCK
             CALL SBYTE_MF (KGRIB(KWORD),ILBLOCK(5),IOFF,IBITS)
         ENDIF
!
!
!
!***
!*    ROUND TO 120 OCTETS, IF REQUIRED.
!***
!
      IF (KROUND.EQ.1)                                                  &
     &   THEN
             INUMBI = KWORD * KNBIT
             I = INUMBI / 960
             I = I * 960
             I = INUMBI - I
             IF (I.NE.0) I = (960 - I) / KNBIT
!
             DO 700 J=KWORD+1,KWORD+I
                KGRIB(J) = 0
  700        CONTINUE
!
             KWORD = KWORD + I
         ENDIF
!
      IF (LHOOK) CALL DR_HOOK('CODEGA',1,ZHOOK_HANDLE)
      ENDSUBROUTINE CODEGA
