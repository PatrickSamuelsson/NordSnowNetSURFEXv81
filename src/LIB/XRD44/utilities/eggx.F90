SUBROUTINE EGGX (PRPI, PRA, KROTEQ, PLONR, PLATR, PBETA,&
 & PLON1, PLAT1, PLON2, PLAT2, PLON0, PLAT0, PRPK, KULOUT,&
 & KSOTRP, KGIV0,&
 & PGELAM, PGELAT, PGM, PGNORX, PGNORY,&
 & KDLSA, KDLSUR, KDGSA, KDGEN, KDLUN, KDLUX, KDGUN, KDGUX,&
 & PDELX, PDELY)  
!****
!----------------------------------------------------------------------

!     GEOGRAPHY OF GRID-POINTS
!     ARPEGE-ALADIN
!     -------------------------

!       -------------------------------------------------------
!     PURPOSE
!     -------
!      PROVIDES THE BASIC GEOGRAPHICAL PARAMETERS OF EACH GRID-POINT
!      IN THE WINDOW OF INTERNAL+COUPLING ZONE, DEFINING THE
!      GEOGRAPHICAL DOMAIN OF INTEREST.
!      SIMULTANEOUSLY, INITIALISES THE INTERNAL GEOGRAPHICAL COMMON
!      /YEMGGCM/ FOR POSSIBLE RE-USE OF POSITION INVERSION SUBROUTINE
!      EGGRVS.

!      THE GEOGRAPHICAL PARAMETERS ARE :
!       - GEOGRAPHIC LONGITUDE
!       - GEOGRAPHIC LATITUDE
!       - MAP FACTOR
!       - COMPONENTS OF VECTOR DIRECTED TOWARDS THE GEOGRAPHIC NORTH
!       POLE FOR PROJECTION OF VECTORS
!       - X GRID-SIZE (DISTANCE OR LONGITUDE INCREMENT)
!       - Y GRID-SIZE (DISTANCE OR LATITUDE INCREMENT)
!      THE LAST TWO DEPEND ON THE GEOMETRY. THE LATTER CAN BE
!       - SPHERICAL GEOMETRY, WITH ROTATION OF DOMAIN TO THE EQUATOR
!       - CARTESIAN GEOMETRY, WITH PROJECTION OF DOMAIN ON A PLANE

!      ROTATION AND PROJECTION CAN BE COMBINED. HOWEVER, PROJECTIONS
!      OTHER THAN MERCATOR SHOULD BE USED WITHOUT ROTATION.

!      EGGX CAN RUN UNDER FULL USER CONTROL OR IN MORE OR LESS
!      AUTOMATIC MODES

!      FROM THESE BASIC PARAMETERS, OTHER CAN EASILY BE DEDUCED, SUCH
!      AS THE CORIOLIS PARAMETER, ETC.

!      (GENERAL WARNING ABOUT AUTOMATIC MODE)
!      --------------------------------------
!       NOTE THAT AUTOMATIC MODE IS NOT SURE TO CONVERGE, AND MOREOVER,
!      IT MAY CONVERGE TOWARD A SOLUTION DIFFERENT FROM WHAT YOU EXPECTED
!      THIS IS SO IN PARTICULAR FOR A DOMAIN OVER THE POLE OR NEAR THE
!      EQUATOR. IT IS RECOMMENDED TO CHECK CAREFULLY A CHOICE OF
!      PARAMETERS BEFORE RUNNING A COMPLETE SUITE

!      (CONVENTIONS FOR LATITUDE, LONGITUDE)
!      -------------------------------------
!       THEY ARE IN RADIANS
!       LATITUDE VARY FROM  -PI/2  (SOUTH POLE) TO  PI/2  (NORTH POLE)
!       LONGITUDE VARY FROM  0 (GREENWICH OR EQUIVALENT) TO 2*PI
!         THE CONVENTION FOR A POLE IS THAT IT HAS LONGITUDE 0

!      (HANDLING OF PROJECTION IN SOUTHERN HEMISPHERE)
!      ------------------------------------------------
!       IT IS ASSUMED THAT DOMAINS NOT ROTATED AND ENTERELY CONTAINED
!       IN THE SOUTHERN HEMISPHERE WILL BE PROJECTED USING STEREO/LAMBERT

!       IN THAT CASE, AND IN THAT CASE ONLY,
!         HSUD IS SET TO -1. IN THE INTERNAL COMMON

!       THE SIGN OF LATITUDES IS CHANGED FOR ALL INTERNAL CALCULATIONS,
!       SO THAT ALL THE TRIGONOMETRY IS UNCHANGED, HOWEVER, THE RESPECTIVE
!       POSITIONS OF POLE AND GRID ARE REVERSED

!       WHEREVER THIS DIFFERENCE MATTERS, THAT IS:
!       - MAPPING POLAR COORDINATES TO (X,Y)
!       - VECTOR ROTATION
!       ALTERNATIVE LINES OR MULTIPLICATIONS BY HSUD ARE INCLUDED

!       THE SIGNS OF OUTGOING OR INCOMING LATITUDES ARE CHANGED AT THE LAST
!        MINUTE OR PRIOR TO ANY COMPUTATION

!      (CONVENTIONS FOR ARRAY ORGANISATION)
!      ------------------------------------
!       THE POINT (KDLUN,KDGUN) HAS GEOGRAPHIC COORDINATES (PLON1,PLAT1)
!       THE POINT (KDLUX,KDGUX)                            (PLON2,PLAT2)
!        THE FIRST INDEX (X DIRECTION) VARIES ROUGHLY
!        WITH INCREASING LONGITUDE
!        THE SECOND INDEX (Y DIRECTION) VARIES
!        ROUGHLY WITH INCREASING LATITUDE
!        THEY ARE ACTUAL LONGITUDE, LATITUDE ONLY IN THE CASE WITHOUT PROJ.

!     ONLY A PART OF THE ARRAYS IS INITIALIZED : THAT PART CORRESPONDING
!     TO INTERNAL + COUPLING ZONES. IT MIGHT BE NECESSARY TO COMPLETE
!     THE ARRAYS (SUCH AS GM) IN ORDER TO MAKE THEM DOUBLY-PERIODIC.

!     ONE TYPE OF ROTATION AND THREE TYPES OF PROJECTIONS ARE RECOGNISED

!      (CONVENTIONS FOR WIND AND OTHER VECTORS ROTATION)
!      --------------------------------------------------
!       GEOGRAPHIC U = PGNORY * U PROJ - PGNORX * V PROJ
!       GEOGRAPHIC V = PGNORX * U PROJ + PGNORY * V PROJ

!       WHEN USING THIS TRANSFORM, DO NOT FORGET THE MAP FACTOR EFFECT

!       THE INVERSE TRANSFORM IS EASY TO DEDUCE (IT IS A ROTATION)

!       (OVERVIEW OF ALGORITHM)
!       -----------------------
!     PARAMETERS ARE EDITED  AND POSSIBLY CHECKED
!     THEY ARE OPTIONNALLY ROTATED
!     SOME AUTOMATIC ADJUSTMENTS OF PROJECTION PARAMETER ARE OPTIONALLY
!     PERFORMED
!     PROJECTION IS PERFORMED (OPTIONNALLY) ON THE ROTATED SPHERE
!     AT THIS STAGE, INCREMENTS OR DISTANCE ON THE MAP ARE DEDUCED
!     COMPONENTS OF ROTATION UNDER PROJECTION ARE COMPUTED
!     INVERSE ROTATION BACK TO GEOGRAPHICAL VALUES IS OPTIONNALLY DONE
!     ON THE RESULTS, COMPONENTS OF ROTATION ARE COMPOSED WITH THOSE
!     OF PROJECTION

!     INPUT PARAMETERS
!     ----------------
!      PRPI : PI (3.14ETC AS GENERALLY DEFINED ELSEWHERE : YOMCST IN ARPEGE)
!      PRA  : A, RADIUS OF SPHERICAL PLANET (M)
!       WARNING :
!       ---------  DEPENDING ON OPTIONS, INPUT PARAMETERS MAY BECOME OUTPUT
!                  PARAMETERS. EVEN IF UNITIALISED, THE VARIABLES MUST BE
!                  DECLARE INDEPENDENTLY.

!      KROTEQ = 0 : NO ROTATION
!             = 1 : POINT (PLONR,PLATR) IS ROTATED TO EQUATOR, THE NORTH POLE
!                   IS ON THE NEW GREENWICH MERIDIAN

!      PLONR : GEOGRAPHIC LONGITUDE OF REFERENCE POINT OF ROTATION
!      PLATR : GEOGRAPHIC LATITUDE OF REFERENCE POINT OF ROTATION

!      PBETA : ANGLE (IN RD) BETWEEN X-AXIS AND ROTATED LATITUDE CIRCLES
!              AT THE REFERENCE LONGITUDE
!              (USUALLY, PBETA = 0. : GIVES PURE PROJECTIONS)

!      PLON1, PLAT1 : GEOGRAPHIC LONGITUDE, LATITUDE OF THE SOUTH-WEST
!                     CORNER OF USEFUL DOMAIN
!      PLON2, PLAT2 : GEOGRAPHIC LONGITUDE, LATITUDE OF THE NORTH-EAST
!                     CORNER OF USEFUL DOMAIN
!       IF ROTATION IS REQUIRED, (PLON1,PLAT1) IS SELF DETERMINED
!       FROM (PLONR,PLATR) AND (PLON2,PLAT2) WHEN KSOTRP = 2
!       IF PROJECTION IS REQUIRED, PLAT1 IS SEFL DETERMINED WHEN
!       KSOTRP = 2. PLON1 MUST BE GIVEN.

!       IF ROTATION IS REQUIRED, (PLON2,PLAT2) IS SELF DETERMINED
!       FROM (PLONR,PLATR) AND (PLON1,PLAT1) WHEN KSOTRP = 1
!       IF PROJECTION IS REQUIRED, PLAT2 IS SEFL DETERMINED WHEN
!       KSOTRP = 1. PLON2 MUST BE GIVEN.

!      PLON0 : GEOGRAPHIC LONGITUDE OF REFERENCE FOR THE PROJECTION
!              (THE VERTICAL ONE IN STEREO/LAMBERT)
!        WARNING :
!        ---------  STRANGE RESULTS MAIN OCCUR IF PLON0 IS OUT OF THE
!                   DOMAIN
!        PLON0 IS NOT REQUIRED WHEN KGIV0 =1 OR 3
!      PLAT0 : GEOGRAPHIC LATITUDE OF REFERENCE FOR THE PROJECTION
!              (WHERE M = 1)
!        PLAT0 IS NOT REQUIRED WHEN KGIV0 = 2 OR 3
!      PRPK  : PROJECTION PARAMETER AND DEFINITION
!              PRPK = 10. PROJECTION TYPE SELF DETERMINED
!                         BY MINIMIZING THE VARIATION OF THE MAP FACTOR
!              PRPK = 1.  POLAR STEREOGRAPHIC PROJECTION
!              0. < PRPK < 1.  LAMBERT CONFORMAL PROJECTION WITH
!                              CONE PARAMETER PRPK
!              PRPK = 0.  MERCATOR CONFORMAL PROJECTION
!              PRPK < 0.  NO PROJECTION
!             ON OUTPUT, PRPK CONTAINS THE EFFECTIVE PROJECTION
!             PARAMETER THAT HAS BEEN USED
!      KSOTRP : ISOTROPY PARAMETER UNDER PROJECTION
!               = 0, PLAT1, PLAT2 USED AS GIVEN. GRID SPACING ISOTROPY
!                    GENERALLY NOT AVAILABLE ( PDELX DIFFERENT FORM PDELY )
!               = 1, GRID SPACING ISOTROPIC, PLAT1 USED AS GIVEN,
!                    PLAT2 CHANGED (PLON2 SELF DETERMINED IF KROTEQ = 1)
!               = 2, GRID SPACING ISOTROPIC, PLAT2 USED AS GIVEN,
!                    PLAT1 CHANGED (PLON1 SELF DETERMINED IF KROTEQ = 1)

!      KGIV0 : CHOICE OF REFERENCE POINT FOR PROJECTION
!               = 0, PLAT0 AND PLON0 USED AS GIVEN
!               = 1, PLAT0 REQUIRED, PLON0 SELF DETERMINED
!               = 2, PLON0 REQUIRED, PLAT0 SELF DETERMINED
!               = 3, PLAT0 AND PLON0 SELF DETERMINED

!      KDLSA:KDLSUR : LOWER AND UPPER FIRST DIMENSIONS OF ARRAYS (X)
!      KDGSA:KDGEN  : LOWER AND UPPER SECOND DIMENSIONS OF ARRAYS (Y)

!      KDLUN:KDLUX  : LOWER AND UPPER FIRST DIMENSIONS OF
!                     THE DOMAIN OF INTEREST, WHERE ARRAYS ARE
!                     INITIALIZED.
!      KDGUN:KDGUX  : LOWER AND UPPER SECOND DIMENSIONS OF
!                     THE DOMAIN OF INTEREST, WHERE ARRAYS ARE
!                     INITIALIZED.
!                   TOGETHER WITH THE CORNERS OF THE DOMAIN, THESE
!                   DEFINE THE GRID RESOLUTION

!      KULOUT : UNIT OF OUTPUT FILE

!     IMPLICIT INPUT
!     --------------
!       NONE
!       THIS IS AN ENVIRONMENT INDEPENDENT SUBROUTINE

!     OTHER INPUT  OR EXTERNALS
!     -------------------------
!       ALL MANNER OF TRIGONOMETRIC FUNCTIONS AND LOGARITHMS
!       CALLS EGGMLT TO COMPUTE MISSING LATITUDE WHEN KSOTRP > 0
!       CALLS EGGRVS TO DEDUCE GEOGRAPHIC PARAMETERS FROM FINAL
!       PROJECTION AND ROTATION PARAMETERS.

!       AFTER A FIRST CALL TO EGGX, ROUTINE EGGRVS CAN BE RE-CALLED
!       EXTERNALLY WITH DIFFERENT (PROJECTED OR) ROTATED POSITIONS

!     OUTPUT PARAMETERS
!     -----------------
!       PGELAM(KDLSA:KDLSUR,KDGSA:KDGEN) :
!                          GEOGRAPHIC LONGITUDE
!       PGELAT(KDLSA:KDLSUR,KDGSA:KDGEN) :
!                          GEOGRAPHIC LATITUDE
!       PGM(KDLSA:KDLSUR,KDGSA:KDGSUR) : MAP FACTOR
!       PGNORX(KDLSA:KDLSUR,KDGSA:KDGEN) :
!              PROJECTION OF GEOGRAPHICAL NORTH UNIT VECTOR ON X AXIS
!       PGNORY(KDLSA:KDLSUR,KDGSA:KDGEN) :
!              PROJECTION OF GEOGRAPHICAL NORTH UNIT VECTOR ON Y AXIS

!       PDELX : GRID SIZE IN M ALONG X IF PROJECTION
!               LONGITUDE INCREMENT IN RD IF SPHERICAL GEOMETRY
!               NECESSARY TO COMPUTE PERIOD AND DERIVATIVES
!       PDELY : GRID SIZE IN M ALONG Y IF PROJECTION
!               LATITUDE INCREMENT IN RD IF SPEHRICAL GEOMETRY

!     WRITTEN BY
!     ---------- ALAIN JOLY

!     NEW NORTHERN HEMISPHERE VERSION : 27/2/92
!     SOUTHERN HEM ISPHERE VERSION : 27/1/93

!---------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK

USE YEMGGCM  , ONLY : NYMGGI   ,NYMGGR   ,NYMGGWH  ,XLATR    ,&
 & XLONR    ,XGGPK    ,XRPKSM   ,XLAT0R   ,XLON0R   ,&
 & XLON0U   ,XIPORE   ,XJPORE   ,XGGM0    ,XLON1R   ,&
 & XLON1U   ,XLAT1R   ,XLON2R   ,HSUD     ,XBETA  

!---------------------------------------------------------------------

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KDLSA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KDLSUR 
INTEGER(KIND=JPIM),INTENT(IN)    :: KDGSA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KDGEN 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRPI
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRA
INTEGER(KIND=JPIM),INTENT(IN)    :: KROTEQ 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PLONR 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PLATR 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PBETA 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PLON1 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PLAT1 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PLON2 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PLAT2 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PLON0 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PLAT0 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PRPK
INTEGER(KIND=JPIM),INTENT(IN)    :: KULOUT
INTEGER(KIND=JPIM),INTENT(INOUT) :: KSOTRP
INTEGER(KIND=JPIM),INTENT(IN)    :: KGIV0 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PGELAM(KDLSA:KDLSUR,KDGSA:KDGEN)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PGELAT(KDLSA:KDLSUR,KDGSA:KDGEN)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PGM(KDLSA:KDLSUR,KDGSA:KDGEN)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PGNORX(KDLSA:KDLSUR,KDGSA:KDGEN)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PGNORY(KDLSA:KDLSUR,KDGSA:KDGEN)
INTEGER(KIND=JPIM),INTENT(IN)    :: KDLUN
INTEGER(KIND=JPIM),INTENT(IN)    :: KDLUX
INTEGER(KIND=JPIM),INTENT(IN)    :: KDGUN
INTEGER(KIND=JPIM),INTENT(IN)    :: KDGUX
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDELX
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDELY

!---------------------------------------------------------------------

INTEGER(KIND=JPIM) :: INBESS, INNEGA, ISPECA, ITERK, ITERKX, JLAT, JLON
INTEGER(KIND=JPIM) :: ISOTRP

LOGICAL :: LLGWH, LL510, LL520
LOGICAL :: LLPLANEX
LOGICAL :: LLPLANEY

REAL(KIND=JPRB) :: ZCONDEG, ZCONRAD, ZCOSA, ZCOSO, ZDCLA0, ZDCLA1,&
 & ZDCLA2, ZDLON, ZDM, ZDMMAX, ZDMMIN, ZDRK, &
 & ZDTLAT, ZFACE, ZIPV, ZJPV, ZLAT, ZLAT2R, &
 & ZLATLIM, ZLON, ZLON2U, ZPIS2, ZPIS4, ZRKAX, &
 & ZRKI, ZRKII, ZRKIN, ZRKOLD, ZRKT, ZRPK, ZSECAN, &
 & ZSECUR, ZSINA, ZSINO, ZUSKP  
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!---------------------------------------------------------------------

#include "eggmlt.h"
#include "eggrvs.h"

#include "abor1.intfb.h"

!---------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('EGGX',0,ZHOOK_HANDLE)
!---------------------------------------------------------------------

ZPIS2 = PRPI*0.5_JPRB
ZPIS4 = PRPI*0.25_JPRB
ZSECUR = 1.E-12_JPRB
ZSECAN = 1.E-05_JPRB

!*
!---------------------------------------------------------------------
!     1.- PRINTING INPUT PARAMETERS
ZCONRAD = PRPI/180._JPRB
ZCONDEG = 180._JPRB/PRPI

WRITE (KULOUT,*) ' '
WRITE (KULOUT,*) ' ---------------------------------- '
WRITE (KULOUT,*) ' '
WRITE (KULOUT,*) '      ARPEGE-ALADIN '
WRITE (KULOUT,*) ' '
WRITE (KULOUT,*) '    GEOGRAPHY OF GRID-POINTS '
WRITE (KULOUT,*) ' '
WRITE (KULOUT,*) ' ---------------------------------- '
WRITE (KULOUT,*) ' '
WRITE (KULOUT,*) ' INPUT PARAMETERS '
WRITE (KULOUT,*) ' '
WRITE (KULOUT,*) ' PI = ',PRPI
WRITE (KULOUT,*) ' RADIUS OF PLANET A = ',PRA*1.E-03_JPRB,' KM '
WRITE (KULOUT,*) ' '
WRITE (KULOUT,*) ' X-SIZE OF ARRAYS ',KDLSUR-KDLSA+1
WRITE (KULOUT,*) ' Y-SIZE OF ARRAYS ',KDGEN-KDGSA+1
WRITE (KULOUT,*) ' '
WRITE (KULOUT,*) ' X WINDOW KDLUN = ',KDLUN,' KDLUX = ',KDLUX
WRITE (KULOUT,*) '          SIZE = ',KDLUX-KDLUN+1
WRITE (KULOUT,*) ' Y WINDOW KDGUN = ',KDGUN,' KDGUX = ',KDGUX
WRITE (KULOUT,*) '          SIZE = ',KDGUX-KDGUN+1
WRITE (KULOUT,*) ' '
WRITE (KULOUT,*) ' ROTATION PARAMETER KROTEQ = ',KROTEQ
WRITE (KULOUT,*) '          PLONR = ',PLONR
WRITE (KULOUT,*) '          PLATR = ',PLATR
WRITE (KULOUT,*) ' '
WRITE (KULOUT,*) ' ANGLE WITH X/LATITUDE AT PLON0 = ',PBETA
WRITE (KULOUT,*) ' '
WRITE (KULOUT,*) ' SW CORNER      PLON1 = ',PLON1
WRITE (KULOUT,*) '                PLAT1 = ',PLAT1
WRITE (KULOUT,*) ' '
WRITE (KULOUT,*) ' NE CORNER      PLON2 = ',PLON2
WRITE (KULOUT,*) '                PLAT2 = ',PLAT2
WRITE (KULOUT,*) ' '
WRITE (KULOUT,*) ' PROJECTION PARAMETER PRPK = ',PRPK
WRITE (KULOUT,*) ' REF POINT      PLON0 = ',PLON0
WRITE (KULOUT,*) '                PLAT0 = ',PLAT0
WRITE (KULOUT,*) ' '
WRITE (KULOUT,*) ' ISOTROPY PARAMETER KSOTRP = ',KSOTRP
WRITE (KULOUT,*) ' '
WRITE (KULOUT,*) ' PROJECTION REF POINT KGIV0 = ',KGIV0
WRITE (KULOUT,*) ' '
WRITE (KULOUT,*) ' ---------------------------------- '
WRITE (KULOUT,*) ' '

!* have a look whether we calculate plane model version
LLPLANEX = KDLUN == KDLUX
LLPLANEY = KDGUN == KDGUX

IF( LLPLANEX.OR.LLPLANEY )THEN
  ISOTRP = KSOTRP
  KSOTRP = 0
  WRITE (KULOUT,*) '!'
  WRITE (KULOUT,*) 'YOU RUN PLANE MODEL'
  WRITE (KULOUT,*) 'KSOTRP LOCALLY RESET TO 0 (ISOTROPIC PARAMETER) '
  WRITE (KULOUT,*) 'ISOTROPY PARAMETER KSOTRP = ',KSOTRP
  WRITE (KULOUT,*) '!'
ENDIF

!     SECURITY CORRECTIONS
IF ( KROTEQ == 0 ) THEN
  PLONR = 0.0_JPRB
  PLATR = 0.0_JPRB
ENDIF
IF ( PRPK < 0.0_JPRB ) THEN
  PLON0 = PLONR
  PLAT0 = PLATR
ENDIF
!     CHECK LONGITUDES
IF ( KROTEQ == 1 ) THEN
  IF ( PLONR < 0.0_JPRB ) THEN
    PLONR = PLONR + 2.0_JPRB*PRPI
    WRITE (KULOUT,*) ' *** EGGX ERROR *** WRONG CONVENTION',&
     & ' USED FOR LONGITUDES '  
    WRITE (KULOUT,*) ' *** NEW PLONR = ',PLONR
  ENDIF
ENDIF
IF ( PLON1 < 0.0_JPRB ) THEN
  PLON1 = PLON1 + 2.0_JPRB*PRPI
  WRITE (KULOUT,*) ' *** EGGX ERROR *** WRONG CONVENTION',&
   & ' USED FOR LONGITUDES '  
  WRITE (KULOUT,*) ' *** NEW PLON1 = ',PLON1
ENDIF
IF ( PLON2 < 0.0_JPRB ) THEN
  PLON2 = PLON2 + 2.0_JPRB*PRPI
  WRITE (KULOUT,*) ' *** EGGX ERROR *** WRONG CONVENTION',&
   & ' USED FOR LONGITUDES '  
  WRITE (KULOUT,*) ' *** NEW PLON2 = ',PLON2
ENDIF
IF ( KGIV0 == 0.OR. KGIV0 == 2 ) THEN
  IF ( PLON0 < 0.0_JPRB ) THEN
    PLON0 = PLON0 + 2.0_JPRB*PRPI
    WRITE (KULOUT,*) ' *** EGGX ERROR *** WRONG CONVENTION',&
     & ' USED FOR LONGITUDES '  
    WRITE (KULOUT,*) ' *** NEW PLON0 = ',PLON0
  ENDIF
ENDIF

NYMGGR = KROTEQ
XLATR = PLATR
XLONR = PLONR
XBETA = PBETA
!*
!--------------------------------------------------------------------
!     2.- ROTATION TO EQUATOR

HSUD = 1.0_JPRB
IF ( KROTEQ == 0 ) THEN
  ! SOUTH HEMISPHERE DETECTION
  IF ( PRPK == 10._JPRB .OR. (PRPK > 0.0_JPRB .AND. PRPK <= 1.0_JPRB) ) THEN
    ! WHEN PROJECTION IS UNKNOWN, IF ONE OF THE REQUESTED LATITUDE IS
    ! NEGATIVE, A GUESS FOR THE OTHER MUST BE PROVIDED : SO BOTH PLAT1
    ! AND PLAT2 ARE ASSUMED KNOWN
    IF ( KSOTRP == 0 ) THEN
      IF ( PLAT1 < 0.0_JPRB .AND. PLAT2 < 0.0_JPRB ) THEN
        HSUD = -1.0_JPRB
        PLAT1 = ABS(PLAT1)
        PLAT2 = ABS(PLAT2)
      ENDIF
    ELSEIF (KSOTRP == 1 ) THEN
      IF ( PLAT1 < 0.0_JPRB ) THEN
        IF ( ZPIS2 >= PLAT2.AND. PLAT2 >= -ZPIS2 ) THEN
          IF ( PLAT2 < 0.0_JPRB ) THEN
            HSUD = -1.0_JPRB
            PLAT1 = ABS(PLAT1)
            PLAT2 = ABS(PLAT2)
          ELSEIF ( PRPK == 10._JPRB .AND. PLAT2 >= 0.0_JPRB ) THEN
            PRPK = 0.0_JPRB
          ELSE
            WRITE (KULOUT,*) ' *** EGGX WARNING *** ',&
             & ' YOU SHOULD USE MERCATOR PROJECTION '  
          ENDIF
        ELSEIF ( PRPK /= 10._JPRB ) THEN
          HSUD = -1.0_JPRB
          PLAT1 = ABS(PLAT1)
        ELSE
          WRITE (KULOUT,*) ' *** EGGX ERROR *** ',&
           & ' REFERENCE POLE (HSUD) CANNOT BE DECIDED '  
          WRITE (KULOUT,*) ' RERUN WITH A REASONABLE GUESS ',' FOR PLAT2 '
          CALL ABOR1(' EGGX: abor1 2.1')
        ENDIF
      ENDIF

    ELSEIF ( KSOTRP == 2 ) THEN
      IF ( PLAT2 < 0.0_JPRB ) THEN
        IF ( ZPIS2 >= PLAT1.AND. PLAT1 >= -ZPIS2 ) THEN
          IF ( PLAT1 < 0.0_JPRB ) THEN
            HSUD = -1.0_JPRB
            PLAT1 = ABS(PLAT1)
            PLAT2 = ABS(PLAT2)
          ELSEIF ( PLAT1 >= 0.0_JPRB ) THEN
            WRITE (KULOUT,*) ' *** EGGX ERROR *** ',&
             & ' PLAT1 CANNOT BE GREATER THAN PLAT2 '  
            WRITE (KULOUT,*) ' RERUN WITH A REASONABLE GUESS ',' FOR PLAT1 '
            CALL ABOR1(' EGGX: abor1 2.2')
          ENDIF
        ELSEIF ( PRPK /= 10._JPRB ) THEN
          HSUD = -1.0_JPRB
          PLAT2 = ABS(PLAT2)
        ELSE
          WRITE (KULOUT,*) ' *** EGGX ERROR *** ',&
           & ' REFERENCE POLE (HSUD) CANNOT BE DECIDED '  
          WRITE (KULOUT,*) ' RERUN WITH A REASONABLE GUESS ',' FOR PLAT1 '
          CALL ABOR1(' EGGX: abor1 2.3')
        ENDIF
      ENDIF
    ENDIF

  ENDIF

  IF ( HSUD < 0.0_JPRB ) THEN
    PLAT0 = ABS( PLAT0 )
  ENDIF

  ! NO ROTATION
  XLON1R =MOD(PLON1,2.0_JPRB*PRPI)
  IF ( KSOTRP /= 2 ) XLAT1R = PLAT1
  XLON2R = MOD(PLON2,2.0_JPRB*PRPI)
  IF ( KSOTRP /= 1 ) ZLAT2R = PLAT2
  IF ( KGIV0 == 0.OR. KGIV0 == 2 ) XLON0R = PLON0
  IF ( KGIV0 == 0.OR. KGIV0 == 1 ) XLAT0R = PLAT0

ELSE
  ! ROTATION
  IF ( KSOTRP == 0.OR. KSOTRP == 1 ) THEN
    ! ROTATION OF SW CORNER
    ! ----------------------
    ZSINA = COS( PLATR )*SIN( PLAT1 )&
     & - SIN( PLATR )*COS( PLAT1 )*COS( PLON1-PLONR )  
    XLAT1R = ASIN( ZSINA )
    IF ( ABS( XLAT1R ) >= ZPIS2 ) THEN
      XLON1R = 0.0_JPRB
    ELSE
      ZCOSA = COS( XLAT1R )
      ZCOSO = ( SIN( PLATR )*SIN( PLAT1 ) +&
       & COS( PLATR )*COS( PLAT1 )*COS( PLON1-PLONR ) )/ZCOSA  
      ZCOSO = MIN(1.0_JPRB,MAX(-1.0_JPRB,ZCOSO))
      ZSINO = ( COS( PLAT1 )*SIN( PLON1-PLONR ) )/ZCOSA
      ZSINO = MIN(1.0_JPRB,MAX(-1.0_JPRB,ZSINO))
      XLON1R = ACOS( ZCOSO )
      IF ( ASIN( ZSINO ) < 0.0_JPRB ) XLON1R = 2.0_JPRB*PRPI - XLON1R
    ENDIF
  ENDIF

  IF ( KSOTRP == 0.OR. KSOTRP == 2 ) THEN
    ! ROTATION OF NE CORNER
    ! ----------------------
    ZSINA = COS( PLATR )*SIN( PLAT2 )&
     & - SIN( PLATR )*COS( PLAT2 )*COS( PLON2-PLONR )  
    ZLAT2R = ASIN( ZSINA )
    IF ( ABS( ZLAT2R ) >= ZPIS2 ) THEN
      XLON2R = 0.0_JPRB
    ELSE
      ZCOSA = COS( ZLAT2R )
      ZCOSO = ( SIN( PLATR )*SIN( PLAT2 ) +&
       & COS( PLATR )*COS( PLAT2 )*COS( PLON2-PLONR ) )/ZCOSA  
      ZCOSO = MIN(1.0_JPRB,MAX(-1.0_JPRB,ZCOSO))
      ZSINO = ( COS( PLAT2 )*SIN( PLON2-PLONR ) )/ZCOSA
      ZSINO = MIN(1.0_JPRB,MAX(-1.0_JPRB,ZSINO))
      XLON2R = ACOS( ZCOSO )
      IF ( ASIN( ZSINO ) < 0.0_JPRB ) XLON2R = 2.0_JPRB*PRPI - XLON2R
    ENDIF
  ENDIF

  !     ROTATION OF PROJECTION REFERENCE POINT
  !     --------------------------------------
  IF ( KGIV0 == 2 ) THEN
    WRITE (KULOUT,*) ' *** EGGX ERROR '
    WRITE (KULOUT,*) ' KGIV0 = 2 IMPOSSIBLE WITH KROTEQ = 1 '
    CALL ABOR1(' EGGX: abor1 2.4')
  ENDIF
  IF ( KGIV0 == 0.OR. KGIV0 == 1 ) THEN
    ZSINA = COS( PLATR )*SIN( PLAT0 )&
     & - SIN( PLATR )*COS( PLAT0 )*COS( PLON0-PLONR )  
    XLAT0R = ASIN( ZSINA )
  ENDIF
  IF ( KGIV0 == 0 ) THEN
    IF ( ABS( XLAT0R ) >= ZPIS2 ) THEN
      XLON0R = 0.0_JPRB
    ELSE
      ZCOSA = MAX( COS( XLAT0R ) , ZSECUR )
      ZCOSO = ( SIN( PLATR )*SIN( PLAT0 ) +&
       & COS( PLATR )*COS( PLAT0 )*COS( PLON0-PLONR ) )/ZCOSA  
      ZCOSO = MIN(1.0_JPRB,MAX(-1.0_JPRB,ZCOSO))
      ZSINO = ( COS( PLAT0 )*SIN( PLON0-PLONR ) )/ZCOSA
      ZSINO = MIN(1.0_JPRB,MAX(-1.0_JPRB,ZSINO))
      XLON0R = ACOS( ZCOSO )
      IF ( ASIN( ZSINO ) < 0.0_JPRB ) XLON0R = 2.0_JPRB*PRPI - XLON0R
    ENDIF
  ENDIF
ENDIF

IF ( KROTEQ /= 0 ) THEN
  IF ( PRPK > 0.0_JPRB ) THEN
    WRITE (KULOUT,*) ' *** EGGX WARNING ',&
     & ' USE OF ROTATION + NON-MERCATOR PROJECTION WILL LEAD ',&
     & ' TO UNPREDICTABLE RESULTS, ESP. IN SOUTH HEMISPHERE '  
    IF ( XLAT1R < 0.0_JPRB .AND. ZLAT2R < 0.0_JPRB ) THEN
      HSUD = -1.0_JPRB
      XLAT1R = ABS( XLAT1R )
      ZLAT2R = ABS( ZLAT2R )
      XLAT0R = ABS( XLAT0R )
    ENDIF
  ENDIF
ENDIF

WRITE (KULOUT,*) ' '
WRITE (KULOUT,*) ' HEMISPHERE INDICATOR HSUD = ',HSUD
WRITE (KULOUT,*) ' '
WRITE (KULOUT,*) ' ROTATED COORDINATES '
WRITE (KULOUT,*) ' SW CORNER      XLON1R = ',XLON1R
WRITE (KULOUT,*) '                XLAT1R = ',XLAT1R
WRITE (KULOUT,*) ' '
WRITE (KULOUT,*) ' NE CORNER      XLON2R = ',XLON2R
WRITE (KULOUT,*) '                ZLAT2R = ',ZLAT2R
WRITE (KULOUT,*) ' '
WRITE (KULOUT,*) ' REF POINT      XLON0R = ',XLON0R
WRITE (KULOUT,*) '                XLAT0R = ',XLAT0R
WRITE (KULOUT,*) ' '

!*
!--------------------------------------------------------------------
!     3.- SPHERICAL GEOMETRY

IF ( PRPK < 0.0_JPRB ) THEN

  !   THE GRID IS MADE UP OF REGULARLY SPACED POINTS IN LONGITUDE (X)
  !   AND LATITUDE (Y) CORRDINATES

  !   PDELX AND PDELY ARE NON DIMENSIONNAL IN THAT CASE

  IF ( KSOTRP == 0 ) THEN
    XLON1U = XLON1R
    ZLON2U = XLON2R
    IF ( ZLON2U < XLON1U ) ZLON2U = 2.0_JPRB*PRPI + ZLON2U
  ENDIF
  !   DEFINES OTHER CORNER
  IF ( KSOTRP == 1 ) THEN
    XLON1U = XLON1R
    ZLAT2R = - XLAT1R
    ZLON2U = XLON1U + ( ZLAT2R-XLAT1R )*&
     & REAL(KDLUX-KDLUN,JPRB)/REAL(KDGUX-KDGUN,JPRB)  
    XLON2R = ZLON2U
    IF ( ZLON2U >= 2.0_JPRB*PRPI ) XLON2R = ZLON2U - 2.0_JPRB*PRPI
  ENDIF
  IF ( KSOTRP == 2 ) THEN
    ZLON2U = XLON2R
    XLAT1R = - ZLAT2R
    XLON1U = ZLON2U - ( ZLAT2R-XLAT1R )*&
     & REAL(KDLUX-KDLUN,JPRB)/REAL(KDGUX-KDGUN,JPRB)  
    XLON1R = XLON1U
    IF ( XLON1U >= 2.0_JPRB*PRPI ) XLON1R = XLON1U - 2.0_JPRB*PRPI
  ENDIF

  IF( .NOT.LLPLANEX.AND..NOT.LLPLANEY )THEN
    PDELX = ( ZLON2U-XLON1U )/REAL( KDLUX-KDLUN ,JPRB)
    PDELY = ( ZLAT2R-XLAT1R )/REAL( KDGUX-KDGUN ,JPRB)
  ELSEIF( LLPLANEX )THEN
    PDELY = ( ZLAT2R-XLAT1R )/REAL( KDGUX-KDGUN )
    PDELX = PDELY
  ELSEIF( LLPLANEY )THEN
    PDELX = ( ZLON2U-XLON1U )/REAL( KDLUX-KDLUN )
    PDELY = PDELX
  ENDIF

  XIPORE=0.0_JPRB
  XJPORE=0.0_JPRB
  XRPKSM=PRPK
  XGGPK = PRPK
  NYMGGI = 10

  DO JLAT = KDGUN, KDGUX
    ZLAT = REAL(JLAT-KDGUN,JPRB)*PDELY

    DO JLON = KDLUN, KDLUX
      ZLON = REAL(JLON-KDLUN,JPRB)*PDELX
      PGELAM(JLON,JLAT) = ZLON
      PGELAT(JLON,JLAT) = ZLAT
    ENDDO

    CALL EGGRVS (PRPI, PRA, PDELX, PDELY, KDLSUR-KDLSA+1,&
     & 1, KDLUX-KDLUN+1, KULOUT,&
     & PGELAM(KDLUN,JLAT), PGELAT(KDLUN,JLAT), PGM(KDLUN,JLAT),&
     & PGNORX(KDLUN,JLAT), PGNORY(KDLUN,JLAT))  
  ENDDO
ENDIF

!*
!--------------------------------------------------------------------
!     4.- MAP FACTOR ON SPHERICAL GEOMETRY

!*
!--------------------------------------------------------------------
!     5.- PREPARATION OF PROJECTION PARAMETERS

IF ( PRPK >= 0.0_JPRB ) THEN

  !   PROJECTION LONGITUDES ARE ALWAYS KNOWN

  !   SPECIAL LOGICAL TRUE WHEN DOMAIN ASTRIDE GREENWICH MERIDIAN
  !    LLGWH = .F. : GREENWICH OUT OF DOMAIN
  !    LLGWH = .T. : GREENWICH WITHIN DOMAIN
  LLGWH = ( XLON2R < XLON1R )
  NYMGGWH = 0
  IF ( LLGWH ) NYMGGWH = 1
  !   SHIFT LONGITUDES WHEN GREENWICH MERIDIAN IS WITHIN THE DOMAIN
  XLON1U = XLON1R
  ZLON2U = XLON2R
  IF ( LLGWH ) THEN
    ZLON2U = XLON2R + 2.0_JPRB*PRPI
  ENDIF

  IF ( KGIV0 == 1.OR. KGIV0 == 3 ) THEN
    IF ( ABS(ABS(ZLON2U-XLON1U)-PRPI) > ZSECAN ) THEN
      WRITE (KULOUT,*) ' NORMAL LONGITUDE DIFFERENCE '
      XLON0U = 0.5_JPRB*( XLON1U + ZLON2U )
      XLON0R = XLON0U
    ELSE
      WRITE (KULOUT,*) ' LONGITUDE DIFFERENCE = PI '
      XLON0U = XLON1U + ZPIS4
      XLON0R = XLON0U
    ENDIF
    IF ( XLON0U >= 2.0_JPRB*PRPI ) XLON0R = XLON0R - 2.0_JPRB*PRPI
    WRITE (KULOUT,*) ' '
    WRITE (KULOUT,*) ' PROJECTION REFERENCE LONGITUDE '
    WRITE (KULOUT,*) ' (ON ROTATED SPHERE) LON0R = ',XLON0R
    WRITE (KULOUT,*) ' GREENWICH LOGICAL = ',LLGWH
    WRITE (KULOUT,*) ' '
  ENDIF

  XLON0U = XLON0R
  IF ( LLGWH .AND. XLON0R < XLON1R ) THEN
    XLON0U = XLON0R + 2.0_JPRB*PRPI
  ENDIF

  !   PROJECTION TYPE IS GIVEN BY THE USER
  !   ------------------------------------

  IF ( PRPK <= 1.0_JPRB ) THEN

    ! DETERMINES MISSING LATITUDE IF ANY
    IF ( KSOTRP >= 1 ) THEN

      ! ADJUSTMENT OF ONE EXTREME LATITUDE IN ORDER TO HAVE PDELX = PDELY
      CALL EGGMLT (PRPI,KDLUX,KDLUN,KDGUX,KDGUN,KULOUT,1,&
       & PRPK,XLON0U,XLON1U,ZLON2U,KSOTRP,XLAT1R,ZLAT2R,&
       & HSUD,XBETA)  

    ENDIF

    ! DETERMINES REFERENCE LATITUDE

    IF ( KGIV0 == 2.OR. KGIV0 == 3 ) THEN
      XLAT0R = 0.5_JPRB*( ZLAT2R + XLAT1R )
      XLAT0R = MIN(ZPIS2,MAX(-ZPIS2,XLAT0R))
      WRITE (KULOUT,*) ' '
      WRITE (KULOUT,*) ' PROJECTION REFERENCE LATITUDE ',&
       & ' (ON ROTATED SPHERE) '  
      WRITE (KULOUT,*) ' LAT0R = ',XLAT0R
      WRITE (KULOUT,*) ' '
    ENDIF

  ENDIF

  !     MAP PROJECTION TYPE AND POSSIBLY ONE CORNER MUST BE DETERMINED
  !     --------------------------------------------------------------
  IF ( PRPK == 10._JPRB ) THEN

    ! EXPLORE VARIATIONS OF MAP FACTOR
    ! --------------------------------

    ZLATLIM = PRPI/6._JPRB
    ! FIRST GUESS : EITHER STEREO OR MERCATOR
    IF ( KSOTRP == 0 ) THEN
      ZRPK = 1.0_JPRB
      IF ( XLAT1R < ZLATLIM ) ZRPK = 0.0_JPRB
    ELSEIF ( KSOTRP == 1 ) THEN
      ZRPK = 1.0_JPRB
      IF ( XLAT1R < ZLATLIM ) ZRPK = 0.0_JPRB
    ELSEIF ( KSOTRP == 2 ) THEN
      ZRPK = 1.0_JPRB
      IF ( ZLAT2R < ZLATLIM ) ZRPK = 0.0_JPRB
    ENDIF

    ! FIRST GUESS POSSIBLE OTHER LATITUDE
    IF ( KSOTRP >= 1 ) THEN
      CALL EGGMLT (PRPI,KDLUX,KDLUN,KDGUX,KDGUN,KULOUT,1,&
       & ZRPK,XLON0U,XLON1U,ZLON2U,KSOTRP,XLAT1R,ZLAT2R,&
       & HSUD,XBETA)  
    ENDIF
    ! FIRST GUESS REFERENCE LATITUDE
    IF ( KGIV0 == 2.OR. KGIV0 == 3 ) THEN
      XLAT0R = 0.5_JPRB*( ZLAT2R + XLAT1R )
      XLAT0R = MIN(ZPIS2,MAX(-ZPIS2,XLAT0R))
    ENDIF

    ! PREPARING FOR OUTER ITERATION LOOP (RK + LATITUDE)
    ITERKX = 10
    ITERK = 0
    ZRKOLD = ZRPK

    LL510=.TRUE.

    DO WHILE(LL510)

      ZRKII = 0.01_JPRB
      ZDRK = 0.01_JPRB
      ZRKI = 0.0_JPRB
      ISPECA = 0

      ! IDENTIFYING SPECIAL CASES THAT MUST GO INTO THE INNER LOOP
      IF ( XLAT1R*ZLAT2R  <=  0.0_JPRB ) THEN
        ! SPECIAL CASE : DOMAIN ASTRIDE EQUATOR
        ZRPK = 0.0_JPRB
        ZRKOLD = ZRPK
        ISPECA = 1
      ELSEIF ( HSUD > 0.0_JPRB .AND.ABS(ZLAT2R) <= ABS(XLAT1R) ) THEN
        ! SPECIAL CASE : DOMAIN INCLUDING NORTH POLE (THE TEST IS A WEAK ONE)
        ZRPK = 1.0_JPRB
        ZRKOLD = ZRPK
        ISPECA = 1
      ELSEIF ( HSUD < 0.0_JPRB .AND.ABS(XLAT1R) <= ABS(ZLAT2R) ) THEN
        ! SPECIAL CASE : DOMAIN INCLUDING SOUTH POLE (THE TEST IS A WEAK ONE)
        ZRPK = 1.0_JPRB
        ZRKOLD = ZRPK
        ISPECA = 1
      ELSEIF ( ABS(ABS(ZLAT2R)-ZPIS2) < ZSECAN ) THEN
        ! VARIOUS OTHER SPECIAL CASES THAT MUST NOT COVER THE WHOLE LOOP
        ZRKI = ZRKII
      ELSEIF ( ABS(ABS(XLAT1R)-ZPIS2) < ZSECAN ) THEN
        ! VARIOUS OTHER SPECIAL CASES THAT MUST NOT COVER THE WHOLE LOOP
        ZRKI = ZRKII
      ENDIF

      IF ( ISPECA /= 1 ) THEN

        ! PREPARE FOR INNER LOOP ON RK
        ZDMMAX = -1.0_JPRB
        ZDMMIN = 1.E+05_JPRB
        ZRKAX = 1.0_JPRB
        ZRKIN = 1.0_JPRB
        INNEGA = 0
        INBESS = 0
        ZRKT = ZRKI

        ! INNER LOOP ON RK

        LL520=.TRUE.

        DO WHILE(LL520)
          ! COMPUTES MAP FACTOR VARIATION FOR VARIOUS PROJECTION
          IF ( ZRKT == 0.0_JPRB ) THEN
            ZDM = COS(XLAT0R)/COS(ZLAT2R) - COS(XLAT0R)/COS(XLAT1R)
          ELSEIF ( ZRKT /= 1.0_JPRB ) THEN
            ZDM = (COS(XLAT0R)**(1.0_JPRB-ZRKT))*&
             & ((1.0_JPRB+SIN(XLAT0R))**ZRKT)*( (COS(XLAT1R)**(ZRKT-1.0_JPRB))*&
             & ((1.0_JPRB+SIN(XLAT1R))**(-ZRKT)) - (COS(ZLAT2R)**(ZRKT-1.0_JPRB))*&
             & ((1.0_JPRB+SIN(ZLAT2R))**(-ZRKT)) )  
          ELSEIF ( ZRKT == 1.0_JPRB ) THEN
            ZDM = (1.0_JPRB+SIN(XLAT0R))*( 1.0_JPRB/(1.0_JPRB+SIN(XLAT1R)) -&
             & 1.0_JPRB/(1.0_JPRB+SIN(ZLAT2R)) )  
          ENDIF
          INBESS = INBESS + 1
          IF ( ZDM <= 0.0_JPRB ) THEN
            INNEGA = INNEGA + 1
          ENDIF
          IF ( ZDM >= ZDMMAX .AND. ZDM > 0.0_JPRB ) THEN
            ZDMMAX = ZDM
            ZRKAX = ZRKT
          ENDIF
          IF ( ZDM <= ZDMMIN .AND. ZDM > 0.0_JPRB ) THEN
            ZDMMIN = ZDM
            ZRKIN = ZRKT
          ENDIF
          ZRKT = ZRKT + ZDRK
          LL520=(ZRKT <= 1.0_JPRB)
        ENDDO ! DO WHILE(LL520)

        WRITE (KULOUT,*) ' '
        WRITE (KULOUT,*) ' CHOICE OF OPTIMAL RK ITERATION ',ITERK
        WRITE (KULOUT,*) ' PREVIOUS RK ',ZRKOLD
        WRITE (KULOUT,*) ' TEST OVER ',INBESS,' VALUES '
        WRITE (KULOUT,*) ' INCLUDING ',INNEGA,' NEGATIVE VALUES'
        WRITE (KULOUT,*) ' RK MINI = ',ZRKI,' INCRMENT = ',ZDRK
        WRITE (KULOUT,*) ' '
        WRITE (KULOUT,*) ' DELTA(M) MAXI = ',ZDMMAX,' AT RK = ',ZRKAX
        WRITE (KULOUT,*) ' DELTA(M) MINI = ',ZDMMIN,' AT RK = ',ZRKIN

        ! UPDATES VALUE OF GUESS ZRPK
        ZRKOLD = ZRPK
        ZRPK = ZRKIN

      ENDIF ! ISPECA /= 1

      IF ( KSOTRP == 0 ) THEN
        ! END THE OUTER LOOP IF BOTH LATITUDES WERE KNOWN
        ZRKOLD = ZRPK
      ELSEIF ( KSOTRP >= 1 ) THEN
        ! DETRMINES NEW OTHER LATITUDE
        CALL EGGMLT (PRPI,KDLUX,KDLUN,KDGUX,KDGUN,KULOUT,1,&
         & ZRPK,XLON0U,XLON1U,ZLON2U,KSOTRP,XLAT1R,ZLAT2R,&
         & HSUD,XBETA)  
      ENDIF
      ! NEW REFERENCE LATITUDE
      IF ( KGIV0 == 2.OR. KGIV0 == 3 ) THEN
        XLAT0R = 0.5_JPRB*( ZLAT2R + XLAT1R )
        XLAT0R = MIN(ZPIS2,MAX(-ZPIS2,XLAT0R))
      ENDIF

      ! COMPLETES INTERATION OF OUTER LOOP (RK AND LATITUDE)
      ITERK = ITERK + 1
      IF ( ITERK > ITERKX ) THEN
        WRITE (KULOUT,*) ' *** EGGX **** TROUBLE '
        WRITE (KULOUT,*) ' NO CONVERGENCE OF AUTOMATIC CHOICE '
        CALL ABOR1(' EGGX: abor1 5.1')
      ENDIF

      LL510=(ABS(ZRPK-ZRKOLD) > ZDRK)

    ENDDO ! DO WHILE(LL510)

    PRPK = ZRPK
    WRITE (KULOUT,*) ' '
    WRITE (KULOUT,*) ' --- EGGX AUTOMATIC CHOICE '
    WRITE (KULOUT,*) ' '
    WRITE (KULOUT,*) ' FINAL VALUE OF PRPK = ',PRPK
    WRITE (KULOUT,*) ' '
    WRITE (KULOUT,*) ' PROJECTION REFERENCE LATITUDE ',&
     & ' (ON ROTATED SPHERE) '  
    WRITE (KULOUT,*) ' FINAL LATITUDE LAT1 R = ',XLAT1R
    WRITE (KULOUT,*) ' FINAL LATITUDE LAT2 R = ',ZLAT2R
    WRITE (KULOUT,*) ' LAT0R = ',XLAT0R
    WRITE (KULOUT,*) ' '
  ENDIF

ENDIF
!     MEMORIZES THE FINAL VALUE OF PRPK
XGGPK = PRPK

!*
!---------------------------------------------------------------------
!     6.- PROJECTION ON CARTESIAN PLAN

!     REMARK : THE CODE IS GENERAL, BUT STEREO/LAMBERT SHOULD
!              NOT BE COMBINED TO ROTATION

IF ( PRPK > 1.0_JPRB ) THEN
  WRITE (KULOUT,*) ' *** EGGX ERROR : NON-EXISTING PROJ.'
  CALL ABOR1(' EGGX: abor1 6.1')
ENDIF

IF ( PRPK > 0.0_JPRB ) THEN
  WRITE (KULOUT,*) ' STEREO OR LAMBERT PROJECTION '
  IF ( PRPK == 1.0_JPRB ) WRITE (KULOUT,*)&
   & ' EFFECTIVELY STREOGRAPHIC PROJECTION '  

  !   COMPUTES BASIC PARAMETERS
  !   -------------------------

  !    HALF COLATITUDE
  ZDCLA1 = ZPIS4 - 0.5_JPRB*XLAT1R
  ZDCLA2 = ZPIS4 - 0.5_JPRB*ZLAT2R
  ZDCLA0 = ZPIS4 - 0.5_JPRB*XLAT0R
  !   PROJECTION CONSTANTS
  IF ( PRPK < 1.0_JPRB ) THEN
    XGGM0 = ( COS( XLAT0R )**(1.0_JPRB-PRPK) )*&
     & ( ( 1.0_JPRB + SIN( XLAT0R ) )**PRPK )  
    ZUSKP = XGGM0/PRPK
    XRPKSM = 1.0_JPRB/ZUSKP
  ELSE
    XGGM0 = 1.0_JPRB + SIN( XLAT0R )
    ZUSKP = XGGM0
    XRPKSM = 1.0_JPRB/ZUSKP
  ENDIF

  !   COMPUTES RESOLUTION
  !   -------------------
  IF( .NOT.LLPLANEX.AND..NOT.LLPLANEY )THEN
    PDELX = PRA*ZUSKP*( (TAN(ZDCLA2)**PRPK)*SIN( PRPK*(&
     & ZLON2U-XLON0U)-XBETA ) - (TAN(ZDCLA1)**PRPK)*&
     & SIN( PRPK*(XLON1U-XLON0U)-XBETA ) )/REAL(KDLUX-KDLUN,JPRB)  
    PDELY = HSUD*PRA*ZUSKP*( (TAN(ZDCLA1)**PRPK)*COS( PRPK*(&
     & XLON1U-XLON0U)-XBETA ) - (TAN(ZDCLA2)**PRPK)*&
     & COS( PRPK*(ZLON2U-XLON0U)-XBETA ) )/REAL(KDGUX-KDGUN,JPRB)  
  ELSEIF( LLPLANEX )THEN
    PDELY = HSUD*PRA*ZUSKP*( (TAN(ZDCLA1)**PRPK)*COS( PRPK*(&
     & XLON1U-XLON0U)-XBETA ) - (TAN(ZDCLA2)**PRPK)*&
     & COS( PRPK*(ZLON2U-XLON0U)-XBETA ) )/REAL(KDGUX-KDGUN)  
    PDELX=PDELY
  ELSEIF( LLPLANEY )THEN
    PDELX = PRA*ZUSKP*( (TAN(ZDCLA2)**PRPK)*SIN( PRPK*(&
     & ZLON2U-XLON0U)-XBETA ) - (TAN(ZDCLA1)**PRPK)*&
     & SIN( PRPK*(XLON1U-XLON0U)-XBETA ) )/REAL(KDLUX-KDLUN)  
    PDELY = PDELX
  ENDIF
      
  WRITE (KULOUT,*) ' '
  WRITE (KULOUT,*) ' MAP FACTOR BASE XGGM0 = ',XGGM0
  WRITE (KULOUT,*) ' ZUSKP = ',ZUSKP
  WRITE (KULOUT,*) ' '
  WRITE (KULOUT,*) ' X GRID SIZE (KM) = ',PDELX*1.E-03_JPRB
  WRITE (KULOUT,*) ' '
  WRITE (KULOUT,*) ' Y GRID SIZE (KM) = ',PDELY*1.E-03_JPRB
  WRITE (KULOUT,*) ' '

  !   COMPUTES POLE LOCATION ON GRID
  !   ------------------------------

  XIPORE =  - PRA*ZUSKP*( TAN( ZDCLA1 )**PRPK )*&
   & SIN( PRPK*(XLON1U-XLON0U)-XBETA )/PDELX  
  XJPORE =  HSUD*PRA*ZUSKP*( TAN( ZDCLA1 )**PRPK )*&
   & COS( PRPK*(XLON1U-XLON0U)-XBETA )/PDELY  
  NYMGGI = 10

  WRITE (KULOUT,*) ' '
  WRITE (KULOUT,*) ' POLE LOCATION ON GRID IP = ',XIPORE,' JP = ',XJPORE

  ZIPV = REAL( KDLUX ,JPRB) - PRA*ZUSKP*( TAN( ZDCLA2 )**PRPK )*&
   & SIN( PRPK*(ZLON2U-XLON0U)-XBETA )/PDELX  - REAL(KDLUN,JPRB)  
  ZJPV = REAL( KDGUX ,JPRB) + PRA*ZUSKP*( TAN( ZDCLA2 )**PRPK )*&
   & COS( PRPK*(ZLON2U-XLON0U)-XBETA )*HSUD/PDELY -&
   & REAL(KDGUN,JPRB)  

  WRITE (KULOUT,*) ' VRF POLE LOCATION ON GRID IP = ',ZIPV,' JP = ',ZJPV

  !   GRID POINTS LOCATION
  !   --------------------

  DO JLAT = KDGUN, KDGUX

    DO JLON = KDLUN, KDLUX
      PGELAM(JLON,JLAT) = REAL(JLON-KDLUN,JPRB)*PDELX
      PGELAT(JLON,JLAT) = REAL(JLAT-KDGUN,JPRB)*PDELY
    ENDDO

    CALL EGGRVS (PRPI, PRA, PDELX, PDELY, KDLSUR-KDLSA+1,&
     & 1, KDLUX-KDLUN+1, KULOUT,&
     & PGELAM(KDLUN,JLAT), PGELAT(KDLUN,JLAT), PGM(KDLUN,JLAT),&
     & PGNORX(KDLUN,JLAT), PGNORY(KDLUN,JLAT))  
  ENDDO

ENDIF

IF ( PRPK == 0.0_JPRB ) THEN
  WRITE (KULOUT,*) ' MERCATOR PROJECTION '

  !   COMPUTES BASIC PARAMETERS
  !   -------------------------

  !    HALF COLATITUDE
  ZDCLA1 = ZPIS4 - 0.5_JPRB*XLAT1R
  ZDCLA2 = ZPIS4 - 0.5_JPRB*ZLAT2R
  ZDCLA0 = ZPIS4 - 0.5_JPRB*XLAT0R

  !   COMPUTES RESOLUTION
  !   -------------------

  ZFACE = PRA*COS( XLAT0R )
  ZDLON =  ZLON2U - XLON1U
  ZDTLAT = LOG( TAN(ZDCLA1)/TAN(ZDCLA2) )
  IF( .NOT.LLPLANEX.AND..NOT.LLPLANEY )THEN
    PDELX = ZFACE*( ZDLON*COS(XBETA) + ZDTLAT*SIN(XBETA) )&
     & /REAL( KDLUX-KDLUN ,JPRB)  
    PDELY = ZFACE*( -ZDLON*SIN(XBETA) + ZDTLAT*COS(XBETA) )&
     & /REAL( KDGUX-KDGUN ,JPRB)  
  ELSEIF( LLPLANEX )THEN
    PDELY = ZFACE*( -ZDLON*SIN(XBETA) + ZDTLAT*COS(XBETA) )&
     & /REAL( KDGUX-KDGUN )  
    PDELX = PDELY
  ELSEIF( LLPLANEY )THEN
    PDELX = ZFACE*( ZDLON*COS(XBETA) + ZDTLAT*SIN(XBETA) )&
     & /REAL( KDLUX-KDLUN )  
    PDELY = PDELX
  ENDIF

  WRITE (KULOUT,*) ' '
  WRITE (KULOUT,*) ' MAP FACTOR BASE COS( LAT0 ) = ',COS( XLAT0R )
  WRITE (KULOUT,*) ' '
  WRITE (KULOUT,*) ' X GRID SIZE (KM) = ',PDELX*1.E-03_JPRB
  WRITE (KULOUT,*) ' '
  WRITE (KULOUT,*) ' Y GRID SIZE (KM) = ',PDELY*1.E-03_JPRB
  WRITE (KULOUT,*) ' '

  !   COMPUTES EQUATOR LOCATION ON GRID
  !   ------------------------------

  XIPORE = - PRA*COS( XLAT0R )*( XLON1U-XLON0U )/PDELX
  XJPORE = + PRA*COS( XLAT0R )*LOG(TAN( ZDCLA1 ) )/PDELY
  XRPKSM=1.0_JPRB
  XGGPK = PRPK
  NYMGGI = 10

  WRITE (KULOUT,*) ' '
  WRITE (KULOUT,*) ' EQUATOR LOCATION ON GRID IE = ',XIPORE,' JE = ',XJPORE

  !     GRID POINTS LOCATION
  !     --------------------

  DO JLAT = KDGUN, KDGUX

    DO JLON = KDLUN, KDLUX
      PGELAM(JLON,JLAT) = REAL(JLON-KDLUN,JPRB)*PDELX
      PGELAT(JLON,JLAT) = REAL(JLAT-KDGUN,JPRB)*PDELY
    ENDDO

    CALL EGGRVS (PRPI, PRA, PDELX, PDELY, KDLSUR-KDLSA+1,&
     & 1, KDLUX-KDLUN+1, KULOUT,&
     & PGELAM(KDLUN,JLAT), PGELAT(KDLUN,JLAT), PGM(KDLUN,JLAT),&
     & PGNORX(KDLUN,JLAT), PGNORY(KDLUN,JLAT))  
  ENDDO

ENDIF

!*
!---------------------------------------------------------------------
!     7.- INVERSE ROTATION BACK TO GEOGRAPHICAL COORDINATES

!    THIS OPERATION IS PERFORMED BY EGGRVS

!    FINAL UPDATE OF ACTUAL CORNERS USED

IF ( KSOTRP == 1 ) THEN
  PLAT2 = PGELAT(KDLUX,KDGUX)
  PLON2 = PGELAM(KDLUX,KDGUX)
ENDIF
IF ( KSOTRP == 2 ) THEN
  PLAT1 = PGELAT(KDLUN,KDGUN)
  PLON1 = PGELAM(KDLUN,KDGUN)
ENDIF

!    PLON1 AND PLON2 MUST BE BETWEEN 0 AND 2*RPI

PLON1=MOD(PLON1,2*PRPI)
PLON2=MOD(PLON2,2*PRPI)

IF ( HSUD < 0.0_JPRB ) THEN
  IF ( KSOTRP /= 2 ) THEN
    PLAT1 = HSUD*PLAT1
  ENDIF
  IF ( KSOTRP /= 1 ) THEN
    PLAT2 = HSUD*PLAT2
  ENDIF
  PLAT0 = HSUD*PLAT0
ENDIF

! KSOTRP reset back to original value
IF( LLPLANEX.OR.LLPLANEY )THEN
  KSOTRP = ISOTRP
ENDIF

WRITE (KULOUT,*) ' '
WRITE (KULOUT,*) ' ---------- '
WRITE (KULOUT,*) ' '
WRITE (KULOUT,*) '  EGGX IS OVER '
WRITE (KULOUT,*) ' '
WRITE (KULOUT,*) ' ---------- '
WRITE (KULOUT,*) ' '
WRITE (KULOUT,*) ' '

!---------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('EGGX',1,ZHOOK_HANDLE)
END SUBROUTINE EGGX
