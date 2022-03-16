!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE HOR_INTERPOL_ROTLATLON(KLUOUT,PFIELDIN,PFIELDOUT)
!     #################################################################################
!
!!****  *HOR_INTERPOL_ROTLATLON * - Interpolation from a rotated lat/lon grid
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     U. Andrae
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2007
!!------------------------------------------------------------------
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, NCOMM, NPROC, IDX_I
USE MODD_PREP,       ONLY : XLAT_OUT, XLON_OUT, LINTERP
USE MODD_GRID_ROTLATLON
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_GRID_GRIB,  ONLY : NNI

!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*      0.1    declarations of arguments
!
INTEGER,              INTENT(IN)  :: KLUOUT    ! logical unit of output listing
REAL, DIMENSION(:,:), INTENT(IN)  :: PFIELDIN  ! field to interpolate horizontally
REAL, DIMENSION(:,:), INTENT(OUT) :: PFIELDOUT ! interpolated field
!
!*      0.2    declarations of local variables
!
#ifdef SFX_MPI
INTEGER, DIMENSION(MPI_STATUS_SIZE) :: ISTATUS
#endif
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZW, ZFIELDIN,ZFGET
INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: IJD,IJGET
REAL, DIMENSION(:), ALLOCATABLE :: XLAT_IND, XLON_IND, XRAT_OUT, XRON_OUT, &
                                   ZFIELDTMP
!
INTEGER :: INFOMPI
INTEGER :: J, I, JI, INO, JL, INL, ILON, ILAT, ISIZE
REAL    :: ZWX, ZWY, ZWSUM
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('HOR_INTERPOL_ROTLATLON',0,ZHOOK_HANDLE)
!
WRITE(KLUOUT,'(A)')' | Running rotated latlon interpolation'
!
INO = SIZE(XLAT_OUT)
INL = SIZE(PFIELDOUT,2)
!
!
!*      1.    Allocations
!
ALLOCATE(XRAT_OUT(INO),XRON_OUT(INO),XLAT_IND(INO),XLON_IND(INO),IJD(INO,INL,4),ZW(INO,INL,4))  
!
!*  Transformation of latitudes/longitudes into rotated coordinates
!


 CALL REGROT(XLON_OUT,XLAT_OUT,XRON_OUT,XRAT_OUT,XRLOP,XRLAP,1)  
!

    IF ( XRILA1 > XRILA2 .AND. XRILO1 < XRILO2 ) THEN  
      DO JI=1,INO
        XLAT_IND(JI) = ( XRILA1 - XRAT_OUT(JI) ) / XRDY + 1.
        XLON_IND(JI) = ( XRON_OUT(JI) - XRILO1 ) / XRDX + 1.
      ENDDO
    ELSEIF ( XRILA1 < XRILA2 .AND. XRILO1 < XRILO2 ) THEN
    DO JI=1,INO
       XLAT_IND(JI) = ( XRAT_OUT(JI) - XRILA1) / XRDY + 1.
       XLON_IND(JI) = ( XRON_OUT(JI) - XRILO1) / XRDX + 1.
    ENDDO
    ELSEIF ( XRILA1 > XRILA2 .AND. XRILO1 > XRILO2 ) THEN
      DO JI=1,INO
        XLAT_IND(JI) = ( XRILA1 - XRAT_OUT(JI) ) / XRDY + 1.
        XLON_IND(JI) = ( XRILO1 - XRON_OUT(JI) ) / XRDX + 1.
      ENDDO
    ELSEIF ( XRILA1 < XRILA2 .AND. XRILO1 > XRILO2 ) THEN
      DO JI=1,INO
        XLAT_IND(JI) = ( XRAT_OUT(JI) - XRILA1 ) / XRDY + 1.
        XLON_IND(JI) = ( XRILO1 - XRON_OUT(JI) ) / XRDX + 1.
      ENDDO
    ENDIF



PFIELDOUT(:,:) = XUNDEF
!
ZW(:,:,:) = 0.
DO JL=1,INL
  !
  DO JI=1,INO
    !
    ILON = INT(XLON_IND(JI))
    ILAT = INT(XLAT_IND(JI))
    !
    ZWX = XLON_IND(JI) - FLOAT(ILON)
    ZWY = XLAT_IND(JI) - FLOAT(ILAT)
    !
    ZW(JI,JL,1) = (1.-ZWX)*(1.-ZWY)
    ZW(JI,JL,2) = (1.-ZWX)*    ZWY
    ZW(JI,JL,3) =     ZWX *(1.-ZWY)
    ZW(JI,JL,4) =     ZWX *    ZWY
    !
    IJD(JI,JL,1) = ILON   + NRX*(ILAT   -1)
    IJD(JI,JL,2) = ILON   + NRX*(ILAT+1 -1)
    IJD(JI,JL,3) = ILON+1 + NRX*(ILAT   -1)
    IJD(JI,JL,4) = ILON+1 + NRX*(ILAT+1 -1)    
    !
  ENDDO
  !
ENDDO
!
ALLOCATE(ZFIELDIN(INO,INL,4))
!
IF (NRANK/=NPIO) THEN
  !
  IDX_I = IDX_I + 1
#ifdef SFX_MPI  
  CALL MPI_SEND(INO,KIND(INO)/4,MPI_INTEGER,NPIO,IDX_I,NCOMM,INFOMPI)
#endif
  !
  IDX_I = IDX_I + 1
#ifdef SFX_MPI  
  CALL MPI_SEND(IJD,SIZE(IJD)*KIND(IJD)/4,MPI_INTEGER,NPIO,IDX_I,NCOMM,INFOMPI)
#endif  
  !
  IDX_I = IDX_I + 1
#ifdef SFX_MPI  
  CALL MPI_RECV(ZFIELDIN,SIZE(ZFIELDIN)*KIND(ZFIELDIN)/4,MPI_REAL,NPIO,IDX_I,NCOMM,ISTATUS,INFOMPI)
#endif  
  !
ELSE
  !
  DO J=0,NPROC-1
    !
    IF (J/=NPIO) THEN
#ifdef SFX_MPI            
      CALL MPI_RECV(ISIZE,KIND(ISIZE)/4,MPI_INTEGER,J,IDX_I+1,NCOMM,ISTATUS,INFOMPI)
#endif      
      ALLOCATE(IJGET(ISIZE,INL,4))
#ifdef SFX_MPI      
      CALL MPI_RECV(IJGET,SIZE(IJGET)*KIND(IJGET)/4,MPI_INTEGER,J,IDX_I+2,NCOMM,ISTATUS,INFOMPI)
#endif      
    ELSE
      ISIZE = INO
      ALLOCATE(IJGET(ISIZE,INL,4))
      IJGET(:,:,:) = IJD(:,:,:)
    ENDIF
    !
    ALLOCATE(ZFGET(ISIZE,INL,4))
    !
    ZFGET(:,:,:) = 0.
    DO JL = 1,INL
      DO JI = 1,ISIZE
        DO I = 1,4
          ZFGET(JI,JL,I) = PFIELDIN(IJGET(JI,JL,I),JL)
        ENDDO
      ENDDO
    ENDDO
    !
    IF (J/=NPIO) THEN
#ifdef SFX_MPI            
      CALL MPI_SEND(ZFGET,SIZE(ZFGET)*KIND(ZFGET)/4,MPI_REAL,J,IDX_I+3,NCOMM,INFOMPI)
#endif      
    ELSE
      ZFIELDIN(:,:,:) = ZFGET(:,:,:)
    ENDIF
    !
    DEALLOCATE(IJGET,ZFGET)
    !
  ENDDO
  !
  IDX_I = IDX_I + 3
  !
ENDIF 
!
DO JL = 1,INL
  DO JI = 1,INO
    !
    DO I = 1,4
      IF (ABS(ZFIELDIN(JI,JL,I)-XUNDEF)<1.E-6) ZW(JI,JL,I) = 0.
    ENDDO
    !
    ZWSUM = ZW(JI,JL,1) + ZW(JI,JL,2) + ZW(JI,JL,3) + ZW(JI,JL,4)
    !
    IF ( ABS(ZWSUM)<1.E-6 ) THEN
      ZW(JI,JL,1) = 1.
    ELSE
      DO I = 1,4
        ZW(JI,JL,I) = ZW(JI,JL,I)/ZWSUM
      ENDDO
    ENDIF
    !
    PFIELDOUT(JI,JL) = ZW(JI,JL,1)*ZFIELDIN(JI,JL,1) + ZW(JI,JL,2)*ZFIELDIN(JI,JL,2) + &
                       ZW(JI,JL,3)*ZFIELDIN(JI,JL,3) + ZW(JI,JL,4)*ZFIELDIN(JI,JL,4)
    !
  ENDDO
ENDDO

!
!*      5.    Extrapolations if some points were not interpolated
!

WRITE(KLUOUT,*) ' Remaining horizontal extrapolations, if any', &
                ALLOCATED(LINTERP)

ALLOCATE(ZFIELDTMP(INO))

DO JL=1,INL

   !* no missing point
   IF (COUNT(PFIELDOUT(:,JL)==XUNDEF .AND. LINTERP(:))==0) CYCLE

   !* no data point
   IF (COUNT(PFIELDOUT(:,JL)/=XUNDEF)==0) CYCLE

   WRITE(KLUOUT,*) ' Total number of valid data     : ',COUNT(PFIELDOUT(:,JL)/=XUNDEF)
   WRITE(KLUOUT,*) ' Number of points to extrapolate: ',COUNT(PFIELDOUT(:,JL)==XUNDEF .AND. LINTERP(:))

   ZFIELDTMP(:) = PFIELDOUT(:,JL)

   CALL HOR_EXTRAPOL_SURF_ROTLATLON(KLUOUT,'LALO',           &
                          XLAT_OUT,XLON_OUT,ZFIELDTMP,       &
                          XLAT_OUT,XLON_OUT,PFIELDOUT(:,JL), &
                          LINTERP)

ENDDO

!
!*      6.    Deallocations
DEALLOCATE(XRAT_OUT,XRON_OUT,XLAT_IND,XLON_IND,IJD,ZFIELDIN,ZFIELDTMP)  
!
IF (LHOOK) CALL DR_HOOK('HOR_INTERPOL_ROTLATLON',1,ZHOOK_HANDLE)
!
CONTAINS
!
SUBROUTINE REGROT(PXREG,PYREG,PXROT,PYROT,PXCEN,PYCEN,KCALL)  
!
USE MODD_CSTS, ONLY : XPI
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!*    CONVERSION BETWEEN REGULAR AND ROTATED SPHERICAL COORDINATES.
!*
!*    PXREG     LONGITUDES OF THE REGULAR COORDINATES
!*    PYREG     LATITUDES OF THE REGULAR COORDINATES
!*    PXROT     LONGITUDES OF THE ROTATED COORDINATES
!*    PYROT     LATITUDES OF THE ROTATED COORDINATES
!*              ALL COORDINATES GIVEN IN DEGREES N (NEGATIVE FOR S)
!*              AND DEGREES E (NEGATIVE VALUES FOR W)
!*    KXDIM     DIMENSION OF THE GRIDPOINT FIELDS IN THE X-DIRECTION
!*    KYDIM     DIMENSION OF THE GRIDPOINT FIELDS IN THE Y-DIRECTION
!*    KX        NUMBER OF GRIDPOINT IN THE X-DIRECTION
!*    KY        NUMBER OF GRIDPOINTS IN THE Y-DIRECTION
!*    PXCEN     REGULAR LONGITUDE OF THE SOUTH POLE OF THE ROTATED GRID
!*    PYCEN     REGULAR LATITUDE OF THE SOUTH POLE OF THE ROTATED GRID
!*
!*    KCALL=-1: FIND REGULAR AS FUNCTIONS OF ROTATED COORDINATES.
!*    KCALL= 1: FIND ROTATED AS FUNCTIONS OF REGULAR COORDINATES.
!*
!*    J.E. HAUGEN   HIRLAM   JUNE -92
!
!-----------------------------------------------------------------------
!
INTEGER, INTENT(IN) :: KCALL
REAL, INTENT(IN) :: PXCEN,PYCEN 
REAL, DIMENSION(:), INTENT(INOUT) :: PXREG, PYREG
REAL, DIMENSION(:), INTENT(INOUT) :: PXROT, PYROT         
!
!-----------------------------------------------------------------------
!
REAL :: ZRAD,ZSYCEN,ZCYCEN,ZXMXC,ZSXMXC,ZCXMXC,ZSYREG,ZCYREG, &
        ZSYROT,ZCYROT,ZCXROT,ZSXROT,ZRADI  
INTEGER :: JI, ISIZE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-----------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('REGROT',0,ZHOOK_HANDLE)
!
ISIZE = SIZE(PXREG)
!
ZRAD = XPI/180.
ZRADI = 1./ZRAD
ZSYCEN = SIN(ZRAD*(PYCEN+90.))
ZCYCEN = COS(ZRAD*(PYCEN+90.))
!
IF (KCALL.EQ.1) THEN
  !
  DO JI = 1,ISIZE
    !
    ZXMXC  = ZRAD*(PXREG(JI) - PXCEN)
    ZSXMXC = SIN(ZXMXC)
    ZCXMXC = COS(ZXMXC)
    !
    ZSYREG = SIN(ZRAD*PYREG(JI))
    ZCYREG = COS(ZRAD*PYREG(JI))
    !
    ZSYROT = ZCYCEN*ZSYREG - ZSYCEN*ZCYREG*ZCXMXC
    ZSYROT = MIN(MAX(ZSYROT,-1.0),+1.0)
    !
    PYROT(JI) = ASIN(ZSYROT)*ZRADI
    !
    ZCYROT = COS(ZRAD*PYROT(JI))
    ZCXROT = (ZCYCEN*ZCYREG*ZCXMXC + ZSYCEN*ZSYREG)/ZCYROT  
    ZCXROT = MIN(MAX(ZCXROT,-1.0),+1.0)
    ZSXROT = ZCYREG*ZSXMXC/ZCYROT
    !
    PXROT(JI) = ACOS(ZCXROT)*ZRADI
    !
    IF (ZSXROT.LT.0.0) PXROT(JI) = -PXROT(JI)
    !
  ENDDO
  !
ELSEIF (KCALL.EQ.-1) THEN
  !
  DO JI = 1,ISIZE
    !
    ZSXROT = SIN(ZRAD*PXROT(JI))
    ZCXROT = COS(ZRAD*PXROT(JI))
    ZSYROT = SIN(ZRAD*PYROT(JI))
    ZCYROT = COS(ZRAD*PYROT(JI))
    !
    ZSYREG = ZCYCEN*ZSYROT + ZSYCEN*ZCYROT*ZCXROT
    ZSYREG = MAX(ZSYREG,-1.0)
    ZSYREG = MIN(ZSYREG,+1.0)
    !
    PYREG(JI) = ASIN(ZSYREG)*ZRADI
    !
    ZCYREG = COS(PYREG(JI)*ZRAD)
    !
    ZCXMXC = (ZCYCEN*ZCYROT*ZCXROT - ZSYCEN*ZSYROT)/ZCYREG  
    ZCXMXC = MAX(ZCXMXC,-1.0)
    ZCXMXC = MIN(ZCXMXC,+1.0)
    ZSXMXC = ZCYROT*ZSXROT/ZCYREG
    ZXMXC  = ACOS(ZCXMXC)*ZRADI
    IF (ZSXMXC.LT.0.0) ZXMXC = -ZXMXC
    !
    PXREG(JI) = ZXMXC + PXCEN
    !
  ENDDO
  !
ELSE
  !
  WRITE(6,'(1X,''INVALID KCALL IN REGROT'')')
  CALL ABOR1_SFX('HOR_INTERPOL_ROTLATON:REGROT:KCALL MUST BE 1 OR -1')
  !
ENDIF
!   
IF (LHOOK) CALL DR_HOOK('REGROT',1,ZHOOK_HANDLE)
!
END SUBROUTINE REGROT
!
!-------------------------------------------------------------------------------------

!     #########
      SUBROUTINE HOR_EXTRAPOL_SURF_ROTLATLON(KLUOUT,HCOORTYPE, &
                     PLAT_IN,PLON_IN,PFIELD_IN, &
                     PLAT,PLON,PFIELD,OINTERP)  
!     ###################################################################
!
!!**** *HOR_EXTRAPOL_SURF_ROTLATLON* extrapolate a surface field
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!       For each point to interpolate, the nearest valid point value is set.
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson          Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     01/12/98
!!     V. Masson    01/2004 extrapolation in latitude and longitude
!!     M. Jidane    11/2013 add OpenMP directives
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_CSTS,       ONLY : XPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER,               INTENT(IN)     :: KLUOUT   ! output listing logical unit
CHARACTER(LEN=4),      INTENT(IN)     :: HCOORTYPE! type of coordinate
REAL,   DIMENSION(:),  INTENT(IN)     :: PLAT_IN  ! input lat. of each grid mesh.
REAL,   DIMENSION(:),  INTENT(IN)     :: PLON_IN  ! input lon. of each grid mesh.
REAL,   DIMENSION(:),  INTENT(IN)     :: PFIELD_IN! input field on grid mesh
REAL,   DIMENSION(:),  INTENT(IN)     :: PLAT     ! latitude of each grid mesh.
REAL,   DIMENSION(:),  INTENT(IN)     :: PLON     ! longitude of each grid mesh.
REAL,   DIMENSION(:),  INTENT(INOUT)  :: PFIELD   ! field on grid mesh
LOGICAL,DIMENSION(:),  INTENT(IN)     :: OINTERP  ! .true. where physical value is needed
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER  :: INO     ! output array size
INTEGER  :: INO_IN  ! input  array size
!
REAL     :: ZLAT  ! latitude of point to define
REAL     :: ZLON  ! longitude of point to define
REAL     :: ZDIST ! current distance to valid point (in lat/lon grid)
REAL     :: ZFIELD! current found field value
REAL     :: ZNDIST! smallest distance to valid point
REAL     :: ZCOSLA! cosine of latitude
!
INTEGER  :: JI    ! loop index on points
INTEGER  :: JISC  ! loop index on valid points
REAL     :: ZLONSC! longitude of valid point
LOGICAL  :: GLALO ! flag true is second coordinate is a longitude or pseudo-lon.
                  !      false if metric coordinates
!
REAL(KIND=JPRB) :: ZRAD ! conversion degrees to radians
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('HOR_EXTRAPOL_SURF_ROTLATLON',0,ZHOOK_HANDLE)
!
INO = SIZE(PFIELD,1)
!
WHERE (.NOT. OINTERP(:)) PFIELD(:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
INO_IN = SIZE(PFIELD_IN)
!
GLALO = HCOORTYPE=='LALO'
!
!-------------------------------------------------------------------------------
!
!*    3.     No data point
!            -------------
!
IF (COUNT(PFIELD_IN(:)/=XUNDEF)==0 .AND. LHOOK) CALL DR_HOOK('HOR_EXTRAPOL_SURF_ROTLATLON',1,ZHOOK_HANDLE)
IF (COUNT(PFIELD_IN(:)/=XUNDEF)==0) RETURN
!
!-------------------------------------------------------------------------------
!
!*      4.   Loop on points to define
!            ------------------------
!
ZRAD=XPI/180.0_JPRB
!
!$OMP PARALLEL DO SCHEDULE(DYNAMIC,1) PRIVATE(JI,JISC,ZLAT,ZLON,ZFIELD,ZCOSLA,ZLONSC,ZDIST,ZNDIST,ZHOOK_HANDLE_OMP)
DO JI=1,INO
  IF (PFIELD(JI)/=XUNDEF) CYCLE
  IF (.NOT. OINTERP(JI))  CYCLE
!
!*      4.1  initialisation
!            --------------
!
  IF (LHOOK) CALL DR_HOOK('HOR_EXTRAPOL_SURF_ROTLATLON OMP',0,ZHOOK_HANDLE_OMP)
  ZNDIST=1.E20
  ZLAT=PLAT(JI)
  ZLON=PLON(JI)
  ZFIELD=PFIELD(JI)
  ZCOSLA=COS(ZLAT*ZRAD)
!
!*      4.2  extrapolation with nearest valid point
!            --------------------------------------
!
  DO JISC=1,INO_IN
    IF (PFIELD_IN(JISC)/=XUNDEF) THEN
      ZLONSC = PLON_IN(JISC)
      IF (GLALO) THEN
        IF (ZLONSC-ZLON> 180.) ZLONSC = ZLONSC - 360.
        IF (ZLONSC-ZLON<-180.) ZLONSC = ZLONSC + 360.
        ZDIST= (PLAT_IN(JISC)-ZLAT) ** 2 + ((ZLONSC-ZLON)*ZCOSLA) ** 2
      ELSE
        ZDIST= (PLAT_IN(JISC)-ZLAT) ** 2 + (ZLONSC-ZLON) ** 2
      END IF
      IF (ZDIST<=ZNDIST) THEN
        ZFIELD=PFIELD_IN(JISC)
        ZNDIST=ZDIST
      END IF
    END IF
  END DO
  PFIELD(JI) = ZFIELD

  IF (LHOOK) CALL DR_HOOK('HOR_EXTRAPOL_SURF_ROTLATLON OMP',1,ZHOOK_HANDLE_OMP)
END DO
!$OMP END PARALLEL DO
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('HOR_EXTRAPOL_SURF_ROTLATLON',1,ZHOOK_HANDLE)
!
END SUBROUTINE HOR_EXTRAPOL_SURF_ROTLATLON

END SUBROUTINE HOR_INTERPOL_ROTLATLON
