!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######################
      MODULE MODD_DATA_COVER_PAR
!     ######################
!
!!****  *MODD_DATA_COVER_PAR* - declaration of prognostic variables related
!!                          to the ground parameterization
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to specify  the 
!     parameters related to the surface parameterization interface.
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!      S. Belair   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       29/04/95                      
!!      (V.Masson)     05/10/98+ add XCDZ0EFF, XRHOSMIN, XRHOSMAX
!!      (V.Masson)     15/03/99 add number of layers
!!      (A.Boone)      02/05/02 add ISBA-ES parameters
!!      (R.Alkama)     05/2012  add 7 new vegtypes (19 rather than 12)
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
!------------------------------------------------------------------------------
!
! Maximum number of cover classes
INTEGER  :: JPCOVER
!last class number of EUROPE
INTEGER, PARAMETER    :: NCOVER_ECO1_END    = 255
!firt class number of EUROPE
INTEGER, PARAMETER    :: NCOVER_ECO2_START  = 301
!
INTEGER, PARAMETER :: NVEGTYPE_OLD = 19
INTEGER, PARAMETER :: NVEGTYPE_ECOSG = 20
!
INTEGER, PARAMETER :: NWATTYPE = 2
INTEGER, PARAMETER :: NSEATYPE = 1
INTEGER, PARAMETER :: NTWNTYPE = 10
INTEGER, DIMENSION(4) :: NTYPE = (/NSEATYPE,NWATTYPE,NVEGTYPE_ECOSG,NTWNTYPE/)
!
LOGICAL, DIMENSION(NVEGTYPE_ECOSG) :: LVEG_PRES
!
INTEGER, PARAMETER :: NCOVER = 573
!
INTEGER :: NVEGTYPE
!
! number of vegetation types
!
INTEGER  :: NVT_C4    ! C4 cultures types
INTEGER  :: NVT_TRBE  ! tropical broadleaf evergreen   EVER
INTEGER  :: NVT_TRBD  ! tropical broadleaf deciduous   TREE
INTEGER  :: NVT_TEBE  ! temperate broadleaf evergreen  TREE 
INTEGER  :: NVT_TEBD  ! temperate broadleaf cold-deciduous (summergreen)  TREE   
INTEGER  :: NVT_TENE  ! temperate needleleaf evergreen CONI
INTEGER  :: NVT_BOBD  ! boreal broadleaf cold-deciduous (summergreen) TREE  
INTEGER  :: NVT_BONE  ! boreal needleleaf evergreen    CONI
INTEGER  :: NVT_BOND  ! boreal needleleaf cold-deciduous (summergreen) CONI
INTEGER  :: NVT_BOGR  ! Arctic Grass  C3               GRAS
INTEGER  :: NVT_SHRB  ! Shrub        (TREE <= 2 m)
INTEGER  :: NVT_GRAS  ! GRASsland      C3
INTEGER  :: NVT_TROG  ! TROpical Grass C4
INTEGER  :: NVT_C3    ! C3 CULTures types
INTEGER  :: NVT_NO    ! NO vegetation
INTEGER  :: NVT_ROCK  ! ROCKs
INTEGER  :: NVT_SNOW  ! Ice-cap ice-sheet
INTEGER  :: NVT_IRR   ! IRRigated crops
INTEGER  :: NVT_PARK  ! irrigated PARKs gardens or peat bogs
!
INTEGER :: NVT_C3W    ! winter C3 crops
INTEGER :: NVT_C3S    ! summer C3 crops
INTEGER :: NVT_FLTR   ! flooded trees
INTEGER :: NVT_FLGR   ! flooded grassland
!
! indexes of these types (C4 cultures types, Broadleaf TREEs, CONIferous trees,
!                         EVERgreen broadleaf trees, GRASsland, TROpical Grass,
!                         C3 CULTures types, NO vegetation, ROCKs,
!                         IRRigated crops, irrigated PARKs gardens or peat bogs)
!
INTEGER :: NUT_CPHR
INTEGER :: NUT_CPMR
INTEGER :: NUT_CPLR
INTEGER :: NUT_OPHR
INTEGER :: NUT_OPMR
INTEGER :: NUT_OPLR
INTEGER :: NUT_LWLR
INTEGER :: NUT_LALR
INTEGER :: NUT_SPAR
INTEGER :: NUT_INDU
!
!---------------------------------------------------------------------------------
!
! Ecosystem corresponding to bare soil or permanent snow
!
INTEGER                                :: NBARE_SOIL
INTEGER                                :: NROCK
INTEGER                                :: NPERMSNOW
!--------------------------------------------------------------------------------- 
!
! Free water bodies
!
INTEGER, DIMENSION(:), ALLOCATABLE     :: NSEA
INTEGER, DIMENSION(:), ALLOCATABLE     :: NWATER
INTEGER :: NNSEA, NNWATER ! Actual number of pure sea or inland water covers
!---------------------------------------------------------------------------------
!
! Covers containing some water
!
INTEGER, DIMENSION(:), ALLOCATABLE     :: N_SOME_SEA_LAKE
INTEGER :: NN_SOME_SEA_LAKE ! Actual number of covers containing some sea or inland water
!---------------------------------------------------------------------------------
!
! maximum number of tiles used at any grid point
!
INTEGER                                :: NTILESFC = 4
!
!---------------------------------------------------------------------------------
!
!* building layers in the ecoclimap data
!
INTEGER                                :: NDATA_ROOF_LAYER
INTEGER                                :: NDATA_ROAD_LAYER
INTEGER                                :: NDATA_WALL_LAYER
INTEGER                                :: NDATA_FLOOR_LAYER
!
!---------------------------------------------------------------------------------
!* names of all ecosystems
!
 CHARACTER(LEN=60), DIMENSION(:,:), ALLOCATABLE :: CNAMES
!---------------------------------------------------------------------------------
!
! reference height for CDN averaging
!
REAL                                :: XCDREF = 20.
!
!---------------------------------------------------------------------------------

!
! parameters defined optionally
!
CHARACTER(LEN=28)        :: YCOVER      ! file name for cover types
CHARACTER(LEN=6)         :: YCOVERFILETYPE   ! data file type
REAL, DIMENSION(:), ALLOCATABLE :: XUNIF_COVER ! value of each cover (cover will be
!                                                   uniform on the horizontal)
REAL                     :: XRM_COVER   ! limit of coverage under which the
                                        ! cover is removed. Default is 1.E-6
REAL                     :: XRM_COAST   ! limit of coast coverage under which
                                        ! the coast is replaced by sea or
                                        ! inland water. Default is 1.
REAL                     :: XRM_LAKE    ! limit of inland lake coverage under which
                                        ! the water is removed. Default is 0.0
LOGICAL                  :: LRM_RIVER   ! delete inland river coverage. Default is false
REAL                     :: XRM_SEA     ! limit of sea coverage under which
                                        ! the sea is removed. Default is 0.0
REAL                     :: XRM_WM      ! limit of total water / total land coverage to remove
                                        ! (needed for DA of snow). Default is 0.0
LOGICAL                  :: LORCA_GRID  ! flag to compatibility between Surfex and Orca grid 
                                        ! (Earth Model over Antarctic)
REAL                     :: XLAT_ANT    ! Lattitude limit from Orca grid (Antartic)
LOGICAL                  :: LUNIF_COVER ! if the uniform cover is provided
LOGICAL                  :: LIMP_COVER  ! Imposed values for Cover from another PGD file

END MODULE MODD_DATA_COVER_PAR












