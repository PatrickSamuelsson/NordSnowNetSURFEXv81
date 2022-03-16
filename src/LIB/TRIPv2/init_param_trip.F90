!     #########
      SUBROUTINE INIT_PARAM_TRIP (TPG, &
                                   KLISTING,HFILE,KLON,KLAT,HTITLE,HTIMEUNIT)
!     #######################################################################
!
!!****  *INIT_PARAM_TRIP*  
!!
!!    PURPOSE
!!    -------
!
!     Define the name and unit of each trip parameter.
!     
!!    REFERENCE
!!    ---------
!!      
!!    AUTHOR
!!    ------
!!      B. Decharme
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    27/05/08 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
!
USE MODD_TRIP_GRID, ONLY : TRIP_GRID_t
!
USE MODE_TRIP_NETCDF
!
USE MODN_TRIP,     ONLY : CGROUNDW, CVIT, LFLOOD
USE MODD_TRIP_PAR, ONLY : XUNDEF, NDIMTAB, LNCPRINT
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE NETCDF
!
USE MODI_GET_LONLAT_TRIP
!
IMPLICIT NONE
!
!
!*      0.1    declarations of arguments
!
!
!
TYPE(TRIP_GRID_t), INTENT(INOUT) :: TPG
!
 CHARACTER(LEN=*), INTENT(IN) :: HFILE, HTITLE, HTIMEUNIT
!
INTEGER, INTENT(IN)          :: KLISTING, KLON, KLAT
!
!*      0.2    declarations of output variables
!
 CHARACTER(LEN=NF90_MAX_NAME), DIMENSION(:), ALLOCATABLE :: YVNAME  !Name of each output variable
 CHARACTER(LEN=NF90_MAX_NAME), DIMENSION(:), ALLOCATABLE :: YVLNAME !Long name of each output variables
 CHARACTER(LEN=NF90_MAX_NAME), DIMENSION(:), ALLOCATABLE :: YUNIT   !Unit of each output variable
!
 CHARACTER(LEN=NF90_MAX_NAME) :: YFILE,YTITLE,YTIMEUNIT
!
LOGICAL, DIMENSION(:), ALLOCATABLE ::  LZLEN
!
REAL,    DIMENSION(:), ALLOCATABLE ::  ZLON
REAL,    DIMENSION(:), ALLOCATABLE ::  ZLAT
LOGICAL, DIMENSION(:), ALLOCATABLE ::  LDOUBLE
!
INTEGER :: INPARAM, INCID, INUM
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
! * Number of output variable
!
IF (LHOOK) CALL DR_HOOK('INIT_PARAM_TRIP',0,ZHOOK_HANDLE)
!
INPARAM = 7
IF(CGROUNDW/='DEF'           )INPARAM = INPARAM + 1
IF(CVIT=='VAR'               )INPARAM = INPARAM + 3
IF(CGROUNDW=='DIF'           )INPARAM = INPARAM + 6
IF(LFLOOD.OR.CGROUNDW=='DIF')INPARAM = INPARAM + 1
IF(LFLOOD                   )INPARAM = INPARAM + 4
!
!
! * Allocate netcdf file attributs
!
ALLOCATE(YVNAME  (INPARAM))
ALLOCATE(YVLNAME (INPARAM))
ALLOCATE(YUNIT   (INPARAM))
ALLOCATE(LZLEN   (INPARAM))
ALLOCATE(LDOUBLE (INPARAM))
!
ALLOCATE(ZLON(KLON))
ALLOCATE(ZLAT(KLAT))
!
! * Initialyse netcdf file attributs
!
YVNAME (1) = 'FLOWDIR                    '
YVLNAME(1) = 'Flow direction             '
YUNIT  (1) = '-                          '
LZLEN  (1) = .FALSE.
LDOUBLE(1) = .FALSE.
!
YVNAME (2) = 'RIVSEQ                     '
YVLNAME(2) = 'River sequence             '
YUNIT  (2) = '-                          '
LZLEN  (2) = .FALSE.
LDOUBLE(2) = .FALSE.
!
YVNAME (3) = 'RIVLEN                     '
YVLNAME(3) = 'River length               '
YUNIT  (3) = 'm                          '
LZLEN  (3) = .FALSE.
LDOUBLE(3) = .TRUE.
!
YVNAME (4) = 'NUM_BAS                    '
YVLNAME(4) = 'Trip basin reference number'
YUNIT  (4) = '-                          '
LZLEN  (4) = .FALSE.
LDOUBLE(4) = .FALSE.
!
YVNAME (5) = 'CELL_AREA                  '
YVLNAME(5) = 'Trip cell area             '
YUNIT  (5) = 'm2                         '
LZLEN  (5) = .FALSE.
LDOUBLE(5) = .TRUE.
!
YVNAME (6) = 'GREEN_ANT                  '
YVLNAME(6) = 'Greenland/Antarctic mask   '
YUNIT  (6) = '-                          '
LZLEN  (6) = .FALSE.
LDOUBLE(6) = .FALSE.
!
YVNAME (7) = 'DR_AREA                    '
YVLNAME(7) = 'Trip drainage area         '
YUNIT  (7) = 'm2                         '
LZLEN  (7) = .FALSE.
LDOUBLE(7) = .TRUE.
!
INUM = 7
!
IF(CGROUNDW/='DEF')THEN
!   
INUM = INUM + 1
!
YVNAME (INUM) = 'TAUG                      '
YVLNAME(INUM) = 'Groundwater transfert time (0=permafrost area)'
YUNIT  (INUM) = 'days                      '
LZLEN  (INUM) = .FALSE.
LDOUBLE(INUM) = .FALSE.
!
ENDIF
!
IF(CVIT=='VAR')THEN
!
INUM = INUM + 1
YVNAME (INUM) = 'N_RIV                     '
YVLNAME(INUM) = 'Manning coefficient       '
YUNIT  (INUM) = '-                         '
LZLEN  (INUM) = .FALSE.
LDOUBLE(INUM) = .FALSE.
!
INUM = INUM + 1
YVNAME (INUM) = 'WIDTHRIV                  '
YVLNAME(INUM) = 'Stream river width        '
YUNIT  (INUM) = 'm                         '
LZLEN  (INUM) = .FALSE.
LDOUBLE(INUM) = .FALSE.
!
INUM = INUM + 1
YVNAME (INUM) = 'SLOPERIV                  '
YVLNAME(INUM) = 'Stream River slope        '
YUNIT  (INUM) = 'm/m                       '
LZLEN  (INUM) = .FALSE.
LDOUBLE(INUM) = .TRUE.
!
ENDIF
!
IF(CGROUNDW=='DIF')THEN
!   
INUM = INUM + 1
YVNAME (INUM) = 'WEFF                      '
YVLNAME(INUM) = 'Effective porosity        '
YUNIT  (INUM) = '                          '
LZLEN  (INUM) = .FALSE.
LDOUBLE(INUM) = .FALSE.
!
INUM = INUM + 1
YVNAME (INUM) = 'TRANS                     '
YVLNAME(INUM) = 'Transmissivity'
YUNIT  (INUM) = 'm2/s                      '
LZLEN  (INUM) = .FALSE.
LDOUBLE(INUM) = .FALSE.
!
INUM = INUM + 1
YVNAME (INUM) = 'NUM_AQUI                  '
YVLNAME(INUM) = 'Numero aquifere           '
YUNIT  (INUM) = '                          '
LZLEN  (INUM) = .FALSE.
LDOUBLE(INUM) = .FALSE.
!
INUM = INUM + 1
YVNAME (INUM) = 'TOPO_RIV                  '
YVLNAME(INUM) = 'River elevation           '
YUNIT  (INUM) = '                          '
LZLEN  (INUM) = .FALSE.
LDOUBLE(INUM) = .FALSE.
!
INUM = INUM + 1
!
YVNAME (INUM) = 'TABGW_F                      '
YVLNAME(INUM) = 'Potential fraction of wt rise'
YUNIT  (INUM) = '-                            '
LZLEN  (INUM) = .TRUE.
LDOUBLE(INUM) = .FALSE.
!
INUM = INUM + 1
!
YVNAME (INUM) = 'TABGW_H                      '
YVLNAME(INUM) = 'Subgrid elevation height     '
YUNIT  (INUM) = 'm                            '
LZLEN  (INUM) = .TRUE.
LDOUBLE(INUM) = .FALSE.
!
ENDIF
!
IF(LFLOOD.OR.CGROUNDW=='DIF')THEN
!
INUM = INUM + 1
YVNAME (INUM) = 'RIVDEPTH                  '
YVLNAME(INUM) = 'Stream River Depth (Hc)   '
YUNIT  (INUM) = 'm                         '
LZLEN  (INUM) = .FALSE.
LDOUBLE(INUM) = .FALSE.
!
ENDIF
!
IF(LFLOOD)THEN
!
INUM = INUM + 1
YVNAME (INUM) = 'NFLOOD                    '
YVLNAME(INUM) = 'Manning coef for flood    '
YUNIT  (INUM) = '-                         '
LZLEN  (INUM) = .FALSE.
LDOUBLE(INUM) = .FALSE.
!
INUM = INUM + 1
YVNAME (INUM) = 'TABF                      '
YVLNAME(INUM) = 'Potential flood fraction  '
YUNIT  (INUM) = '-                         '
LZLEN  (INUM) = .TRUE.
LDOUBLE(INUM) = .FALSE.
!
INUM = INUM + 1
YVNAME (INUM)= 'TABH                      '
YVLNAME(INUM)= 'Topographic height        '
YUNIT  (INUM)= 'm                         '
LZLEN  (INUM)= .TRUE.
LDOUBLE(INUM) = .FALSE.
!
INUM = INUM + 1
YVNAME (INUM)= 'TABVF                     '
YVLNAME(INUM)= 'Potential flood volume    '
YUNIT  (INUM)= 'kg/m2                     '
LZLEN  (INUM)= .TRUE.
LDOUBLE(INUM) = .FALSE.
!
ENDIF
!
! * Create netcdf file
!
YFILE     = HFILE(1:LEN_TRIM(HFILE))
YTITLE    = HTITLE(1:LEN_TRIM(HTITLE))
YTIMEUNIT = HTIMEUNIT(1:LEN_TRIM(HTIMEUNIT))
!
 CALL GET_LONLAT_TRIP(TPG, &
                     KLON,KLAT,ZLON,ZLAT)
!
IF(ALL(.NOT.LZLEN(:)))THEN
   CALL NCCREATE(KLISTING,YFILE,YTITLE,YTIMEUNIT,YVNAME,YVLNAME,YUNIT,   &
                   ZLON,ZLAT,XUNDEF,LNCPRINT,INCID,.FALSE.,ODOUBLE=LDOUBLE)  
ELSE
   CALL NCCREATE(KLISTING,YFILE,YTITLE,YTIMEUNIT,YVNAME,YVLNAME,YUNIT,   &
                   ZLON,ZLAT,XUNDEF,LNCPRINT,INCID,.FALSE.,NDIMTAB,LZLEN,ODOUBLE=LDOUBLE)  
ENDIF
!
 CALL NCCLOSE(KLISTING,LNCPRINT,YFILE,INCID)
!
! * Deallocate netcdf file attributs
!
DEALLOCATE(YVNAME  )
DEALLOCATE(YVLNAME )
DEALLOCATE(YUNIT   )
DEALLOCATE(ZLON    )
DEALLOCATE(ZLAT    )
!
IF (LHOOK) CALL DR_HOOK('INIT_PARAM_TRIP',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
END SUBROUTINE INIT_PARAM_TRIP
