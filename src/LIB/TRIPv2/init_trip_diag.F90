!     #########
      SUBROUTINE INIT_TRIP_DIAG (TPDG, TPG, &
                                  KLISTING,HFILE,KLON,KLAT,HTITLE,HTIMEUNIT,OTIME)
!     #######################################################################
!
!!****  *INIT_TRIP_DIAG*  
!!
!!    PURPOSE
!!    -------
!
!     Define the name and unit of each trip output variable.
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
USE MODD_TRIP_DIAG, ONLY : TRIP_DIAG_t
USE MODD_TRIP_GRID, ONLY : TRIP_GRID_t
!
USE MODE_TRIP_NETCDF
!
USE MODN_TRIP_RUN,   ONLY : LDIAG_MISC
USE MODN_TRIP,       ONLY : CGROUNDW, CVIT, LFLOOD
!
USE MODD_TRIP_OASIS, ONLY : LCPL_LAND
!
USE MODD_TRIP_PAR, ONLY : XUNDEF, LNCPRINT
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
TYPE(TRIP_DIAG_t), INTENT(INOUT) :: TPDG
TYPE(TRIP_GRID_t), INTENT(INOUT) :: TPG
!
 CHARACTER(LEN=*), INTENT(IN) :: HFILE, HTITLE, HTIMEUNIT
!
INTEGER, INTENT(IN)          :: KLISTING, KLON, KLAT
!
LOGICAL, INTENT(IN)          :: OTIME
!
!*      0.2    declarations of output variables
!
 CHARACTER(LEN=NF90_MAX_NAME), DIMENSION(:), ALLOCATABLE :: YVNAME  !Name of each output variable
 CHARACTER(LEN=NF90_MAX_NAME), DIMENSION(:), ALLOCATABLE :: YVLNAME !Long name of each output variables
 CHARACTER(LEN=NF90_MAX_NAME), DIMENSION(:), ALLOCATABLE :: YUNIT   !Unit of each output variable
!
 CHARACTER(LEN=NF90_MAX_NAME) :: YFILE,YTITLE,YTIMEUNIT
!
REAL, DIMENSION(:), ALLOCATABLE ::  ZLON
REAL, DIMENSION(:), ALLOCATABLE ::  ZLAT
!
INTEGER :: INDIAG, INCID, INUM
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
! * Number of output variable
!
IF (LHOOK) CALL DR_HOOK('INIT_TRIP_DIAG',0,ZHOOK_HANDLE)
INUM   = 0
INDIAG = 2
IF(LDIAG_MISC) INDIAG = INDIAG + 1
IF(LCPL_LAND.AND.LDIAG_MISC) INDIAG = INDIAG + 2
IF(CVIT=='VAR')     INDIAG = INDIAG + 2
IF(CGROUNDW/='DEF') INDIAG = INDIAG + 2
IF(CGROUNDW=='DIF')THEN 
  INDIAG = INDIAG + 3
  IF(LDIAG_MISC)INDIAG = INDIAG + 2
ENDIF
IF(LFLOOD)THEN
  INDIAG = INDIAG + 3
  IF(LDIAG_MISC) INDIAG = INDIAG + 8
ENDIF
!
! * Allocate netcdf file attributs
!
ALLOCATE(YVNAME  (INDIAG))
ALLOCATE(YVLNAME (INDIAG))
ALLOCATE(YUNIT   (INDIAG))
!
ALLOCATE(ZLON(KLON))
ALLOCATE(ZLAT(KLAT))
!
! * Initialyse netcdf file attributs
!
INUM = INUM + 1
YVNAME (INUM) = 'SURF_STO                  '
YVLNAME(INUM) = 'River storage             '
YUNIT  (INUM) = 'kg m-2                    '
!
INUM = INUM + 1
YVNAME (INUM) = 'QDIS                      '
YVLNAME(INUM) = 'Discharge                 '
YUNIT  (INUM) = 'm3 s-1                    '
!
IF(LDIAG_MISC)THEN
! 
  INUM = INUM + 1
  YVNAME (INUM) = 'QSIN                      '
  YVLNAME(INUM) = 'Inflow to the river       '
  YUNIT  (INUM) = 'm3 s-1                    '
! 
ENDIF
!
IF(LCPL_LAND.AND.LDIAG_MISC)THEN
! 
  INUM = INUM + 1
  YVNAME (INUM) = 'RUNOFF                    '
  YVLNAME(INUM) = 'Input surface runoff (can be used to force TRIP offline)'
  YUNIT  (INUM) = 'mm of water               '
! 
  INUM = INUM + 1
  YVNAME (INUM) = 'DRAIN                     '
  YVLNAME(INUM) = 'Input drainage or recharge (can be used to force TRIP offline)'
  YUNIT  (INUM) = 'mm of water               '  
! 
ENDIF
!
IF(CGROUNDW/='DEF')THEN
!        
  INUM = INUM + 1
  YVNAME (INUM) = 'QGF'
  YVLNAME(INUM) = 'Groundwater-river exchange'
  YUNIT  (INUM) = 'm3 s-1                    '
!
  INUM = INUM + 1
  YVNAME (INUM) = 'GROUND_STO                '
  IF(CGROUNDW=='CST')THEN
    YVLNAME(INUM) = 'Groundwater storage     '
  ELSEIF(CGROUNDW=='DIF')THEN
    YVLNAME(INUM) = 'Groundwater mass equivalent'
  ENDIF
  YUNIT  (INUM) = 'kg m-2                    '
!
ENDIF
!
IF(CGROUNDW=='DIF')THEN
!
  INUM = INUM + 1
  YVNAME (INUM) = 'HGROUND                   '
  YVLNAME(INUM) = 'Groundwater height        '
  YUNIT  (INUM) = 'm                         '
!
  INUM = INUM + 1
  YVNAME (INUM) = 'FWTD                      '
  YVLNAME(INUM) = 'grid-cell fraction of wtd '
  YUNIT  (INUM) = '-                         '
!
  INUM = INUM + 1
  YVNAME (INUM) = 'WTD                       '
  YVLNAME(INUM) = 'Wat Tab Depth for coupling'
  YUNIT  (INUM) = 'm                         '
!
  IF(LDIAG_MISC)THEN
!
    INUM = INUM + 1
    YVNAME (INUM) = 'QGCELL                    ' 
    YVLNAME(INUM) = 'Grid-cell fluxes budget   '
    YUNIT  (INUM) = 'm3 s-1                    '
!
    INUM = INUM + 1
    YVNAME (INUM)= 'HGHRIV                     '
    YVLNAME(INUM)= 'Hground - Hriver           '
    YUNIT  (INUM)= 'm                          '
!
  ENDIF
!
ENDIF
!
IF(CVIT=='VAR')THEN
!
  INUM = INUM + 1
  YVNAME (INUM) = 'VEL                       '
  YVLNAME(INUM) = 'Stream flow velocity      '
  YUNIT  (INUM) = 'm s-1                     '
!
  INUM = INUM + 1
  YVNAME (INUM) = 'HSTREAM                   '
  YVLNAME(INUM) = 'Stream river height       '
  YUNIT  (INUM) = 'm                         '
!
ENDIF
!
IF(LFLOOD)THEN
!        
  INUM = INUM + 1
  YVNAME (INUM) = 'FLOOD_STO                 '
  YVLNAME(INUM) = 'Floodplain storage        '
  YUNIT  (INUM) = 'kg m-2                    '
!
  INUM = INUM + 1
  YVNAME (INUM) = 'FFLOOD                    '
  YVLNAME(INUM) = 'TRIP flooded fraction     '
  YUNIT  (INUM) = '-                         '
!
  INUM = INUM + 1
  YVNAME (INUM) = 'HFLOOD                    '
  YVLNAME(INUM) = 'Flood depth               '
  YUNIT  (INUM) = 'm                         '
!
  IF(LDIAG_MISC)THEN
!
    INUM = INUM + 1
    YVNAME (INUM)= 'FSOURCE                      '
    YVLNAME(INUM)= 'Floodplains source (Pf-Ef-If) (can be used to force TRIP offline)'
    YUNIT  (INUM)= 'mm of water                  '
!
    INUM = INUM + 1
    YVNAME (INUM)= 'VFIN                      '
    YVLNAME(INUM)= 'River to flood velocity   '
    YUNIT  (INUM)= 'm s-1                     '
!
    INUM = INUM + 1
    YVNAME (INUM)= 'QRF                       '
    YVLNAME(INUM)= 'River flow to floodplain  '
    YUNIT  (INUM)= 'm3 s-1                    '
!
    INUM = INUM + 1
    YVNAME (INUM)= 'VFOUT                     '
    YVLNAME(INUM)= 'Flood to river velocity   '
    YUNIT  (INUM)= 'm s-1                     '
!
    INUM = INUM + 1
    YVNAME (INUM)= 'QFR                       '
    YVLNAME(INUM)= 'Flood flow to river       '
    YUNIT  (INUM)= 'm3 s-1                    '
!
    INUM = INUM + 1
    YVNAME (INUM)= 'HSF                         '
    YVLNAME(INUM)= 'River-Flood depth comparison'
    YUNIT  (INUM)= 'm                           '
!
    INUM = INUM + 1
    YVNAME (INUM)= 'WF                          '
    YVLNAME(INUM)= 'Flood width during dt       '
    YUNIT  (INUM)= 'm                           '
!
    INUM = INUM + 1
    YVNAME (INUM)= 'LF                          '
    YVLNAME(INUM)= 'Flood lenght during dt      '
    YUNIT  (INUM)= 'm                           '
!
  ENDIF
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
 CALL NCCREATE(KLISTING,YFILE,YTITLE,YTIMEUNIT,YVNAME,YVLNAME,YUNIT,ZLON,ZLAT,XUNDEF,LNCPRINT,INCID,OTIME)
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
IF (LHOOK) CALL DR_HOOK('INIT_TRIP_DIAG',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
END SUBROUTINE INIT_TRIP_DIAG
