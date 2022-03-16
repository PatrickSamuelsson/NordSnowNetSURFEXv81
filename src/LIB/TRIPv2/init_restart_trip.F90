!     #########
      SUBROUTINE INIT_RESTART_TRIP (TPG, &
                                     KLISTING,HFILE,KLON,KLAT,HTITLE,HTIMEUNIT,OTIME)
!     #######################################################################
!
!!****  *INIT_RESTART_TRIP*  
!!
!!    PURPOSE
!!    -------
!
!     Define the name and unit of each trip restart variables.
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
USE MODN_TRIP,     ONLY : CGROUNDW, LFLOOD
USE MODD_TRIP_PAR, ONLY : XUNDEF, LNCPRINT
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
LOGICAL, INTENT(IN)          :: OTIME
!
!*      0.2    declarations of restart variables
!
 CHARACTER(LEN=NF90_MAX_NAME), DIMENSION(:), ALLOCATABLE :: YVNAME  !Name of each restart variable
 CHARACTER(LEN=NF90_MAX_NAME), DIMENSION(:), ALLOCATABLE :: YVLNAME !Long name of each restart variables
 CHARACTER(LEN=NF90_MAX_NAME), DIMENSION(:), ALLOCATABLE :: YUNIT   !Unit of each restart variable
!
 CHARACTER(LEN=NF90_MAX_NAME) :: YFILE,YTITLE,YTIMEUNIT
!
REAL,    DIMENSION(:), ALLOCATABLE ::  ZLON
REAL,    DIMENSION(:), ALLOCATABLE ::  ZLAT
LOGICAL, DIMENSION(:), ALLOCATABLE ::  LDOUBLE
!
INTEGER :: IND, INCID, INUM
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
! * Number of restart variable
!
IF (LHOOK) CALL DR_HOOK('INIT_RESTART_TRIP',0,ZHOOK_HANDLE)
IND = 1
IF(CGROUNDW=='CST'.OR.CGROUNDW=='DIF') IND = IND + 1
IF(LFLOOD)IND = IND + 3
!
! * Allocate netcdf file attributs
!
ALLOCATE(YVNAME  (IND))
ALLOCATE(YVLNAME (IND))
ALLOCATE(YUNIT   (IND))
ALLOCATE(LDOUBLE (IND))
LDOUBLE(:)=.TRUE.
!
ALLOCATE(ZLON(KLON))
ALLOCATE(ZLAT(KLAT))
!
! * Initialyse netcdf file attributs
!
YVNAME (1) = 'SURF_STO                  '
YVLNAME(1) = 'River storage             '
YUNIT  (1) = 'kg                        '
!
INUM = 1
!
IF(CGROUNDW=='CST')THEN
!        
INUM = INUM + 1
YVNAME (INUM) = 'GROUND_STO                '
YVLNAME(INUM) = 'Groundwater storage       '
YUNIT  (INUM) = 'kg                        '
!
ELSEIF(CGROUNDW=='DIF')THEN
!        
INUM = INUM + 1
YVNAME (INUM) = 'HGROUND                   '
YVLNAME(INUM) = 'Groundwater height        '
YUNIT  (INUM) = 'm                         '
!
ENDIF
!
IF(LFLOOD)THEN
!
INUM = INUM + 1
YVNAME (INUM) = 'FLOOD_STO                 '
YVLNAME(INUM) = 'Floodplain storage        '
YUNIT  (INUM) = 'kg                        '

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
 CALL NCCREATE(KLISTING,YFILE,YTITLE,YTIMEUNIT,YVNAME,YVLNAME,YUNIT,ZLON,ZLAT, &
              XUNDEF,LNCPRINT,INCID,OTIME,ODOUBLE=LDOUBLE)
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
IF (LHOOK) CALL DR_HOOK('INIT_RESTART_TRIP',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
END SUBROUTINE INIT_RESTART_TRIP
