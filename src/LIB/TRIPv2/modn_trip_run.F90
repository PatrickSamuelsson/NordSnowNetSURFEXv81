!#######################
MODULE  MODN_TRIP_RUN
!#######################
!
!!****  *MODN_TRIP_RUN* define the variables and namelist for TRIP
!                       driver programs
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/2013
!!      S.Sénési    08/11/16 : interface to XIOS
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
IMPLICIT NONE
!
!*    Names of model
!     --------------
!
CHARACTER(LEN=6)                  :: CMODEL_NAME  = 'trip'
!
!*    Names of files
!     --------------
!
 CHARACTER(LEN=28), PARAMETER      :: CNAMELIST    = 'TRIP_OPTIONS.nam'
 CHARACTER(LEN=15)                 :: CFILE_FRC    = 'TRIP_FORCING.nc'
!
!
!*    General flags defining forcing options
!     -------------------------------------
!
!
LOGICAL                           :: LCUMFRC  = .FALSE.  ! Cumulated (or not) forcing variables 
 CHARACTER(LEN=6)                  :: CREADFRC = 'VECTOR' ! Forcing file format
                                                         ! VECTOR = vector (normaly ilat*ilon)
                                                         ! LATLON = Regular lat lon grid
!
 CHARACTER(LEN=8)                  :: CDRAIN     = 'DRAIN'  ! Drainage name in FORCING.nc file                                                      
 CHARACTER(LEN=8)                  :: CRUNOFF    = 'RUNOFF' ! Surface runoff name in FORCING.nc file
 CHARACTER(LEN=8)                  :: CSRC_FLOOD = '      ' ! Flood source term (P-E-I) name in FORCING.nc file
!
!
!*    General flag 
!     ------------
!
LOGICAL                           :: LDIAG_MISC = .FALSE.  ! if true, more diag for model testing
LOGICAL                           :: LRESTART   = .TRUE.   ! write restart file
LOGICAL                           :: LPRINT     = .FALSE.  ! write some information in an ascii file 
LOGICAL                           :: LWR_DIAG   = .TRUE.   ! write diag file
!
!*    Time steps
!     ----------
!
REAL                              :: XTSTEP_RUN  = 86400.0
REAL                              :: XTSTEP_DIAG = 86400.0
!
!-------------------------------------------------------------------------------
!
!*       1.    NAMELISTS
!              ---------
!
NAMELIST/NAM_TRIP_RUN/CREADFRC,CDRAIN,CRUNOFF,LCUMFRC,LDIAG_MISC, &
                      LPRINT,LRESTART,XTSTEP_RUN,XTSTEP_DIAG,     &
                      LWR_DIAG
!
!-------------------------------------------------------------------------------
END MODULE MODN_TRIP_RUN
