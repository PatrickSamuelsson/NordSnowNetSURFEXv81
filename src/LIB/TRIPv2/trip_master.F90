!###################################################################
PROGRAM TRIP_MASTER
!###################################################################
!
!
!!****  *TRIP_MASTER*  
!!
!!    PURPOSE
!!    -------
!!   
!!    Driver for TRIP from CNRM
!!      
!!    REFERENCE
!!    ---------
!!      
!!    AUTHOR
!!    ------
!!      B. Decharme
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/08
!!      S.Sénési    08/11/16 : interface to XIOS 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURFEX_TRIP_n
USE MODD_OFF_TRIP_n
!
USE MODD_TRIP_LISTING
!
USE MODN_TRIP_RUN, ONLY : LRESTART, LPRINT, LWR_DIAG,  &
                          XTSTEP_RUN, XTSTEP_DIAG
!
USE MODD_TRIP_PAR, ONLY : XUNDEF, NUNDEF, XDAY
!
USE MODE_RW_TRIP
!
USE MODI_READ_NAM_TRIP_RUN
USE MODI_READ_NAM_TRIP
USE MODI_READ_NAM_TRIP_GRID
!
USE MODI_ABORT_TRIP
USE MODI_GET_TRIP_GRID_CONF
!
USE MODI_INIT_TRIP
USE MODI_INIT_TRIP_PAR
USE MODI_TRIP_RUN_CONF
USE MODI_TRIP_RESTART
USE MODI_TRIP_DIAG_RUN
USE MODI_TRIP_RUN
!
USE MODI_TRIP_OASIS_INIT
USE MODI_TRIP_OASIS_READ_NAM
USE MODI_TRIP_OASIS_DEFINE
USE MODI_TRIP_OASIS_END
!
USE MODI_TRIP_XIOS_INIT
!
#ifdef SFX_MPI
#ifdef SFX_MPL
USE MPL_DATA_MODULE, ONLY : LMPLUSERCOMM, MPLUSERCOMM
#endif
#endif
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#if defined CPLOASIS || defined WXIOS
INCLUDE 'mpif.h'
#endif
!
INTEGER                           :: IYEAR               ! current year         (UTC)
INTEGER                           :: IMONTH              ! current month        (UTC)
INTEGER                           :: IDAY                ! current day          (UTC)
REAL                              :: ZTIME               ! current time           (s)
REAL                              :: ZRUNTIME            ! total simulated time   (s)
!
INTEGER                           :: INB_TSTEP_RUN       ! number of time step in the run
INTEGER                           :: ILON                ! Number of longitude
INTEGER                           :: ILAT                ! Number of latittude
!
INTEGER                           :: INB_OL              ! number of time step if forcing offline
INTEGER                           :: ILON_OL             ! Number of longitude if forcing offline
INTEGER                           :: ILAT_OL             ! Number of latittude if forcing offline
!
INTEGER                           :: IERR                ! Error value
INTEGER                           :: INPROC              ! Number of processes
INTEGER                           :: IRANK               ! Local process number
INTEGER                           :: ILOCAL_COMM         ! Local communicator
LOGICAL                           :: GOASIS              ! OASIS used(default=.false.)
LOGICAL                           :: GXIOS               ! XIOS used(default=.false.)
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! --------------------------------------------------------------------------------------
! * 0. MPI and OASIS must be initialized before any DR_HOOK call
! --------------------------------------------------------------------------------------
!
 CALL TRIP_OASIS_INIT(GOASIS,GXIOS,ILOCAL_COMM,PRUNTIME=ZRUNTIME)
!
#ifdef SFX_MPI
#ifdef SFX_MPL
IF (ILOCAL_COMM/=0) THEN
  LMPLUSERCOMM = .TRUE.
  MPLUSERCOMM = ILOCAL_COMM
ENDIF
#endif
#endif
!
! --------------------------------------------------------------------------------------
! * 1. Alloc trip variables and open listing
! --------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_MASTER',0,ZHOOK_HANDLE)
!
 CALL TRIP_ALLOC_LIST(1)
!
 CALL INIT_TRIP_PAR
!
OPEN(UNIT=NLISTING,FILE=CLISTING,FORM='FORMATTED',ACTION='WRITE')
!
! --------------------------------------------------------------------------------------
! * 2. Check run attributes
! --------------------------------------------------------------------------------------
!
!Inquire if trip is parallel or not: TRIP is only a monoprocess model for now
!
INPROC = NUNDEF
IRANK  = NUNDEF
!
#if defined CPLOASIS || defined WXIOS
IF (ILOCAL_COMM==0) THEN
  ILOCAL_COMM = MPI_COMM_WORLD
ENDIF
 CALL MPI_COMM_SIZE(ILOCAL_COMM,INPROC,IERR)
 CALL MPI_COMM_RANK(ILOCAL_COMM,IRANK,IERR)  
!
IF(INPROC==NUNDEF.OR.IRANK==NUNDEF)THEN
  WRITE(NLISTING,*)'TRIP_MASTER: PROBLEM WITH MPI, INPROC = ',INPROC
  WRITE(NLISTING,*)'TRIP_MASTER: PROBLEM WITH MPI, IRANK  = ',IRANK
  CALL ABORT_TRIP('TRIP_MASTER: PROBLEM WITH MPI')
ENDIF
#endif
!
IF(INPROC>1.AND.INPROC<NUNDEF)THEN
  WRITE(NLISTING,*)'TRIP_MASTER: TRIP NOT YET PARALLELIZED, NPROC SHOULD BE 1'
  CALL ABORT_TRIP('TRIP_MASTER: TRIP NOT YET PARALLELIZED')
ENDIF
!
IF(GOASIS)THEN
  WRITE(NLISTING,*) '!!!!!!!!!!!!!!!!!!!!!!!'
  WRITE(NLISTING,*) '    OASIS is used      ' 
  WRITE(NLISTING,*) '                       '
  WRITE(NLISTING,*) 'Number of processes   :', INPROC
  WRITE(NLISTING,*) 'Local process number  :', IRANK
  WRITE(NLISTING,*) 'Local communicator    :', ILOCAL_COMM
  WRITE(NLISTING,*) '!!!!!!!!!!!!!!!!!!!!!!!'
  WRITE(NLISTING,*) '                       '
ELSE
  WRITE(NLISTING,*) '!!!!!!!!!!!!!!!!!!!!!!!'
  WRITE(NLISTING,*) '   TRIP run offline    ' 
  WRITE(NLISTING,*) '!!!!!!!!!!!!!!!!!!!!!!!'
  WRITE(NLISTING,*) '                       '
ENDIF
!
! --------------------------------------------------------------------------------------
! * 3. read namelists
! --------------------------------------------------------------------------------------
!
 CALL READ_NAM_TRIP_RUN(NLISTING)
!
 CALL READ_NAM_TRIP(NLISTING)
!
IF(GOASIS)THEN
  CALL TRIP_OASIS_READ_NAM(NLISTING,ZRUNTIME)
ENDIF
!
! --------------------------------------------------------------------------------------
! * 4. TRIP initializations
! --------------------------------------------------------------------------------------
!
YTRIP_CUR => YTRIP_LIST(1)
!
 CALL READ_NAM_TRIP_GRID(YTRIP_CUR%TPG,NLISTING)
!
 CALL INIT_TRIP(YTRIP_CUR%TPDG, YTRIP_CUR%TP, YTRIP_CUR%TPG, &
               IYEAR,IMONTH,IDAY,ZTIME,ILON,ILAT,XTSTEP_RUN, &
               XTSTEP_DIAG,LRESTART,GXIOS)
!
! --------------------------------------------------------------------------------------
! * 5. TRIP - OASIS  grid, partitions and local field definitions
! --------------------------------------------------------------------------------------
!
IF(GOASIS)THEN
  CALL TRIP_OASIS_DEFINE(NLISTING,ILON,ILAT)
ENDIF
!
! --------------------------------------------------------------------------------------
! * 5.2 XIOS init
! --------------------------------------------------------------------------------------
!
#ifdef WXIOS
IF (GXIOS) THEN 
   CALL TRIP_XIOS_INIT(YTRIP_CUR%TPG,ILOCAL_COMM,ILON,ILAT,&
                       IYEAR,IMONTH,IDAY,ZTIME)
ENDIF
#endif
!
! --------------------------------------------------------------------------------------
! * 6. Get run configuration
! --------------------------------------------------------------------------------------
!
 CALL TRIP_RUN_CONF(NLISTING,GOASIS,IYEAR,IMONTH,IDAY,ZTIME, &
                   ILON,ILAT,INB_TSTEP_RUN,ZRUNTIME         )
!
IF(GOASIS)THEN
  INB_OL  = 0
  ILON_OL = 0
  ILAT_OL = 0
ELSE
  INB_OL  = INB_TSTEP_RUN
  ILON_OL = ILON
  ILAT_OL = ILAT
ENDIF
!
! --------------------------------------------------------------------------------------
! * 7. Read and prepare drainage and runoff if offline
! --------------------------------------------------------------------------------------
!
 CALL TRIP_RUN(YTRIP_CUR%TPDG, YTRIP_CUR%TP, YTRIP_CUR%TPG, &
              GOASIS,GXIOS,                     &
              NLISTING,ILON,ILAT,INB_TSTEP_RUN, &
              ZRUNTIME,ILON_OL,ILAT_OL,INB_OL,  &
              IYEAR,IMONTH,IDAY,ZTIME           )
!
!-------------------------------------------------------------------------------
! * 9. Store run mean diagnostic and write restart
!-------------------------------------------------------------------------------
!
IF (GXIOS) THEN 
   CALL WRITE_TRIP(NLISTING,'dummy.nc','areacellr',YTRIP_CUR%TPG%GMASK,YTRIP_CUR%TPG%XAREA,OXIOS=.TRUE.)
ELSEIF(LWR_DIAG)THEN
   CALL TRIP_DIAG_RUN(YTRIP_CUR%TPDG, YTRIP_CUR%TPG, &
                      NLISTING,ILON,ILAT,ZRUNTIME)
ENDIF
!
IF(LRESTART)THEN
   CALL TRIP_RESTART(YTRIP_CUR%TP, YTRIP_CUR%TPG, &
                     NLISTING,IYEAR,IMONTH,IDAY,ZTIME,ILON,ILAT)
ENDIF
!
! --------------------------------------------------------------------------------------
! * 10. End of run
! --------------------------------------------------------------------------------------
!
 CLOSE(NLISTING)
!
WRITE(*,*) ' '
WRITE(*,*) '    ------------------------------'
WRITE(*,*) '    | TRIP MASTER ENDS CORRECTLY |'
WRITE(*,*) '    ------------------------------'
WRITE(*,*) ' '
!
 CALL TRIP_DEALLO_LIST
!
IF (LHOOK) CALL DR_HOOK('TRIP_MASTER',1,ZHOOK_HANDLE)
!
! --------------------------------------------------------------------------------------
! * 11. MPI and OASIS must be finalized after the last DR_HOOK call
! --------------------------------------------------------------------------------------
!
CALL TRIP_OASIS_END(GOASIS,GXIOS)
!
!-------------------------------------------------------------------------------
END PROGRAM TRIP_MASTER
