!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
! *****************************************************************************************
PROGRAM SODA

!
! ------------------------------------------------------------------------------------------
!!
!!    SODA: SURFEX Offline Data Assimilation
!!
!!    PURPOSE
!!    -------
!!    Program to perform surface data assimilation within SURFEX 
!!
!!
!!    METHOD
!!    ------
!!    Different methods for different tiles
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
!!    T. Aspelien                  met.no
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original         04/2012
!!
!! 03/2014 E. Martin change indices names in OMP module according to GMAP changes
!  05/2013 B. Decharme New coupling variables XTSURF (for AGCM)
!  02/2016 B. Decharme MODD_IO_SURF_ARO not used
!  09/2016 S. Munier XTSTEP_OUTPUT set to 0
!----------------------------------------------------------------------------
!
USE MODD_ISBA_n, ONLY : ISBA_P_t, ISBA_PE_t
!
USE MODD_OFF_SURFEX_n
USE MODE_MODELN_SURFEX_HANDLER
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, WLOG_MPI, PREP_LOG_MPI, NPROC, NCOMM,   &
                            NINDEX, NSIZE_TASK, END_LOG_MPI, NSIZE
!
USE MODD_MASK, ONLY: NMASK_FULL
!
USE MODD_TYPE_DATE_SURF, ONLY : DATE_TIME, DATE
!
USE MODD_WRITE_SURF_ATM, ONLY : LFIRST_WRITE, NCPT_WRITE
!
USE MODD_SURF_CONF, ONLY : CPROGNAME, CSOFTWARE
USE MODD_SURF_PAR,  ONLY : XUNDEF,NUNDEF
USE MODD_PREP_SNOW,  ONLY : NIMPUR
!
USE MODD_ASSIM, ONLY : LASSIM, LAROME, LALADSURF, CASSIM_ISBA, NVAR, XF, XF_PATCH,  &
                       NOBSTYPE, XAT2M_ISBA, XAHU2M_ISBA, CVAR, COBS, NECHGU, XI,   &
                       XLAI_PASS, XBIO_PASS, CBIO, NIVAR, XYO, NIFIC, NPRINTLEV,    &
                       NOBS, NPRINTLEV, LREAD_ALL, NENS,LBIAS_CORRECTION,           &
                       LEXTRAP_SEA,LEXTRAP_WATER,LEXTRAP_NATURE,LOBSHEADER,NOBSMAX, &
                       CFILE_FORMAT_FG,CFILE_FORMAT_LSM,CFILE_FORMAT_OBS,           &
                       CFILE_FORMAT_CLIM,CFILE_FORMAT_SST,NNCO,LWATERTG2,LOBSNAT,   &
                       LSWE,LREAD_SST_FROM_FILE,CASSIM_SEA,LAESST,LPIO
!
USE MODD_FORC_ATM,       ONLY : CSV, XDIR_ALB, XSCA_ALB, XEMIS, XTSRAD, XTSUN, XZS, &
                                XZREF, XUREF, XTA, XQA, XSV, XU, XV, XSW_BANDS,     &
                                XZENITH, XAZIM, XCO2,XIMPWET,XIMPDRY, XRHOA, XTSURF
!
USE MODD_WRITE_BIN,  ONLY : NWRITE
!
#ifdef SFX_OL
USE MODD_IO_SURF_OL, ONLY : XSTART, XCOUNT, XSTRIDE, XSTARTW, XCOUNTW, &
                            LTIME_WRITTEN, LPARTW, LDEF_ol=>LDEF
                            
#endif
!
#ifdef SFX_NC
USE MODD_IO_SURF_NC,   ONLY : CFILEIN_NC, CFILEIN_NC_SAVE, CFILEPGD_NC, CFILEOUT_NC, &
                              LDEF_nc=>LDEF, CLUOUT_NC
#endif
#ifdef SFX_ASC
USE MODD_IO_SURF_ASC,  ONLY : CFILEIN, CFILEIN_SAVE, CFILEPGD, CFILEOUT, LCREATED
#endif
#ifdef SFX_FA
USE MODD_IO_SURF_FA,   ONLY : CFILEIN_FA, CFILEIN_FA_SAVE, CFILEPGD_FA, CDNOMC, CFILEOUT_FA, &
                              NUNIT_FA, IVERBFA, LFANOCOMPACT
USE MODE_WRITE_SURF_FA, ONLY : FAIDX_WRT
#endif
#ifdef SFX_LFI
USE MODD_IO_SURF_LFI,    ONLY : CFILEIN_LFI, CFILEIN_LFI_SAVE, &
                                CFILEPGD_LFI, CFILE_LFI, CLUOUT_LFI, CFILEOUT_LFI ,LMNH_COMPATIBLE
#endif
!
USE MODD_SURF_ATM_TURB_n, ONLY : SURF_ATM_TURB_t
!
USE MODN_IO_OFFLINE,     ONLY : NAM_IO_OFFLINE, CNAMELIST, CPGDFILE, CPREPFILE, CSURFFILE, &
                                CSURF_FILETYPE, CTIMESERIES_FILETYPE, LLAND_USE, YALG_MPI, &
                                LDIAG_FA_NOCOMPACT, LOUT_TIMENAME, XIO_FRAC, LRESTART_2M,  &
                                XTSTEP_OUTPUT, LFAGMAP
!
USE MODE_POS_SURF,  ONLY : POSNAM
!
USE MODI_SURFEX_ALLOC
USE MODI_DEALLOC_SURF_ATM_N
USE MODI_INIT_OUTPUT_OL_N
USE MODI_SET_SURFEX_FILEIN
USE MODI_INIT_INDEX_MPI
USE MODI_GATHER_AND_WRITE_MPI
USE MODI_READ_AND_SEND_MPI
USE MODI_ABOR1_SFX
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_READ_ALL_NAMELISTS
USE MODI_INIT_IO_SURF_n
USE MODI_END_IO_SURF_n
USE MODI_READ_SURF
USE MODI_IO_BUFF_CLEAN
USE MODI_UNPACK_SAME_RANK
USE MODI_GET_SIZE_FULL_n
USE MODI_INIT_SURF_ATM_n
USE MODI_ASSIM_SURF_ATM_n
USE MODI_ASSIM_READ_FIELD
USE MODI_WRITE_SURF_ATM_n
USE MODI_WRITE_DIAG_SURF_ATM_n
USE MODI_ADD_FORECAST_TO_DATE_SURF
USE MODI_FLAG_UPDATE
USE MODI_FLAG_DIAG_UPDATE
USE MODI_TEST_NAM_VAR_SURF
USE MODI_INIT_OUTPUT_NC_n
USE MODI_CLOSE_FILEOUT_OL
USE MODI_WRITE_HEADER_MNH
!
USE MODE_EKF, ONLY : GET_FILE_NAME, SET_FILEIN
!
USE YOMHOOK,             ONLY : LHOOK,DR_HOOK
USE PARKIND1,            ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE 'mpif.h'
#endif
!
!*    0.     Declaration of local variables
!            ------------------------------
!
TYPE(ISBA_P_t), POINTER :: PK
TYPE(ISBA_PE_t), POINTER :: PEK
!
TYPE(DATE) :: TDATE_END
!
 CHARACTER(LEN=200) :: YMFILE     ! Name of the observation, perturbed or reference file!
 CHARACTER(LEN=3)  :: YINIT
 CHARACTER(LEN=2), PARAMETER  :: YTEST        = 'OK'          ! must be equal to 'OK'
 CHARACTER(LEN=28)            :: YATMFILE  ='   '  ! name of the Atmospheric file
 CHARACTER(LEN=6)             :: YATMFILETYPE ='      '                     ! type of the Atmospheric file
 CHARACTER(LEN=28)            :: YLUOUT    ='LISTING_SODA                '  ! name of listing
 CHARACTER(LEN=28)            :: YOBS
 CHARACTER(LEN=28)            :: YFILEIN
 CHARACTER(LEN=3)             :: YVAR
!
 CHARACTER(LEN=100) :: YNAME
 CHARACTER(LEN=10)  :: YRANK
 CHARACTER(LEN=3) :: YENS
!
REAL, ALLOCATABLE, DIMENSION(:,:) :: ZYO_NAT
REAL, ALLOCATABLE, DIMENSION(:) :: ZNATURE
!
REAL,ALLOCATABLE, DIMENSION(:)   :: ZWORK
REAL,ALLOCATABLE, DIMENSION(:)   :: ZLSM                ! Land-Sea mask
REAL,ALLOCATABLE, DIMENSION(:)   :: ZCON_RAIN           ! Amount of convective liquid precipitation
REAL,ALLOCATABLE, DIMENSION(:)   :: ZSTRAT_RAIN         ! Amount of stratiform liquid precipitation
REAL,ALLOCATABLE, DIMENSION(:)   :: ZCON_SNOW           ! Amount of convective solid precipitation
REAL,ALLOCATABLE, DIMENSION(:)   :: ZSTRAT_SNOW         ! Amount of stratiform solid precipitation
REAL,ALLOCATABLE, DIMENSION(:)   :: ZCLOUDS             ! Cloudcover
REAL,ALLOCATABLE, DIMENSION(:)   :: ZEVAPTR             ! Evaporation
REAL,ALLOCATABLE, DIMENSION(:)   :: ZEVAP               ! Evaporation
REAL,ALLOCATABLE, DIMENSION(:)   :: ZTSC                ! Climatological surface temperature
REAL,ALLOCATABLE, DIMENSION(:)   :: ZTS                 ! Surface temperature
REAL,ALLOCATABLE, DIMENSION(:)   :: ZT2M                ! Screen level temperature
REAL,ALLOCATABLE, DIMENSION(:)   :: ZHU2M               ! Screen level relative humidity
REAL,ALLOCATABLE, DIMENSION(:)   :: ZSNC
REAL,ALLOCATABLE, DIMENSION(:)   :: ZSWE                ! Snow water equvivalent (amount of snow on the ground)
REAL,ALLOCATABLE, DIMENSION(:)   :: ZSWEC               ! Climatological snow water equvivalent (amount of snow on the ground)
REAL,ALLOCATABLE, DIMENSION(:)   :: ZUCLS
REAL,ALLOCATABLE, DIMENSION(:)   :: ZVCLS
REAL,ALLOCATABLE, DIMENSION(:)   :: ZSST                ! SST from external file
REAL,ALLOCATABLE, DIMENSION(:)   :: ZSIC                ! SIC from external file
REAL,ALLOCATABLE, DIMENSION(:)   :: ZLAT
REAL,ALLOCATABLE, DIMENSION(:)   :: ZLON
!
REAL    :: ZTIME
REAL    :: ZTIME_OUT           ! output time since start of the run (s)
!
LOGICAL, ALLOCATABLE, DIMENSION(:) :: GD_MASKEXT
LOGICAL :: GLKEEPEXTZONE
LOGICAL :: GFOUND
!
TYPE (DATE_TIME)                 :: TTIME               ! Current date and time  
!
CHARACTER(LEN=14)                :: YTAG
CHARACTER(LEN=6)                 :: YMASK
!
INTEGER, DIMENSION(11)  :: IDATEF
INTEGER :: IDIM_FULL
INTEGER :: ISV                 ! Number of scalar species
INTEGER :: ISW                 ! Number of radiative bands 
INTEGER :: IYEAR, IMONTH, IDAY, IHOUR
INTEGER :: IYEAR_OUT, IMONTH_OUT, IDAY_OUT
INTEGER :: JL,JI,JJ,JP,INB,ICPT, IMASK
INTEGER :: INW, JNW
INTEGER :: ISTEP
INTEGER :: IOBS
INTEGER :: IGPCOMP
INTEGER :: ILUOUT
INTEGER :: ILUNAM
INTEGER :: IRET, INBFA
INTEGER :: IRESP, ISTAT               ! Response value
INTEGER :: INFOMPI, ILEVEL
INTEGER :: ISIZE, IENS, ISIZE_FULL
!
INTEGER :: ISIZE_NATURE, INPATCH
!
! Flag diag :
!
INTEGER :: I2M, IBEQ, IDSTEQ
LOGICAL :: GFRAC, GDIAG_GRID, GSURF_BUDGET, GRAD_BUDGET, GCOEF,    &
           GSURF_VARS, GDIAG_OCEAN, GDIAG_SEAICE, GWATER_PROFILE, &
           GSURF_EVAP_BUDGET, GFLOOD,  GPGD_ISBA, GCH_NO_FLUX_ISBA,&
           GSURF_MISC_BUDGET_ISBA, GPGD_TEB, GSURF_MISC_BUDGET_TEB
!
TYPE(SURF_ATM_TURB_t) :: AT         ! atmospheric turbulence parameters
!
REAL(KIND=JPRB)                  :: ZHOOK_HANDLE
! ******************************************************************************************
!
INFOMPI=1
!
#ifdef SFX_MPI
 CALL MPI_INIT_THREAD(MPI_THREAD_MULTIPLE,ILEVEL,INFOMPI)
#endif
!
IF (LHOOK) CALL DR_HOOK('SODA',0,ZHOOK_HANDLE)
!
 CSOFTWARE = 'SODA'
!
#ifdef SFX_MPI
NCOMM = MPI_COMM_WORLD
 CALL MPI_COMM_SIZE(NCOMM,NPROC,INFOMPI)
 CALL MPI_COMM_RANK(NCOMM,NRANK,INFOMPI)
 LPIO=.TRUE.
 IF ( NRANK /= NPIO ) LPIO=.FALSE.
#endif
!
 CALL PREP_LOG_MPI
!
!--------------------------------------
!
IF (LPIO) THEN
  WRITE(*,*)
  WRITE(*,*) '   ------------------------------------'
  WRITE(*,*) '   |               SODA               |'
  WRITE(*,*) '   | SURFEX OFFLINE DATA ASSIMILATION |'
  WRITE(*,*) '   ------------------------------------'
  WRITE(*,*)
ENDIF
!
WRITE(YRANK,FMT='(I10)') NRANK
YNAME=TRIM(YLUOUT)//ADJUSTL(YRANK)
!
! Open ascii outputfile for writing
#ifdef SFX_LFI
CLUOUT_LFI =  ADJUSTL(ADJUSTR(YLUOUT)//'.txt')
#endif
#ifdef SFX_NC
CLUOUT_NC = ADJUSTL(ADJUSTR(YLUOUT)//'.txt')
#endif
 CALL GET_LUOUT('ASCII ',ILUOUT)
OPEN(UNIT=ILUOUT,FILE=ADJUSTL(ADJUSTR(YNAME)//'.txt'),FORM='FORMATTED',ACTION='WRITE')
!
! Read offline specific things
 CALL OPEN_NAMELIST('ASCII ',ILUNAM,CNAMELIST)
 CALL POSNAM(ILUNAM,'NAM_IO_OFFLINE',GFOUND)
IF (GFOUND) READ (UNIT=ILUNAM,NML=NAM_IO_OFFLINE)
 CALL CLOSE_NAMELIST('ASCII ',ILUNAM)
!
IF (NPROC==1) THEN 
  XIO_FRAC=1.
ELSE
  XIO_FRAC = MAX(MIN(XIO_FRAC,1.),0.)
ENDIF
!
! Check validity of NAM_IO_OFFLINE settings
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CSURF_FILETYPE',CSURF_FILETYPE,'ASCII ','LFI   ','FA    ','NC    ')
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CTIMESERIES_FILETYPE',CTIMESERIES_FILETYPE,'NETCDF','TEXTE ','BINARY',&
                        'ASCII ','LFI   ','FA    ','NONE  ','OFFLIN','NC    ')  
!
IF (CTIMESERIES_FILETYPE=='NETCDF') CTIMESERIES_FILETYPE='OFFLIN'
!
! Setting input files read from namelist
IF ( CSURF_FILETYPE == "LFI   " ) THEN
#ifdef SFX_LFI
  CFILEIN_LFI      = CPREPFILE
  CFILE_LFI        = CPREPFILE
  CFILEIN_LFI_SAVE = CPREPFILE
  CFILEPGD_LFI     = CPGDFILE
#endif
ELSEIF ( CSURF_FILETYPE == "FA    " ) THEN
#ifdef SFX_FA
  CFILEIN_FA      = ADJUSTL(ADJUSTR(CPREPFILE)//'.fa')
  CFILEIN_FA_SAVE = ADJUSTL(ADJUSTR(CPREPFILE)//'.fa')
  CFILEPGD_FA     = ADJUSTL(ADJUSTR(CPGDFILE)//'.fa')
#endif
ELSEIF ( CSURF_FILETYPE == "ASCII " ) THEN
#ifdef SFX_ASC
  CFILEIN      = ADJUSTL(ADJUSTR(CPREPFILE)//'.txt')
  CFILEIN_SAVE = ADJUSTL(ADJUSTR(CPREPFILE)//'.txt')
  CFILEPGD     = ADJUSTL(ADJUSTR(CPGDFILE)//'.txt')
#endif
ELSEIF ( CSURF_FILETYPE == "NC    " ) THEN
#ifdef SFX_NC
  CFILEIN_NC      = ADJUSTL(ADJUSTR(CPREPFILE)//'.nc')
  CFILEIN_NC_SAVE = ADJUSTL(ADJUSTR(CPREPFILE)//'.nc')
  CFILEPGD_NC     = ADJUSTL(ADJUSTR(CPGDFILE)//'.nc')
#endif
ELSE
  CALL ABOR1_SFX(TRIM(CSURF_FILETYPE)//" is not implemented!")
ENDIF
!
! Allocation of Surfex Types
 CALL SURFEX_ALLOC_LIST(1)
 YSC => YSURF_LIST(1)
!
! Reading all namelist (also assimilation)
CALL READ_ALL_NAMELISTS(YSC, CSURF_FILETYPE,'ALL',.FALSE.)
!
!*     0.2.    Goto model of Surfex Types 
!
ICURRENT_MODEL = 1
!
CPROGNAME = CSURF_FILETYPE
LREAD_ALL = .TRUE.
!
! Initialization netcdf file handling
IF (LPIO) THEN
  !
  XSTART            = NUNDEF
  XSTRIDE           = NUNDEF
  XCOUNT            = NUNDEF
  XSTARTW           = 0
  XCOUNTW           = 1
  LPARTW            = .TRUE.
  !
ENDIF
!
CALL INIT_INDEX_MPI(YSC%DTCO, YSC%U, YSC%UG, YSC%GCP, CSURF_FILETYPE, 'OFF', YALG_MPI, XIO_FRAC, .FALSE.)
!
!
! Initialize time information
IYEAR    = NUNDEF
IMONTH   = NUNDEF
IDAY     = NUNDEF
ZTIME    = XUNDEF
CALL IO_BUFF_CLEAN
CALL SET_SURFEX_FILEIN(CSURF_FILETYPE,'PREP')
CALL INIT_IO_SURF_n(YSC%DTCO, YSC%U, CSURF_FILETYPE,'FULL  ','SURF  ','READ ') 
CALL READ_SURF(CSURF_FILETYPE,'DIM_FULL  ',IDIM_FULL,  IRESP)
CALL READ_SURF(CSURF_FILETYPE,'DTCUR     ',TTIME,  IRESP)
CALL END_IO_SURF_n(CSURF_FILETYPE)
!
CALL GET_SIZE_FULL_n(CSURF_FILETYPE,IDIM_FULL,YSC%U%NSIZE_FULL,ISIZE_FULL)
!
IF (ALLOCATED(NMASK_FULL)) DEALLOCATE(NMASK_FULL)
!
ISV = 0
ALLOCATE(CSV(ISV))
!
ALLOCATE(XCO2(ISIZE_FULL))
ALLOCATE(XIMPWET(ISIZE_FULL,NIMPUR))
ALLOCATE(XIMPDRY(ISIZE_FULL,NIMPUR))
ALLOCATE(XRHOA(ISIZE_FULL))
ALLOCATE(XZENITH(ISIZE_FULL))
ALLOCATE(XAZIM(ISIZE_FULL))
ALLOCATE(XEMIS(ISIZE_FULL))
ALLOCATE(XTSRAD(ISIZE_FULL))
ALLOCATE(XTSURF(ISIZE_FULL))
!
ISW = 0
ALLOCATE(XSW_BANDS(           ISW))
ALLOCATE(XDIR_ALB (ISIZE_FULL,ISW))
ALLOCATE(XSCA_ALB (ISIZE_FULL,ISW))
!
! Indicate that zenith and azimuth angles are not initialized
XZENITH = XUNDEF
XAZIM   = XUNDEF
XCO2    = 0.
XRHOA   = 1.
XIMPWET=XUNDEF
XIMPDRY=XUNDEF
!
! Sanity check
IF ( .NOT. LASSIM ) CALL ABOR1_SFX("YOU CAN'T RUN SODA WITHOUT SETTING LASSIM=.TRUE. IN THE ASSIM NAMELIST")
!
! Set the number of initializations to be done
! Default is one
INB = 1
IF ( CASSIM_ISBA == 'EKF  ' ) THEN
  ! Has to do initialization for all the perturbations + 
  ! control + the real run at last
  INB = NVAR + 2
  ISIZE = NVAR
ELSEIF ( CASSIM_ISBA == 'ENKF ' ) THEN
  INB = NENS
  IF (LBIAS_CORRECTION) INB = INB + 1
  ISIZE = NENS
ENDIF
!
IF (LPIO) WRITE(*,*) "INITIALIZING SURFEX..."
!
YINIT = 'ALL'
!
IYEAR  = TTIME%TDATE%YEAR
IMONTH = TTIME%TDATE%MONTH
IDAY   = TTIME%TDATE%DAY
ZTIME  = TTIME%TIME
IHOUR  = NINT(ZTIME/3600.)
TDATE_END = TTIME%TDATE
!
NOBS = 0
!
LREAD_ALL = .FALSE.
!
DO NIFIC = INB,1,-1
  !
  ! If we have more than one initialization to do
  ! For last initialization, we must re-do the first.
  ! Could be avoided by introducing knowlegde of LASSIM on this level
  IF ( CASSIM_ISBA == 'EKF  ' .OR. CASSIM_ISBA == 'ENKF ' ) THEN
    !
    IF (CASSIM_ISBA == 'EKF  ') THEN
      IF ( NIFIC<INB ) THEN
        YMFILE = "PREP_"
        CALL GET_FILE_NAME(IYEAR,IMONTH,IDAY,IHOUR,YMFILE)              
        WRITE(YVAR,'(I1.1)') NIFIC-1
        YFILEIN = TRIM(YMFILE)//"_EKF_PERT"//ADJUSTL(YVAR)
      ELSE
        YFILEIN = "PREP_INIT"
      ENDIF
    ELSEIF (CASSIM_ISBA == 'ENKF ') THEN
      YMFILE = "PREP_"
      CALL GET_FILE_NAME(IYEAR,IMONTH,IDAY,IHOUR,YMFILE)              
      WRITE(YVAR,'(I2.0)') NIFIC
      YFILEIN = TRIM(YMFILE)//"_EKF_ENS"//ADJUSTL(YVAR)
    ENDIF
    !
    CALL SET_FILEIN(YFILEIN)
    !
  ENDIF
  !
  IF (NIFIC<INB) THEN
    CALL DEALLOC_SURF_ATM_n(YSC)
    CALL SURFEX_ALLOC(YSC)
  ENDIF
  !
  IF ( (CASSIM_ISBA=='EKF  '.OR.CASSIM_ISBA=='ENKF ') .AND. NIFIC==1 ) LREAD_ALL = .TRUE.
  !    
  ! Initialize the SURFEX interface
  CALL IO_BUFF_CLEAN
  CALL INIT_SURF_ATM_n(YSC, CSURF_FILETYPE, YINIT, LLAND_USE, ISIZE_FULL, ISV, ISW,    &
                       CSV, XCO2, XIMPWET, XIMPDRY, XRHOA, XZENITH, XAZIM, XSW_BANDS,  &
                       XDIR_ALB, XSCA_ALB,XEMIS, XTSRAD, XTSURF, IYEAR, IMONTH, IDAY,  &
                       ZTIME, TDATE_END, AT, YATMFILE, YATMFILETYPE, YTEST   )
                       
  !
  IF ( CASSIM_ISBA=='EKF  ' .OR. CASSIM_ISBA=='ENKF ' ) THEN
    !
    ISIZE_NATURE = YSC%U%NSIZE_NATURE
    INPATCH = YSC%IM%O%NPATCH
    !
    IF ( NIFIC==INB ) THEN
      ALLOCATE(XLAI_PASS(ISIZE_NATURE,INPATCH))
      ALLOCATE(XBIO_PASS(ISIZE_NATURE,INPATCH))     
      IF (CASSIM_ISBA=='EKF  ') ALLOCATE(XI(ISIZE_NATURE,INPATCH,ISIZE))
      ALLOCATE(XF       (ISIZE_NATURE,INPATCH,ISIZE+1,NVAR))
      ALLOCATE(XF_PATCH (ISIZE_NATURE,INPATCH,ISIZE+1,NOBSTYPE))
    ENDIF
    !
    IF ( CASSIM_ISBA=='EKF  ' .AND. NIFIC<INB .OR. CASSIM_ISBA=='ENKF ') THEN
      !
      ! Set the global state values for this control value
      XF_PATCH(:,:,NIFIC,:) = XUNDEF
      DO JP=1,INPATCH
        PK => YSC%IM%NP%AL(JP)
        PEK => YSC%IM%NPE%AL(JP)      

        DO IOBS = 1,NOBSTYPE
          DO JI = 1,PK%NSIZE_P
            IMASK =PK%NR_P(JI)
            SELECT CASE (TRIM(COBS(IOBS)))
              CASE("T2M")
                XF_PATCH(JI,JP,NIFIC,IOBS) = XAT2M_ISBA(JI,1)
              CASE("HU2M")
                XF_PATCH(JI,JP,NIFIC,IOBS) = XAHU2M_ISBA(JI,1)
              CASE("WG1")
                XF_PATCH(IMASK,JP,NIFIC,IOBS) = PEK%XWG(JI,1)
              CASE("WG2")
                XF_PATCH(IMASK,JP,NIFIC,IOBS) = PEK%XWG(JI,2)
              CASE("LAI")
                XF_PATCH(IMASK,JP,NIFIC,IOBS) = PEK%XLAI(JI)
              CASE("SWE")
                XF_PATCH(IMASK,JP,NIFIC,IOBS) = PEK%TSNOW%WSNOW(JI,1)
              CASE DEFAULT
                CALL ABOR1_SFX("Mapping of "//COBS(IOBS)//" is not defined in SODA!")
            END SELECT
          ENDDO
        ENDDO
      ENDDO
      !
      ! Prognostic fields for assimilation (Control vector)
      XF(:,:,NIFIC,:) = XUNDEF
      DO JP = 1,INPATCH
        PK => YSC%IM%NP%AL(JP)
        PEK => YSC%IM%NPE%AL(JP) 

        DO JL = 1,NVAR
          DO JI = 1,PK%NSIZE_P
            IMASK = PK%NR_P(JI)      
            SELECT CASE (TRIM(CVAR(JL)))
              CASE("TG1")
                XF(IMASK,JP,NIFIC,JL) = PEK%XTG(JI,1)
              CASE("TG2")
                XF(IMASK,JP,NIFIC,JL) = PEK%XTG(JI,2)
              CASE("WG1")
                XF(IMASK,JP,NIFIC,JL) = PEK%XWG(JI,1)
              CASE("WG2")
                XF(IMASK,JP,NIFIC,JL) = PEK%XWG(JI,2)
              CASE("WG3")
                XF(IMASK,JP,NIFIC,JL) = PEK%XWG(JI,3) 
              CASE("WG4")
                XF(IMASK,JP,NIFIC,JL) = PEK%XWG(JI,4)
              CASE("WG5")
                XF(IMASK,JP,NIFIC,JL) = PEK%XWG(JI,5)
              CASE("WG6")
                XF(IMASK,JP,NIFIC,JL) = PEK%XWG(JI,6)
              CASE("WG7")
                XF(IMASK,JP,NIFIC,JL) = PEK%XWG(JI,7)
              CASE("WG8")
                XF(IMASK,JP,NIFIC,JL) = PEK%XWG(JI,8)
              CASE("WGI1")
                XF(IMASK,JP,NIFIC,JL) = PEK%XWGI(JI,1)
              CASE("WGI2")
                XF(IMASK,JP,NIFIC,JL) = PEK%XWGI(JI,2)
              CASE("WGI3")
                XF(IMASK,JP,NIFIC,JL) = PEK%XWGI(JI,3) 
              CASE("WGI4")
                XF(IMASK,JP,NIFIC,JL) = PEK%XWGI(JI,4)
              CASE("WGI5")
                XF(IMASK,JP,NIFIC,JL) = PEK%XWGI(JI,5)
              CASE("WGI6")
                XF(IMASK,JP,NIFIC,JL) = PEK%XWGI(JI,6)
              CASE("WGI7")
                XF(IMASK,JP,NIFIC,JL) = PEK%XWGI(JI,7)
              CASE("WGI8")
                XF(IMASK,JP,NIFIC,JL) = PEK%XWGI(JI,8)
              CASE("LAI")
                XF(IMASK,JP,NIFIC,JL) = PEK%XLAI(JI)
              CASE("SWE")
                XF(IMASK,JP,NIFIC,JL) = PEK%TSNOW%WSNOW(JI,1)
              CASE DEFAULT
                CALL ABOR1_SFX("Mapping of "//TRIM(CVAR(JL))//" is not defined in SODA!")
            END SELECT
          ENDDO
        ENDDO
      ENDDO
      !
      IF ( NIFIC==1 ) THEN
        !
        XLAI_PASS(:,:) = XUNDEF
        XBIO_PASS(:,:) = XUNDEF
        DO JP = 1,INPATCH
          PK => YSC%IM%NP%AL(JP)
          PEK => YSC%IM%NPE%AL(JP)   

          DO JL = 1,NVAR
            DO JI = 1,PK%NSIZE_P
              IMASK = PK%NR_P(JI)          
              IF (TRIM(CVAR(JL))=="LAI") THEN
                IF ( INPATCH==1 .AND. TRIM(CBIO)/="LAI" ) THEN
                  CALL ABOR1_SFX("Mapping of "//CBIO//" is not defined in EKF with NPATCH=1!")
                ENDIF
                SELECT CASE (TRIM(CBIO))
                  CASE("BIOMA1","BIOMASS1")
                    XBIO_PASS(IMASK,JP) = PEK%XBIOMASS(JI,1)
                  CASE("BIOMA2","BIOMASS2")
                    XBIO_PASS(IMASK,JP) = PEK%XBIOMASS(JI,2)
                  CASE("RESPI1","RESP_BIOM1")
                    XBIO_PASS(IMASK,JP) = PEK%XRESP_BIOMASS(JI,1)
                  CASE("RESPI2","RESP_BIOM2")
                    XBIO_PASS(IMASK,JP) = PEK%XRESP_BIOMASS(JI,2)
                  CASE("LAI")
                    XBIO_PASS(IMASK,JP) = PEK%XLAI(JI)
                  CASE DEFAULT
                    CALL ABOR1_SFX("Mapping of "//CBIO//" is not defined in EKF!")
                END SELECT
                !
                XLAI_PASS(IMASK,JP) = PEK%XLAI(JI)          
                !
              ENDIF
              !
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      !
    ELSE
      !
      XI(:,:,:) = XUNDEF
      DO JP = 1,INPATCH 
        PK => YSC%IM%NP%AL(JP)
        PEK => YSC%IM%NPE%AL(JP) 

        DO JL = 1,NVAR
          DO JI = 1,PK%NSIZE_P
            IMASK = PK%NR_P(JI)             
            SELECT CASE (TRIM(CVAR(JL)))
              CASE("TG1")
                XI(IMASK,JP,JL) = PEK%XTG(JI,1)
              CASE("TG2")
                XI(IMASK,JP,JL) = PEK%XTG(JI,2)
              CASE("WG1")
                XI(IMASK,JP,JL) = PEK%XWG(JI,1)
              CASE("WG2")
                XI(IMASK,JP,JL) = PEK%XWG(JI,2)
              CASE("WG3")
                XI(IMASK,JP,JL) = PEK%XWG(JI,3)  
              CASE("WG4")
                XI(IMASK,JP,JL) = PEK%XWG(JI,4)
              CASE("WG5")
                XI(IMASK,JP,JL) = PEK%XWG(JI,5)
              CASE("WG6")
                XI(IMASK,JP,JL) = PEK%XWG(JI,6)
              CASE("WG7")
                XI(IMASK,JP,JL) = PEK%XWG(JI,7)
              CASE("WG8")
                XI(IMASK,JP,JL) = PEK%XWG(JI,8)  
              CASE("WGI1")
                XI(IMASK,JP,JL) = PEK%XWGI(JI,1)
              CASE("WGI2")
                XI(IMASK,JP,JL) = PEK%XWGI(JI,2)
              CASE("WGI3")
                XI(IMASK,JP,JL) = PEK%XWGI(JI,3)  
              CASE("WGI4")
                XI(IMASK,JP,JL) = PEK%XWGI(JI,4)
              CASE("WGI5")
                XI(IMASK,JP,JL) = PEK%XWGI(JI,5)
              CASE("WGI6")
                XI(IMASK,JP,JL) = PEK%XWGI(JI,6)
              CASE("WGI7")
                XI(IMASK,JP,JL) = PEK%XWGI(JI,7)
              CASE("WGI8")
                XI(IMASK,JP,JL) = PEK%XWGI(JI,8)
              CASE("LAI")
                XI(IMASK,JP,JL) = PEK%XLAI(JI)
              CASE DEFAULT
                CALL ABOR1_SFX("Mapping of "//TRIM(CVAR(JL))//" is not defined in SODA!")
            END SELECT
          ENDDO
        ENDDO
      ENDDO        
      !
    ENDIF
    !
  ENDIF
  !
ENDDO
!
! Allocate input fields to the assimilation interface
ALLOCATE(ZLSM        (ISIZE_FULL))
ALLOCATE(ZCON_RAIN   (ISIZE_FULL))
ALLOCATE(ZSTRAT_RAIN (ISIZE_FULL))
ALLOCATE(ZCON_SNOW   (ISIZE_FULL))
ALLOCATE(ZSTRAT_SNOW (ISIZE_FULL))
ALLOCATE(ZCLOUDS     (ISIZE_FULL))
ALLOCATE(ZEVAPTR     (ISIZE_FULL))
ALLOCATE(ZEVAP       (ISIZE_FULL))
ALLOCATE(ZTSC        (ISIZE_FULL))
ALLOCATE(ZSWEC       (ISIZE_FULL))
ALLOCATE(ZTS         (ISIZE_FULL))
ALLOCATE(ZUCLS       (ISIZE_FULL))
ALLOCATE(ZVCLS       (ISIZE_FULL))
ALLOCATE(ZSST        (ISIZE_FULL))
ALLOCATE(ZSIC        (ISIZE_FULL))
ZTS(:) = XUNDEF

! OI needs first guess values used in oi_cacsts
IF (CASSIM_ISBA=="OI   ") THEN
  YMASK='FULL'
  CALL ASSIM_READ_FIELD(YSC%DTCO, YSC%U, YSC%UG, YSC%USS,TTIME,CFILE_FORMAT_FG,YMASK,"CON_RAIN",ZCON_RAIN)
  CALL ASSIM_READ_FIELD(YSC%DTCO, YSC%U, YSC%UG, YSC%USS,TTIME,CFILE_FORMAT_FG,YMASK,"STRAT_RAIN",ZSTRAT_RAIN)
  CALL ASSIM_READ_FIELD(YSC%DTCO, YSC%U, YSC%UG, YSC%USS,TTIME,CFILE_FORMAT_FG,YMASK,"CON_SNOW",ZCON_SNOW)
  CALL ASSIM_READ_FIELD(YSC%DTCO, YSC%U, YSC%UG, YSC%USS,TTIME,CFILE_FORMAT_FG,YMASK,"STRAT_SNOW",ZSTRAT_SNOW)
  CALL ASSIM_READ_FIELD(YSC%DTCO, YSC%U, YSC%UG, YSC%USS,TTIME,CFILE_FORMAT_FG,YMASK,"CLOUDS",ZCLOUDS)
  CALL ASSIM_READ_FIELD(YSC%DTCO, YSC%U, YSC%UG, YSC%USS,TTIME,CFILE_FORMAT_FG,YMASK,"EVAP",ZEVAP)
  CALL ASSIM_READ_FIELD(YSC%DTCO, YSC%U, YSC%UG, YSC%USS,TTIME,CFILE_FORMAT_FG,YMASK,"EVAPTR",ZEVAPTR)
  CALL ASSIM_READ_FIELD(YSC%DTCO, YSC%U, YSC%UG, YSC%USS,TTIME,CFILE_FORMAT_FG,YMASK,"U10M",ZUCLS)
  CALL ASSIM_READ_FIELD(YSC%DTCO, YSC%U, YSC%UG, YSC%USS,TTIME,CFILE_FORMAT_FG,YMASK,"V10M",ZVCLS)
ENDIF

! If we want to extrapolate values, we need to have a land-sea-mask available
IF (LEXTRAP_NATURE .OR. LWATERTG2 .OR. (TRIM(CASSIM_SEA) == "INPUT" .AND. LAESST)) THEN
  YMASK='FULL'
  CALL ASSIM_READ_FIELD(YSC%DTCO, YSC%U, YSC%UG, YSC%USS,TTIME,CFILE_FORMAT_LSM,YMASK,'LSM',ZLSM)
ENDIF

! The observations used in the analysis is read.
IF ( CASSIM_ISBA=="EKF  " .OR. CASSIM_ISBA=="ENKF " ) THEN
  ALLOCATE(XYO(ISIZE_NATURE,NOBSTYPE))
ENDIF

! Allocate observations
ALLOCATE(ZT2M        (ISIZE_FULL))
ALLOCATE(ZHU2M       (ISIZE_FULL))
ALLOCATE(ZSWE        (ISIZE_FULL))

ZT2M  = 999.
ZHU2M = 999.
ZSWE  = 999.

DO IOBS = 1,NOBSTYPE
  YOBS=COBS(IOBS)
  ! The following observations is assumed to be available for the all points/whole field as they might be used for several tiles
  IF ( TRIM(YOBS) == "T2M" .OR. TRIM(YOBS) == "HU2M" .OR. TRIM(YOBS) == "SWE" ) THEN
    YMASK="FULL"
    ALLOCATE(ZWORK(YSC%U%NSIZE_FULL))
    IF (LOBSNAT .AND. LPIO ) WRITE(*,*) 'WARNING: Observation '//YOBS//&
                                    & ' is assumed to be for the full dimension and LOBSNAT=.TRUE.'
  ELSE
    IF (LOBSNAT) THEN
      YMASK="NATURE"
      ALLOCATE(ZWORK(YSC%U%NSIZE_NATURE))
    ELSE
      YMASK="FULL"
      ALLOCATE(ZWORK(YSC%U%NSIZE_FULL))
    ENDIF
  ENDIF
     
  ! Read observation
  CALL ASSIM_READ_FIELD(YSC%DTCO, YSC%U, YSC%UG, YSC%USS,TTIME,CFILE_FORMAT_OBS,YMASK,YOBS,ZWORK)

  SELECT CASE (TRIM(YOBS))
    CASE ("T2M")
      ZT2M(:)=ZWORK(:)
    CASE ("HU2M")
      ZHU2M(:)=ZWORK(:)
    CASE ("SWE")
      ZSWE(:)=ZWORK(:)
    CASE DEFAULT
      ! Default is assumed to be control variables for EKF
      IF ( TRIM(CASSIM_ISBA) == "OI" ) CALL ABOR1_SFX("You are not supposed to read this observation for OI: "//TRIM(YOBS))
      XYO(:,IOBS)=ZWORK(:)
      IF (( TRIM(CASSIM_ISBA) == "EKF" .OR. TRIM(CASSIM_ISBA) == "ENKF" ) .AND. ( NPRINTLEV > 2 )) &
          WRITE(ILUOUT,*) 'read in obs: ', XYO(1,IOBS), YOBS
  END SELECT
  DEALLOCATE(ZWORK)
ENDDO

! Climatological fields are only used in OI
IF (TRIM(CASSIM_ISBA) == "OI") THEN
  YMASK='FULL'
  CALL ASSIM_READ_FIELD(YSC%DTCO, YSC%U, YSC%UG, YSC%USS,TTIME,CFILE_FORMAT_CLIM,YMASK,"SWEC",ZSWEC)
  CALL ASSIM_READ_FIELD(YSC%DTCO, YSC%U, YSC%UG, YSC%USS,TTIME,CFILE_FORMAT_CLIM,YMASK,"TSC",ZTSC)
ENDIF

! Set SST and SIC
IF ( TRIM(CASSIM_SEA) == "INPUT" ) THEN
  IF ( LREAD_SST_FROM_FILE ) THEN
    YMASK='FULL'
    CALL ASSIM_READ_FIELD(YSC%DTCO, YSC%U, YSC%UG, YSC%USS,TTIME,CFILE_FORMAT_SST,YMASK,"SST",ZSST)
    CALL ASSIM_READ_FIELD(YSC%DTCO, YSC%U, YSC%UG, YSC%USS,TTIME,CFILE_FORMAT_SST,YMASK,"SIC",ZSIC)
    ! Fill missing values with original values
    IF ( YSC%U%NSIZE_SEA>0 .AND. YSC%U%CSEA/="NONE") THEN
      ALLOCATE(ZWORK(SIZE(ZSST)))
      CALL UNPACK_SAME_RANK(YSC%U%NR_SEA,YSC%SM%S%XSST,ZWORK)
      WHERE ( ZSST(:) == -9999 .AND. ZWORK /= XUNDEF )
         ZSST(:)=ZWORK(:)
      ENDWHERE
      DEALLOCATE(ZWORK)
    ENDIF
  ELSE
    IF ( YSC%U%NSIZE_SEA>0 .AND. YSC%U%CSEA/="NONE") THEN
      CALL UNPACK_SAME_RANK(YSC%U%NR_SEA,YSC%SM%S%XSST,ZSST)
      CALL UNPACK_SAME_RANK(YSC%U%NR_SEA,YSC%SM%S%XSIC,ZSIC)
    ELSE
      ZSST(:) = XUNDEF
      ZSIC(:) = XUNDEF
    ENDIF
  ENDIF
ENDIF

IF ( .NOT. LASSIM ) CALL ABOR1_SFX("YOU CAN'T RUN SODA WITHOUT SETTING LASSIM=.TRUE. IN THE ASSIM NAMELIST")
!
ALLOCATE(GD_MASKEXT(ISIZE_FULL))
GD_MASKEXT(:) = .FALSE.
!
ALLOCATE(ZLON(ISIZE_FULL))
ALLOCATE(ZLAT(ISIZE_FULL))
ZLON(:) = YSC%UG%G%XLON(:)
ZLAT(:) = YSC%UG%G%XLAT(:)        
!
GLKEEPEXTZONE = .TRUE.
!
IF (LPIO) WRITE(*,*) 'PERFORMIMG OFFLINE SURFEX DATA ASSIMILATION...'
CALL ASSIM_SURF_ATM_n(YSC%U, YSC%IM, YSC%SM, YSC%TM, YSC%WM,              &
                      CSURF_FILETYPE, ISIZE_FULL, ZCON_RAIN, ZSTRAT_RAIN, &
                      ZCON_SNOW, ZSTRAT_SNOW, ZCLOUDS, ZLSM, ZEVAPTR,     &
                      ZEVAP, ZSWEC, ZTSC, ZTS, ZT2M, ZHU2M, ZSWE, ZSST,   &
                      ZSIC, ZUCLS, ZVCLS, YTEST, GD_MASKEXT, ZLON, ZLAT,  &
                      GLKEEPEXTZONE )
!
DEALLOCATE(ZCON_RAIN)
DEALLOCATE(ZSTRAT_RAIN)
DEALLOCATE(ZCON_SNOW)
DEALLOCATE(ZSTRAT_SNOW)
DEALLOCATE(ZCLOUDS)
DEALLOCATE(ZLSM)
DEALLOCATE(ZEVAPTR)
DEALLOCATE(ZEVAP)
DEALLOCATE(ZTSC)
DEALLOCATE(ZSWEC)
DEALLOCATE(ZTS)
DEALLOCATE(ZT2M)
DEALLOCATE(ZHU2M)
DEALLOCATE(ZSWE)
DEALLOCATE(ZUCLS)
DEALLOCATE(ZVCLS)
DEALLOCATE(ZSST)
DEALLOCATE(ZSIC)
!
ZTIME_OUT  = ZTIME
IDAY_OUT   = IDAY
IMONTH_OUT = IMONTH
IYEAR_OUT  = IYEAR
!
IF (LPIO) THEN
  !
  IF(LOUT_TIMENAME)THEN
    ! if true, change the name of output file at the end of a day
    ! (ex: 19860502_00h00 -> 19860501_24h00)                     
    IF(ZTIME==0.0)THEN
      ZTIME_OUT = 86400.
      IDAY_OUT   = IDAY-1
      IF(IDAY_OUT==0)THEN
        IMONTH_OUT = IMONTH - 1
        IF(IMONTH_OUT==0)THEN
          IMONTH_OUT=12
          IYEAR_OUT = IYEAR - 1
        ENDIF
        SELECT CASE (IMONTH_OUT)
          CASE(4,6,9,11)
            IDAY_OUT=30
          CASE(1,3,5,7:8,10,12)
            IDAY_OUT=31
          CASE(2)
            IF( ((MOD(IYEAR_OUT,4)==0).AND.(MOD(IYEAR_OUT,100)/=0)) .OR. (MOD(IYEAR_OUT,400)==0))THEN 
              IDAY_OUT=29
            ELSE
             IDAY_OUT=28
           ENDIF
        END SELECT
      ENDIF
    ENDIF
    !
  ENDIF
  !
  WRITE(YTAG,FMT='(I4.4,I2.2,I2.2,A1,I2.2,A1,I2.2)') IYEAR_OUT,IMONTH_OUT,IDAY_OUT,&
    '_',INT(ZTIME_OUT/3600.),'h',NINT(ZTIME_OUT)/60-60*INT(ZTIME_OUT/3600.)  
  CFILEOUT    = ADJUSTL(ADJUSTR(CSURFFILE)//'.'//YTAG//'.txt')
#ifdef SFX_LFI  
  CFILEOUT_LFI= ADJUSTL(ADJUSTR(CSURFFILE)//'.'//YTAG)
#endif
#ifdef SFX_FA    
  CFILEOUT_FA = ADJUSTL(ADJUSTR(CSURFFILE)//'.'//YTAG//'.fa')
#endif
#ifdef SFX_NC  
  CFILEOUT_NC = ADJUSTL(ADJUSTR(CSURFFILE)//'.'//YTAG//'.nc')
#endif
  !
  !
ENDIF
!
ISIZE = 1
IF (CASSIM_ISBA=="ENKF ") THEN
  ISIZE = NENS
  IF (LBIAS_CORRECTION) ISIZE = ISIZE + 1
ENDIF
!
NWRITE = 1
XSTARTW = 1
XTSTEP_OUTPUT = 0.
LTIME_WRITTEN = .FALSE.
!
DO IENS = 1,ISIZE
  !
  IF (CASSIM_ISBA=="ENKF ") THEN
    !
    YMFILE = "PREP_"
    CALL GET_FILE_NAME(IYEAR,IMONTH,IDAY,IHOUR,YMFILE)              
    WRITE(YVAR,'(I3)') IENS
    YFILEIN = TRIM(YMFILE)//"_EKF_ENS"//ADJUSTL(YVAR)
    CALL SET_FILEIN(YFILEIN)
    !
    LREAD_ALL = .TRUE.
    !
    CALL DEALLOC_SURF_ATM_n(YSC)
    CALL SURFEX_ALLOC(YSC)
    !
    ! Initialize the SURFEX interface
    CALL IO_BUFF_CLEAN
    CALL INIT_SURF_ATM_n(YSC, CSURF_FILETYPE,YINIT, LLAND_USE, ISIZE_FULL, ISV, ISW,      &
                         CSV, XCO2, XIMPWET,XIMPDRY, XRHOA, XZENITH, XAZIM, XSW_BANDS,    &
                         XDIR_ALB, XSCA_ALB, XEMIS, XTSRAD, XTSURF, IYEAR, IMONTH, IDAY,  &
                         ZTIME, TDATE_END, AT, YATMFILE, YATMFILETYPE, YTEST  )
                          
    !
    DO JP = 1,INPATCH
      PK => YSC%IM%NP%AL(JP)
      PEK => YSC%IM%NPE%AL(JP)  
      !
      DO JL=1,NVAR
        DO JI = 1,PK%NSIZE_P
          IMASK = PK%NR_P(JI) 
          !
          ! Update the modified values
          SELECT CASE (TRIM(CVAR(JL)))
            CASE("TG1")
              PEK%XTG(JI,1) = XF(IMASK,JP,IENS,JL)
            CASE("TG2")
              PEK%XTG(JI,2) = XF(IMASK,JP,IENS,JL)
            CASE("WG1")
              PEK%XWG(JI,1) = XF(IMASK,JP,IENS,JL)
            CASE("WG2")
              PEK%XWG(JI,2) = XF(IMASK,JP,IENS,JL)
            CASE("WG3")
              PEK%XWG(JI,3) = XF(IMASK,JP,IENS,JL)  
            CASE("WG4")
              PEK%XWG(JI,4) = XF(IMASK,JP,IENS,JL)  
            CASE("WG5")
              PEK%XWG(JI,5) = XF(IMASK,JP,IENS,JL)  
            CASE("WG6")
              PEK%XWG(JI,6) = XF(IMASK,JP,IENS,JL)  
            CASE("WG7")
              PEK%XWG(JI,7) = XF(IMASK,JP,IENS,JL)  
            CASE("WG8")
              PEK%XWG(JI,8) = XF(IMASK,JP,IENS,JL)        
            CASE("LAI") 
              PEK%XLAI(JI) = XF(IMASK,JP,IENS,JL)
            CASE DEFAULT
              CALL ABOR1_SFX("Mapping of "//TRIM(CVAR(JL))//" is not defined in EKF!")
          END SELECT
        ENDDO
      ENDDO
    ENDDO
    !
  ENDIF
  !
  LFIRST_WRITE = .TRUE.

  IF (LPIO) THEN
    !* name of the file
    CFILEOUT    = ADJUSTL(ADJUSTR(CSURFFILE)//'.txt')
    CFILEOUT_LFI= CSURFFILE
    CFILEOUT_FA = ADJUSTL(ADJUSTR(CSURFFILE)//'.fa')
    !CFILEOUT_NC = ADJUSTL(ADJUSTR(CSURFFILE)//'.nc')

    !* opens the file
    IF (CSURF_FILETYPE=='FA    ') THEN
#ifdef SFX_FA    
        
      LFANOCOMPACT = .TRUE.
      IDATEF(1)= IYEAR
      IDATEF(2)= IMONTH
      IDATEF(3)= IDAY
      IDATEF(4)= FLOOR(ZTIME/3600.)
      IDATEF(5)= FLOOR(ZTIME/60.) - IDATEF(4) * 60
      IDATEF(6)= NINT(ZTIME) - IDATEF(4) * 3600 - IDATEF(5) * 60
      IDATEF(7:11) = 0
      NUNIT_FA = 19
      IF (LFAGMAP) THEN
#ifdef SFX_ARO
        CALL FAITOU_NO_CACDRE_CHECK(IRET,NUNIT_FA,.TRUE.,CFILEOUT_FA,'UNKNOWN',.TRUE.,.FALSE.,IVERBFA,0,INB,CDNOMC)
#endif
      ELSE
        CALL FAITOU(IRET,NUNIT_FA,.TRUE.,CFILEOUT_FA,'UNKNOWN',.TRUE.,.FALSE.,IVERBFA,0,INB,CDNOMC)
      ENDIF
      CALL FANDAR(IRET,NUNIT_FA,IDATEF)
#endif      
    END IF
    !
  ENDIF

  ! 
#ifdef SFX_NC  
  LDEF_nc = .TRUE.
#endif
  IF (CTIMESERIES_FILETYPE=="NC    ") THEN
    CALL INIT_OUTPUT_NC_n (YSC%TM%BDD, YSC%CHE, YSC%CHN, YSC%CHU, YSC%SM%DTS, YSC%TM%DTT, &
                           YSC%DTZ, YSC%IM, YSC%UG, YSC%U, YSC%DUO%CSELECT)                   
  ENDIF
  !
#ifdef SFX_OL 
  LDEF_ol = .TRUE.
#endif  
  IF (CTIMESERIES_FILETYPE=="OFFLIN") THEN
    CALL INIT_OUTPUT_OL_n (YSC)
  ENDIF
  !
  INW = 1
  IF (CTIMESERIES_FILETYPE=="NC    ".OR.CTIMESERIES_FILETYPE=="OFFLIN") INW = 2
  !
  DO JNW = 1,INW
    CALL IO_BUFF_CLEAN
    CALL WRITE_SURF_ATM_n(YSC, CTIMESERIES_FILETYPE,'ALL',LLAND_USE)
    CALL WRITE_DIAG_SURF_ATM_n(YSC, CTIMESERIES_FILETYPE,'ALL')
#ifdef SFX_NC
    LDEF_nc = .FALSE.
#endif
#ifdef SFX_OL 
    LDEF_ol = .FALSE.
#endif
    NCPT_WRITE = 0
    LFIRST_WRITE = .FALSE.
  ENDDO
  !
  CALL FLAG_UPDATE(YSC%IM%ID%O, YSC%DUO,.FALSE.,.TRUE.,.FALSE.,.FALSE.)
  !
  IF (LRESTART_2M) THEN
    I2M       = 1
    GPGD_ISBA = .TRUE.
  ELSE
    I2M       = 0
    GPGD_ISBA = .FALSE.
  ENDIF  
  GFRAC                  = .TRUE.  
  GDIAG_GRID             = .TRUE.
  GSURF_BUDGET           = .FALSE.
  GRAD_BUDGET            = .FALSE.
  GCOEF                  = .FALSE.
  GSURF_VARS             = .FALSE.
  IBEQ                   = 0
  IDSTEQ                 = 0
  GDIAG_OCEAN            = .FALSE.
  GDIAG_SEAICE           = .FALSE.
  GWATER_PROFILE         = .FALSE.
  GSURF_EVAP_BUDGET      = .FALSE.
  GFLOOD                 = .FALSE.
  GPGD_ISBA              = .FALSE.  
  GCH_NO_FLUX_ISBA       = .FALSE.
  GSURF_MISC_BUDGET_ISBA = .FALSE.
  GPGD_TEB               = .FALSE.
  GSURF_MISC_BUDGET_TEB  = .FALSE.  
  !
  CALL FLAG_DIAG_UPDATE(YSC%FM, YSC%IM, YSC%SM, YSC%TM, YSC%WM, YSC%DUO, YSC%U, YSC%SV,  &          
                        GFRAC, GDIAG_GRID, I2M, GSURF_BUDGET, GRAD_BUDGET, GCOEF, &
                        GSURF_VARS, IBEQ, IDSTEQ, GDIAG_OCEAN, GDIAG_SEAICE,      &
                        GWATER_PROFILE, GSURF_EVAP_BUDGET, GFLOOD,  GPGD_ISBA,    &
                        GCH_NO_FLUX_ISBA, GSURF_MISC_BUDGET_ISBA, GPGD_TEB,       &
                        GSURF_MISC_BUDGET_TEB    )
  ! 
  YSC%DUO%LSNOWDIMNC = .FALSE.
  !
  YENS = '   '
  IF (ISIZE>1) WRITE(YENS,'(I3)') IENS
  !
  IF ( CSURF_FILETYPE == "LFI   " ) THEN
#ifdef SFX_LFI
    CFILEOUT_LFI     = TRIM(TRIM(CSURFFILE)//ADJUSTL(YENS))
#endif
  ELSEIF ( CSURF_FILETYPE == "FA    " ) THEN
#ifdef SFX_FA
    CFILEOUT_FA  = ADJUSTL(TRIM(ADJUSTR(CSURFFILE)//ADJUSTL(YENS))//'.fa')
#endif
  ELSEIF ( CSURF_FILETYPE == "ASCII " ) THEN
#ifdef SFX_ASC
    CFILEOUT = ADJUSTL(TRIM(ADJUSTR(CSURFFILE)//ADJUSTL(YENS))//'.txt')
    LCREATED = .FALSE.
#endif
  ELSEIF ( CSURF_FILETYPE == "NC    " ) THEN
#ifdef SFX_NC
    CFILEOUT_NC = ADJUSTL(TRIM(ADJUSTR(CSURFFILE)//ADJUSTL(YENS))//'.nc')
#endif
  ELSE
    CALL ABOR1_SFX(TRIM(CSURF_FILETYPE)//" is not implemented!")
  ENDIF
  !
#ifdef SFX_NC
  LDEF_nc = .TRUE.
#endif
  !  
  IF (CSURF_FILETYPE=="NC    ") THEN
    CALL INIT_OUTPUT_NC_n (YSC%TM%BDD, YSC%CHE, YSC%CHN, YSC%CHU, YSC%SM%DTS, YSC%TM%DTT, &
                           YSC%DTZ, YSC%IM, YSC%UG, YSC%U, YSC%DUO%CSELECT)
  ENDIF
  !  
  INW = 1
  IF (CSURF_FILETYPE=="NC    ") INW = 2
  !
  LFIRST_WRITE = .TRUE.
  ! 
  IF (ASSOCIATED(YSC%DUO%CSELECT)) DEALLOCATE(YSC%DUO%CSELECT)
  ALLOCATE(YSC%DUO%CSELECT(0))
  !
  DO JNW = 1,INW
    !
    CALL IO_BUFF_CLEAN
    !  
    ! Store results from assimilation
    CALL WRITE_SURF_ATM_n(YSC, CSURF_FILETYPE,'ALL',LLAND_USE)
    !IF (YSC%DUO%LREAD_BUDGETC.AND..NOT.YSC%IM%ID%O%LRESET_BUDGETC) THEN
      CALL WRITE_DIAG_SURF_ATM_n(YSC, CSURF_FILETYPE,'ALL')
    !ENDIF
    !
#ifdef SFX_NC
    LDEF_nc = .FALSE.
#endif
    NCPT_WRITE = 0
    LFIRST_WRITE = .FALSE.
    !
  ENDDO  
  !
ENDDO
!
IF (LPIO .AND. CSURF_FILETYPE=='FA    ') THEN
#ifdef SFX_FA
  IF (LFAGMAP) THEN
    IF (LMNH_COMPATIBLE) CALL WRITE_HEADER_MNH
    CALL FAIDX_WRT
  ENDIF
  CALL FAIRME(IRET,NUNIT_FA,'UNKNOWN')
#endif
END IF
!
!*    3.     Close parallelized I/O
!            ----------------------
!
IF (CTIMESERIES_FILETYPE=='OFFLIN') CALL CLOSE_FILEOUT_OL
!
IF (LPIO) THEN
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) '    -----------------------'
  WRITE(ILUOUT,*) '    | SODA ENDS CORRECTLY |'
  WRITE(ILUOUT,*) '    -----------------------'
  !
  WRITE(*,*) ' '
  WRITE(*,*) '    -----------------------'
  WRITE(*,*) '    | SODA ENDS CORRECTLY |'
  WRITE(*,*) '    -----------------------'
  !
ENDIF
!
CLOSE(ILUOUT)
!
CALL SURFEX_DEALLO_LIST
!
IF (ALLOCATED(NINDEX)) DEALLOCATE(NINDEX)
IF (ALLOCATED(NSIZE_TASK)) DEALLOCATE(NSIZE_TASK)
!
CALL END_LOG_MPI
!
IF (LHOOK) CALL DR_HOOK('SODA',1,ZHOOK_HANDLE)
!
#ifdef SFX_MPI
 CALL MPI_FINALIZE(INFOMPI)
#endif
!
!-------------------------------------------------------------------------------
!
END PROGRAM SODA
