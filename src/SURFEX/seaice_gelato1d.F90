MODULE ICE_GELATO
USE ABSTRACT_ICE
USE MODD_TYPES_GLT, ONLY: T_GLT
USE MODD_GLT_PARAM, ONLY : t_glt_param
USE MODD_GLT_VHD, ONLY : t_glt_vhd
USE YOMHOOK,   ONLY : LHOOK,   DR_HOOK
USE PARKIND1,  ONLY : JPRB
IMPLICIT NONE
PRIVATE

TYPE, PUBLIC, EXTENDS(SEA_ICE_t) :: GELATO_t
  PRIVATE
    TYPE(T_GLT) :: TGLT !< GELATO sea ice state
    TYPE(t_glt_param) :: GLTPARAM !< GELATO parameters
    TYPE(t_glt_vhd)   :: GLTVHD! new structure

    REAL, POINTER :: XTICE(:)
    REAL, POINTER :: XSIC(:)
    REAL, POINTER :: XICE_ALB(:)
  CONTAINS
    PROCEDURE :: INIT
    PROCEDURE :: PREP
    PROCEDURE :: ASSIM
    PROCEDURE :: RUN
    PROCEDURE :: DEALLOC

    PROCEDURE :: READSURF
    PROCEDURE :: WRITESURF
    PROCEDURE :: WRITE_DIAG

    PROCEDURE :: GET_RESPONSE
    PROCEDURE :: DIAG_MISC

    PROCEDURE :: SET_DAMPING
END TYPE GELATO_t

CONTAINS

SUBROUTINE INIT(THIS, HPROGRAM)
USE MODD_GLT_PARAM,  ONLY :  GLOBGLTPARAM
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODE_GLT_DIA_LU
  IMPLICIT NONE
  CLASS(GELATO_t) :: THIS
  CHARACTER(LEN=6), INTENT(IN)  :: HPROGRAM

  INTEGER :: ILUOUT         ! logical unit of output file
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('ICE_GELATO:INIT', 0, ZHOOK_HANDLE)

  NULLIFY(THIS%XTICE, THIS%XSIC, THIS%XICE_ALB)

  CALL GET_LUOUT(HPROGRAM,ILUOUT)

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! Setting those Gelato parameters which are not usually set by an
  ! external file but by the gelato library caller program
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! .. Number of categories considered in observations towards which damping is
  ! applied
  GLOBGLTPARAM%ntd=1
  ! .. One input non-solar forcing flux per ice category (nt) or one input
  ! non-solar flux to be shared between all the categories (1)
  GLOBGLTPARAM%nnflxin=1
  ! .. Which is the leading process number (useless now ?)
  GLOBGLTPARAM%gelato_leadproc=0
  ! Adapt proc number and print flags to the Surfex proc numbering scheme
  GLOBGLTPARAM%gelato_myrank=nrank
  GLOBGLTPARAM%lwg=(nrank == npio)


  ! Setting those Gelato parameters which are usually set by an
  ! external file (gltpar)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  !  Gelato model parameters
  ! =========================
  !
  ! .. These parameters can be (theoretically) freely changed by the user
  !
  !
  ! 1. Options to run GELATO
  ! -------------------------
  !
  !  - nmkinit    : create initial conditions file
  !       nmkinit=0  --> use a restart file instead
  !       nmkinit=1  --> use sea ice analytical initialization
  !       nmkinit=2  --> use a sea ice fraction climatology
  !  - nrstout    : create an output restart file
  !       nrstout=0  --> no output restart
  !       nrstout=1  --> output restart will be created
  !  - nrstgl4    : about restart format
  !       nrstgl4=0  --> use an old format restart (before Gelato 4)
  !       nrstgl4=1  --> use a new format restart (Gelato 4 and newer)
  !  - nthermo    : disable/enable sea ice thermodynamics
  !       nthermo=0  --> no thermodynamics
  !       nthermo=1  --> thermodynamics enabled
  !  - ndynami    : disable/enable sea ice dynamics
  !       ndynami=0  --> no dynamics
  !       ndynami=1  --> dynamics enabled
  !  - nadvect    : disable/enable ice transport
  !       nadvect=0  --> no ice transport
  !       nadvect=1  --> ice transport enabled
  !  - ntimers    : disable/enable timers
  !       ntimers=0  --> no timers
  !       ntimers=1  --> timers enabled
  !  - ndyncor    : correct water and salt non-conservation due to advection
  !       ndyncor=0  --> no correction
  !       ndyncor=1  --> correction
  !  - ncdlssh    : take ssh into account when computing concentration/dilution
  !       ncdlssh=0  --> ssh not taken into account
  !       ncdlssh=1  --> ssh taken into account
  !  - niceage    : disable/enable ice age computation
  !       niceage=0  --> no ice age computation
  !       niceage=1  --> ice age computation enabled
  !  - nicesal    : disable/enable ice salinity computation
  !       nicesal=0  --> no ice salinity computation
  !       nicesal=1  --> ice salinity computation enabled
  !  - nmponds    : disable/enable melt pond computation (for ice surface albedo)
  !       nmponds=0  --> no melt ponds computation
  !       nmponds=1  --> melt ponds computation enabled
  !  - nsnwrad    : snowfall radiative effect
  !       nsnwrad=0  --> no radiative effect of snow melting into sea water
  !                      (recommended in case of coupling, if snow fall in
  !                      your atm. model does not cause any heat gain for the
  !                      atmosphere)
  !       nsnwrad=1  --> generates a negative heat flux sent to the ocean by
  !                      Gelato, due to the melting of snow into the ocean
  !  - nleviti    : sea ice is levitating over the ocean or not
  !       nleviti=0  --> sea ice is not levitating (a freshwater flux due to the
  !                      melting/freezing of ice is sent to the ocean model)
  !                  --> sea ice is levitating
  !  - nsalflx    : ice-ocean salt flux parameterisation
  !                 (if 2 or 3, check ocean topmost level dz parameter xhtopoc !)
  !       nsalflx=1  --> approximated calculation
  !       nsalflx=2  --> exact calculation
  !       nsalflx=3  --> exact calculation, but SSS replaced with standard sal.
  !       nsalflx=4  --> simplified calculation as in LIM2 (fixed ocean and ice reference salinity)
  !  - nextqoc    : ocean-ice heat flux
  !       nextqoc=1  --> the %qoc given as an input is taken into account
  !       nextqoc=0  --> the %qoc is computed by Gelato
  !  - nicesub    : ice sublimation
  !       nicesub=1  --> take ice sublimation into account (non heat conservative)
  !       nicesub=0  --> no ice sublimation
  !  - cnflxin    : input fluxes
  !       cnflxin    --> 'mixed' : only one flux, to share between water/ice
  !       cnflxin    --> 'double': one flux for water, one flux for ice
  !       cnflxin    --> 'multi' : one flux for water, one flux for each ice cat
  !
  GLOBGLTPARAM%nmkinit = 0
  GLOBGLTPARAM%nrstout = 0
  GLOBGLTPARAM%nrstgl4 = 1
  GLOBGLTPARAM%nthermo = 1
  GLOBGLTPARAM%ndynami = 0
  GLOBGLTPARAM%nadvect = 0
  GLOBGLTPARAM%ntimers = 0
  GLOBGLTPARAM%ndyncor = 0
  GLOBGLTPARAM%ncdlssh = 1
  GLOBGLTPARAM%niceage = 1
  GLOBGLTPARAM%nicesal = 1
  GLOBGLTPARAM%nmponds = 1
  GLOBGLTPARAM%nsnwrad = 1
  GLOBGLTPARAM%nleviti = 1
  GLOBGLTPARAM%nsalflx = 2
  GLOBGLTPARAM%nextqoc = 0
  GLOBGLTPARAM%nicesub = 1
  GLOBGLTPARAM%cnflxin = 'double'
  !
  !
  ! 2. Damping and restoring
  ! -------------------------
  !
  !  - cfsidmp    : sea ice fraction constraint
  !       cfsidmp='NONE'       --> no sea ice fraction constraint
  !       cfsidmp='DAMP'       --> damp
  !       cfsidmp='PRESCRIBE'  --> prescribe
  !  - xfsidmpeft : sea ice fraction damping e-folding time (in days)
  !  - chsidmp    : sea ice thickness constraint
  !       chsidmp='NONE'       --> no sea ice thickness constraint
  !       chsidmp='DAMP_ADD'   --> damp (thickness of all ice categories is
  !         modified by the same value: h_i => h_i + add)
  !       chsidmp='DAMP_FAC'   --> damp (thickness of all ice categories is
  !         modified by the same factor: h_i => h_i * fac)
  !       chsidmp='PRESCRIBE'  --> prescribe
  !  - xhsidmpeft : sea ice thickness damping e-folding time (in days)
  !
  GLOBGLTPARAM%cfsidmp='NONE'
  GLOBGLTPARAM%xfsidmpeft=0.
  GLOBGLTPARAM%chsidmp='NONE'
  GLOBGLTPARAM%xhsidmpeft=0.
  !
  !
  ! 3. Diagnostics output
  ! ----------------------
  !
  !  - cdiafmt    : diagnostics format
  !       cdiafmt='GELATO'  --> Gelato Vairmer format
  !       cdiafmt='VMAR5'   --> IPCC AR5 vairmer format
  !       cdiafmt='NCAR5'   --> IPCC AR5 NetCDF format (not active yet)
  !  - cdialev    : diagnostics level
  !       . cdialev can include one, two or all of the letters b, d and t
  !       . If cdiafmt='GELATO'
  !           - 1: makes the 2D diag. file (2D fields), called
  !          '2d.[ave|ins].vairmer'. Contains: ice concentration, thickness,
  !          velocity components, thin ice+thick ice, snow thickness).
  !       .   - 2:  add more detailed 2D diagnostics to the 2D diag file,
  !          like: solar short wave flux, non solar flux, water flux crossing
  !          the leads+sea ice ensemble...
  !       .   - 3: makes the 0d.ins.vairmer diag. file (0D fields).
  !          Contains: sea ice area, extent, volume, for both hemispheres +
  !          transports at most Arctic Straits
  !           - Example: cdialev=13 or 31 means that you want only the 2d basic
  !          diagnostics + 0d diagnostic.
  !       . If cdiafmt='VMAR5' or 'NCAR5'
  !           - 1: save only the priority 1 fields
  !           - 2: save only the priority 2 fields
  !           - 3: save only the priority 3 fields
  !           - x: save only my personal fields (additional)
  !           - note fields e.g. in priority 2 fields can have a space dimension
  !          equal to grid size nxglo*nyglo or equal to 1
  !           - VMAR5: output in Vairmer; NCAR5: output in NetCDF.
  !           - Example: cdialev=123x means you want all AR5 fields + yours
  !  - dttave     : period for averaged fields (days, optional, default=365)
  !  - navedia    : average the output over dttave and over the whole run
  !  - ninsdia    : output delivered once per time step
  !  - ndiamax    : maximum number of diagnostic files
  !  - nsavinp    : allows to save gelato routine input in a file (used in
  !                 coupled mode)
  !       nsavinp=0 --> gelato routine input is not saved
  !       nsavinp=1 --> gelato routine input is saved in a file
  !  - nsavout    : allows to save gelato routine output in a file (used in
  !                 coupled mode)
  !       nsavout=0 --> gelato routine input is not saved
  !       nsavout=1 --> gelato routine input is saved in a file
  !  - nupdbud    : compute budgets (for model energy conservation tests)
  !       nupdbud=0  --> no budgets computations (for operational runs)
  !       nupdbud=1  --> budgets computations (for model validation)
  !  - nprinto    : GELATO prints output as follows
  !       nprinto=0  --> minimum output print
  !       nprinto=1  --> print fields statistics : mini, maxi, av.
  !       nprinto=2  --> print fields + field statistics
  !  - nprlast    : GELATO prints output as nprinto levels (last time step only)
  !  - cinsfld    : list of fields to be delivered at every time step
  !                 (all -> deliver all fields)
  !                 Note that one line per requested field should be given, e.g.:
  !                     cinsfld = sit
  !                     cinsfld = sic
  !                     ...
  !
  GLOBGLTPARAM%cdiafmt = 'VMAR5'
  GLOBGLTPARAM%cdialev = ''
  GLOBGLTPARAM%dttave = 30.
  GLOBGLTPARAM%navedia = 0
  GLOBGLTPARAM%ninsdia = 0
  GLOBGLTPARAM%ndiamax = 90
  GLOBGLTPARAM%nsavinp = 0
  GLOBGLTPARAM%nsavout = 0
  GLOBGLTPARAM%nupdbud = 0
  GLOBGLTPARAM%nprinto = 0
  GLOBGLTPARAM%nprlast = 0
  !
  !
  ! 4. Grid definition
  ! -------------------
  !
  !  - cn_grdname   : grid name radical. Defined the grid you are running on.
  !         . Available (pre-coded) options are :
  !         'OPAG8', 'NEMO1', 'ORCA2' or 'MICOM'.
  !         For these precoded grids, any nbndco, nxglo and nyglo values you
  !         will specify in gltpar will be ignored by the code.
  !         . You may specify another cgrdname, but then the nbndco, nxglo
  !         and nyglo values you provide in gltpar MUST MAKE SENSE and will
  !         be taken into account.
  !  - rn_htopoc    : reference thickness (in m) of the topmost ocean level
  !          . This is important if Gelato is coupled to an ocean model, to
  !          send the right concentration / dilution flux to the ocean.
  !
  GLOBGLTPARAM%cn_grdname = 'SURFEX'
  GLOBGLTPARAM%rn_htopoc = 10.
  !
  !
  ! 5. Run date position and time step
  ! -----------------------------------
  !
  !  - nidate     : initial date for running GELATO, YYYYMMDD (-)
  !  - niter      : number of iterations from reference date (-)
  !  - dtt        : time step for dynamics and thermodynamics (s)
  !
  GLOBGLTPARAM%nidate = 20010101
  GLOBGLTPARAM%niter = 100000
  GLOBGLTPARAM%dtt = XUNDEF  ! means : same time step as seaflux
  !
  !
  ! 6. Number of ice categories
  ! ----------------------------
  !
  !  - nt         : number of ice thicknesses (-)
  !  - thick      : boundaries for thickness categories (-)
  !
  GLOBGLTPARAM%nt = 1
  IF (ALLOCATED(GLOBGLTPARAM%thick)) DEALLOCATE( GLOBGLTPARAM%thick )
  ALLOCATE( GLOBGLTPARAM%thick(GLOBGLTPARAM%nt+1) )
  GLOBGLTPARAM%thick(1)= -.01
  GLOBGLTPARAM%thick(2) = 1000.
  !
  !
  ! 7. Number of layers in the ice-snow slab
  ! -----------------------------------------
  !
  ! .. Number of layers when solving the problem of vertical heat
  ! diffusion through the ice and snow slab. Note that if the
  ! scheme is explicit, nslay=1 is compulsory.
  !
  !  - nilay      : number of ice layers in vertical discretisation (-)
  !  - nslay      : number of snow layers in vertical discretisation (-)
  !  - xh*        : vertical coordinate parameters
  !  If you need to run the model with constant vertical levels
  !  (not recommended), specify xh1=1. and xh2=0.
  !
  GLOBGLTPARAM%nilay = 9
  GLOBGLTPARAM%nslay = 1
  GLOBGLTPARAM%xh0 = 4.392339514718992e-01
  GLOBGLTPARAM%xh1 = 1.049607477174487e-01
  GLOBGLTPARAM%xh2 = 9.507487632412231e-02
  GLOBGLTPARAM%xh3 = 1.
  GLOBGLTPARAM%xh4 = 5.208820443636069
  !
  !
  ! 8. Elastic Viscous-Plastic sea ice rheology parameters
  ! -------------------------------------------------------
  !
  !  - ntstp      : number of dynamics time steps during one
  !                 thermodynamics time step.
  !  - ndte       : number of subcycles for velocity computations
  !                 during sea ice EVP dynamics.
  !
  GLOBGLTPARAM%ntstp = 1
  GLOBGLTPARAM%ndte = 100
  !
  !
  ! 9. Limit Values for sea ice
  ! ----------------------------
  !
  !  - xfsimax  : maximum allowable fractional area for sea ice
  !  - xicethcr : ice thickness that represents the limit between thin
  ! and thick ice (m)
  !  - xhsimin  : minimum allowable ice thickness
  !
  GLOBGLTPARAM%xfsimax = .995
  GLOBGLTPARAM%xicethcr = .8
  GLOBGLTPARAM%xhsimin = .2
  !
  !
  ! 10.  Parameterizations
  ! -----------------------
  !
  ! .. If you need a standard parameterization of low clouds (not simulated
  ! by your atmosphere model), a reasonable value for this parameter should
  ! be 0.25. If you don't need this parameterization, use alblc=0.
  ! (it is not recommended to use values other than 0...)
  !  - alblc      : albedo of low clouds
  !  - xlmelt     : lateral melting parameterization factor
  !  - xswhdfr    : fraction of the solar radiation absorbed by snow that
  ! is involved in the vertical heat diffusion (the rest contributes to direct
  ! warming/melting)
  !  - albyngi    : parameterisation of young ice albedo (exponential formulation)
  !       albyngi=0.  --> albedo of young ice does not depend on thickness
  !       albyngi=1.  --> albedo of young ice depends on thickness
  !  - albimlt    : albedo of melting ice
  !  - albsmlt    : albedo of melting snow
  !  - albsdry    : albedo of dry snow
  !
  GLOBGLTPARAM%alblc = 0.
  GLOBGLTPARAM%xlmelt = 3.e-3
  GLOBGLTPARAM%xswhdfr = 1.00
  GLOBGLTPARAM%albyngi = 1.
  GLOBGLTPARAM%albimlt = 0.56
  GLOBGLTPARAM%albsmlt = 0.77
  GLOBGLTPARAM%albsdry = 0.84
  !
  !
  ! 11.  Logical units
  ! -------------------
  !
  !  - ngrdlu     : unit for reading the grid
  !  - nsavlu     : unit for writing input/output fields for Gelato
  !  - nrstlu     : unit for reading/writing Gelato restart
  !  - n0vilu     : unit for writing 0D Glt Instantaneous diags
  !  - n0valu     : unit for writing 0D Glt Averaged diags
  !  - n2vilu     : unit for writing 2D Glt or IPCC-AR5 Instantaneous diags
  !  - n2valu     : unit for writing 2D Glt or IPCC-AR5 Averaged diags
  !  - nxvilu     : unit for writing Instantaneous additional diags (AR5 case)
  !  - nxvalu     : unit for writing Averaged additional diags (AR5 case)
  !  - nibglu     : unit for iceberg physics input/output
  !  - nspalu     : spare unit for personal use !
  !  - noutlu     : unit for GELATO output
  !  - ntimlu     : unit for GELATO timers
  !
  GLOBGLTPARAM%ngrdlu = 153
  GLOBGLTPARAM%nsavlu = 111
  GLOBGLTPARAM%nrstlu = 151
  GLOBGLTPARAM%n0vilu = 123
  GLOBGLTPARAM%n0valu = 125
  GLOBGLTPARAM%n2vilu = 121
  GLOBGLTPARAM%n2valu = 122
  GLOBGLTPARAM%nxvilu = 133
  GLOBGLTPARAM%nxvalu = 131
  GLOBGLTPARAM%nibglu = 120
  GLOBGLTPARAM%nspalu = 130
  GLOBGLTPARAM%noutlu = ILUOUT
  GLOBGLTPARAM%ntimlu = 201
  !
  !
  ! 12. Path to keep Gelato I/O fields
  ! -----------------------------------
  !
  ! .. You must define this path (complete), but without "/" at the end if
  ! you want to keep Gelato daily input/output variables (for example to
  ! "replay" a simulation with input/output data obtained in coupled mode).
  ! This variable is used only if nsavinp=1 or nsavout=1.
  !
  !  - ciopath    : path for input/output fields to gelato routine
  !
  GLOBGLTPARAM%ciopath = '.'

  !COPY THE DEFAULT VALUES INSIDE CURRENT BLOCK
  THIS%GLTPARAM=GLOBGLTPARAM

#if ! defined in_arpege
 !BROKEN INTERFACE CONTACT NAPOLY ADRIEN METEO FRANCE 
 CALL OPNDIA(THIS%GLTPARAM%n0valu,THIS%GLTPARAM%n0vilu,THIS%GLTPARAM%n2valu, &
             THIS%GLTPARAM%n2vilu,THIS%GLTPARAM%navedia,THIS%GLTPARAM%ndiap1,&
             THIS%GLTPARAM%ndiap2,THIS%GLTPARAM%ndiap3,THIS%GLTPARAM%ndiapx, &
             THIS%GLTPARAM%ninsdia,THIS%GLTPARAM%noutlu,THIS%GLTPARAM%nxvalu,&
             THIS%GLTPARAM%nxvilu,THIS%GLTPARAM%lp1,THIS%GLTPARAM%lwg,       &
             THIS%GLTPARAM%cdiafmt)
#endif

  IF (LHOOK) CALL DR_HOOK('ICE_GELATO:INIT', 1, ZHOOK_HANDLE)
END SUBROUTINE INIT

SUBROUTINE PREP(THIS, DTCO, U, GCP, YDCTL, KLU, KLAT, &
                HPROGRAM, HATMFILE, HATMFILETYPE, HPGDFILE, HPGDFILETYPE)
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_DATA_COVER_n, ONLY: DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY: SURF_ATM_t
USE MODD_GRID_CONF_PROJ_n, ONLY: GRID_CONF_PROJ_t
USE MODE_PREP_CTL, ONLY: PREP_CTL
USE MODD_PREP_SEAFLUX,   ONLY : XSIC_UNIF
USE MODI_GLTOOLS_ALLOC
USE MODI_GLTOOLS_READNAM
USE MODI_PREP_HOR_SEAICE_FIELD
USE MODI_READ_PREP_SEAFLUX_CONF
USE MODI_OPEN_AUX_IO_SURF
USE MODI_READ_SURF
USE MODI_CLOSE_AUX_IO_SURF
  IMPLICIT NONE
  CLASS(GELATO_t) :: THIS
  TYPE(DATA_COVER_t),    INTENT(INOUT) :: DTCO
  TYPE(SURF_ATM_t),      INTENT(INOUT) :: U
  TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
  TYPE (PREP_CTL),       INTENT(INOUT) :: YDCTL
  INTEGER, INTENT(IN) :: KLU
  INTEGER, INTENT(IN) :: KLAT
  CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  !< program calling surf. schemes
  CHARACTER(LEN=28),  INTENT(IN)  :: HATMFILE    !< name of the Atmospheric file
  CHARACTER(LEN=6),   INTENT(IN)  :: HATMFILETYPE!< type of the Atmospheric file
  CHARACTER(LEN=28),  INTENT(IN)  :: HPGDFILE    !< name of the PGD file
  CHARACTER(LEN=6),   INTENT(IN)  :: HPGDFILETYPE!< type of the PGD file

  INTEGER :: JI
  INTEGER :: ILUOUT
  LOGICAL :: GFOUND         ! Return code when searching namelist
  INTEGER :: ILUNAM         ! logical unit of namelist file

  CHARACTER(LEN=6)  :: YFILETYPE ! type of input file
  CHARACTER(LEN=28) :: YFILE     ! name of file
  CHARACTER(LEN=6)  :: YFILEPGDTYPE ! type of input file
  CHARACTER(LEN=28) :: YFILEPGD     ! name of file
  LOGICAL           :: GUNIF     ! flag for prescribed uniform field
  CHARACTER(LEN=30) :: YWORK
  INTEGER           :: IRESP
  CHARACTER(LEN=9) :: YFIELD
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('ICE_GELATO:PREP', 0, ZHOOK_HANDLE)

  CALL GET_LUOUT(HPROGRAM,ILUOUT)
  CALL GLTOOLS_READNAM(.FALSE.,ILUOUT,THIS%GLTPARAM)

  !-------------------------------------------------------------------------------------
  !
  !*      Creating default initial state for Gelato
  !
  !
  THIS%GLTPARAM%nx = KLU
  THIS%GLTPARAM%ny=1
  THIS%GLTPARAM%nyglo=1
  THIS%GLTPARAM%nxglo=THIS%GLTPARAM%nx
  CALL GLTOOLS_ALLOC( &
    THIS%TGLT,THIS%GLTPARAM%ndiamax,THIS%GLTPARAM%ndynami,THIS%GLTPARAM%nl,&
    THIS%GLTPARAM%nnflxin,THIS%GLTPARAM%noutlu,THIS%GLTPARAM%nt,        &
    THIS%GLTPARAM%ntd,THIS%GLTPARAM%nx,THIS%GLTPARAM%ny,THIS%GLTPARAM%lp1  )

  CALL READ_PREP_SEAFLUX_CONF(.FALSE., & ! LMERCATOR
    HPROGRAM,'SIC      ',YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,&
    HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,ILUOUT,GUNIF)
  !read seaice scheme
  CALL OPEN_AUX_IO_SURF(YFILE,YFILETYPE,'FULL  ')
  CALL READ_SURF(YFILETYPE,'SEAICE_SCHEM',YWORK,IRESP,HDIR='A')
  CALL CLOSE_AUX_IO_SURF(YFILE,YFILETYPE)

  IF (XSIC_UNIF == XUNDEF .AND. TRIM(YWORK)=='GELATO') THEN

    CALL PREP_HOR_SEAICE_FIELD( &
      DTCO, U, GCP, KLAT, HPROGRAM,'ICEUSTAR ',HATMFILE,HATMFILETYPE, &
      HPGDFILE,HPGDFILETYPE,YDCTL, THIS%TGLT%ust(:,1))
    CALL PREP_HOR_SEAICE_FIELD( &
      DTCO, U, GCP, KLAT, HPROGRAM,'ICEAGE_1 ',HATMFILE,HATMFILETYPE, &
      HPGDFILE,HPGDFILETYPE,YDCTL, THIS%TGLT%sit(1,:,1)%age)
    CALL PREP_HOR_SEAICE_FIELD( &
      DTCO, U, GCP, KLAT, HPROGRAM,'ICEVMP_1 ',HATMFILE,HATMFILETYPE, &
      HPGDFILE,HPGDFILETYPE,YDCTL, THIS%TGLT%sit(1,:,1)%vmp)
    CALL PREP_HOR_SEAICE_FIELD( &
      DTCO, U, GCP, KLAT, HPROGRAM,'ICEASN_1 ',HATMFILE,HATMFILETYPE, &
      HPGDFILE,HPGDFILETYPE,YDCTL, THIS%TGLT%sit(1,:,1)%asn)
    CALL PREP_HOR_SEAICE_FIELD( &
      DTCO, U, GCP, KLAT, HPROGRAM,'ICEFSI_1 ',HATMFILE,HATMFILETYPE, &
      HPGDFILE,HPGDFILETYPE,YDCTL, THIS%TGLT%sit(1,:,1)%fsi)
    CALL PREP_HOR_SEAICE_FIELD( &
      DTCO, U, GCP, KLAT, HPROGRAM,'ICESSI_1 ',HATMFILE,HATMFILETYPE, &
      HPGDFILE,HPGDFILETYPE,YDCTL, THIS%TGLT%sit(1,:,1)%ssi)
    CALL PREP_HOR_SEAICE_FIELD( &
      DTCO, U, GCP, KLAT, HPROGRAM,'ICEHSI_1 ',HATMFILE,HATMFILETYPE, &
      HPGDFILE,HPGDFILETYPE,YDCTL, THIS%TGLT%sit(1,:,1)%hsi)
    CALL PREP_HOR_SEAICE_FIELD( &
      DTCO, U, GCP, KLAT, HPROGRAM,'ICETSF_1 ',HATMFILE,HATMFILETYPE, &
      HPGDFILE,HPGDFILETYPE,YDCTL, THIS%TGLT%sit(1,:,1)%tsf)
    CALL PREP_HOR_SEAICE_FIELD( &
      DTCO, U, GCP, KLAT, HPROGRAM,'ICEHSN_1 ',HATMFILE,HATMFILETYPE, &
      HPGDFILE,HPGDFILETYPE,YDCTL, THIS%TGLT%sit(1,:,1)%hsn)
    CALL PREP_HOR_SEAICE_FIELD( &
      DTCO, U, GCP, KLAT, HPROGRAM,'ICERSN_1 ',HATMFILE,HATMFILETYPE, &
      HPGDFILE,HPGDFILETYPE,YDCTL, THIS%TGLT%sit(1,:,1)%rsn)
    DO JI = 1, 10
      IF(JI < 10) THEN
        WRITE(YFIELD, '("ICEH_1_",I1)') JI
      ELSE
        WRITE(YFIELD, '("ICEH_1_",I2)') JI
      ENDIF

      CALL PREP_HOR_SEAICE_FIELD( &
        DTCO, U, GCP, KLAT, HPROGRAM,YFIELD,HATMFILE,HATMFILETYPE, &
        HPGDFILE,HPGDFILETYPE,YDCTL, THIS%TGLT%sil(JI,1,:,1)%ent)
    END DO
  ELSE
    !
    !*       G1    Prognostic fields with only space dimension(s) :
    !
    THIS%TGLT%ust(:,1)=0.
    !
    !*       G2     Prognostic fields with space and ice-category dimension(s) :
    !
    ! sea ice age
    THIS%TGLT%sit(:,:,1)%age=0.
    ! melt pond volume
    THIS%TGLT%sit(:,:,1)%vmp=0.
    ! sea ice surface albedo
    THIS%TGLT%sit(:,:,1)%asn=0.
    ! sea ice fraction
    THIS%TGLT%sit(:,:,1)%fsi=0.
    ! sea ice thickness
    THIS%TGLT%sit(:,:,1)%hsi=1.*THIS%TGLT%sit(:,:,1)%fsi
    ! sea ice salinity
    THIS%TGLT%sit(:,:,1)%ssi=0.
    ! sea ice surface temperature
    THIS%TGLT%sit(:,:,1)%tsf=260.
    ! snow thickness
    THIS%TGLT%sit(:,:,1)%hsn=0.
    ! snow density
    THIS%TGLT%sit(:,:,1)%rsn=100.
    !
    !*       G3     Prognostic fields with space, ice-category and layer dimensions :
    !
    ! sea ice vertical gltools_enthalpy profile for all types and levels
    THIS%TGLT%sil(:,:,:,1)%ent=-1000.
  ENDIF

  IF (LHOOK) CALL DR_HOOK('ICE_GELATO:PREP', 1, ZHOOK_HANDLE)
END SUBROUTINE PREP

SUBROUTINE ASSIM(THIS, HPROGRAM, PSIC_IN, PLON_IN, PLAT_IN)
USE MODI_ABOR1_SFX
IMPLICIT NONE
  CLASS(GELATO_t) :: THIS
  CHARACTER(LEN=6),   INTENT(IN) :: HPROGRAM
  REAL,               INTENT(IN) :: PSIC_IN(:)
  REAL,               INTENT(IN) :: PLON_IN(:)
  REAL,               INTENT(IN) :: PLAT_IN(:)

  CALL ABOR1_SFX('Not implemented')
END SUBROUTINE ASSIM

SUBROUTINE RUN( &
    THIS, HPROGRAM, PTIMEC, PTSTEP, KSTEP, ESM_CPL, PFSIC, PFSIT, PSI_FLX_DRV, PFREEZING_SST, &
    PZENITH, PSW_TOT, PLW, &
    PPEW_A_COEF, PPEW_B_COEF, PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF)
!     #######################################################################
!
!!****  *SEAICE_GELATO1D_n*
!!
!!    PURPOSE
!!    -------
!     Run Gelato Sea-ice model, which provides sea-ice cover, temperature
!     and albedo
!
!!**  METHOD
!!    ------
!     At relevant time steps :
!     i) Feed Gelato input interface structures with relevant variables
!        taken in XCPL_xxx fields from MODD_SEAFLUX_n
!     ii) call the minimal Gelato sequence , namely :
!        a) ingest and transform inputs regarding atm and sea state,
!        b) run Gelato ,
!        c) have it process its ouptuts for atm and oce,
!     iii) feed output arguments using content of Gelato output interface
!        structure
!
!     Note : for the time being, SST and SSS are not averaged over coupling
!     period
!
!!    EXTERNAL :
!!    ---------
!!    - a number of glt<xxx> routines
!!
!!    IMPLICIT ARGUMENTS :
!!    -------------------
!!    Variables XCPL_xxx in MODD_SEAFLUX_n, and also LINTERPOL_SIC and LINTERPOL_SIT
!!
!!    REFERENCE :
!!    ---------
!!    Salas y Melia D (2002) A global coupled sea ice-ocean model.
!!    Ocean Model 4:137-172
!!
!!    AUTHOR
!!    ------
!!     S.Senesi  *Meteo-France* CNRM - GAME
!!
!!    MODIFICATIONS
!!    -------------
!!      Original     01/2014
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE ABSTRACT_ICE, ONLY : ESM_CPL_t
!
USE MODD_CSTS,ONLY : XTT
USE MODD_SURF_PAR,   ONLY : XUNDEF

USE MODI_GLT_GELATO
USE MODI_GLT_SNDATMF
USE MODI_GLT_SNDMLRF
USE MODI_GLT_GETMLRF
USE MODI_GLT_GETATMF
USE MODI_GLTOOLS_CHKINP
USE MODI_GLTOOLS_CHKOUT
USE MODE_GLT_STATS
USE MODE_GLTOOLS_SWFRZT
USE MODE_GLT_DIA_AR5
!
!
USE MODI_GET_LUOUT
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
  !
  !*      0.1    declarations of arguments
  !
  !
  !
  CLASS(GELATO_t) :: THIS
  CHARACTER(LEN=6),    INTENT(IN) :: HPROGRAM  ! program calling surf. schemes
  REAL,                INTENT(IN) :: PTIMEC    ! current duration since start of the run (s)
  REAL,                INTENT(IN) :: PTSTEP    ! surface time-step (s)
  INTEGER, INTENT(IN) :: KSTEP
  TYPE(ESM_CPL_t), INTENT(INOUT) :: ESM_CPL
  REAL, INTENT(IN) :: PFSIC(:)
  REAL, INTENT(IN) :: PFSIT(:)
  REAL, INTENT(IN) :: PSI_FLX_DRV
  REAL, INTENT(IN) :: PFREEZING_SST

  REAL, INTENT(IN) :: PZENITH(:) !< Zenithal angle at t  (radian from the vertical)
  REAL, INTENT(IN) :: PSW_TOT(:) !< Shortwave radiation flux at the surface.
  REAL, INTENT(IN) :: PLW(:) !< Longwave radiation flux at the surface.

  REAL, INTENT(IN) :: PPEW_A_COEF(:) ! implicit coefficients   (m2s/kg)
  REAL, INTENT(IN) :: PPEW_B_COEF(:) ! needed if HCOUPLING='I' (m/s)
  REAL, INTENT(IN) :: PPET_A_COEF(:)
  REAL, INTENT(IN) :: PPEQ_A_COEF(:)
  REAL, INTENT(IN) :: PPET_B_COEF(:)
  REAL, INTENT(IN) :: PPEQ_B_COEF(:)
  !
  !*      0.2    declarations of local variables
  !
  REAL, DIMENSION(  SIZE(THIS%XSST)) :: ZSST  ! sea surface temperature with frozen sea set at freezing point
  REAL, DIMENSION(  SIZE(THIS%XSST)) :: ZSIC  ! Will hold a forcing SIC field, even if PFSIC is missing
  !
  INTEGER :: IT      ! total number of Gelato timesteps in one atmospheric timestep
  INTEGER :: JT      ! Running index 1 -> IT
  REAL    :: ZT      ! total number of Gelato timesteps in one atmospheric timestep
  REAL    :: ZTIMEG  ! Gelato simulation time (s)

  ! The time step for ice coupling could be a namelist parameter
  ! for now, it will be set, here, as equal to the ice model time step
  REAL :: ZICE_COUPLING_TSTEP
  !
  !
  INTEGER         :: ILUOUT              ! output listing logical unit
  REAL(KIND=JPRB) :: ZHOOK_HANDLE
  !
  !-------------------------------------------------------------------------------
  !
  IF (LHOOK) CALL DR_HOOK('ICE_GELATO:RUN',0,ZHOOK_HANDLE)
  !
  CALL GET_LUOUT(HPROGRAM,ILUOUT)
  !
  ! Must restore Gelato problem size (nx) to the correct value for the NPROMA block
  !
  THIS%GLTPARAM%nx=SIZE(THIS%XSSS)
  !
  ! Time steps stuff : default Gelato time step equals surface time step
  !
  IT = KSTEP
  IF(IT == 1) THEN
    ZT  = 1
    THIS%GLTPARAM%DTT=PTSTEP
  ELSE
    ZT=FLOAT(IT)
    THIS%GLTPARAM%DTT=PTSTEP/ZT
  END IF
  !
  !          Initializations
  !________________________________________________________________________
  !
  ! Ocean part
  !----------------------------------------------------------------------------------
  ! Surface salinity
  THIS%TGLT%oce_all(:,1)%sml=THIS%XSSS(:)

  ! Ensure that SSS-dependant freezing point temperature is used on
  ! locations where SIC (or SST) forcing value calls for it

  ! First init ZSST with freezing temperature (which depends on local salinity)
  ! (Gelato uses Celsius scale for freezing temperatures)
  ZSST=RESHAPE(glt_swfrzt2d(RESHAPE(THIS%XSSS,[SIZE(THIS%XSSS),1]), & 
               THIS%GLTPARAM%nx,THIS%GLTPARAM%ny) + XTT,[SIZE(THIS%XSSS)])

  ! Then replace freezing temp with Surfex-provided SST (THIS%XSST) where
  ! there is no (explicit or implicit) seaice and temperature is warmer
  ! than freezing point. And inits ZSIC accordingly
  IF (THIS%LINTERPOL_SIC) THEN
     ZSIC = PFSIC
     WHERE ( ZSIC(:) < 1.e-10 .AND. THIS%XSST(:) > ZSST(:) )
        ZSST(:)=THIS%XSST(:)
     ENDWHERE
  ELSE
     ! Implicit sea-ice cover
     WHERE (THIS%XSST(:) - XTT > PFREEZING_SST + 0.1 )
        ZSST(:)= THIS%XSST(:)
        ZSIC(:)=0.
     ELSEWHERE
        ZSIC(:)=1.
     ENDWHERE
  ENDIF
  !
  !        Run Gelato for the length of the atmospheric time step
  !________________________________________________________________________
  !
  !  Reset output accumulation/averaging fields
  THIS%XSIC    = 0.
  THIS%XTICE   = 0.
  THIS%XICE_ALB= 0.
  THIS%GLTPARAM%LP1 = (THIS%GLTPARAM%LWG.AND.THIS%GLTPARAM%NPRINTO>=1)
  THIS%GLTPARAM%LP2 = (THIS%GLTPARAM%LWG.AND.THIS%GLTPARAM%NPRINTO>=2)
  THIS%GLTPARAM%LP3 = (THIS%GLTPARAM%LWG.AND.THIS%GLTPARAM%NPRINTO>=3)
  THIS%GLTPARAM%LP4 = (THIS%GLTPARAM%LWG.AND.THIS%GLTPARAM%NPRINTO>=4)
  THIS%GLTPARAM%LP5 = (THIS%GLTPARAM%LWG.AND.THIS%GLTPARAM%NPRINTO>=5)

  DO JT=1,IT
     IF (SIZE(THIS%XSSS) > 0) THEN
        THIS%TGLT%oce_all(:,1)%tml=ZSST(:)
        IF (THIS%LINTERPOL_SIC) THIS%TGLT%sit_d(1,:,1)%fsi=ZSIC(:)
        IF (THIS%LINTERPOL_SIT) THIS%TGLT%sit_d(1,:,1)%hsi=PFSIT(:)
        ! Gelato will compute heat flux from ocean by itself, thanks to
        ! imposed namelist parameter nextqoc=0
        THIS%TGLT%oce_all(:,1)%qoc=0.
        ! Zero Frazil flux
        THIS%TGLT%oce_all(:,1)%qml=0.
        ! Don't bother for sea level variations
        THIS%TGLT%oce_all(:,1)%ssh=0.
        ! (velocity components are useless in Surfex 1D setting)
        !
        ! Atmosphere part
        !----------------
        ! Feed Gelato input structure with flux values from XCPL_xx
        !
        THIS%TGLT%atm_all(:,1)%lip=ESM_CPL%XCPL_SEA_RAIN(:) / PTSTEP
        THIS%TGLT%atm_all(:,1)%sop=ESM_CPL%XCPL_SEA_SNOW(:) / PTSTEP
        ! Fluxes over Sea water
        THIS%TGLT%atm_wat(:,1)%eva=ESM_CPL%XCPL_SEA_EVAP(:) / PTSTEP
        THIS%TGLT%atm_wat(:,1)%swa=ESM_CPL%XCPL_SEA_SNET(:) / PTSTEP
        THIS%TGLT%atm_wat(:,1)%nsf=ESM_CPL%XCPL_SEA_HEAT(:) / PTSTEP
        THIS%TGLT%atm_wat(:,1)%dfl=PSI_FLX_DRV ! W m-2 K-1
        ! Fluxes over Sea ice
        THIS%TGLT%atm_ice(1,:,1)%eva=ESM_CPL%XCPL_SEAICE_EVAP(:) / PTSTEP
        THIS%TGLT%atm_ice(1,:,1)%swa=ESM_CPL%XCPL_SEAICE_SNET(:) / PTSTEP
        THIS%TGLT%atm_ice(1,:,1)%nsf=ESM_CPL%XCPL_SEAICE_HEAT(:) / PTSTEP
        THIS%TGLT%atm_ice(1,:,1)%dfl=PSI_FLX_DRV ! W m-2 K-1
        ! (stress components are useless in Surfex 1D setting)
        !
        !       Let Gelato process its input data
        !
        CALL GLT_GETMLRF(THIS%TGLT%oce_all,THIS%TGLT%tml,THIS%GLTPARAM%nx,THIS%GLTPARAM%ny)
        CALL GLT_GETATMF(THIS%TGLT, &
          THIS%GLTPARAM%nnflxin, THIS%GLTPARAM%noutlu, THIS%GLTPARAM%nt, THIS%GLTPARAM%nx, &
          THIS%GLTPARAM%ny, THIS%GLTPARAM%lp1, THIS%GLTPARAM%lwg)
        CALL GLTOOLS_CHKINP( 20010101,THIS%TGLT, &
          THIS%GLTPARAM%n0vilu,   &
          THIS%GLTPARAM%n2vilu,   &
          THIS%GLTPARAM%nnflxin,  &
          THIS%GLTPARAM%noutlu,   &
          THIS%GLTPARAM%nprinto,  &
          THIS%GLTPARAM%nsavinp,  &
          THIS%GLTPARAM%nsavlu,   &
          THIS%GLTPARAM%nt,       &
          THIS%GLTPARAM%ntd,      &
          THIS%GLTPARAM%nx,       &
          THIS%GLTPARAM%nxglo,    &
          THIS%GLTPARAM%ny,       &
          THIS%GLTPARAM%nyglo,    &
          THIS%GLTPARAM%xdomsrf_g,&
          THIS%GLTPARAM%lwg,      &
          THIS%GLTPARAM%ciopath)
        !
        ! Compute gelato time index
        !
        THIS%TGLT%IND%CUR = ( PTIMEC + JT * THIS%GLTPARAM%DTT ) / THIS%GLTPARAM%DTT
        !
        !       Let Gelato thermodynamic scheme run
        !
        CALL GLT_GELATO( THIS%TGLT, THIS%GLTPARAM, THIS%GLTVHD )
        !
        ! Have Gelato feed its coupling ouptut interface
        !
        CALL GLT_SNDATMF( &
          THIS%TGLT, THIS%GLTPARAM%nnflxin, THIS%GLTPARAM%alblc)
        CALL GLT_SNDMLRF( &
          THIS%TGLT%bat,THIS%TGLT%dom,THIS%TGLT%atm_all,THIS%TGLT%tml, &
          THIS%TGLT%dia,THIS%TGLT%sit,THIS%TGLT%tfl,THIS%TGLT%ust,THIS%TGLT%all_oce, &
          THIS%GLTPARAM%nadvect,THIS%GLTPARAM%ncdlssh,THIS%GLTPARAM%ndyncor, &
          THIS%GLTPARAM%nleviti,THIS%GLTPARAM%nsalflx,THIS%GLTPARAM%nt,THIS%GLTPARAM%nx, &
          THIS%GLTPARAM%ny,THIS%GLTPARAM%dtt,THIS%GLTPARAM%rn_htopoc)

        CALL WRIDIA_AR5( &
          THIS%TGLT, &
          THIS%GLTPARAM%gelato_leadproc, &
          THIS%GLTPARAM%gelato_myrank,   &
          THIS%GLTPARAM%n0valu,          &
          THIS%GLTPARAM%n0vilu,          &
          THIS%GLTPARAM%n2valu,          &
          THIS%GLTPARAM%n2vilu,          &
          THIS%GLTPARAM%navedia,         &
          THIS%GLTPARAM%ndiamax,         &
          THIS%GLTPARAM%ndiap1,          &
          THIS%GLTPARAM%ndiap2,          &
          THIS%GLTPARAM%ndiap3,          &
          THIS%GLTPARAM%niceage,         &
          THIS%GLTPARAM%nicesal,         &
          THIS%GLTPARAM%ninsdia,         &
          THIS%GLTPARAM%nleviti,         &
          THIS%GLTPARAM%nmponds,         &
          THIS%GLTPARAM%noutlu,          &
          THIS%GLTPARAM%nt,              &
          THIS%GLTPARAM%nx,              &
          THIS%GLTPARAM%nxglo,           &
          THIS%GLTPARAM%ny,              &
          THIS%GLTPARAM%nyglo,           &
          THIS%GLTPARAM%dtt,             &
          THIS%GLTPARAM%dttave,          &
          THIS%GLTPARAM%lp1,             &
          THIS%GLTPARAM%lwg,             &
          THIS%GLTPARAM%cdiafmt,         &
          THIS%GLTPARAM%cinsfld)
        CALL GLTOOLS_CHKOUT( &
           20010101,               &
           THIS%TGLT,              &
           THIS%GLTPARAM%n0vilu,   &
           THIS%GLTPARAM%n2vilu,   &
           THIS%GLTPARAM%nnflxin,  &
           THIS%GLTPARAM%noutlu,   &
           THIS%GLTPARAM%nprinto,  &
           THIS%GLTPARAM%nsavlu,   &
           THIS%GLTPARAM%nsavout,  &
           THIS%GLTPARAM%nt,       &
           THIS%GLTPARAM%nx,       &
           THIS%GLTPARAM%nxglo,    &
           THIS%GLTPARAM%ny,       &
           THIS%GLTPARAM%nyglo,    &
           THIS%GLTPARAM%xdomsrf_g,&
           THIS%GLTPARAM%lp1,      &
           THIS%GLTPARAM%lwg,      &
           THIS%GLTPARAM%ciopath)
        ! Sum output fields over Gelato model time step duration
        THIS%XSIC     = THIS%XSIC     + THIS%TGLT%ice_atm(1,:,1)%fsi * THIS%GLTPARAM%DTT
        THIS%XTICE    = THIS%XTICE    + THIS%TGLT%ice_atm(1,:,1)%tsf * THIS%GLTPARAM%DTT
        THIS%XICE_ALB = THIS%XICE_ALB + THIS%TGLT%ice_atm(1,:,1)%alb * THIS%GLTPARAM%DTT
     ENDIF
  END DO
  !   Average output fields over coupling time
  THIS%XSIC     = THIS%XSIC     / (IT * THIS%GLTPARAM%DTT)
  THIS%XTICE    = THIS%XTICE    / (IT * THIS%GLTPARAM%DTT)
  THIS%XICE_ALB = THIS%XICE_ALB / (IT * THIS%GLTPARAM%DTT)
  !
  ! Resets input accumulation fields for next step
  !
  ESM_CPL%XCPL_SEA_RAIN=0.
  ESM_CPL%XCPL_SEA_SNOW=0.
  ESM_CPL%XCPL_SEA_EVAP=0.
  ESM_CPL%XCPL_SEA_SNET=0.
  ESM_CPL%XCPL_SEA_HEAT=0.
  ESM_CPL%XCPL_SEAICE_EVAP=0.
  ESM_CPL%XCPL_SEAICE_SNET=0.
  ESM_CPL%XCPL_SEAICE_HEAT=0.
  !
  IF (LHOOK) CALL DR_HOOK('ICE_GELATO:RUN',1,ZHOOK_HANDLE)
  !!------------------------------------------------------------------------------
  !!------------------------------------------------------------------------------
END SUBROUTINE RUN

SUBROUTINE DEALLOC(THIS)
USE MODI_GLTOOLS_DEALLOC
USE MODE_GLT_DIA_LU
  IMPLICIT NONE
  CLASS(GELATO_t) :: THIS
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('ICE_GELATO:DEALLOC', 0, ZHOOK_HANDLE)

  IF(ASSOCIATED(THIS%XSIC)) THEN
    DEALLOCATE(THIS%XSIC)
    DEALLOCATE(THIS%XTICE)
    DEALLOCATE(THIS%XICE_ALB)
  END IF

  IF (ALLOCATED(THIS%TGLT%bat)) THEN
    CALL GLTOOLS_DEALLOC(THIS%TGLT, THIS%GLTPARAM%nnflxin, THIS%GLTPARAM%noutlu, THIS%GLTPARAM%ntd, THIS%GLTPARAM%lwg)
  END IF

#if ! defined in_arpege
!BROKEN INTERFACE CONTACT NAPOLY ADRIEN METEO FRANCE 
  CALL CLSDIA(THIS%GLTPARAM%n0valu,THIS%GLTPARAM%n0vilu,THIS%GLTPARAM%n2valu, &
            THIS%GLTPARAM%n2vilu,THIS%GLTPARAM%navedia,THIS%GLTPARAM%ndiap1,&
            THIS%GLTPARAM%ndiap2,THIS%GLTPARAM%ndiap3,THIS%GLTPARAM%ndiapx, &
            THIS%GLTPARAM%ninsdia,THIS%GLTPARAM%noutlu,THIS%GLTPARAM%nxvalu,&
            THIS%GLTPARAM%nxvilu,THIS%GLTPARAM%lp1,THIS%GLTPARAM%lwg,       &
            THIS%GLTPARAM%cdiafmt)
#endif

  IF (LHOOK) CALL DR_HOOK('ICE_GELATO:DEALLOC', 1, ZHOOK_HANDLE)
END SUBROUTINE DEALLOC

SUBROUTINE READSURF(THIS, G, HPROGRAM, KLU, KLUOUT)
USE MODD_SFX_GRID_n, ONLY : GRID_t
!
USE MODD_CSTS, ONLY           : XPI, XTTSI, XTT
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_SFX_OASIS,      ONLY : LCPL_SEAICE
USE MODD_WATER_PAR,      ONLY : XALBSEAICE
!
USE MODD_GLT_CONST_THM, ONLY : epsil1
USE LIB_MPP,            ONLY : MPP_SUM
USE MODI_GLT_SNDATMF
USE MODI_GLTOOLS_ALLOC
USE MODI_GLTOOLS_READNAM
!
USE MODI_READ_SURF
USE MODI_INTERPOL_SST_MTH
!
USE MODI_GET_LUOUT
USE MODI_ABOR1_SFX
!
  IMPLICIT NONE
  CLASS(GELATO_t) :: THIS
  TYPE(GRID_t), INTENT(INOUT)   :: G
  CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM
  INTEGER,           INTENT(IN) :: KLU
  INTEGER,           INTENT(IN) :: KLUOUT

  INTEGER           :: IRESP          ! Error code after reading
  !
  CHARACTER(LEN=5)  :: YLVL
  !
  CHARACTER(LEN=12) :: YCATEG         ! category to read
  CHARACTER(LEN=12) :: YLEVEL         ! Level to read
  CHARACTER(LEN=200) :: YMESS         ! Error Message

  INTEGER :: JX,JK,JL                 ! loop counter on ice categories and layers and grid points
  INTEGER :: inl_in_file,int_in_file  ! file values for ice catgories and layers numbers
  REAL :: ZFSIT
  !
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('ICE_GELATO:READSURF', 0, ZHOOK_HANDLE)

  IF(.NOT. ASSOCIATED(THIS%XTICE)) THEN
    ALLOCATE( &
      THIS%XTICE(KLU), &
      THIS%XSIC(KLU), &
      THIS%XICE_ALB(KLU) &
      )
  END IF
  !
  THIS%GLTPARAM%nx=KLU
  THIS%GLTPARAM%nxglo=THIS%GLTPARAM%nx
#if ! defined in_arpege
  CALL mpp_sum(THIS%GLTPARAM%nxglo) ! Should also sum up over NPROMA blocks, in Arpege; but not that easy....
#else
  IF (NPRINTO > 0) THEN
     WRITE(KLUOUT,*)'Gelato cannot yet compute global averages when running in Arpege (because of collective comm vs. NPROMA blocks)'
  ENDIF
  THIS%GLTPARAM%nxglo=max(THIS%GLTPARAM%nxglo,1)
#endif

! TODO:
! Try to conserve volume (or not) in case of a sea ice thickness constraint
!S%GLTPARAM%CCSVDMP=S%CONSTRAIN_CSV

  !
  !* Physical dimensions are set for Gelato , as a 1D field (second dimension is degenerated)
  !
  ! Supersedes Gelato hard defaults with a Gelato genuine namelist
  ! if available (for Gelato wizzards !)
  CALL GLTOOLS_READNAM(.FALSE., KLUOUT, THIS%GLTPARAM)
  !
  THIS%GLTPARAM%ny=1
  THIS%GLTPARAM%nyglo=1
  CALL GLTOOLS_ALLOC( &
    THIS%TGLT, &
    THIS%GLTPARAM%ndiamax, THIS%GLTPARAM%ndynami, THIS%GLTPARAM%nl, THIS%GLTPARAM%nnflxin,&
    THIS%GLTPARAM%noutlu, THIS%GLTPARAM%nt, THIS%GLTPARAM%ntd, THIS%GLTPARAM%nx, THIS%GLTPARAM%ny,&
    THIS%GLTPARAM%lp1)
  !
  !*       0.     Check dimensions : number of layers and ice categories
  !
  CALL READ_SURF(HPROGRAM,'ICENL',inl_in_file,IRESP)
  IF (inl_in_file /= THIS%GLTPARAM%nl) THEN
     WRITE(YMESS,'("Mismatch in # of seaice layers : prep=",I2," nml=",I2)') inl_in_file, THIS%GLTPARAM%nl
     CALL ABOR1_SFX(YMESS)
  END IF
  CALL READ_SURF(HPROGRAM,'ICENT',int_in_file,IRESP)
  IF (int_in_file /= THIS%GLTPARAM%nt) THEN
     WRITE(YMESS,'("Mismatch in # of seaice categories : prep=",I2," nml=",I2)') int_in_file, THIS%GLTPARAM%nt
     CALL ABOR1_SFX(YMESS)
  END IF
  !
  !*       1.     (Semi-)prognostic fields with only space dimension(s) :
  !
  CALL READ_SURF(HPROGRAM,'ICEUSTAR',THIS%TGLT%ust(:,1),IRESP)
  !
  !*       2.     Prognostic fields with space and ice-category dimension(s) :
  !
  DO JK=1, THIS%GLTPARAM%nt
     WRITE(YLVL,'(I2)') JK
     YCATEG='_'//ADJUSTL(YLVL)
     ! .. Read sea ice age for type JK
     CALL READ_SURF(HPROGRAM,'ICEAGE'//YCATEG,THIS%TGLT%sit(JK,:,1)%age,IRESP)
     ! .. Read melt pond volume for type JK
     CALL READ_SURF(HPROGRAM,'ICEVMP'//YCATEG,THIS%TGLT%sit(JK,:,1)%vmp,IRESP)
     ! .. Read sea ice surface albedo for type JK
     CALL READ_SURF(HPROGRAM,'ICEASN'//YCATEG,THIS%TGLT%sit(JK,:,1)%asn,IRESP)
     ! .. Read sea ice fraction for type JK
     CALL READ_SURF(HPROGRAM,'ICEFSI'//YCATEG, THIS%TGLT%sit(JK,:,1)%fsi,IRESP)
     ! .. Read sea ice thickness for type JK
     CALL READ_SURF(HPROGRAM,'ICEHSI'//YCATEG, THIS%TGLT%sit(JK,:,1)%hsi,IRESP)
     ! .. Read sea ice salinity for type JK
     CALL READ_SURF(HPROGRAM,'ICESSI'//YCATEG, THIS%TGLT%sit(JK,:,1)%ssi,IRESP)
     ! .. Read sea ice surface temperature for type JK
     CALL READ_SURF(HPROGRAM,'ICETSF'//YCATEG, THIS%TGLT%sit(JK,:,1)%tsf,IRESP)
     ! .. Read snow thickness for type JK
     CALL READ_SURF(HPROGRAM,'ICEHSN'//YCATEG, THIS%TGLT%sit(JK,:,1)%hsn,IRESP)
     ! .. Read snow density for type JK
     CALL READ_SURF(HPROGRAM,'ICERSN'//YCATEG, THIS%TGLT%sit(JK,:,1)%rsn,IRESP)
     !
     !*       3.     Prognostic fields with space, ice-category and layer dimensions :
     !
     DO JL=1, THIS%GLTPARAM%nl
        WRITE(YLVL,'(I2)') JL
        YLEVEL=YCATEG(1:LEN_TRIM(YCATEG))//'_'//ADJUSTL(YLVL)
        ! .. Read sea ice vertical gltools_enthalpy profile for type JK and level JL
        CALL READ_SURF(HPROGRAM,'ICEH'//YLEVEL, THIS%TGLT%sil(JL,JK,:,1)%ent,IRESP)
     END DO
  END DO
  !
  !    4.  Compute ice class existence boolean from ice fractions:
  !
  WHERE ( THIS%TGLT%sit(:,:,1)%fsi<epsil1 )
     THIS%TGLT%sit(:,:,1)%esi = .FALSE.
  ELSEWHERE
     THIS%TGLT%sit(:,:,1)%esi = .TRUE.
  ENDWHERE
  !
  !    4.1 Run original Gelato checks on values read in restart
  !
  ! .. Detect negative ice concentrations
  !
  DO JX=1, THIS%GLTPARAM%nx
     DO JL=1, THIS%GLTPARAM%nt
        IF ( THIS%TGLT%sit(JL,JX,1)%fsi<0. ) THEN
           WRITE(KLUOUT,*)  &
                '**** WARNING **** Correcting problem in ice conc. < 0 at i=',  &
                1,' j=',JX,' k=',JL
           THIS%TGLT%sit(JL,JX,1)%fsi = 0.
        ENDIF
     END DO
     !
     zfsit = SUM( THIS%TGLT%sit(:,JX,1)%fsi )
     !
     ! .. Detect total concentrations that exceed unity
     !
     IF ( zfsit>1. ) THEN
        WRITE(KLUOUT,*)  &
             '**** WARNING **** Correcting problem in total ice conc. >1 at i=',  &
             1,' j=',JX,' fsi=',zfsit
        THIS%TGLT%sit(:,JX,1)%fsi = THIS%TGLT%sit(:,JX,1)%fsi / zfsit
     ENDIF
     !
     ! .. Detect non zero concentrations but zero thickness (no consequence)
     !
     WHERE( THIS%TGLT%sit(:,JX,1)%fsi>epsil1 .AND. THIS%TGLT%sit(:,JX,1)%hsi<epsil1)
        THIS%TGLT%sit(:,JX,1)%fsi=0.
        THIS%TGLT%sit(:,JX,1)%hsi=0.
        THIS%TGLT%sit(:,JX,1)%hsn=0.
     ENDWHERE
     !
  END DO

  !    5. Initalize Gelato domain parameters
  !
  !    All points of Surfex 1D grid in seaflux are sea points
  !
  THIS%TGLT%dom(:,1)%tmk=1
  THIS%TGLT%dom(:,1)%imk=1
  !
  !    Masks for U- and V- grid point are not used
  !
  THIS%TGLT%dom(:,1)%umk=1
  THIS%TGLT%dom(:,1)%vmk=1
  !
  !    lat,lon,srf are inherited from seaflux grid
  !
  THIS%TGLT%dom(:,1)%lon=G%XLON(:)*XPI/180.
  THIS%TGLT%dom(:,1)%lat=G%XLAT(:)*XPI/180.
  !
  !    Except in Gelato dynamics, mesh lengths are used only to compute mesh area
  !    Hence, a simple setting can be used
  !
  THIS%TGLT%dom(:,1)%dxc=G%XMESH_SIZE(:)**0.5
  THIS%TGLT%dom(:,1)%dyc=THIS%TGLT%dom(:,1)%dxc
  THIS%TGLT%dom(:,1)%srf=G%XMESH_SIZE(:)
  !
  !    Surface of local and global ocean domain (ghost points are masked out)
  !
  THIS%GLTPARAM%xdomsrf = SUM( THIS%TGLT%dom(:,1)%srf, MASK=(THIS%TGLT%dom(:,1)%tmk==1) )
  THIS%GLTPARAM%xdomsrf_g = THIS%GLTPARAM%xdomsrf
#if ! defined in_arpege
  CALL mpp_sum(THIS%GLTPARAM%xdomsrf_g)
#else
  ! Avoid zero divide in Gelato computation of global area averages
  THIS%GLTPARAM%xdomsrf_g = MAX(THIS%GLTPARAM%xdomsrf_g, 1.e-9)
#endif
  !
  !    7. Initalize Gelato time parameters
  !
  THIS%TGLT%ind%beg=1
  !
  !   Dummy high value for end time. Implies only that Gelato won't output
  !   its own format of run-long averaged diagnostics (which are useless
  !   in Surfex diags logic)
  !
  THIS%TGLT%ind%end=50000000
  !
  !   8. Initalize Gelato bathymetry - change sign w.r.t Surfex
  !
  THIS%TGLT%bat(:,1) = -THIS%XSEABATHY
  !
  !! Initialize the coupling variables with 'snapshot' prognostic variables
  ! (for now, averaged over ice categories)
  !
  CALL GLT_SNDATMF( THIS%TGLT, THIS%GLTPARAM%nnflxin, THIS%GLTPARAM%alblc, XTTSI - XTT )
  THIS%XSIC(:)     = THIS%TGLT%ice_atm(1,:,1)%fsi
  THIS%XTICE(:)    = THIS%TGLT%ice_atm(1,:,1)%tsf
  THIS%XICE_ALB(:) = THIS%TGLT%ice_atm(1,:,1)%alb
  !
  ! Must init ocean mixed layer temp with sensible value for getting correct diag for time step=0
  THIS%TGLT%oce_all(:,1)%tml = THIS%XSST(:)
  !
  IF (LHOOK) CALL DR_HOOK('ICE_GELATO:READSURF',1,ZHOOK_HANDLE)
  !
  !-------------------------------------------------------------------------------
END SUBROUTINE READSURF

SUBROUTINE WRITESURF(THIS, HSELECT, HPROGRAM)
USE MODI_WRITE_SURF
  IMPLICIT NONE
  CLASS(GELATO_t) :: THIS
  CHARACTER(LEN=*), INTENT(IN) :: HSELECT(:)
  CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM

  INTEGER           :: IRESP           ! Error code after reading
  !
  CHARACTER(LEN=5)  :: YLVL
  !
  CHARACTER(LEN=6)  :: YICECAT
  CHARACTER(LEN=20) :: YFORM
  CHARACTER(LEN=12) :: YCATEG           ! Category to write
  CHARACTER(LEN=12) :: YLEVEL           ! Level to write
  CHARACTER(LEN=100):: YCOMMENT         ! Error Message
  !
  INTEGER :: JK,JL                   ! loop counter on ice categories and layes
  !
  REAL(KIND=JPRB) :: ZHOOK_HANDLE
  !
  !-----------------------------------------------------------------------------
  IF (LHOOK) CALL DR_HOOK('ICE_GELATO:WRITE_SURF',0,ZHOOK_HANDLE)

  YCOMMENT='Number of sea-ice layers'
  CALL WRITE_SURF(HSELECT,HPROGRAM,'ICENL',THIS%GLTPARAM%nl,IRESP,YCOMMENT)
  YCOMMENT='Number of ice categories'
  CALL WRITE_SURF(HSELECT,HPROGRAM,'ICENT',THIS%GLTPARAM%nt,IRESP,YCOMMENT)
  !
  !*       1.     Prognostic fields with only space dimension(s) :
  !
  YCOMMENT='ICEUSTAR ()'
  CALL WRITE_SURF(HSELECT,HPROGRAM,'ICEUSTAR',THIS%TGLT%ust(:,1),IRESP,YCOMMENT)
  !
  !*       2.     Prognostic fields with space and ice-category dimension(s) :
  !
  DO JK=1,THIS%GLTPARAM%nt
    WRITE(YICECAT,'(I2)') JK
    YCATEG='_'//ADJUSTL(YICECAT)
    ! .. Write sea ice age for type JK
    YCOMMENT='X_Y_ICEAGE'//YCATEG//' (s)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,'ICEAGE'//YCATEG,THIS%TGLT%sit(JK,:,1)%age,IRESP,YCOMMENT)
    ! .. Write melt pond volume for type JK
    YCOMMENT='X_Y_ICEVMP'//YCATEG//' (m3)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,'ICEVMP'//YCATEG,THIS%TGLT%sit(JK,:,1)%vmp,IRESP,YCOMMENT)
    ! .. Write sea ice surface albedo for type JK
    YCOMMENT='X_Y_ICEASN'//YCATEG//' ([0-1])'
    CALL WRITE_SURF(HSELECT,HPROGRAM,'ICEASN'//YCATEG,THIS%TGLT%sit(JK,:,1)%asn,IRESP,YCOMMENT)
    ! .. Write sea ice fraction for type JK
    YCOMMENT='X_Y_ICEFSI'//YCATEG//' ([0-1])'
    CALL WRITE_SURF(HSELECT,HPROGRAM,'ICEFSI'//YCATEG, THIS%TGLT%sit(JK,:,1)%fsi,IRESP,YCOMMENT)
    ! .. Write sea ice thickness for type JK
    YCOMMENT='X_Y_ICEHSI'//YCATEG//' (m)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,'ICEHSI'//YCATEG, THIS%TGLT%sit(JK,:,1)%hsi,IRESP,YCOMMENT)
    ! .. Write sea ice salinity for type JK
    YCOMMENT='X_Y_ICESSI'//YCATEG//' (psu)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,'ICESSI'//YCATEG, THIS%TGLT%sit(JK,:,1)%ssi,IRESP,YCOMMENT)
    ! .. Write sea ice surface temperature for type JK
    YCOMMENT='X_Y_ICETSF'//YCATEG//' (K)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,'ICETSF'//YCATEG, THIS%TGLT%sit(JK,:,1)%tsf,IRESP,YCOMMENT)
    ! .. Write snow thickness for type JK
    YCOMMENT='X_Y_ICEHSN'//YCATEG//' (m)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,'ICEHSN'//YCATEG, THIS%TGLT%sit(JK,:,1)%hsn,IRESP,YCOMMENT)
    ! .. Write snow density for type JK
    YCOMMENT='X_Y_ICERSN'//YCATEG//' (kg m-3)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,'ICERSN'//YCATEG, THIS%TGLT%sit(JK,:,1)%rsn,IRESP,YCOMMENT)
    !
    !*       3.     Prognostic fields with space and ice-category and layer dimension(s) :
    !
    DO JL=1,THIS%GLTPARAM%NL
      WRITE(YLVL,'(I2)') JL
      YLEVEL = YCATEG(1:LEN_TRIM(YCATEG))//'_'//ADJUSTL(YLVL)
      YFORM='(A6,I1.1,A4)'
      IF (JL >= 10)  YFORM='(A6,I2.2,A4)'
      WRITE(YCOMMENT,FMT=YFORM) 'X_Y_ICEH',JL,' (J/kg)'
      ! .. Write sea ice vertical gltools_enthalpy profile for type JK and level JL
      CALL WRITE_SURF(HSELECT, &
        HPROGRAM,'ICEH'//YLEVEL, THIS%TGLT%sil(JL,JK,:,1)%ent,IRESP,YCOMMENT)
    END DO

  END DO

  IF (LHOOK) CALL DR_HOOK('ICE_GELATO:WRITE_SURF',1,ZHOOK_HANDLE)
END SUBROUTINE WRITESURF

SUBROUTINE WRITE_DIAG(THIS, HSELECT, HPROGRAM)
IMPLICIT NONE
  CLASS(GELATO_t) :: THIS
  CHARACTER(LEN=*), INTENT(IN) :: HSELECT(:)
  CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM

  RETURN
END SUBROUTINE WRITE_DIAG

SUBROUTINE GET_RESPONSE(THIS, PSIC, PTICE, PICE_ALB)
IMPLICIT NONE
  CLASS(GELATO_t) :: THIS
  REAL, INTENT(OUT) :: PSIC(:)
  REAL, INTENT(OUT) :: PTICE(:)
  REAL, INTENT(OUT) :: PICE_ALB(:)
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('ICE_GELATO:GET_RESPONSE', 0, ZHOOK_HANDLE)

  PSIC(:) = THIS%XSIC(:)
  PTICE(:) = THIS%XTICE(:)
  PICE_ALB(:) = THIS%XICE_ALB(:)

  IF (LHOOK) CALL DR_HOOK('ICE_GELATO:GET_RESPONSE', 1, ZHOOK_HANDLE)
END SUBROUTINE GET_RESPONSE

SUBROUTINE DIAG_MISC(THIS, DGMSI)
USE MODD_DIAG_MISC_SEAICE_n, ONLY: DIAG_MISC_SEAICE_t
USE MODE_GLT_STATS , ONLY: GLT_AVHICEM, GLT_AVHSNWM
IMPLICIT NONE
  CLASS(GELATO_t) :: THIS
  TYPE(DIAG_MISC_SEAICE_t), INTENT(IN OUT) :: DGMSI
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('ICE_GELATO:DIAG_MISC', 0, ZHOOK_HANDLE)

  DGMSI%XSIT  = RESHAPE(glt_avhicem(THIS%TGLT%dom,THIS%TGLT%sit,THIS%GLTPARAM%nt, &
                        THIS%GLTPARAM%nx,THIS%GLTPARAM%ny),[THIS%GLTPARAM%NX])
  DGMSI%XSND  = RESHAPE(glt_avhsnwm(THIS%TGLT%dom,THIS%TGLT%sit,THIS%GLTPARAM%nt, &
                        THIS%GLTPARAM%nx,THIS%GLTPARAM%ny), [THIS%GLTPARAM%NX])
  DGMSI%XMLT  = THIS%TGLT%oce_all(:,1)%tml

  IF (LHOOK) CALL DR_HOOK('ICE_GELATO:DIAG_MISC', 1, ZHOOK_HANDLE)
END SUBROUTINE DIAG_MISC

SUBROUTINE SET_DAMPING(THIS, PSIC_EFOLDING_TIME, PSIT_EFOLDING_TIME)
IMPLICIT NONE
  CLASS(GELATO_t) :: THIS
  REAL, INTENT(IN) :: PSIC_EFOLDING_TIME
  REAL, INTENT(IN) :: PSIT_EFOLDING_TIME

  IF(THIS%LINTERPOL_SIC)THEN
    IF (PSIC_EFOLDING_TIME==0.0) THEN
      THIS%GLTPARAM%CFSIDMP='PRESCRIBE'
    ELSE
      THIS%GLTPARAM%CFSIDMP='DAMP'
      THIS%GLTPARAM%XFSIDMPEFT=PSIC_EFOLDING_TIME
    ENDIF
  ENDIF
  !
  IF(THIS%LINTERPOL_SIT)THEN
    IF (PSIT_EFOLDING_TIME==0.0) THEN
      THIS%GLTPARAM%CHSIDMP='PRESCRIBE'
    ELSE
      THIS%GLTPARAM%CHSIDMP='DAMP_FAC'
      THIS%GLTPARAM%XHSIDMPEFT= PSIT_EFOLDING_TIME
    ENDIF
  ENDIF
END SUBROUTINE SET_DAMPING
END MODULE ICE_GELATO
