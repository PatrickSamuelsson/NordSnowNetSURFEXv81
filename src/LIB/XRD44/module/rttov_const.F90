!
MODULE RTTOV_CONST
  ! Description:
  ! Definition of all parameters (constants) for RTTOV
  !
  ! Copyright:
  !    This software was developed within the context of
  !    the EUMETSAT Satellite Application Facility on
  !    Numerical Weather Prediction (NWP SAF), under the
  !    Cooperation Agreement dated 25 November 1998, between
  !    EUMETSAT and the Met Office, UK, by one or more partners
  !    within the NWP SAF. The partners in the NWP SAF are
  !    the Met Office, ECMWF, KNMI and MeteoFrance.
  !
  !    Copyright 2002, EUMETSAT, All Rights Reserved.
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  !  1.0   01/12/2002  New F90 code with structures (P Brunel A Smith)
  !  1.1   29/01/2003  New platforms and instruments (P Brunel)
  !                    Hard limits for input profiles
  !  1.2   19/02/2003  Some changes to limits and comments (R Saunders)
  !  1.3   06/05/2003  Change version number to 7.3.1
  !                    and add references for physical constants (P Brunel)
  !  1.4      08/2003  Added variables for MW scattering (F Chevallier)
  !  1.5   18/09/2003  Added coefficients for cloud absorption properties (P Francis)
  !  1.6   15/10/2003  Added new sections in parameter files for scatt   (F Chevallier)
  !  1.7   23/11/2003  Added new definitions of polarisations 2.1 (S English)
  !  1.8   25/08/2005  Made inst_name a parameter (R Saunders)
  !  1.9   11/01/2006  Added logical flag for surface humidity use (R Saunders)
  !  1.10  12/01/2006  Marco Matricardi (ECMWF):
  !           --       Added variables for CO2,CO,N2O and CH4 molecules.
  !           --       Added parameters for the computation of the refractive index
  !           --       of air.
  !  1.11  06/02/2006  Added logical flag for linear in tau approx (R Saunders)
  !  1.12  06/04/2006  Added Meghatropiques (R. Saunders)
  !  1.13  14/03/2007  Added units conversion constants
  !  1.14  16/05/2007  Added polarimetric sensor type (R Saunders)
  !  1.15  25/09/2007  Added maximum number of warnings for checkinput (P Brunel)
  !  1.16  11/10/2007  Remove zhusta* and zice* constants ( P.Marguinaud )
  !  1.17  07/12/2007  Remove maximum number of warnings for checkinput (P Brunel)
  !  1.18  12/12/2007  Added hard limits for trace gases (R Saunders)
  !  1.19  13/12/2007  Renamed linear_tau (R Saunders)
  !  1.20  01/11/2007  Added parameters for section length and AD/K code (A. Geer)
  !  1.21  16/01/2008  Facility to apply regression limits  (N. Bormann)
  !  1.22  04/03/2008  Made min hard limit > zero (R Saunders)
  !  1.23  14/04/2008  Added SSM/T2 (R Saunders)
  !  1.24  02/06/2008  Changed mixing ratio for CO (R Saunders)
  !  1.25  12/08/2008  Added SSMISZ for SSMIS chan19-22 - Zeeman (P. Rayer)
  !  1.26  29/01/2009  Add Kalpana and FY-3 (R Saunders)
  !  1.27  26/05/2009  Add more platforms and sensors (R Saunders)
  !  1.28  02/12/2009  Add principal component capability (Marco matricardi)
  !  1.29  15/01/2010  Add rttov9 intervals constants (P Marguinaud)
  !  1.30  05/07/2010  Add maximum solar zenith angle constant (J Hocking)
  !  1.31  01/02/2011  Updates to platform and sensor lists (J Hocking)
  !  1.32  09/11/2012  Add theta_eff for Lambertian refl (R SAunders)
  !  1.33  07/05/2013  Add ice crystal diameter and ice water content hard limits (J Vidot)
  !  1.34  28/04/2015  Add GMI sensor, backphasing from RRTTOV 11.2 (P Chambon)
  !
  ! 2010/03 Code cleaning Pascal Brunel, Philippe Marguinaud
  !

  USE PARKIND1, ONLY : JPIM     ,JPRB
  IMPLICIT NONE

  !1.0 Precision and numerical constants
  ! Try to ensure this is large enough to avoid overflows in reciprocals but small enough to not affect the calculations.
  ! these parameters are defined at bottom of module (because they use 
  REAL(JPRB), PARAMETER :: MAX_EXP_EXPONENT = 50._JPRB ! approx 1e22 which should be sufficiently big for most purposes
  REAL(JPRB), PARAMETER :: MIN_EXPONENT     = 1E-16_JPRB ! approx log_10(1+2^-52) - anything raised to this power or smaller 
                                                         ! should be approx equal to 1
! small_val is defined in rttov_transmit to avoid compiler incompatibility
!   ! small_val is used in rttov_transmit to ensure small values do not result in underflows. In subsequent calculations
!   ! these values are multiplied together hence the exponent of 1/3.
!   Real(jprb)            :: small_val = (tiny(min_exponent)) ** (0.333333_jprb) ! XLF doesn't like 1/3

  !1.1 general
  !-----------
  ! Version number of the current code

  INTEGER(KIND=JPIM), PARAMETER :: VERSION = 11
  INTEGER(KIND=JPIM), PARAMETER :: RELEASE = 2
  INTEGER(KIND=JPIM), PARAMETER :: MINOR_VERSION = 0

  INTEGER(KIND=JPIM), PARAMETER :: VERSION_COMPATIBLE_MIN = 10 ! minimum version number
  INTEGER(KIND=JPIM), PARAMETER :: VERSION_COMPATIBLE_MAX = 11 ! maximum version number
          ! compatible for coefficients.
          ! coef files with "id_comp_lvl" outside range will be rejected

  CHARACTER (LEN=16), PARAMETER :: RTTOV_MAGIC_STRING = '%RTTOV_COEFF    '
  REAL(KIND=JPRB),    PARAMETER :: RTTOV_MAGIC_NUMBER = 1.2345E+12_JPRB

  INTEGER(KIND=JPIM), PARAMETER :: DEFAULT_ERR_UNIT = 0  ! standard error unit number
                              ! standard error unit number is 7 for HPUX
  
  INTEGER(KIND=JPIM), PARAMETER :: MAX_FASTEM_VERSION = 6  ! Highest FASTEM version number available

  !1.2 physical constants
  !----------------------
  ! Molecular weights  (g/mole) are calculated by adding NIST Standard Atomic Weights
  ! Molecular weight of dry air refers to US standard atmosphere 1976
  ! NIST  Standard Atomic Weight are:
  ! H    1.00794   (7)
  ! C   12.0107    (8)
  ! N   14.0067    (2)
  ! O   15.9994    (3)
  REAL(KIND=JPRB), PARAMETER :: MAIR = 28.9644_JPRB
  REAL(KIND=JPRB), PARAMETER :: MH2O = 18.01528_JPRB
  REAL(KIND=JPRB), PARAMETER :: MO3  = 47.9982_JPRB
  REAL(KIND=JPRB), PARAMETER :: MCO2 = 44.0095_JPRB
  REAL(KIND=JPRB), PARAMETER :: MCH4 = 16.04246_JPRB
  REAL(KIND=JPRB), PARAMETER :: MN2O = 44.0128_JPRB
  REAL(KIND=JPRB), PARAMETER :: MCO  = 28.0101_JPRB

  ! Avogadro constant from NIST (mol-1)
  REAL(KIND=JPRB), PARAMETER :: NA = 6.02214129E23_JPRB
  
  ! Gravity from NIST 9.80665 ms-1 (exact)
  REAL(KIND=JPRB), PARAMETER :: GRAVITY = 9.80665_JPRB

  ! Fundamental constants taken from http://physics.nist.gov/cuu/index.html
  ! Barry N. Taylor (Fundamental Constants Data Center of NIST) and Peter J.
  ! Mohr (Atomic Physics Division of NIST). Values as of 01/12/2010.
  ! c1 = 2hc**2; c2 = hc/k; units are consistent with those used within RTTOV.
  ! NB Planck fn constant values (c1, c2) in core code are still taken from coef files.
  !    Newer values are available from NIST: we will update in a future version.
  Real(KIND=JPRB), Parameter :: c1 = .00001191042722_jprb   ! * mW/(m2 sr cm-4)
  Real(KIND=JPRB), Parameter :: c2 = 1.4387752_jprb         ! cm K
  Real(KIND=JPRB), Parameter :: speedl = 29979245800.0_jprb ! Speed of light cm s-1

  !
  ! Kaye & Laby latest library edition is 16e 1995, and gives
  ! * standard value  g = 9.80665 ms-1 exactly (p.191)
  ! * earth mean radius r= 6371.00 km (p191)
  !    [defined as [(r_equator)^2 (r_pole)]^1/3]
  REAL(KIND=JPRB), PARAMETER :: PI      = 3.1415926535_JPRB
  REAL(KIND=JPRB), PARAMETER :: DEG2RAD = PI/180.0_JPRB
  REAL(KIND=JPRB), PARAMETER :: EARTHRADIUS = 6371.00_JPRB
  REAL(KIND=JPRB), PARAMETER :: FLATT       = 3.3528107E-3_JPRB
  REAL(KIND=JPRB), PARAMETER :: OMEGA       = 7292115E-11_JPRB
  REAL(KIND=JPRB), PARAMETER :: EQRAD       = 6378.137_JPRB
  REAL(KIND=JPRB), PARAMETER :: GRAVE       = 9.7803267715_JPRB
  REAL(KIND=JPRB), PARAMETER :: Z4PI_R      = 0.0795774715_JPRB
  REAL(KIND=JPRB), PARAMETER :: PI_R        = 0.3183098862_JPRB
  REAL(KIND=JPRB), PARAMETER :: THETA_EFF   = 55.0_JPRB
  REAL(KIND=JPRB), PARAMETER :: SEC_THETA_EFF   = 1.743446796_JPRB

  ! The Cosmic Microwave Background Spectrum from the Full COBE FIRAS Data Set
  ! Fixsen D.J. et all
  ! Astrophysical Journal v.473, p.576 December 1996
  ! CMBR = 2.728 +- 0.004K
  REAL(KIND=JPRB), PARAMETER :: TCOSMIC     = 2.728_JPRB
  !  Real(Kind=jprb), Parameter :: tcosmic     = 0.1_JPRB !used for ECMWF tests

  ! Universal gas constant R = 8.314510 J/mol/K
  REAL(KIND=JPRB), PARAMETER :: RGP = 8.314510_JPRB
  REAL(KIND=JPRB), PARAMETER :: RGC = 8.314472_JPRB

  ! mean molar mass of dry air rm = 0.0289644 kg.mol^-1
  REAL(KIND=JPRB), PARAMETER :: RM = 0.0289644_JPRB

  ! units conversion from  mixing ratio to ppmv
  REAL(KIND=JPRB), PARAMETER :: Q_MIXRATIO_TO_PPMV  = 1.60771704E+6_JPRB
  REAL(KIND=JPRB), PARAMETER :: O3_MIXRATIO_TO_PPMV = 6.03504E+5_JPRB
  REAL(KIND=JPRB), PARAMETER :: CO2_MIXRATIO_TO_PPMV= 6.58114E+5_JPRB
  REAL(KIND=JPRB), PARAMETER :: CO_MIXRATIO_TO_PPMV = 1.0340699E+6_JPRB
  REAL(KIND=JPRB), PARAMETER :: N2O_MIXRATIO_TO_PPMV= 6.58090E+5_JPRB
  REAL(KIND=JPRB), PARAMETER :: CH4_MIXRATIO_TO_PPMV= 1.80548E+6_JPRB

  ! zero temperature(K)
  REAL(KIND=JPRB), PARAMETER :: T0 = 273.15_JPRB
  ! standard pressure
  REAL(KIND=JPRB), PARAMETER :: P0 = 1013.25_JPRB

  !1.3 satellite and instrument information
  !----------------------------------------

  !platform id codes
  INTEGER(KIND=JPIM), PARAMETER :: NPLATFORMS = 41
  INTEGER(KIND=JPIM), PARAMETER :: &
       & PLATFORM_ID_NOAA      = 1, &
       & PLATFORM_ID_DMSP      = 2, &
       & PLATFORM_ID_METEOSAT  = 3, &
       & PLATFORM_ID_GOES      = 4, &
       & PLATFORM_ID_GMS       = 5, &
       & PLATFORM_ID_FY2       = 6, &
       & PLATFORM_ID_TRMM      = 7, &
       & PLATFORM_ID_ERS       = 8, &
       & PLATFORM_ID_EOS       = 9, &
       & PLATFORM_ID_METOP     = 10, &
       & PLATFORM_ID_ENVISAT   = 11, &
       & PLATFORM_ID_MSG       = 12, &
       & PLATFORM_ID_FY1       = 13, &
       & PLATFORM_ID_ADEOS     = 14, &
       & PLATFORM_ID_MTSAT     = 15, &
       & PLATFORM_ID_CORIOLIS  = 16, &
       & PLATFORM_ID_JPSS      = 17, &
       & PLATFORM_ID_GIFTS     = 18, &
       & PLATFORM_ID_SENTINEL3 = 19, &
       & PLATFORM_ID_MEGHATR   = 20, &
       & PLATFORM_ID_KALPANA   = 21, &
       & PLATFORM_ID_INSAT_3D  = 22, & ! This is spare - use ID 40 below for INSAT3
       & platform_id_fy3       = 23, &
       & platform_id_coms      = 24, &
       & platform_id_meteorm   = 25, &
       & platform_id_gosat     = 26, &
       & platform_id_calipso   = 27, &
       & platform_id_dummy     = 28, &
       & platform_id_gcomw     = 29, &
       & platform_id_nimbus    = 30, &
       & platform_id_himawari  = 31, &
       & platform_id_mtg       = 32, &
       & platform_id_saral     = 33, &
       & platform_id_metopsg   = 34, &
       & platform_id_landsat   = 35, &
       & platform_id_jason     = 36, &
       & platform_id_gpm       = 37, &
       & platform_id_insat1    = 38, &
       & platform_id_insat2    = 39, &
       & platform_id_insat3    = 40, &
       & platform_id_ground    = 41    ! For ground-based sensors

  !platform names
  CHARACTER (LEN=9), PARAMETER :: PLATFORM_NAME(NPLATFORMS) = &
       & (/ 'noaa     ', 'dmsp     ', 'meteosat ', 'goes     ', 'gms      ', &
          & 'fy2      ', 'trmm     ', 'ers      ', 'eos      ', 'metop    ', &
          & 'envisat  ', 'msg      ', 'fy1      ', 'adeos    ', 'mtsat    ', &
          & 'coriolis ', 'jpss     ', 'gifts    ', 'sentinel3', 'meghatr  ', &
          & 'kalpana  ', 'insat_3d ', 'fy3      ', 'coms     ', 'meteor-m ', &
          & 'gosat    ', 'calipso  ', 'dummy    ', 'gcom-w   ', 'nimbus   ', &
          & 'himawari ', 'mtg      ', 'saral    ', 'metopsg  ', 'landsat  ', &
          & 'jason    ', 'gpm      ', 'insat1   ', 'insat2   ', 'insat3   ', &
          & 'ground   ' /)

  !instrument id codes
  INTEGER(KIND=JPIM), PARAMETER :: &
       & INST_ID_HIRS   =  0, INST_ID_MSU    =  1, INST_ID_SSU    =  2, INST_ID_AMSUA   =  3, &
       & INST_ID_AMSUB  =  4, INST_ID_AVHRR  =  5, INST_ID_SSMI   =  6, INST_ID_VTPR1   =  7, &
       & INST_ID_VTPR2  =  8, INST_ID_TMI    =  9, INST_ID_SSMIS  = 10, INST_ID_AIRS    = 11, &
       & INST_ID_HSB    = 12, INST_ID_MODIS  = 13, INST_ID_ATSR   = 14, INST_ID_MHS     = 15, &
       & INST_ID_IASI   = 16, INST_ID_AMSRE  = 17, INST_ID_GMSIM  = 18, INST_ID_ATMS    = 19, &
       & INST_ID_MVIRI  = 20, INST_ID_SEVIRI = 21, INST_ID_GOESIM = 22, INST_ID_GOESSD  = 23, &
       & INST_ID_MTSATIM= 24, INST_ID_VISSR  = 25, INST_ID_MVISR  = 26, INST_ID_CRIS    = 27, &
       & INST_ID_CMIS   = 28, INST_ID_VIIRS  = 29, INST_ID_WINDSAT= 30, INST_ID_GIFTS   = 31, &
       & INST_ID_SSMT1  = 32, INST_ID_SSMT2  = 33, INST_ID_SAPHIR = 34, INST_ID_MADRAS  = 35, &
       & INST_ID_SSMISZ = 36, INST_ID_VHRR   = 37, INST_ID_INSATIM= 38, INST_ID_INSATSD = 39, &
       & INST_ID_MWTS   = 40, INST_ID_MWHS   = 41, INST_ID_IRAS   = 42, INST_ID_MWRI    = 43, &
       & INST_ID_ABI    = 44, INST_ID_MI     = 45, INST_ID_MSUMR  = 46, INST_ID_TANSOFTS= 47, &
       & INST_ID_IIR    = 48, INST_ID_MWR    = 49, INST_ID_DUMMYIR= 50, INST_ID_DUMMYMW = 51, &
       & INST_ID_DUMMYHI= 52, INST_ID_DUMMYPO= 53, INST_ID_SCAMS  = 54, INST_ID_SMMR    = 55, &
       & INST_ID_AHI    = 56, INST_ID_IRS    = 57, INST_ID_ALTIKA = 58, INST_ID_IASING  = 59, &
       & INST_ID_TM     = 60, INST_ID_FCI    = 61, INST_ID_AMSR1  = 62, INST_ID_AMSR2   = 63, &
       & INST_ID_VISSR2 = 64, INST_ID_SLSTR  = 65, INST_ID_TIRS   = 66, INST_ID_AMR     = 67, &
       & INST_ID_OLI    = 68, INST_ID_IRIS   = 69, INST_ID_ICI    = 70, INST_ID_GMI     = 71, &
       & INST_ID_MWTS2  = 72, INST_ID_MWHS2  = 73, INST_ID_ASTER  = 74, INST_ID_HATPRO  = 75

  INTEGER(KIND=JPIM), PARAMETER :: NINST = 76
  ! List of instruments  !!!! HIRS is number 0
  CHARACTER (LEN=8), DIMENSION(0:NINST-1),PARAMETER :: INST_NAME =       &
        & (/ 'hirs    ', 'msu     ', 'ssu     ', 'amsua   ', 'amsub   ',  &
           & 'avhrr   ', 'ssmi    ', 'vtpr1   ', 'vtpr2   ', 'tmi     ',  &
           & 'ssmis   ', 'airs    ', 'hsb     ', 'modis   ', 'atsr    ',  &
           & 'mhs     ', 'iasi    ', 'amsre   ', 'imager  ', 'atms    ',  &
           & 'mviri   ', 'seviri  ', 'imager  ', 'sounder ', 'imager  ',  &
           & 'vissr   ', 'mvisr   ', 'cris    ', 'cmis    ', 'viirs   ',  &
           & 'windsat ', 'gifts   ', 'ssmt1   ', 'ssmt2   ', 'saphir  ',  &
           & 'madras  ', 'ssmisz  ', 'vhrr    ', 'imager  ', 'sounder ',  &
           & 'mwts    ', 'mwhs    ', 'iras    ', 'mwri    ', 'abi     ',  &
           & 'mi      ', 'msumr   ', 'tansofts', 'iir     ', 'mwr     ',  &
           & 'dummyir ', 'dummymw ', 'dummyhi ', 'dummypo ', 'scams   ',  &
           & 'smmr    ', 'ahi     ', 'irs     ', 'altika  ', 'iasing  ',  &
           & 'tm      ', 'fci     ', 'amsr    ', 'amsr2   ', 'vissr   ',  &
           & 'slstr   ', 'tirs    ', 'amr     ', 'oli     ', 'iris    ',  &
           & 'ici     ', 'gmi     ', 'mwts2   ', 'mwhs2   ', 'aster   ',  &
           & 'hatpro  ' /)


  !1.4 Coefficient file Section names
  !----------------------------------
  INTEGER(KIND=JPIM), PARAMETER :: NSECTIONS = 43
  INTEGER(KIND=JPIM), PARAMETER :: LENSECTION = 34
  CHARACTER(LEN=LENSECTION), PARAMETER :: SECTION_TYPES(NSECTIONS) = &
    & (/ 'IDENTIFICATION                    ', 'LINE-BY-LINE                      ', &
       & 'FAST_MODEL_VARIABLES              ', 'FILTER_FUNCTIONS                  ', &
       & 'FUNDAMENTAL_CONSTANTS             ', 'SSIREM                            ', &
       & 'FASTEM                            ', 'REFERENCE_PROFILE                 ', &
       & 'PROFILE_LIMITS                    ', 'FAST_COEFFICIENTS                 ', &
       & 'COEF_SUB_FILES                    ', 'GAZ_UNITS                         ', &
       & 'DIMENSIONS                        ', 'FREQUENCIES                       ', &
       & 'HYDROMETEOR                       ', 'CONVERSIONS                       ', &
       & 'EXTINCTION                        ', 'ALBEDO                            ', &
       & 'ASYMMETRY                         ', 'GAS_SPECTRAL_INTERVAL             ', &
       & 'TRANSMITTANCE_TRESHOLD            ', 'SOLAR_SPECTRUM                    ', &
       & 'WATER_OPTICAL_CONSTANT            ', 'WAVE_SPECTRUM                     ', &
       & 'AEROSOLS_PARAMETERS               ', 'AEROSOLS_COMPONENTS               ', &
       & 'WATERCLOUD_TYPES                  ', 'WATERCLOUD_PARAMETERS             ', &
       & 'ICECLOUD_TYPES                    ', 'HEXAGONAL_PARAMETERS              ', &
       & 'AGGREGATE_PARAMETERS              ', 'PRINCOMP_PREDICTORS               ', &
       & 'PRINCOMP_EIGENVECTORS             ', 'PRINCOMP_COEFFICIENTS             ', &
       & 'EMISSIVITY_COEFFICIENTS           ', 'PC_REFERENCE_PROFILE              ', &
       & 'PC_PROFILE_LIMITS                 ', 'INSTRUMENT_NOISE                  ', &
       & 'PLANCK_WEIGHTED                   ', 'SOLAR_FAST_COEFFICIENTS           ', &
       & 'README_SPECTRAL_RESPONSE_FUNCTION ', 'NLTE_RADIANCE_COEFS               ', &
       & 'PRESSURE_MODULATED_CELL           '/)

  !sensors id codes
  INTEGER(KIND=JPIM), PARAMETER :: NSENSORS = 4
  INTEGER(KIND=JPIM), PARAMETER :: &
       & SENSOR_ID_IR     = 1, &
       & SENSOR_ID_MW     = 2, &
       & SENSOR_ID_HI     = 3, &
       & SENSOR_ID_PO     = 4

  !sensors names
  CHARACTER (LEN=2), PARAMETER :: SENSOR_NAME(NSENSORS) = &
       & (/ 'ir', 'mw', 'hi', 'po' /)

  ! these codes are for the instrument from the inst_name array
  INTEGER(KIND=JPIM), PARAMETER :: SENSOR_ID(0:NINST-1) = (/ &
    SENSOR_ID_IR, SENSOR_ID_MW, SENSOR_ID_IR, SENSOR_ID_MW, SENSOR_ID_MW,  &
    SENSOR_ID_IR, SENSOR_ID_MW, SENSOR_ID_IR, SENSOR_ID_IR, SENSOR_ID_MW,  &
    SENSOR_ID_MW, SENSOR_ID_HI, SENSOR_ID_MW, SENSOR_ID_IR, SENSOR_ID_IR,  &
    SENSOR_ID_MW, SENSOR_ID_HI, SENSOR_ID_MW, SENSOR_ID_IR, SENSOR_ID_MW,  &
    SENSOR_ID_IR, SENSOR_ID_IR, SENSOR_ID_IR, SENSOR_ID_IR, SENSOR_ID_IR,  &
    SENSOR_ID_IR, SENSOR_ID_IR, SENSOR_ID_HI, SENSOR_ID_MW, SENSOR_ID_IR,  &
    SENSOR_ID_PO, SENSOR_ID_HI, SENSOR_ID_MW, SENSOR_ID_MW, SENSOR_ID_MW,  &
    SENSOR_ID_MW, SENSOR_ID_MW, SENSOR_ID_IR, SENSOR_ID_IR, SENSOR_ID_IR,  &
    SENSOR_ID_MW, SENSOR_ID_MW, SENSOR_ID_IR, SENSOR_ID_MW, SENSOR_ID_IR,  &
    SENSOR_ID_IR, SENSOR_ID_IR, SENSOR_ID_HI, SENSOR_ID_IR, SENSOR_ID_MW,  &
    SENSOR_ID_IR, SENSOR_ID_MW, SENSOR_ID_HI, SENSOR_ID_PO, SENSOR_ID_MW,  &
    SENSOR_ID_MW, SENSOR_ID_IR, SENSOR_ID_HI, SENSOR_ID_MW, SENSOR_ID_HI,  &
    SENSOR_ID_IR, SENSOR_ID_IR, SENSOR_ID_MW, SENSOR_ID_MW, SENSOR_ID_IR,  &
    SENSOR_ID_IR, SENSOR_ID_IR, SENSOR_ID_MW, SENSOR_ID_IR, SENSOR_ID_HI,  &
    SENSOR_ID_MW, SENSOR_ID_MW, SENSOR_ID_MW, SENSOR_ID_MW, SENSOR_ID_IR,  &
    SENSOR_ID_MW /)

  !gas id codes
  INTEGER(KIND=JPIM), PARAMETER :: NGASES_MAX = 8
  INTEGER(KIND=JPIM), PARAMETER :: &
        & GAS_ID_MIXED       = 1, &
        & GAS_ID_WATERVAPOUR = 2, &
        & GAS_ID_OZONE       = 3, &
        & GAS_ID_WVCONT      = 4, &
        & GAS_ID_CO2         = 5, &
        & GAS_ID_N2O         = 6, &
        & GAS_ID_CO          = 7, &
        & GAS_ID_CH4         = 8

  !gas names
  CHARACTER (LEN=12), PARAMETER :: GAS_NAME(NGASES_MAX) = &
        & (/ 'Mixed_gases ', &
           & 'Water_vapour', &
           & 'Ozone       ', &
           & 'WV_Continuum', &
           & 'CO2         ', &
           & 'N2O         ', &
           & 'CO          ', &
           & 'CH4         ' /)

  !gas units
  INTEGER(KIND=JPIM), PARAMETER :: NGASES_UNIT = 2
  INTEGER(KIND=JPIM), PARAMETER :: &
        & GAS_UNIT_SPECCONC  = 1, &
        & GAS_UNIT_PPMV      = 2
  CHARACTER (LEN=12), PARAMETER :: GAS_UNIT_NAME(NGASES_UNIT) = &
        & (/ 'spec. concen', &
           & 'ppmv        '  /)


  !1.5 error reporting
  !-------------------
  !error status values
  INTEGER(KIND=JPIM), PARAMETER :: ERRORSTATUS_SUCCESS = 0
  INTEGER(KIND=JPIM), PARAMETER :: ERRORSTATUS_FATAL   = 1


  !1.6 surface types
  !-----------------
  INTEGER(KIND=JPIM), PARAMETER :: NSURFTYPE = 2
  INTEGER(KIND=JPIM), PARAMETER :: SURFTYPE_LAND = 0
  INTEGER(KIND=JPIM), PARAMETER :: SURFTYPE_SEA = 1
  INTEGER(KIND=JPIM), PARAMETER :: SURFTYPE_SEAICE = 2

  !1.7 water types
  !---------------
  INTEGER(KIND=JPIM), PARAMETER :: NWATERTYPE = 1
  INTEGER(KIND=JPIM), PARAMETER :: WATERTYPE_FRESH_WATER = 0
  INTEGER(KIND=JPIM), PARAMETER :: WATERTYPE_OCEAN_WATER = 1

  !1.8 ice cloud parameterisations
  !-------------------------------
  INTEGER(KIND=JPIM), PARAMETER :: NISH = 4 ! Max valid value for ish
  INTEGER(KIND=JPIM), PARAMETER :: NIDG = 4 ! Max valid value for idg

  !
  !1.9 Hard limits for control of input profile
  !--------------------------------------------
  ! Temperature
  REAL(KIND=JPRB), PARAMETER :: TMAX   = 400.0_JPRB       ! degK
  REAL(KIND=JPRB), PARAMETER :: TMIN   = 90.0_JPRB        ! degK
  ! Water Vapour
  REAL(KIND=JPRB), PARAMETER :: QMAX   = 0.60E+06_JPRB    ! ppmv 0.373_JPRB kg/kg
  REAL(KIND=JPRB), PARAMETER :: QMIN   = 0.1E-10_JPRB     ! ppmv
  ! Ozone
  REAL(KIND=JPRB), PARAMETER :: O3MAX  = 1000.0_JPRB      ! ppmv  1.657E-3_JPRB kg/kg
  REAL(KIND=JPRB), PARAMETER :: O3MIN  = 0.1E-10_JPRB     ! ppmv
  ! CO2
  REAL(KIND=JPRB), PARAMETER :: CO2MAX = 1000.0_JPRB      ! ppmv
  REAL(KIND=JPRB), PARAMETER :: CO2MIN = 0.1E-10_JPRB     ! ppmv
  ! CO
  REAL(KIND=JPRB), PARAMETER :: COMAX  = 10.0_JPRB        ! ppmv
  REAL(KIND=JPRB), PARAMETER :: COMIN  = 0.1E-10_JPRB     ! ppmv
  ! N2O
  REAL(KIND=JPRB), PARAMETER :: N2OMAX = 10.0_JPRB        ! ppmv
  REAL(KIND=JPRB), PARAMETER :: N2OMIN = 0.1E-10_JPRB     ! ppmv
  ! CH4
  REAL(KIND=JPRB), PARAMETER :: CH4MAX = 50.0_JPRB        ! ppmv
  REAL(KIND=JPRB), PARAMETER :: CH4MIN = 0.1E-10_JPRB     ! ppmv
  ! Cloud Liquid Water
  REAL(KIND=JPRB), PARAMETER :: CLWMAX = 1.0_JPRB         ! kg/kg
  REAL(KIND=JPRB), PARAMETER :: CLWMIN = 0.0_JPRB         ! kg/kg
  ! Surface Pressure
  REAL(KIND=JPRB), PARAMETER :: PMAX   = 1200.0_JPRB      ! surface pressure hPa
  REAL(KIND=JPRB), PARAMETER :: PMIN   = 400.0_JPRB       ! hPa
  ! Surface Wind
  REAL(KIND=JPRB), PARAMETER :: WMAX   =  100.0_JPRB      ! surface wind speed (m/s)
  ! Zenith Angle
  REAL(KIND=JPRB), PARAMETER :: ZENMAX = 75.0_JPRB        ! zenith angle (Deg) = secant 3.86_JPRB
  REAL(KIND=JPRB), PARAMETER :: ZENMAXV9 = 84.0_JPRB      ! larger zenmax for v9 predictors
  ! Cloud Top Pressure
  REAL(KIND=JPRB), PARAMETER :: CTPMAX = 1100.0_JPRB      ! (hPa)
  REAL(KIND=JPRB), PARAMETER :: CTPMIN =   50.0_JPRB      ! (hPa)
  ! Magnetic field strength
  REAL(KIND=JPRB), PARAMETER :: BEMAX = 0.7_JPRB          ! (Gauss)
  REAL(KIND=JPRB), PARAMETER :: BEMIN = 0.2_JPRB          ! (Guass)
  ! Ice Crystal Diameter
  REAL(KIND=JPRB), PARAMETER :: DGMIN_HEX =  12.2_JPRB    ! (micron)
  REAL(KIND=JPRB), PARAMETER :: DGMAX_HEX =  118.29_JPRB  ! (micron)
  REAL(KIND=JPRB), PARAMETER :: DGMIN_AGG =  5.61_JPRB    ! (micron)
  REAL(KIND=JPRB), PARAMETER :: DGMAX_AGG =  166.46_JPRB  ! (micron)  
  ! Ice Water Content
  REAL(KIND=JPRB), PARAMETER :: IWCMIN_HEX =  0.000608_JPRB ! (g.m-3)
  REAL(KIND=JPRB), PARAMETER :: IWCMAX_HEX =  0.254639_JPRB ! (g.m-3)
  REAL(KIND=JPRB), PARAMETER :: IWCMIN_AGG =  0.000235_JPRB ! (g.m-3)
  REAL(KIND=JPRB), PARAMETER :: IWCMAX_AGG =  0.489046_JPRB ! (g.m-3)  


  !1.10  Maximum Optical Depth
  !--------------------------
  ! maximum value of optical depth for transmittance calculation
  ! e(-30) -> 10**-14
  ! e(-50) -> 10**-22
  REAL(KIND=JPRB), PARAMETER  :: MAX_OPTICAL_DEPTH = 50._JPRB

  !1.11  Maximum solar zenith angle for which to apply solar calculation
  !---------------------------------------------------------------------
  REAL(KIND=JPRB), PARAMETER  :: MAX_SOL_ZEN = 84._JPRB
  
  
  !2 RTTOV7 aux parameters
  !-------------------------
  INTEGER(KIND=JPIM), PARAMETER :: FASTEM_SP = 5  ! max. number of fastem surface parameters
  REAL(KIND=JPRB), PARAMETER    :: MWCLDTP = 322.0_JPRB  ! Upper pressure level (HPa) for lwp calcs
  REAL(KIND=JPRB), PARAMETER    :: PRESSURE_TOP = 0.004985_JPRB ! Pressure of top level for
                                                ! Line/Line calculations (hPa)
  REAL(KIND=JPRB) , DIMENSION(8), PARAMETER :: DCOEFF =        &! Debye coefs
        & (/ 17.1252_JPRB, 134.2450_JPRB, 310.2125_JPRB,  5.667_JPRB,   &
          & 188.7979_JPRB,  80.5419_JPRB,   0.1157_JPRB,  4.8417_JPRB/)

  !2.1 Polarisation definitions
  !----------------------------
  ! == pol_id +1
  !   1 average of vertical and horizontal
  !   2 nominal vertical at nadir, rotating
  !      with view angle
  !   3 nominal horizontal at nadir, rotating
  !      with view angle
  !   4 vertical
  !   5 horizontal
  !   6 + 45 minus -45 (3rd stokes vector)
  !   7 left circular - right circular (4th stokes vector)
  INTEGER(KIND=JPIM), DIMENSION(7), PARAMETER :: NPOLAR_COMPUTE = &
   & (/ 2, 2, 2, 1, 1, 2, 4/)
  INTEGER(KIND=JPIM), DIMENSION(7), PARAMETER :: NPOLAR_RETURN = &
   & (/ 1, 1, 1, 1, 1, 2, 4/)

  ! pol_v and pol_h give proportion of v and h pol to use in emissivity calculation
  ! pol_s3 adds the 3rd/4th stokes vectors
  REAL(KIND=JPRB), PARAMETER :: POL_V(3,7) = RESHAPE( &
    & (/ 0.5_JPRB, 0.0_JPRB, 0.0_JPRB, &
       & 0.0_JPRB, 0.0_JPRB, 1.0_JPRB, &
       & 0.0_JPRB, 1.0_JPRB, 0.0_JPRB, &
       & 1.0_JPRB, 0.0_JPRB, 0.0_JPRB, &
       & 0.0_JPRB, 0.0_JPRB, 0.0_JPRB, &
       & 0.0_JPRB, 0.0_JPRB, 0.0_JPRB, &
       & 0.0_JPRB, 0.0_JPRB, 0.0_JPRB  /), (/3,7/) )
  REAL(KIND=JPRB), PARAMETER :: POL_H(3,7) = RESHAPE( &
    & (/ 0.5_JPRB, 0.0_JPRB, 0.0_JPRB, &
       & 0.0_JPRB, 1.0_JPRB, 0.0_JPRB, &
       & 0.0_JPRB, 0.0_JPRB, 1.0_JPRB, &
       & 0.0_JPRB, 0.0_JPRB, 0.0_JPRB, &
       & 1.0_JPRB, 0.0_JPRB, 0.0_JPRB, &
       & 0.0_JPRB, 0.0_JPRB, 0.0_JPRB, &
       & 0.0_JPRB, 0.0_JPRB, 0.0_JPRB  /), (/3,7/) )
  REAL(KIND=JPRB), PARAMETER :: POL_S3(0:1,7) = RESHAPE( &
    & (/ 0.0_JPRB, 0.0_JPRB, &
       & 0.0_JPRB, 0.0_JPRB, &
       & 0.0_JPRB, 0.0_JPRB, &
       & 0.0_JPRB, 0.0_JPRB, &
       & 0.0_JPRB, 0.0_JPRB, &
       & 1.0_JPRB, 0.0_JPRB, &
       & 0.0_JPRB, 1.0_JPRB  /), (/2,7/) )

  !3 RTTOVSCATT aux parameters
  !---------------------------
  ! Minimum cloud cover processed by rttov_scatt
  REAL(KIND=JPRB), PARAMETER :: CCTHRES = 0.05_JPRB
  ! Minimum single scattering albedo processed by rttov_scatt
  REAL(KIND=JPRB), PARAMETER :: MIN_SSA = 1.0E-03_JPRB
  ! Rain density (g.cm-3)
  REAL(KIND=JPRB), PARAMETER :: RHO_RAIN = 1.0_JPRB
  ! Snow density (g.cm-3)
  REAL(KIND=JPRB), PARAMETER :: RHO_SNOW = 0.1_JPRB

  ! Flags to identify function in shared K/Adjoint routines
  INTEGER(KIND=JPIM), PARAMETER :: ADK_ADJOINT = 0
  INTEGER(KIND=JPIM), PARAMETER :: ADK_K       = 1

  !4 Parameters to compute refractive index of air
  !--------------------------------------------------------
  REAL(KIND=JPRB), PARAMETER :: D1   =8341.87_JPRB
  REAL(KIND=JPRB), PARAMETER :: D2   =2405955.0_JPRB
  REAL(KIND=JPRB), PARAMETER :: D3   =130.0_JPRB
  REAL(KIND=JPRB), PARAMETER :: D4   =15996.0_JPRB
  REAL(KIND=JPRB), PARAMETER :: D5   =38.9_JPRB
  REAL(KIND=JPRB), PARAMETER :: DCO2 =0.540_JPRB
  REAL(KIND=JPRB), PARAMETER :: ED1  =96095.43_JPRB
  REAL(KIND=JPRB), PARAMETER :: ED2  =0.601_JPRB
  REAL(KIND=JPRB), PARAMETER :: ED3  =0.00972_JPRB
  REAL(KIND=JPRB), PARAMETER :: ED4  =0.003661_JPRB
  REAL(KIND=JPRB), PARAMETER :: EW1  =3.7345_JPRB
  REAL(KIND=JPRB), PARAMETER :: EW2  =0.0401_JPRB
  REAL(KIND=JPRB), PARAMETER :: HTOP =100.0_JPRB
  REAL(KIND=JPRB), PARAMETER :: CTOM =1.0E-4_JPRB
  REAL(KIND=JPRB), PARAMETER :: WAVER=1700.0_JPRB

  !5 RTTOV8_M_SCATT
  !--------------------------------------------------------
  INTEGER(KIND=JPIM), PARAMETER :: NAER_MAX = 13

  INTEGER(KIND=JPIM), PARAMETER :: &
        & AER_ID_INSO       = 1, &
        & AER_ID_WASO       = 2, &
        & AER_ID_SOOT       = 3, &
        & AER_ID_SSAM       = 4, &
        & AER_ID_SSCM       = 5, &
        & AER_ID_MINM       = 6, &
        & AER_ID_MIAM       = 7, &
        & AER_ID_MICM       = 8, &
        & AER_ID_MITR       = 9, &
        & AER_ID_SUSO       =10, &
        & AER_ID_VOLA       =11, &
        & AER_ID_VAPO       =12, &
        & AER_ID_ASDU       =13

  CHARACTER (LEN=4), PARAMETER :: AER_NAME(NAER_MAX) = &
        & (/ 'inso', &
           & 'waso', &
           & 'soot', &
           & 'ssam', &
           & 'sscm', &
           & 'minm', &
           & 'miam', &
           & 'micm', &
           & 'mitr', &
           & 'suso', &
           & 'vola', &
           & 'vapo', &
           & 'asdu'  /)

  INTEGER(KIND=JPIM), PARAMETER :: NPHANGLE = 208

  REAL(KIND=JPRB), PARAMETER :: PHANGLE(NPHANGLE) = &
             (/   0.0_JPRB,   0.1_JPRB,   0.2_JPRB,   0.3_JPRB,   0.4_JPRB,   0.5_JPRB,   0.6_JPRB, &
              &   0.7_JPRB,   0.8_JPRB,   0.9_JPRB,   1.0_JPRB,   1.1_JPRB,   1.2_JPRB,   1.3_JPRB, &
              &   1.4_JPRB,   1.5_JPRB,   1.6_JPRB,   1.7_JPRB,   1.8_JPRB,   1.9_JPRB,   2.0_JPRB, &
              &   2.1_JPRB,   2.2_JPRB,   2.3_JPRB,   2.4_JPRB,   2.5_JPRB,   2.6_JPRB,   2.7_JPRB, &
              &   2.8_JPRB,   2.9_JPRB,   3.0_JPRB,   4.0_JPRB,   5.0_JPRB,   6.0_JPRB,   7.0_JPRB, &
              &   8.0_JPRB,   9.0_JPRB,  10.0_JPRB,  11.0_JPRB,  12.0_JPRB,  13.0_JPRB,  14.0_JPRB, &
              &  15.0_JPRB,  16.0_JPRB,  17.0_JPRB,  18.0_JPRB,  19.0_JPRB,  20.0_JPRB,  21.0_JPRB, &
              &  22.0_JPRB,  23.0_JPRB,  24.0_JPRB,  25.0_JPRB,  26.0_JPRB,  27.0_JPRB,  28.0_JPRB, &
              &  29.0_JPRB,  30.0_JPRB,  31.0_JPRB,  32.0_JPRB,  33.0_JPRB,  34.0_JPRB,  35.0_JPRB, &
              &  36.0_JPRB,  37.0_JPRB,  38.0_JPRB,  39.0_JPRB,  40.0_JPRB,  41.0_JPRB,  42.0_JPRB, &
              &  43.0_JPRB,  44.0_JPRB,  45.0_JPRB,  46.0_JPRB,  47.0_JPRB,  48.0_JPRB,  49.0_JPRB, &
              &  50.0_JPRB,  51.0_JPRB,  52.0_JPRB,  53.0_JPRB,  54.0_JPRB,  55.0_JPRB,  56.0_JPRB, &
              &  57.0_JPRB,  58.0_JPRB,  59.0_JPRB,  60.0_JPRB,  61.0_JPRB,  62.0_JPRB,  63.0_JPRB, &
              &  64.0_JPRB,  65.0_JPRB,  66.0_JPRB,  67.0_JPRB,  68.0_JPRB,  69.0_JPRB,  70.0_JPRB, &
              &  71.0_JPRB,  72.0_JPRB,  73.0_JPRB,  74.0_JPRB,  75.0_JPRB,  76.0_JPRB,  77.0_JPRB, &
              &  78.0_JPRB,  79.0_JPRB,  80.0_JPRB,  81.0_JPRB,  82.0_JPRB,  83.0_JPRB,  84.0_JPRB, &
              &  85.0_JPRB,  86.0_JPRB,  87.0_JPRB,  88.0_JPRB,  89.0_JPRB,  90.0_JPRB,  91.0_JPRB, &
              &  92.0_JPRB,  93.0_JPRB,  94.0_JPRB,  95.0_JPRB,  96.0_JPRB,  97.0_JPRB,  98.0_JPRB, &
              &  99.0_JPRB, 100.0_JPRB, 101.0_JPRB, 102.0_JPRB, 103.0_JPRB, 104.0_JPRB, 105.0_JPRB, &
              & 106.0_JPRB, 107.0_JPRB, 108.0_JPRB, 109.0_JPRB, 110.0_JPRB, 111.0_JPRB, 112.0_JPRB, &
              & 113.0_JPRB, 114.0_JPRB, 115.0_JPRB, 116.0_JPRB, 117.0_JPRB, 118.0_JPRB, 119.0_JPRB, &
              & 120.0_JPRB, 121.0_JPRB, 122.0_JPRB, 123.0_JPRB, 124.0_JPRB, 125.0_JPRB, 126.0_JPRB, &
              & 127.0_JPRB, 128.0_JPRB, 129.0_JPRB, 130.0_JPRB, 131.0_JPRB, 132.0_JPRB, 133.0_JPRB, &
              & 134.0_JPRB, 135.0_JPRB, 136.0_JPRB, 137.0_JPRB, 138.0_JPRB, 139.0_JPRB, 140.0_JPRB, &
              & 141.0_JPRB, 142.0_JPRB, 143.0_JPRB, 144.0_JPRB, 145.0_JPRB, 146.0_JPRB, 147.0_JPRB, &
              & 148.0_JPRB, 149.0_JPRB, 150.0_JPRB, 151.0_JPRB, 152.0_JPRB, 153.0_JPRB, 154.0_JPRB, &
              & 155.0_JPRB, 156.0_JPRB, 157.0_JPRB, 158.0_JPRB, 159.0_JPRB, 160.0_JPRB, 161.0_JPRB, &
              & 162.0_JPRB, 163.0_JPRB, 164.0_JPRB, 165.0_JPRB, 166.0_JPRB, 167.0_JPRB, 168.0_JPRB, &
              & 169.0_JPRB, 170.0_JPRB, 171.0_JPRB, 172.0_JPRB, 173.0_JPRB, 174.0_JPRB, 175.0_JPRB, &
              & 176.0_JPRB, 177.0_JPRB, 178.0_JPRB, 179.0_JPRB, 180.0_JPRB /)

  INTEGER(KIND=JPIM), PARAMETER :: NWCL_MAX = 5

  INTEGER(KIND=JPIM), PARAMETER :: &
        & WCL_ID_STCO       = 1, &
        & WCL_ID_STMA       = 2, &
        & WCL_ID_CUCC       = 3, &
        & WCL_ID_CUCP       = 4, &
        & WCL_ID_CUMA       = 5

  CHARACTER (LEN=4), PARAMETER :: WCL_NAME(NWCL_MAX) = &
        & (/ 'stco', &
           & 'stma', &
           & 'cucc', &
           & 'cucp', &
           & 'cuma' /)

  INTEGER(KIND=JPIM), PARAMETER:: NCLDTYP = 6

  REAL(KIND=JPRB), PARAMETER :: E00       = 611.21_JPRB
  REAL(KIND=JPRB), PARAMETER :: T00       = 273.16_JPRB
  REAL(KIND=JPRB), PARAMETER :: TI        = T00 - 23.0_JPRB

  REAL(KIND=JPRB), PARAMETER :: MIN_TAU = 1.0E-8_JPRB
  REAL(KIND=JPRB), PARAMETER :: MIN_OD  = 1.0E-5_JPRB

!
! These are the RTTOV9 wavenumbers that make intervals
!
  REAL(KIND=JPRB), PARAMETER :: RTTOV9_WV0690_50 =  690.50_JPRB, &
                                RTTOV9_WV1050_00 = 1050.00_JPRB, &
                                RTTOV9_WV1095_25 = 1095.25_JPRB, &
                                RTTOV9_WV1100_25 = 1100.25_JPRB, &
                                RTTOV9_WV1350_25 = 1350.25_JPRB, &
                                RTTOV9_WV1750_25 = 1750.25_JPRB, &
                                RTTOV9_WV1900_25 = 1900.25_JPRB, &
                                RTTOV9_WV1995_00 = 1995.00_JPRB, &
                                RTTOV9_WV2000_00 = 2000.00_JPRB, &
                                RTTOV9_WV2250_00 = 2250.00_JPRB, &
                                RTTOV9_WV2295_25 = 2295.25_JPRB, &
                                RTTOV9_WV2360_00 = 2360.00_JPRB, &
                                RTTOV9_WV2380_25 = 2380.25_JPRB, &
                                RTTOV9_WV2660_25 = 2660.25_JPRB, &
                                RTTOV9_WV2760_25 = 2760.25_JPRB

!
! Parameters for solar overcast radiance calculation
!

REAL(KIND=JPRB), PARAMETER :: OVERCAST_ALBEDO_WVN = 10000._JPRB ! Wavenumber (cm-1) at which albedo changes
REAL(KIND=JPRB), PARAMETER :: OVERCAST_ALBEDO1    = 0.7_JPRB    ! Overcast albedo for wvn > limit
REAL(KIND=JPRB), PARAMETER :: OVERCAST_ALBEDO2    = 0.6_JPRB    ! Overcast albedo for wvn <= limit

!
! Parameters for Rayleigh cross-section parameterization taken from Bucholzt 1995.
!
REAL(KIND=JPRB), PARAMETER :: RAY_MIN_WVN = 5000.0_JPRB,     & ! Min wavenumber (cm-1) for which Rayleigh is calculated
                              ray_scs_wlm = 0.5_JPRB,        & ! Wavelength limit: below 0.5um the
                              ray_scs_a1 = 3.01577E-28_JPRB, & !   first set of parameters a1-d1 are used
                              ray_scs_b1 = -3.55212_JPRB,    & !   while above this a2-d2 are used.
                              ray_scs_c1 = -1.35579_JPRB,    &
                              ray_scs_d1 = -0.11563_JPRB,    &
                              ray_scs_a2 = 4.01061E-28_JPRB, &
                              ray_scs_b2 = -3.99668_JPRB,    &
                              ray_scs_c2 = -1.10298E-3_JPRB, &
                              ray_scs_d2 = -2.71393E-2_JPRB

!
! Interpolation modes
!       MODE                     USER->COEF LEVEL          COEF->USER LEVEL
!                            (profile interpolation)   (optical depth/weighting fn interpolation)
! interp_rochon:                    Rochon                 Rochon,     op dep
! interp_loglinear:                 Log-linear             Log-linear, op dep
! interp_rochon_loglinear:          Rochon                 Log-linear, op dep
! interp_rochon_wfn:                Rochon                 Rochon,     weighting fns
! interp_rochon_loglinear_wfn:      Rochon                 Log-linear, weighting fns
INTEGER(KIND=JPIM), PARAMETER :: NINTERP_MODES               = 5_JPIM ! Number of valid interpolation options
INTEGER(KIND=JPIM), PARAMETER :: INTERP_ROCHON               = 1_JPIM
INTEGER(KIND=JPIM), PARAMETER :: INTERP_LOGLINEAR            = 2_JPIM
INTEGER(KIND=JPIM), PARAMETER :: INTERP_ROCHON_LOGLINEAR     = 3_JPIM
INTEGER(KIND=JPIM), PARAMETER :: INTERP_ROCHON_WFN           = 4_JPIM
INTEGER(KIND=JPIM), PARAMETER :: INTERP_ROCHON_LOGLINEAR_WFN = 5_JPIM

END MODULE RTTOV_CONST
