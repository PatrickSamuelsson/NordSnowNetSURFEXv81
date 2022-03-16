!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE OI_CONTROL (YSC, &
 & P__SURFTEMPERATURE,       &
 & P__SURFPREC_EAU_CON,      &
 & P__SURFPREC_EAU_GEC,      &
 & P__SURFPREC_NEI_CON,      &
 & P__SURFPREC_NEI_GEC,      &
 & P__ATMONEBUL_BASSE,       &
 & P__SURFXEVAPOTRANSP,      &
 & P__SURFFLU_LAT_MEVA,      &
 & P__SURFACCPLUIE,          &
 & P__SURFACCNEIGE,          &
 & P__SURFACCGRAUPEL,        &
 & P__CLSTEMPERATURE,        &
 & P__CLSHUMI_RELATIVE,      &
 & P__CLSVENT_ZONAL,         &
 & P__CLSVENT_MERIDIEN,      &
 & P__SURFIND_TERREMER,      &
 & P__SURFRESERV_NEIGE,      &
 & P__LON,                   &
 & P__LAT,                   &
 & LD_MASKEXT)

! ------------------------------------------------------------------------------------------
!  *****************************************************************************************
!
!  Program to perform within SURFEX 
!  a soil analysis for water content and temperature 
!  using the Meteo-France optimum interpolation technique of Giard and Bazile (2000)
!
!  Derived from CANARI subroutines externalized by Lora Taseva (Dec. 2007)
!
!  Author : Jean-Francois Mahfouf (01/2008)
!
!  Modifications : 
!   (05/2008)  : The I/O of this version follow the newly available LFI format in SURFEX  
!   (01/2009)  : Read directly atmospheric FA files using XRD library instead of using "edf"
!   (06/2009)  : Modifications to allow the assimilation of ASCAT superficial soil moisture
!   (09/2010)  : More parameters to goto_surfex
!   (03/2011)  : Initialization of ZEVAPTR (F.Bouyssel)
!   (03/2013)  : Use 10m wind from upperair instead surfex one (F.Taillefer)
!   (10/2017)  : Bugfix to update sea or lake surface temperature in case XNATURE=0 and ITM>0.5 (F.Bouyssel)
!
! ******************************************************************************************
! ------------------------------------------------------------------------------------------
!
!
!
!
USE MODD_SURFEX_n, ONLY : SURFEX_t
USE MODE_MODELN_SURFEX_HANDLER
!
USE MODD_TYPE_DATE_SURF
USE MODD_CSTS,       ONLY : XDAY, XPI, XRHOLW, XLVTT, NDAYSEC
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_ASSIM
USE MODD_OL_FILEID

USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
USE MODN_IO_OFFLINE, ONLY : NAM_IO_OFFLINE, CNAMELIST, CSURF_FILETYPE


USE MODD_IO_SURF_FA, ONLY : CFILEIN_FA, CFILEIN_FA_SAVE, CDNOMC, &
                            NDGUX,  NDLUX,  PERPK,  PELON0, PELAT0, &
                            PEDELX, PEDELY, PELON1, PELAT1, PEBETA
#ifdef SFX_ARO
USE MODD_IO_SURF_ARO,ONLY : NGPTOT, NGPTOT_CAP, NPROMA, NINDX1, NINDX2, NBLOCK, NKPROMA, &
                                                             YSURFEX_CACHE_OUT,          &
                            SURFEX_FIELD_BUF_PREALLOC, SURFEX_FIELD_BUF_SET_RECORD
USE MODD_SURFEX_ARO, ONLY : YSURFEX_ARO_ALL, YSURFEX_ARO_CUR
#endif

USE MODE_POS_SURF,  ONLY : POSNAM

USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_READ_ALL_NAMELISTS
USE MODI_INI_DATA_COVER
USE MODI_INIT_IO_SURF_n
USE MODI_READ_SURF
USE MODI_SET_SURFEX_FILEIN
USE MODI_GET_SIZE_FULL_n
USE MODI_READ_COVER_n
USE MODI_CONVERT_COVER_FRAC
USE MODI_GET_1D_MASK
USE MODI_END_IO_SURF_n
USE MODI_IO_BUFF_CLEAN
USE MODI_OI_BC_SOIL_MOISTURE
USE MODI_OI_CACSTS
USE MODI_FLAG_UPDATE
USE MODI_WRITE_SURF

USE YOMHOOK ,ONLY : LHOOK, DR_HOOK
USE PARKIND1  ,ONLY : JPRB

IMPLICIT NONE
!
TYPE(SURFEX_t), INTENT(INOUT) :: YSC
!
REAL(KIND=JPRB), OPTIONAL, DIMENSION (:) ::  P__SURFTEMPERATURE
REAL(KIND=JPRB), OPTIONAL, DIMENSION (:) ::  P__SURFPREC_EAU_CON
REAL(KIND=JPRB), OPTIONAL, DIMENSION (:) ::  P__SURFPREC_EAU_GEC
REAL(KIND=JPRB), OPTIONAL, DIMENSION (:) ::  P__SURFPREC_NEI_CON
REAL(KIND=JPRB), OPTIONAL, DIMENSION (:) ::  P__SURFPREC_NEI_GEC
REAL(KIND=JPRB), OPTIONAL, DIMENSION (:) ::  P__ATMONEBUL_BASSE
REAL(KIND=JPRB), OPTIONAL, DIMENSION (:) ::  P__SURFXEVAPOTRANSP
REAL(KIND=JPRB), OPTIONAL, DIMENSION (:) ::  P__SURFFLU_LAT_MEVA
REAL(KIND=JPRB), OPTIONAL, DIMENSION (:) ::  P__SURFACCPLUIE
REAL(KIND=JPRB), OPTIONAL, DIMENSION (:) ::  P__SURFACCNEIGE
REAL(KIND=JPRB), OPTIONAL, DIMENSION (:) ::  P__SURFACCGRAUPEL
REAL(KIND=JPRB), OPTIONAL, DIMENSION (:) ::  P__CLSTEMPERATURE
REAL(KIND=JPRB), OPTIONAL, DIMENSION (:) ::  P__CLSHUMI_RELATIVE
REAL(KIND=JPRB), OPTIONAL, DIMENSION (:) ::  P__CLSVENT_ZONAL
REAL(KIND=JPRB), OPTIONAL, DIMENSION (:) ::  P__CLSVENT_MERIDIEN
REAL(KIND=JPRB), OPTIONAL, DIMENSION (:) ::  P__SURFIND_TERREMER
REAL(KIND=JPRB), OPTIONAL, DIMENSION (:) ::  P__SURFRESERV_NEIGE
REAL(KIND=JPRB), OPTIONAL, DIMENSION (:) ::  P__LON
REAL(KIND=JPRB), OPTIONAL, DIMENSION (:) ::  P__LAT
LOGICAL,         OPTIONAL, DIMENSION (:) ::  LD_MASKEXT

INTEGER :: IGPCOMP
INTEGER :: IDAT

CHARACTER(LEN=28) :: YNAMELIST = 'OPTIONS.nam                 '

!    Declarations of local variables

CHARACTER(LEN=6)            :: YPROGRAM
CHARACTER(LEN=6), PARAMETER :: YPROGRAM2 = 'FA    '
CHARACTER(LEN=2)            :: CMONTH
INTEGER                     :: IYEAR                ! current year (UTC)
INTEGER                     :: IMONTH               ! current month (UTC)
INTEGER                     :: IDAY                 ! current day (UTC)
INTEGER                     :: NSSSSS               ! current time since start of the run (s)
INTEGER                     :: IRESP                ! return code
TYPE (DATE_TIME)            :: TTIME                ! Current date and time
INTEGER                     :: ISIZE
INTEGER                     :: ISIZE1
LOGICAL                     :: LLKEEPEXTZONE

! Arrays for soil OI analysis
REAL, DIMENSION (:,:), ALLOCATABLE :: PWS, PWP, PTS, PTP, PTL, PSNS, PRSMIN, PD2, PLAI, PVEG
REAL, DIMENSION (:),   ALLOCATABLE :: PSST, PSAB, PARG, PLAT, PLON, PTCLS, PHCLS, PUCLS, PVCLS,  &
                                     & PEVAP, PEVAPTR, PT2M_O, PHU2M_O, PTS_O, ZT2INC, ZH2INC,   &
                                     & ZWS, ZWP, ZTL, ZTS, ZTP, ZTCLS, ZHCLS, ZUCLS, ZVCLS,      &
                                     & PSSTC, PWPINC1, PWPINC2, PWPINC3, PT2MBIAS, PH2MBIAS,     &
                                     & PRRCN, PRRCL, PRRSN, PRRSL, PATMNEB, PITM,                &
                                     & PIVEG, PTSC, PTPC, PWSC, PWPC, PSNC, ZEVAP,               &
                                     & ZEVAPTR, PGELAT, PGELAM, PGEMU, ZWSINC, ZWPINC, ZTSINC,   &
                                     & ZTPINC, ZTLINC, ZSNINC, ZSNS, ZPX, ZPY, PSM_O, PSIG_SMO,  &
                                     & PLSM_O, PWS_O, ZWGINC, PLST, PTRD3, ZSST, ZLST, ZALT,     &
                                     & PALBF, PEMISF, PZ0F, PZ0H
REAL, DIMENSION (:),   ALLOCATABLE :: ZSST1, ZLST1, PSST1, PLST1, PLAT1, PLON1, ZALT1

INTEGER                            :: IVERSION, IBUGFIX
INTEGER                            :: JJ,J1
CHARACTER(LEN=10)                  :: YVAR    ! Name of the prognostic variable (in LFI file)
CHARACTER(LEN=100)                 :: YPREFIX ! Prefix of the prognostic variable  (in LFI file)
INTEGER                            :: INOBS   ! number of observations
INTEGER :: ILUNAM
LOGICAL :: GFOUND

REAL                               :: PLAT0,PLON0,PRPK,PLATOR,PLONOR,DELX,DELY,PBETA,ZTHRES
REAL(KIND=JPRB)                    :: Z1S2PI, ZPIS180, ZSCAL, ZRTCLS

INTEGER :: ISIZE_FULL

REAL(KIND=JPRB) :: ZHOOK_HANDLE
! ----------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK ('OI_CONTROL', 0, ZHOOK_HANDLE)

PRINT *,'--------------------------------------------------------------------------'
PRINT *,'|                                                                        |'
PRINT *,'|                             ENTER OI_ASSIM                             |'
PRINT *,'|                                                                        |'
PRINT *,'--------------------------------------------------------------------------'

! Patrick Samuelsson (SMHI)
! Next four lines are commented in MF NWP bf4 for cy43t. Hmhm, avoid these
! comments here since I don't know the reason...
CALL OPEN_NAMELIST('ASCII ',ILUNAM,CNAMELIST)
CALL POSNAM(ILUNAM,'NAM_IO_OFFLINE',GFOUND)
IF (GFOUND) READ (UNIT=ILUNAM,NML=NAM_IO_OFFLINE)
CALL CLOSE_NAMELIST('ASCII ',ILUNAM)

CALL READ_ALL_NAMELISTS(YSC,CSURF_FILETYPE,'ALL',.FALSE.)


YPROGRAM = 'AROME'
#ifdef SFX_ARO
IGPCOMP = MIN (NGPTOT, NGPTOT_CAP)
NBLOCK   = 1
NINDX1   = 1 + (NBLOCK - 1) * NPROMA
NINDX2   = MIN (NBLOCK * NPROMA, IGPCOMP)
NKPROMA  = NINDX2 - NINDX1 + 1
YSURFEX_ARO_CUR => YSURFEX_ARO_ALL(NBLOCK)
#endif

ICURRENT_MODEL = 1


LLKEEPEXTZONE = .FALSE.

Z1S2PI=1.0_JPRB/(2.0_JPRB*XPI)
ZPIS180=XPI/180.0_JPRB

!   Update some constants dependant from NACVEG

!  scaling of soil moisture increments when assimilation window is different from 6 hours
XRSCALDW = REAL(NECHGU)/6.0_JPRB
!  half assimilation window in sec
NITRAD   = NECHGU*1800

CALL INI_DATA_COVER(YPROGRAM, YSC%DTCO, YSC%U)


!   Read grid dimension for allocation

CALL INIT_IO_SURF_n(YSC%DTCO, YSC%U, &
                        YPROGRAM,'FULL  ','SURF  ','READ ')

!   Find current time

CALL READ_SURF(&
               YPROGRAM,'DTCUR',TTIME,IRESP)

!   Time initializations

IYEAR  = TTIME%TDATE%YEAR
IMONTH = TTIME%TDATE%MONTH
IDAY   = TTIME%TDATE%DAY
NSSSSS = TTIME%TIME
IF (NSSSSS > NDAYSEC) NSSSSS = NSSSSS - NDAYSEC

!   Reading grid characteristics to perform nature mask

CALL END_IO_SURF_n(YPROGRAM)
CALL SET_SURFEX_FILEIN(YPROGRAM,'PGD ') ! change input file name to pgd name
CALL INIT_IO_SURF_n(YSC%DTCO, YSC%U, &
                        YPROGRAM,'FULL  ','SURF  ','READ ')

CALL READ_SURF(&
               YPROGRAM,'SEA   ',YSC%U%CSEA   ,IRESP)
CALL READ_SURF(&
               YPROGRAM,'WATER ',YSC%U%CWATER ,IRESP)
CALL READ_SURF(&
               YPROGRAM,'NATURE',YSC%U%CNATURE,IRESP)
CALL READ_SURF(&
               YPROGRAM,'TOWN  ',YSC%U%CTOWN  ,IRESP)

CALL READ_SURF(&
               YPROGRAM,'DIM_FULL  ',YSC%U%NDIM_FULL,  IRESP)
CALL END_IO_SURF_n(YPROGRAM)
CALL INIT_IO_SURF_n(YSC%DTCO, YSC%U, &
                        YPROGRAM,'FULL  ','SURF  ','READ ')

CALL READ_SURF(&
               YPROGRAM,'DIM_SEA   ',YSC%U%NDIM_SEA,   IRESP)
CALL READ_SURF(&
               YPROGRAM,'DIM_NATURE',YSC%U%NDIM_NATURE,IRESP)
CALL READ_SURF(&
               YPROGRAM,'DIM_WATER ',YSC%U%NDIM_WATER, IRESP)
CALL READ_SURF(&
               YPROGRAM,'DIM_TOWN  ',YSC%U%NDIM_TOWN,  IRESP)

!
!   Get total dimension of domain (excluding extension zone)

CALL GET_SIZE_FULL_n(YPROGRAM,YSC%U%NDIM_FULL,YSC%U%NSIZE_FULL,ISIZE_FULL)
YSC%U%NSIZE_FULL = ISIZE_FULL

ISIZE = YSC%U%NSIZE_FULL

ALLOCATE (PSAB(ISIZE)) 
ALLOCATE (PARG(ISIZE))
ALLOCATE (ZALT(ISIZE))

CALL READ_SURF(&
               YPROGRAM,'SAND',      PSAB,  IRESP)
CALL READ_SURF(&
               YPROGRAM,'CLAY',      PARG,  IRESP)
CALL READ_SURF(&
               YPROGRAM,'ZS',        ZALT,  IRESP)

CALL READ_COVER_n(YSC%DTCO, YSC%U, &
                  YPROGRAM)

!   Perform masks (only nature used)

ALLOCATE(YSC%U%XSEA   (ISIZE))
ALLOCATE(YSC%U%XNATURE(ISIZE))
ALLOCATE(YSC%U%XWATER (ISIZE))
ALLOCATE(YSC%U%XTOWN  (ISIZE))

CALL CONVERT_COVER_FRAC(YSC%DTCO, &
                        YSC%U%XCOVER,YSC%U%LCOVER,YSC%U%XSEA,YSC%U%XNATURE,YSC%U%XTOWN,YSC%U%XWATER)

YSC%U%NSIZE_NATURE = COUNT(YSC%U%XNATURE(:) > 0.0)
YSC%U%NSIZE_TOWN   = COUNT(YSC%U%XTOWN(:)   > 0.0)
YSC%U%NSIZE_WATER  = COUNT(YSC%U%XWATER(:)  > 0.0)
YSC%U%NSIZE_SEA    = COUNT(YSC%U%XSEA(:)    > 0.0)

ALLOCATE(YSC%U%NR_NATURE (YSC%U%NSIZE_NATURE))
ALLOCATE(YSC%U%NR_TOWN   (YSC%U%NSIZE_TOWN  ))
ALLOCATE(YSC%U%NR_WATER  (YSC%U%NSIZE_WATER ))
ALLOCATE(YSC%U%NR_SEA    (YSC%U%NSIZE_SEA   ))

CALL GET_1D_MASK( YSC%U%NSIZE_SEA,    ISIZE, YSC%U%XSEA   , YSC%U%NR_SEA   )
CALL GET_1D_MASK( YSC%U%NSIZE_WATER,  ISIZE, YSC%U%XWATER , YSC%U%NR_WATER )
CALL GET_1D_MASK( YSC%U%NSIZE_TOWN,   ISIZE, YSC%U%XTOWN  , YSC%U%NR_TOWN  )
CALL GET_1D_MASK( YSC%U%NSIZE_NATURE, ISIZE, YSC%U%XNATURE, YSC%U%NR_NATURE)

! Allocate arrays

YSC%IM%O%NPATCH = 1

ALLOCATE (PWS(ISIZE,1))
ALLOCATE (PWP(ISIZE,1))
ALLOCATE (PTS(ISIZE,1))
ALLOCATE (PTP(ISIZE,1))
ALLOCATE (PTL(ISIZE,1))
ALLOCATE (PSST(ISIZE))
ALLOCATE (PSNS(ISIZE,1))
ALLOCATE (PLAI(ISIZE,1))
ALLOCATE (PVEG(ISIZE,1))
ALLOCATE (PRSMIN(ISIZE,1))
ALLOCATE (PD2(ISIZE,1))
ALLOCATE (PTCLS(ISIZE))
ALLOCATE (PHCLS(ISIZE))
ALLOCATE (PUCLS(ISIZE))
ALLOCATE (PVCLS(ISIZE))
ALLOCATE (PEVAP(ISIZE))
ALLOCATE (PLST(ISIZE))
ALLOCATE (PTRD3(ISIZE))

ALLOCATE (ZLST(ISIZE))
ALLOCATE (ZSST(ISIZE))

!  Read prognostic variables

CALL END_IO_SURF_n(YPROGRAM)
CALL SET_SURFEX_FILEIN(YPROGRAM,'PREP') ! change input file name to pgd name
CALL INIT_IO_SURF_n(YSC%DTCO, YSC%U, &
                        YPROGRAM,'FULL  ','SURF  ','READ ')

IF (YSC%U%NSIZE_NATURE>0 .AND. YSC%U%CNATURE/='NONE') THEN
  CALL READ_SURF(&
               YPROGRAM,'WG1',       PWS,   IRESP)
  CALL READ_SURF(&
               YPROGRAM,'WG2',       PWP,   IRESP)
  CALL READ_SURF(&
               YPROGRAM,'TG1',       PTS,   IRESP)
  CALL READ_SURF(&
               YPROGRAM,'TG2',       PTP,   IRESP)
  CALL READ_SURF(&
               YPROGRAM,'WGI2',      PTL,   IRESP)

  CALL READ_SURF(&
               YPROGRAM,'VERSION',IVERSION,IRESP)
  CALL READ_SURF(&
               YPROGRAM,'BUG',IBUGFIX,IRESP)
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
    CALL READ_SURF(&
               YPROGRAM,'WSN_VEG1',PSNS,  IRESP)
  ELSE
    CALL READ_SURF(&
               YPROGRAM,'WSNOW_VEG1',PSNS,  IRESP)
  ENDIF
ENDIF

IF (YSC%U%NSIZE_SEA>0 .AND. YSC%U%CSEA/='NONE') THEN
  CALL READ_SURF(&
               YPROGRAM,'SST',       PSST,  IRESP)
ENDIF

IF (YSC%U%NSIZE_WATER>0 .AND. YSC%U%CWATER/='NONE') THEN
  CALL READ_SURF(&
               YPROGRAM,'TS_WATER',  PLST,  IRESP)
ENDIF

IF (YSC%U%NSIZE_TOWN>0 .AND. YSC%U%CTOWN/='NONE' .AND. LAROME) THEN
  CALL READ_SURF(&
               YPROGRAM,'VERSION',IVERSION,IRESP)
  CALL READ_SURF(&
               YPROGRAM,'BUG',IBUGFIX,IRESP)
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
    CALL READ_SURF(&
               YPROGRAM,'TROAD3',   PTRD3,  IRESP)
  ELSE
    CALL READ_SURF(&
               YPROGRAM,'T_ROAD3',   PTRD3,  IRESP)
  ENDIF
ELSE
  PTRD3(:) = XUNDEF
ENDIF

CALL READ_SURF(&
               YPROGRAM,'T2M',       PTCLS, IRESP)
CALL READ_SURF(&
               YPROGRAM,'HU2M',      PHCLS, IRESP)

! Read constant surface fields

CALL READ_SURF(&
               YPROGRAM,'RSMIN',     PRSMIN,IRESP)
CALL READ_SURF(&
               YPROGRAM,'DG2',       PD2,   IRESP)
CALL READ_SURF(&
               YPROGRAM,'LAI',       PLAI,  IRESP)
CALL READ_SURF(&
               YPROGRAM,'VEG',       PVEG,  IRESP)

IF (NPRINTLEV>0) THEN
  JJ = YSC%U%NR_NATURE(1)
  PRINT *,'value in PREP file => WG1       ',PWS(JJ,1)
  PRINT *,'value in PREP file => WG2       ',PWP(JJ,1)
  PRINT *,'value in PREP file => TG1       ',PTS(JJ,1)
  PRINT *,'value in PREP file => TG2       ',PTP(JJ,1)
  PRINT *,'value in PREP file => WGI2      ',PTL(JJ,1)
  PRINT *,'value in PREP file => WSNOW_VEG1',PSNS(JJ,1)
  PRINT *,'value in PREP file => SST       ',PSST(JJ)
  PRINT *,'value in PREP file => LAI       ',PLAI(JJ,1)
  PRINT *,'value in PREP file => VEG       ',PVEG(JJ,1)
  PRINT *,'value in PREP file => RSMIN     ',PRSMIN(JJ,1)
  PRINT *,'value in PREP file => DATA_DG2  ',PD2(JJ,1)
  PRINT *,'value in PREP file => SAND      ',PSAB(JJ)
  PRINT *,'value in PREP file => CLAY      ',PARG(JJ)
  PRINT *,'value in PREP file => ZS        ',ZALT(JJ)
ENDIF

CALL END_IO_SURF_n(YPROGRAM)
CALL IO_BUFF_CLEAN

!  Interface (allocate arrays)

ALLOCATE (PLAT(ISIZE))
ALLOCATE (PLON(ISIZE))
ALLOCATE (ZPX(ISIZE))
ALLOCATE (ZPY(ISIZE))
ALLOCATE (PEVAPTR(ISIZE))
ALLOCATE (ZWP(ISIZE))
ALLOCATE (ZWS(ISIZE))
ALLOCATE (ZTL(ISIZE))
ALLOCATE (ZTS(ISIZE))
ALLOCATE (ZTP(ISIZE))
ALLOCATE (ZSNS(ISIZE))
ALLOCATE (ZTCLS(ISIZE))
ALLOCATE (ZHCLS(ISIZE))
ALLOCATE (ZUCLS(ISIZE))
ALLOCATE (ZVCLS(ISIZE))
ALLOCATE (PSSTC(ISIZE))
ALLOCATE (PWPINC1(ISIZE))
ALLOCATE (PWPINC2(ISIZE))
ALLOCATE (PWPINC3(ISIZE))
ALLOCATE (PT2MBIAS(ISIZE))
ALLOCATE (PH2MBIAS(ISIZE))
ALLOCATE (PRRCN(ISIZE))
ALLOCATE (PRRCL(ISIZE))
ALLOCATE (PRRSN(ISIZE))
ALLOCATE (PRRSL(ISIZE))
ALLOCATE (PATMNEB(ISIZE))
ALLOCATE (PITM(ISIZE))
ALLOCATE (PIVEG(ISIZE))
ALLOCATE (PTSC(ISIZE))
ALLOCATE (PTPC(ISIZE))
ALLOCATE (PWSC(ISIZE))
ALLOCATE (PWPC(ISIZE))
ALLOCATE (PSNC(ISIZE))
ALLOCATE (ZEVAP(ISIZE))
ALLOCATE (ZEVAPTR(ISIZE))
ALLOCATE (PGELAT(ISIZE))
ALLOCATE (PGELAM(ISIZE))
ALLOCATE (PGEMU(ISIZE))
ALLOCATE (PT2M_O(ISIZE))
ALLOCATE (PHU2M_O(ISIZE))
ALLOCATE (PTS_O(ISIZE))
ALLOCATE (PSM_O(ISIZE))
ALLOCATE (PSIG_SMO(ISIZE))
ALLOCATE (PLSM_O(ISIZE))
ALLOCATE (PWS_O(ISIZE))
ALLOCATE (ZWGINC(ISIZE))
ALLOCATE (PALBF(ISIZE))
ALLOCATE (PEMISF(ISIZE))
ALLOCATE (PZ0F(ISIZE))
ALLOCATE (PZ0H(ISIZE))



!  Read model forecast quantities

IF (LAROME) THEN
  PRRSL(:) = P__SURFACCPLUIE   (1:ISIZE)
  PRRSN(:) = P__SURFACCNEIGE   (1:ISIZE)
  PRRCN(:) = P__SURFACCGRAUPEL (1:ISIZE)
  PRRCL(:) = 0.0
!   CALL READ_SURF(YPROGRAM2,'SURFIND.VEG.DOMI',PIVEG  ,IRESP) 
  PIVEG(:) = 0.0
ELSE
  PRRCL(:) = P__SURFPREC_EAU_CON (1:ISIZE)
  PRRSL(:) = P__SURFPREC_EAU_GEC (1:ISIZE)
  PRRCN(:) = P__SURFPREC_NEI_CON (1:ISIZE)
  PRRSN(:) = P__SURFPREC_NEI_GEC (1:ISIZE)
  PIVEG(:) = 0.0
ENDIF
  PATMNEB(:) = P__ATMONEBUL_BASSE  (1:ISIZE)
  PITM(:)    = P__SURFIND_TERREMER (1:ISIZE)
  PEVAP(:)   = P__SURFFLU_LAT_MEVA (1:ISIZE)
IF (.NOT.LALADSURF) THEN
   PEVAPTR(:) = P__SURFXEVAPOTRANSP (1:ISIZE)
ELSE
  PEVAPTR(:) = 0.0
ENDIF
PALBF(:) = XUNDEF
PEMISF(:) = XUNDEF
PZ0F(:) = XUNDEF
PZ0H(:) = XUNDEF


PT2M_O(:)  = P__CLSTEMPERATURE   (1:ISIZE)
PHU2M_O(:) = P__CLSHUMI_RELATIVE (1:ISIZE)
PTS_O(:)   = P__SURFTEMPERATURE  (1:ISIZE)
PUCLS(:)   = P__CLSVENT_ZONAL    (1:ISIZE)
PVCLS(:)   = P__CLSVENT_MERIDIEN (1:ISIZE)


!  Read ASCAT SM observations (in percent)

INOBS = 0
IF (LOBSWG) THEN
  OPEN(UNIT=111,FILE='ASCAT_SM.DAT')
  DO JJ=1,YSC%U%NDIM_FULL
    READ(111,*,END=990) PSM_O(JJ),PSIG_SMO(JJ),PLSM_O(JJ)
    IF (PLSM_O(JJ) < 1.0)          PSM_O(JJ) = 999.0 ! data rejection if not on land
    IF (PSIG_SMO(JJ) > XSIGWGO_MAX) PSM_O(JJ) = 999.0 ! data rejection of error too large
    IF (PSM_O(JJ) /= 999.0) INOBS = INOBS + 1
  ENDDO
990 CONTINUE
  CLOSE(UNIT=111)
  PRINT *,'READ ASCAT SM OK'
ELSE
  PSM_O(:)    = 999.0
  PSIG_SMO(:) = 999.0
  PLSM_O(:)   = 0.0
ENDIF
PRINT *,' NUMBER OF ASCAT OBSERVATIONS AFTER INITIAL CHECKS  :: ',INOBS
INOBS = 0

! Perform bias correction of SM observations

CALL OI_BC_SOIL_MOISTURE(ISIZE,PSM_O,PSAB,PWS_O)

PSNC(:) = P__SURFRESERV_NEIGE (1:ISIZE)


PLAT(:) = P__LAT (1:ISIZE)
PLON(:) = P__LON (1:ISIZE)

!  Allocate arrays to produce analysis increments  

ALLOCATE (ZT2INC(ISIZE))
ALLOCATE (ZH2INC(ISIZE))
ALLOCATE (ZWSINC(ISIZE))
ALLOCATE (ZWPINC(ISIZE))
ALLOCATE (ZTLINC(ISIZE))
ALLOCATE (ZTSINC(ISIZE))
ALLOCATE (ZTPINC(ISIZE))
ALLOCATE (ZSNINC(ISIZE))

! Screen-level innovations

ZT2INC(:) = PT2M_O(:) - PTCLS(:)
ZH2INC(:) = PHU2M_O(:) - PHCLS(:)

! Threshold for background check

ZTHRES=XRTHR_QC*SQRT(XSIGWGO**2 + XSIGWGB**2)

! Superficial soil moisture innovations in (m3/m3)

DO JJ = 1, ISIZE
  IF (PWS_O(JJ) /= 999.0) THEN
    ZWGINC(JJ) = PWS_O(JJ) - PWS(JJ,1)
    IF (ABS(ZWGINC(JJ)) > ZTHRES) THEN 
      ZWGINC(JJ) = 0.0 ! background check
    ELSE
      INOBS = INOBS + 1
    ENDIF
  ELSE
    ZWGINC(JJ) = 0.0
  ENDIF
ENDDO
PRINT *,' NUMBER OF ASCAT OBSERVATIONS AFTER BACKGROUND CHECK  :: ',INOBS

! Interface (define arrays and perform unit conversions)

PARG(:) = PARG(:)*100.0
PSAB(:) = PSAB(:)*100.0

ZWS(:) = XUNDEF
ZWP(:) = XUNDEF
ZTL(:) = XUNDEF

WHERE (YSC%U%XNATURE(:)>0.0_JPRB) 
  ZWS(:)      = PWS(:,1)*XRD1*XRHOLW     ! conversion of m3/m3 -> mm
  ZWP(:)      = PWP(:,1)*PD2(:,1)*XRHOLW  ! conversion of m3/m3 -> mm
  ZTL(:)      = PTL(:,1)*PD2(:,1)*XRHOLW  ! conversion of m3/m3 -> mm
END WHERE

ZTCLS(:)    = PTCLS(:)
ZHCLS(:)    = PHCLS(:)
ZUCLS(:)    = PUCLS(:)
ZVCLS(:)    = PVCLS(:)
PSSTC(:)    = PTS_O(:)
PWPINC1(:)  = XUNDEF
PWPINC2(:)  = XUNDEF
PWPINC3(:)  = XUNDEF
PT2MBIAS(:) = XUNDEF
PH2MBIAS(:) = XUNDEF

! Climatological arrays set to missing values

PSNC(:)     =  PSNS(:,1) ! need to read the snow climatology
PWSC(:)     =  XUNDEF
PWPC(:)     =  XUNDEF
PTSC(:)     =  XUNDEF
PTPC(:)     =  XUNDEF

DO JJ = 1, ISIZE
  PGELAT(JJ)   = PLAT(JJ)
  PGELAM(JJ)   = PLON(JJ)
  PGEMU(JJ)    = SIN(PLAT(JJ)*ZPIS180)
ENDDO

ZEVAP(:)   =  (PEVAP(:)/XLVTT*XDAY)/(NECHGU*3600.) ! conversion W/m2 -> mm/day
ZEVAPTR(:) =  PEVAPTR(:)*XDAY
ZSNS(:)    =  PSNS(:,1)

DO JJ = 1, ISIZE
  ZTS(JJ) = PTS(JJ,1)
  ZTP(JJ) = PTP(JJ,1)
ENDDO

IDAT = IYEAR*10000. + IMONTH*100. + IDAY

! Avoid division by zero in next WHERE statement; 
! this may occur in the extension zone
WHERE (LD_MASKEXT (1:ISIZE))
  PD2(:,1) = 1.0
  ZT2INC(:) = 0.0_JPRB
  ZH2INC(:) = 0.0_JPRB
END WHERE

PRINT *,'           '
PRINT *,'Mean T2m increments  ',SUM(ZT2INC)/YSC%U%NDIM_FULL
PRINT *,'Mean HU2m increments ',SUM(ZH2INC)/YSC%U%NDIM_FULL
PRINT *,'           '

!  Soil analysis based on optimal interpolation

 CALL OI_CACSTS(ISIZE,ZT2INC,ZH2INC,ZWGINC,ZSNINC,PWS_O,               &
                IDAT,NSSSSS,                                           &
                ZTP,ZWP,ZTL,ZSNS,ZTS,ZWS,                              &
                ZTCLS,ZHCLS,ZUCLS,ZVCLS,PSSTC,PWPINC1,PWPINC2,PWPINC3, &
                PT2MBIAS,PH2MBIAS,                                     &
                PRRCL,PRRSL,PRRCN,PRRSN,PATMNEB,ZEVAP,ZEVAPTR,         &
                PITM,PVEG(:,1),                                        &
                PALBF, PEMISF, PZ0F,                                   &
                PIVEG,PARG,PD2(:,1),PSAB,PLAI(:,1),PRSMIN(:,1),        &
                PZ0H,                                                  &
                PTSC,PTPC,PWSC,PWPC,PSNC,                              &
                PGELAT,PGELAM,PGEMU                                    ) 

!  Store increments

ZWSINC(:) = 0.0_JPRB
ZWPINC(:) = 0.0_JPRB
ZTLINC(:) = 0.0_JPRB
ZSNINC(:) = 0.0_JPRB

WHERE (YSC%U%XNATURE(:)>0.0_JPRB)
  ZWSINC(:) = ZWS(:) - PWS(:,1)*(XRD1*XRHOLW)    
  ZWPINC(:) = ZWP(:) - PWP(:,1)*(PD2(:,1)*XRHOLW) 
  ZTLINC(:) = ZTL(:) - PTL(:,1)*(PD2(:,1)*XRHOLW) 
  ZSNINC(:) = ZSNS(:) - PSNS(:,1)
END WHERE

! Avoid division by zero in next WHERE statement; 
! this may occur in the extension zone
WHERE (LD_MASKEXT (1:ISIZE))
  PD2(:,1) = 1.0
END WHERE

!  Define soil moiture analyses over NATURE points

WHERE (YSC%U%XNATURE(:)>0.0_JPRB)
  PWS(:,1)  = ZWS(:)/(XRD1*XRHOLW)
  PWP(:,1)  = ZWP(:)/(PD2(:,1)*XRHOLW)
  PTL(:,1)  = ZTL(:)/(PD2(:,1)*XRHOLW)
  PSNS(:,1) = ZSNS(:)
END WHERE

!  Perform temperature analysis according to surface types

ZTSINC(:) = 0.0_JPRB
ZTPINC(:) = 0.0_JPRB

! a) Temperature analysis of NATURE points

WHERE (YSC%U%XNATURE(:)>0.0_JPRB)
  ZTSINC(:) = ZTS(:) - PTS(:,1)
  ZTPINC(:) = ZTP(:) - PTP(:,1)
  PTS(:,1)  = ZTS(:)
  PTP(:,1)  = ZTP(:)
END WHERE

PRINT *, '---------------------------------------------------------------'
PRINT *, 'Mean WS increments over NATURE ',SUM(ZWSINC,YSC%U%XNATURE > 0.0_JPRB)/YSC%U%NSIZE_NATURE
PRINT *, 'Mean WP increments over NATURE ',SUM(ZWPINC,YSC%U%XNATURE > 0.0_JPRB)/YSC%U%NSIZE_NATURE
PRINT *, 'Mean TS increments over NATURE ',SUM(ZTSINC,YSC%U%XNATURE > 0.0_JPRB)/YSC%U%NSIZE_NATURE
PRINT *, 'Mean TP increments over NATURE ',SUM(ZTPINC,YSC%U%XNATURE > 0.0_JPRB)/YSC%U%NSIZE_NATURE
PRINT *, 'Mean TL increments over NATURE ',SUM(ZTLINC,YSC%U%XNATURE > 0.0_JPRB)/YSC%U%NSIZE_NATURE
PRINT *, 'Mean SN increments over NATURE ',SUM(ZSNINC,YSC%U%XNATURE > 0.0_JPRB)/YSC%U%NSIZE_NATURE
PRINT *, '---------------------------------------------------------------'

ZTSINC(:) = 0.0_JPRB

! b) Temperature analysis of SEA and LAKE points

ZRTCLS=ZSCAL/(5.0_JPRB*XDAY)
DO JJ = 1, ISIZE
  IF (YSC%U%XSEA(JJ)>0.0_JPRB) THEN
    ZTSINC(JJ) = PTS_O(JJ) - PSST(JJ)
    IF (PITM(JJ) <= 0.5_JPRB) THEN
      PSST(JJ) = PTS_O(JJ)
    ELSE
      IF (YSC%U%XNATURE(JJ)>0.0_JPRB) PSST(JJ) = PTP(JJ,1)
      IF (YSC%U%XNATURE(JJ)==0.0_JPRB) PSST(JJ) = PSST(JJ) + ZRTCLS*(PTCLS(JJ)-PSST(JJ)) 
    ENDIF
  ENDIF
  IF (YSC%U%XWATER(JJ)>0.0_JPRB) THEN
    IF (PITM(JJ) <= 0.5_JPRB) THEN
      PLST(JJ) = PTS_O(JJ)
    ELSE
      IF (YSC%U%XNATURE(JJ)>0.0_JPRB) PLST(JJ) = PTP(JJ,1)
      IF (YSC%U%XNATURE(JJ)==0.0_JPRB) PLST(JJ) = PLST(JJ) + ZRTCLS*(PTCLS(JJ)-PLST(JJ))
    ENDIF
  ENDIF
ENDDO

PRINT *, 'Mean TS increments over SEA    ',SUM(ZTSINC,YSC%U%XSEA > 0.0_JPRB)/YSC%U%NSIZE_SEA

! c) Temperature analysis of TOWN points

WHERE (YSC%U%XTOWN(:)>0.0_JPRB)
  PTRD3(:) = PTRD3(:) + ZT2INC(:)*Z1S2PI
END WHERE

PRINT *, '---------------------------------------------------------------'

 CALL FLAG_UPDATE(YSC%IM%ID%O, YSC%DUO, .FALSE.,.FALSE.,.TRUE.,.FALSE.)
CALL INIT_IO_SURF_n(YSC%DTCO, YSC%U, YPROGRAM,'FULL  ','SURF  ','WRITE')

#ifdef SFX_ARO
! Count 2D fields in MSE
! NINDX1, NINDX2, NKPROMA already set 
  CALL SURFEX_FIELD_BUF_SET_RECORD (YSURFEX_CACHE_OUT, .FALSE.)

  CALL WRITE
   
  CALL SURFEX_FIELD_BUF_PREALLOC (YSURFEX_CACHE_OUT)
  CALL SURFEX_FIELD_BUF_SET_RECORD (YSURFEX_CACHE_OUT, .TRUE.)

#endif
 CALL WRITE

CALL END_IO_SURF_n(YPROGRAM)
CALL IO_BUFF_CLEAN


! -------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK ('OI_CONTROL', 1, ZHOOK_HANDLE)

CONTAINS

SUBROUTINE WRITE 

REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK ('OI_CONTROL:WRITE', 0, ZHOOK_HANDLE)

CALL DD ('WG1', PWS (:,1))

YVAR='WG1'
YPREFIX='X_Y_WG1 (m3/m3)                                   '
CALL WRITE_SURF(YSC%DUO%CSELECT,  &
                YPROGRAM,YVAR,PWS,IRESP,HCOMMENT=YPREFIX)

CALL DD ('WG2', PWP (:,1))

YVAR='WG2'
YPREFIX='X_Y_WG2 (m3/m3)                                   '
CALL WRITE_SURF(YSC%DUO%CSELECT,  &
                YPROGRAM,YVAR,PWP,IRESP,HCOMMENT=YPREFIX)

CALL DD ('WGI2', PTL (:,1))

YVAR='WGI2'
YPREFIX='X_Y_WGI2 (m3/m3)                                  '
CALL WRITE_SURF(YSC%DUO%CSELECT,  &
                YPROGRAM,YVAR,PTL,IRESP,HCOMMENT=YPREFIX)

CALL DD ('TG1', PTS (:,1))

YVAR='TG1'
YPREFIX='X_Y_TG1 (K)                                      '
CALL WRITE_SURF(YSC%DUO%CSELECT,  &
                YPROGRAM,YVAR,PTS,IRESP,HCOMMENT=YPREFIX)

CALL DD ('TG2', PTP (:,1))

YVAR='TG2'
YPREFIX='X_Y_TG2 (K)                                       '
CALL WRITE_SURF(YSC%DUO%CSELECT, &
                YPROGRAM,YVAR,PTP,IRESP,HCOMMENT=YPREFIX)

CALL DD ('SST', PSST)

YVAR='SST'
YPREFIX='X_Y_SST (K)                                       '
CALL WRITE_SURF(YSC%DUO%CSELECT,  &
                YPROGRAM,YVAR,PSST,IRESP,HCOMMENT=YPREFIX)

CALL DD ('TS_WATER', PLST)

YVAR='TS_WATER'
YPREFIX='X_Y_TS_WATER (K)                                  '
CALL WRITE_SURF(YSC%DUO%CSELECT,  &
                YPROGRAM,YVAR,PLST,IRESP,HCOMMENT=YPREFIX)

IF (YSC%U%NSIZE_TOWN > 0 .AND. LAROME) THEN
  CALL DD ('T_ROAD3', PTRD3)

  YVAR='TROAD3'
  YPREFIX='X_Y_T_ROAD3 (K)                                   '
  CALL WRITE_SURF(YSC%DUO%CSELECT,  &
                YPROGRAM,YVAR,PTRD3,IRESP,HCOMMENT=YPREFIX)
ENDIF

CALL DD ('WSNOW_VEG1', PSNS (:,1))

YVAR='WSN_VEG1'
YPREFIX='X_Y_WSNOW_VEG1 (kg/m2)                            '
CALL WRITE_SURF(YSC%DUO%CSELECT, &
                YPROGRAM,YVAR,PSNS,IRESP,HCOMMENT=YPREFIX)

IF (LHOOK) CALL DR_HOOK ('OI_CONTROL:WRITE', 1, ZHOOK_HANDLE)

END SUBROUTINE WRITE

SUBROUTINE DD (CDN, PX)
CHARACTER(LEN=*), INTENT (IN) :: CDN
REAL, INTENT (IN) :: PX (:)

REAL :: ZX (SIZE (PX))
INTEGER :: JI, JN
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK ('OI_CONTROL:DD', 0, ZHOOK_HANDLE)

#ifdef SFX_ARO
IF (LHOOK) CALL DR_HOOK ('OI_CONTROL:DD', 1, ZHOOK_HANDLE)
RETURN
#endif
JN = COUNT (.NOT. LD_MASKEXT)
ZX (1:JN) = PACK (PX, .NOT. LD_MASKEXT)

WRITE (0, *) TRIM(CDN)//" = " 
WRITE (0, *) JN, MINVAL(ZX(1:JN)), MAXVAL(ZX(1:JN))
!WRITE (0, '(10(E14.6,", "))') ZX (1:N)

IF (LHOOK) CALL DR_HOOK ('OI_CONTROL:DD', 1, ZHOOK_HANDLE)

END SUBROUTINE DD

END SUBROUTINE OI_CONTROL
