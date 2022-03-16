!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE ASSIM_NATURE_ISBA_OI (IO, S, K, NP, NPE, ID, HPROGRAM, KI, &
                                PRRCL,    PRRSL,  PRRCN,   PRRSN,     &
                                PATMNEB,  PITM,   PEVAPTR, PEVAP,     &
                                PSNC,     PTSC,   PUCLS,   PVCLS,     &
                                PTS_O,    PT2M_O, PHU2M_O,            &
                                HTEST, OD_MASKEXT, PLON_IN, PLAT_IN   )

! ------------------------------------------------------------------------------------------
!  *****************************************************************************************
!
!  Routine to perform OI within SURFEX 
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
!   (07/2011)  : Read pgd+prep (B. Decharme)
!   (04/2012)  : Made as a subroutine (T. Aspelien)
!   (06/2013)  : Separating IO (T. Aspelien)
! ******************************************************************************************
! ------------------------------------------------------------------------------------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_S_t, ISBA_K_t, ISBA_NP_t, ISBA_NPE_t, ISBA_P_t, ISBA_PE_t
USE MODD_SURFEX_n, ONLY : ISBA_DIAG_t
!
USE MODD_CSTS,            ONLY : XDAY, XPI, XRHOLW, XLVTT, NDAYSEC
USE MODD_SURF_PAR,        ONLY : XUNDEF 
!
USE MODD_ASSIM,           ONLY : LOBSWG, NITRAD, NPRINTLEV, NECHGU, XRD1, XRSCALDW,   &
                                 XRTHR_QC, XSIGWGB, XSIGWGO, XSIGWGO_MAX, XAT2M_ISBA, &
                                 XAHU2M_ISBA, XAZON10M_ISBA, XAMER10M_ISBA, LPIO
!

USE YOMHOOK,              ONLY : LHOOK,   DR_HOOK
USE PARKIND1,             ONLY : JPRB
!
USE MODI_PACK_SAME_RANK
USE MODI_ABOR1_SFX
USE MODI_OI_BC_SOIL_MOISTURE
USE MODI_OI_CACSTS
USE MODI_ASSIM_GATHER_WRITE_INCREMENTS
USE MODI_GET_LUOUT
!
IMPLICIT NONE
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_K_t), INTENT(INOUT) :: K
TYPE(ISBA_NP_t), INTENT(INOUT) :: NP
TYPE(ISBA_NPE_t), INTENT(INOUT) :: NPE
TYPE(ISBA_DIAG_t), INTENT(INOUT) :: ID
!
TYPE(ISBA_P_t), POINTER :: PK
TYPE(ISBA_PE_t), POINTER :: PEK
!
CHARACTER(LEN=6),    INTENT(IN) :: HPROGRAM  ! program calling surf. schemes
INTEGER,             INTENT(IN) :: KI
REAL, DIMENSION(KI), INTENT(IN) :: PRRCL
REAL, DIMENSION(KI), INTENT(IN) :: PRRSL
REAL, DIMENSION(KI), INTENT(IN) :: PRRCN
REAL, DIMENSION(KI), INTENT(IN) :: PRRSN
REAL, DIMENSION(KI), INTENT(IN) :: PATMNEB
REAL, DIMENSION(KI), INTENT(IN) :: PITM
REAL, DIMENSION(KI), INTENT(IN) :: PEVAPTR
REAL, DIMENSION(KI), INTENT(IN) :: PEVAP
REAL, DIMENSION(KI), INTENT(IN) :: PSNC
REAL, DIMENSION(KI), INTENT(IN) :: PTSC
REAL, DIMENSION(KI), INTENT(IN) :: PUCLS
REAL, DIMENSION(KI), INTENT(IN) :: PVCLS
REAL, DIMENSION(KI), INTENT(IN) :: PTS_O
REAL, DIMENSION(KI), INTENT(IN) :: PT2M_O
REAL, DIMENSION(KI), INTENT(IN) :: PHU2M_O
CHARACTER(LEN=2),    INTENT(IN) :: HTEST ! must be equal to 'OK'
LOGICAL,  DIMENSION (KI) ::  OD_MASKEXT
REAL(KIND=JPRB), DIMENSION (:), INTENT(IN) ::  PLON_IN
REAL(KIND=JPRB), DIMENSION (:), INTENT(IN) ::  PLAT_IN

!    Declarations of local variables

! Arrays with ISBA dimension
REAL, DIMENSION (KI) :: ZSM_O, ZSIG_SMO, ZLSM_O, ZWS_O,  ZSAB,   ZARG, &
                        ZTCLS, ZHCLS,    ZT2INC, ZH2INC, ZWGINC

! Allocatable arrays for patch dimension packed from ISBA dimension
REAL,ALLOCATABLE,DIMENSION(:) :: ZSAB_P,   ZARG_P,    ZGELAM_P,  ZGELAT_P, ZITM_P,    &
                                 ZRRCL_P,  ZRRSL_P,   ZRRCN_P,   ZRRSN_P,  ZATMNEB_P, &
                                 ZEVAP_P,  ZEVAPTR_P,                                 &      
                                 ZTCLS_P,  ZHCLS_P,   ZUCLS_P,   ZVCLS_P,             &
                                 ZTS_O_P,  ZT2M_O_P,  ZHU2M_O_P, ZSM_O_P,  ZWS_O_P,   &
                                 ZSNC_P,   ZTSC_P,                                    &
                                 ZT2INC_P, ZH2INC_P,  ZWGINC_P,  ZSNINC_P

 
! Allocatable arrays for patch dimensionn
REAL,ALLOCATABLE,DIMENSION(:) :: ZWS,  ZWP,  ZTL,  ZTP0, ZTS0,         &
                                 ZWS0, ZWP0, ZTL0, ZTP,  ZTS,          &
                                 ZWSC, ZWPC,ZTPC,  ZSSTC,              &
                                 ZSNS, ZD2, ZRSMIN, ZLAI, ZVEG, ZIVEG, &
                                 ZALBF, ZEMISF, ZZ0F, ZZ0H,            &
                                 ZWPINC1, ZWPINC2, ZWPINC3,            &
                                 ZT2MBIAS, ZH2MBIAS, ZGEMU,            &
                                 ZWSINC, ZWPINC, ZTLINC,ZTSINC,ZTPINC 

!                 
CHARACTER(LEN=2) :: CPATCH
REAL :: ZTHRES
INTEGER  :: IDAT
INTEGER  :: IYEAR                      ! current year (UTC)
INTEGER  :: IMONTH                     ! current month (UTC)
INTEGER  :: IDAY                       ! current day (UTC)
INTEGER  :: ISSSSS                     ! current time since start of the run (s)
INTEGER  :: JI, JP, JL                 ! loop variables
INTEGER  :: INOBS                      ! number of observations
INTEGER  :: IOBS_T2M                   ! number of T2M observations
INTEGER  :: IOBS_RH2M                  ! number of RH2M observations
INTEGER  :: ILUOUT
!
REAL(KIND=JPRB)      :: ZHOOK_HANDLE
!
! ----------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ASSIM_NATURE_ISBA_OI',0,ZHOOK_HANDLE)

IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('ASSIM_NATURE_ISBA_OI: FATAL ERROR DURING ARGUMENT TRANSFER')
ENDIF

CALL GET_LUOUT(HPROGRAM,ILUOUT)
IF ( LPIO .AND. NPRINTLEV > 0 ) THEN 
  WRITE(ILUOUT,*) '--------------------------------------------------------------------------'
  WRITE(ILUOUT,*) '|                                                                        |'
  WRITE(ILUOUT,*) '|                             ENTER OI_ASSIM                             |'
  WRITE(ILUOUT,*) '|                                                                        |'
  WRITE(ILUOUT,*) '--------------------------------------------------------------------------'
ENDIF
!
!   Update some constants dependant from NACVEG
!
!  scaling of soil moisture increments when assimilation window is different
!  from 6 hours
XRSCALDW = REAL(NECHGU)/6.0
!  half assimilation window in sec
NITRAD = NECHGU*1800
!
!   Time initializations 
!
IYEAR  = S%TTIME%TDATE%YEAR
IMONTH = S%TTIME%TDATE%MONTH
IDAY   = S%TTIME%TDATE%DAY
ISSSSS = S%TTIME%TIME
IF ( ISSSSS>NDAYSEC ) ISSSSS = ISSSSS - NDAYSEC
IDAT = IYEAR*10000. + IMONTH*100. + IDAY


! Sand and clay (use layer 1)
ZSAB  (:) = K%XSAND(:,1)*100.
ZARG  (:) = K%XCLAY(:,1)*100.
IF ( NPRINTLEV > 1 ) THEN
  WRITE(*,*) 'value in PREP file => SAND      ',SUM(ZSAB)/KI
  WRITE(*,*) 'value in PREP file => CLAY      ',SUM(ZARG)/KI
ENDIF

! First guess
IF ( IO%LCANOPY) THEN
  ZTCLS(:)=XAT2M_ISBA(:,1)
  ZHCLS(:)=XAHU2M_ISBA(:,1)
ELSE
  IF ( IO%NPATCH > 2 ) THEN
    CALL ABOR1_SFX('ABORT: NPATCH > 2 NEED TO DEFINE WHICH PATCH(ES) ARE REPRESENTATIVE FOR OBSERVED T2M')
  ELSE
    ZTCLS(:)=XAT2M_ISBA(:,1)
    ZHCLS(:)=XAHU2M_ISBA(:,1)
    IF ( IO%NPATCH == 2 ) THEN
       WHERE(ZTCLS(:)==XUNDEF)
          ZTCLS(:)=XAT2M_ISBA(:,2)
       ENDWHERE
       WHERE(ZHCLS(:)==XUNDEF)
          ZHCLS(:)=XAHU2M_ISBA(:,2)
       ENDWHERE
    ENDIF
  ENDIF
ENDIF

! Screen-level innovations
IOBS_T2M=0
ZT2INC=0.
DO JI=1,KI
  IF ( PT2M_O(JI) .NE. 999. ) THEN
    ZT2INC(JI) = PT2M_O(JI) - ZTCLS(JI)
    IOBS_T2M=IOBS_T2M+1
  ENDIF
ENDDO
ZH2INC=0.
IOBS_RH2M=0
DO JI=1,KI
  IF ( PHU2M_O(JI) .NE. 999. ) THEN
    ZH2INC(JI) = PHU2M_O(JI) - MAX(0.,MIN(1.0,ZHCLS(JI)))
    IOBS_RH2M=IOBS_RH2M+1
  ENDIF
ENDDO

! Avoid division by zero in WHERE statements; 
! this may occur in the extension zone
WHERE (OD_MASKEXT(:))
  ZT2INC(:) = 0.0
  ZH2INC(:) = 0.0
END WHERE

IF ( NPRINTLEV > 0 ) THEN
  CALL ASSIM_GATHER_WRITE_INCREMENTS(HPROGRAM,"T2M",ZTCLS,PT2M_O,999.)
  CALL ASSIM_GATHER_WRITE_INCREMENTS(HPROGRAM,"HU2M",MAX(0.,MIN(1.0,ZHCLS(:))),PHU2M_O,999.)
ENDIF

!  Read ASCAT SM observations (in percent) for nature tile points. Won't work on more processors
IF ( LOBSWG ) THEN
  INOBS = 0
  OPEN(UNIT=111,FILE='ASCAT_SM.DAT')
  DO JI=1,KI
    READ(111,*) ZSM_O(JI),ZSIG_SMO(JI),ZLSM_O(JI)
    ! data rejection if not on land
    ! data rejection of error too large
    IF ( ZLSM_O(JI)<1.0 .OR. ZSIG_SMO(JI)>XSIGWGO_MAX ) ZSM_O(JI) = 999.0
    IF ( ZSM_O(JI)/=999.0 ) INOBS = INOBS + 1
  ENDDO
  CLOSE(UNIT=111)
  IF ( .NOT. LPIO ) CALL ABOR1_SFX("LOBSWG is not implemented with multi-processors yet")
  IF ( LPIO .AND. NPRINTLEV > 0 ) WRITE(ILUOUT,*) 'READ ASCAT SM OK'

  IF ( LPIO .AND. NPRINTLEV > 0 ) WRITE(ILUOUT,*) ' NUMBER OF ASCAT OBSERVATIONS AFTER INITIAL CHECKS  :: ',INOBS

  ! Perform bias correction of SM observations
  CALL OI_BC_SOIL_MOISTURE(KI,ZSM_O,ZSAB,ZWS_O)

  ! Threshold for background check
  ZTHRES = XRTHR_QC*SQRT(XSIGWGO**2 + XSIGWGB**2)
  ! Superficial soil moisture innovations in (m3/m3)
  INOBS = 0
  DO JI=1,KI
    IF ( ZWS_O(JI)/=999.0 ) THEN
      ZWGINC(JI) = ZWS_O(JI) - ZWS(JI)
      IF ( ABS(ZWGINC(JI))>ZTHRES ) THEN
        ZWGINC(JI) = 0.0 ! background check
      ELSE
        INOBS = INOBS + 1
      ENDIF
    ELSE
      ZWGINC(JI) = 0.0
    ENDIF
  ENDDO
  IF ( LPIO .AND. NPRINTLEV > 0 ) THEN
    WRITE(ILUOUT,*) ' NUMBER OF ASCAT OBSERVATIONS AFTER BACKGROUND CHECK  :: ',INOBS
  ENDIF
ELSE
  ZWS_O (:) = 999.
  ZWGINC(:) = 0.
ENDIF

! Update ISBA diagnostics
WHERE ( PT2M_O(:) .NE. 999. )
  ID%D%XT2M(:)  = PT2M_O(:)
ENDWHERE
WHERE ( PHU2M_O(:) .NE. 999. )
  ID%D%XHU2M(:) = PHU2M_O(:)
ENDWHERE

! Patch loop
DO JP = 1,IO%NPATCH
   
  PK => NP%AL(JP)
  PEK => NPE%AL(JP)

  ALLOCATE(ZRRCL_P(PK%NSIZE_P))
  ALLOCATE(ZRRSL_P(PK%NSIZE_P))
  ALLOCATE(ZRRCN_P(PK%NSIZE_P))
  ALLOCATE(ZRRSN_P(PK%NSIZE_P))
  ALLOCATE(ZATMNEB_P(PK%NSIZE_P))
  ALLOCATE(ZITM_P(PK%NSIZE_P))
  ALLOCATE(ZEVAP_P(PK%NSIZE_P))
  ALLOCATE(ZEVAPTR_P(PK%NSIZE_P))
  ALLOCATE(ZSNC_P(PK%NSIZE_P))
  ALLOCATE(ZTSC_P(PK%NSIZE_P))
  ALLOCATE(ZUCLS_P(PK%NSIZE_P))
  ALLOCATE(ZVCLS_P(PK%NSIZE_P))
  ALLOCATE(ZTS_O_P(PK%NSIZE_P))
  ALLOCATE(ZT2M_O_P(PK%NSIZE_P))
  ALLOCATE(ZHU2M_O_P(PK%NSIZE_P))
  ALLOCATE(ZGELAM_P(PK%NSIZE_P))
  ALLOCATE(ZGELAT_P(PK%NSIZE_P))
  ALLOCATE(ZTCLS_P(PK%NSIZE_P))
  ALLOCATE(ZT2INC_P(PK%NSIZE_P))
  ALLOCATE(ZHCLS_P(PK%NSIZE_P))
  ALLOCATE(ZH2INC_P(PK%NSIZE_P))
  ALLOCATE(ZWGINC_P(PK%NSIZE_P))
  ALLOCATE(ZSNINC_P(PK%NSIZE_P))
  ALLOCATE(ZWS_O_P(PK%NSIZE_P))
  ALLOCATE(ZSAB_P(PK%NSIZE_P))
  ALLOCATE(ZARG_P(PK%NSIZE_P))


  ! Local patch arrays
  ALLOCATE(ZTS0(PK%NSIZE_P))
  ALLOCATE(ZTP0(PK%NSIZE_P))
  ALLOCATE(ZWS0(PK%NSIZE_P))
  ALLOCATE(ZWP0(PK%NSIZE_P))
  ALLOCATE(ZTL0(PK%NSIZE_P))
  ALLOCATE(ZIVEG(PK%NSIZE_P))
  ALLOCATE(ZALBF(PK%NSIZE_P))
  ALLOCATE(ZEMISF(PK%NSIZE_P))
  ALLOCATE(ZZ0F(PK%NSIZE_P))
  ALLOCATE(ZZ0H(PK%NSIZE_P))
  ALLOCATE(ZSNS(PK%NSIZE_P))
  ALLOCATE(ZWPINC1(PK%NSIZE_P))
  ALLOCATE(ZWPINC2(PK%NSIZE_P))
  ALLOCATE(ZWPINC3(PK%NSIZE_P))
  ALLOCATE(ZT2MBIAS(PK%NSIZE_P))
  ALLOCATE(ZH2MBIAS(PK%NSIZE_P))
  ALLOCATE(ZWSC(PK%NSIZE_P))
  ALLOCATE(ZWPC(PK%NSIZE_P))
  ALLOCATE(ZTPC(PK%NSIZE_P))
  ALLOCATE(ZSSTC(PK%NSIZE_P))
  ALLOCATE(ZTP(PK%NSIZE_P))
  ALLOCATE(ZWP(PK%NSIZE_P))
  ALLOCATE(ZTL(PK%NSIZE_P))
  ALLOCATE(ZTS(PK%NSIZE_P))
  ALLOCATE(ZWS(PK%NSIZE_P))
  ALLOCATE(ZD2(PK%NSIZE_P))
  ALLOCATE(ZVEG(PK%NSIZE_P))
  ALLOCATE(ZLAI(PK%NSIZE_P))
  ALLOCATE(ZRSMIN(PK%NSIZE_P))
  ALLOCATE(ZWSINC(PK%NSIZE_P))
  ALLOCATE(ZWPINC(PK%NSIZE_P))
  ALLOCATE(ZTLINC(PK%NSIZE_P))
  ALLOCATE(ZTSINC(PK%NSIZE_P))
  ALLOCATE(ZTPINC(PK%NSIZE_P))
  ALLOCATE(ZGEMU(PK%NSIZE_P))

  ! Pack ISBA fields to this patch
  CALL PACK_SAME_RANK(PK%NR_P,PRRCL,ZRRCL_P)
  CALL PACK_SAME_RANK(PK%NR_P,PRRSL,ZRRSL_P)
  CALL PACK_SAME_RANK(PK%NR_P,PRRCN,ZRRCN_P)
  CALL PACK_SAME_RANK(PK%NR_P,PRRSN,ZRRSN_P)
  CALL PACK_SAME_RANK(PK%NR_P,PATMNEB,ZATMNEB_P)
  CALL PACK_SAME_RANK(PK%NR_P,PITM,ZITM_P)
  CALL PACK_SAME_RANK(PK%NR_P,PEVAP,ZEVAP_P)
  CALL PACK_SAME_RANK(PK%NR_P,PEVAPTR,ZEVAPTR_P)
  CALL PACK_SAME_RANK(PK%NR_P,PSNC,ZSNC_P)
  CALL PACK_SAME_RANK(PK%NR_P,PTSC,ZTSC_P)
  CALL PACK_SAME_RANK(PK%NR_P,PUCLS,ZUCLS_P)
  CALL PACK_SAME_RANK(PK%NR_P,PVCLS,ZVCLS_P)
  CALL PACK_SAME_RANK(PK%NR_P,PTS_O,ZTS_O_P)
  CALL PACK_SAME_RANK(PK%NR_P,PT2M_O,ZT2M_O_P)
  CALL PACK_SAME_RANK(PK%NR_P,PHU2M_O,ZHU2M_O_P)
  CALL PACK_SAME_RANK(PK%NR_P,PLON_IN,ZGELAM_P)
  CALL PACK_SAME_RANK(PK%NR_P,PLAT_IN,ZGELAT_P)
  CALL PACK_SAME_RANK(PK%NR_P,ZTCLS,ZTCLS_P)
  CALL PACK_SAME_RANK(PK%NR_P,ZT2INC,ZT2INC_P)
  CALL PACK_SAME_RANK(PK%NR_P,ZHCLS,ZHCLS_P)
  CALL PACK_SAME_RANK(PK%NR_P,ZH2INC,ZH2INC_P)
  CALL PACK_SAME_RANK(PK%NR_P,ZWGINC,ZWGINC_P)
  CALL PACK_SAME_RANK(PK%NR_P,ZWS_O,ZWS_O_P)
  CALL PACK_SAME_RANK(PK%NR_P,ZSAB,ZSAB_P)
  CALL PACK_SAME_RANK(PK%NR_P,ZARG,ZARG_P)

  ! Set undefined
  ZWPINC1 (:) = XUNDEF
  ZWPINC2 (:) = XUNDEF
  ZWPINC3 (:) = XUNDEF
  ZT2MBIAS(:) = XUNDEF
  ZH2MBIAS(:) = XUNDEF
  ZSNINC_P(:) = XUNDEF
 
  ! Climatological arrays set to missing values
  ZWSC(:) = XUNDEF
  ZWPC(:) = XUNDEF
  ZTPC(:) = XUNDEF
 
  ! SST not used in cacsts
  ZSSTC(:)    = 0.

  ZGEMU(:)=SIN(ZGELAT_P(:)*XPI/180.)
  ZEVAP_P  (:) =  (ZEVAP_P(:)/XLVTT*XDAY)/(NECHGU*3600.) ! conversion W/m2 -> mm/day
  ZEVAPTR_P(:) =  ZEVAPTR_P(:)*XDAY

  ! Set PIVEG (SURFIND.VEG.DOMI) since it is not available
  ZIVEG(:) = 0.0

  ZALBF(:) = XUNDEF
  ZEMISF(:) = XUNDEF
  ZZ0F(:) = XUNDEF
  ZZ0H(:) = XUNDEF

  ! Initialize patch variables
  ZD2(:)=PK%XDG(:,2)
  ZTS0(:)=PEK%XTG(:,1)
  ZTP0(:)=PEK%XTG(:,2)
  ZWS0(:)=PEK%XWG(:,1)
  ZWP0(:)=PEK%XWG(:,2)
  ZTL0(:)=PEK%XWGI(:,2)
  ZSNS(:)=0.
  DO JL=1,PEK%TSNOW%NLAYER
    ZSNS(:)=ZSNS(:)+PEK%TSNOW%WSNOW(:,JL)
  ENDDO
  ZRSMIN(:)=PEK%XRSMIN(:)
  ZLAI(:)=PEK%XLAI(:)
  ZVEG(:)=PEK%XVEG(:)

  ! PRINT 
  IF ( NPRINTLEV > 1 ) THEN
    WRITE(*,*) 'value in PREP file => WG1       ',SUM(ZWS0)/PK%NSIZE_P,' for patch ',JP
    WRITE(*,*) 'value in PREP file => WG2       ',SUM(ZWP0)/PK%NSIZE_P,' for patch ',JP
    WRITE(*,*) 'value in PREP file => TG1       ',SUM(ZTS0)/PK%NSIZE_P,' for patch ',JP
    WRITE(*,*) 'value in PREP file => TG2       ',SUM(ZTP0)/PK%NSIZE_P,' for patch ',JP
    WRITE(*,*) 'value in PREP file => WGI2      ',SUM(ZTL0)/PK%NSIZE_P,' for patch ',JP
    WRITE(*,*) 'value in PREP file => WSNOW_VEG1',SUM(ZSNS)/PK%NSIZE_P,' for patch ',JP
    WRITE(*,*) 'value in PREP file => LAI       ',SUM(ZLAI)/PK%NSIZE_P,' for patch ',JP
    WRITE(*,*) 'value in PREP file => VEG       ',SUM(ZVEG)/PK%NSIZE_P,' for patch ',JP
    WRITE(*,*) 'value in PREP file => RSMIN     ',SUM(ZRSMIN)/PK%NSIZE_P,' for patch ',JP
    WRITE(*,*) 'value in PREP file => DATA_DG2  ',SUM(ZD2)/PK%NSIZE_P,' for patch ',JP
  ENDIF
 
  ! Convert m3/m3 -> mm for oi_cacsts  
  WHERE ( ZWS0(:)/=XUNDEF )
    ZWS(:) = ZWS0(:) * XRD1   * XRHOLW 
    ZWP(:) = ZWP0(:) * ZD2(:) * XRHOLW 
    ZTL(:) = ZTL0(:) * ZD2(:) * XRHOLW 
  ENDWHERE

  ZTS(:) = ZTS0(:)
  ZTP(:) = ZTP0(:)
  CALL OI_CACSTS(PK%NSIZE_P, ZT2INC_P, ZH2INC_P, ZWGINC_P, ZSNINC_P, ZWS_O_P,     &
               IDAT, ISSSSS,                                                      &
               ZTP, ZWP, ZTL, ZSNS, ZTS, ZWS,                                     &
               ZTCLS_P, ZHCLS_P, ZUCLS_P, ZVCLS_P, ZSSTC,                         &
               ZWPINC1, ZWPINC2, ZWPINC3, ZT2MBIAS, ZH2MBIAS,                     &
               ZRRCL_P, ZRRSL_P, ZRRCN_P, ZRRSN_P, ZATMNEB_P, ZEVAP_P, ZEVAPTR_P, &
               ZITM_P, ZVEG, ZALBF, ZEMISF, ZZ0F,                                 &
               ZIVEG, ZARG_P, ZD2, ZSAB_P, ZLAI, ZRSMIN, ZZ0H,                    &
               ZTSC_P, ZTPC, ZWSC, ZWPC, ZSNC_P, ZGELAT_P, ZGELAM_P, ZGEMU  )  

 ! PRINT statistics of the soil analysis
  WRITE(CPATCH(1:2),'(I2)') JP
  CALL ASSIM_GATHER_WRITE_INCREMENTS(HPROGRAM,"WG1  PATCH: "//TRIM(CPATCH),(ZWS0(:)* XRD1 * XRHOLW),ZWS,XUNDEF)
  CALL ASSIM_GATHER_WRITE_INCREMENTS(HPROGRAM,"WG2  PATCH: "//TRIM(CPATCH),(ZWP0(:)* ZD2(:) * XRHOLW),ZWP,XUNDEF)
  CALL ASSIM_GATHER_WRITE_INCREMENTS(HPROGRAM,"TG1  PATCH: "//TRIM(CPATCH),ZTS0,ZTS,XUNDEF)
  CALL ASSIM_GATHER_WRITE_INCREMENTS(HPROGRAM,"TG2  PATCH: "//TRIM(CPATCH),ZTP0,ZTP,XUNDEF)
  CALL ASSIM_GATHER_WRITE_INCREMENTS(HPROGRAM,"WGI2 PATCH: "//TRIM(CPATCH),(ZTL0(:)* ZD2(:) * XRHOLW),ZTL,XUNDEF)

  !  Perform soil moisture analysis
  ZWSINC(:) = 0.0
  ZWPINC(:) = 0.0
  ZTLINC(:) = 0.0
  WHERE ( ZWS0(:)/=XUNDEF )
    ZWSINC(:) = ZWS(:) - ZWS0(:) * XRD1 * XRHOLW
    ZWPINC(:) = ZWP(:) - ZWP0(:) * ZD2(:) * XRHOLW
    ZTLINC(:) = ZTL(:) - ZTL0(:) * ZD2(:) * XRHOLW
  END WHERE

  WHERE (ZWS0(:)/=XUNDEF)
    ZWS0(:)  = ZWS(:)/ (XRD1*XRHOLW)
    ZWP0(:)  = ZWP(:)/ (ZD2(:)*XRHOLW)
    ZTL0(:)  = ZTL(:)/ (ZD2(:)*XRHOLW)
  ENDWHERE

  !  Perform temperature analysis
  ZTSINC(:) = 0.0
  ZTPINC(:) = 0.0
  WHERE (ZTS(:)/=XUNDEF)
    ZTSINC(:) = ZTS(:) - ZTS0(:)
    ZTPINC(:) = ZTP(:) - ZTP0(:)
    ZTS0(:)   = ZTS(:)
    ZTP0(:)   = ZTP(:)
  END WHERE


  ! Update modified variables
  PEK%XWG (:,1) = ZWS0(:)
  PEK%XWG (:,2) = ZWP0(:)
  PEK%XTG (:,1) = ZTS0(:)
  PEK%XTG (:,2) = ZTP0(:)
  PEK%XWGI(:,2) = ZTL0(:)

  ! ISBA patch diagnostic variables
  WHERE ( ZT2M_O_P(:) .NE. 999. )
    ID%ND%AL(JP)%XT2M(:)  = ZT2M_O_P(:)
  ENDWHERE
  WHERE ( ZHU2M_O_P(:) .NE. 999. )
    ID%ND%AL(JP)%XHU2M(:) = ZHU2M_O_P(:)
  ENDWHERE

  ! Deallocate patch arrays
  DEALLOCATE(ZRRCL_P)
  DEALLOCATE(ZRRSL_P)
  DEALLOCATE(ZRRCN_P)
  DEALLOCATE(ZRRSN_P)
  DEALLOCATE(ZATMNEB_P)
  DEALLOCATE(ZITM_P)
  DEALLOCATE(ZEVAP_P)
  DEALLOCATE(ZEVAPTR_P)
  DEALLOCATE(ZSNC_P)
  DEALLOCATE(ZTSC_P)
  DEALLOCATE(ZUCLS_P)
  DEALLOCATE(ZVCLS_P)
  DEALLOCATE(ZTS_O_P)
  DEALLOCATE(ZT2M_O_P)
  DEALLOCATE(ZHU2M_O_P)
  DEALLOCATE(ZGELAM_P)
  DEALLOCATE(ZGELAT_P)
  DEALLOCATE(ZTCLS_P)
  DEALLOCATE(ZT2INC_P)
  DEALLOCATE(ZHCLS_P)
  DEALLOCATE(ZH2INC_P)
  DEALLOCATE(ZSAB_P)
  DEALLOCATE(ZARG_P)
  DEALLOCATE(ZWGINC_P)
  DEALLOCATE(ZSNINC_P)
  DEALLOCATE(ZWS_O_P)

  DEALLOCATE(ZTS0)
  DEALLOCATE(ZTP0)
  DEALLOCATE(ZTS)
  DEALLOCATE(ZTL)
  DEALLOCATE(ZTP)
  DEALLOCATE(ZWP)
  DEALLOCATE(ZWS)
  DEALLOCATE(ZWS0)
  DEALLOCATE(ZWP0)
  DEALLOCATE(ZTL0)
  DEALLOCATE(ZIVEG)
  DEALLOCATE(ZALBF)
  DEALLOCATE(ZEMISF)
  DEALLOCATE(ZZ0F)
  DEALLOCATE(ZZ0H)
  DEALLOCATE(ZSNS)
  DEALLOCATE(ZWPINC1)
  DEALLOCATE(ZWPINC2)
  DEALLOCATE(ZWPINC3)
  DEALLOCATE(ZT2MBIAS)
  DEALLOCATE(ZH2MBIAS)
  DEALLOCATE(ZWSC)
  DEALLOCATE(ZWPC)
  DEALLOCATE(ZTPC)
  DEALLOCATE(ZSSTC)
  DEALLOCATE(ZD2)
  DEALLOCATE(ZVEG)
  DEALLOCATE(ZLAI)
  DEALLOCATE(ZRSMIN)
  DEALLOCATE(ZWSINC)
  DEALLOCATE(ZWPINC)
  DEALLOCATE(ZTLINC)
  DEALLOCATE(ZTSINC)
  DEALLOCATE(ZTPINC)
  DEALLOCATE(ZGEMU)
ENDDO
!
! -------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ASSIM_NATURE_ISBA_OI',1,ZHOOK_HANDLE)
END SUBROUTINE ASSIM_NATURE_ISBA_OI
