!     #########
SUBROUTINE READ_NAMELISTS_ASSIM(HPROGRAM)
!     #######################################################
!
!---------------------------    
!
USE MODD_ASSIM,           ONLY : LASSIM,CASSIM,CASSIM_SEA,CASSIM_WATER,CASSIM_ISBA,   &
                                 CASSIM_TEB,NPRINTLEV,LAROME,LECSST,                  &
                                 LAESST,LAESNM,LSWE,LALADSURF,LREAD_SST_FROM_FILE,    &
                                 CFILE_FORMAT_SST,LEXTRAP_SEA,LEXTRAP_WATER,LEXTRAP_SNOW,&
                                 LEXTRAP_NATURE,LWATERTG2,NBOUTPUT,NECHGU,NECHGXFU, XRCLIMCA,   &
                                 XRCLISST,XSIGH2MO,XSIGT2MO, XSIGWGO,XSIGWGB,XSIGW2B, &
                                 LOBSWG,LOBS2M,LIMVEG,XSPRECIP2,XRTHR_QC,XSIGWGO_MAX, &
                                 XRSCAL_JAC,LPRT,LSIM,LBEV,LBFIXED,NOBSTYPE,          &
                                 LOBSHEADER,CFILE_FORMAT_LSM,CFILE_FORMAT_OBS,        &
                                 CFILE_FORMAT_FG,CFILE_FORMAT_CLIM,COBS_M,XERROBS_M,XQCOBS_M,&
                                 NNCO,NIVAR,NVAR,CVAR_M,CPREFIX_M,XSIGMA_M,XTPRT_M,   &
                                 NNCV,XSCALE_Q,XSCALE_QLAI,CBIO,CPREFIX_BIO,XALPH,    &
                                 NENS,NIE,XINFL_M,XADDINFL_M,XASSIM_WINH,LOBSNAT,     &
                                 XADDTIMECORR_M,LENS_GEN,LPB_CORRELATIONS,            &
                                 LPERTURBATION_RUN,LBIAS_CORRECTION,LENKF,LDENKF
!
USE MODI_DEFAULT_ASSIM
USE MODI_READ_ASSIM_CONF
USE MODI_INI_ASSIM
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM      ! program calling surf. schemes
REAL(KIND=JPRB)                 :: ZHOOK_HANDLE

!---------------------------------------------------
IF (LHOOK) CALL DR_HOOK('READ_NAMELISTS_ASSIM',0,ZHOOK_HANDLE)

! Set default assimilation options/schemes
CALL DEFAULT_ASSIM(LASSIM,CASSIM,CASSIM_SEA,CASSIM_WATER,    &
                   CASSIM_ISBA,CASSIM_TEB,NPRINTLEV,         &
                   LAROME,LECSST,LAESST,LAESNM,LSWE,         &
                   LALADSURF,LREAD_SST_FROM_FILE,            &
                   CFILE_FORMAT_SST,LEXTRAP_SEA,LEXTRAP_WATER,&
                   LEXTRAP_NATURE,LEXTRAP_SNOW,LWATERTG2,    &
                   NBOUTPUT,NECHGU,NECHGXFU,XRCLIMCA,XRCLISST,        &
                   XSIGH2MO,XSIGT2MO,XSIGWGO,XSIGWGB,        &
                   XSIGW2B,LOBSWG,LOBS2M,LIMVEG,XSPRECIP2,   &
                   XRTHR_QC,XSIGWGO_MAX,XRSCAL_JAC,LPRT,     &
                   LSIM,LBEV,LBFIXED,NOBSTYPE,LOBSHEADER,    &
                   CFILE_FORMAT_OBS,LOBSNAT,CFILE_FORMAT_FG, &
                   CFILE_FORMAT_LSM,CFILE_FORMAT_CLIM,COBS_M,&
                   XERROBS_M,XQCOBS_M,NNCO,NIVAR,NVAR,CVAR_M,&
                   CPREFIX_M,XSIGMA_M,XTPRT_M,NNCV,XSCALE_Q, &
                   XSCALE_QLAI,CBIO,CPREFIX_BIO,XALPH,       &
                   NENS,NIE,XINFL_M,XADDINFL_M,XASSIM_WINH,  &
                   XADDTIMECORR_M,LENS_GEN,LPB_CORRELATIONS, &
                   LPERTURBATION_RUN,LBIAS_CORRECTION,       &
                   LENKF,LDENKF,'OK')
!
! Set default assimilations values/constants
CALL INI_ASSIM
!
! Override with namelist values
CALL READ_ASSIM_CONF(HPROGRAM)

IF (LHOOK) CALL DR_HOOK('READ_NAMELISTS_ASSIM',1,ZHOOK_HANDLE)
!---------------------------------------------------------
END SUBROUTINE READ_NAMELISTS_ASSIM
