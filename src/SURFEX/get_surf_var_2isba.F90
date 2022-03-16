!     #########
      SUBROUTINE GET_SURF_VAR_2ISBA(U, O, DGMI,HPROGRAM, KI, PNATURE, &
&                                     PTG2, PSWI1, PSWI2, PWGI1, PWGI2, &
&                                     PWR, PSNA, PSND, PHV)
!     #######################################################################
!
!!****  *GET_SURF_VAR_2ISBA* - gets some surface fields over nature
!!
!!    PURPOSE
!!    -------
!!
!!    This program returns some surface variables needed by the atmosphere when
!!    ISBA old scheme variables are computed
!!
!!**  METHOD
!!    ------
!!
!!    Several functions are called in order to initialize surface variables
!!    needed by the atmospheric model. These functions fill the required arrays by
!!    the surface model values computed during the run.
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
!!      F. Taillefer  06/2016
!!
!!    MODIFICATIONS
!!    -------------
!       Fuxing Wang   05/2020 Remove IM%I.
!                        Use diagnostics computed from average_diag_misc_isban.F90
!!           
!       
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_ISBA_n ,ONLY : ISBA_PE_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_TYPE_SNOW
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
!
USE YOMHOOK     ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1    ,ONLY : JPRB
!
USE MODI_GET_FRAC_n
USE MODI_GET_LUOUT
USE MODI_GET_1D_MASK
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
CHARACTER(LEN=6),    INTENT(IN)   :: HPROGRAM    
INTEGER,             INTENT(IN)   :: KI          ! number of points
!
REAL, DIMENSION(KI), INTENT(OUT)  :: PNATURE     ! nature fraction

REAL, DIMENSION(KI), INTENT(OUT)  :: PTG2        ! Soil layer 2 Temperature    (K)
REAL, DIMENSION(KI), INTENT(OUT)  :: PSWI1       ! Soil layer 1 SWI
REAL, DIMENSION(KI), INTENT(OUT)  :: PSWI2       ! Soil layer 2 SWI
REAL, DIMENSION(KI), INTENT(OUT)  :: PWGI1       ! Soil layer 1 Ice Content    (m3/m3)
REAL, DIMENSION(KI), INTENT(OUT)  :: PWGI2       ! Soil layer 2 Ice Content    (m3/m3)
REAL, DIMENSION(KI), INTENT(OUT)  :: PWR         ! Water Intercepted           (kg/m2)
REAL, DIMENSION(KI), INTENT(OUT)  :: PSNA        ! Snow albedo
REAL, DIMENSION(KI), INTENT(OUT)  :: PSND        ! Snow layer 1 density        (kg/m2)
REAL, DIMENSION(KI), INTENT(OUT)  :: PHV         ! Halstead coefficient
TYPE(SURF_ATM_t),       INTENT(INOUT) :: U
TYPE(ISBA_OPTIONS_t),   INTENT(IN)    :: O
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DGMI
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(KI)    :: ZFIELD1, ZFIELD2, ZFIELD3, ZFIELD4
INTEGER, DIMENSION(KI) :: IMASK

INTEGER :: INATURE ! dimension of nature tile

INTEGER :: ILUOUT, JI

REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!*   0. Logical unit for writing out
!
IF (LHOOK) CALL DR_HOOK('GET_SURF_VAR_2ISBA',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!*   1. Initializations
!
PTG2(:) = XUNDEF
PSWI1(:) = XUNDEF
PSWI2(:) = XUNDEF
PWGI1(:) = XUNDEF
PWGI2(:) = XUNDEF
PWR(:) = XUNDEF
PSNA(:) = XUNDEF
PSND(:) = XUNDEF
PHV(:) = XUNDEF

!-------------------------------------------------------------------------------
!*   2. Fraction of each tile
!
CALL GET_FRAC_n(U,HPROGRAM, KI, ZFIELD1, ZFIELD2, ZFIELD3, ZFIELD4)
PNATURE = ZFIELD3
!
!-------------------------------------------------------------------------------
!*   3. Soil variables from surfex (nature only)
!
INATURE = COUNT(PNATURE (:) > 0.0)
IMASK(:)=0
CALL GET_1D_MASK(INATURE, KI, PNATURE, IMASK(1:INATURE))

IF(O%CISBA=='DIF') THEN ! DIF case
  DO JI = 1, INATURE
    PTG2(IMASK(JI))  = DGMI%XFRD2_TG(JI)
  
    PSWI1(IMASK(JI)) = DGMI%XSWI(JI,1)
    PSWI2(IMASK(JI)) = DGMI%XFRD2_SWI(JI)
  
    PWGI1(IMASK(JI)) = DGMI%XWGI(JI,1)
    PWGI2(IMASK(JI)) = DGMI%XFRD2_TWGI(JI)
    PWR(IMASK(JI))   = DGMI%XWR(JI)
    PSNA(IMASK(JI))  = DGMI%XSNOWALB(JI)
    PSND(IMASK(JI))  = DGMI%XSNOWRHO(JI)
  ENDDO

ELSE ! Force-restore case
  DO JI = 1, INATURE
    PTG2(IMASK(JI))  = DGMI%XTG(JI,2)

    PSWI1(IMASK(JI)) = DGMI%XSWI(JI,1)
    PSWI2(IMASK(JI)) = DGMI%XSWI(JI,2)

    PWGI1(IMASK(JI)) = DGMI%XWGI(JI,1)
    PWGI2(IMASK(JI)) = DGMI%XWGI(JI,2)
    PWR(IMASK(JI))   = DGMI%XWR(JI)
    PSNA(IMASK(JI))  = DGMI%XSNOWALB(JI)
    PSND(IMASK(JI))  = DGMI%XSNOWRHO(JI)
  ENDDO
ENDIF
!
!==============================================================================
IF (LHOOK) CALL DR_HOOK('GET_SURF_VAR_2ISBA',1,ZHOOK_HANDLE)
END SUBROUTINE GET_SURF_VAR_2ISBA
