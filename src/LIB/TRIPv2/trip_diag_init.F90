SUBROUTINE TRIP_DIAG_INIT(PSOUT,PSIN,PVEL,PHS,PGOUT,PGNEG,PHG_OLD,   &
                          PWTD,PFWTD,PQGCELL,PHGHS,                  &
                          PQFR,PQRF,PVFIN,PVFOUT,PHSF,PDISCHARGE,    &
                          PGSTO_ALL,PGSTO2_ALL,PGIN_ALL,PGOUT_ALL    )  
!     #####################################################
!
!!****  *TRIP_DIAG_INIT*  
!!
!!    PURPOSE
!!    -------
!
!     TRIP diag compuation
!     
!!      
!!    AUTHOR
!!    ------
!!      B. Decharme
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    12/12/13 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODN_TRIP_RUN,  ONLY : LDIAG_MISC
USE MODN_TRIP,      ONLY : CGROUNDW, CVIT, LFLOOD
USE MODD_TRIP_PAR,  ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(OUT) :: PSOUT      !streamflow                    [kg/s]
REAL, DIMENSION(:,:), INTENT(OUT) :: PSIN       !grid-cell input streamflow    [kg/s]
REAL, DIMENSION(:,:), INTENT(OUT) :: PVEL       !river velocity                [m/s]
REAL, DIMENSION(:,:), INTENT(OUT) :: PHS        !River heigh                   [m]
REAL, DIMENSION(:,:), INTENT(OUT) :: PGOUT      !Groundwater outflow           [kg/s]
REAL, DIMENSION(:,:), INTENT(OUT) :: PGNEG      !Groundwater intflow (neg)     [kg/s]
REAL, DIMENSION(:,:), INTENT(OUT) :: PWTD       !Water table depth for coupling[m]
REAL, DIMENSION(:,:), INTENT(OUT) :: PFWTD      !Fraction of water table to rise
REAL, DIMENSION(:,:), INTENT(OUT) :: PQGCELL    !lateral groundwater exchanges [kg/s]
REAL, DIMENSION(:,:), INTENT(OUT) :: PHGHS      !groundwater minus river heigh [m]
REAL, DIMENSION(:,:), INTENT(OUT) :: PQFR       !floodplains to river exchange [kg/s]
REAL, DIMENSION(:,:), INTENT(OUT) :: PQRF       !river to floodplains exchange [kg/s]
REAL, DIMENSION(:,:), INTENT(OUT) :: PVFIN      !QRF velocity                  [m/s]
REAL, DIMENSION(:,:), INTENT(OUT) :: PVFOUT     !QFR velocity                  [m/s]
REAL, DIMENSION(:,:), INTENT(OUT) :: PHSF       !river minus flodd heigh       [m]
REAL, DIMENSION(:,:), INTENT(OUT) :: PDISCHARGE !Cumulated river discharges    [kg]
REAL, DIMENSION(:,:), INTENT(OUT) :: PHG_OLD    ! Water table depth before     
!                                                 water mass conservation      [m]
!
REAL                , INTENT(OUT) :: PGSTO_ALL  !Global groundwater storage at t    [kg]
REAL                , INTENT(OUT) :: PGSTO2_ALL !Global groundwater storage at t-1  [kg]
REAL                , INTENT(OUT) :: PGIN_ALL   !Global gw recharge + lateral input [kg/m2/s]
REAL                , INTENT(OUT) :: PGOUT_ALL  !Global gw outflow                  [kg/m2/s]
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('TRIP_DIAG_INIT',0,ZHOOK_HANDLE)
!
!
PDISCHARGE(:,:) = 0.0
PSOUT     (:,:) = 0.0
PSIN      (:,:) = 0.0
PGOUT     (:,:) = 0.0
PGNEG     (:,:) = 0.0
!
PVEL      (:,:) = XUNDEF        
PHS       (:,:) = XUNDEF    
PQFR      (:,:) = XUNDEF  
PQRF      (:,:) = XUNDEF
PVFIN     (:,:) = XUNDEF
PVFOUT    (:,:) = XUNDEF
PHSF      (:,:) = XUNDEF
PQGCELL   (:,:) = XUNDEF
PWTD      (:,:) = XUNDEF
PFWTD     (:,:) = XUNDEF
PHGHS     (:,:) = XUNDEF
PHG_OLD   (:,:) = XUNDEF
!
IF(CGROUNDW=='DIF')THEN
   IF(LDIAG_MISC)THEN
     PQGCELL(:,:) = 0.0
     PHGHS  (:,:) = 0.0
   ENDIF
ENDIF
!
IF(CVIT=='VAR')THEN
  PVEL(:,:) = 0.0
  PHS (:,:) = 0.0
ENDIF
!
IF(LFLOOD)THEN 
  PQFR  (:,:) = 0.0
  PQRF  (:,:) = 0.0
  PVFIN (:,:) = 0.0
  PVFOUT(:,:) = 0.0
  PHSF  (:,:) = 0.0
ENDIF
!
PGSTO_ALL  = XUNDEF
PGSTO2_ALL = XUNDEF
PGIN_ALL   = XUNDEF
PGOUT_ALL  = XUNDEF
!
!
IF (LHOOK) CALL DR_HOOK('TRIP_DIAG_INIT',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE TRIP_DIAG_INIT
