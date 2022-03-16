SUBROUTINE TRIP_DIAG_CPL_ESM (TP, TPG, &
                              PTSTEP_RUN,PDISCHARGE,PCALVING,PWTD,PFWTD)  
!     #################################################################
!
!!****  *TRIP_DIAG_CPL_ESM*  
!!
!!    PURPOSE
!!    -------
!
!     TRIP cpl diag compuation
!     
!!      
!!    AUTHOR
!!    ------
!!      B. Decharme
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    12/12/13 
!!      B. Decharme 10/2016  bug surface/groundwater coupling
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE MODD_TRIP, ONLY : TRIP_t
USE MODD_TRIP_GRID, ONLY : TRIP_GRID_t
!
USE MODD_TRIP_PAR,   ONLY : XUNDEF, XRHOLW
!
USE MODD_TRIP_OASIS, ONLY : LCPL_SEA, LCPL_LAND, LCPL_GW,    &
                            LCPL_FLOOD, LCPL_CALVSEA
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(TRIP_t), INTENT(INOUT) :: TP
TYPE(TRIP_GRID_t), INTENT(INOUT) :: TPG
!
REAL,                 INTENT(IN) :: PTSTEP_RUN !Run  timestep                   [s]
REAL, DIMENSION(:,:), INTENT(IN) :: PDISCHARGE !Cumulated river discharges      [kg]
REAL, DIMENSION(:,:), INTENT(IN) :: PCALVING   !Input claving flux from glacier [kg/s]
REAL, DIMENSION(:,:), INTENT(IN) :: PWTD       !Water table depth               [m]
REAL, DIMENSION(:,:), INTENT(IN) :: PFWTD      !Fraction of Water table to rise
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_DIAG_CPL_ESM',0,ZHOOK_HANDLE)
!
!*       1.      Actualisation of sea coupling diagnostic:
!               ------------------------------------------
!
!
! River discharges to ocean [kg/m2]
!
IF(LCPL_SEA)THEN
  WHERE(TPG%NGRCN(:,:)==9.OR.TPG%NGRCN(:,:)==12)
    TP%XCPL_RIVDIS(:,:) = TP%XCPL_RIVDIS(:,:) + PDISCHARGE(:,:) / TPG%XAREA(:,:)
  ENDWHERE
ENDIF
!
! Calving flux over greenland and antarctica [kg/m2]
!
IF(LCPL_CALVSEA)THEN
  WHERE(TPG%GMASK_GRE(:,:))
    TP%XCPL_CALVGRE(:,:) = TP%XCPL_CALVGRE(:,:) + PCALVING(:,:) * PTSTEP_RUN / TPG%XAREA(:,:)
  ENDWHERE
  WHERE(TPG%GMASK_ANT(:,:))
    TP%XCPL_CALVANT(:,:) = TP%XCPL_CALVANT(:,:) + PCALVING(:,:) * PTSTEP_RUN / TPG%XAREA(:,:)
  ENDWHERE
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       2.      Actualisation of land coupling diagnostic:
!               -------------------------------------------
!
IF(LCPL_LAND)THEN
!
! Water table depth (negative above the surface) and fraction of water table to rise
!
  IF(LCPL_GW)THEN
    WHERE(TPG%GMASK_GW(:,:))
          TP%XCPL_WTD (:,:) = PWTD (:,:)
          TP%XCPL_FWTD(:,:) = PFWTD(:,:)
    ELSEWHERE(TPG%GMASK(:,:))
          TP%XCPL_WTD (:,:) = XUNDEF
          TP%XCPL_FWTD(:,:) = 0.0
    ENDWHERE
  ENDIF
!
! Flood fraction [-] and potential infiltration [kg/m2]
!       
  IF(LCPL_FLOOD)THEN
    TP%XCPL_FFLOOD (:,:) = TP%XFFLOOD    (:,:)
    TP%XCPL_PIFLOOD(:,:) = TP%XFLOOD_STO (:,:) / TPG%XAREA(:,:)             
  ENDIF
!  
ENDIF
!
IF (LHOOK) CALL DR_HOOK('TRIP_DIAG_CPL_ESM',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE TRIP_DIAG_CPL_ESM
