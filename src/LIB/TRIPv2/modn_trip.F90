!#######################
MODULE  MODN_TRIP
!#######################
!
!!****  *MODN_TRIP* define the variables and namelist for TRIP
!                       driver programs
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/2013
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
IMPLICIT NONE
!
!Stream flow velocity scheme
!
 CHARACTER(LEN=3) :: CVIT  = 'DEF'     !Type of stream flow velocity
                                      !'DEF' = Constant velocit = 0.5m/s
                                      !'VAR' = variable velocity
!
REAL             :: XCVEL = 0.5       ! Constant velocity value
!
!Groundwater scheme
!
 CHARACTER(LEN=3) :: CGROUNDW = 'DEF'  !Use groundwater scheme
                                      !'DEF' = No groundwater scheme
                                      !'CST' = Constant transfert time
                                      !'DIF' = Groundwater diffusive scheme 
!
LOGICAL          :: LGWSUBF  = .TRUE. !Use sub-grid fraction to couple with SURFEX
                                      !as in Verges et al., JGR, 2014
!
REAL             :: XGWSUBD  = 0.0    !Sub-grid depth uses to adjust the WTD 
                                      !used to compute the sub-grid fraction
!
!Floodplains scheme
!                                     
LOGICAL          :: LFLOOD = .FALSE.  !if true, use TRIP-FLOOD
!
!Other attributes
!
REAL             :: XRATMED     = 1.4  ! Meandering ratio
REAL             :: XTSTEP      = 3600.
!
!-------------------------------------------------------------------------------
!
!*       1.    NAMELISTS
!              ---------
!
NAMELIST/NAM_TRIP/CVIT,CGROUNDW,LGWSUBF,XGWSUBD, &
                  LFLOOD,XCVEL,XRATMED,XTSTEP
!
!-------------------------------------------------------------------------------
END MODULE MODN_TRIP
