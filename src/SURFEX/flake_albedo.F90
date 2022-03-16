!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE FLAKE_ALBEDO( KI, PDIR_SW   , PSCA_SW , KSW,      &
                               PDIR_ALB  , PSCA_ALB,           &
                               PGLOBAL_SW, PALB                )
!     ##########################################################################
!
!!****  *FLAKE_ALBEDO*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates  albedo and emissivity 
!         
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!      
!!    AUTHOR
!!    ------
!!
!!      P. Le Moigne           * Meteo-France *
!!
!!      Modified by P. Le Moigne - 10/2013 : bug in ZSW_UP declaration
!!               by E. Kourzeneva (FMI) - 10/2016 : Introduce explicit arrays (othervise problems with arrays of zero length)
!!                                                  Introduce lower limit of SW radiation, to avoid a random rezult
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE MODD_SURF_PAR,     ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER,              INTENT(IN)   :: KI                 ! number of points
INTEGER,              INTENT(IN)   :: KSW                ! number of short-wave spectral bands

REAL, DIMENSION(KI,KSW), INTENT(IN)   :: PDIR_SW            ! direct incoming solar radiation
REAL, DIMENSION(KI,KSW), INTENT(IN)   :: PSCA_SW            ! diffuse incoming solar radiation
REAL, DIMENSION(KI,KSW), INTENT(IN)   :: PDIR_ALB           ! direct  albedo
REAL, DIMENSION(KI,KSW), INTENT(IN)   :: PSCA_ALB           ! diffuse albedo

REAL, DIMENSION(KI)  , INTENT(OUT)  :: PGLOBAL_SW         ! global incoming SW rad.
REAL, DIMENSION(KI)  , INTENT(OUT)  :: PALB               ! albedo
!
!-------------------------------------------------------------------------------
!
!*      0.     Local variables
!              ---------------
!
INTEGER                          :: JSWB, JI
!ek_beg
!REAL, DIMENSION(SIZE(PDIR_SW,1)) :: ZSW_UP
REAL :: ZSW_UP ! ek: with explicit loops, it may be just a scalar
!ek_end
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*      1.     surface albedo for each wavelength
!              ----------------------------------
!
IF (LHOOK) CALL DR_HOOK('FLAKE_ALBEDO',0,ZHOOK_HANDLE)
!
  DO JI=1,KI

!* total shortwave incoming radiation

    PGLOBAL_SW(JI) = 0.

    DO JSWB=1,KSW
      PGLOBAL_SW(JI) = PGLOBAL_SW(JI) + (PDIR_SW(JI,JSWB) + PSCA_SW(JI,JSWB))
    END DO
!
!* total shortwave upcoming radiation
! 
    ZSW_UP = 0.
    DO JSWB=1,KSW
      ZSW_UP =  ZSW_UP                               &
              + PDIR_ALB(JI,JSWB) * PDIR_SW(JI,JSWB) &
              + PSCA_ALB(JI,JSWB) * PSCA_SW(JI,JSWB)
    END DO

!
!* global albedo
!
!ek: here use 0.1 W/m**2 instead of zero, to avoid a random result
    IF(PGLOBAL_SW(JI).GT.0.1) THEN
      PALB(JI) = ZSW_UP / PGLOBAL_SW(JI)
    ELSE
      PALB(JI) = PDIR_ALB(JI,1)
    END IF

  END DO

!
IF (LHOOK) CALL DR_HOOK('FLAKE_ALBEDO',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE FLAKE_ALBEDO
