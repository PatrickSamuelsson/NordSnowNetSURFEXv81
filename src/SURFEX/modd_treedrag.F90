!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!!
!!    #####################
      MODULE MODD_TREEDRAG
!!    #####################
!!
!!*** *MODD_TREEDRAG*
!!
!!    PURPOSE
!!    -------
!       Declaration to take into account tree drag in the atmospheric model
!              instead of SURFEX. The Z0 forest is therefore reduced to
!              the Z0 grass
!!
!!**  AUTHOR
!!    ------
!!    C.Lac                   *CNRM*
!
!!    MODIFICATIONS
!!    -------------
!!    Original 30/06/11
!!    P. Samuelsson, SMHI  02/2020: Added XALLEN_TERM and XGRASS_H_DNM
!!    S. Viana, AEMET      06/2020: Added LFAKETREE,XHFAKETREE,XFFAKETREE
!!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
IMPLICIT NONE
LOGICAL    ::     LTREEDRAG=.FALSE.    ! flag used to  take into account tree drag in 
!                                      ! the atmospheric model instead of SURFEX.
LOGICAL    ::     LFAKETREE=.FALSE.    ! Flag to activate fake trees option in low vegetation areas.
REAL       ::     XSCALE_H_TREE=1.0    ! Scale the tree height with this factor
REAL       ::     XALLEN_TERM=3.5      ! The term is in the expression for height of crops
REAL       ::     XGRASS_H_DNM=6.0     ! The denominater value is in the expression for height of grass
REAL       ::     XHFAKETREE=10.       ! Height of fake trees 
REAL       ::     XFFAKETREE=0.1       ! Fraction of fake trees 
REAL       ::     XZ0_MIN_LIMIT=0.     ! Limit Z0 (m) in Z0V_FROM_LAI to this minimum value.
REAL       ::     XZ0_MAX_LIMIT=999.   ! Limit Z0 (m) in Z0V_FROM_LAI to this maximum value.
REAL       ::     XFORLAT1=1.0         ! 
REAL       ::     XFORLAT2=0.0         ! 
REAL       ::     XFORFRAC1=1.0        ! 
REAL       ::     XFORFRAC2=0.0        ! 
!
END MODULE MODD_TREEDRAG
