!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!!
!!    #####################
      MODULE MODN_TREEDRAG
!!    #####################
!!
!!*** *MODN_TREEDRAG*
!!
!!    PURPOSE
!!    -------
!       Namelist to take into account tree drag in the atmospheric model
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
!!    P. Samuelsson, SMHI  02/2020: Added XALLEN_TERM, XGRASS_H_DNM
!!    S. Viana, AEMET      06/2020: Added LFAKETREE,XHFAKETREE,XFFAKETREE
!!    IMPLICIT ARGUMENTS
!!    ------------------
USE MODD_TREEDRAG                           
!!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
IMPLICIT NONE
SAVE
NAMELIST /NAM_TREEDRAG/  &
     LTREEDRAG, LFAKETREE, XSCALE_H_TREE, XALLEN_TERM, XGRASS_H_DNM, XHFAKETREE, XFFAKETREE, & 
     XZ0_MIN_LIMIT, XZ0_MAX_LIMIT, &
     XALLEN_TERM, XGRASS_H_DNM, &
     XFORLAT1, XFORLAT2, XFORFRAC1, XFORFRAC2

!
END MODULE MODN_TREEDRAG
