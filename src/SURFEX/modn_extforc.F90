!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!!
!!    #####################
      MODULE MODN_EXTFORC
!!    #####################
!!
!!*** *MODN_EXTFORC*
!!
!!    PURPOSE
!!    -------
!       Namelist to enable external surface forcing to be read from a file
!!
!!**  AUTHOR
!!    ------
!!    E. Gleeson                   
!!    ------------------

USE MODD_EXTFORC                           
!!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
IMPLICIT NONE
SAVE
NAMELIST /NAM_EXTFORC/  &
     EXT_FORC, FORCINGFILE

!
END MODULE MODN_EXTFORC
