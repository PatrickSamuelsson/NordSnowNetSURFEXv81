!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!!
!!    #####################
      MODULE MODN_MUSCEXT
!!    #####################
!!
!!*** *MODN_MUSCEXT*
!!
!!    PURPOSE
!!    -------
!       Namelist to enable SST surface forcing to be read from a file
!       for MUSC
!!
!!**  AUTHOR
!!    ------
!!    E. Gleeson                   
!!    ------------------

USE MODD_MUSCEXT                           
!!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
IMPLICIT NONE
SAVE
NAMELIST /NAM_MUSCEXT/  &
     SST_FORC, FORCINGFILE

!
END MODULE MODN_MUSCEXT
