!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!!
!!    #####################
      MODULE MODD_MUSCEXT
!!    #####################
!!
!!*** *MODD_MUSCEXT*
!!
!!    PURPOSE
!!    -------
!      Read SST surface forcing from a file for use in MUSC 
!!
!!**  AUTHOR
!!    ------
!!    E. Gleeson
!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
IMPLICIT NONE
LOGICAL            ::    SST_FORC=.FALSE.                                  ! Whether or not SST forcing is supplied
CHARACTER(LEN=300) ::    FORCINGFILE='/home/musctest/SST_forcing.dat'      ! File containing the forcings

!
END MODULE MODD_MUSCEXT
