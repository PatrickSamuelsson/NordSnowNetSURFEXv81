!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!!
!!    #####################
      MODULE MODD_EXTFORC
!!    #####################
!!
!!*** *MODD_EXTFORC*
!!
!!    PURPOSE
!!    -------
!      Read surface forcing from an ASCII file
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
LOGICAL            ::    EXT_FORC=.FALSE.               ! Whether or not external forcing is supplied
CHARACTER(LEN=300) ::    FORCINGFILE='Forcing.dat'      ! File containing the forcings

!
END MODULE MODD_EXTFORC
