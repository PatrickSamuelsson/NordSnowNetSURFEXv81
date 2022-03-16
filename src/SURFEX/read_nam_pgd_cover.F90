!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_NAM_PGD_COVER(HPROGRAM)  
!     ##############################################################
!
!!**** *READ_NAM_PGD_COVER* reads namelist for Cover
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    B. Decharme        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    02/2010
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_DATA_COVER_PAR, ONLY : NCOVER, &
     YCOVER, YCOVERFILETYPE, XUNIF_COVER, &
     XRM_COVER, XRM_COAST, XRM_LAKE, &
     XRM_SEA, XRM_WM, &
     LORCA_GRID, XLAT_ANT, &
     LUNIF_COVER, LIMP_COVER, LRM_RIVER            
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
!
USE MODE_POS_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!                                   
CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM    ! Type of program
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                           :: ILUOUT    ! output listing logical unit
INTEGER                           :: ILUNAM    ! namelist file logical unit
LOGICAL                           :: GFOUND    ! flag when namelist is present
!
!*    0.3    Declaration of namelists
!            ------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
NAMELIST/NAM_COVER/ YCOVER, YCOVERFILETYPE, XUNIF_COVER, XRM_COVER, XRM_COAST,     &
                    XRM_LAKE, LRM_RIVER, XRM_SEA, XRM_WM, LORCA_GRID, XLAT_ANT,    &
                    LIMP_COVER 
!
!-------------------------------------------------------------------------------
!
!*    1.      Initializations of defaults
!             ---------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_NAM_PGD_COVER',0,ZHOOK_HANDLE)

ALLOCATE(XUNIF_COVER(NCOVER))

XUNIF_COVER(:) = 0.
YCOVER         = '                          '
YCOVERFILETYPE = '      '
XRM_COVER      = 1.E-6
XRM_COAST      = 1.0
XRM_LAKE       = 0.0
LRM_RIVER      = .FALSE.
XRM_SEA        = 0.0
XRM_WM         = 0.0
!
LORCA_GRID     = .FALSE.
XLAT_ANT       = -77.0
!
LIMP_COVER     = .FALSE.
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*    2.      Reading of namelist
!             -------------------
!
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL POSNAM(ILUNAM,'NAM_COVER',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_COVER)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
!-------------------------------------------------------------------------------
!

IF (ANY(XUNIF_COVER/=0.)) THEN
   LUNIF_COVER=.TRUE.
ELSE
   LUNIF_COVER=.FALSE.
   DEALLOCATE(XUNIF_COVER)
END IF
 
IF (LHOOK) CALL DR_HOOK('READ_NAM_PGD_COVER',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_NAM_PGD_COVER
