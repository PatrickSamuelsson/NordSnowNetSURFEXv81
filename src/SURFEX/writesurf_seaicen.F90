!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_SEAICE_n (HSELECT, S, HPROGRAM)
!     #########################################
!
!!****  *WRITESURF_SEAICE_n* - write seaice scheme variables
!!
!!
!!    PURPOSE : writes state variable and 'domain' structure
!!    -------
!!
!!**  METHOD : 
!!    ------
!!      For now, only Gelato scheme is handled
!!
!!      quite standard in Surfex : use WRITE_SURF with 
!!         relevant field names (same names as in genuine gelato restarts)
!!
!!    EXTERNALS : WRITE_SURF, GLT_ALLOC, GET_TYPE_DIM
!!    --------
!!
!!    IMPLICIT ARGUMENTS : Gelato state variable, and some namelist parameters
!!    ------------------
!!
!!    REFERENCE : 
!!    ---------
!!
!!    AUTHOR : S. Sénési   *Meteo France*
!!    ------
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2014
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
!
USE MODI_WRITE_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_TYPE_DIM_n
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT
!
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP           ! Error code after reading
!
INTEGER           :: JMTH, INMTH
CHARACTER(LEN=2 ) :: YMTH
CHARACTER(LEN=5)  :: YLVL
!
CHARACTER(LEN=6)  :: YICECAT
CHARACTER(LEN=20) :: YFORM
CHARACTER(LEN=12) :: YRECFM           ! Name of the article to be read
CHARACTER(LEN=12) :: YCATEG           ! Category to write
CHARACTER(LEN=12) :: YLEVEL           ! Level to write
CHARACTER(LEN=100):: YCOMMENT         ! Error Message
!
INTEGER :: JK,JL                   ! loop counter on ice categories and layes 
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('WRITESURF_SEAICE_n',0,ZHOOK_HANDLE)
!
!
CALL S%ICE%WRITESURF(HSELECT, HPROGRAM)
!
!
!-------------------------------------------------------------------------------
!
!* sea ice cover
!
IF(S%LINTERPOL_SIC)THEN
!
   INMTH=SIZE(S%XSIC_MTH,2)
!
   DO JMTH=1,INMTH
      WRITE(YMTH,'(I2)') (JMTH-1)
      YRECFM='SIC_MTH'//ADJUSTL(YMTH(:LEN_TRIM(YMTH)))
      YCOMMENT='Sea ice coverage at month t'//ADJUSTL(YMTH(:LEN_TRIM(YMTH)))
      CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,S%XSIC_MTH(:,JMTH),IRESP,HCOMMENT=YCOMMENT)
   ENDDO
!
ENDIF
!
YRECFM='SIC'
YCOMMENT='Sea ice coverage'
CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,S%XSIC(:),IRESP,HCOMMENT=YCOMMENT)  
!
!
!* sea ice thickness constraint
!
IF(S%LINTERPOL_SIT)THEN
!
   INMTH=SIZE(S%XSIT_MTH,2)
!
   DO JMTH=1,INMTH
      WRITE(YMTH,'(I2)') (JMTH-1)
      YRECFM='SIT_MTH'//ADJUSTL(YMTH(:LEN_TRIM(YMTH)))
      YCOMMENT='Sea ice thickness constraint at month t'//ADJUSTL(YMTH(:LEN_TRIM(YMTH)))
      CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,S%XSIT_MTH(:,JMTH),IRESP,HCOMMENT=YCOMMENT)
   ENDDO
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_SEAICE_n',1,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------
END SUBROUTINE WRITESURF_SEAICE_n
