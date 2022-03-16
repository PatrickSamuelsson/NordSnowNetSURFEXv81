!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_SEAICE_n (G, S, HPROGRAM,KLU,KLUOUT)
!     #########################################
!
!!****  *READ_SEAICE_n* - read seaice scheme variables
!!
!!
!!    PURPOSE : feed seaice scheme state variable and 'domain' structure
!!    -------
!!
!!**  METHOD : 
!!    -------
!!      For now, only Gelato model is handled
!!
!!      for state variable : quite standard in Surfex : use READ_SURF with 
!!         relevant field names (same names as in genuine gelato restarts)
!!      for domain information : copy from MODD_SEAFLUX_GRID
!!      for bathymetry : copy from MODD_SEAFLUX
!!
!!    EXTERNALS : READ_SURF, GLTOOLS_ALLOC, GET_TYPE_DIM, ABOR1_SFX
!!    --------
!!
!!    IMPLICIT ARGUMENTS : Gelato state variable, and a few namelist parameters
!!    ------------------
!!
!!    REFERENCE : routine restartr in original Gelato sources (V6.0.20)
!!    ---------
!!
!!    AUTHOR : S. Sénési   *Meteo France*	
!!    ------
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2014
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
!
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
!
USE MODD_CSTS, ONLY           : XPI, XTTSI, XTT
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_SFX_OASIS,      ONLY : LCPL_SEAICE
USE MODD_WATER_PAR,      ONLY : XALBSEAICE
!
USE MODI_READ_SURF
USE MODI_INTERPOL_SST_MTH
!
USE MODI_GET_LUOUT
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
!
!
TYPE(GRID_t), INTENT(INOUT) :: G
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
!
CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
INTEGER,           INTENT(IN)  :: KLU      ! number of sea patch point
INTEGER,           INTENT(IN)  :: KLUOUT
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! Error code after reading
!
INTEGER           :: JMTH, INMTH
CHARACTER(LEN=2 ) :: YMTH
CHARACTER(LEN=5)  :: YLVL
!
CHARACTER(LEN=12) :: YCATEG         ! category to read
CHARACTER(LEN=12) :: YLEVEL         ! Level to read
CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
CHARACTER(LEN=200) :: YMESS         ! Error Message
!
INTEGER :: JX,JK,JL                 ! loop counter on ice categories and layers and grid points
INTEGER :: inl_in_file,int_in_file  ! file values for ice catgories and layers numbers
REAL :: ZFSIT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('READ_SEAICE_n',0,ZHOOK_HANDLE)
!
IF (.NOT.S%LHANDLE_SIC) THEN 
   ALLOCATE(S%XSIC(0))
   IF (LHOOK) CALL DR_HOOK('READ_SEAICE_n',1,ZHOOK_HANDLE)
   RETURN
ENDIF
!
ALLOCATE(S%XSIC(KLU))
S%XSIC(:)=XUNDEF
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     Dealing with external sea-ice cover data (either for nudging or forcing)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
IF(S%LVOLATILE_SIC .OR. S%LINTERPOL_SIC) THEN
   ALLOCATE(S%XFSIC(KLU))
   S%XFSIC = XUNDEF
ELSE
   ALLOCATE(S%XFSIC(0))
END IF

IF(S%LINTERPOL_SIC)THEN
   !
   !Precedent, Current, Next, and Second-next Monthly SIC
   INMTH=4   
   !
   ALLOCATE(S%XSIC_MTH(KLU,INMTH))
   DO JMTH=1,INMTH
      WRITE(YMTH,'(I2)') (JMTH-1)
      YRECFM='SIC_MTH'//ADJUSTL(YMTH(:LEN_TRIM(YMTH)))
      CALL READ_SURF(HPROGRAM,YRECFM,S%XSIC_MTH(:,JMTH),IRESP)
      CALL CHECK_SEAICE(YRECFM,S%XSIC_MTH(:,JMTH))
   ENDDO
   !
   CALL INTERPOL_SST_MTH(S,'C')
   !
   IF (ANY(S%XFSIC(:)>1.0).OR.ANY(S%XFSIC(:)<0.0)) THEN
     CALL ABOR1_SFX('READ_SEAICE_n: FSIC should be >=0 and <=1') 
   ENDIF                 
   !
ELSE
   ! 
   ALLOCATE(S%XSIC_MTH(0,0))
   !
ENDIF
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                             Worrying about a seaice scheme 
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
IF (TRIM(S%CSEAICE_SCHEME) == 'NONE' ) THEN
   IF (S%LINTERPOL_SIC ) THEN
      S%XTICE=S%XSST
      S%XSIC=S%XFSIC
      S%XICE_ALB=XALBSEAICE           
      IF (LHOOK) CALL DR_HOOK('READ_SEAICE_n',1,ZHOOK_HANDLE)
      RETURN
   ELSE
      CALL ABOR1_SFX("READ_SEAICE_n: MUST HAVE CINTERPOL_SIC /= NONE WITH CSEAICE_SCHEME == NONE ") 
   ENDIF
ELSE
   IF (TRIM(S%CSEAICE_SCHEME) /= 'GELATO' .AND. TRIM(S%CSEAICE_SCHEME) /= 'SICE') THEN
      WRITE(KLUOUT,*)'READ_SEAICE_n:CSEAICE_SCHEME read in PREP, ',S%CSEAICE_SCHEME,', is not yet handled'
      CALL ABOR1_SFX("READ_SEAICE_n:CAN ONLY HANDLE GELATO SEAICE MODEL YET (and not the one quoted in PREP)") 
   ENDIF
ENDIF
!
!* Sea ice thickness nudging data
!
IF(S%LINTERPOL_SIT)THEN
   !
   ALLOCATE(S%XFSIT(KLU))
   !
   !Precedent, Current, Next, and Second-next Monthly SIT
   INMTH=4   
   !
   ALLOCATE(S%XSIT_MTH(KLU,INMTH))
   DO JMTH=1,INMTH
      WRITE(YMTH,'(I2)') (JMTH-1)
      YRECFM='SIT_MTH'//ADJUSTL(YMTH(:LEN_TRIM(YMTH)))
      CALL READ_SURF(HPROGRAM,YRECFM,S%XSIT_MTH(:,JMTH),IRESP)
      CALL CHECK_SEAICE(YRECFM,S%XSIT_MTH(:,JMTH))
   ENDDO
   !
   CALL INTERPOL_SST_MTH(S,'H')
   !
ELSE
   !
   ALLOCATE(S%XFSIT(0))
   ALLOCATE(S%XSIT_MTH(0,0))
   !
ENDIF

CALL S%ICE%BIND_INPUTS(S%XSEABATHY, S%XSST, S%XSSS)
CALL S%ICE%READSURF(G, HPROGRAM, KLU, KLUOUT)
CALL S%ICE%SET_DAMPING(S%XSIC_EFOLDING_TIME, S%XSIT_EFOLDING_TIME)
CALL S%ICE%GET_RESPONSE(S%XSIC, S%XTICE, S%XICE_ALB)
!
IF (LHOOK) CALL DR_HOOK('READ_SEAICE_n',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
CONTAINS
!-------------------------------------------------------------------------------
!
SUBROUTINE CHECK_SEAICE(HFIELD,PFIELD)
!
!
IMPLICIT NONE
!
CHARACTER(LEN=12),  INTENT(IN) :: HFIELD
REAL, DIMENSION(:), INTENT(IN) :: PFIELD
!
REAL            :: ZMAX,ZMIN
INTEGER         :: JI, IERRC
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('READ_SEAICE_n:CHECK_SEAICE',0,ZHOOK_HANDLE)
!
ZMIN=-1.0E10
ZMAX=1.0E10
!
IERRC=0
!
DO JI=1,KLU
   IF(PFIELD(JI)>ZMAX.OR.PFIELD(JI)<ZMIN)THEN
      IERRC=IERRC+1
      WRITE(KLUOUT,*)'PROBLEM FIELD '//TRIM(HFIELD)//' =',PFIELD(JI),&
                     'NOT REALISTIC AT LOCATION (LAT/LON)',G%XLAT(JI),G%XLON(JI)
   ENDIF
ENDDO
!         
IF(IERRC>0) CALL ABOR1_SFX('READ_SEAICE_n: FIELD '//TRIM(HFIELD)//' NOT REALISTIC')
!
IF (LHOOK) CALL DR_HOOK('READ_SEAICE_n:CHECK_SEAICE',1,ZHOOK_HANDLE)

END SUBROUTINE CHECK_SEAICE
!
!------------------------------------------------------------------------------
!
END SUBROUTINE READ_SEAICE_n
