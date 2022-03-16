!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_FLAKE (DTCO, USS, FG, F, SB, UG, U, GCP, &
                       HPROGRAM,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,YDCTL)
!     #################################################################################
!
!!****  *PREP_FLAKE* - prepares FLAKE fields
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     S. Malardel
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      S. Riette   06/2009 PREP_FLAKE_SBL has no more argument
!!      E. Kourzeneva 09/2010 (i)  Change the default initialisation,
!!                            (ii) Include the possibility to use 
!!                                 lake climate data
!!      P. Marguinaud10/2014, Support for a 2-part PREP
!!------------------------------------------------------------------
!
!
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_FLAKE_n, ONLY : FLAKE_t
USE MODD_CANOPY_n, ONLY : CANOPY_t
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODI_PREP_HOR_FLAKE_FIELD
USE MODI_PREP_VER_FLAKE
USE MODI_PREP_SBL
USE MODI_PREP_OUTPUT_GRID
USE MODI_GET_LUOUT
USE MODI_CLI_LAKE
!
USE MODN_PREP_FLAKE
!
USE MODD_READ_NAMELIST,ONLY : LNAM_READ
USE MODD_SURF_ATM,     ONLY : LVERTSHIFT
USE MODD_PREP,         ONLY : XZS_LS
USE MODD_PREP_FLAKE,   ONLY : LCLIM_LAKE
USE MODD_SURF_PAR,     ONLY : XUNDEF
!
!USE MODD_CSTS,       ONLY : XTT
USE modd_flake_parameters, ONLY : &
  tpl_T_f,  & ! Fresh water freezing point [K]
  tpl_T_r,  &! Temperature of maximum density of fresh water [K]
  C_T_min,  &! Minimum value of the shape factor C_T (thermocline)
  C_T_max,  &! Maximum value of the shape factor C_T (thermocline)
  h_ML_min_flk,  &! Minimum mixed-layer depth [m]
  h_Snow_min_flk,  &! Minimum snow thickness [m]
  h_Ice_min_flk,  &! Minimum ice thickness [m]
  H_Ice_max        ! Maximum ice tickness in  
USE MODE_PREP_CTL, ONLY : PREP_CTL, PREP_CTL_CAN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_CLEAN_PREP_OUTPUT_GRID
!
USE MODI_ABOR1_SFX

IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SSO_t), INTENT(INOUT) :: USS
!
TYPE(GRID_t), INTENT(INOUT) :: FG
TYPE(FLAKE_t), INTENT(INOUT) :: F
TYPE(CANOPY_t), INTENT(INOUT) :: SB
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
TYPE (PREP_CTL),    INTENT(INOUT) :: YDCTL
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=28),  INTENT(IN)  :: HATMFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HATMFILETYPE! type of the Atmospheric file
 CHARACTER(LEN=28),  INTENT(IN)  :: HPGDFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HPGDFILETYPE! type of the Atmospheric file
!
!*      0.2    declarations of local variables
!
INTEGER :: ISIZE
INTEGER :: ILUOUT
CHARACTER(LEN=6)              :: YFILETYPE ! type of input file
CHARACTER(LEN=28)             :: YFILE     ! name of file
CHARACTER(LEN=6)              :: YFILEPGDTYPE ! type of input file
CHARACTER(LEN=28)             :: YFILEPGD     ! name of file
LOGICAL, DIMENSION(10)  :: GUNIF  ! if all lake fields except TS and T_MNW are given as unified
!LOGICAL, DIMENSION(11) :: GFIELD ! if all lake fields except TS are given from the file
LOGICAL :: GUNIF_TS   ! if TS field is given as unified
LOGICAL :: GUNIF_ZS   ! if ZS field is given as unified
LOGICAL :: GINTERP    ! if the field needs horizontal interpolation
LOGICAL :: GINTERP_ZS ! if the ZS field needs horizontal interpolation
!LOGICAL :: GFIELD_TS  !  if TS field is given from the file
LOGICAL :: GFILE      ! if file name exists in the namelist
!LOGICAL :: GSAME_GRID ! if flake grid is the same as the grid in the file to read 
LOGICAL :: GUNIF_G    ! if in general we use unified fields
LOGICAL :: GPROFILE   ! if there is possible to give a temperature profile for a stratified lake (.TRUE.) 
                      ! or just mixed lakes are possible 
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
!*      1.     Default of configuration
!
!
IF (LHOOK) CALL DR_HOOK('PREP_FLAKE',0,ZHOOK_HANDLE)
!
IF (.NOT. PREP_CTL_CAN (YDCTL)) THEN
  CALL ABOR1_SFX('PREP_FLAKE: TWO STEP PREP NOT IMPLEMENTED')
ENDIF

 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL PREP_OUTPUT_GRID(UG%G, FG, U%NSIZE_FULL, ILUOUT)
!
ISIZE = SIZE(FG%XLAT)
!
!-------------------------------------------------------------------------------------
!
!*      2.     Reading and horizontal interpolations
!
IF (.NOT.LCLIM_LAKE) THEN

!              First reading the configuration and defining the setup
  CALL READ_PREP_FLAKE_CONF(HPROGRAM,'TS     ',YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,&
                            HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,ILUOUT,GUNIF_TS)
  GFILE = YFILETYPE/='      '
  CALL READ_PREP_FLAKE_CONF(HPROGRAM,'T_SNOW ',YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,&
                            HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,ILUOUT,GUNIF(1))
  CALL READ_PREP_FLAKE_CONF(HPROGRAM,'T_ICE  ',YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,&
                          HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,ILUOUT,GUNIF(2))
  CALL READ_PREP_FLAKE_CONF(HPROGRAM,'T_WML  ',YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,&
                          HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,ILUOUT,GUNIF(3))
  CALL READ_PREP_FLAKE_CONF(HPROGRAM,'T_BOT  ',YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,&
                          HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,ILUOUT,GUNIF(4))
  CALL READ_PREP_FLAKE_CONF(HPROGRAM,'T_B1   ',YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,&
                          HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,ILUOUT,GUNIF(5))
  CALL READ_PREP_FLAKE_CONF(HPROGRAM,'CT     ',YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,&
                          HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,ILUOUT,GUNIF(6))
  CALL READ_PREP_FLAKE_CONF(HPROGRAM,'H_SNOW ',YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,&
                          HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,ILUOUT,GUNIF(7))
  CALL READ_PREP_FLAKE_CONF(HPROGRAM,'H_ICE  ',YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,&
                          HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,ILUOUT,GUNIF(8))
  CALL READ_PREP_FLAKE_CONF(HPROGRAM,'H_ML   ',YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,&
                          HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,ILUOUT,GUNIF(9))
  CALL READ_PREP_FLAKE_CONF(HPROGRAM,'H_B1   ',YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,&
                          HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,ILUOUT,GUNIF(10))
  CALL READ_PREP_FLAKE_CONF(HPROGRAM,'ZS     ',YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,&
                          HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,ILUOUT,GUNIF_ZS)
  IF(ALL(GUNIF)) THEN
    GUNIF_G = .TRUE.
    GPROFILE = .TRUE.
    WRITE(ILUOUT,*) "WARNING! The unified profile for lakes is taken from the namelist."
    WRITE(ILUOUT,*) "WARNING! The profile will be checked for the constrains, some values might be changed!"   
    WRITE(ILUOUT,*) "WARNING! TS will be defined from the profile, might be changed!"    
  ELSE
    IF(GUNIF_TS) THEN
      GUNIF_G = .TRUE.
      GPROFILE = .FALSE.
      WRITE(ILUOUT,*) "WARNING! At least one of lake profile variables was not indicated, so set the mixed profile!"  
    ELSE
      IF(GFILE) THEN
        GUNIF_G = .FALSE.
        GPROFILE = .TRUE.
      ELSE
        CALL ABOR1_SFX('PREP_FLAKE: SOME OF LAKE VARIABLES IS MISSING, TS IS ALSO MISSING, FILE NAME IS NOT GIVEN!')
      END IF
    END IF
  END IF

!
!*      2.0    Large scale orography
!
 CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, GCP, ISIZE, F, &
                           HPROGRAM,'ZS     ',YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,GUNIF_ZS,GINTERP_ZS)
!
!*      2.1    FLake variables
!
!
 CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, GCP, ISIZE, F, &
                           HPROGRAM,'TS     ',YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,GUNIF_G,GINTERP)
 CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, GCP, ISIZE, F, &
                           HPROGRAM,'T_SNOW ',YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,GUNIF_G,GINTERP)
 CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, GCP, ISIZE, F, &
                           HPROGRAM,'T_ICE  ',YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,GUNIF_G,GINTERP)
 CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, GCP, ISIZE, F, &
                           HPROGRAM,'T_WML  ',YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,GUNIF_G,GINTERP)
 CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, GCP, ISIZE, F, &
                           HPROGRAM,'T_BOT  ',YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,GUNIF_G,GINTERP)
 CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, GCP, ISIZE, F, &
                           HPROGRAM,'T_B1   ',YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,GUNIF_G,GINTERP)
 CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, GCP, ISIZE, F, &
                           HPROGRAM,'CT     ',YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,GUNIF_G,GINTERP)
 CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, GCP, ISIZE, F, &
                           HPROGRAM,'H_SNOW ',YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,GUNIF_G,GINTERP)
 CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, GCP, ISIZE, F, &
                           HPROGRAM,'H_ICE  ',YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,GUNIF_G,GINTERP)
 CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, GCP, ISIZE, F, &
                           HPROGRAM,'H_ML   ',YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,GUNIF_G,GINTERP)
 CALL PREP_HOR_FLAKE_FIELD(DTCO, UG, U, USS, GCP, ISIZE, F, &
                           HPROGRAM,'H_B1   ',YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,GUNIF_G,GINTERP)
 ALLOCATE(F%XT_MNW(ISIZE))
 IF (GINTERP) THEN
  GPROFILE = .FALSE.
 END IF
  !
ENDIF
!
!
!-------------------------------------------------------------------------------------
!
 CALL CLEAN_PREP_OUTPUT_GRID
!
!*      2.2    Roughness
!
ALLOCATE(F%XZ0(ISIZE))
F%XZ0 = 0.001
!
!*      2.2    Friction velocity
!
ALLOCATE(F%XUSTAR(ISIZE))
F%XUSTAR = 0.

!-------------------------------------------------------------------------------------

!
!*      3.     Vertical interpolations of all variables
!
IF(.NOT.LCLIM_LAKE) THEN
  IF (LVERTSHIFT)THEN   
    IF (GPROFILE) THEN 
      WRITE(ILUOUT,*) "WARNING: Vertical shift for the lake temperature profile is impossible!"
      IF(GUNIF_G) THEN
        WRITE(ILUOUT,*) "WARNING: So, set the mixed profiles from the shifted surface temperature."
        IF(.NOT.GUNIF_TS) THEN 
          WHERE(F%XH_SNOW > 0.0)
            F%XTS(:) = F%XT_SNOW(:)
          END WHERE
          WHERE(F%XH_SNOW == 0.0 .AND. F%XH_ICE > 0.0)
            F%XTS(:) = F%XT_ICE(:)
          END WHERE
          WHERE(F%XH_SNOW == 0.0 .AND. F%XH_ICE == 0.0)
            F%XTS(:) = F%XT_WML(:)
          ENDWHERE
        END IF
        CALL PREP_VER_FLAKE(F)
        GPROFILE=.FALSE.
        WRITE(ILUOUT,*) "WARNING: If you want to keep stratification in lakes, set LVERTSHIFT = F."  
      ELSE
        WRITE(ILUOUT,*) "WARNING: Vertical shift for lakes will be skipped even with LVERTSHIFT = F."
      END IF
    ELSE
      CALL PREP_VER_FLAKE(F)
    END IF
  ENDIF
  DEALLOCATE(XZS_LS)
END IF
!
!-------------------------------------------------------------------------------------
!
!*      4.    Compute T_MNW and give the default profile if needed 
!              or read data from climate files 
!
IF (LCLIM_LAKE) THEN

  ALLOCATE(F%XTS(ISIZE))
  ALLOCATE(F%XT_SNOW(ISIZE)) 
  ALLOCATE(F%XT_ICE(ISIZE))  
  ALLOCATE(F%XT_WML(ISIZE))
  ALLOCATE(F%XT_MNW(ISIZE)) 
  ALLOCATE(F%XT_BOT(ISIZE))  
  ALLOCATE(F%XT_B1(ISIZE))
  ALLOCATE(F%XCT(ISIZE))  
  ALLOCATE(F%XH_SNOW(ISIZE))  
  ALLOCATE(F%XH_ICE(ISIZE))
  ALLOCATE(F%XH_ML(ISIZE))
  ALLOCATE(F%XH_B1(ISIZE))

  CALL CLI_LAKE(FG, F)

  GPROFILE =.TRUE.
END IF

IF (GPROFILE) THEN
  WHERE(F%XH_SNOW > 0.0)
    F%XTS(:) = F%XT_SNOW(:)
  END WHERE
  WHERE(F%XH_SNOW == 0.0 .AND. F%XH_ICE > 0.0)
    F%XTS(:) = F%XT_ICE(:)
  END WHERE
  WHERE(F%XH_SNOW == 0.0 .AND. F%XH_ICE == 0.0)
    F%XTS(:) = F%XT_WML(:)
  ENDWHERE
END IF

! Check constraints for the profiles,
! make corrections according to the constratints
! and calculate the mean water temperature in case of unified profiles
IF(GPROFILE) THEN
  WHERE (F%XH_ML < 0.0)
    F%XH_ML = 0.0
  END WHERE 
  WHERE (F%XH_ML > F%XWATER_DEPTH)
    F%XH_ML = F%XWATER_DEPTH
  END WHERE
  WHERE (F%XCT < C_T_min)
    F%XCT = C_T_min
  END WHERE
  WHERE (F%XCT > C_T_max )
    F%XCT = C_T_max
  END WHERE
  WHERE (F%XT_BOT < tpl_T_f )
    F%XT_BOT = tpl_T_f
  END WHERE
  IF (.NOT.LCLIM_LAKE) THEN
    IF (GUNIF_G) THEN 
! in case of unified profiles, calculate the mean water temperature
      WHERE (F%XT_WML < tpl_T_f)
        F%XT_WML = tpl_T_f
      END WHERE
      WHERE(F%XH_ML >= F%XWATER_DEPTH-h_ML_min_flk)
        F%XT_MNW = F%XT_WML
      ELSEWHERE
        F%XT_MNW = F%XT_WML-(F%XT_WML-F%XT_BOT)*(1.-F%XH_ML/F%XWATER_DEPTH)*F%XCT
      END WHERE
    ELSE
! in case of the input file, calculate the mixed layer temperature
      WHERE (F%XT_MNW < tpl_T_f)
        F%XT_MNW = tpl_T_f
      END WHERE
      WHERE(F%XH_ML >= F%XWATER_DEPTH-h_ML_min_flk)
        F%XT_WML = F%XT_MNW
      ELSEWHERE
        F%XT_WML = (F%XT_MNW-(1.-F%XH_ML/F%XWATER_DEPTH)*F%XCT*F%XT_BOT)/ &
                 (1.-(1.-F%XH_ML/F%XWATER_DEPTH)*F%XCT)
        WHERE (F%XT_WML < tpl_T_f)
          F%XT_WML = tpl_T_f
          F%XT_MNW=F%XT_WML-(F%XT_WML-F%XT_BOT)*(1.-F%XH_ML/F%XWATER_DEPTH)*F%XCT
        END WHERE   
      END WHERE    
    END IF
  END IF 
  WHERE (F%XH_SNOW < h_Snow_min_flk)
    F%XH_SNOW = 0.0
    F%XT_SNOW = tpl_T_f
  END WHERE
  WHERE (F%XT_SNOW > tpl_T_f)
    F%XT_SNOW = tpl_T_f
  END WHERE
  WHERE (F%XH_ICE < h_Ice_min_flk)
    F%XH_SNOW = 0.0
    F%XT_SNOW = tpl_T_f
    F%XH_ICE = 0.0
    F%XT_ICE = tpl_T_f
  END WHERE
  WHERE(F%XH_ICE > H_Ice_max)
    F%XH_ICE = H_Ice_max
  END WHERE
  WHERE (F%XT_ICE > tpl_T_f)
    F%XT_ICE = tpl_T_f
  END WHERE
  WHERE (F%XH_ICE > h_Ice_min_flk)
    F%XT_WML = tpl_T_f
    WHERE (F%XT_WML == F%XT_BOT)
      F%XH_ML = 0.0
      F%XCT = C_T_min
      F%XT_MNW = F%XT_WML 
    END WHERE
    WHERE (F%XH_ML >= F%XWATER_DEPTH-h_ML_min_flk)
      F%XH_ML = 0.0
      F%XCT = C_T_min
      F%XT_MNW = F%XT_WML 
      F%XT_BOT = F%XT_WML      
    END WHERE
! was from Dmitrii's code, but then corrected
 !   WHERE (F%XT_MNW > tpl_T_r)
 !     F%XT_MNW = tpl_T_r 
 !   END WHERE
    WHERE (F%XT_BOT > tpl_T_r .OR. F%XT_MNW > tpl_T_r)
      F%XT_BOT = tpl_T_r
      F%XT_MNW=F%XT_WML-(F%XT_WML-F%XT_BOT)*(1.-F%XH_ML/F%XWATER_DEPTH)*F%XCT 
    END WHERE
  ELSEWHERE
! Ensure mixing
    WHERE (F%XH_ML >= F%XWATER_DEPTH-h_ML_min_flk)
      F%XH_ML = F%XWATER_DEPTH
      F%XCT = C_T_min
      F%XT_WML = F%XT_MNW
      F%XT_BOT = F%XT_MNW
    END WHERE
! Avoid temperature of maximum density crossover
    WHERE ((F%XT_WML <= tpl_T_r .AND. F%XT_BOT > tpl_T_r).OR. &
           (F%XT_WML >= tpl_T_r .AND. F%XT_BOT < tpl_T_r))
      F%XT_BOT = tpl_T_r
      F%XT_MNW=F%XT_WML-(F%XT_WML-F%XT_BOT)*(1.-F%XH_ML/F%XWATER_DEPTH)*F%XCT 
    END WHERE
! Avoid inversion
    WHERE ((F%XT_WML <= tpl_T_r .AND. F%XT_BOT < F%XT_WML).OR. &
           (F%XT_WML >= tpl_T_r .AND. F%XT_BOT > F%XT_WML))
      F%XH_ML = F%XWATER_DEPTH
      F%XCT = C_T_min
      F%XT_WML = F%XT_MNW
      F%XT_BOT = F%XT_MNW
    END WHERE
  END WHERE
END IF

IF(.NOT.GPROFILE) THEN
  F%XT_WML=MAX(F%XTS(:),tpl_T_f) 
  F%XT_SNOW=MIN(F%XTS(:),tpl_T_f)
  F%XT_ICE=MIN(F%XTS(:),tpl_T_f)
  F%XH_B1=5.0 
  F%XCT=C_T_min
  F%XH_SNOW=0.0   
  WHERE (F%XTS <= tpl_T_f)
   F%XT_BOT= tpl_T_r
   F%XT_B1= tpl_T_r - 0.1
   F%XH_ICE=0.01
   F%XH_ML=F%XWATER_DEPTH/2.
   F%XT_MNW=F%XT_WML-(F%XT_WML-F%XT_BOT)*(1.-F%XH_ML/F%XWATER_DEPTH)*F%XCT
  ELSEWHERE
   F%XT_BOT=F%XTS
   F%XT_B1=F%XTS-0.1
   F%XH_ICE=0.0
   F%XH_ML=F%XWATER_DEPTH
   F%XT_MNW=F%XTS 
  END WHERE
END IF 
!
!-------------------------------------------------------------------------------------
!
!*      6.     Preparation of SBL air variables
!
F%LSBL = LWAT_SBL
IF (F%LSBL) CALL PREP_SBL(ISIZE, SB)
!
IF (LHOOK) CALL DR_HOOK('PREP_FLAKE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_FLAKE
