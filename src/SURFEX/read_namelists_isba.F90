!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE READ_NAMELISTS_ISBA(HPROGRAM)
!     #######################################################
!
!---------------------------    
!
USE MODD_AGRI,           ONLY : LAGRIP
USE MODD_DEEPSOIL,       ONLY : LPHYSDOMC, LDEEPSOIL
USE MODD_ISBA_ALBEDO,    ONLY : CALBEDO
!
USE MODI_DEFAULT_AGRI
USE MODI_DEFAULT_DEEPSOIL
USE MODI_READ_ISBA_CONF
USE MODI_READ_NAM_PGD_ISBA
!
!--------------------------------------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER                  :: IPATCH           ! number of patches
INTEGER                  :: IGROUND_LAYER    ! number of soil layers
CHARACTER(LEN=3)         :: YISBA            ! ISBA option
CHARACTER(LEN=4)         :: YPEDOTF          ! Pedo transfert function for DIF
CHARACTER(LEN=3)         :: YPHOTO           ! photosynthesis option
LOGICAL                  :: GTR_ML           ! new radiative transfert
CHARACTER(LEN=4)         :: YALBEDO
REAL                     :: ZRM_PATCH        ! threshold to remove little fractions of patches
CHARACTER(LEN=28)        :: YSAND            ! file name for sand fraction
CHARACTER(LEN=28)        :: YCLAY            ! file name for clay fraction
CHARACTER(LEN=28)        :: YSOC_TOP         ! file name for organic carbon top soil
CHARACTER(LEN=28)        :: YSOC_SUB         ! file name for organic carbon sub soil
CHARACTER(LEN=28)        :: YCTI             ! file name for topographic index
CHARACTER(LEN=28)        :: YRUNOFFB         ! file name for runoffb parameter
CHARACTER(LEN=28)        :: YWDRAIN          ! file name for wdrain parameter
CHARACTER(LEN=28)        :: YPERM            ! file name for permafrost distribution
CHARACTER(LEN=6)         :: YSANDFILETYPE    ! sand data file type
CHARACTER(LEN=6)         :: YCLAYFILETYPE    ! clay data file type
CHARACTER(LEN=6)         :: YSOCFILETYPE     ! organic carbon data file type
CHARACTER(LEN=6)         :: YCTIFILETYPE     ! topographic index data file type
CHARACTER(LEN=6)         :: YRUNOFFBFILETYPE ! subgrid runoff data file type
CHARACTER(LEN=6)         :: YWDRAINFILETYPE  ! subgrid drainage data file type
CHARACTER(LEN=6)         :: YPERMFILETYPE    !! permafrost distribution data file type
REAL                     :: XUNIF_SAND       ! uniform value of sand fraction  (-)
REAL                     :: XUNIF_CLAY       ! uniform value of clay fraction  (-)
REAL                     :: XUNIF_SOC_TOP    ! uniform value of organic carbon top soil (kg/m2)
REAL                     :: XUNIF_SOC_SUB    ! uniform value of organic carbon sub soil (kg/m2)
REAL                     :: XUNIF_RUNOFFB    ! uniform value of subgrid runoff coefficient
REAL                     :: XUNIF_WDRAIN     ! uniform subgrid drainage parameter
REAL                     :: XUNIF_PERM       ! uniform permafrost distribution
LOGICAL                  :: LIMP_SAND        ! Imposed maps of Sand
LOGICAL                  :: LIMP_CLAY        ! Imposed maps of Clay
LOGICAL                  :: LIMP_SOC         ! Imposed maps of organic carbon
LOGICAL                  :: LIMP_CTI         ! Imposed maps of topographic index statistics
LOGICAL                  :: LIMP_PERM        ! Imposed maps of permafrost distribution
REAL, DIMENSION(150)     :: ZSOILGRID        ! Soil grid reference for DIF
CHARACTER(LEN=28)        :: YPH           ! file name for pH
CHARACTER(LEN=28)        :: YFERT         ! file name for fertilisation rate
CHARACTER(LEN=6)         :: YPHFILETYPE   ! pH data file type
CHARACTER(LEN=6)         :: YFERTFILETYPE ! fertilisation data file type
REAL                     :: XUNIF_PH      ! uniform value of pH
REAL                     :: XUNIF_FERT    ! uniform value of fertilisation rate
LOGICAL                  :: GMEB      ! Multi-energy balance (MEB)
!
!---------------------------------------------------
!                  
IF (LHOOK) CALL DR_HOOK('READ_NAMELISTS_ISBA',0,ZHOOK_HANDLE)
!                   
 CALL DEFAULT_AGRI(LAGRIP)
!
 CALL DEFAULT_DEEPSOIL(LDEEPSOIL,LPHYSDOMC)
!
 CALL READ_ISBA_CONF(HPROGRAM)
!
 CALL READ_NAM_PGD_ISBA(HPROGRAM, IPATCH, IGROUND_LAYER,                        &
                       YISBA,  YPEDOTF, YPHOTO, GTR_ML, YALBEDO, ZRM_PATCH,      &
                       YCLAY, YCLAYFILETYPE, XUNIF_CLAY, LIMP_CLAY,              &
                       YSAND, YSANDFILETYPE, XUNIF_SAND, LIMP_SAND,              &
                       YSOC_TOP, YSOC_SUB, YSOCFILETYPE, XUNIF_SOC_TOP,          &
                       XUNIF_SOC_SUB, LIMP_SOC, YCTI, YCTIFILETYPE, LIMP_CTI,    &
                       YPERM, YPERMFILETYPE, XUNIF_PERM, LIMP_PERM, GMEB,        &                       
                       YRUNOFFB, YRUNOFFBFILETYPE, XUNIF_RUNOFFB,                &
                       YWDRAIN,  YWDRAINFILETYPE , XUNIF_WDRAIN, ZSOILGRID,      &
                       YPH, YPHFILETYPE, XUNIF_PH, YFERT, YFERTFILETYPE,         &
                       XUNIF_FERT                          )
 CALBEDO = YALBEDO
!
IF (LHOOK) CALL DR_HOOK('READ_NAMELISTS_ISBA',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------
!
END SUBROUTINE READ_NAMELISTS_ISBA
