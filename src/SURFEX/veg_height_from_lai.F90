!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#######################
MODULE MODI_VEG_HEIGHT_FROM_LAI
!#######################
!
INTERFACE VEG_HEIGHT_FROM_LAI
!
    FUNCTION VEG_HEIGHT_FROM_LAI_0D(PLAI,PH_TREE,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PH_VEG)
!
REAL,                 INTENT(IN) :: PLAI         ! Leaf area Index
REAL,                 INTENT(IN) :: PH_TREE      ! height of trees
REAL,   DIMENSION(:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,              INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PVEGTYPE))  :: PH_VEG          ! vegetation height
!
END FUNCTION VEG_HEIGHT_FROM_LAI_0D
!
!
    FUNCTION VEG_HEIGHT_FROM_LAI_1D(PLAI,PH_TREE,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PH_VEG)
!
REAL,   DIMENSION(:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:),   INTENT(IN) :: PH_TREE      ! height of trees
REAL,   DIMENSION(:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,                INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PVEGTYPE,1),SIZE(PVEGTYPE,2))  :: PH_VEG          ! vegetation height
!
END FUNCTION VEG_HEIGHT_FROM_LAI_1D
!
!
    FUNCTION VEG_HEIGHT_FROM_LAI_2D(PLAI,PH_TREE,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PH_VEG)
!
REAL,   DIMENSION(:,:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:,:),   INTENT(IN) :: PH_TREE      ! height of trees
REAL,   DIMENSION(:,:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,                  INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PVEGTYPE,1),SIZE(PVEGTYPE,2),SIZE(PVEGTYPE,3))  :: PH_VEG          ! vegetation height
!
END FUNCTION VEG_HEIGHT_FROM_LAI_2D
!
    FUNCTION VEG_HEIGHT_FROM_LAI_VEGTYPE(PLAI,PH_TREE,OAGRI_TO_GRASS) RESULT(PH_VEG)
!
REAL,   DIMENSION(:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:),   INTENT(IN) :: PH_TREE      ! height of trees
LOGICAL,                INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PLAI))  :: PH_VEG  ! vegetation height
!
END FUNCTION VEG_HEIGHT_FROM_LAI_VEGTYPE
!
END INTERFACE
!
END MODULE MODI_VEG_HEIGHT_FROM_LAI
!

!   ###########################################################
    FUNCTION VEG_HEIGHT_FROM_LAI_0D(PLAI,PH_TREE,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PH_VEG)
!   ###########################################################
!!
!!    PURPOSE
!!    -------
!
!     Calculates vegetation height from leaf
!    area index and type of vegetation
!    (most of types; forest and vineyards; grassland)
!              
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      V. Masson and A. Boone          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    25/03/99
!!      P. Samuelsson 02/2012 MEB
!!      P. Marguinaud (Oct 2016) : Port to single precision
!!      S. Viana     (June 2020) : Enhance vegetation height for every low vegetation type (Patch 1): Consider that 
!!                                 a fraction XFFAKETREE of every low veg area is covered by trees of height XHFAKETREE,
!!                                 and log-average vegetation height accordingly using the Z0 averaging formula (Mason 1988)
!!                                 This will increase Z0 for patch 1. This is controlled by LFAKETREE, XHFAKETREE, XFFAKETREE 
!!                                 in the namelist block %NAM_TREEDRAG. Not active by default.
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK, NVT_SNOW, NVT_PARK,        &
                                NVT_TEBD, NVT_BONE, NVT_TRBE, NVT_TRBD,      &
                                NVT_TEBE, NVT_TENE, NVT_BOBD, NVT_BOND,      &
                                NVT_SHRB, NVT_C3, NVT_C4, NVT_IRR,           &
                                NVT_GRAS, NVT_BOGR, NVT_TROG, NVT_C3W,       &
                                NVT_C3S, NVT_FLTR, NVT_FLGR
USE MODD_TREEDRAG,       ONLY : LTREEDRAG, XALLEN_TERM, XGRASS_H_DNM, LFAKETREE, XHFAKETREE, XFFAKETREE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,                 INTENT(IN) :: PLAI         ! Leaf area Index
REAL,                 INTENT(IN) :: PH_TREE      ! height of trees
REAL,   DIMENSION(:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,              INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PVEGTYPE))  :: PH_VEG          ! vegetation height
!
!*      0.2    declarations of local variables
!
REAL                            :: ZALLEN_H    ! Allen formula for height
REAL                            :: ZLAI        ! LAI for vegetated areas
!
REAL                            :: ZAVG_H      ! averaged height
REAL                            :: ZZREF       ! Reference height 
!
INTEGER                         :: JTYPE       ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_VEG_HEIGHT_FROM_LAI:VEG_HEIGHT_FROM_LAI_0D',0,ZHOOK_HANDLE)
!
!-----------------------------------------------------------------
!
ZZREF=10.0
!
ZLAI = PLAI
IF ( PVEGTYPE(NVT_NO  ) + PVEGTYPE(NVT_ROCK) + PVEGTYPE(NVT_SNOW) < 1.) THEN
  ZLAI = PLAI / (1.-PVEGTYPE(NVT_NO)-PVEGTYPE(NVT_ROCK)-PVEGTYPE(NVT_SNOW))
END IF
!
ZALLEN_H = 0.
IF ( PLAI /= XUNDEF) THEN
  ZALLEN_H = EXP((ZLAI-XALLEN_TERM)/(1.3))
END IF
!
IF (NVT_PARK>0) THEN
  PH_VEG(NVT_PARK) = ZLAI / XGRASS_H_DNM                    ! irr. grassland
  IF (LFAKETREE) THEN 
    PH_VEG(NVT_PARK) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(NVT_PARK)/ZZREF))**2 + XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
    PH_VEG(NVT_PARK) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(NVT_PARK)))
  ENDIF
ELSEIF (NVT_FLGR>0) THEN
  PH_VEG(NVT_FLGR) = ZLAI / XGRASS_H_DNM
  IF (LFAKETREE) THEN 
    PH_VEG(NVT_FLGR) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(NVT_FLGR)/ZZREF))**2 + XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
    PH_VEG(NVT_FLGR) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(NVT_FLGR)))
  ENDIF
ENDIF
IF (LTREEDRAG) THEN
  PH_VEG(NVT_TEBD) = ZLAI / XGRASS_H_DNM                  ! forest
  PH_VEG(NVT_BONE) = ZLAI / XGRASS_H_DNM                  ! forest
  PH_VEG(NVT_TRBE) = ZLAI / XGRASS_H_DNM                  ! forest
  PH_VEG(NVT_TRBD) = ZLAI / XGRASS_H_DNM                  ! forest
  PH_VEG(NVT_TEBE) = ZLAI / XGRASS_H_DNM                  ! forest
  PH_VEG(NVT_TENE) = ZLAI / XGRASS_H_DNM                  ! forest
  PH_VEG(NVT_BOBD) = ZLAI / XGRASS_H_DNM                  ! forest
  PH_VEG(NVT_BOND) = ZLAI / XGRASS_H_DNM                  ! forest
  PH_VEG(NVT_SHRB) = ZLAI / XGRASS_H_DNM                  ! forest  
  IF (NVT_FLTR>0) PH_VEG(NVT_FLTR) = ZLAI / XGRASS_H_DNM
ELSE
  PH_VEG(NVT_TEBD) = PH_TREE                  ! forest
  PH_VEG(NVT_BONE) = PH_TREE                  ! forest
  PH_VEG(NVT_TRBE) = PH_TREE                  ! forest
  PH_VEG(NVT_TRBD) = PH_TREE                  ! forest
  PH_VEG(NVT_TEBE) = PH_TREE                  ! forest
  PH_VEG(NVT_TENE) = PH_TREE                  ! forest
  PH_VEG(NVT_BOBD) = PH_TREE                  ! forest
  PH_VEG(NVT_BOND) = PH_TREE                  ! forest
  PH_VEG(NVT_SHRB) = PH_TREE                  ! forest  
  IF (NVT_FLTR>0) PH_VEG(NVT_FLTR) = PH_TREE
END IF
PH_VEG(NVT_GRAS) = ZLAI / XGRASS_H_DNM                    ! grassland
PH_VEG(NVT_BOGR) = ZLAI / XGRASS_H_DNM                    ! boreal grassland
PH_VEG(NVT_TROG) = ZLAI / XGRASS_H_DNM                    ! tropical grassland
IF (LFAKETREE) THEN 
  PH_VEG(NVT_GRAS) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(NVT_GRAS)/ZZREF))**2 + XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
  PH_VEG(NVT_GRAS) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(NVT_GRAS)))
  PH_VEG(NVT_BOGR) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(NVT_BOGR)/ZZREF))**2 + XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
  PH_VEG(NVT_BOGR) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(NVT_BOGR)))
  PH_VEG(NVT_TROG) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(NVT_TROG)/ZZREF))**2 + XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
  PH_VEG(NVT_TROG) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(NVT_TROG)))
ENDIF
IF(OAGRI_TO_GRASS)THEN
  IF (NVT_C3>0) THEN
    PH_VEG(NVT_C3  ) = ZLAI / XGRASS_H_DNM
    IF (LFAKETREE) THEN   
      PH_VEG(NVT_C3) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(NVT_C3)/ZZREF))**2 + XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
      PH_VEG(NVT_C3) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(NVT_C3)))
    ENDIF
  ELSEIF (NVT_C3W>0 .AND. NVT_C3S>0) THEN
    PH_VEG(NVT_C3W ) = ZLAI / XGRASS_H_DNM
    PH_VEG(NVT_C3S ) = ZLAI / XGRASS_H_DNM
    IF (LFAKETREE) THEN       
      PH_VEG(NVT_C3W) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(NVT_C3W)/ZZREF))**2 + XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
      PH_VEG(NVT_C3W) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(NVT_C3W)))
      PH_VEG(NVT_C3S) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(NVT_C3S)/ZZREF))**2 + XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
      PH_VEG(NVT_C3S) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(NVT_C3S)))
    ENDIF
  ENDIF
  PH_VEG(NVT_C4  ) = ZLAI / XGRASS_H_DNM
  IF (LFAKETREE) THEN
      PH_VEG(NVT_C4) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(NVT_C4)/ZZREF))**2 + XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
      PH_VEG(NVT_C4) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(NVT_C4)))
  ENDIF
  IF (NVT_IRR>0) PH_VEG(NVT_IRR ) = ZLAI / XGRASS_H_DNM
ELSE
  IF (NVT_C3>0) THEN
    PH_VEG(NVT_C3  ) = MIN(1. , ZALLEN_H )          ! cultures
    IF (LFAKETREE) THEN
      PH_VEG(NVT_C3) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(NVT_C3)/ZZREF))**2 + XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
      PH_VEG(NVT_C3) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(NVT_C3)))
    ENDIF
  ELSEIF (NVT_C3W>0 .AND. NVT_C3S>0) THEN
    PH_VEG(NVT_C3W ) =  MIN(1. , ZALLEN_H )
    PH_VEG(NVT_C3S ) =  MIN(1. , ZALLEN_H )
    IF (LFAKETREE) THEN
      PH_VEG(NVT_C3W) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(NVT_C3W)/ZZREF))**2 + XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
      PH_VEG(NVT_C3W) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(NVT_C3W)))
      PH_VEG(NVT_C3S) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(NVT_C3S)/ZZREF))**2 + XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
      PH_VEG(NVT_C3S) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(NVT_C3S)))
    ENDIF
  ENDIF
  PH_VEG(NVT_C4  ) = MIN(2.5, ZALLEN_H )          ! C4 types
  IF (LFAKETREE) THEN
    PH_VEG(NVT_C4) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(NVT_C4)/ZZREF))**2 + XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
    PH_VEG(NVT_C4) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(NVT_C4)))
  ENDIF
  IF (NVT_IRR>0) THEN 
    PH_VEG(NVT_IRR ) = MIN(2.5, ZALLEN_H )          ! irrigated crops (as C4)
    IF (LFAKETREE) THEN  
      PH_VEG(NVT_IRR) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(NVT_IRR)/ZZREF))**2 + XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
      PH_VEG(NVT_IRR) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(NVT_IRR)))
    ENDIF
  ENDIF
ENDIF
PH_VEG(NVT_NO  ) = 0.1                          ! no vegetation (smooth)
PH_VEG(NVT_ROCK) = 1.                           ! no vegetation (rocks)
PH_VEG(NVT_SNOW) = 0.01                         ! no vegetation (snow)
!
PH_VEG(:) = MAX(PH_VEG(:),0.001)
!
IF (LHOOK) CALL DR_HOOK('MODI_VEG_HEIGHT_FROM_LAI:VEG_HEIGHT_FROM_LAI_0D',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
END FUNCTION VEG_HEIGHT_FROM_LAI_0D
!
!   ###########################################################
    FUNCTION VEG_HEIGHT_FROM_LAI_1D(PLAI,PH_TREE,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PH_VEG)
!   ###########################################################
!!
!!    PURPOSE
!!    -------
!
!     Calculates vegetation height from leaf
!    area index and type of vegetation
!    (most of types; forest and vineyards; grassland)
!              
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      V. Masson and A. Boone          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    25/03/99
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK, NVT_SNOW, NVT_PARK,        &
                                NVT_TEBD, NVT_BONE, NVT_TRBE, NVT_TRBD,      &
                                NVT_TEBE, NVT_TENE, NVT_BOBD, NVT_BOND,      &
                                NVT_SHRB, NVT_C3, NVT_C4, NVT_IRR,           &
                                NVT_GRAS, NVT_BOGR, NVT_TROG, NVT_C3W,       &
                                NVT_C3S, NVT_FLTR, NVT_FLGR
USE MODD_TREEDRAG,       ONLY : LTREEDRAG, XALLEN_TERM, XGRASS_H_DNM, LFAKETREE, XHFAKETREE, XFFAKETREE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,   DIMENSION(:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:),   INTENT(IN) :: PH_TREE      ! height of trees
REAL,   DIMENSION(:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,                INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PVEGTYPE,1),SIZE(PVEGTYPE,2))  :: PH_VEG          ! vegetation height
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PLAI))                  :: ZALLEN_H    ! Allen formula for height
REAL, DIMENSION(SIZE(PLAI))                  :: ZLAI        ! LAI for vegetated areas
!
REAL, DIMENSION(SIZE(PLAI))                  :: ZAVG_H      ! averaged height
REAL                                         :: ZZREF       ! reference height        
!
INTEGER                                      :: JTYPE       ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_VEG_HEIGHT_FROM_LAI:VEG_HEIGHT_FROM_LAI_1D',0,ZHOOK_HANDLE)
!
!-----------------------------------------------------------------
!
ZZREF=10.0
!
PH_VEG(:,:) = XUNDEF
!
ZLAI(:) = PLAI(:)
WHERE ( PVEGTYPE(:,NVT_NO  ) + PVEGTYPE(:,NVT_ROCK) + PVEGTYPE(:,NVT_SNOW) < 1.) 
  ZLAI(:) = PLAI(:) / (1.-PVEGTYPE(:,NVT_NO)-PVEGTYPE(:,NVT_ROCK)-PVEGTYPE(:,NVT_SNOW))
END WHERE
!
ZALLEN_H(:) = 0.
WHERE (PLAI(:) /= XUNDEF)
  ZALLEN_H(:) = EXP((ZLAI(:)-XALLEN_TERM)/(1.3))
END WHERE
!
!
IF (NVT_PARK>0) THEN
  PH_VEG(:,NVT_PARK) = ZLAI(:) / XGRASS_H_DNM                 ! irr. grassland
  IF (LFAKETREE) THEN
    PH_VEG(:,NVT_PARK) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(:,NVT_PARK)/ZZREF))**2 + (ZLAI(:)/ZLAI(:))*XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
    PH_VEG(:,NVT_PARK) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(:,NVT_PARK)))
  ENDIF
ELSEIF (NVT_FLGR>0) THEN
  PH_VEG(:,NVT_FLGR) = ZLAI(:) / XGRASS_H_DNM
  IF (LFAKETREE) THEN
    PH_VEG(:,NVT_FLGR) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(:,NVT_FLGR)/ZZREF))**2 + (ZLAI(:)/ZLAI(:))*XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
    PH_VEG(:,NVT_FLGR) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(:,NVT_FLGR)))
  ENDIF
ENDIF
!
IF (LTREEDRAG) THEN
  PH_VEG(:,NVT_TEBD) = ZLAI(:) / XGRASS_H_DNM         ! forest
  PH_VEG(:,NVT_BONE) = ZLAI(:) / XGRASS_H_DNM         ! forest
  PH_VEG(:,NVT_TRBE) = ZLAI(:) / XGRASS_H_DNM         ! forest
  PH_VEG(:,NVT_TRBD) = ZLAI(:) / XGRASS_H_DNM         ! forest
  PH_VEG(:,NVT_TEBE) = ZLAI(:) / XGRASS_H_DNM         ! forest
  PH_VEG(:,NVT_TENE) = ZLAI(:) / XGRASS_H_DNM         ! forest
  PH_VEG(:,NVT_BOBD) = ZLAI(:) / XGRASS_H_DNM         ! forest
  PH_VEG(:,NVT_BOND) = ZLAI(:) / XGRASS_H_DNM         ! forest
  PH_VEG(:,NVT_SHRB) = ZLAI(:) / XGRASS_H_DNM         ! forest  
  IF (NVT_FLTR>0) PH_VEG(:,NVT_FLTR) = ZLAI(:) / XGRASS_H_DNM
ELSE
  PH_VEG(:,NVT_TEBD) = PH_TREE(:)           ! forest
  PH_VEG(:,NVT_BONE) = PH_TREE(:)           ! forest
  PH_VEG(:,NVT_TRBE) = PH_TREE(:)           ! forest
  PH_VEG(:,NVT_TRBD) = PH_TREE(:)           ! forest
  PH_VEG(:,NVT_TEBE) = PH_TREE(:)           ! forest
  PH_VEG(:,NVT_TENE) = PH_TREE(:)           ! forest
  PH_VEG(:,NVT_BOBD) = PH_TREE(:)           ! forest
  PH_VEG(:,NVT_BOND) = PH_TREE(:)           ! forest
  PH_VEG(:,NVT_SHRB) = PH_TREE(:)           ! forest
  IF (NVT_FLTR>0) PH_VEG(:,NVT_FLTR) = PH_TREE(:)  
END IF
PH_VEG(:,NVT_GRAS) = ZLAI(:) / XGRASS_H_DNM           ! grassland
PH_VEG(:,NVT_BOGR) = ZLAI(:) / XGRASS_H_DNM           ! boreal grassland
PH_VEG(:,NVT_TROG) = ZLAI(:) / XGRASS_H_DNM           ! tropical grassland
IF (LFAKETREE) THEN
  PH_VEG(:,NVT_GRAS) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(:,NVT_GRAS)/ZZREF))**2 + (ZLAI(:)/ZLAI(:))*XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
  PH_VEG(:,NVT_GRAS) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(:,NVT_GRAS)))
  PH_VEG(:,NVT_BOGR) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(:,NVT_BOGR)/ZZREF))**2 + (ZLAI(:)/ZLAI(:))*XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
  PH_VEG(:,NVT_BOGR) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(:,NVT_BOGR)))
  PH_VEG(:,NVT_TROG) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(:,NVT_TROG)/ZZREF))**2 + (ZLAI(:)/ZLAI(:))*XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
  PH_VEG(:,NVT_TROG) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(:,NVT_TROG)))
ENDIF
IF(OAGRI_TO_GRASS)THEN
  IF (NVT_C3>0) THEN
    PH_VEG(:,NVT_C3) = ZLAI(:) / XGRASS_H_DNM
    IF (LFAKETREE) THEN 
      PH_VEG(:,NVT_C3) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(:,NVT_C3)/ZZREF))**2 + (ZLAI(:)/ZLAI(:))*XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
      PH_VEG(:,NVT_C3) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(:,NVT_C3)))
    ENDIF
  ELSEIF (NVT_C3W>0 .AND. NVT_C3S>0) THEN
    PH_VEG(:,NVT_C3W ) = ZLAI(:) / XGRASS_H_DNM
    PH_VEG(:,NVT_C3S ) = ZLAI(:) / XGRASS_H_DNM
    IF (LFAKETREE) THEN 
      PH_VEG(:,NVT_C3W) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(:,NVT_C3W)/ZZREF))**2 + (ZLAI(:)/ZLAI(:))*XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
      PH_VEG(:,NVT_C3W) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(:,NVT_C3W)))
      PH_VEG(:,NVT_C3S) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(:,NVT_C3S)/ZZREF))**2 + (ZLAI(:)/ZLAI(:))*XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
      PH_VEG(:,NVT_C3S) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(:,NVT_C3S)))
    ENDIF
  ENDIF
  PH_VEG(:,NVT_C4  ) = ZLAI(:) / XGRASS_H_DNM
  IF (LFAKETREE) THEN  
    PH_VEG(:,NVT_C4) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(:,NVT_C4)/ZZREF))**2 + (ZLAI(:)/ZLAI(:))*XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
    PH_VEG(:,NVT_C4) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(:,NVT_C4)))
  ENDIF
  IF (NVT_IRR>0) PH_VEG(:,NVT_IRR ) = ZLAI(:) / XGRASS_H_DNM
ELSE
  IF (NVT_C3>0) THEN
    PH_VEG(:,NVT_C3  ) = MIN(1. , ZALLEN_H(:) )          ! cultures
    IF (LFAKETREE) THEN  
      PH_VEG(:,NVT_C3) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(:,NVT_C3)/ZZREF))**2 + (ZLAI(:)/ZLAI(:))*XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
      PH_VEG(:,NVT_C3) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(:,NVT_C3)))
    ENDIF
  ELSEIF (NVT_C3W>0 .AND. NVT_C3S>0) THEN
    PH_VEG(:,NVT_C3W ) = MIN(1. , ZALLEN_H(:) )
    PH_VEG(:,NVT_C3S ) = MIN(1. , ZALLEN_H(:) )
    IF (LFAKETREE) THEN  
      PH_VEG(:,NVT_C3W) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(:,NVT_C3W)/ZZREF))**2 + (ZLAI(:)/ZLAI(:))*XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
      PH_VEG(:,NVT_C3W) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(:,NVT_C3W)))
      PH_VEG(:,NVT_C3S) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(:,NVT_C3S)/ZZREF))**2 + (ZLAI(:)/ZLAI(:))*XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
      PH_VEG(:,NVT_C3S) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(:,NVT_C3S)))
    ENDIF
  ENDIF
  PH_VEG(:,NVT_C4  ) = MIN(2.5, ZALLEN_H(:) )          ! C4 types
  IF (LFAKETREE) THEN
    PH_VEG(:,NVT_C4) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(:,NVT_C4)/ZZREF))**2 + (ZLAI(:)/ZLAI(:))*XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
    PH_VEG(:,NVT_C4) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(:,NVT_C4)))
  ENDIF
  IF (NVT_IRR>0) THEN 
    PH_VEG(:,NVT_IRR ) = MIN(2.5, ZALLEN_H(:) )          ! irrigated crops (as C4)
    IF (LFAKETREE) THEN
      PH_VEG(:,NVT_IRR) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(:,NVT_IRR)/ZZREF))**2 + (ZLAI(:)/ZLAI(:))*XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
      PH_VEG(:,NVT_IRR) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(:,NVT_IRR)))
    ENDIF
  ENDIF
ENDIF
PH_VEG(:,NVT_NO  ) = 0.1                    ! no vegetation (smooth)
PH_VEG(:,NVT_ROCK) = 1.                     ! no vegetation (rocks)
PH_VEG(:,NVT_SNOW) = 0.01                   ! no vegetation (snow)
!
PH_VEG(:,:) = MAX(PH_VEG(:,:),0.001)
!
IF (LHOOK) CALL DR_HOOK('MODI_VEG_HEIGHT_FROM_LAI:VEG_HEIGHT_FROM_LAI_1D',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
END FUNCTION VEG_HEIGHT_FROM_LAI_1D
!
!   ###########################################################
    FUNCTION VEG_HEIGHT_FROM_LAI_2D(PLAI,PH_TREE,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PH_VEG)
!   ###########################################################
!!
!!    PURPOSE
!!    -------
!
!     Calculates vegetation height from leaf
!    area index and type of vegetation
!    (most of types; forest and vineyards; grassland)
!              
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      V. Masson and A. Boone          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    25/03/99
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK, NVT_SNOW, NVT_PARK,        &
                                NVT_TEBD, NVT_BONE, NVT_TRBE, NVT_TRBD,      &
                                NVT_TEBE, NVT_TENE, NVT_BOBD, NVT_BOND,      &
                                NVT_SHRB, NVT_C3, NVT_C4, NVT_IRR,           &
                                NVT_GRAS, NVT_BOGR, NVT_TROG, NVT_C3W,       &
                                NVT_C3S, NVT_FLTR, NVT_FLGR
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_TREEDRAG,       ONLY : LTREEDRAG, XALLEN_TERM, XGRASS_H_DNM, LFAKETREE, XHFAKETREE, XFFAKETREE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,   DIMENSION(:,:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:,:),   INTENT(IN) :: PH_TREE      ! height of trees
REAL,   DIMENSION(:,:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,                  INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PVEGTYPE,1),SIZE(PVEGTYPE,2),SIZE(PVEGTYPE,3))  :: PH_VEG          ! vegetation height
!
!*      0.2    declarations of local variables
!

REAL, DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2))                  :: ZALLEN_H ! Allen formula for height
REAL, DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2))                  :: ZLAI     ! LAI for vegetated areas
!
REAL, DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2))                  :: ZAVG_H   ! averaged height
REAL                                                        :: ZZREF    ! reference height        
!
INTEGER                                                     :: JTYPE    ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_VEG_HEIGHT_FROM_LAI:VEG_HEIGHT_FROM_LAI_2D',0,ZHOOK_HANDLE)
!
!-----------------------------------------------------------------
!
ZZREF=10.0
!
PH_VEG(:,:,:)=XUNDEF
!
ZLAI(:,:) = PLAI(:,:)
WHERE ( PVEGTYPE(:,:,NVT_NO  ) + PVEGTYPE(:,:,NVT_ROCK) + PVEGTYPE(:,:,NVT_SNOW) < 1.) 
  ZLAI(:,:) = PLAI(:,:) / (1.-PVEGTYPE(:,:,NVT_NO)-PVEGTYPE(:,:,NVT_ROCK)-PVEGTYPE(:,:,NVT_SNOW))
END WHERE
!
ZALLEN_H(:,:) = 0.
WHERE(PLAI(:,:)/=XUNDEF)
  ZALLEN_H(:,:) = EXP((ZLAI(:,:)-XALLEN_TERM)/(1.3))
END WHERE
!
!
IF (NVT_PARK>0) THEN
  PH_VEG(:,:,NVT_PARK) = ZLAI(:,:) / XGRASS_H_DNM               ! irr. grassland
  IF (LFAKETREE) THEN
    PH_VEG(:,:,NVT_PARK) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(:,:,NVT_PARK)/ZZREF))**2 + (ZLAI(:,:)/ZLAI(:,:))*XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
    PH_VEG(:,:,NVT_PARK) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(:,:,NVT_PARK)))
  ENDIF
ELSEIF (NVT_FLGR>0) THEN
  PH_VEG(:,:,NVT_FLGR) = ZLAI(:,:) / XGRASS_H_DNM 
ENDIF
!
IF (LTREEDRAG) THEN
  PH_VEG(:,:,NVT_TEBD) = ZLAI(:,:) / XGRASS_H_DNM         ! forest
  PH_VEG(:,:,NVT_BONE) = ZLAI(:,:) / XGRASS_H_DNM         ! forest
  PH_VEG(:,:,NVT_TRBE) = ZLAI(:,:) / XGRASS_H_DNM         ! forest
  PH_VEG(:,:,NVT_TRBD) = ZLAI(:,:) / XGRASS_H_DNM         ! forest
  PH_VEG(:,:,NVT_TEBE) = ZLAI(:,:) / XGRASS_H_DNM         ! forest
  PH_VEG(:,:,NVT_TENE) = ZLAI(:,:) / XGRASS_H_DNM         ! forest
  PH_VEG(:,:,NVT_BOBD) = ZLAI(:,:) / XGRASS_H_DNM         ! forest
  PH_VEG(:,:,NVT_BOND) = ZLAI(:,:) / XGRASS_H_DNM         ! forest
  PH_VEG(:,:,NVT_SHRB) = ZLAI(:,:) / XGRASS_H_DNM         ! forest  
  IF (NVT_FLTR>0) PH_VEG(:,:,NVT_FLTR) = ZLAI(:,:) / XGRASS_H_DNM
ELSE
  PH_VEG(:,:,NVT_TEBD) = PH_TREE(:,:)           ! forest
  PH_VEG(:,:,NVT_BONE) = PH_TREE(:,:)           ! forest
  PH_VEG(:,:,NVT_TRBE) = PH_TREE(:,:)           ! forest
  PH_VEG(:,:,NVT_TRBD) = PH_TREE(:,:)           ! forest
  PH_VEG(:,:,NVT_TEBE) = PH_TREE(:,:)           ! forest
  PH_VEG(:,:,NVT_TENE) = PH_TREE(:,:)           ! forest
  PH_VEG(:,:,NVT_BOBD) = PH_TREE(:,:)           ! forest
  PH_VEG(:,:,NVT_BOND) = PH_TREE(:,:)           ! forest
  PH_VEG(:,:,NVT_SHRB) = PH_TREE(:,:)           ! forest   
  IF (NVT_FLTR>0) PH_VEG(:,:,NVT_FLTR) = PH_TREE(:,:)
END IF
PH_VEG(:,:,NVT_GRAS) = ZLAI(:,:) / XGRASS_H_DNM               ! grassland
PH_VEG(:,:,NVT_BOGR) = ZLAI(:,:) / XGRASS_H_DNM               ! boreal grassland
PH_VEG(:,:,NVT_TROG) = ZLAI(:,:) / XGRASS_H_DNM               ! tropical grassland
IF (LFAKETREE) THEN
  PH_VEG(:,:,NVT_GRAS) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(:,:,NVT_GRAS)/ZZREF))**2 + (ZLAI(:,:)/ZLAI(:,:))*XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
  PH_VEG(:,:,NVT_GRAS) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(:,:,NVT_GRAS)))
  PH_VEG(:,:,NVT_BOGR) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(:,:,NVT_BOGR)/ZZREF))**2 + (ZLAI(:,:)/ZLAI(:,:))*XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
  PH_VEG(:,:,NVT_BOGR) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(:,:,NVT_BOGR)))
  PH_VEG(:,:,NVT_TROG) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(:,:,NVT_TROG)/ZZREF))**2 + (ZLAI(:,:)/ZLAI(:,:))*XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
  PH_VEG(:,:,NVT_TROG) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(:,:,NVT_TROG)))
ENDIF
IF(OAGRI_TO_GRASS)THEN
  IF (NVT_C3>0) THEN
    PH_VEG(:,:,NVT_C3  ) = ZLAI(:,:) / XGRASS_H_DNM
    IF (LFAKETREE) THEN
      PH_VEG(:,:,NVT_C3) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(:,:,NVT_C3)/ZZREF))**2 + (ZLAI(:,:)/ZLAI(:,:))*XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
      PH_VEG(:,:,NVT_C3) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(:,:,NVT_C3)))
    ENDIF
  ELSEIF (NVT_C3W>0 .AND. NVT_C3S>0) THEN
    PH_VEG(:,:,NVT_C3W ) = ZLAI(:,:) / XGRASS_H_DNM
    PH_VEG(:,:,NVT_C3S ) = ZLAI(:,:) / XGRASS_H_DNM
    IF (LFAKETREE) THEN
      PH_VEG(:,:,NVT_C3W) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(:,:,NVT_C3W)/ZZREF))**2 + (ZLAI(:,:)/ZLAI(:,:))*XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
      PH_VEG(:,:,NVT_C3W) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(:,:,NVT_C3W)))
      PH_VEG(:,:,NVT_C3S) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(:,:,NVT_C3S)/ZZREF))**2 + (ZLAI(:,:)/ZLAI(:,:))*XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
      PH_VEG(:,:,NVT_C3S) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(:,:,NVT_C3S)))
    ENDIF
  ENDIF
  PH_VEG(:,:,NVT_C4  ) = ZLAI(:,:) / XGRASS_H_DNM
  IF (LFAKETREE) THEN
    PH_VEG(:,:,NVT_C4) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(:,:,NVT_C4)/ZZREF))**2 + (ZLAI(:,:)/ZLAI(:,:))*XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
    PH_VEG(:,:,NVT_C4) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(:,:,NVT_C4)))
  ENDIF
  IF (NVT_IRR>0) THEN 
    PH_VEG(:,:,NVT_IRR ) = ZLAI(:,:) / XGRASS_H_DNM
    IF (LFAKETREE) THEN    
      PH_VEG(:,:,NVT_IRR) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(:,:,NVT_IRR)/ZZREF))**2 + (ZLAI(:,:)/ZLAI(:,:))*XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
      PH_VEG(:,:,NVT_IRR) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(:,:,NVT_IRR)))
    ENDIF
  ENDIF
ELSE
  IF (NVT_C3>0) THEN
    PH_VEG(:,:,NVT_C3  ) = MIN(1. , ZALLEN_H(:,:) )          ! cultures
    IF (LFAKETREE) THEN    
      PH_VEG(:,:,NVT_C3) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(:,:,NVT_C3)/ZZREF))**2 + (ZLAI(:,:)/ZLAI(:,:))*XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
      PH_VEG(:,:,NVT_C3) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(:,:,NVT_C3)))
    ENDIF
  ELSEIF (NVT_C3W>0 .AND. NVT_C3S>0) THEN
    PH_VEG(:,:,NVT_C3S ) = MIN(2.5, ZALLEN_H(:,:) )
    PH_VEG(:,:,NVT_C3W ) = MIN(2.5, ZALLEN_H(:,:) )
    IF (LFAKETREE) THEN
      PH_VEG(:,:,NVT_C3S) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(:,:,NVT_C3S)/ZZREF))**2 + (ZLAI(:,:)/ZLAI(:,:))*XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
      PH_VEG(:,:,NVT_C3S) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(:,:,NVT_C3S)))
      PH_VEG(:,:,NVT_C3W) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(:,:,NVT_C3W)/ZZREF))**2 + (ZLAI(:,:)/ZLAI(:,:))*XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
      PH_VEG(:,:,NVT_C3W) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(:,:,NVT_C3W)))
    ENDIF
  ENDIF
  PH_VEG(:,:,NVT_C4  ) = MIN(2.5, ZALLEN_H(:,:) )          ! C4 types
  IF (LFAKETREE) THEN  
    PH_VEG(:,:,NVT_C4) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(:,:,NVT_C4)/ZZREF))**2 + (ZLAI(:,:)/ZLAI(:,:))*XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
    PH_VEG(:,:,NVT_C4) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(:,:,NVT_C4)))
  ENDIF
  IF (NVT_IRR>0) THEN
    PH_VEG(:,:,NVT_IRR ) = MIN(2.5, ZALLEN_H(:,:) )          ! irrigated crops (as C4)
    IF (LFAKETREE) THEN  
      PH_VEG(:,:,NVT_IRR) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(:,:,NVT_IRR)/ZZREF))**2 + (ZLAI(:,:)/ZLAI(:,:))*XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
      PH_VEG(:,:,NVT_IRR) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(:,:,NVT_IRR)))
    ENDIF
  ENDIF
ENDIF
PH_VEG(:,:,NVT_NO  ) = 0.1                          ! no vegetation (smooth)
PH_VEG(:,:,NVT_ROCK) = 1.                           ! no vegetation (rocks)
PH_VEG(:,:,NVT_SNOW) = 0.01                         ! no vegetation (snow)
!
PH_VEG(:,:,:) = MAX(PH_VEG(:,:,:),0.001)
!
IF (LHOOK) CALL DR_HOOK('MODI_VEG_HEIGHT_FROM_LAI:VEG_HEIGHT_FROM_LAI_2D',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
END FUNCTION VEG_HEIGHT_FROM_LAI_2D
!
!
!
!   ###########################################################
    FUNCTION VEG_HEIGHT_FROM_LAI_VEGTYPE(PLAI,PH_TREE,OAGRI_TO_GRASS) RESULT(PH_VEG)
!   ###########################################################
!!
!!    PURPOSE
!!    -------
!
!     Calculates vegetation height from leaf
!    area index and type of vegetation for each patch
!    (most of types; forest and vineyards; grassland)
!              
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!        F.Solmon
!!      V. Masson and A. Boone          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    25/03/99
!!      
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK, NVT_SNOW, NVT_PARK,        &
                                NVT_TEBD, NVT_BONE, NVT_TRBE, NVT_TRBD,      &
                                NVT_TEBE, NVT_TENE, NVT_BOBD, NVT_BOND,      &
                                NVT_SHRB, NVT_C3, NVT_C4, NVT_IRR,           &
                                NVT_GRAS, NVT_BOGR, NVT_TROG, NVT_C3W,       &
                                NVT_C3S, NVT_FLTR, NVT_FLGR
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_TREEDRAG,       ONLY : LTREEDRAG, XALLEN_TERM, XGRASS_H_DNM, LFAKETREE, XHFAKETREE, XFFAKETREE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,   DIMENSION(:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:),   INTENT(IN) :: PH_TREE      ! height of trees
LOGICAL,                INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PLAI))  :: PH_VEG          ! vegetation height
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PLAI))     :: ZALLEN_H    ! Allen formula for height
REAL                            :: ZZREF       ! Reference height
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_VEG_HEIGHT_FROM_LAI:VEG_HEIGHT_FROM_LAI_VEGTYPE',0,ZHOOK_HANDLE)
!
!
!-----------------------------------------------------------------
!
ZZREF=10.0
PH_VEG(:) = XUNDEF
!IF (ANY (PLAI(:)/= XUNDEF)) THEN
  ZALLEN_H(:) = XUNDEF
  WHERE (PLAI(:)/= XUNDEF)
    ZALLEN_H(:) = EXP((PLAI(:)-XALLEN_TERM)/(1.3))
  END WHERE
!ENDIF
!
!
IF (NVT_PARK>0) THEN
  IF (PLAI(NVT_PARK)/=XUNDEF) THEN
    PH_VEG(NVT_PARK) = PLAI(NVT_PARK) / XGRASS_H_DNM          ! irr. grasslands
    IF (LFAKETREE) THEN
      PH_VEG(NVT_PARK) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(NVT_PARK)/ZZREF))**2 + XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
      PH_VEG(NVT_PARK) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(NVT_PARK)))
    ENDIF
  ENDIF
ELSEIF (NVT_FLGR>0) THEN
  IF (PLAI(NVT_FLGR)/=XUNDEF) THEN
    PH_VEG(NVT_FLGR) = PLAI(NVT_FLGR) / XGRASS_H_DNM
    IF (LFAKETREE) THEN
      PH_VEG(NVT_FLGR) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(NVT_FLGR)/ZZREF))**2 + XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
      PH_VEG(NVT_FLGR) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(NVT_FLGR)))
    ENDIF
  ENDIF  
ENDIF
IF (LTREEDRAG) THEN
  IF (PLAI(NVT_TEBD)/=XUNDEF) PH_VEG(NVT_TEBD) = PLAI(NVT_TEBD) / XGRASS_H_DNM        ! broadleaf forest
  IF (PLAI(NVT_BONE)/=XUNDEF) PH_VEG(NVT_BONE) = PLAI(NVT_BONE) / XGRASS_H_DNM        ! coniferous forest
  IF (PLAI(NVT_TRBE)/=XUNDEF) PH_VEG(NVT_TRBE) = PLAI(NVT_TRBE) / XGRASS_H_DNM        ! euqatorial forest
  IF (PLAI(NVT_TRBD)/=XUNDEF) PH_VEG(NVT_TRBD) = PLAI(NVT_TRBD) / XGRASS_H_DNM        ! broadleaf forest
  IF (PLAI(NVT_TEBE)/=XUNDEF) PH_VEG(NVT_TEBE) = PLAI(NVT_TEBE) / XGRASS_H_DNM        ! coniferous forest
  IF (PLAI(NVT_TENE)/=XUNDEF) PH_VEG(NVT_TENE) = PLAI(NVT_TENE) / XGRASS_H_DNM        ! euqatorial forest
  IF (PLAI(NVT_BOBD)/=XUNDEF) PH_VEG(NVT_BOBD) = PLAI(NVT_BOBD) / XGRASS_H_DNM        ! broadleaf forest
  IF (PLAI(NVT_BOND)/=XUNDEF) PH_VEG(NVT_BOND) = PLAI(NVT_BOND) / XGRASS_H_DNM        ! coniferous forest
  IF (PLAI(NVT_SHRB)/=XUNDEF) PH_VEG(NVT_SHRB) = PLAI(NVT_SHRB) / XGRASS_H_DNM        ! euqatorial forest  
  IF (NVT_FLTR>0) THEN
    IF (PLAI(NVT_FLTR)/=XUNDEF) PH_VEG(NVT_FLTR) = PLAI(NVT_FLTR) / XGRASS_H_DNM
  ENDIF
ELSE
  IF (PH_TREE(NVT_TEBD)/=XUNDEF) PH_VEG(NVT_TEBD) = PH_TREE(NVT_TEBD)          ! broadleaf forest
  IF (PH_TREE(NVT_BONE)/=XUNDEF) PH_VEG(NVT_BONE) = PH_TREE(NVT_BONE)          ! coniferous forest
  IF (PH_TREE(NVT_TRBE)/=XUNDEF) PH_VEG(NVT_TRBE) = PH_TREE(NVT_TRBE)          ! euqatorial forest
  IF (PH_TREE(NVT_TRBD)/=XUNDEF) PH_VEG(NVT_TRBD) = PH_TREE(NVT_TRBD)          ! broadleaf forest
  IF (PH_TREE(NVT_TEBE)/=XUNDEF) PH_VEG(NVT_TEBE) = PH_TREE(NVT_TEBE)          ! coniferous forest
  IF (PH_TREE(NVT_TENE)/=XUNDEF) PH_VEG(NVT_TENE) = PH_TREE(NVT_TENE)          ! euqatorial forest
  IF (PH_TREE(NVT_BOBD)/=XUNDEF) PH_VEG(NVT_BOBD) = PH_TREE(NVT_BOBD)          ! broadleaf forest
  IF (PH_TREE(NVT_BOND)/=XUNDEF) PH_VEG(NVT_BOND) = PH_TREE(NVT_BOND)          ! coniferous forest
  IF (PH_TREE(NVT_SHRB)/=XUNDEF) PH_VEG(NVT_SHRB) = PH_TREE(NVT_SHRB)          ! euqatorial forest  
  IF (NVT_FLTR>0) THEN
    IF (PH_TREE(NVT_FLTR)/=XUNDEF) PH_VEG(NVT_FLTR) = PH_TREE(NVT_FLTR)
  ENDIF
END IF
IF (PLAI(NVT_GRAS)/=XUNDEF) THEN
PH_VEG(NVT_GRAS) = PLAI(NVT_GRAS) / XGRASS_H_DNM          ! grassland
  IF (LFAKETREE) THEN
    PH_VEG(NVT_GRAS) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(NVT_GRAS)/ZZREF))**2 + XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
    PH_VEG(NVT_GRAS) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(NVT_GRAS)))
  ENDIF
ENDIF
IF (PLAI(NVT_BOGR)/=XUNDEF) THEN
PH_VEG(NVT_BOGR) = PLAI(NVT_BOGR) / XGRASS_H_DNM          ! boreal grassland
  IF (LFAKETREE) THEN
    PH_VEG(NVT_BOGR) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(NVT_BOGR)/ZZREF))**2 + XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
    PH_VEG(NVT_BOGR) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(NVT_BOGR)))
  ENDIF
ENDIF
IF (PLAI(NVT_TROG)/=XUNDEF) THEN 
  PH_VEG(NVT_TROG) = PLAI(NVT_TROG) / XGRASS_H_DNM          ! tropical grassland
  IF (LFAKETREE) THEN  
    PH_VEG(NVT_TROG) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(NVT_TROG)/ZZREF))**2 + XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
    PH_VEG(NVT_TROG) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(NVT_TROG)))
  ENDIF
ENDIF
IF(OAGRI_TO_GRASS)THEN
  IF (NVT_C3>0) THEN
    IF (PLAI(NVT_C3  )/=XUNDEF) THEN 
      PH_VEG(NVT_C3  ) = PLAI(NVT_C3)  / XGRASS_H_DNM  ! cultures
      IF (LFAKETREE) THEN
        PH_VEG(NVT_C3) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(NVT_C3)/ZZREF))**2 + XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
        PH_VEG(NVT_C3) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(NVT_C3)))
      ENDIF
    ENDIF
  ELSEIF (NVT_C3W>0 .AND. NVT_C3S>0) THEN
    IF (PLAI(NVT_C3W )/=XUNDEF) THEN
      PH_VEG(NVT_C3W ) = PLAI(NVT_C3W) / XGRASS_H_DNM
      IF (LFAKETREE) THEN
        PH_VEG(NVT_C3W) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(NVT_C3W)/ZZREF))**2 + XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
        PH_VEG(NVT_C3W) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(NVT_C3W)))
      ENDIF
    ENDIF
    IF (PLAI(NVT_C3S )/=XUNDEF) THEN
      PH_VEG(NVT_C3S ) = PLAI(NVT_C3S) / XGRASS_H_DNM
      IF (LFAKETREE) THEN
        PH_VEG(NVT_C3S) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(NVT_C3S)/ZZREF))**2 + XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
        PH_VEG(NVT_C3S) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(NVT_C3S)))
      ENDIF
    ENDIF
  ENDIF
  IF (PLAI(NVT_C4  )/=XUNDEF) THEN
  PH_VEG(NVT_C4  ) = PLAI(NVT_C4)  / XGRASS_H_DNM  ! C4 types
    IF (LFAKETREE) THEN
    PH_VEG(NVT_C4) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(NVT_C4)/ZZREF))**2 + XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
    PH_VEG(NVT_C4) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(NVT_C4)))
    ENDIF
  ENDIF
  IF (NVT_IRR>0) THEN
    IF (PLAI(NVT_IRR )/=XUNDEF) PH_VEG(NVT_IRR ) = PLAI(NVT_IRR) / XGRASS_H_DNM  ! irrigated crops (as C4)
      IF (LFAKETREE) THEN
        PH_VEG(NVT_IRR) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(NVT_IRR)/ZZREF))**2 + XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
        PH_VEG(NVT_IRR) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(NVT_IRR)))
      ENDIF
  ENDIF
ELSE
  IF (NVT_C3>0) THEN
    IF (ZALLEN_H(NVT_C3  )/=XUNDEF) THEN
      PH_VEG(NVT_C3  ) = MIN(1. , ZALLEN_H(NVT_C3) )  ! cultures
      IF (LFAKETREE) THEN
        PH_VEG(NVT_C3) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(NVT_C3)/ZZREF))**2 + XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
        PH_VEG(NVT_C3) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(NVT_C3)))
      ENDIF
    ENDIF
  ELSEIF (NVT_C3W>0 .AND. NVT_C3S>0) THEN
    IF (ZALLEN_H(NVT_C3W )/=XUNDEF) THEN
    PH_VEG(NVT_C3W ) = MIN(1. , ZALLEN_H(NVT_C3W) )
      IF (LFAKETREE) THEN
        PH_VEG(NVT_C3W) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(NVT_C3W)/ZZREF))**2 + XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
        PH_VEG(NVT_C3W) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(NVT_C3W)))
      ENDIF
    ENDIF 
    IF (ZALLEN_H(NVT_C3S )/=XUNDEF) THEN
    PH_VEG(NVT_C3S ) = MIN(1. , ZALLEN_H(NVT_C3S) )
      IF (LFAKETREE) THEN
        PH_VEG(NVT_C3S) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(NVT_C3S)/ZZREF))**2 + XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
        PH_VEG(NVT_C3S) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(NVT_C3S)))
      ENDIF
    ENDIF
  ENDIF
  IF (ZALLEN_H(NVT_C4  )/=XUNDEF) THEN
  PH_VEG(NVT_C4  ) = MIN(2.5, ZALLEN_H(NVT_C4) )  ! C4 types
    IF (LFAKETREE) THEN
      PH_VEG(NVT_C4) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(NVT_C4)/ZZREF))**2 + XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
      PH_VEG(NVT_C4) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(NVT_C4)))
    ENDIF
  ENDIF
  IF (NVT_IRR>0) THEN
    IF (ZALLEN_H(NVT_IRR )/=XUNDEF) THEN
    PH_VEG(NVT_IRR ) = MIN(2.5, ZALLEN_H(NVT_IRR) ) ! irrigated crops (as C4)
      IF (LFAKETREE) THEN
        PH_VEG(NVT_IRR) = (1-XFFAKETREE) / (LOG(0.13*PH_VEG(NVT_IRR)/ZZREF))**2 + XFFAKETREE / (LOG(0.13*XHFAKETREE/ZZREF))**2
        PH_VEG(NVT_IRR) = ZZREF / 0.13 * EXP (-1./SQRT(PH_VEG(NVT_IRR)))
      ENDIF
    ENDIF
  ENDIF
ENDIF
PH_VEG(NVT_NO  ) = 0.1                          ! no vegetation (smooth)
PH_VEG(NVT_ROCK) = 1.                           ! no vegetation (rocks)
PH_VEG(NVT_SNOW) = 0.01                         ! no vegetation (snow)
!
PH_VEG(:) = MAX(PH_VEG(:),0.001)

!
IF (LHOOK) CALL DR_HOOK('MODI_VEG_HEIGHT_FROM_LAI:VEG_HEIGHT_FROM_LAI_VEGTYPE',1,ZHOOK_HANDLE)
!
END FUNCTION VEG_HEIGHT_FROM_LAI_VEGTYPE
!
