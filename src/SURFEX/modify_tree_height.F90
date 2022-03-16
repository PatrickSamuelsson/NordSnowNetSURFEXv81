!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE MODIFY_TREE_HEIGHT (PPAR_H_TREE, PLAT, OCOVER, PCOVER)
!     ################################################
!
!!****  *MODIFY_TREE_HEIGHT* - modify ECOSG tree height
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      P. Samuelsson   *SMHI*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    05/2020 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_TREEDRAG,       ONLY : XSCALE_H_TREE, &
                                XFORLAT1, XFORLAT2, &
                                XFORFRAC1, XFORFRAC2
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
REAL, DIMENSION(:,:),    INTENT(INOUT):: PPAR_H_TREE ! tree height
REAL, DIMENSION(:),      INTENT(IN)   :: PLAT        ! latitude
LOGICAL, DIMENSION(:),   INTENT(IN)   :: OCOVER      ! active covers
REAL, DIMENSION(:,:),    INTENT(IN)   :: PCOVER      ! cover fraction
!
INTEGER   :: JN, JCOVER, NCOVER
REAL, DIMENSION(SIZE(PLAT))    :: ZLATFACT, ZFORFRFACT, ZSUMLAND, &
                                  ZSUMFOREST, ZFRACFOREST
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODIFY_TREE_HEIGHT',0,ZHOOK_HANDLE)
!
! Modify by latitude for MetCoOp domain a la Mariken Homleid
!
!ZLATFACT(:) = 2.6 - 0.03*PLAT(:)
ZLATFACT(:) = XFORLAT1 + XFORLAT2*PLAT(:)
ZLATFACT(:) = MIN(1.1,MAX(0.5,ZLATFACT(:)))
!
! Modify by fraction of forest a la Patrick Samuelsson
!
NCOVER=0
ZSUMLAND(:)=0.0
ZSUMFOREST(:)=0.0

DO JCOVER=1,SIZE(OCOVER)
  IF(OCOVER(JCOVER))THEN
    NCOVER=NCOVER+1
    IF(JCOVER>3 .AND. JCOVER<24)THEN
      ZSUMLAND(:)=ZSUMLAND(:)+PCOVER(:,NCOVER)
    ENDIF
    IF(JCOVER>6 .AND. JCOVER<16 .OR. JCOVER==22)THEN
      ZSUMFOREST(:)=ZSUMFOREST(:)+PCOVER(:,NCOVER)
    ENDIF
  ENDIF
ENDDO

ZFRACFOREST(:)=0.0
WHERE(ZSUMLAND>0.0)
  ZFRACFOREST(:) = ZSUMFOREST(:)/ZSUMLAND(:)
END WHERE

!ZFORFRFACT(:) = 1.5-0.5*ZFRACFOREST(:)
ZFORFRFACT(:) = XFORFRAC1 + XFORFRAC2*ZFRACFOREST(:)

! All factors together

DO JN=1,NVEGTYPE
  WHERE(PPAR_H_TREE(:,JN)/=XUNDEF)
    PPAR_H_TREE(:,JN) = PPAR_H_TREE(:,JN) * XSCALE_H_TREE * ZLATFACT(:) * ZFORFRFACT(:)
  END WHERE
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODIFY_TREE_HEIGHT',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------
!
END SUBROUTINE MODIFY_TREE_HEIGHT

