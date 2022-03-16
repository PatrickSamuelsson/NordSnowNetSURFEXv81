!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE TREAT_GLOBAL_LAKE_DEPTH (DTCO, UG, U, USS, &
!                                          HPROGRAM, LKEEP_BAYS, &
                                          HPROGRAM,&
                                          XMAX_DEPTH,XMIN_DEPTH,&
                                          PDEPTH,KSTATUS)
!     ##############################################################
!
!!**** *TREAT_GLOBAL_LAKE_DEPTH* monitor for averaging and interpolations of ISBA physiographic fields
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
!!    S. Faroux        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    17/02/11
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
USE MODD_PGD_GRID,       ONLY : NL
!ek_beg
!USE MODD_PGDWORK,        ONLY : XALL, NSIZE_ALL, XSUMVAL, NSIZE
USE MODD_PGDWORK,        ONLY : XALL, NSIZE_ALL, XSUMVAL, NSIZE, XFRAC_LDB
!ek_end
USE MODD_DATA_LAKE,      ONLY : CLAKELDB, CSTATUSLDB, NGRADDEPTH_LDB, NGRADSTATUS_LDB 
!
USE MODI_GET_LUOUT
USE MODI_TREAT_FIELD
!ek USE MODI_PACK_SAME_RANK
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!ek USE MODI_GET_SURF_MASK_n
!ek USE MODI_GET_TYPE_DIM_n
!ek_beg
USE MODI_INTERPOL_FIELD
!ek_end
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM  ! Type of program
!ek_beg
!LOGICAL, INTENT(IN) :: LKEEP_BAYS
REAL, INTENT(IN) :: XMAX_DEPTH, XMIN_DEPTH
!ek_end
REAL, DIMENSION(:),INTENT(OUT):: PDEPTH    ! physiographic field
INTEGER, DIMENSION(:),INTENT(OUT):: KSTATUS   ! physiographic field
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!ek INTEGER                        :: ILU    ! expected physical size of full surface array
INTEGER                        :: ILUOUT ! output listing logical unit
!ek INTEGER, DIMENSION(:), POINTER :: IMASK  ! mask for packing from complete field to nature field
INTEGER                        :: IDIM   !
INTEGER                        :: JI
!
!ek CHARACTER(LEN=6)    :: YMASK
INTEGER, DIMENSION(NL) :: ISTATUS
REAL, DIMENSION(NL,1) :: ZDEPTH, ZSTATUS    ! physiographic field on full grid
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('TREAT_GLOBAL_LAKE_DEPTH',0,ZHOOK_HANDLE)
ZDEPTH (:,:) = XUNDEF
ZSTATUS(:,:) = XUNDEF
!-------------------------------------------------------------------------------
!
!*    2.      Output listing logical unit
!             ---------------------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*    4.      Averages the field
!             ------------------
!
!ek_beg
ALLOCATE(XFRAC_LDB (NL))
XFRAC_LDB(:) = XUNDEF
!ek_end

ALLOCATE(NSIZE_ALL(U%NDIM_FULL,1))
ALLOCATE(XALL     (U%NDIM_FULL,NGRADDEPTH_LDB,1))
!
!
NSIZE_ALL(:,:) = 0.
XALL   (:,:,:) = 0.
!
 CALL TREAT_FIELD(UG, U, USS, &
                  HPROGRAM,'SURF  ','DIRECT','A_LDBD', CLAKELDB,   &
                 'water depth         ',ZDEPTH      )
!
DEALLOCATE(XSUMVAL)
DEALLOCATE(NSIZE)
!
ALLOCATE(NSIZE_ALL(U%NDIM_FULL,1))
ALLOCATE(XALL    (U%NDIM_FULL,NGRADSTATUS_LDB,1))
!
NSIZE_ALL  (:,:) = 0.
XALL     (:,:,:) = 0.
!
 CALL TREAT_FIELD(UG, U, USS, &
                  HPROGRAM,'SURF  ','DIRECT','A_LDBS', CSTATUSLDB,  &
                 'water status        ',ZSTATUS             )

WHERE(ZSTATUS(:,1).NE.XUNDEF)
  ISTATUS = NINT(ZSTATUS(:,1))
ELSEWHERE
  ISTATUS=NUNDEF
END WHERE

DEALLOCATE(XSUMVAL)

! ek_beg
!ek: limitation for the lake depth (compulsory, if FLake is used)
DO JI = 1, SIZE(ZDEPTH)
  IF(ISTATUS(JI).NE.0.AND.ISTATUS(JI).NE.NUNDEF) THEN
    ZDEPTH(JI,1)=MAX(ZDEPTH(JI,1),XMIN_DEPTH)
    ZDEPTH(JI,1)=MIN(ZDEPTH(JI,1),XMAX_DEPTH)
  END IF
END DO

!ek:  Interpolation is possible, but only with the nearest neighbour method (KNPTS=1)
 CALL INTERPOL_FIELD(UG, U, &
         HPROGRAM,ILUOUT,NSIZE(:,1),ZDEPTH(:,1),'water depth         ',KNPTS=1)
 CALL INTERPOL_FIELD(UG, U, &
         HPROGRAM,ILUOUT,NSIZE(:,1),ZSTATUS(:,1),'water status        ',KNPTS=1)
 CALL INTERPOL_FIELD(UG, U, &
         HPROGRAM,ILUOUT,NSIZE(:,1),XFRAC_LDB(:),'ldb water fraction  ',KNPTS=1)

DEALLOCATE(NSIZE)
!ek_end 

!
!-------------------------------------------------------------------------------
!
!*    5.      Consistancy check
!             ------------------
!
!ek_beg
DO JI = 1, SIZE(ZDEPTH)

  IF(U%XWATER(JI).EQ.0.0.AND.XFRAC_LDB(JI).EQ.0.0) THEN 
    CYCLE   
  END IF

  IF(U%XWATER(JI).EQ.0.0.AND.XFRAC_LDB(JI).GT.0.0) THEN 
    ZDEPTH(JI,1) = 0.0
    ISTATUS(JI) = 0
    CYCLE
  END IF

  IF(U%XWATER(JI).GT.0.0.AND.XFRAC_LDB(JI).EQ.0.0) THEN 
    ZDEPTH(JI,1) = 10.0
    ISTATUS(JI) = 1
    CYCLE       
  END IF

END DO
!
!*    6.      Mask for the field
!             ------------------
!
!ek: no masking here, it is moved to pgd_flake
!YMASK='WATER '
! CALL GET_TYPE_DIM_n(DTCO, U, &
!                     YMASK,IDIM)
!IF (IDIM/=SIZE(PDEPTH) .OR. IDIM/=SIZE(KSTATUS)) THEN
!   WRITE(ILUOUT,*)'Wrong dimension of MASK: ',IDIM,SIZE(PDEPTH),SIZE(KSTATUS)
!   CALL ABOR1_SFX('TREAT_GLOBAL_LAKE_DEPTH: WRONG DIMENSION OF MASK')
!ENDIF
!
!ALLOCATE(IMASK(IDIM))
!ILU=0
! CALL GET_SURF_MASK_n(DTCO, U, &
!                      YMASK,IDIM,IMASK,ILU,ILUOUT)
! CALL PACK_SAME_RANK(IMASK,ZDEPTH(:,1),PDEPTH(:))
! CALL PACK_SAME_RANK(IMASK,ISTATUS(:),KSTATUS(:))
!DEALLOCATE(IMASK)
PDEPTH=ZDEPTH(:,1)
KSTATUS=ISTATUS
DEALLOCATE(XFRAC_LDB)
!ek_end
!
IF (LHOOK) CALL DR_HOOK('TREAT_GLOBAL_LAKE_DEPTH',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE TREAT_GLOBAL_LAKE_DEPTH
