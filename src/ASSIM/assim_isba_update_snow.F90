!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE ASSIM_ISBA_UPDATE_SNOW (IO, S, NP, NPE, HPROGRAM, KI, PSWE, HTEST )

! ------------------------------------------------------------------------------------------
!  *****************************************************************************************
!
!  Routine to update snow field for ISBA
!  Trygve Aspelien, Separating IO  06/2013
!
!
! ******************************************************************************************
! ------------------------------------------------------------------------------------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_NP_t, ISBA_NPE_t, ISBA_S_t, ISBA_PE_t, ISBA_P_t
!
USE MODD_CSTS,        ONLY : XTT
USE MODD_ASSIM,       ONLY : LSWE, LSWEPSINI, XSWEPSINI, LSWEPSMIN, XSWEPSMIN
USE MODD_DATA_COVER_PAR, ONLY : NVT_SNOW
USE MODD_SURF_PAR,    ONLY : XUNDEF
USE MODD_SNOW_PAR,    ONLY : XANSMIN, XANSMAX, XRHOSMIN, XRHOSMAX
USE MODE_SNOW3L,      ONLY : SNOW3LGRID

USE MODI_SNOW_T_WLIQ_TO_HEAT
USE MODI_PACK_SAME_RANK
USE MODI_ABOR1_SFX
USE MODI_ASSIM_GATHER_WRITE_INCREMENTS
!
USE YOMHOOK,          ONLY : LHOOK,DR_HOOK
USE PARKIND1,         ONLY : JPRB
!
IMPLICIT NONE
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t),       INTENT(INOUT) :: S
TYPE(ISBA_NP_t),      INTENT(INOUT) :: NP
TYPE(ISBA_NPE_t),     INTENT(INOUT) :: NPE
!
CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM  ! program calling surf. schemes
INTEGER,             INTENT(IN)    :: KI
REAL, DIMENSION(KI), INTENT(IN)    :: PSWE
CHARACTER(LEN=2),    INTENT(IN)    :: HTEST     ! must be equal to 'OK'
!
! Logical value that is TRUE if first guess is from patch 1
LOGICAL                            :: LPATCH1
!
!    Declarations of local variables
!
TYPE(ISBA_P_t),      POINTER       :: PK
TYPE(ISBA_PE_t),     POINTER       :: PEK
!
REAL, ALLOCATABLE,   DIMENSION(:)  :: ZSWEGP    ! Grid point average of first guess
REAL, ALLOCATABLE,   DIMENSION(:)  :: ZSWEGP_P  ! Grid point average of first guess

CHARACTER(LEN=2)                :: CP
REAL, ALLOCATABLE, DIMENSION(:) :: ZSWE       ! Patch value of updated snow
REAL, ALLOCATABLE, DIMENSION(:) :: ZSWE_ORIG  ! Patch value of 1. guess
REAL, ALLOCATABLE, DIMENSION(:) :: ZSWEINC 
REAL, ALLOCATABLE, DIMENSION(:) :: ZSWE_IN
!    Addtional snow fields with D95 snow scheme 
REAL, ALLOCATABLE, DIMENSION(:) :: ZSNA
REAL, ALLOCATABLE, DIMENSION(:) :: ZSNR,ZSNR_ORIG
REAL, ALLOCATABLE, DIMENSION(:) :: ZD,ZHEAT,ZD_ORIG
REAL, ALLOCATABLE, DIMENSION(:) :: ZWEIGHT_P
REAL, ALLOCATABLE, DIMENSION(:,:) :: ZDEPTH,ZSWE_FG,ZWEIGHT
LOGICAL,ALLOCATABLE,DIMENSION(:) :: OMASK

REAL,PARAMETER :: ZSNOWCRITD=0.001
INTEGER  :: JL,JP,JI,JII
REAL :: ZSWEMIN,ZSWEMAX,ZSWEMEAN,ZINCMIN,ZINCMAX,ZINCMEAN,ZWEIGHT_LLIM,ZWEIGHT_ULIM
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! ----------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ASSIM_ISBA_UPDATE_SNOW',0,ZHOOK_HANDLE)
!
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('ASSIM_ISBA_n: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF

! Logical value that is TRUE if first guess is from patch 1,
!                       FALSE if first guess is a grid point average
LPATCH1 = .FALSE.

! First guess grid average of SWE
ALLOCATE(ZSWE_FG(KI,IO%NPATCH))
ALLOCATE(ZWEIGHT(KI,IO%NPATCH))
ALLOCATE(ZSWEGP(KI))

ZSWE_FG=0.
ZSWEGP=0.
DO JP=1,IO%NPATCH
  PK  => NP%AL(JP)
  PEK => NPE%AL(JP)
  DO JI=1,PK%NSIZE_P
    ZSWE_FG(PK%NR_P(JI),JP)=SUM(PEK%TSNOW%WSNOW(JI,:))
    ZSWEGP(PK%NR_P(JI))=ZSWEGP(PK%NR_P(JI)) + ZSWE_FG(PK%NR_P(JI),JP)*S%XPATCH(PK%NR_P(JI),JP)
  ENDDO
ENDDO

! Set upper and lower limits on how much patch SWE is alloved to deviate from
! patch 1. Could be extended to have different limits for different patches.
! When first guess is the grid point average, the limits define deviation from grid point average
ZWEIGHT_LLIM=0.8
ZWEIGHT_ULIM=1.25
IF ( PEK%TSNOW%SCHEME == '3-L' ) ZWEIGHT_LLIM=0.25

ZWEIGHT=1.
IF ( IO%NPATCH > 1 ) THEN
  DO JP=2,IO%NPATCH
    DO JI=1,KI
      IF ( ZSWE_FG(JI,1) > 0.0 ) THEN
         IF (ZSWE_FG(JI,JP) > ZSWE_FG(JI,1) ) THEN
           ZWEIGHT(JI,JP)=MIN((ZSWE_FG(JI,JP)/ZSWE_FG(JI,1)),ZWEIGHT_ULIM)
         ELSEIF (ZSWE_FG(JI,JP) < ZSWE_FG(JI,1) ) THEN
           ZWEIGHT(JI,JP)=MAX((ZSWE_FG(JI,JP)/ZSWE_FG(JI,1)),ZWEIGHT_LLIM)
         ENDIF
      ENDIF
    ENDDO
  ENDDO
ENDIF

! Loop patches
DO JP=1,IO%NPATCH

  PK  => NP%AL(JP)
  PEK => NPE%AL(JP)
  ALLOCATE(ZSWE(PK%NSIZE_P))
  ALLOCATE(ZSWE_ORIG(PK%NSIZE_P))
  ALLOCATE(ZD_ORIG(PK%NSIZE_P))
  ALLOCATE(ZSWEINC(PK%NSIZE_P))
  ALLOCATE(ZSWE_IN(PK%NSIZE_P))
  ALLOCATE(ZSWEGP_P(PK%NSIZE_P))
  ALLOCATE(ZSNA(PK%NSIZE_P))
  ALLOCATE(ZSNR(PK%NSIZE_P))
  ALLOCATE(ZSNR_ORIG(PK%NSIZE_P))
  ALLOCATE(OMASK(PK%NSIZE_P))
  ALLOCATE(ZWEIGHT_P(PK%NSIZE_P))

  ZSWE(:)=0.
  ! Pack ISBA fields to this patch
  CALL PACK_SAME_RANK(PK%NR_P,PSWE,ZSWE_IN)
  CALL PACK_SAME_RANK(PK%NR_P,ZSWEGP,ZSWEGP_P)
  CALL PACK_SAME_RANK(PK%NR_P,ZWEIGHT(:,JP),ZWEIGHT_P)

  ! Default snow depth
  ZD_ORIG=0.
  ZD_ORIG(:)=SUM(PEK%TSNOW%WSNOW(:,:)/PEK%TSNOW%RHO(:,:),MASK=(PEK%TSNOW%RHO /= XUNDEF),DIM=2)
  ! Default SWE
  ZSWE_ORIG(:)=SUM(PEK%TSNOW%WSNOW(:,:),DIM=2)

  ! Find average snow pack density if more than one layer
  IF ( PEK%TSNOW%NLAYER > 1 ) THEN
    IF ( PEK%TSNOW%SCHEME == '3-L' ) THEN
      ZSNR(:)=XUNDEF
      WHERE ( ZD_ORIG(:) /= 0. )
        ZSNR(:)=ZSWE_ORIG(:)/ZD_ORIG(:)
      ENDWHERE
    ELSE
       CALL ABOR1_SFX('ASSIM_ISBA_n: THE SNOW SCHEME '//TRIM(PEK%TSNOW%SCHEME)//' IS A MULTILAYER SCHEME BUT NO CONVERSION FROM DEPTH TO METER IS IMPLEMENTED')
    ENDIF
  ELSE
    ZSNR(:)=PEK%TSNOW%RHO(:,1)
  ENDIF

  ! Convert PSWE from meter to SWE
  IF ( .NOT. LSWE ) THEN
    ! Convert observed snow depth to SWE
    WHERE( ZSNR(:) /= XUNDEF )
        ZSWE_IN(:)=ZSWE_IN(:)*ZSNR(:)
    ELSEWHERE
      ! Invent snow with "average" density
      ZSWE_IN(:)=ZSWE_IN(:) * ( 0.5 * ( XRHOSMIN + XRHOSMAX ))
    ENDWHERE
  ENDIF

  IF ( LPATCH1 ) THEN
  ! Analysed values weighted to keep the ratio of snow at different patches constant
    ZSWE(:) = ZSWE_IN(:)*ZWEIGHT_P(:)

  ELSE
  ! Analysed values weighted to keep the ratio of snow at different patches,
    WHERE (ZSWEGP_P(:) .GT. 1.0E-10 ) 
       ZSWE(:) = ZSWE_IN(:)*ZSWE_ORIG(:)/ZSWEGP_P(:)
    ELSE WHERE
       ZSWE(:) = ZSWE_IN(:)
    END WHERE
  ! BUT within certain limits (LLIM, ULIM) of the grid point average
    WHERE (ZSWE(:) .GT. ZSWE_IN(:)*ZWEIGHT_ULIM )
      ZSWE(:) = ZSWE_IN(:)*ZWEIGHT_ULIM
    END WHERE
    WHERE (ZSWE(:) .LT. ZSWE_IN(:)*ZWEIGHT_LLIM )
      ZSWE(:) = ZSWE_IN(:)*ZWEIGHT_LLIM
    END WHERE
  ENDIF

  ! Set snow=0 where 1. guess = 0 and Ts>0, to avoid that the snow analysis introduce snow where it is no snow.
  WHERE ( ZSWE_ORIG(:)<1.0E-10 .AND. PEK%XTG(:,1) >XTT )
     ZSWE(:)   = 0.0
  END WHERE

  ! Set initial SWE = XSWEPSMIN on vegetation type PERMANENT SNOW 
  IF ( LSWEPSINI ) THEN
    WHERE (PK%XVEGTYPE_PATCH(:,NVT_SNOW) > 0.5)
      ZSWE(:) = XSWEPSINI*PK%XVEGTYPE_PATCH(:,NVT_SNOW)
    ENDWHERE
  ENDIF

  ! Make sure that the SWE >= XSWEPSMIN on vegetation type PERMANENT SNOW 
  IF ( LSWEPSMIN ) THEN
    WHERE ( PK%XVEGTYPE_PATCH(:,NVT_SNOW) > 0.75 .AND. ZSWE(:) < XSWEPSMIN*PK%XVEGTYPE_PATCH(:,NVT_SNOW))
        ZSWE(:) = XSWEPSMIN*PK%XVEGTYPE_PATCH(:,NVT_SNOW)
    ENDWHERE
  ENDIF

  ! Snow albedo and density are given initial values in points  
  ! which get initial snow in the snow analysis
  OMASK=.FALSE.
  ZSNA(:)=PEK%TSNOW%ALB(:)
  ZSNR_ORIG(:)=ZSNR(:)
  WHERE ( ZSWE_ORIG(:) < 1.0E-10 .AND. ZSWE(:)>= 1.0E-10 )
    ZSNA(:)    = 0.5 * ( XANSMIN + XANSMAX )
    ZSNR(:)    = 0.5 * ( XRHOSMIN + XRHOSMAX )
    ! Create a mask for new snow points
    OMASK(:)=.TRUE.
  END WHERE

  IF ( JP < 10 ) THEN
   WRITE(CP(1:1),'(I1)') JP
   CP(2:2)=" "
  ELSE
   WRITE(CP(1:2),'(I2)') JP 
  ENDIF
  CALL ASSIM_GATHER_WRITE_INCREMENTS(HPROGRAM,"WSN_VEG_P"//CP,ZSWE_ORIG,ZSWE,XUNDEF,LSTAT=.TRUE.)
  CALL ASSIM_GATHER_WRITE_INCREMENTS(HPROGRAM,"ASN_VEG_P"//CP,PEK%TSNOW%ALB(:),ZSNA,XUNDEF,LSTAT=.TRUE.)
  CALL ASSIM_GATHER_WRITE_INCREMENTS(HPROGRAM,"RSN_VEG_P"//CP,ZSNR_ORIG,ZSNR,XUNDEF,LSTAT=.TRUE.)

  IF ( PEK%TSNOW%SCHEME == 'D95' ) THEN

    ! Update snow/albedo/density
    PEK%TSNOW%ALB(:)=ZSNA(:)

    ! Only modify density for new snow
    WHERE ( OMASK(:) )
      PEK%TSNOW%RHO(:,1)=ZSNR(:)
    ENDWHERE

    ! Set SWE
    PEK%TSNOW%WSNOW(:,1)=ZSWE(:)

    ! Find modified snow depth in grid points
    ALLOCATE(ZD(PK%NSIZE_P))
    ZD=0.
    WHERE ( PEK%TSNOW%RHO(:,1) /= XUNDEF )
      ZD(:)=PEK%TSNOW%WSNOW(:,1)/PEK%TSNOW%RHO(:,1)
    ENDWHERE
    ! Print snow depth increments
    CALL ASSIM_GATHER_WRITE_INCREMENTS(HPROGRAM,"DSN_VEG_P"//CP,ZD_ORIG,ZD,XUNDEF,LSTAT=.TRUE.)
    DEALLOCATE(ZD)

  ! ISBA-ES need heat and possible rescaling of layers
  ELSE IF ( PEK%TSNOW%SCHEME == '3-L' .OR. PEK%TSNOW%SCHEME =='1-L' ) THEN

    ! Update snow/albedo/density
    PEK%TSNOW%ALB(:)=ZSNA(:)

    ! Initialize density and heat for new snow
    ALLOCATE(ZHEAT(PK%NSIZE_P))   
    DO JL=1,PEK%TSNOW%NLAYER
      WHERE ( OMASK(:))
        PEK%TSNOW%RHO(:,JL)=ZSNR(:)
      ENDWHERE
      CALL SNOW_T_WLIQ_TO_HEAT(ZHEAT,PEK%TSNOW%RHO(:,JL),PEK%XTG(:,1))
      WHERE ( OMASK(:))
        PEK%TSNOW%HEAT(:,JL)=ZHEAT(:)
        PEK%TSNOW%AGE(:,JL)=0
      ENDWHERE

    ENDDO
    DEALLOCATE(ZHEAT)

    ! Find modified snow depth in grid points
    ALLOCATE(ZD(PK%NSIZE_P))
    ZD=0.
    WHERE ( ZSNR(:) /= XUNDEF )
      ZD(:)=ZSWE(:)/ZSNR(:)
    ENDWHERE

    !* Rescale snow pack after update for multilayer scheme
    IF ( PEK%TSNOW%SCHEME == '3-L' ) THEN
      ALLOCATE(ZDEPTH(PK%NSIZE_P,PEK%TSNOW%NLAYER))
      CALL SNOW3LGRID(ZDEPTH(:,:),ZD(:))
      !* snow content profile for each grid level
      DO JL=1,PEK%TSNOW%NLAYER
        WHERE(ZSNR(:)/=XUNDEF.AND.ZD(:)>0.)
          PEK%TSNOW%WSNOW(:,JL) = ZSNR(:) * ZDEPTH(:,JL)
        ELSEWHERE(ZSNR(:)==XUNDEF.OR.ZD(:)==0.0)
          PEK%TSNOW%WSNOW(:,JL) = 0.0
        END WHERE
      END DO
      DEALLOCATE(ZDEPTH)
    ENDIF

    ! Remove very small values to keep ISBA-ES stable
    ZD=0.
    ZD(:)=SUM(PEK%TSNOW%WSNOW(:,:)/PEK%TSNOW%RHO(:,:),MASK=(PEK%TSNOW%RHO/=XUNDEF),DIM=2)
    DO JI = 1,PK%NSIZE_P
      IF( ZD(JI) < ZSNOWCRITD ) THEN
        IF ( ZD(JI) > 0 ) THEN
          WRITE(*,*) 'Found a possible critical point for patch ',JP,' and ZD ',ZD(JI),JI,PK%NSIZE_P
        ENDIF
        PEK%TSNOW%WSNOW(JI,:) = 0.0
        PEK%TSNOW%RHO(JI,:)   = XUNDEF
        PEK%TSNOW%ALB(JI)     = XUNDEF
        PEK%TSNOW%HEAT(JI,:)  = XUNDEF
        PEK%TSNOW%AGE(JI,:)   = XUNDEF
      END IF
    ENDDO

    ! Finished snow pack
    ZD=0.
    ZD(:)=SUM(PEK%TSNOW%WSNOW(:,:)/PEK%TSNOW%RHO(:,:),MASK=(PEK%TSNOW%RHO/=XUNDEF),DIM=2)

    ! Print snow depth increments
    CALL ASSIM_GATHER_WRITE_INCREMENTS(HPROGRAM,"DSN_VEG_P"//CP,ZD_ORIG,ZD,XUNDEF,LSTAT=.TRUE.)
    DEALLOCATE(ZD)

  ELSE
    CALL ABOR1_SFX('ASSIM_ISBA_n: THE SNOW SCHEME '//TRIM(PEK%TSNOW%SCHEME)//&
       & ' HAS NO SWE UPDATE IMPLEMENTED')
  ENDIF

  DEALLOCATE(ZWEIGHT_P)
  DEALLOCATE(OMASK)
  DEALLOCATE(ZSWE)
  DEALLOCATE(ZSWE_ORIG)
  DEALLOCATE(ZD_ORIG)
  DEALLOCATE(ZSWEINC)
  DEALLOCATE(ZSWE_IN)
  DEALLOCATE(ZSWEGP_P)
  DEALLOCATE(ZSNA)
  DEALLOCATE(ZSNR)
  DEALLOCATE(ZSNR_ORIG)
ENDDO

DEALLOCATE(ZSWE_FG)
DEALLOCATE(ZWEIGHT)
DEALLOCATE(ZSWEGP)

!
! -------------------------------------------------------------------------------------
 IF (LHOOK) CALL DR_HOOK('ASSIM_ISBA_UPDATE_SNOW',1,ZHOOK_HANDLE)
 END SUBROUTINE ASSIM_ISBA_UPDATE_SNOW

