MODULE MODE_SEAICE_SICE
USE MODD_SURF_PAR, ONLY: XUNDEF
USE YOMHOOK,       ONLY: LHOOK, DR_HOOK
USE PARKIND1,      ONLY: JPRB
IMPLICIT NONE
PUBLIC

INTEGER, PARAMETER, PRIVATE :: &
  SICE_ALBEDO_SIMPLE   = 0, &
  SICE_ALBEDO_REMO     = 1, &
  SICE_ALBEDO_HADCM3   = 2, &
  SICE_ALBEDO_HIRHAM   = 3, &
  SICE_ALBEDO_PEROVICH = 4

TYPE, PUBLIC :: SICE_CONFIG_t
  LOGICAL :: LICE_HAS_SNOW = .FALSE.
  LOGICAL :: LICE_MASS_BALANCE = .FALSE.

  INTEGER :: NICE_ALBEDO = SICE_ALBEDO_HIRHAM

  REAL :: XOCEAN_HEAT_FLUX = 4.
  REAL :: XNEW_ICE_THK = 0.1
END TYPE SICE_CONFIG_t

TYPE, PUBLIC :: SEA_ICE_FORCING_t
  INTEGER :: KSIZE
  REAL, POINTER, DIMENSION( : ) :: &
    RHOA,           &   !< Air density.
    PSURF,          &   !< Surface pressure.
    WIND,           &   !< Wind speed.

    UREF,           &   !< Reference height for temperatrue [m]
    ZREF,           &   !< Reference height for wind [m]

    TA,             &   !< Air temperature
    QA,             &   !< Specific humidity

    ZENITH,         &   !< Zenithal angle
    SW,             &   !< Shortwave radiation flux at the surface.
    LW,             &   !< Longwave radiation flux at the surface.

    PRATE_R,        &   !< Rainfall rate.
    PRATE_S,        &   !< Snowfall rate.

    EXNS,           &   !< Exner function at the surface.
    EXNA,           &   !< Exner function at the atmospheric forcing level.
    CH,             &
    PPEW_A_COEF,    &   !< Implicit coupling A-coefficient for wind.
    PPEW_B_COEF,    &   !< Implicit coupling B-coefficient for wind.
    PPET_A_COEF,    &   !< Implicit coupling A-coefficient for temperature.
    PPEQ_A_COEF,    &   !< Implicit coupling A-coefficient for humidity.
    PPET_B_COEF,    &   !< Implicit coupling B-coefficient for temperature.
    PPEQ_B_COEF         !< Implicit coupling B-coefficient for humidity.
END TYPE SEA_ICE_FORCING_t

INTEGER, PARAMETER :: MF_GRID = 1,  &
                      MF_LAYER= 2

!< Model field descriptor, stores meta information about model variable and
!! pointer to that variable. Descriptors are used for operations which involves
!! all fields of some model. These operations are IO and masking. Current
!! version of the model_descriptor can store single and multilayer fields.
TYPE :: MODEL_FIELD
  CHARACTER( LEN = 64 ) :: CNAME      = '' !< Name of model field for IO operations.
  CHARACTER( LEN = 64 ) :: CCOMMENT   = '' !< Comment for model field record.
  CHARACTER( LEN = 16 ) :: CUNITS     = '' !< Model field units description.

  INTEGER               :: NCONFIG(2) = [0,0]   !< Model grid configuration.
  LOGICAL               :: LDIAG      = .FALSE. !< Diagnostic fields may be written to separate files.
  LOGICAL               :: LINTERNAL  = .FALSE. !< Internal model fields not used in IO operations.
  LOGICAL               :: LDEPENDENT = .FALSE. !< This field is just a pointer to another field, should not be modified

  REAL, POINTER         :: P1(:)      => NULL() !< Pointer to model grid.
  REAL, POINTER         :: P2(:,:)    => NULL() !< Pointer to multilayer model grid.
  REAL                  :: XDEFAULT   =  XUNDEF !< Default dummy value for described model variable.
END TYPE MODEL_FIELD

CONTAINS

SUBROUTINE PACK( MF, MASK )
IMPLICIT NONE
  TYPE(MODEL_FIELD), INTENT(INOUT) :: MF(:) !< Model fields
  INTEGER, INTENT(IN) :: MASK(:) !< Mask for selecting working points.

  INTEGER :: I
  REAL( KIND = JPRB ) :: ZHOOK_HANDLE

  IF( LHOOK ) CALL DR_HOOK( 'MODE_SEAICE_SICE:PACK', 0, ZHOOK_HANDLE )

  DO I = 1, SIZE( MF )
    IF(MF(I)%LDEPENDENT) CYCLE
    IF( SIZE(MASK) == 1 ) THEN ! One-point masking case
      IF( MASK(1) == 1 ) CYCLE ! If mask points to first element of the masked array, masking is not needed
      ! If masking is needed we swap masked value with value of the first element of the array
      IF(ASSOCIATED(MF(I)%P1)) MF(I)%P1( [1,MASK(1)]    ) = MF(I)%P1( [MASK(1),1]    )
      IF(ASSOCIATED(MF(I)%P2)) MF(I)%P2( [1,MASK(1)], : ) = MF(I)%P2( [MASK(1),1], : )
    ELSE
      ! Values, which should be masked are selected through the vector subscript
      IF(ASSOCIATED(MF(I)%P1)) MF(I)%P1( :SIZE(MASK)    ) = MF(I)%P1( MASK    )
      IF(ASSOCIATED(MF(I)%P2)) MF(I)%P2( :SIZE(MASK), : ) = MF(I)%P2( MASK, : )
    END IF
  END DO

  IF( LHOOK ) CALL DR_HOOK( 'MODE_SEAICE_SICE:PACK', 1, ZHOOK_HANDLE )
END SUBROUTINE PACK

SUBROUTINE UNPACK( MF, MASK )
IMPLICIT NONE
  TYPE(MODEL_FIELD), INTENT(INOUT) :: MF(:) !< Model fields
  INTEGER, INTENT(IN) :: MASK(:) !< Mask for selecting working points.

  LOGICAL, ALLOCATABLE :: G_CLEANUP_MASK(:)
  INTEGER :: N_CLEANUP_SIZE

  INTEGER :: I,J,K
  REAL( KIND = JPRB ) :: ZHOOK_HANDLE

  ! FIXME: Handling of empty chuncks.
  !IF(IS_EMPTY) RETURN

  IF( LHOOK ) CALL DR_HOOK( 'MODE_SEAICE_SICE:UNPACK', 0, ZHOOK_HANDLE )

  DO I = 1, SIZE( MF )
    IF(MF(I)%LDEPENDENT) CYCLE
    IF( SIZE(MASK) == 1 ) THEN ! One-point unmasking
      IF( MASK(1) == 1 ) CYCLE
      ! Just swap first element of the array with specified by the mask one.
      IF(ASSOCIATED(MF(I)%P1)) MF(I)%P1( [1,MASK(1)]    ) = MF(I)%P1( [MASK(1),1]    )
      IF(ASSOCIATED(MF(I)%P2)) MF(I)%P2( [1,MASK(1)], : ) = MF(I)%P2( [MASK(1),1], : )
    ELSE ! Multi-point unmasking with cleaning
      IF(ASSOCIATED(MF(I)%P1)) THEN
        MF(I)%P1( MASK    ) = MF(I)%P1( :SIZE(MASK)    ) ! Place packed array elements on their initial positions
        WHERE([( .NOT.ANY( MASK == J ), J = 1, SIZE( MF(I)%P1 ) )]) MF(I)%P1 = MF(I)%XDEFAULT
      END IF
      IF(ASSOCIATED(MF(I)%P2)) THEN
        MF(I)%P2( MASK, : ) = MF(I)%P2( :SIZE(MASK), : )
        ! Invert mask and clear unmasked values
        N_CLEANUP_SIZE = SIZE( MF(I)%P2, 1 )
        IF(.NOT. ALLOCATED(G_CLEANUP_MASK)) THEN
            ALLOCATE(G_CLEANUP_MASK(N_CLEANUP_SIZE))
        ELSE
            ! Hypothetical case of non-uniform shapes of the
            ! model field vector members.
            IF(N_CLEANUP_SIZE /= SIZE(G_CLEANUP_MASK)) THEN
                DEALLOCATE(G_CLEANUP_MASK)
                ALLOCATE(G_CLEANUP_MASK(N_CLEANUP_SIZE))
            END IF
        END IF
        ! Fill the cleanup mask. This mask is filled by .TRUE. values
        ! except elements at positions set by the mask array. Those
        ! elements are set to .FALSE.
        G_CLEANUP_MASK(:) = [( .NOT.ANY( MASK == J ), J = 1, SIZE( MF(I)%P2, 1 ) )]
        DO K = 1, SIZE( MF(I)%P2, 2 )
            WHERE(G_CLEANUP_MASK) MF(I)%P2(:,K) = MF(I)%XDEFAULT
        END DO
      END IF
    END IF
  END DO

  IF(ALLOCATED(G_CLEANUP_MASK)) DEALLOCATE(G_CLEANUP_MASK)
  IF( LHOOK ) CALL DR_HOOK( 'MODE_SEAICE_SICE:UNPACK', 1, ZHOOK_HANDLE )
END SUBROUTINE UNPACK

SUBROUTINE PRUNE(MF, MASK)
IMPLICIT NONE
  TYPE(MODEL_FIELD), INTENT(INOUT) :: MF(:) !< Model fields
  LOGICAL, OPTIONAL, INTENT(IN) :: MASK(:)

  INTEGER :: I,K
  REAL( KIND = JPRB ) :: ZHOOK_HANDLE

  IF( LHOOK ) CALL DR_HOOK( 'MODE_SEAICE_SICE:PRUNE', 0, ZHOOK_HANDLE )

  IF(PRESENT(MASK)) THEN
    DO I = 1, SIZE(MF)
      IF(ASSOCIATED(MF(I)%P1)) WHERE(MASK) MF(I)%P1 = MF(I)%XDEFAULT
      IF(ASSOCIATED(MF(I)%P2)) THEN
        DO K = 1, SIZE( MF(I)%P2, 2 )
          WHERE(MASK) MF(I)%P2(:,K) = MF(I)%XDEFAULT
        END DO
      END IF
    END DO
  ELSE
    DO I = 1, SIZE(MF)
      IF(ASSOCIATED(MF(I)%P1)) MF(I)%P1(:  ) = MF(I)%XDEFAULT
      IF(ASSOCIATED(MF(I)%P2)) MF(I)%P2(:,:) = MF(I)%XDEFAULT
    END DO
  END IF

  IF( LHOOK ) CALL DR_HOOK( 'MODE_SEAICE_SICE:PRUNE', 1, ZHOOK_HANDLE )
END SUBROUTINE PRUNE

! Perform input/output operation over model fields
SUBROUTINE IO( MF, HSELECT, HPROGRAM, IS_DIAG, IS_READ )
USE MODI_WRITE_SURF
USE MODI_READ_SURF
IMPLICIT NONE
  TYPE( MODEL_FIELD ),  INTENT( IN OUT ) :: MF(:)
  CHARACTER(LEN=*), INTENT(IN) :: HSELECT(:)
  CHARACTER( LEN = 6 ), INTENT( IN ) :: HPROGRAM
  LOGICAL, OPTIONAL,    INTENT( IN ) :: IS_DIAG, IS_READ

  INTEGER              :: JLAYER
  INTEGER              :: IRESP    ! IRESP  : return-code if a problem appears
  CHARACTER(LEN = 16)  :: YRECFM   ! Name of the article to be read
  CHARACTER(LEN = 4 )  :: YLVL
  CHARACTER(LEN = 100) :: YCOMMENT ! Comment string
  CHARACTER(LEN = 25)  :: YFORM    ! Writing format

  INTEGER :: I, NUM_FIELDS, NUM_LAYERS
  LOGICAL :: GDIAG, GREAD
  REAL( KIND = JPRB ) :: ZHOOK_HANDLE

  IF( LHOOK ) CALL DR_HOOK( 'MODE_SEAICE_SICE:IO', 0, ZHOOK_HANDLE )

  GDIAG = .FALSE. ! Default case -- process prognostic variables
  GREAD = .FALSE. ! Default action -- write
  IF( PRESENT( IS_DIAG ) ) GDIAG = IS_DIAG
  IF( PRESENT( IS_READ ) ) GREAD = IS_READ

  NUM_FIELDS = SIZE(MF)

  DO I = 1, NUM_FIELDS
    ! Select corresponding to the value of gdiag set of model fields. For
    ! diagnostic IO will be selected only diagnostic fields, for prognostic
    ! IO -- diagnostic fields will be skipped. Also skip internal fileds,
    ! they should be not presented in the output.
    IF( (GDIAG .NEQV. MF(I)%LDIAG) .OR. MF(I)%LINTERNAL ) CYCLE
    IF( GREAD .AND. MF(I)%LDIAG ) CYCLE ! if action is set to read, skip diagnostic fields

    IF( MF(I)%NCONFIG( MF_LAYER ) > 0 ) THEN ! multilayer field
      YFORM  = '(A,I1.1," (",A,")")' ! comment for written field, in form $FIELD_$LAYER ($UNITS)
      NUM_LAYERS = MF(I)%NCONFIG( MF_LAYER )
      DO JLAYER = 1, NUM_LAYERS
        WRITE( YLVL, '("_",I2.2)' ) JLAYER
        YRECFM = TRIM(MF(I)%CNAME) // ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
        IF (JLAYER >= 10) YFORM = '(A,I2.2," (",A,")")'
        WRITE(YCOMMENT,YFORM) 'X_Y_' // TRIM(MF(I)%CNAME), JLAYER, TRIM(MF(I)%CUNITS)
        IF( GREAD ) THEN
          CALL READ_SURF ( HPROGRAM, YRECFM, MF(I)%P2(:,JLAYER), IRESP )
        ELSE
          CALL WRITE_SURF( HSELECT, HPROGRAM, YRECFM, MF(I)%P2(:,JLAYER), IRESP, HCOMMENT = YCOMMENT )
        END IF
        YCOMMENT = ''
      END DO
    ELSE ! single layer field
      YRECFM = TRIM(MF(I)%CNAME)
      YFORM  = '(A," (",A,")")' ! for single layer field comment will be written as $FIELD ($UNITS)
      WRITE(YCOMMENT,YFORM) 'X_Y_' // TRIM(MF(I)%CNAME), TRIM(MF(I)%CUNITS)
      IF( GREAD ) THEN
        CALL READ_SURF ( HPROGRAM, YRECFM, MF(I)%P1(:), IRESP )
      ELSE
        CALL WRITE_SURF( HSELECT, HPROGRAM, YRECFM, MF(I)%P1(:), IRESP, HCOMMENT = YCOMMENT )
      END IF
    END IF
  END DO

  IF( LHOOK ) CALL DR_HOOK( 'MODE_SEAICE_SICE:IO', 1, ZHOOK_HANDLE )
END SUBROUTINE IO

FUNCTION READ_SICE_CONFIG(HPROGRAM) RESULT(CONFIG)
USE MODE_POS_SURF
USE MODI_CLOSE_NAMELIST
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
IMPLICIT NONE
  CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  !< program calling surf. schemes
  TYPE(SICE_CONFIG_t), TARGET :: CONFIG

  LOGICAL :: GFOUND
  INTEGER :: ILUNAM
  INTEGER :: ILUOUT

  LOGICAL, POINTER :: LICE_HAS_SNOW
  LOGICAL, POINTER :: LICE_MASS_BALANCE
  REAL, POINTER :: XOCEAN_HEAT_FLUX
  INTEGER, POINTER :: NICE_ALBEDO
  REAL, POINTER :: XNEW_ICE_THK
  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  NAMELIST /NAM_SEAICE_SICE/ &
    LICE_HAS_SNOW,           &
    LICE_MASS_BALANCE,       &
    XOCEAN_HEAT_FLUX,        &
    NICE_ALBEDO,             &
    XNEW_ICE_THK

  IF (LHOOK) CALL DR_HOOK('MODE_SEAICE_SICE:READ_SICE_CONFIG', 0, ZHOOK_HANDLE)

  LICE_HAS_SNOW => CONFIG%LICE_HAS_SNOW
  LICE_MASS_BALANCE => CONFIG%LICE_MASS_BALANCE
  XOCEAN_HEAT_FLUX => CONFIG%XOCEAN_HEAT_FLUX
  NICE_ALBEDO => CONFIG%NICE_ALBEDO
  XNEW_ICE_THK => CONFIG%XNEW_ICE_THK

  CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
  CALL GET_LUOUT(HPROGRAM,ILUOUT)
  CALL POSNAM(ILUNAM,'NAM_SEAICE_SICE',GFOUND,ILUOUT)
  IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_SEAICE_SICE)
  CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)

  IF (LHOOK) CALL DR_HOOK('MODE_SEAICE_SICE:READ_SICE_CONFIG', 1, ZHOOK_HANDLE)
END FUNCTION READ_SICE_CONFIG

ELEMENTAL FUNCTION ICE_ALBEDO(PTSURF, SCHEME) RESULT( ALBEDO )
USE MODD_CSTS, ONLY: XTTSI, XTT
IMPLICIT NONE
  REAL,    INTENT( IN ) :: PTSURF
  INTEGER, INTENT( IN ) :: SCHEME
  REAL                  :: ALBEDO

  REAL                  :: ZTSURF_CELS

  ZTSURF_CELS = PTSURF - XTT

  SELECT CASE( SCHEME )
    CASE( SICE_ALBEDO_SIMPLE   )
      ALBEDO = 0.5
    CASE( SICE_ALBEDO_REMO     )
      IF( ZTSURF_CELS <= -3. ) THEN
          ALBEDO = 0.85
      ELSE
          ALBEDO = 0.55 - 0.1*ZTSURF_CELS
      END IF
    CASE( SICE_ALBEDO_HIRHAM   )
      ALBEDO = 0.7 - EXP( -0.5*( XTT - PTSURF ) )*(0.7 - 0.3)
    CASE( SICE_ALBEDO_HADCM3   )
      IF( ZTSURF_CELS <= -10. ) THEN
        ALBEDO = 0.8
      ELSE IF( ZTSURF_CELS > 0. ) THEN
        ALBEDO = 0.5
      ELSE
        ALBEDO = 0.8 - 0.03*( ZTSURF_CELS + 10. )
      END IF
    CASE( SICE_ALBEDO_PEROVICH )
      IF( PTSURF < XTTSI ) THEN
        ALBEDO = 0.71
      ELSE
        ALBEDO = 0.61
      END IF
  END SELECT
END FUNCTION ICE_ALBEDO
END MODULE MODE_SEAICE_SICE
