!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE ASSIM_GATHER_FIELD(HPROGRAM,PFIELD_LOCAL,PFIELD_GLOBAL)
  USE PARKIND1,        ONLY : JPIM, JPRB
  USE YOMHOOK,         ONLY : LHOOK, DR_HOOK
  USE MODD_ASSIM,      ONLY : LPIO
  USE MODD_SURFEX_MPI, ONLY : NPROC,NINDEX
  USE MODD_SURF_PAR,   ONLY : XUNDEF
  USE MODI_GATHER_AND_WRITE_MPI

  IMPLICIT NONE
  CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM
  REAL,DIMENSION(:), INTENT(IN)  :: PFIELD_LOCAL
  REAL,DIMENSION(:), INTENT(OUT) :: PFIELD_GLOBAL
  INTEGER                        :: ISIZE,JI
  REAL,DIMENSION(:),ALLOCATABLE  :: ZWORK

  REAL(KIND=JPRB)                :: ZHOOK_HANDLE
  IF (LHOOK) CALL DR_HOOK('ASSIM_GATHER_FIELD',0,ZHOOK_HANDLE)

  IF(HPROGRAM == 'AROME ') THEN
#ifdef SFX_ARO
    CALL ARO_GET_SIZE_GLOBAL(SIZE(PFIELD_LOCAL),ISIZE)
    IF (LPIO) THEN
      ALLOCATE(ZWORK(ISIZE))
      ZWORK(:)=XUNDEF
    ELSE
      ! This breaks bounds checking
      ALLOCATE(ZWORK(0))
    ENDIF
    CALL ARO_GATHER_FIELD(SIZE(PFIELD_LOCAL),ISIZE,PFIELD_LOCAL,ZWORK(:))
#else
    CALL ABOR1_SFX('SFX_ARO should be defined while calling SURFEX from AROME')
#endif
  ELSE
    ISIZE=SIZE(NINDEX)
    IF (LPIO) THEN
      IF ( SIZE(PFIELD_GLOBAL) /= ISIZE ) THEN
        WRITE(*,*) "SIZE(PFIELD_GLOBAL):",SIZE(PFIELD_GLOBAL),"ISIZE:",ISIZE
        CALL ABOR1_SFX('Mismatch in dimensions between global fields')
      ENDIF

      ALLOCATE(ZWORK(ISIZE))
      ZWORK(:)=XUNDEF
    ELSE
      ALLOCATE(ZWORK(0))
    ENDIF

    ! Gather field
    IF ( NPROC > 1 ) THEN
       CALL GATHER_AND_WRITE_MPI(PFIELD_LOCAL(:),ZWORK(:))
    ELSE
       ZWORK(1:SIZE(PFIELD_LOCAL))=PFIELD_LOCAL(:)
    ENDIF
  ENDIF

  IF (LPIO) THEN
    PFIELD_GLOBAL(:)=ZWORK(:)
  ENDIF
  DEALLOCATE(ZWORK)
  IF (LHOOK) CALL DR_HOOK('ASSIM_GATHER_FIELD',1,ZHOOK_HANDLE)
END SUBROUTINE ASSIM_GATHER_FIELD
