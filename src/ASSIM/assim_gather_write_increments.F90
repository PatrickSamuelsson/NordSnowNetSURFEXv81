!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE ASSIM_GATHER_WRITE_INCREMENTS(HPROGRAM,HVAR,PFIELD_OLD,PFIELD_NEW,PUNDEF,LSTAT)
  USE PARKIND1,        ONLY : JPIM, JPRB
  USE YOMHOOK ,        ONLY : LHOOK, DR_HOOK
  USE MODD_SURF_PAR,   ONLY : XUNDEF
  USE MODD_SURFEX_MPI, ONLY : NINDEX
  USE MODD_ASSIM,      ONLY : LPIO
  USE MODI_GET_LUOUT
  USE MODI_ASSIM_GATHER_FIELD
  IMPLICIT NONE
  CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM
  CHARACTER(LEN=*),  INTENT(IN) :: HVAR
  REAL,DIMENSION(:), INTENT(IN) :: PFIELD_OLD
  REAL,DIMENSION(:), INTENT(IN) :: PFIELD_NEW
  REAL,              INTENT(IN) :: PUNDEF
  LOGICAL,OPTIONAL,  INTENT(IN) :: LSTAT
  INTEGER                       :: IOBS,ISIZE,JI,ILUOUT
  REAL,DIMENSION(:),ALLOCATABLE :: ZWORK,ZWORK_NEW,ZWORK_OLD
  
  LOGICAL,ALLOCATABLE,DIMENSION(:) :: LMASK
  LOGICAL                          :: OSTAT

  REAL(KIND=JPRB) :: ZHOOK_HANDLE
  IF (LHOOK) CALL DR_HOOK('ASSIM_GATHER_WRITE_INCREMENTS',0,ZHOOK_HANDLE)

  OSTAT=.FALSE.
  IF (PRESENT(LSTAT)) OSTAT=LSTAT

  CALL GET_LUOUT(HPROGRAM,ILUOUT)
  ! Get global size
  IF(HPROGRAM == 'AROME ') THEN
#ifdef SFX_ARO
    CALL ARO_GET_SIZE_GLOBAL(SIZE(PFIELD_OLD),ISIZE)
#else
    CALL ABOR1_SFX('SFX_ARO should be defined while calling SURFEX from AROME')
#endif
  ELSE
    ISIZE=SIZE(NINDEX)
  ENDIF

  IF ( LPIO ) THEN
    ALLOCATE(ZWORK_OLD(ISIZE))
    ALLOCATE(ZWORK_NEW(ISIZE))
  ELSE
    ALLOCATE(ZWORK_OLD(0))
    ALLOCATE(ZWORK_NEW(0))
  ENDIF

  CALL ASSIM_GATHER_FIELD(HPROGRAM,PFIELD_OLD,ZWORK_OLD)
  CALL ASSIM_GATHER_FIELD(HPROGRAM,PFIELD_NEW,ZWORK_NEW)

  IF ( LPIO ) THEN

    ALLOCATE(ZWORK(ISIZE))
    ALLOCATE(LMASK(ISIZE))

    LMASK(:)=.FALSE. 
    LMASK(:)=(ZWORK_OLD(:) /= PUNDEF .AND. ZWORK_OLD(:) /= XUNDEF .AND. ZWORK_NEW(:) /= PUNDEF .AND. ZWORK_NEW(:) /= XUNDEF)
    IOBS=COUNT(LMASK)
    WHERE ( LMASK(:) )
      ZWORK(:) = ZWORK_NEW(:) - ZWORK_OLD(:)
    ELSEWHERE
      ZWORK(:) = 0.
    ENDWHERE
    IF (IOBS > 0 ) THEN
      write(ILUOUT,*) 'Mean increment for '//TRIM(HVAR)//' is: ',&
                & SUM(ZWORK(:))/REAL(IOBS),' Defined values: ',IOBS
    ELSE
      write(ILUOUT,*) 'No defined values found for variable '//TRIM(HVAR)
    ENDIF
    IF ( OSTAT ) THEN
      IF ( COUNT(LMASK) > 0 ) THEN
        WRITE(ILUOUT,*) 'New field: min,mean,max',MINVAL(ZWORK_NEW,MASK=LMASK),&
          SUM(ZWORK_NEW,MASK=LMASK)/COUNT(LMASK),MAXVAL(ZWORK_NEW,MASK=LMASK)
        WRITE(ILUOUT,*) 'Old field: min,mean,max',MINVAL(ZWORK_OLD,MASK=LMASK),&
          SUM(ZWORK_OLD,MASK=LMASK)/COUNT(LMASK),MAXVAL(ZWORK_OLD,MASK=LMASK)
      ELSE
         WRITE(ILUOUT,*) 'All values masked out'
      ENDIF
    ENDIF
    DEALLOCATE(LMASK)
    DEALLOCATE(ZWORK)
  ENDIF
  DEALLOCATE(ZWORK_NEW)
  DEALLOCATE(ZWORK_OLD)

  IF (LHOOK) CALL DR_HOOK('ASSIM_GATHER_WRITE_INCREMENTS',1,ZHOOK_HANDLE)
END SUBROUTINE ASSIM_GATHER_WRITE_INCREMENTS
