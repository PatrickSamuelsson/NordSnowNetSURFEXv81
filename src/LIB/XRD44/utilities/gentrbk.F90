!-- Generic traceback calls here

SUBROUTINE GENTRBK_DUMMY
END SUBROUTINE GENTRBK_DUMMY

#ifdef __INTEL_COMPILER
SUBROUTINE INTEL_TRBK()
USE IFCORE
#ifdef SFX_MPI
USE MPL_MODULE, ONLY : MPL_MYRANK
#endif
CHARACTER*80 MESSAGE
LOGICAL :: DONE_TRACEBACK = .FALSE.
INTEGER :: MYPROC,MYTHREAD

#ifdef _OPENMP
INTEGER,EXTERNAL :: OMP_GET_THREAD_NUM
#endif

IF(DONE_TRACEBACK) THEN
  WRITE(0,*) "INTEL_TRBK already called"
  RETURN
ENDIF
#ifdef SFX_MPI
MYPROC=MPL_MYRANK()
#else
MYPROC=0
#endif
#ifdef _OPENMP
MYTHREAD=OMP_GET_THREAD_NUM() + 1
#else
MYTHREAD=1
#endif

#ifndef BOM
  WRITE(MESSAGE,'(A,I4,A,I2,A)') &
  &           "Process ",MYPROC," thread ",MYTHREAD, &
  &           " calling tracebackqq from intel_trbk()"
#ifndef __INTEL_COMPILER
  CALL TRACEBACKQQ(MESSAGE, USER_EXIT_CODE=-1)
#endif
#endif
#ifdef LINUX
  WRITE(0,*) "Process ",MYPROC," thread ",MYTHREAD, &
 &           " calling linux_trbk from intel_trbk()"
  CALL LINUX_TRBK() ! See ifsaux/utilities/linuxtrbk.c
#endif
DONE_TRACEBACK=.TRUE.
END SUBROUTINE INTEL_TRBK
#endif

#ifndef VPP
SUBROUTINE ERRTRA
END SUBROUTINE ERRTRA
#endif

#ifdef NECSX
SUBROUTINE NECSX_TRBK(CDMESS)
IMPLICIT NONE
CHARACTER(LEN=*), INTENT(IN) :: CDMESS
CALL MESPUT(CDMESS, LEN(CDMESS), 1)
CALL DBX_TRBK()
END SUBROUTINE NECSX_TRBK

SUBROUTINE NECSX_TRBK_FL(CDMESS, CDFILENAME, KLINENO)
USE PARKIND1  ,ONLY : JPIM
IMPLICIT NONE
CHARACTER(LEN=*), INTENT(IN) :: CDMESS
CHARACTER(LEN=*), INTENT(IN) :: CDFILENAME
INTEGER(KIND=JPIM), INTENT(IN) :: KLINENO
CHARACTER(LEN=LEN(CDMESS)+LEN(CDFILENAME)+30) CLOCAL
WRITE(CLOCAL,'(a," at ",a,":",i6.6)') TRIM(CDMESS),TRIM(CDFILENAME),KLINENO
CALL NECSX_TRBK(TRIM(CLOCAL))
END SUBROUTINE NECSX_TRBK_FL
#endif
