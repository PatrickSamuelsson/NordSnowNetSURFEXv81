SUBROUTINE C_MPL_BARR
IMPLICIT NONE
#ifdef SFX_MPI
include "mpif.h"
#endif
INTEGER :: IERR

CALL MPI_BARRIER (MPI_COMM_WORLD, IERR)

END SUBROUTINE C_MPL_BARR

