MODULE YOMHOOKSTACK

! Used by dr_hook_util to monitor thread stack usage 
! Need "export STACKCHECK=yes"

USE PARKIND1  ,ONLY : JPIM     ,JPRB,      JPIB

IMPLICIT NONE

SAVE

INTEGER(KIND=JPIB), ALLOCATABLE :: ISAVE(:) 
INTEGER(KIND=JPIB), ALLOCATABLE :: IMAXSTACK(:) 
LOGICAL,   ALLOCATABLE :: LL_THREAD_FIRST(:)
CHARACTER(LEN=3)       :: CSTACK

END MODULE YOMHOOKSTACK

