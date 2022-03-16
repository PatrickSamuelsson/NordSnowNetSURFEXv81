SUBROUTINE EGGDIR (PRPI, PRA, PDELX, PDELY, KPROF,&
 & KBEG, KEND, KULOUT, PGX, PGY)  
!-------------------------------------------------------------------
USE PARKIND1  ,ONLY : JPIM     ,JPRB
!-------------------------------------------------------------------
IMPLICIT NONE
INTEGER(KIND=JPIM),INTENT(IN)    :: KPROF 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRPI 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRA 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDELX 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDELY 
INTEGER(KIND=JPIM),INTENT(IN)    :: KBEG 
INTEGER(KIND=JPIM),INTENT(IN)    :: KEND 
INTEGER(KIND=JPIM),INTENT(IN)    :: KULOUT 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGX(KPROF) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGY(KPROF) 
!-------------------------------------------------------------------
END SUBROUTINE EGGDIR
