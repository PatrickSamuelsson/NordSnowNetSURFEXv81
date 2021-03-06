SUBROUTINE UNCOMPACT(PVALCO_COMPACT,KNELEM,KBITEXPONENT,KBITRADICAL,KPACK)

USE PARKIND1 , ONLY : JPRB, JPRM
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK

IMPLICIT NONE

INTEGER(KIND=4) :: KNELEM
REAL(KIND=JPRB) :: PVALCO_COMPACT(KNELEM)
REAL(KIND=JPRB) :: ZCOEF,VALMIN,VALMAX
INTEGER(KIND=4) :: KBITEXPONENT,KBITRADICAL
INTEGER(KIND=8) :: KPACK(KNELEM)

REAL(KIND=JPRM) :: ZTMPBUF
INTEGER(KIND=4) :: ITMPBUF
INTEGER(KIND=8) :: ICMPBUF
INTEGER(KIND=4),ALLOCATABLE :: IBUF(:)
INTEGER(KIND=4) :: IPROMA,IPTRTAB,IPTRBIT,IENDBIT

REAL(KIND=JPRB) :: ZHOOK_HANDLE

EQUIVALENCE(ZTMPBUF,ITMPBUF)

IF (LHOOK) CALL DR_HOOK('UNCOMPACT',0,ZHOOK_HANDLE)

!SOLUTION 1
!IPTRBIT=0
!IPTRTAB=1
!DO IPROMA=1,KNELEM
!  ITMPBUF=KPACK(IPROMA)
!   PVALCO_COMPACT(IPROMA)=ZTMPBUF
!END DO

!SOLUTION 2
ITMPBUF=IBITS(KPACK(1),0,32)
VALMIN=ZTMPBUF
ITMPBUF=IBITS(KPACK(1),32,32)
VALMAX=ZTMPBUF
!PRINT*,VALMIN," ",VALMAX
IF (VALMAX-VALMIN<=0.0) THEN
   PVALCO_COMPACT(1:KNELEM)=VALMIN
ELSE
   IPTRTAB=2
   IPTRBIT=0
   ZCOEF=(2**KBITRADICAL-1)
   DO IPROMA=2,KNELEM+1
      IF (IPTRBIT+KBITRADICAL<64) THEN
         ICMPBUF=IBITS(KPACK(IPTRTAB),IPTRBIT,KBITRADICAL)
         IPTRBIT=IPTRBIT+KBITRADICAL
      ELSE
         IENDBIT=64-IPTRBIT
         ICMPBUF=IBITS(KPACK(IPTRTAB),IPTRBIT,IENDBIT)+ISHFT(IBITS(KPACK(IPTRTAB+1),0,KBITRADICAL-IENDBIT),IENDBIT)
         IPTRTAB=IPTRTAB+1
         IPTRBIT=KBITRADICAL-IENDBIT
         
      END IF
      
      PVALCO_COMPACT(IPROMA-1)=ICMPBUF*(VALMAX-VALMIN)/ZCOEF+VALMIN
      !PRINT*,PVALCO_COMPACT(IPROMA-1)
   END DO
END IF


!SOLUTION 3 CA MARCHE PAS BIEN
!ALLOCATE(IBUF(KNELEM))
!DO IPROMA=1,KNELEM
!          IF (IPTRBIT+KBITRADICAL+2+KBITEXPONENT<64) THEN
!             IBUF(IPROMA)=IBITS(KPACK(IPTRTAB),IPTRBIT,KBITRADICAL+2+KBITEXPONENT)
!             IPTRBIT=IPTRBIT+KBITRADICAL+2+KBITEXPONENT
             
!          ELSE
!             IENDBIT=64-IPTRBIT
!             IBUF(IPROMA)=IBITS(KPACK(IPTRTAB),IPTRBIT,IENDBIT)
!             IPTRTAB=IPTRTAB+1
!             IBUF(IPROMA)=IBUF(IPROMA)+ISHFT(IBITS(KPACK(IPTRTAB),0,KBITRADICAL+2+KBITEXPONENT-IENDBIT),IENDBIT)
!             IPTRBIT=KBITRADICAL+2+KBITEXPONENT-IENDBIT
!          END IF
!       END DO
!WRITE(6,*) 'DEBUG UNCOMPACT',IBUF(1)
!DO IPROMA=1,KNELEM
!ITMPBUF=0
!ITMPBUF=ISHFT(IBITS(IBUF(IPROMA),0,KBITRADICAL),23-KBITRADICAL)+ISHFT(IBITS(IBUF(IPROMA),KBITRADICAL,KBITEXPONENT),23)&
!+ISHFT(2**(7-KBITEXPONENT)-1+IBITS(IBUF(IPROMA),KBITRADICAL+KBITEXPONENT,1),23+KBITEXPONENT)&
!&+ISHFT(IBITS(IBUF(IPROMA),KBITRADICAL+KBITEXPONENT+1,1),31)
!PVALCO_COMPACT(IPROMA)=ZTMPBUF
!END DO

IF (LHOOK) CALL DR_HOOK('UNCOMPACT',1,ZHOOK_HANDLE)

END SUBROUTINE UNCOMPACT
