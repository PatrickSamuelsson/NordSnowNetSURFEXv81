SUBROUTINE EC_CRAY_MEMINFO(IU,IDSTRING,KCOMM)

IMPLICIT NONE

!-- EC_CRAY_MEMINFO:
!   Author   : Peter Towers (ECMWF)  : 2015-2016
!   Modified : Sami Saarinen (ECMWF) : 21-SEP-2016 : Added getenv EC_MEMINFO -- export EC_MEMINFO=0 disables any EC_MEMINFO output
#ifdef SFX_MPI
include "mpif.h"
#endif

INTEGER(KIND=4), INTENT(IN) :: KCOMM
INTEGER(KIND=4), INTENT(IN) :: IU
CHARACTER*(*), INTENT(IN) :: IDSTRING
INTEGER(KIND = 4) :: ID,KULOUT
INTEGER(KIND=4) :: I,J,MYPROC,NPROC,LEN,ERROR,ITAG,NODENUM
INTEGER(KIND=8) :: TASKSMALL,NODEHUGE,MEMFREE,CACHED,NFREE
INTEGER(KIND=8) :: SMALLPAGE0,SMALLPAGE1,HUGEPAGE0,HUGEPAGE1
INTEGER(KIND=8) :: SENDBUF(9),RECVBUF(9)
INTEGER(KIND=8) :: GETHWM,GETMAXRSS
INTEGER(KIND=8) :: HEAP_SIZE
INTEGER(KIND=4) :: PAGESIZE,N18
INTEGER(KIND=4) :: NODE0(18),NODE1(18)
INTEGER(KIND=8) :: BUCKET0(18),BUCKET1(18)
REAL(KIND=4) :: PERCENT_USED(2)
CHARACTER(LEN=512) :: TMPDIR
CHARACTER(LEN=512) :: PROGRAM
CHARACTER(LEN=8)  :: NODENAME,LASTNODE
CHARACTER(LEN=12)  :: VAL
CHARACTER(LEN=1)  :: M
CHARACTER(LEN=160) ::LINE
CHARACTER(LEN=56) :: FILENAME
CHARACTER(LEN=128) :: JOBNAME
CHARACTER(LEN=128) :: JOBID
CHARACTER(LEN=2) :: EC_MEMINFO
CHARACTER(LEN=4) :: CSTAR
CHARACTER(LEN=LEN(CSTAR)+1+LEN(IDSTRING)) :: ID_STRING
CHARACTER (LEN = 10) ::  CLDATEOD,CLTIMEOD,CLZONEOD
INTEGER(KIND=4) :: IVALUES(8)
#ifdef SFX_MPI
INTEGER(KIND=4) :: IRECV_STATUS(MPI_STATUS_SIZE)
#endif
LOGICAL :: LLNOCOMM, LLNOHDR
CHARACTER(LEN=64) :: CLPFX
CHARACTER(LEN=3) :: ZUM
INTEGER(KIND=4) :: IPFXLEN
INTEGER OMP_GET_MAX_THREADS
EXTERNAL OMP_GET_MAX_THREADS

CALL GETENV('EC_MEMINFO',EC_MEMINFO)
IF (EC_MEMINFO == '0') RETURN

LLNOCOMM = (KCOMM == -1 .or. KCOMM == -2)
LLNOHDR = (KCOMM == -2)

CALL FLUSH(0)

IF (LLNOCOMM) THEN
   ! Direct call to obtain EC_meminfo -output
   ERROR = 0
   MYPROC = 0
   NPROC = 1
   CLPFX = IDSTRING
   IPFXLEN = LEN_TRIM(CLPFX)
   ZUM = 'tsk'
ELSE
   CLPFX = ' '
   IPFXLEN = 0
   ZUM = 'sum'
#ifdef SFX_MPI
   CALL MPI_BARRIER(KCOMM,ERROR)
   
   CALL MPI_COMM_RANK(KCOMM, MYPROC, ERROR)
   
   IF(ERROR /= 0 ) THEN
      WRITE(0,*) CLPFX(1:IPFXLEN)//"## EC_CRAY_MEMINFO error code ",ERROR," from MPI_COMM_RANK"
      CALL MPI_ABORT(KCOMM,-1,ERROR)
   ENDIF

   CALL MPI_COMM_SIZE(KCOMM,NPROC,ERROR)

   IF(ERROR /= 0 ) THEN
      WRITE(0,*) CLPFX(1:IPFXLEN)//"## EC_CRAY_MEMINFO error code ",ERROR," from MPI_COMM_SIZE"
      CALL MPI_ABORT(KCOMM,-1,ERROR)
   ENDIF
#else
   NPROC = 1
   MYPROC = 0
#endif
ENDIF

IF(MYPROC == 0) THEN 
  CALL GETARG(0,PROGRAM)
!
! Use already open file for output or $TMPDIR/meminfo
!
  IF(IU == -1) THEN
    CALL GETENV('TMPDIR',TMPDIR)
    IF (TMPDIR == ' ') TMPDIR = '.'
    KULOUT=501
    OPEN(UNIT=KULOUT,FILE=TRIM(TMPDIR)//"/"//"meminfo",STATUS='unknown', &
         ACTION='write',POSITION='append')
  ELSE
    KULOUT=IU
  ENDIF

  CALL DATE_AND_TIME(CLDATEOD,CLTIMEOD,CLZONEOD,IVALUES)
  CALL GETENV('EC_JOB_NAME',JOBNAME)
  IF(JOBNAME == '') THEN
    CALL GETENV('PBS_JOBNAME',JOBNAME)
  ENDIF
  CALL GETENV('PBS_JOBID',JOBID)

  IF (.not.LLNOCOMM) THEN
     WRITE(KULOUT,'(a,/,a)') CLPFX(1:IPFXLEN)//"## EC_MEMINFO ",CLPFX(1:IPFXLEN)//"## EC_MEMINFO"
     WRITE(KULOUT,'(4a)') CLPFX(1:IPFXLEN)//"## EC_MEMINFO Detailed memory information ", &
          "for program ",TRIM(PROGRAM)
#ifdef SFX_OMP
     WRITE(KULOUT,'(a,i5,a,i3,a,a,'':'',a,'':'',a,a,a,''-'',a,''-'',a)') &
          CLPFX(1:IPFXLEN)//"## EC_MEMINFO Running with ",NPROC, &
          " tasks and ", OMP_GET_MAX_THREADS(), " threads at time ", &
          CLTIMEOD(1:2),CLTIMEOD(3:4),CLTIMEOD(5:10), &
          " on ",CLDATEOD(7:8),CLDATEOD(5:6),CLDATEOD(3:4)
#else
     WRITE(KULOUT,'(a,i5,a,i3,a,'':'',a,a,a,''-'',a,''-'',a)') &
          CLPFX(1:IPFXLEN)//"## EC_MEMINFO Running with ",NPROC, &
          " tasks and 1 threads at time ", &
          CLTIMEOD(1:2),CLTIMEOD(3:4),CLTIMEOD(5:10), &
          " on ",CLDATEOD(7:8),CLDATEOD(5:6),CLDATEOD(3:4)
#endif
     WRITE(KULOUT,'(4a)') CLPFX(1:IPFXLEN)//"## EC_MEMINFO The Job Name is ",TRIM(JOBNAME), &
          " and the Job ID is ",TRIM(JOBID)
     WRITE(KULOUT,'(a)')  CLPFX(1:IPFXLEN)//"## EC_MEMINFO "
  ENDIF
  IF (.not.LLNOHDR) THEN
     WRITE(KULOUT,'(3a)') CLPFX(1:IPFXLEN)//"## EC_MEMINFO ", &
                       "              | TC    | MEMORY USED(MB) |", &
                       "          MEMORY FREE(MB)        INCLUDING CACHED |  %USED %HUGE"
     WRITE(KULOUT,'(4a)') CLPFX(1:IPFXLEN)//"## EC_MEMINFO ", &
                       "              | Malloc| Inc Heap        |", &
                       " Numa node 0    | Numa node 1    |                |"
     WRITE(KULOUT,'(4a)') CLPFX(1:IPFXLEN)//"## EC_MEMINFO ", &
                       "Node Name     | Heap  | RSS("//zum//")        |", &
                       " Small  Huge or | Small  Huge or | Total          |" 
     WRITE(KULOUT,'(4a)') CLPFX(1:IPFXLEN)//"## EC_MEMINFO ", &
                       "              | ("//zum//") | Small    Huge   |", &
                       "  Only   Small  |  Only   Small  | Memfree+Cached |"
  ENDIF
  IF(IU == -1) THEN
    WRITE(0,'(a,/,a)') CLPFX(1:IPFXLEN)//"## EC_MEMINFO ",CLPFX(1:IPFXLEN)//"## EC_MEMINFO"
    WRITE(0,'(4a)') CLPFX(1:IPFXLEN)//"## EC_MEMINFO Detailed memory information ", &
                    "for program ",TRIM(PROGRAM)
#ifdef SFX_OMP
    WRITE(0,'(a,i5,a,i3,a,a,'':'',a,'':'',a,a,a,''-'',a,''-'',a)') &
                    CLPFX(1:IPFXLEN)//"## EC_MEMINFO Running with ",NPROC, &
                    " tasks and ", OMP_GET_MAX_THREADS(), " threads at time ", &
                    CLTIMEOD(1:2),CLTIMEOD(3:4),CLTIMEOD(5:10), &
                    " on ",CLDATEOD(7:8),CLDATEOD(5:6),CLDATEOD(3:4)
#else
    WRITE(0,'(a,i5,a,i3,a,a,'':'',a,a,a,''-'',a,''-'',a)') &
                    CLPFX(1:IPFXLEN)//"## EC_MEMINFO Running with ",NPROC, &
                    " tasks and 1 threads at time ", &
                    CLTIMEOD(1:2),CLTIMEOD(3:4),CLTIMEOD(5:10), &
                    " on ",CLDATEOD(7:8),CLDATEOD(5:6),CLDATEOD(3:4)
#endif
    WRITE(0,'(4a)') CLPFX(1:IPFXLEN)//"## EC_MEMINFO The Job Name is ",TRIM(JOBNAME), &
                    " and the Job ID is ",TRIM(JOBID)
    WRITE(0,'(a)')  CLPFX(1:IPFXLEN)//"## EC_MEMINFO "
    WRITE(0,'(3a)') CLPFX(1:IPFXLEN)//"## EC_MEMINFO ", &
                    "              | TC    | MEMORY USED(MB) |", &
                    "          MEMORY FREE(MB)        INCLUDING CACHED |  %USED %HUGE"
    WRITE(0,'(4a)') CLPFX(1:IPFXLEN)//"## EC_MEMINFO ", &
                    "              | Malloc| Inc Heap        |", &
                    " Numa node 0    | Numa node 1    |                |"
    WRITE(0,'(4a)') CLPFX(1:IPFXLEN)//"## EC_MEMINFO ", &
                    "Node Name     | Heap  | RSS("//zum//")        |", &
                    " Small  Huge or | Small  Huge or | Total          |" 
    WRITE(0,'(4a)') CLPFX(1:IPFXLEN)//"## EC_MEMINFO ", &
                    "              | ("//zum//") | Small    Huge   |", &
                    "  Only   Small  |  Only   Small  | Memfree+Cached |" 
  ENDIF
ENDIF

IF(ERROR /= 0 ) THEN
  WRITE(0,*) CLPFX(1:IPFXLEN)//"## EC_CRAY_MEMINFO error code ",ERROR," from MPI_BARRIER"
#ifdef SFX_MPI
  CALL MPI_ABORT(KCOMM,-1,ERROR)
#endif
ENDIF

#ifndef DARWIN
CALL EC_GETHOSTNAME(NODENAME) ! from support/env.c
#endif

PAGESIZE=2048
CALL GETENV("HUGETLB_DEFAULT_PAGE_SIZE",VAL)
I=INDEX(VAL,"M")
IF(I > 0) THEN
   READ(VAL(1:I-1),*) PAGESIZE
   PAGESIZE=PAGESIZE*1024
ENDIF

NODEHUGE=0

WRITE(FILENAME,'(a,i0,a)') "/sys/kernel/mm/hugepages/hugepages-", &
     PAGESIZE,"kB/nr_hugepages"

IF(PAGESIZE > 0) THEN
  OPEN(502,FILE=FILENAME,STATUS="old")
  READ(502,*) NODEHUGE
  CLOSE(502)
ENDIF

NODEHUGE=NODEHUGE*PAGESIZE
MEMFREE = 0
CACHED = 0
      
OPEN(FILE="/proc/meminfo",UNIT=502)
DO I=1,10
  READ(502,'(a)') LINE
  IF(LINE(1:7) == "MemFree") THEN
    READ(LINE(9:80),*) MEMFREE 
  ELSEIF(LINE(1:6) == "Cached") THEN
    READ(LINE(8:80),*) CACHED
  ENDIF
ENDDO
CLOSE(502)

NODEHUGE=NODEHUGE/1024
MEMFREE=MEMFREE/1024
CACHED=CACHED/1024

TASKSMALL=GETMAXRSS()/(1024*1024)

OPEN(FILE="/proc/buddyinfo",UNIT=502)

READ(502,'(a)') LINE
READ(502,'(a)') LINE
READ(502,'(a)') LINE
NODE0(:)=-1
N18=0
READ(LINE(22:),*,END=98) NODE0
98 CONTINUE
N18 = COUNT(NODE0 >= 0)
NODE1(:)=0
READ(502,'(a)',END=99) LINE
READ(LINE(22:),*) NODE1(1:N18)

99 CONTINUE
CLOSE(502)

BUCKET0(:) = 0
BUCKET1(:) = 0
BUCKET0(1)=NODE0(1)*4096
BUCKET1(1)=NODE1(1)*4096
DO I=2,N18
  BUCKET0(I)=NODE0(I)*4096
  BUCKET1(I)=NODE1(I)*4096
  DO J=2,I
    BUCKET0(I)=BUCKET0(I)*2
    BUCKET1(I)=BUCKET1(I)*2
  ENDDO
ENDDO

SMALLPAGE0=0
SMALLPAGE1=0
DO I=1,9
   SMALLPAGE0=SMALLPAGE0+BUCKET0(I)
   SMALLPAGE1=SMALLPAGE1+BUCKET1(I)
ENDDO
HUGEPAGE0=0
HUGEPAGE1=0
DO I=10,N18
   HUGEPAGE0=HUGEPAGE0+BUCKET0(I)
   HUGEPAGE1=HUGEPAGE1+BUCKET1(I)
ENDDO

SMALLPAGE0=SMALLPAGE0/(1024*1024)
SMALLPAGE1=SMALLPAGE1/(1024*1024)
HUGEPAGE0=HUGEPAGE0/(1024*1024)
HUGEPAGE1=HUGEPAGE1/(1024*1024)

HEAP_SIZE=GETHWM()/(1024*1024)

ITAG = 98765
IF(MYPROC == 0) THEN
    NODENUM=1
    LASTNODE=NODENAME
#ifdef SFX_MPI
    DO I=1,NPROC-1
        CALL MPI_RECV(NODENAME(1:8),8,MPI_BYTE,I,ITAG,KCOMM,IRECV_STATUS,ERROR)
        IF(ERROR /= 0 ) THEN
          WRITE(0,*) CLPFX(1:IPFXLEN)//"## EC_CRAY_MEMINFO error code ",ERROR," from MPI_RECV"
          CALL MPI_ABORT(KCOMM,-1,ERROR)
        ENDIF
        CALL MPI_RECV(RECVBUF(1:9),9,MPI_INTEGER8,I,ITAG+1,KCOMM,IRECV_STATUS,ERROR)
        IF(ERROR /= 0 ) THEN
          WRITE(0,*) CLPFX(1:IPFXLEN)//"## EC_CRAY_MEMINFO error code ",ERROR," from MPI_RECV"
          CALL MPI_ABORT(KCOMM,-1,ERROR)
        ENDIF
        IF(LASTNODE==NODENAME)THEN
          HEAP_SIZE=HEAP_SIZE+RECVBUF(8)
          TASKSMALL=TASKSMALL+RECVBUF(9)
        ELSE
          PERCENT_USED(2) = 0
          IF(HEAP_SIZE > NODEHUGE) THEN
! running with small pages
            PERCENT_USED(1)=100.0*(TASKSMALL+NODEHUGE)/(TASKSMALL+NODEHUGE+MEMFREE+CACHED)
            CSTAR = " s/p"
          ELSE
! running with huge pages
            PERCENT_USED(1)=100.0*(HEAP_SIZE+TASKSMALL)/(TASKSMALL+NODEHUGE+MEMFREE+CACHED)
            IF (NODEHUGE > 0) THEN
               NFREE = HUGEPAGE0 + HUGEPAGE1
               PERCENT_USED(2) = (100.0*(NODEHUGE - NFREE))/NODEHUGE
            ENDIF
            CSTAR = " H/p"
          ENDIF
          IF (.not.LLNOCOMM) THEN
             ID_STRING = CSTAR//":"//IDSTRING
          ELSE
             ID_STRING = CSTAR
          ENDIF
          WRITE(KULOUT,'(a,i4,1x,a,3i8,2x,2i8,1x,2i8,2x,2i8,3x,2f6.1,a)') CLPFX(1:IPFXLEN)//"## EC_MEMINFO ", &
 &                      NODENUM,LASTNODE,HEAP_SIZE,TASKSMALL,NODEHUGE,   &
 &                      SMALLPAGE0,HUGEPAGE0,SMALLPAGE1,HUGEPAGE1,MEMFREE,CACHED, &
 &                      PERCENT_USED,trim(ID_STRING)
          IF(IU == -1) THEN
            WRITE(0,'(a,i4,1x,a,3i8,2x,2i8,1x,2i8,2x,2i8,3x,2f6.1,a)') CLPFX(1:IPFXLEN)//"## EC_MEMINFO ", &
 &                   NODENUM,LASTNODE,HEAP_SIZE,TASKSMALL,NODEHUGE,   &
 &                   SMALLPAGE0,HUGEPAGE0,SMALLPAGE1,HUGEPAGE1,MEMFREE,CACHED, &
 &                   PERCENT_USED,trim(ID_STRING)
          ENDIF

          NODEHUGE=RECVBUF(1)
          MEMFREE=RECVBUF(2)
          CACHED=RECVBUF(3)
          SMALLPAGE0=RECVBUF(4)
          SMALLPAGE1=RECVBUF(5)
          HUGEPAGE0=RECVBUF(6)
          HUGEPAGE1=RECVBUF(7)
          HEAP_SIZE=RECVBUF(8)
          TASKSMALL=RECVBUF(9)
          NODENUM=NODENUM+1
          LASTNODE=NODENAME
        ENDIF
    ENDDO
#endif
    PERCENT_USED(2) = 0
    IF(HEAP_SIZE > NODEHUGE) THEN
! running with small pages
      PERCENT_USED(1)=100.0*(TASKSMALL+NODEHUGE)/(TASKSMALL+NODEHUGE+MEMFREE+CACHED)
      CSTAR = " s/p"
    ELSE
! running with huge pages
      PERCENT_USED(1)=100.0*(HEAP_SIZE+TASKSMALL)/(TASKSMALL+NODEHUGE+MEMFREE+CACHED)
      IF (NODEHUGE > 0) THEN
         NFREE = HUGEPAGE0 + HUGEPAGE1
         PERCENT_USED(2) = (100.0*(NODEHUGE - NFREE))/NODEHUGE
      ENDIF
      CSTAR = " H/p"
    ENDIF
    IF (.not.LLNOCOMM) THEN
       ID_STRING = CSTAR//":"//IDSTRING
    ELSE
       ID_STRING = CSTAR
    ENDIF
    WRITE(KULOUT,'(a,i4,1x,a,3i8,2x,2i8,1x,2i8,2x,2i8,3x,2f6.1,a)') CLPFX(1:IPFXLEN)//"## EC_MEMINFO ", &
 &                NODENUM,LASTNODE,HEAP_SIZE,TASKSMALL,NODEHUGE,   &
 &                SMALLPAGE0,HUGEPAGE0,SMALLPAGE1,HUGEPAGE1,MEMFREE,CACHED, &
 &                PERCENT_USED,trim(ID_STRING)

    IF(IU == -1) THEN
      WRITE(0,'(a,i4,1x,a,3i8,2x,2i8,1x,2i8,2x,2i8,3x,2f6.1,a)') CLPFX(1:IPFXLEN)//"## EC_MEMINFO ", &
 &                  NODENUM,LASTNODE,HEAP_SIZE,TASKSMALL,NODEHUGE,   &
 &                  SMALLPAGE0,HUGEPAGE0,SMALLPAGE1,HUGEPAGE1,MEMFREE,CACHED, &
 &                  PERCENT_USED,trim(ID_STRING)
      CLOSE(KULOUT)
    ENDIF
ELSE
#ifdef SFX_MPI
    CALL MPI_SEND(NODENAME(1:8),8,MPI_BYTE,0,ITAG,KCOMM,ERROR)
    IF(ERROR /= 0 ) THEN
       WRITE(0,*) CLPFX(1:IPFXLEN)//"## EC_CRAY_MEMINFO error code ",ERROR," from MPI_SEND"
       CALL MPI_ABORT(KCOMM,-1,ERROR)
    ENDIF
#endif
    SENDBUF(1)=NODEHUGE
    SENDBUF(2)=MEMFREE
    SENDBUF(3)=CACHED
    SENDBUF(4)=SMALLPAGE0
    SENDBUF(5)=SMALLPAGE1
    SENDBUF(6)=HUGEPAGE0
    SENDBUF(7)=HUGEPAGE1
    SENDBUF(8)=HEAP_SIZE
    SENDBUF(9)=TASKSMALL
#ifdef SFX_MPI
    CALL MPI_SEND(SENDBUF(1:9),9,MPI_INTEGER8,0,ITAG+1,KCOMM,ERROR)
    IF(ERROR /= 0 ) THEN
       WRITE(0,*) CLPFX(1:IPFXLEN)//"## EC_CRAY_MEMINFO error code ",ERROR," from MPI_SEND"
       CALL MPI_ABORT(KCOMM,-1,ERROR)
    ENDIF
#endif
ENDIF
#ifdef SFX_MPI
IF (.not.LLNOCOMM) CALL MPI_BARRIER(KCOMM,ERROR)
#endif
END SUBROUTINE EC_CRAY_MEMINFO

SUBROUTINE MEMINFO(KOUT,KSTEP)
IMPLICIT NONE
INTEGER(KIND=4), INTENT(IN) :: KOUT, KSTEP
CHARACTER(LEN=32) CLSTEP
CHARACTER(LEN=160) :: LINE
CHARACTER(LEN=8) :: NODENAME
INTEGER(KIND=8) :: NODE(0:17), ISMALL, IHUGE, ITOTAL
INTEGER(KIND=4) :: I,INUMA,ICOMM
WRITE(CLSTEP,'(11X,"STEP",I5," :")') KSTEP
ICOMM = -2 ! No headers from EC_CRAY_MEMINFO by default
IF (KSTEP == 0) ICOMM = -1 ! Do print headers, too
#ifdef _CRAYFTN
CALL EC_CRAY_MEMINFO(KOUT,TRIM(CLSTEP),ICOMM)
#endif
CALL FLUSH(KOUT)
RETURN ! For now
#ifndef DARWIN
CALL EC_GETHOSTNAME(NODENAME) ! from support/env.c
#endif
OPEN(FILE="/proc/buddyinfo",UNIT=502,ERR=98)
READ(502,'(a)') LINE
READ(502,'(a)') LINE
DO INUMA=0,1
   NODE(:)=0
   READ(502,'(a)',END=99) LINE
   READ(LINE(22:160),*,ERR=99,END=99) NODE
   ISMALL = 0
   DO I=0,8
      ISMALL = ISMALL + NODE(I) * (2**I)
   ENDDO
   ! Pages >= 2M
   IHUGE = 0
   DO I=9,SIZE(NODE)-1
      IHUGE = IHUGE + NODE(I) * (2**I)
   ENDDO
   ITOTAL = ISMALL + IHUGE
   ISMALL = (ISMALL * 4096)/(1024*1024)
   IHUGE = (IHUGE * 4096)/(1024*1024)
   ITOTAL = (ITOTAL * 4096)/(1024*1024)
   WRITE(KOUT,'("   MEMINFO: STEP=",I0," ",A," NUMA# ",I0," : Free Total = SMALL + HUGEPAGES in MB: ",I0," = ",I0," + ",I0)') &
        & KSTEP, NODENAME, INUMA, ITOTAL, ISMALL, IHUGE
   WRITE(KOUT,'(" BUDDYINFO: STEP=",I0," ",A," NUMA# ",I0," : Count of free 2^(0..",I0,")*4096B blocks: ",A)') &
        & KSTEP, NODENAME, INUMA, SIZE(NODE)-1, LINE(22:160)
ENDDO
99 CONTINUE
CLOSE(502)
98 CONTINUE
CALL FLUSH(KOUT)
END SUBROUTINE MEMINFO
