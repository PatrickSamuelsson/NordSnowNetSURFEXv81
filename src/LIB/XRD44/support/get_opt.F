      FUNCTION GET_OPT(CLOPT,CLARG,LLMPOFF)
#ifdef SFX_MPI
      USE MPL_ARG_MOD
#endif
!
!     CRAY function
!
!     The get_opt utility breaks up options in command lines for easy parsing
!     by shell procedures and checks for legal options.
!     CLOPT is a string of recognized option letters.
!     CLARG is the option-argument.
!
!     Author : Vijay Saravane *METEO-FRANCE*
!
      USE PARKIND1, ONLY : JPIM
      IMPLICIT NONE
      INTEGER(KIND=JPIM) :: GET_OPT
      CHARACTER(LEN=*) :: CLOPT,CLARG
      CHARACTER(LEN=LEN(CLARG)) :: COPT
      CHARACTER(LEN=1) :: COPT1
      LOGICAL :: LLMPOFF
      INTEGER(KIND=JPIM) :: IARG, NBARGS
      DATA IARG /1/
      SAVE IARG
!
!-----------------------------------------------
!
#ifdef T3D
      NBARGS=IPXFARGC()
#else
      IF (LLMPOFF) THEN
        NBARGS=COMMAND_ARGUMENT_COUNT()
      ELSE
#ifdef SFX_MPI
        NBARGS=MPL_IARGC()
#endif
      ENDIF
#endif
! si on a lu tous les arguments on sort ...
      IF (IARG.GT.NBARGS) THEN
         GET_OPT = 0
         RETURN
      ENDIF
! sinon on prend l'option en cours ...
#ifdef T3D
      CALL PXFGETARG(IARG,COPT,ILEN,IERR)
#else
      IF (LLMPOFF) THEN
        CALL GETARG(IARG,COPT)
      ELSE
#ifdef SFX_MPI
        CALL MPL_GETARG(IARG,COPT)
#endif
      ENDIF
#endif
      COPT1=COPT(2:2)
! si c'est pas une bonne option on sort ...
      IF (INDEX(CLOPT,COPT1).EQ.0) THEN
         GET_OPT = 0
         RETURN
      ENDIF
! sinon on prend l'argument de l'option
! cet argument est utilise dans getvarg
      IF (LEN_TRIM(COPT).GE.3) THEN  ! l argument est colle a l'option
         CLARG=COPT(3:LEN_TRIM(COPT))
         IARG=IARG+1
      ELSE                           ! l argument est separe par un blanc
#ifdef T3D
         CALL PXFGETARG(IARG+1,CLARG,ILEN,IERR)
#else
         IF (LLMPOFF) THEN
           CALL GETARG(IARG+1,CLARG)
         ELSE
#ifdef SFX_MPI
           CALL MPL_GETARG(IARG+1,CLARG)
#endif
         ENDIF
#endif
         IARG=IARG+2
      ENDIF
!
! et on retourne la valeur du caractere de l'option
      GET_OPT = IACHAR(COPT1)
!
      RETURN
      ENDFUNCTION GET_OPT
!
!
#ifdef SV2
      SUBROUTINE GETARG(IARG, CLARG)
      USE PARKIND1, ONLY : JPIM
      IMPLICIT NONE
      INTEGER(KIND=JPIM), INTENT(IN) :: IARG
      CHARACTER(LEN=*), INTENT(OUT) :: CLARG
      INTEGER(KIND=JPIM) :: ILEN, IERR
      CALL PXFGETARG(IARG,CLARG,ILEN,IERR)
      ENDSUBROUTINE GETARG
#endif
