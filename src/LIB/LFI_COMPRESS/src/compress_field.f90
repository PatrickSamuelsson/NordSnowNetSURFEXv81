!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source: /home/cvsroot/mesonh/libtools/lib/COMPRESS/src/compress.f90,v $ $Revision: 1.1.2.1 $ $Date: 2007/03/02 13:15:26 $
!-----------------------------------------------------------------
SUBROUTINE COMPRESS_FIELD(XTAB,KX,KY,KNBTOT,KNBUSE)
USE MODD_COMPPAR
USE MODE_SEARCHGRP

#ifdef NAGf95
USE,INTRINSIC :: IEEE_ARITHMETIC
#endif

IMPLICIT NONE 

REAL,PARAMETER :: PPFLOATMIN = 2.0**(-126)

INTEGER, INTENT(IN) :: KX,KY
!INTEGER, INTENT(IN) :: KNBLEV
INTEGER, INTENT(IN) :: KNBTOT
REAL(KIND=8),DIMENSION(KNBTOT),INTENT(INOUT) :: XTAB

INTEGER, INTENT(OUT) :: KNBUSE

INTEGER :: INBLEV
INTEGER,DIMENSION(:), ALLOCATABLE :: ITAB
REAL :: XMIN,XMAX
TYPE(SOP_t) :: SOPRES
INTEGER :: IND1, IND2
INTEGER :: GELT,IBE
INTEGER :: ILEVNBELT
INTEGER :: NBITCOD
INTEGER :: II, JI, JJ
INTEGER :: BITOFFSET
INTEGER :: GRPIDX,GRPOFF,IDXSAVE,OFFSAVE
INTEGER :: nbgroupmod
INTEGER :: IEXTCOD
CHARACTER(LEN=8),PARAMETER :: KEYWORD='COMPRESS'
REAL,DIMENSION(KNBTOT) :: XWORKTAB
LOGICAL :: LUPREAL,LNAN
#ifndef NAGf95
LOGICAL, EXTERNAL :: IEEE_IS_NAN
#endif

ILEVNBELT = KX*KY
LUPREAL = .FALSE.
LNAN    = .FALSE.

! Check for NAN and change Upper and Lower bound according to 32bits real limits.
DO JI=1,KNBTOT
  IF (IEEE_IS_NAN(XTAB(JI))) THEN 
    XTAB(JI)=0.
    LNAN = .TRUE.
  ELSE IF (ABS(XTAB(JI)) > HUGE(1.0_4)) THEN
    XTAB(JI) = SIGN(REAL(HUGE(1.0_4)/1.1,8),XTAB(JI))
    LUPREAL = .TRUE.
  ELSEIF (ABS(XTAB(JI)) < TINY(1.0_4)) THEN
    XTAB(JI) = 0.
  END IF
END DO

XMIN=MINVAL(XTAB)
XMAX=MAXVAL(XTAB)
PRINT *,'MINVAL,MAXVAL= ',XMIN,XMAX
IF (LNAN) PRINT *,"==================> NAN values DETECTED : set to 0.0"
IF (LUPREAL) PRINT *,"==================> OVERFLOW values DETECTED : set to ",HUGE(1.0_4)/1.1

! Convert 64 bits real to 32 bits real
XWORKTAB(:) = XTAB(:)
!
! BEWARE : Now XTAB is overwritten. 
!          XWORKTAB contains the 32 bits floating point data.
!
CALL SET_FILLIDX(0,0)
! store 8 characters header string in buffer
DO II=1,LEN(KEYWORD)
  CALL FILL_BBUFF(XTAB,8,ICHAR(KEYWORD(II:II)))
END DO

! is whole array XTAB64 a constant field ?

IF (xmin == xmax) THEN
  PRINT *,"--------> CONSTANT ARRAY !"
  CALL FILL_BBUFF(XTAB,32,JPCSTENCOD)
  CALL FILL_BBUFF(XTAB,32,KNBTOT)
  CALL FILL_BBUFF(XTAB,32,xmin)
  CALL GET_FILLIDX(KNBUSE,BITOFFSET)
  KNBUSE=KNBUSE+1
  RETURN
END IF


INBLEV = KNBTOT/(ILEVNBELT)
IF (KNBTOT /= (INBLEV*ILEVNBELT)) THEN
  PRINT *,'Pb in COMPRESS_FIELD : KNBTOT must be a multiple of KX*KY'
  STOP
END IF



ALLOCATE(ITAB(ILEVNBELT))
CALL INI_SOPDATA(SOPRES)

CALL FILL_BBUFF(XTAB,32,JPEXTENCOD)
CALL FILL_BBUFF(XTAB,32,KNBTOT)
CALL FILL_BBUFF(XTAB,32,KX)
CALL FILL_BBUFF(XTAB,32,KY)

DO JI=1,INBLEV
  IND1=(JI-1)*ILEVNBELT+1
  IND2=JI*ILEVNBELT
  IF (LPDEBUG) PRINT *,"---- Compressing Level ",JI," ----"
  CALL COMP_FOPEXT(XWORKTAB(IND1:IND2),ITAB,IEXTCOD)
  IF (IEXTCOD /= JPCONST) THEN
    CALL INVERTCOL(ITAB,KX,KY)
    CALL RECSEARCH(ITAB,SOPRES)
    GELT = MAXVAL(SOPRES%IEND(1:SOPRES%NBGRP)-SOPRES%IBEG(1:SOPRES%NBGRP)+1)
    IBE = FMINBITS_IN_WORD(GELT)
    CALL GET_FILLIDX(GRPIDX,GRPOFF) ! save the idx/offset for future NBGRP modification
    CALL FILL_BBUFF(XTAB,32,SOPRES%NBGRP)
    CALL FILL_BBUFF(XTAB,5,IBE)
  
    NBGROUPMOD = SOPRES%NBGRP
    DO II=1,SOPRES%NBGRP
      GELT = SOPRES%IEND(II)-SOPRES%IBEG(II)+1
      nbitcod  = FMINBITS_IN_WORD(SOPRES%VALMAX(II)-SOPRES%VALMIN(II))
      !    PRINT *, 'Groupe',II,'(',GELT,')',':',SOPRES%IBEG(II),SOPRES%IEND(II),&
      !         &'MIN,MAX=',SOPRES%VALMIN(II),SOPRES%VALMAX(II),&
      !         &'(',SOPRES%VALMAX(II)-SOPRES%VALMIN(II),'/',&
      !         &nbitcod,')'
      IF (nbitcod >= 16) THEN
        PRINT *,'-----> ERREUR FATALE : Groupe',II,'codage sur ',nbitcod,'bits'
      END IF
      IF (GELT > 1) THEN
        ! Plus d'un element dans le groupe
        IF ((17*GELT) < (17+4+IBE+nbitcod*GELT)) THEN
          ! on prefere GELT groupes de 1 elt
          DO JJ=SOPRES%IBEG(II),SOPRES%IEND(II)
            ! 1 seul elt par groupe
            CALL FILL_BBUFF(XTAB,1,1)
            CALL FILL_BBUFF(XTAB,16,ITAB(JJ))
          END DO
          NBGROUPMOD = NBGROUPMOD+GELT-1
        ELSE
          CALL FILL_BBUFF(XTAB,1,0)
          CALL FILL_BBUFF(XTAB,16,SOPRES%VALMIN(II))
          CALL FILL_BBUFF(XTAB,4,nbitcod)
          CALL FILL_BBUFF(XTAB,IBE,GELT)
          IF (nbitcod > 0) THEN
            DO JJ=SOPRES%IBEG(II),SOPRES%IEND(II)
              ! stockage des GELT ?carts/VALMIN
              CALL FILL_BBUFF(XTAB,nbitcod,ITAB(JJ)-SOPRES%VALMIN(II))
            END DO
          END IF
        END IF
      ELSE
        ! 1 seul elt dans groupe
        CALL FILL_BBUFF(XTAB,1,1)
        CALL FILL_BBUFF(XTAB,16,SOPRES%VALMIN(II))
      END IF
    END DO
    IF (NBGROUPMOD > SOPRES%NBGRP) THEN
      ! we must change the number of elements 
      CALL GET_FILLIDX(IDXSAVE,OFFSAVE) ! save the current idx/offset
      CALL SET_FILLIDX(GRPIDX,GRPOFF)   
      CALL FILL_BBUFF(XTAB,32,NBGROUPMOD)
      CALL SET_FILLIDX(IDXSAVE,OFFSAVE) ! restore the current idx/offset
    END IF
  END IF
END DO

CALL GET_FILLIDX(IDXSAVE,OFFSAVE)
KNBUSE=IDXSAVE+1

DEALLOCATE(ITAB)

CONTAINS 

SUBROUTINE COMP_FOPEXT(PTAB,KTAB,KEXTCOD)
REAL,    DIMENSION(:), INTENT(IN) :: PTAB
INTEGER, DIMENSION(:), INTENT(OUT):: KTAB 
INTEGER,               INTENT(OUT):: KEXTCOD

LOGICAL,DIMENSION(SIZE(PTAB)) :: GMASK
REAL :: XMIN1,XMAX1,XRANGE1
REAL :: XMIN2,XMAX2,XRANGE2
REAL :: XREF,XMAX,XCOEFF
INTEGER :: INTRANGE
INTEGER :: INDCOR   ! correction d'index pour la supression du min
LOGICAL :: GMINEXCL,GMAXEXCL,GLOG
INTEGER :: IEXTCOD2

XMIN1=MINVAL(PTAB(:))
XMAX1=MAXVAL(PTAB(:))
XRANGE1=XMAX1-XMIN1
IF (LPDEBUG) PRINT *,"XMIN1,XMAX1,XRANGE1 = ",XMIN1,XMAX1,XRANGE1

IF (XRANGE1 > 0.) THEN
  XMIN2=MINVAL(PTAB,MASK=PTAB>XMIN1)
  XMAX2=MAXVAL(PTAB,MASK=PTAB<XMAX1)
  XRANGE2 = XMAX2-XMIN2
  IF (LPDEBUG) PRINT *,"XMIN2,XMAX2,XRANGE2 = ",XMIN2,XMAX2,XRANGE2
  IF (XRANGE2 > 0.) THEN
    GLOG     = .FALSE.
    GMINEXCL = .FALSE.
    GMAXEXCL = .FALSE.
    GMASK(:) = .TRUE.
    INDCOR = 0
    KEXTCOD = JPNORM
    INTRANGE=65535
    XREF = XMIN1
    XMAX = XMAX1

    ! Check for range between 0 and 1 to convert to LOG values
    IF (XMIN1 >= 0. .AND. XMAX1 < 1.) THEN
      IF ((XMAX2/XMIN2)>10.) THEN 
        GLOG = .TRUE.
        KEXTCOD = JPOTHER
        IEXTCOD2 = JPLOG
        INTRANGE=INTRANGE-1
        INDCOR = 1           ! On reserve la valeur 0 dans tous les cas
        IF (XMIN1 == 0.0) THEN
          XREF = LOG(XMIN2)
          WHERE (PTAB < XMIN2)
            KTAB  = 0
            GMASK = .FALSE.
          END WHERE
        ELSE
          XREF = LOG(XMIN1)
        END IF
        XMAX1 = LOG(XMAX1)
        XMAX  = XMAX1
        XMAX2 = LOG(XMAX2)
        XRANGE2 = XMAX2 - XREF
        IF (LPDEBUG) PRINT *,"EXTENCOD,  LOG conversion enabled : XMIN1, XREF, XMAX1, XMAX2 =",&
             &XMIN1,XREF,XMAX1,XMAX2
      END IF
    ELSE
      ! Check for MIN value exclusion
      IF ((XMIN2-XMIN1) > XRANGE2) THEN
        ! Min value excluded 
        GMINEXCL = .TRUE.
        XREF=XMIN2
        INTRANGE=INTRANGE-1
        INDCOR = 1
        WHERE (PTAB < XMIN2)
          KTAB = 0
          GMASK = .FALSE.
        END WHERE
        IF (LPDEBUG) PRINT *,"EXTENCOD,     Min value isolated :",XMIN1
        KEXTCOD = JPMINEXCL
      END IF
      ! Check for MAX value exclusion
      IF ((XMAX1-XMAX2) > XRANGE2) THEN
        ! Max value excluded
        GMAXEXCL = .TRUE.
        XMAX=XMAX2
        INTRANGE=INTRANGE-1
        WHERE (PTAB > XMAX2)
          KTAB = 65535
          GMASK = .FALSE.
        END WHERE
        
        IF (GMINEXCL) THEN
          KEXTCOD = JPMINMAXEXCL ! Min et Max exclus
          IF (LPDEBUG) PRINT *,"EXTENCOD, and Max value isolated :",XMAX1
        ELSE
          KEXTCOD = JPMAXEXCL ! Max exclus
          IF (LPDEBUG) PRINT *,"EXTENCOD, Max value isolated :",XMAX1
        END IF
      END IF
    END IF
    !
    XCOEFF=(XMAX-XREF)/INTRANGE
    IF (XCOEFF < PPFLOATMIN) THEN
      XCOEFF = PPFLOATMIN
      PRINT *, "very low range DATA : XCOEFF set to",XCOEFF
    END IF
    IF (LPDEBUG) PRINT *,"XCOEFF = ",XCOEFF
    IF (GLOG) THEN
      WHERE(GMASK)
        KTAB = INDCOR + NINT((LOG(PTAB)-XREF)/XCOEFF)
      END WHERE
    ELSE
      WHERE(GMASK)
        KTAB = INDCOR + NINT((PTAB(:)-XREF)/XCOEFF)
      END WHERE
    END IF
    IF (LPDEBUG) PRINT *,"KEXTCOD = ",KEXTCOD
    CALL FILL_BBUFF(XTAB,3,KEXTCOD)
    IF (GLOG)     CALL FILL_BBUFF(XTAB,3,IEXTCOD2)
    IF (GMINEXCL) CALL FILL_BBUFF(XTAB,32,XMIN1)
    IF (GMAXEXCL) CALL FILL_BBUFF(XTAB,32,XMAX1)
    CALL FILL_BBUFF(XTAB,32,XREF)
    CALL FILL_BBUFF(XTAB,32,XCOEFF)
  ELSE
    IF (XRANGE2 < 0.) THEN
      ! only 2 values in PTAB array
      !
      ! KTAB(i)= 0 if PTAB(i)==XMIN1
      !          1 if PTAB(i)==XMAX1
      !
      IF (LPDEBUG) PRINT *,"EXTENCOD, 2 values in array :",XMIN1,XMAX1
      KEXTCOD = JP2VAL
      CALL FILL_BBUFF(XTAB,3,KEXTCOD)
      CALL FILL_BBUFF(XTAB,32,XMIN1)
      CALL FILL_BBUFF(XTAB,32,XMAX1)
      WHERE (PTAB < XMAX1) 
        KTAB = 0
      ELSEWHERE
        KTAB = 1
      END WHERE
    ELSE
      ! XRANGE2 == 0. <==> XMIN2=XMAX2 
      ! 3 values in PTAB array :
      !
      !          0 if PTAB(i)==XMIN1      ! KTAB(i)= 1 if PTAB(i)==XMIN2(=XMAX2)
      !          2 if PTAB(i)==XMAX1
      !
      IF (LPDEBUG) PRINT *,"EXTENCOD, 3 values in array :",XMIN1,XMIN2,XMAX1
      KEXTCOD = JP3VAL
      CALL FILL_BBUFF(XTAB,3,KEXTCOD)
      CALL FILL_BBUFF(XTAB,32,XMIN1)
      CALL FILL_BBUFF(XTAB,32,XMIN2)
      CALL FILL_BBUFF(XTAB,32,XMAX1)
      WHERE (PTAB < XMIN2)
        KTAB = 0
      ELSEWHERE
        KTAB = 1
      END WHERE
      WHERE (PTAB > XMIN2) KTAB = 2
    END IF
    
  END IF
ELSE
  ! Constant array found : save its 32 bits real value.
  KEXTCOD=JPCONST
  CALL FILL_BBUFF(XTAB,3,KEXTCOD)
  CALL FILL_BBUFF(XTAB,32,XMIN1)
  IF (LPDEBUG) PRINT *,"EXTENCOD, constant array : ",XMIN1
END IF
END SUBROUTINE COMP_FOPEXT

END SUBROUTINE COMPRESS_FIELD
