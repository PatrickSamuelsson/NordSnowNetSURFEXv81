! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FAGRTR_FORT           &
&                     (FA, KREP, KUNIT)
USE FA_MOD, ONLY : FA_COM, FAGR1TAB
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KUNIT
!
INTEGER (KIND=JPLIKB) JJ
INTEGER (KIND=JPLIKB) IVERS

INTEGER :: IOST

REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('FAGRTR_MT',0,ZHOOK_HANDLE)

KREP=0

IF (ASSOCIATED (FA%YGR1TAB)) THEN
  DEALLOCATE (FA%YGR1TAB)
  NULLIFY (FA%YGR1TAB)
ENDIF

IOST = 0

READ (KUNIT, '("VERSION = ",I10)', ERR=999, IOSTAT=IOST) IVERS

SELECT CASE (IVERS)
  CASE (1)
    CALL FAGRTR_FORT_V1
  CASE DEFAULT
    WRITE (FA%NULOUT, *) "UNKNOWN VERSION :", IVERS
    CALL SDL_SRLABORT
END SELECT

999 CONTINUE

IF (IOST /= 0) THEN
  KREP = IOST
ENDIF

IF (LHOOK) CALL DR_HOOK('FAGRTR_MT',1,ZHOOK_HANDLE)

CONTAINS

SUBROUTINE FAGRTR_FORT_V1

READ (KUNIT, '(I10)', ERR=999, IOSTAT=IOST) FA%NBPARC
ALLOCATE (FA%YGR1TAB (FA%NBPARC))

DO JJ = 1, FA%NBPARC
  READ (KUNIT, '(" | ",A8," | ",A24," | ",7I10," | ",L1," | ",E23.16," | ",L1," | ")', &
&       ERR=999, IOSTAT=IOST)                                                          &
&       FA%YGR1TAB (JJ)%CIPREF, FA%YGR1TAB (JJ)%CISUFF, FA%YGR1TAB (JJ)%NCODPA(1:7),   &
&       FA%YGR1TAB (JJ)%LFNIVA, FA%YGR1TAB (JJ)%FMULTI, FA%YGR1TAB (JJ)%LMULTI
ENDDO

999 CONTINUE

END SUBROUTINE FAGRTR_FORT_V1

END SUBROUTINE FAGRTR_FORT

! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FAGRTR64                    &
&           (KREP, KUNIT)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                  FA_COM_DEFAULT_INIT,  &
&                  NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KUNIT                                  ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAGRTR_FORT                           &
&           (FA, KREP, KUNIT)

END SUBROUTINE FAGRTR64

SUBROUTINE FAGRTR                      &
&           (KREP, KUNIT)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                  FA_COM_DEFAULT_INIT,  &
&                  NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KUNIT                                  ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAGRTR_MT                             &
&           (FA, KREP, KUNIT)

END SUBROUTINE FAGRTR

SUBROUTINE FAGRTR_MT                       &
&           (FA, KREP, KUNIT)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KUNIT                                  !   OUT
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  IUNIT                                  ! IN   
! Convert arguments

IUNIT      = INT (     KUNIT, JPLIKB)

CALL FAGRTR_FORT                           &
&           (FA, IREP, IUNIT)

KREP       = INT (      IREP, JPLIKM)

END SUBROUTINE FAGRTR_MT

!INTF KREP            OUT
!INTF KUNIT         IN                                                                 

