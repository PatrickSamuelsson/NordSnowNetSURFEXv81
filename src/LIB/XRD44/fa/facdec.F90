! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FACDEC_FORT                                &
&                     (FA, KREP, PA, PMIN, KNBIT, KDEC)
USE FA_MOD, ONLY : FA_COM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!      Sous-programme de calcul du FACteur d'echelle DECimal optimal
!     pour coder un champ d'amplitude PA donnee, sur KNBIT bits avec
!     une precision maximale, compte tenu d'un intervalle fixe
!     de facteurs decimaux.
!**
!    Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                                    0: OK; -1: pb rencontre
!                PA     (Entree) ==> Amplitude du champ a compacter;
!                PMIN   (Entree) ==> Minimum du champ a compacter;
!                KNBIT  (Entree) ==> Nb de bits servant au compactage;
!                KDEC   (Sortie) ==> Facteur d'echelle decimal optimal;
!
!     Modifications:
!      R. El Khatib 12-Aug-2009 Bugfix for compatibility with Gribex
!      R. El Khatib 22-Jul-2015 Threshold for the field amplitude raised
!        from 1.E-12 to 1.E-11 in order to avoid the deterioration of too flat fields
!
TYPE(FA_COM) :: FA
REAL (KIND=JPDBLR) PA, PMIN
INTEGER (KIND=JPLIKB) KREP, KNBIT, KDEC
!
REAL (KIND=JPDBLR) XNBINT, XTINYR4, XHUGER4
INTEGER (KIND=JPLIKB) IDECMIN, IDECMAX, INBINT
INTEGER (KIND=JPLIKB) JDEC, IE, INUTIL, INUMAX, IEMAX
INTEGER (KIND=JPLIKB) INU0, IE0

!
!**
!     1.  -  CONTROLES ET INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FACDEC_MT',0,ZHOOK_HANDLE)
IF (KNBIT.LE.0 .OR. KNBIT.GT.64) THEN
  KREP=-1
  WRITE (UNIT=FA%NULOUT,FMT=*)'****'
  WRITE (UNIT=FA%NULOUT,FMT=*)                               &
&         '**** FACDEC: ERROR, bits number out of range 1-64'
  WRITE (UNIT=FA%NULOUT,FMT=*)'****         KNBIT = ',KNBIT
  WRITE (UNIT=FA%NULOUT,FMT=*)                                   &
&         '**** ! Optimal decimal scale factor is not computed !'
  WRITE (UNIT=FA%NULOUT,FMT=*)'****'
  GOTO 1001
ENDIF
IF (ABS(PA) .LT. TINY(PA)) THEN
  KREP = 0
  KDEC = 0
  IF (FA%LFAMOP) THEN
    WRITE (UNIT=FA%NULOUT,FMT=*)'////'
    WRITE (UNIT=FA%NULOUT,FMT=*)                               &
&         '//// FACDEC: WARNING, range of the field is null :', &
&                        PA
    WRITE (UNIT=FA%NULOUT,FMT=*)'////'
  ENDIF
  GOTO 1001
ENDIF

KREP    = 0
INUMAX  = 0
IDECMIN = -15
IDECMAX =   5
XTINYR4 = TINY(1._4)
XHUGER4 = HUGE(1._4)
INBINT  = 2**KNBIT -1
XNBINT  = REAL (INBINT, JPDBLR)
! Cas du facteur decimal nul (reference a calculer dans tous les cas)
JDEC    = 0
CALL FACTEC_FORT                                    &
&              (FA, KREP, PA, KNBIT, JDEC, IE0, INU0)
!
!**
!     2.  -  BOUCLE TESTANT LES FACTEURS DECIMAUX
!-----------------------------------------------------------------------
!
!!OCL SCALAR
DO JDEC = IDECMIN, IDECMAX
!
! 3 tests sur la pertinence de ce facteur decimal
!
! 0/ PA * 10**JDEC > 1.E-11
!    Du fait d'un test exagere dans gribex 
  IF (PA * 10._JPDBLR**JDEC .LE. 1.E-11_JPDBLR) CYCLE
! 1/ PMIN * 10**JDEC > TINY(real*4) pour decodage eventuel
!    de la valeur de reference en arithmetique 32 bits IEEE.
  IF (ABS(PMIN) .GT. TINY(PMIN)) THEN
    IF (LOG10(ABS(PMIN))+REAL (JDEC, JPDBLR) .LE. LOG10( XTINYR4 ) ) CYCLE
  ELSE
!  instruction "bidon" pour que l'optimisation ne fasse pas
!  l'evaluation de LOG10(ABS(PMIN)) par anticipation (Pb sur VPP5000)
    PMIN = 0._JPDBLR
  ENDIF
! 2/ PA * 10**JDEC inclus dans le domaine de validite des real*8
  IF (ABS(LOG10(ABS(PA))+REAL (JDEC, JPDBLR)) .GE. REAL (RANGE(PA), JPDBLR)) CYCLE
!
  CALL FACTEC_FORT                                     &
&                (FA, KREP, PA, KNBIT, JDEC, IE, INUTIL)
  IF (KREP.NE.0) CYCLE
! 3/ PMIN*10**JDEC + (2**KNBIT-1)*2**IE < HUGE(real*4) pour decodage
!    eventuel du max du champ en arithmetique 32 bits IEEE.
  IF (PMIN*10._JPDBLR**JDEC + XNBINT*2._JPDBLR**IE .GE. XHUGER4) CYCLE
! 4/ Le facteur d'echelle binaire doit pouvoir etre code sur 1 octet,
!    il doit donc etre compris entre -126 et 127:
  IF (IE.LT.-126 .OR. IE.GT.127) CYCLE
!
  IF (INUTIL.GT.INUMAX) THEN
    INUMAX = INUTIL
    IEMAX  = IE
    KDEC   = JDEC
  ENDIF
ENDDO
!
IF (INUMAX.EQ.0) THEN
  KREP=-1
  WRITE (UNIT=FA%NULOUT,FMT=*)'****'
  WRITE (UNIT=FA%NULOUT,FMT=*)                                    &
&         '**** FACDEC: all the decimal factors comprised between'
  WRITE (UNIT=FA%NULOUT,FMT=*)                              &
&         '**** ',IDECMIN,' and ',IDECMAX,' are rejected !!'
  WRITE (UNIT=FA%NULOUT,FMT=*)                             &
&         '**** Range and min of the field are :', PA, PMIN
  WRITE (UNIT=FA%NULOUT,FMT=*) &
&         '****'
  WRITE (UNIT=FA%NULOUT,FMT=*)'****'
  GOTO 1001
ENDIF
IF (FA%LFAMOP) THEN
  WRITE (UNIT=FA%NULOUT,FMT=*)                                 &
&         'FACDEC: champ d''amplitude ',PA,' ,de minimum ',PMIN
  WRITE (UNIT=FA%NULOUT,FMT=*)                  &
&         '        => fact decimal opt de',KDEC, &
&          ' ,pour 1 fact binaire de ',IEMAX
  WRITE (UNIT=FA%NULOUT,FMT='(1X,A,I3,A,I9,A,I9,A,F5.1,A,E11.4)') &
&    ' Eff des',                                                   &
&    KNBIT,' bits = ',INUMAX,' sur ',INBINT,' soit: ',             &
&    REAL (INUMAX, JPDBLR)*100._JPDBLR/XNBINT,                     &
&    ' % et une precision de ',                                    &
&    10._JPDBLR**(-KDEC)*2._JPDBLR**(IEMAX-1)
  WRITE (UNIT=FA%NULOUT,FMT=*)                              &
&         '        a comparer, si le fact decimal = 0, avec'
  WRITE (UNIT=FA%NULOUT,FMT='(1X,A,I9,A,I9,A,F5.1,A,E11.4)') &
&   ' une efficacite de ',                                    &
&   INU0,' sur ',INBINT,' soit: ',                            &
&   REAL (INU0, JPDBLR)*100._JPDBLR/XNBINT,                   &
&   ' % et une precision de ',2._JPDBLR**(IE0-1)
ENDIF
!
1001 CONTINUE
!
IF (LHOOK) CALL DR_HOOK('FACDEC_MT',1,ZHOOK_HANDLE)
END SUBROUTINE FACDEC_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FACDEC64                     &
&           (KREP, PA, PMIN, KNBIT, KDEC)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
REAL (KIND=JPDBLR)     PA                                     ! IN   
REAL (KIND=JPDBLR)     PMIN                                   ! IN   
INTEGER (KIND=JPLIKB)  KNBIT                                  ! IN   
INTEGER (KIND=JPLIKB)  KDEC                                   !   OUT

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FACDEC_FORT                            &
&           (FA, KREP, PA, PMIN, KNBIT, KDEC)

END SUBROUTINE FACDEC64

SUBROUTINE FACDEC                       &
&           (KREP, PA, PMIN, KNBIT, KDEC)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
REAL (KIND=JPDBLR)     PA                                     ! IN   
REAL (KIND=JPDBLR)     PMIN                                   ! IN   
INTEGER (KIND=JPLIKM)  KNBIT                                  ! IN   
INTEGER (KIND=JPLIKM)  KDEC                                   !   OUT

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FACDEC_MT                              &
&           (FA, KREP, PA, PMIN, KNBIT, KDEC)

END SUBROUTINE FACDEC

SUBROUTINE FACDEC_MT                        &
&           (FA, KREP, PA, PMIN, KNBIT, KDEC)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
REAL (KIND=JPDBLR)     PA                                     ! IN   
REAL (KIND=JPDBLR)     PMIN                                   ! IN   
INTEGER (KIND=JPLIKM)  KNBIT                                  ! IN   
INTEGER (KIND=JPLIKM)  KDEC                                   !   OUT
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INBIT                                  ! IN   
INTEGER (KIND=JPLIKB)  IDEC                                   !   OUT
! Convert arguments

INBIT      = INT (     KNBIT, JPLIKB)

CALL FACDEC_FORT                            &
&           (FA, IREP, PA, PMIN, INBIT, IDEC)

KREP       = INT (      IREP, JPLIKM)
KDEC       = INT (      IDEC, JPLIKM)

END SUBROUTINE FACDEC_MT

!INTF KREP            OUT 
!INTF PA            IN    
!INTF PMIN          IN    
!INTF KNBIT         IN    
!INTF KDEC            OUT 
