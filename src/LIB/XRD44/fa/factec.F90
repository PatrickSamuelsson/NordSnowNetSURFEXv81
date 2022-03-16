! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FACTEC_FORT                                      &
&                     (FA, KREP, PA, KNBIT, KDEC, KE, KNUTIL)
USE FA_MOD, ONLY : FA_COM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!      Sous-programme de calcul du FACTeur d'EChelle binaire associe
!     a un champ d'amplitude donnee, code sur KNBIT bits.
!**
!    Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                PA     (Entree) ==> Amplitude du champ a compacter;
!                KNBIT  (Entree) ==> Nb de bits servant au compactage;
!                KDEC   (Entree) ==> Facteur d'echelle decimal;
!                KE     (Sortie) ==> Facteur d'echelle binaire;
!                KNUTIL (Sortie) ==> Nombre d'entiers utilises pour
!                                    representer le champ.
!
!     Modifications:
!
TYPE(FA_COM) :: FA
REAL (KIND=JPDBLR) PA
INTEGER (KIND=JPLIKB) KREP, KNBIT, KDEC, KE, KNUTIL
!
REAL (KIND=JPDBLR) ZTWO, ZHALF, ZTEN

!
!**
!     1.  -  CONTROLES ET INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FACTEC_MT',0,ZHOOK_HANDLE)
ZTWO   = 2._JPDBLR
ZHALF  = 0.5_JPDBLR
ZTEN   = 10._JPDBLR
KREP   = 0
KE     = 0
KNUTIL = 0
IF (KNBIT.LE.0 .OR. KNBIT.GT.64) THEN
  KREP=-1
  WRITE (UNIT=FA%NULOUT,FMT=*)'****'
  WRITE (UNIT=FA%NULOUT,FMT=*)                               &
&         '**** FACTEC: ERROR, bits number out of range 1-64'
  WRITE (UNIT=FA%NULOUT,FMT=*)'****         KNBIT = ',KNBIT
  WRITE (UNIT=FA%NULOUT,FMT=*)                                 &
&         '****         Binary scale factor is not computed !!'
  WRITE (UNIT=FA%NULOUT,FMT=*)'****'
  GOTO 1001
ENDIF
IF ( ABS(PA).LT.TINY(PA) ) THEN
  WRITE (UNIT=FA%NULOUT,FMT=*)'----'
  WRITE (UNIT=FA%NULOUT,FMT=*)                              &
&         '---- FACTEC: Warning, the range of the field is', &
&         ' considered as zero'
  WRITE (UNIT=FA%NULOUT,FMT=*)'----'
  KE = 0
  KNUTIL = 1
  GOTO 1001
ENDIF
IF ( ABS(LOG10(ABS(PA))+REAL (KDEC, JPDBLR))  &
&     .GE. REAL (RANGE(PA), JPDBLR) ) THEN
  KREP=-1
  WRITE (UNIT=FA%NULOUT,FMT=*)'****'
  WRITE (UNIT=FA%NULOUT,FMT=*)                                  &
&                '**** FACTEC: ERROR, PA*10**KDEC exceeds real', &
&                'representation of KIND=',JPDBLR
  WRITE (UNIT=FA%NULOUT,FMT=*)                              &
&         '****         LOG10(ABS(PA)), KDEC, RANGE(PA) = ', &
&         LOG10(ABS(PA)), KDEC, RANGE(PA)
  WRITE (UNIT=FA%NULOUT,FMT=*)                                 &
&         '****         Binary scale factor is not computed !!'
  WRITE (UNIT=FA%NULOUT,FMT=*)'****'
  GOTO 1001
ENDIF
!
!**
!     2.  -  CALCUL DU FACTEUR D'ECHELLE BINAIRE
!-----------------------------------------------------------------------
!
KE = FLOOR( LOG( (PA*10._8**KDEC) /                 &
&                 (2._8**KNBIT-0.5_8) )/LOG(2._8),  &
&                 KIND=JPLIKB ) + 1
KNUTIL = FLOOR( 0.5_8 + PA*(10._8**KDEC)*(2._8**(-KE)), &
&                KIND=JPLIKB )

1001 CONTINUE

IF (LHOOK) CALL DR_HOOK('FACTEC_MT',1,ZHOOK_HANDLE)
END SUBROUTINE


!INTF KREP            OUT 
!INTF PA            IN    
!INTF KNBIT         IN    
!INTF KDEC          IN    
!INTF KE              OUT 
!INTF KNUTIL          OUT 
