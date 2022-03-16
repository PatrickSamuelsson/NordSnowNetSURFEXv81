! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FAVORI_FORT                                              &
&                     (FA,  KNGRIB, KNBPDG, KNBCSP, KSTRON, KPUILA, &
&                      KDMOPL)
USE FA_MOD, ONLY : FA_COM, JPNIIL
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Ce sous-programme permet de connaitre les options implicites
!     courantes liees au codage GRIB des champs.
!     CES OPTIONS NE SONT UTILISEES QUE POUR (RE)ECRIRE DES CHAMPS
!     codes en GRIB, et les valeurs implicites ne servent
!     que LORS d'une OUVERTURE de FICHIER.
!       ( Visualisation (?) Options R (??) Implicites )
!**
!     Arguments : KNGRIB ==> Niveau de codage GRIB (-1,0,1,2,3);
!     (tous de    KNBPDG ==> Nombre de bits par valeur point-de-grille;
!      SORTIE)    KNBCSP ==> Nombre de bits par partie reelle/imaginaire
!                            de coeff. spectral;
!                 KSTRON ==> Sous-troncature non compactee;
!                 KPUILA ==> Puissance de laplacien;
!                 KDMOPL ==> Degre de modulation de KPUILA.
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KNGRIB, KNBPDG, KNBCSP
INTEGER (KIND=JPLIKB) KSTRON, KPUILA, KDMOPL
!
INTEGER (KIND=JPLIKB) IREP, INIMES, INUMER
!
LOGICAL LLVERG
!
!
!
CHARACTER(LEN=FA%JPXNOM) CLACTI 
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR

!**
!     1.  -  INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FAVORI_MT',0,ZHOOK_HANDLE)
CLACTI=''
IF (FA%FAVORI_LLPREA) THEN
!
!          A la premiere utilisation, appel au sous-programme "FARINE".
!
  CALL FARINE_FORT             &
&                (FA, 2_JPLIKB )
  FA%FAVORI_LLPREA=.FALSE.
ENDIF
!
!         Verrouillage global eventuel.
!
IF (FA%LFAMUL) CALL LFIVER_FORT                        &
&                              (FA%LFI, FA%VRGLAS,'ON')
LLVERG=FA%LFAMUL
!**
!     2.  -  RECOPIE DES VALEURS EN COMMON DANS LES ARGUMENTS.
!-----------------------------------------------------------------------
!
KNGRIB=FA%NIGRIB
KNBPDG=FA%NBIPDG
KNBCSP=FA%NBICSP
KSTRON=FA%NSTROI
KPUILA=FA%NPUILA
KDMOPL=FA%NMIDPL
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE VIA LE SOUS-PROGRAMME "FAIPAR"
!-----------------------------------------------------------------------
!
!
!        Deverrouillage global eventuel.
!
IF (LLVERG) CALL LFIVER_FORT                         &
&                           (FA%LFI, FA%VRGLAS,'OFF')
!
IF (FA%NIMSGA.EQ.2) THEN
  IREP=0
  INIMES=2
  CLNSPR='FAVORI'
  WRITE (UNIT=CLMESS,FMT='(''KNGRIB='',I2,'', KNBPDG='',I3,  &
&         '', KNBCSP='',I3,'', KSTRON='',I3,'', KPUILA='',I3, &
&         '', KDMOPL='',I3)')                                 &
&    KNGRIB,KNBPDG,KNBCSP,KSTRON,KPUILA,KDMOPL
  INUMER=JPNIIL
  CALL FAIPAR_FORT                                      &
&                 (FA, INUMER,INIMES,IREP,.FALSE.,CLMESS, &
&                  CLNSPR,CLACTI,.FALSE.)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('FAVORI_MT',1,ZHOOK_HANDLE)
END SUBROUTINE FAVORI_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FAVORI64                                        &
&           (KNGRIB, KNBPDG, KNBCSP, KSTRON, KPUILA, KDMOPL)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KNGRIB                                 !   OUT
INTEGER (KIND=JPLIKB)  KNBPDG                                 !   OUT
INTEGER (KIND=JPLIKB)  KNBCSP                                 !   OUT
INTEGER (KIND=JPLIKB)  KSTRON                                 !   OUT
INTEGER (KIND=JPLIKB)  KPUILA                                 !   OUT
INTEGER (KIND=JPLIKB)  KDMOPL                                 !   OUT

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAVORI_FORT                                               &
&           (FA, KNGRIB, KNBPDG, KNBCSP, KSTRON, KPUILA, KDMOPL)

END SUBROUTINE FAVORI64

SUBROUTINE FAVORI                                          &
&           (KNGRIB, KNBPDG, KNBCSP, KSTRON, KPUILA, KDMOPL)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KNGRIB                                 !   OUT
INTEGER (KIND=JPLIKM)  KNBPDG                                 !   OUT
INTEGER (KIND=JPLIKM)  KNBCSP                                 !   OUT
INTEGER (KIND=JPLIKM)  KSTRON                                 !   OUT
INTEGER (KIND=JPLIKM)  KPUILA                                 !   OUT
INTEGER (KIND=JPLIKM)  KDMOPL                                 !   OUT

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAVORI_MT                                                 &
&           (FA, KNGRIB, KNBPDG, KNBCSP, KSTRON, KPUILA, KDMOPL)

END SUBROUTINE FAVORI

SUBROUTINE FAVORI_MT                                           &
&           (FA, KNGRIB, KNBPDG, KNBCSP, KSTRON, KPUILA, KDMOPL)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KNGRIB                                 !   OUT
INTEGER (KIND=JPLIKM)  KNBPDG                                 !   OUT
INTEGER (KIND=JPLIKM)  KNBCSP                                 !   OUT
INTEGER (KIND=JPLIKM)  KSTRON                                 !   OUT
INTEGER (KIND=JPLIKM)  KPUILA                                 !   OUT
INTEGER (KIND=JPLIKM)  KDMOPL                                 !   OUT
! Local integers
INTEGER (KIND=JPLIKB)  INGRIB                                 !   OUT
INTEGER (KIND=JPLIKB)  INBPDG                                 !   OUT
INTEGER (KIND=JPLIKB)  INBCSP                                 !   OUT
INTEGER (KIND=JPLIKB)  ISTRON                                 !   OUT
INTEGER (KIND=JPLIKB)  IPUILA                                 !   OUT
INTEGER (KIND=JPLIKB)  IDMOPL                                 !   OUT
! Convert arguments


CALL FAVORI_FORT                                               &
&           (FA, INGRIB, INBPDG, INBCSP, ISTRON, IPUILA, IDMOPL)

KNGRIB     = INT (    INGRIB, JPLIKM)
KNBPDG     = INT (    INBPDG, JPLIKM)
KNBCSP     = INT (    INBCSP, JPLIKM)
KSTRON     = INT (    ISTRON, JPLIKM)
KPUILA     = INT (    IPUILA, JPLIKM)
KDMOPL     = INT (    IDMOPL, JPLIKM)

END SUBROUTINE FAVORI_MT

!INTF KNGRIB          OUT 
!INTF KNBPDG          OUT 
!INTF KNBCSP          OUT 
!INTF KSTRON          OUT 
!INTF KPUILA          OUT 
!INTF KDMOPL          OUT 
