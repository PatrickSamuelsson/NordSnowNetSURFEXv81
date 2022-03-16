! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FAPRST_FORT                                             &
&                     (FA,  KREP, KNUMER, KNUMOD, KHCTPI, CDOPER, LDPRGR)
USE FA_MOD, ONLY : FA_COM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!      Sous-programme permettant de mettre en place les defauts de compression
!     utilises par PROGRID
!**
!    Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                KNUMER (Entree) ==> Numero de l'unite logique;
!                KNUMOD (Entree) ==> Numero du modele
!                KHCTPI (Entree) ==> Voir PROGRID
!                CDOPER (Entree) ==> Voir PROGRID
!                LDPRGR (Entree) ==> Activer ou non
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KNUMER, KNUMOD, KHCTPI
!
CHARACTER CDOPER
!
LOGICAL LDPRGR
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FAPRST_MT',0,ZHOOK_HANDLE)

KREP=0

IF (LDPRGR) THEN
  CALL FAREGU_FORT (FA, KNUMER, 'IDMOD',  KNUMOD,   1_JPLIKB)
  CALL FAREGU_FORT (FA, KNUMER, 'FACDEC', 0_JPLIKB, 1_JPLIKB)
  SELECT CASE (CDOPER)
    CASE ('K')
      CALL FAREGU_FORT (FA, KNUMER, 'GEXTE',  0_JPLIKB, 1_JPLIKB)
      CALL FAREGU_FORT (FA, KNUMER, 'BOUST',  0_JPLIKB, 1_JPLIKB)
      CALL FAREGU_FORT (FA, KNUMER, 'DIFFE',  0_JPLIKB, 1_JPLIKB)
    CASE ('X')
      CALL FAREGU_FORT (FA, KNUMER, 'GEXTE',  1_JPLIKB, 1_JPLIKB)
      CALL FAREGU_FORT (FA, KNUMER, 'BOUST',  1_JPLIKB, 1_JPLIKB)
      CALL FAREGU_FORT (FA, KNUMER, 'DIFFE', -1_JPLIKB, 1_JPLIKB)
    CASE ('L')
      CALL FAREGU_FORT (FA, KNUMER, 'GEXTE',  1_JPLIKB, 1_JPLIKB)
      CALL FAREGU_FORT (FA, KNUMER, 'BOUST',  1_JPLIKB, 1_JPLIKB)
      CALL FAREGU_FORT (FA, KNUMER, 'DIFFE',  2_JPLIKB, 1_JPLIKB)
  END SELECT
ENDIF

IF (LHOOK) CALL DR_HOOK('FAPRST_MT',1,ZHOOK_HANDLE)

END SUBROUTINE FAPRST_FORT

! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FAPRST64                                        &
&           (KREP, KNUMER, KNUMOD, KHCTPI, CDOPER, LDPRGR)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                  FA_COM_DEFAULT_INIT,  &
&                  NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  KNUMOD                                 ! IN   
INTEGER (KIND=JPLIKB)  KHCTPI                                 ! IN   
CHARACTER              CDOPER                                 ! IN   
LOGICAL                LDPRGR                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAPRST_FORT                                               &
&           (FA, KREP, KNUMER, KNUMOD, KHCTPI, CDOPER, LDPRGR)

END SUBROUTINE FAPRST64

SUBROUTINE FAPRST                                          &
&           (KREP, KNUMER, KNUMOD, KHCTPI, CDOPER, LDPRGR)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                  FA_COM_DEFAULT_INIT,  &
&                  NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKM)  KNUMOD                                 ! IN   
INTEGER (KIND=JPLIKM)  KHCTPI                                 ! IN   
CHARACTER              CDOPER                                 ! IN   
LOGICAL                LDPRGR                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAPRST_MT                                                 &
&           (FA, KREP, KNUMER, KNUMOD, KHCTPI, CDOPER, LDPRGR)

END SUBROUTINE FAPRST

SUBROUTINE FAPRST_MT                                           &
&           (FA, KREP, KNUMER, KNUMOD, KHCTPI, CDOPER, LDPRGR)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKM)  KNUMOD                                 ! IN   
INTEGER (KIND=JPLIKM)  KHCTPI                                 ! IN   
CHARACTER              CDOPER                                 ! IN   
LOGICAL                LDPRGR                                 ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  INUMOD                                 ! IN   
INTEGER (KIND=JPLIKB)  IHCTPI                                 ! IN   
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)
INUMOD     = INT (    KNUMOD, JPLIKB)
IHCTPI     = INT (    KHCTPI, JPLIKB)

CALL FAPRST_FORT                                               &
&           (FA, IREP, INUMER, INUMOD, IHCTPI, CDOPER, LDPRGR)

KREP       = INT (      IREP, JPLIKM)

END SUBROUTINE FAPRST_MT

!INTF KREP            OUT                                                              
!INTF KNUMER        IN                                                                 
!INTF KNUMOD        IN                                                                 
!INTF KHCTPI        IN                                                                 
!INTF CDOPER        IN                                                                 
!INTF LDPRGR        IN                                                                 

