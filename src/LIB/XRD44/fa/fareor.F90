! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FAREOR_FORT                                             &
&                     (FA,  KREP, KNUMER, PCHAMM, PCHAMF, LDFTOM)
USE FA_MOD, ONLY : FA_COM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!      Sous-programme de REORDONNANCEMENT des coefficients d'un champ horizontal spectral
!      destine a etre ecrit sur un fichier ARPEGE/ALADIN, ou bien qui vient
!      d'etre lu.
!
!**
!    Arguments : KREP   (Sortie)        ==> Code-reponse du sous-programme;
!                KNUMER (Entree)        ==> Numero de l'unite logique;
!    ( Tableau ) PCHAMM (Entree/Sortie) ==> Valeurs du champ, rangement modele
!    ( Tableau ) PCHAMF (Entree/Sortie) ==> Valeurs du champ, rangement fichier
!                LDFTOM (Entree)        ==> Fichier vers modele (T), modele vers
!                                           fichier (F)
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KNUMER
!
REAL (KIND=JPDBLR) PCHAMM (*), PCHAMF (*)
!
LOGICAL LDFTOM
!
INTEGER (KIND=JPLIKB) IRANG, IRANGC, INIMES
INTEGER (KIND=JPLIKB) ISMAX
INTEGER (KIND=JPLIKB), POINTER :: IISMAX (:), IDIM0GG (:)
CHARACTER(LEN=FA%JPLSPX) CLNSPR
CHARACTER(LEN=FA%JPLMES) CLMESS 
LOGICAL LLMLAM, LLFATA, LLRLFI

REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FAREOR_MT',0,ZHOOK_HANDLE)

KREP = 0
LLRLFI = .FALSE.

CALL FANUMU_FORT (FA, KNUMER,IRANG)

IF (IRANG .EQ. 0) THEN
  KREP = -51
  GOTO 1001
ENDIF

IRANGC=FA%FICHIER(IRANG)%NUCADR
LLMLAM=FA%CADRE(IRANGC)%LIMLAM

ISMAX   =  FA%CADRE(IRANGC)%NSMAX     
IISMAX  => FA%CADRE(IRANGC)%NISMAX     
IDIM0GG => FA%CADRE(IRANGC)%NDIM0GG

IF (LLMLAM) THEN
  CALL FAREOR_LAM
ELSE
  CALL FAREOR_GLO
ENDIF

1001 CONTINUE

LLFATA=LLMOER (KREP,IRANG)

IF (LLFATA) THEN
  INIMES=2
ELSE
  INIMES=IXNVMS(IRANG)
ENDIF

IF (.NOT.LLFATA.AND.INIMES.NE.2)  THEN 
  IF (LHOOK) CALL DR_HOOK('FAREOR_MT',1,ZHOOK_HANDLE)
  RETURN
ENDIF

CLNSPR='FAREOR'

WRITE (UNIT=CLMESS,FMT='(''KREP='',I5,'', KNUMER='',I3)') &
&   KREP,KNUMER

CALL FAIPAR_FORT                                       &
&               (FA, KNUMER,INIMES,KREP,LLFATA,CLMESS, &
&                CLNSPR, '',LLRLFI)

IF (LHOOK) CALL DR_HOOK('FAREOR_MT',1,ZHOOK_HANDLE)

CONTAINS

SUBROUTINE FAREOR_LAM

INTEGER (KIND=JPLIKB) :: II, ISP, JM, JN

II=0
IF (LDFTOM) THEN
  DO JN=0,ISMAX
    DO JM=0,IISMAX(JN)
      ISP=IDIM0GG(JM)+4*JN
      II=II+4
      PCHAMM(ISP:ISP+3)=PCHAMF(II-3:II)
    ENDDO
  ENDDO
ELSE
  DO JN=0,ISMAX  
    DO JM=0,IISMAX(JN)
      ISP=IDIM0GG(JM)+4*JN
      II=II+4
      PCHAMF(II-3:II)=PCHAMM(ISP:ISP+3)
    ENDDO
  ENDDO
ENDIF

END SUBROUTINE FAREOR_LAM

SUBROUTINE FAREOR_GLO

INTEGER(KIND=JPLIKB) :: II, IM, ISP, JM, JN

IF (LDFTOM) THEN
  II=0
  DO JN=0,ISMAX
    DO JM=-JN,-1
      ISP=IDIM0GG(-JM)+(JN+JM)*2 +1
      II = II + 1
      PCHAMM(ISP)=PCHAMF(II)
    ENDDO
    ISP=IDIM0GG(0)+JN*2
    II = II + 1
    PCHAMM(ISP)=PCHAMF(II)
    PCHAMM(ISP+1)=0.0_JPRB
    DO JM=1,JN
      ISP=IDIM0GG(JM)+(JN-JM)*2
      II = II + 1
      PCHAMM(ISP)=PCHAMF(II)
    ENDDO
  ENDDO
ELSE
  II=0
  DO JN=0,ISMAX
    DO JM=-JN,JN
      IM=ABS(JM)
      IF (JM < 0) THEN
        ISP=IDIM0GG(IM)+(JN-IM)*2 +1
      ELSE
        ISP=IDIM0GG(IM)+(JN-IM)*2
      ENDIF
      II = II + 1
      PCHAMF(II)=PCHAMM(ISP)
    ENDDO
  ENDDO
ENDIF

END SUBROUTINE FAREOR_GLO


#include "facom2.llmoer.h"
#include "facom2.ixnvms.h"

END SUBROUTINE FAREOR_FORT

! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FAREOR64                                      &
&                     (KREP, KNUMER, PCHAMM, PCHAMF, LDFTOM)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                  FA_COM_DEFAULT_INIT,  &
&                  NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
REAL (KIND=JPDBLR)     PCHAMM     (*)                         ! INOUT
REAL (KIND=JPDBLR)     PCHAMF     (*)                         ! INOUT
LOGICAL                LDFTOM                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAREOR_FORT                                             &
&                     (FA,  KREP, KNUMER, PCHAMM, PCHAMF, LDFTOM)

END SUBROUTINE FAREOR64

SUBROUTINE FAREOR                                        &
&                     (KREP, KNUMER, PCHAMM, PCHAMF, LDFTOM)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                  FA_COM_DEFAULT_INIT,  &
&                  NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
INTEGER (KIND=JPDBLR)  PCHAMM     (*)                         ! INOUT
INTEGER (KIND=JPDBLR)  PCHAMF     (*)                         ! INOUT
LOGICAL                LDFTOM                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAREOR_MT                                                 &
&                     (FA,  KREP, KNUMER, PCHAMM, PCHAMF, LDFTOM)

END SUBROUTINE FAREOR

SUBROUTINE FAREOR_MT                                         &
&                     (FA, KREP, KNUMER, PCHAMM, PCHAMF, LDFTOM)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE(FA_COM)           FA
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
INTEGER (KIND=JPDBLR)  PCHAMM     (*)                         ! INOUT
INTEGER (KIND=JPDBLR)  PCHAMF     (*)                         ! INOUT
LOGICAL                LDFTOM                                 ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)

CALL FAREOR_FORT                                             &
&                     (FA, IREP, INUMER, PCHAMM, PCHAMF, LDFTOM)

KREP       = INT (      IREP, JPLIKM)

END SUBROUTINE FAREOR_MT

!INTF KREP            OUT                                                              
!INTF KNUMER        IN                                                                 
!INTF PCHAMM        INOUT DIMS=*                         KIND=JPDBLR                   
!INTF PCHAMF        INOUT DIMS=*                         KIND=JPDBLR                   
!INTF LDCOSP        IN                                                                 

