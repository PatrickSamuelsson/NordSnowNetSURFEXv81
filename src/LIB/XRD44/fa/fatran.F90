! Nov-2012 P. Marguinaud Use local INDIRECT array
! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FATRAN_FORT                                            &
&                     (FA,  KREP,  KNUMER,  PCHAME, PCHAMS, LDOPT )
USE FA_MOD, ONLY : FA_COM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!      Sous-programme du logiciel de Fichiers ARPEGE permettant la
!      TRANsposition d'un champ spectral ARPEGE ou ALADIN,
!      d'un rangement des coeff selon le MODELE vers un rangement
!      des coeff selon FA+GRIB_version0 et inversement.
!
!**
!    Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                KNUMER (Entree) ==> Rang de l'unite logique;
!    ( Tableau ) PCHAME (Entree) ==> Valeurs du champ a transposer;
!    ( Tableau ) PCHAMS (Sortie) ==> Valeurs du champ transpose;
!                LDOPT   (Entree) ==> Option de transposition;
!                                    si .TRUE.  alors PCHAME range comme MODELE
!                                                     (soit "verticalement")
!                                                     PCHAMS range comme FA-GRIB0
!                                                     (soit "horizontalement")
!                                    si .FALSE. alors PCHAME range comme FA-GRIB0
!                                                     PCHAMS range comme MODELE
!*
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KNUMER
!
LOGICAL LDOPT
!
REAL (KIND=JPDBLR) PCHAME(*), PCHAMS(*)
!
INTEGER (KIND=JPLIKB) JN, JM, J, INDEX, ILOW, IHIGH
INTEGER (KIND=JPLIKB) IRANGC, IRANG
INTEGER (KIND=JPLIKB) INIMES, ITRONC, IMSMAX
INTEGER (KIND=JPLIKB), ALLOCATABLE :: IND(:,:)
INTEGER (KIND=JPLIKB), ALLOCATABLE :: INDIRECT(:)
!
LOGICAL LLMLAM
!
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA

!
!
!
!**
!     1.  -  CONTROLES ET INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FATRAN_MT',0,ZHOOK_HANDLE)
KREP=0
CALL FANUMU_FORT                 &
&               (FA, KNUMER,IRANG)
!
IF (IRANG.EQ.0) THEN
  KREP=-51
  GOTO 1001
ENDIF
IRANGC=FA%FICHIER(IRANG)%NUCADR
LLMLAM=FA%CADRE(IRANGC)%LIMLAM
ITRONC=FA%CADRE(IRANGC)%MTRONC
IF (LLMLAM) THEN
  IMSMAX = FA%CADRE(IRANGC)%NOMPAR(2)
ENDIF

!
! Initialisation de l'indirection pour IRANGC, si ce n'est pas deja fait.
! Si ARPEGE, INDIRECT(J,IRANGC)=INDEX signifie que les indices J dans
!   le tableau "FA+GRIB0" et INDEX dans le tableau "modele ARPEGE"
!    designent un meme coeff spectral.
! Si Aladin, INDIRECT(JM*(ITRONC+1)+JN+1,IRANGC)=J signifie que l'indice
!   J dans le tableau "FA+GRIB0" est le premier coeff associe au couple (JM,JN)
!   ou JM est le nombre d'onde zonal et JN le nombre d'onde meridien.
!   4 coeff spectraux sont associes a chaque couple (JM,JN) car JM varie
!   de 0 a IMSMAX et JN varie de 0 a ITRONC (soit 1/4 de l'ellipse).
!
!  CAS ARPEGE
!
IF (.NOT.LLMLAM) THEN
  ALLOCATE (IND(0:ITRONC,-ITRONC:ITRONC))
  ALLOCATE (INDIRECT ((ITRONC+1)**2))
  DO JN=0,ITRONC
    ILOW=JN**2+1
    IHIGH=(JN+1)**2
    JM=-JN-1
    DO J=ILOW,IHIGH
      JM=JM+1
      IND(JN,JM)=J
    ENDDO
  ENDDO
!
  INDEX=-1
  DO JM=0,ITRONC
  DO JN=JM,ITRONC
    INDEX=INDEX+2
    INDIRECT  (IND(JN, JM))=INDEX
    IF (JM.NE.0) THEN
      INDIRECT(IND(JN,-JM))=INDEX+1
    ENDIF
  ENDDO
  ENDDO
  DEALLOCATE (IND)
ENDIF
!
!  CAS ALADIN
!
IF (LLMLAM) THEN
  ALLOCATE (INDIRECT ((ITRONC+1)**2))
  DO JN=0,ITRONC
  DO J=FA%CADRE(IRANGC)%NOZPAR(2*JN+3), FA%CADRE(IRANGC)%NOZPAR(2*JN+4), 4
    JM=(J-FA%CADRE(IRANGC)%NOZPAR(2*JN+3)) / 4
    INDIRECT(JM*(ITRONC+1)+JN+1) = J
  ENDDO
  ENDDO
ENDIF
!**
!     2.  -  TRANSPOSITION DES DONNEES
!-----------------------------------------------------------------------
!
!  CAS ALADIN
!
IF (LLMLAM) THEN
    IF (LDOPT) THEN
! PCHAME range comme MODELE (soit "verticalement")
!
    DO JM=0,IMSMAX
    DO INDEX=FA%CADRE(IRANGC)%NOMPAR(2*JM+3), FA%CADRE(IRANGC)%NOMPAR(2*JM+4), 4
      JN = (INDEX-FA%CADRE(IRANGC)%NOMPAR(2*JM+3)) / 4
      J  = INDIRECT(JM*(ITRONC+1)+JN+1)
      PCHAMS(J  )=PCHAME(INDEX  )  
      PCHAMS(J+1)=PCHAME(INDEX+1)
      PCHAMS(J+2)=PCHAME(INDEX+2)
      PCHAMS(J+3)=PCHAME(INDEX+3)
    ENDDO
    ENDDO
  ELSE
! PCHAME range comme FA+GRIB0 (soit "horizontalement")
!
    DO JM=0,IMSMAX
    DO INDEX=FA%CADRE(IRANGC)%NOMPAR(2*JM+3), FA%CADRE(IRANGC)%NOMPAR(2*JM+4), 4
      JN = (INDEX-FA%CADRE(IRANGC)%NOMPAR(2*JM+3)) / 4
      J  = INDIRECT(JM*(ITRONC+1)+JN+1)
      PCHAMS(INDEX  )=PCHAME(J  )  
      PCHAMS(INDEX+1)=PCHAME(J+1)
      PCHAMS(INDEX+2)=PCHAME(J+2)
      PCHAMS(INDEX+3)=PCHAME(J+3)
    ENDDO
    ENDDO
  ENDIF
ELSE
!
!  CAS ARPEGE
!
!  1/ Passage du rangement des coeff. spectraux du type modele ARPEGE
!  a celui de FA associe a GRIB version0 (et pas associe a GRIBEX qui
!  reprend la structure de tableau de ARPEGE).
!
  IF (LDOPT) THEN
    DO JN=0,ITRONC
      ILOW=JN**2+1
      IHIGH=(JN+1)**2
      DO J=ILOW,IHIGH
        PCHAMS(J)=PCHAME(INDIRECT(J))
      ENDDO
    ENDDO
!
!  2/ Passage du rangement des coeff. spectraux du type FA associe
!  a GRIB version0 (et pas associe a GRIBEX qui reprend la structure de
!  tableau de ARPEGE) a celui du type modele ARPEGE.
!
  ELSE
!  Initialisation de la partie "JM=0" a zero, pour y introduire
!  ensuite uniquement les coeff reels correspondant dans PCHAME
!  (les coeff imaginaires etant donc crees et mis a zero).
    PCHAMS(1:2*(ITRONC+1))=0._JPDBLR
!
    DO JN=0,ITRONC
      ILOW=JN**2+1
      IHIGH=(JN+1)**2
      DO J=ILOW,IHIGH
        PCHAMS(INDIRECT(J))=PCHAME(J)
      ENDDO
    ENDDO
  ENDIF
ENDIF
!
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE, AVEC "ABORT" EVENTUEL,
!            VIA LE SOUS-PROGRAMME "FAIPAR" .
!-----------------------------------------------------------------------
!
1001 CONTINUE

IF (ALLOCATED (INDIRECT)) DEALLOCATE (INDIRECT)

LLFATA=LLMOER (KREP,IRANG)
!
IF (LLFATA) THEN
  INIMES=2
ELSE
  INIMES=IXNVMS(IRANG)
ENDIF
!
IF (INIMES.EQ.0)  THEN 
  IF (LHOOK) CALL DR_HOOK('FATRAN_MT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='FATRAN'
!
WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', IRANG='',I4, &
&         '', LDOPT='',L2)')  KREP, IRANG, LDOPT
CALL FAIPAR_FORT                                     &
&               (FA, KNUMER,INIMES,KREP,LLFATA,CLMESS, &
&               CLNSPR,CLNSPR,.FALSE.)
!
IF (LHOOK) CALL DR_HOOK('FATRAN_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"
#include "facom2.ixnvms.h"

END SUBROUTINE FATRAN_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FATRAN64                             &
&           (KREP, KNUMER, PCHAME, PCHAMS, LDOPT)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
REAL (KIND=JPDBLR)     PCHAME     (*)                         ! IN   
REAL (KIND=JPDBLR)     PCHAMS     (*)                         !   OUT
LOGICAL                LDOPT                                  ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FATRAN_FORT                                    &
&           (FA, KREP, KNUMER, PCHAME, PCHAMS, LDOPT)

END SUBROUTINE FATRAN64

SUBROUTINE FATRAN                               &
&           (KREP, KNUMER, PCHAME, PCHAMS, LDOPT)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
REAL (KIND=JPDBLR)     PCHAME     (*)                         ! IN   
REAL (KIND=JPDBLR)     PCHAMS     (*)                         !   OUT
LOGICAL                LDOPT                                  ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FATRAN_MT                                      &
&           (FA, KREP, KNUMER, PCHAME, PCHAMS, LDOPT)

END SUBROUTINE FATRAN

SUBROUTINE FATRAN_MT                                &
&           (FA, KREP, KNUMER, PCHAME, PCHAMS, LDOPT)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
REAL (KIND=JPDBLR)     PCHAME     (*)                         ! IN   
REAL (KIND=JPDBLR)     PCHAMS     (*)                         !   OUT
LOGICAL                LDOPT                                  ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)

CALL FATRAN_FORT                                    &
&           (FA, IREP, INUMER, PCHAME, PCHAMS, LDOPT)

KREP       = INT (      IREP, JPLIKM)

END SUBROUTINE FATRAN_MT

!INTF KREP            OUT                               
!INTF KNUMER        IN                                  
!INTF PCHAME        IN    DIMS=*                        
!INTF PCHAMS          OUT DIMS=*                        
!INTF LDOPT         IN                                  
