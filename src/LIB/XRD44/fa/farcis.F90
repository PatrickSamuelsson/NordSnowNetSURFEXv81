! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FARCIS_FORT                                          &
&                     (FA,  KREP, KRANG, PCHAMP, KSTRON, KPUILA )
USE FA_MOD, ONLY : FA_COM, JPNIIL
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Sous-programme INTERNE du logiciel de Fichiers ARPEGE:
!     elimination de la "puissance de laplacien" d'un champ en coeffi-
!     cients spectraux issu d'un codage GRIB, de maniere a restituer
!     le champ "d'origine" (a la precision du codage pres) .
!     ( Reconstitution des CoeffIcients Spectraux )
!**
!    Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                KRANG  (Entree) ==> Rang de l'unite logique;
!    ( Tableau ) PCHAMP (Entree ET Sortie) ==> Champ en coef. spectraux;
!                KSTRON (Entree) ==> Sous-troncature non compactee;
!                KPUILA (Entree) ==> Puissance de laplacien utilisee.
!
!      ( Les 2 derniers parametres sont ceux qui ont ete effectivement
!        utilises lors de l'ecriture du champ )
!*
!       En mode multi-taches, il doit y avoir verrouillage du fichier
!     concerne avant l'appel au sous-programme.
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KRANG, KSTRON, KPUILA
!
REAL (KIND=JPDBLR) PCHAMP (FA%JPXCSP)
!
INTEGER (KIND=JPLIKB) IRANGC, ITRONC, INUMER, IDIMNC
INTEGER (KIND=JPLIKB) ILCHAM, IMTRONC, IPUISX, J
INTEGER (KIND=JPLIKB) INDICE, JN, INDLAP, IMLIM
INTEGER (KIND=JPLIKB) IOFF, IM, JIND, IPUIS2
INTEGER (KIND=JPLIKB) IRAPOR, IPUISR, INIMES, IDEB, IFIN
!
LOGICAL LLMLAM
!
CHARACTER(LEN=FA%JPXNOM) CLACTI 
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA

!**
!     1.  -  CONTROLES DES PARAMETRES D'APPEL, INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FARCIS_MT',0,ZHOOK_HANDLE)
CLACTI=''
IF (KRANG.LE.0.OR.KRANG.GT.FA%JPNXFA) THEN
  KREP=-66
  GOTO 1001
ENDIF
!
IF (FA%LIXLAP) THEN
  CALL FAIXLA_FORT           &
&                 (FA)
  FA%LIXLAP=.FALSE.
ENDIF
!
IRANGC=FA%FICHIER(KRANG)%NUCADR
ITRONC=FA%CADRE(IRANGC)%MTRONC
LLMLAM=FA%CADRE(IRANGC)%LIMLAM
!
IF (LLMLAM) IMTRONC=FA%CADRE(IRANGC)%NOZPAR(2)
IF (ITRONC.LE.KSTRON) THEN
  KREP=-88
  GOTO 1001
ELSEIF (LLMLAM.AND.IMTRONC.LE.KSTRON) THEN
  KREP=-88
  GOTO 1001
ELSEIF (LLMLAM.AND.(IMTRONC.GT.3*ITRONC &
&    .OR.ITRONC.GT.3*IMTRONC)) THEN
! Il s'agit d'un garde-fou, modifiable (ne pas oublier FAPULA et FACSIM)
  KREP=-114
  GOTO 1001
ELSE        
  KREP=0
ENDIF
!
IDIMNC=(1+KSTRON)**2
IF (LLMLAM) THEN
  ILCHAM=FA%CADRE(IRANGC)%NSFLAM
ELSE  
  ILCHAM=(1+ITRONC)**2
ENDIF  
!**
!     2.  -  RECONSTITUTION DU CHAMP "D'ORIGINE", DEBARRASSE DE LA
!            PUISSANCE DE LAPLACIEN QUI N'AFFECTE QUE LA PARTIE HORS
!            SOUS-TRONCATURE NON COMPACTEE.
!-----------------------------------------------------------------------
!
!        On essaie d'eviter l'exponentiation, en preferant multiplier
!     que diviser.
!
IF (KPUILA.NE.0) THEN
!
  IPUISX=ABS (KPUILA)
!
  IF (KPUILA.GT.0) THEN
    INDICE=1
  ELSE
    INDICE=0
  ENDIF
!
  IF (IPUISX.LE.FA%JPUILA) THEN
!
    IF (LLMLAM) THEN
!$OMP PARALLEL DO PRIVATE(JN,IMLIM,IDEB,IFIN,JIND,IOFF,IM,INDLAP) &
!$OMP&  IF(FA%LOPENMP)
      DO JN=1,ITRONC
      IMLIM=KSTRON-JN
      IDEB=MAX(FA%CADRE(IRANGC)%NOZPAR(2*JN+3)+4*(1+IMLIM), &
&             FA%CADRE(IRANGC)%NOZPAR(2*JN+3)+4)
      IFIN=FA%CADRE(IRANGC)%NOZPAR(2*JN+4)
      DO JIND=IDEB,IFIN
      IOFF=JIND-FA%CADRE(IRANGC)%NOZPAR(2*JN+3)
      IM=IOFF/4
      INDLAP=((JN-1)*FA%JPXTRO)+IM
      PCHAMP(JIND)=PCHAMP(JIND)*FA%XLAP2DA(INDLAP,IPUISX,INDICE)
      ENDDO
      ENDDO
!$OMP END PARALLEL DO
    ELSE
      DO J=IDIMNC+1,ILCHAM
      PCHAMP(J)=PCHAMP(J)*FA%XLAP2D(J,IPUISX,INDICE)
      ENDDO
    ENDIF
  ELSEIF (IPUISX.LE.2*FA%JPUILA) THEN
    IPUIS2=IPUISX/2
!
    IF (IPUISX.EQ.2*IPUIS2) THEN
!
      IF (LLMLAM) THEN
!$OMP PARALLEL DO PRIVATE(JN,IMLIM,IDEB,IFIN,JIND,IOFF,IM,INDLAP) &
!$OMP&  IF(FA%LOPENMP)
        DO JN=1,ITRONC
        IMLIM=KSTRON-JN
        IDEB=MAX(FA%CADRE(IRANGC)%NOZPAR(2*JN+3)+4*(1+IMLIM), &
&               FA%CADRE(IRANGC)%NOZPAR(2*JN+3)+4)
        IFIN=FA%CADRE(IRANGC)%NOZPAR(2*JN+4)
        DO JIND=IDEB,IFIN
        IOFF=JIND-FA%CADRE(IRANGC)%NOZPAR(2*JN+3)
        IM=IOFF/4
        INDLAP=((JN-1)*FA%JPXTRO)+IM
        PCHAMP(JIND)=PCHAMP(JIND)                  &
&          *( FA%XLAP2DA(INDLAP,IPUIS2,INDICE)**2 )
        ENDDO
        ENDDO
!$OMP END PARALLEL DO
      ELSE
        DO J=IDIMNC+1,ILCHAM
        PCHAMP(J)=PCHAMP(J)*( FA%XLAP2D(J,IPUIS2,INDICE)**2 )
        ENDDO
      ENDIF
!
    ELSE
!
      IF (LLMLAM) THEN
!$OMP PARALLEL DO PRIVATE(JN,IMLIM,IDEB,IFIN,JIND,IOFF,IM,INDLAP) &
!$OMP&  IF(FA%LOPENMP)
        DO JN=1,ITRONC
        IMLIM=KSTRON-JN
        IDEB=MAX(FA%CADRE(IRANGC)%NOZPAR(2*JN+3)+4*(1+IMLIM), &
&               FA%CADRE(IRANGC)%NOZPAR(2*JN+3)+4)
        IFIN=FA%CADRE(IRANGC)%NOZPAR(2*JN+4)
        DO JIND=IDEB,IFIN
        IOFF=JIND-FA%CADRE(IRANGC)%NOZPAR(2*JN+3)
        IM=IOFF/4
        INDLAP=((JN-1)*FA%JPXTRO)+IM
        PCHAMP(JIND)=PCHAMP(JIND)                         &
&          *( FA%XLAP2DA(INDLAP,FA%JPUILA,INDICE)          &
&            *FA%XLAP2DA(INDLAP,IPUISX-FA%JPUILA,INDICE) )
        ENDDO
        ENDDO
!$OMP END PARALLEL DO
      ELSE
        DO J=IDIMNC+1,ILCHAM
        PCHAMP(J)=PCHAMP(J)*( FA%XLAP2D(J,FA%JPUILA,INDICE)       &
&                          *FA%XLAP2D(J,IPUISX-FA%JPUILA,INDICE) )
        ENDDO
      ENDIF
!
    ENDIF
!
  ELSE
    IRAPOR=1+(IPUISX-1)/FA%JPUILA
    IPUISR=IPUISX/IRAPOR
!
    IF (IPUISX.EQ.IRAPOR*IPUISR) THEN
!
      IF (LLMLAM) THEN
!$OMP PARALLEL DO PRIVATE(JN,IMLIM,IDEB,IFIN,JIND,IOFF,IM,INDLAP) &
!$OMP&  IF(FA%LOPENMP)
        DO JN=1,ITRONC
        IMLIM=KSTRON-JN
        IDEB=MAX(FA%CADRE(IRANGC)%NOZPAR(2*JN+3)+4*(1+IMLIM), &
&               FA%CADRE(IRANGC)%NOZPAR(2*JN+3)+4)
        IFIN=FA%CADRE(IRANGC)%NOZPAR(2*JN+4)
        DO JIND=IDEB,IFIN
        IOFF=JIND-FA%CADRE(IRANGC)%NOZPAR(2*JN+3)
        IM=IOFF/4
        INDLAP=((JN-1)*FA%JPXTRO)+IM
        PCHAMP(JIND)=PCHAMP(JIND)                        &
&           *( FA%XLAP2DA(INDLAP,IPUISR,INDICE)**IRAPOR )
        ENDDO
        ENDDO
!$OMP END PARALLEL DO
      ELSE
        DO J=IDIMNC+1,ILCHAM
        PCHAMP(J)=PCHAMP(J)*( FA%XLAP2D(J,IPUISR,INDICE)**IRAPOR )
        ENDDO
      ENDIF
!
    ELSE
!
      IF (LLMLAM) THEN
!$OMP PARALLEL DO PRIVATE(JN,IMLIM,IDEB,IFIN,JIND,IOFF,IM,INDLAP) &
!$OMP&  IF(FA%LOPENMP)
        DO JN=1,ITRONC
        IMLIM=KSTRON-JN
        IDEB=MAX(FA%CADRE(IRANGC)%NOZPAR(2*JN+3)+4*(1+IMLIM), &
&               FA%CADRE(IRANGC)%NOZPAR(2*JN+3)+4)
        IFIN=FA%CADRE(IRANGC)%NOZPAR(2*JN+4)
        DO JIND=IDEB,IFIN
        IOFF=JIND-FA%CADRE(IRANGC)%NOZPAR(2*JN+3)
        IM=IOFF/4
        INDLAP=((JN-1)*FA%JPXTRO)+IM
        PCHAMP(JIND)=PCHAMP(JIND)                                 &
&          *( FA%XLAP2DA(INDLAP,FA%JPUILA,INDICE)**(IRAPOR-1)      &
&         *FA%XLAP2DA(INDLAP,IPUISX-FA%JPUILA*(IRAPOR-1),INDICE) )
        ENDDO
        ENDDO
!$OMP END PARALLEL DO
      ELSE
        DO J=IDIMNC+1,ILCHAM
        PCHAMP(J)=PCHAMP(J)*                                       &
&                 (FA%XLAP2D(J,FA%JPUILA,INDICE)**(IRAPOR-1)        &
&                *FA%XLAP2D(J,IPUISX-FA%JPUILA*(IRAPOR-1),INDICE) )
        ENDDO
      ENDIF
!
    ENDIF
!
  ENDIF
!
ENDIF
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE EVENTUELLE,
!            VIA LE SOUS-PROGRAMME "FAIPAR" .
!-----------------------------------------------------------------------
!
1001 CONTINUE
LLFATA=LLMOER (KREP,KRANG)
!
IF (FA%LFAMOP.OR.LLFATA) THEN
  INIMES=2
  CLNSPR='FARCIS'
  INUMER=JPNIIL
  WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KRANG='',I4,        &
&    '', PCHAMP(1)='',G12.5,'', KSTRON='',I4,'', KPUILA='',I3)') &
&     KREP,KRANG,PCHAMP(1),KSTRON,KPUILA
  CALL FAIPAR_FORT                                      &
&                 (FA, INUMER,INIMES,KREP,.FALSE.,CLMESS, &
&               CLNSPR,CLACTI,.FALSE.)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('FARCIS_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"

END SUBROUTINE FARCIS_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FARCIS64                             &
&           (KREP, KRANG, PCHAMP, KSTRON, KPUILA)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KRANG                                  ! IN   
REAL (KIND=JPDBLR)     PCHAMP     (*)                 ! INOUT
INTEGER (KIND=JPLIKB)  KSTRON                                 ! IN   
INTEGER (KIND=JPLIKB)  KPUILA                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FARCIS_FORT                                    &
&           (FA, KREP, KRANG, PCHAMP, KSTRON, KPUILA)

END SUBROUTINE FARCIS64

SUBROUTINE FARCIS                               &
&           (KREP, KRANG, PCHAMP, KSTRON, KPUILA)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KRANG                                  ! IN   
REAL (KIND=JPDBLR)     PCHAMP     (*)                 ! INOUT
INTEGER (KIND=JPLIKM)  KSTRON                                 ! IN   
INTEGER (KIND=JPLIKM)  KPUILA                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FARCIS_MT                                      &
&           (FA, KREP, KRANG, PCHAMP, KSTRON, KPUILA)

END SUBROUTINE FARCIS

SUBROUTINE FARCIS_MT                                &
&           (FA, KREP, KRANG, PCHAMP, KSTRON, KPUILA)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KRANG                                  ! IN   
REAL (KIND=JPDBLR)     PCHAMP     (FA%JPXCSP)                 ! INOUT
INTEGER (KIND=JPLIKM)  KSTRON                                 ! IN   
INTEGER (KIND=JPLIKM)  KPUILA                                 ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  IRANG                                  ! IN   
INTEGER (KIND=JPLIKB)  ISTRON                                 ! IN   
INTEGER (KIND=JPLIKB)  IPUILA                                 ! IN   
! Convert arguments

IRANG      = INT (     KRANG, JPLIKB)
ISTRON     = INT (    KSTRON, JPLIKB)
IPUILA     = INT (    KPUILA, JPLIKB)

CALL FARCIS_FORT                                    &
&           (FA, IREP, IRANG, PCHAMP, ISTRON, IPUILA)

KREP       = INT (      IREP, JPLIKM)

END SUBROUTINE FARCIS_MT

!INTF KREP            OUT                               
!INTF KRANG         IN                                  
!INTF PCHAMP        INOUT DIMS=FA%JPXCSP                
!INTF KSTRON        IN                                  
!INTF KPUILA        IN                                  
