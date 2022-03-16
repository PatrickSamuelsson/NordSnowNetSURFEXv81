! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI

SUBROUTINE LFIINTECR_FORT                          &
&           (LFI, KREP, KNUMER, CDNOMA, KTAB, KLONG)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
!
!****
!        SOUS-PROGRAMME D'ECRITURE  D'UN TABLEAU D'ENTIERS A PARTIR 
!     D'UN ARTICLE (DE DONNEES) PAR *NOM*
!     SUR UNE UNITE LOGIQUE OUVERTE POUR LE LOGICIEL DE FICHIERS INDEXES
!     *LFI*; L'ARTICLE EN SORTIE EST UN "BLOC" DE DONNEES ADJACENTES.
!**
!    ARGUMENTS : KREP   (SORTIE) ==> CODE-REPONSE DU SOUS-PROGRAMME;
!                KNUMER (ENTREE) ==> LFI%NUMERO DE L'UNITE LOGIQUE;
!                CDNOMA (ENTREE) ==> NOM DE L'ARTICLE A RECHERCHER;
!                KTAB   (ENTREE) ==> PREMIER MOT A ECRIRE
!                KLONG  (ENTREE) ==> LONGUEUR DE L'ARTICLE A LIRE.
!
!
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 ! IN   
INTEGER (KIND=JPLIKB)  KLONG                                  ! IN   
INTEGER (KIND=JPLIKM)  KTAB       (KLONG)                     ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  ITAB       (KLONG)                     ! IN   
! Convert arguments

ITAB       = INT (      KTAB, JPLIKB)

CALL LFIECR_FORT                                 &
&           (LFI, KREP, KNUMER, CDNOMA, ITAB, KLONG)

END SUBROUTINE LFIINTECR_FORT

! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFIINTECR64                        &
&           (KREP, KNUMER, CDNOMA, KTAB, KLONG)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT,  &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 ! IN   
INTEGER (KIND=JPLIKB)  KLONG                                  ! IN   
INTEGER (KIND=JPLIKM)  KTAB       (KLONG)                     ! IN   

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIINTECR_FORT                                 &
&           (LFI, KREP, KNUMER, CDNOMA, KTAB, KLONG)

END SUBROUTINE LFIINTECR64

SUBROUTINE LFIINTECR                          &
&           (KREP, KNUMER, CDNOMA, KTAB, KLONG)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT,  &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 ! IN   
INTEGER (KIND=JPLIKM)  KLONG                                  ! IN   
INTEGER (KIND=JPLIKM)  KTAB       (KLONG)                     ! IN   

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIINTECR_MT                                  &
&           (LFI, KREP, KNUMER, CDNOMA, KTAB, KLONG)

END SUBROUTINE LFIINTECR

SUBROUTINE LFIINTECR_MT                            &
&           (LFI, KREP, KNUMER, CDNOMA, KTAB, KLONG)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 ! IN   
INTEGER (KIND=JPLIKM)  KLONG                                  ! IN   
INTEGER (KIND=JPLIKM)  KTAB       (KLONG)                     ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  ILONG                                  ! IN   
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)
ILONG      = INT (     KLONG, JPLIKB)

CALL LFIECR_FORT                                 &
&           (LFI, IREP, INUMER, CDNOMA, KTAB, ILONG)

KREP       = INT (      IREP, JPLIKM)

END SUBROUTINE LFIINTECR_MT

!INTF KREP            OUT                               
!INTF KNUMER        IN                                  
!INTF CDNOMA        IN                                  
!INTF KTAB          IN    DIMS=KLONG                    
!INTF KLONG         IN                                  
