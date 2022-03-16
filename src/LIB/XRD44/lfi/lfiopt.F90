! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI

SUBROUTINE LFIOPT_FORT                                     &
&                     (LFI, KREP, KNUMER, LDNOMM, CDNOMF,  &
&                      CDSTTO, LDERFA,                     &
&                      LDIMST, KNIMES)
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        SOUS-PROGRAMME DE RECUPERATION DES OPTIONS D'OUVERTURE
!     D'UN FICHIER INDEXE, PAR LE LOGICIEL LFI.
!**
!     ARGUMENTS : KREP   (SORTIE) ==> CODE-REPONSE DU SOUS-PROGRAMME;
!                 KNUMER (ENTREE) ==> LFI%NUMERO DE L'UNITE LOGIQUE;
!                 LDNOMM (SORTIE) ==> VRAI SI L'UNITE LOGIQUE DOIT ETRE
!                                     ASSOCIEE A UN NOM DE FICHIER EXP-
!                                     LICITE LORS DE L'"OPEN" FORTRAN;
!                 CDNOMF (SORTIE) ==> NOM DE FICHIER EXPLICITE, SI
!                                     *LDNOMM* EST VRAI - MEME SI CE
!                                     N'EST PAS LE CAS, CE *DOIT* ETRE
!                                     UN OBJET DE TYPE "CHARACTER" .
!                 CDSTTO (SORTIE) ==> "STATUS" POUR L'"OPEN" FORTRAN
!                                     ('OLD','NEW','UNKNOWN','SCRATCH')
!                                     PAR DEFAUT, METTRE 'UNKNOWN';
!                 LDERFA (SORTIE) ==> OPTION D'ERREUR FATALE;
!                 LDIMST (SORTIE) ==> OPTION IMPRESSION DE STATISTIQUES
!                                     AU MOMENT DE LA FERMETURE;
!                 KNIMES (SORTIE) ==> NIVEAU DE LA MESSAGERIE (0,1 OU 2)
!                                     ( 0==>RIEN, 2==>TOUT )
CHARACTER CPNOMD*(*)
PARAMETER ( CPNOMD='%%%%% FICHIER SANS NOM %%%%%' )
TYPE(LFICOM) :: LFI
INTEGER (KIND=JPLIKB) KREP, KNUMER, KNIMES
INTEGER (KIND=JPLIKB) IRANG, IREP
!
LOGICAL LDNOMM, LDERFA, LDIMST
!
CHARACTER CDNOMF*(*), CDSTTO*(*)

REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('LFIOPT_FORT',0,ZHOOK_HANDLE)

IREP=0
IRANG=0
CALL LFINUM_FORT                    &
&               (LFI, KNUMER,IRANG)

IF (IRANG .EQ. 0) THEN
  IREP=-1
  GOTO 1001
ENDIF

LDNOMM=CPNOMD.NE.LFI%CNOMFI(IRANG)
CDNOMF=LFI%CNOMFI(IRANG)
CDSTTO=LFI%CSTAOP(IRANG)
LDERFA=LFI%LERFAT(IRANG)
LDIMST=LFI%LISTAT(IRANG)
KNIMES=LFI%NIVMES(IRANG)

1001 CONTINUE

KREP=IREP

IF (LHOOK) CALL DR_HOOK('LFIOPT_FORT',1,ZHOOK_HANDLE)
END SUBROUTINE LFIOPT_FORT




! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFIOPT64                                      &
&           (KREP, KNUMER, LDNOMM, CDNOMF, CDSTTO, LDERFA, &
&           LDIMST, KNIMES)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
LOGICAL                LDNOMM                                 !   OUT
CHARACTER (LEN=*)      CDNOMF                                 !   OUT
CHARACTER (LEN=*)      CDSTTO                                 !   OUT
LOGICAL                LDERFA                                 !   OUT
LOGICAL                LDIMST                                 !   OUT
INTEGER (KIND=JPLIKB)  KNIMES                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIOPT_FORT                                               &
&           (LFI, KREP, KNUMER, LDNOMM, CDNOMF, CDSTTO, LDERFA, &
&           LDIMST, KNIMES)

END SUBROUTINE LFIOPT64

SUBROUTINE LFIOPT                                        &
&           (KREP, KNUMER, LDNOMM, CDNOMF, CDSTTO, LDERFA, &
&           LDIMST, KNIMES)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
LOGICAL                LDNOMM                                 !   OUT
CHARACTER (LEN=*)      CDNOMF                                 !   OUT
CHARACTER (LEN=*)      CDSTTO                                 !   OUT
LOGICAL                LDERFA                                 !   OUT
LOGICAL                LDIMST                                 !   OUT
INTEGER (KIND=JPLIKM)  KNIMES                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIOPT_MT                                                &
&           (LFI, KREP, KNUMER, LDNOMM, CDNOMF, CDSTTO, LDERFA, &
&           LDIMST, KNIMES)

END SUBROUTINE LFIOPT

SUBROUTINE LFIOPT_MT                                          &
&           (LFI, KREP, KNUMER, LDNOMM, CDNOMF, CDSTTO, LDERFA, &
&           LDIMST, KNIMES)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
LOGICAL                LDNOMM                                 !   OUT
CHARACTER (LEN=*)      CDNOMF                                 !   OUT
CHARACTER (LEN=*)      CDSTTO                                 !   OUT
LOGICAL                LDERFA                                 !   OUT
LOGICAL                LDIMST                                 !   OUT
INTEGER (KIND=JPLIKM)  KNIMES                                 !   OUT
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  INIMES                                 !   OUT
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)

CALL LFIOPT_FORT                                               &
&           (LFI, IREP, INUMER, LDNOMM, CDNOMF, CDSTTO, LDERFA, &
&           LDIMST, INIMES)

KREP       = INT (      IREP, JPLIKM)
KNIMES     = INT (    INIMES, JPLIKM)

END SUBROUTINE LFIOPT_MT

!INTF KREP            OUT 
!INTF KNUMER        IN    
!INTF LDNOMM          OUT 
!INTF CDNOMF          OUT 
!INTF CDSTTO          OUT 
!INTF LDERFA          OUT 
!INTF LDIMST          OUT 
!INTF KNIMES          OUT 
