! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FAIOPT_FORT                                             &
&                     (FA,  KREP, KNUMER, LDNOMM, CDNOMF, CDSTTU,  &
&                      LDERFA, LDIMST, KNIMES, CDNOMC)
USE FA_MOD, ONLY : FA_COM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Sous-programme de recuperation des options d'OUVERTURE d'une unite 
!     logique "Fichier ARPEGE".
!     Il s'agit d'un fichier indexe, traite par le logiciel LFI.
!
!**
!     ARGUMENTS : Ce sont les memes que pour "FAITOU", sauf KNBARP
!                 et KNBARI qui ont ete retires
!
!                 KREP   (Sortie) ==> Code-reponse du sous-programme;
!                 KNUMER (Entree) ==> Numero de l'unite logique;
!                 LDNOMM (Sortie) ==> Vrai si l'unite logique doit etre
!                                     associee a un NOM de Fichier EXP-
!                                     LICITE lors de l'"OPEN" FORTRAN;
!                 CDNOMF (Sortie) ==> Nom de fichier explicite, si
!                                     *LDNOMM* est VRAI - Meme si ce
!                                     n'est pas le cas, ce *DOIT* ETRE
!                                     UN OBJET DE TYPE "CHARACTER" .
!                 CDSTTU (Sortie) ==> "STATUS" pour l'"OPEN" FORTRAN
!                                     ('OLD','NEW','UNKNOWN','SCRATCH')
!                                     par defaut, mettre 'UNKNOWN';
!                 LDERFA (Sortie) ==> Option d'erreur fatale;
!                 LDIMST (Sortie) ==> Option impression de Statistiques
!                                     au moment de la fermeture;
!                 KNIMES (Sortie) ==> Niveau de la Messagerie (0,1 ou 2)
!                                     ( 0==>Rien, 2==>Tout )
!                 CDNOMC (Sortie) ==> Nom du CADRE associe au fichier.
!*
!     N.B. :  Pour un fichier en mode creation, ce cadre doit avoir ete
!          defini au prealable (via le sous-programme FACADE, ou par
!          l'ouverture d'un fichier preexistant).
!             Pour un fichier ARPEGE preexistant, le cadre est lu sur le
!          fichier; s'il etait deja defini auparavant, il y a controle
!          de coherence entre les deux versions du cadre.
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KNUMER, KNIMES
!
CHARACTER CPNOMD*(*)
PARAMETER ( CPNOMD='%%%%% FICHIER SANS NOM %%%%%' )
!
INTEGER (KIND=JPLIKB) IRANG, IRANGC
LOGICAL LDNOMM, LDERFA, LDIMST
INTEGER (KIND=JPLIKB) IREP
!
CHARACTER CDNOMF*(*), CDSTTU*(*), CDNOMC*(*)
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('FAIOPT_MT',0,ZHOOK_HANDLE)

IREP=0
IRANG=0
CALL FANUMU_FORT                  &
&               (FA, KNUMER, IRANG)

IF (IRANG .EQ. 0) THEN
  IREP=-1
  GOTO 1001
ENDIF

IRANGC=FA%FICHIER(IRANG)%NUCADR
CDNOMC=FA%CADRE(IRANGC)%CNOMCA
LDNOMM=FA%FICHIER(IRANG)%LNOMME
KNIMES=FA%FICHIER(IRANG)%NIVOMS
LDERFA=FA%FICHIER(IRANG)%LERRFA
CDNOMF=CPNOMD
CDSTTU=''
LDIMST=.FALSE.

IF (LDNOMM) THEN
  CALL LFIOPT_FORT                                        &
&                 (FA%LFI, IREP, KNUMER, LDNOMM, CDNOMF,  &
&                  CDSTTU, LDERFA, LDIMST, KNIMES)

  IF (IREP .NE. 0) GOTO 1001
ENDIF

1001 CONTINUE
KREP=IREP
IF (LHOOK) CALL DR_HOOK('FAIOPT_MT',1,ZHOOK_HANDLE)
END SUBROUTINE FAIOPT_FORT




! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FAIOPT64                                      &
&           (KREP, KNUMER, LDNOMM, CDNOMF, CDSTTU, LDERFA, &
&           LDIMST, KNIMES, CDNOMC)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
LOGICAL                LDNOMM                                 !   OUT
CHARACTER (LEN=*)      CDNOMF                                 !   OUT
CHARACTER (LEN=*)      CDSTTU                                 !   OUT
LOGICAL                LDERFA                                 !   OUT
LOGICAL                LDIMST                                 !   OUT
INTEGER (KIND=JPLIKB)  KNIMES                                 !   OUT
CHARACTER (LEN=*)      CDNOMC                                 !   OUT

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAIOPT_FORT                                             &
&           (FA, KREP, KNUMER, LDNOMM, CDNOMF, CDSTTU, LDERFA, &
&           LDIMST, KNIMES, CDNOMC)

END SUBROUTINE FAIOPT64

SUBROUTINE FAIOPT                                        &
&           (KREP, KNUMER, LDNOMM, CDNOMF, CDSTTU, LDERFA, &
&           LDIMST, KNIMES, CDNOMC)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
LOGICAL                LDNOMM                                 !   OUT
CHARACTER (LEN=*)      CDNOMF                                 !   OUT
CHARACTER (LEN=*)      CDSTTU                                 !   OUT
LOGICAL                LDERFA                                 !   OUT
LOGICAL                LDIMST                                 !   OUT
INTEGER (KIND=JPLIKM)  KNIMES                                 !   OUT
CHARACTER (LEN=*)      CDNOMC                                 !   OUT

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAIOPT_MT                                               &
&           (FA, KREP, KNUMER, LDNOMM, CDNOMF, CDSTTU, LDERFA, &
&           LDIMST, KNIMES, CDNOMC)

END SUBROUTINE FAIOPT

SUBROUTINE FAIOPT_MT                                         &
&           (FA, KREP, KNUMER, LDNOMM, CDNOMF, CDSTTU, LDERFA, &
&           LDIMST, KNIMES, CDNOMC)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
LOGICAL                LDNOMM                                 !   OUT
CHARACTER (LEN=*)      CDNOMF                                 !   OUT
CHARACTER (LEN=*)      CDSTTU                                 !   OUT
LOGICAL                LDERFA                                 !   OUT
LOGICAL                LDIMST                                 !   OUT
INTEGER (KIND=JPLIKM)  KNIMES                                 !   OUT
CHARACTER (LEN=*)      CDNOMC                                 !   OUT
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  INIMES                                 !   OUT
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)

CALL FAIOPT_FORT                                             &
&           (FA, IREP, INUMER, LDNOMM, CDNOMF, CDSTTU, LDERFA, &
&           LDIMST, INIMES, CDNOMC)

KREP       = INT (      IREP, JPLIKM)
KNIMES     = INT (    INIMES, JPLIKM)

END SUBROUTINE FAIOPT_MT

!INTF KREP            OUT 
!INTF KNUMER        IN    
!INTF LDNOMM          OUT 
!INTF CDNOMF          OUT 
!INTF CDSTTU          OUT 
!INTF LDERFA          OUT 
!INTF LDIMST          OUT 
!INTF KNIMES          OUT 
!INTF CDNOMC          OUT 
