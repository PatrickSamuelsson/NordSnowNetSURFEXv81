! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI
SUBROUTINE LFIFMP_FORT                       &
&                     (LFI, KNUMER, KRANFM )
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Sous-programme *INTERNE* du Logiciel de Fichiers Indexes LFI.
!     Calcule le rang de l'unite logique *KNUMER* dans la table des
!     Unites Logiques a Facteur Multiplicatif predefini;
!     si l'Unite Logique n'y est pas trouvee, le resultat est ZERO.
!
!        En mode Multi-Taches, il est necessaire de verrouiller
!     Globalement le code faisant appel a ce sous-programme.
!**
!       ARGUMENTS : KNUMER (Entree) ==> Numero d'unite logique cherche;
!                   KRANFM (Sortie) ==> Rang dans la table des unites
!                                       logiques a Facteur Multiplicatif
!                                       predefini (0 si pas trouve).
!
!
TYPE(LFICOM) :: LFI
INTEGER (KIND=JPLIKB) KNUMER, KRANFM, J, IRANFM

!**
!     1.  -  RECHERCHE DIRECTE DANS LA TABLE *LFI%MULOFM*.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('LFIFMP_FORT',0,ZHOOK_HANDLE)
DO J=1,LFI%NULOFM
!
IF (KNUMER.EQ.LFI%MULOFM(J)) THEN
  IRANFM=J
  GOTO 102
ENDIF
!
ENDDO
!
IRANFM=0
!
102 CONTINUE
KRANFM=IRANFM
!
IF (LHOOK) CALL DR_HOOK('LFIFMP_FORT',1,ZHOOK_HANDLE)
END SUBROUTINE LFIFMP_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFIFMP64           &
&           (KNUMER, KRANFM)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  KRANFM                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIFMP_FORT                &
&           (LFI, KNUMER, KRANFM)

END SUBROUTINE LFIFMP64

SUBROUTINE LFIFMP             &
&           (KNUMER, KRANFM)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKM)  KRANFM                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIFMP_MT                  &
&           (LFI, KNUMER, KRANFM)

END SUBROUTINE LFIFMP

SUBROUTINE LFIFMP_MT             &
&           (LFI, KNUMER, KRANFM)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKM)  KRANFM                                 !   OUT
! Local integers
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  IRANFM                                 !   OUT
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)

CALL LFIFMP_FORT                &
&           (LFI, INUMER, IRANFM)

KRANFM     = INT (    IRANFM, JPLIKM)

END SUBROUTINE LFIFMP_MT

!INTF KNUMER        IN    
!INTF KRANFM          OUT 
