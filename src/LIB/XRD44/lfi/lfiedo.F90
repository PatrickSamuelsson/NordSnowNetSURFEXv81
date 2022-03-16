! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI
SUBROUTINE LFIEDO_FORT                                           &
&                     (LFI, KREP, KNUMER, KREC, KTAB, KNBECR,  &
&                      KFACTM, YDFIC, KRETIN)
USE LFIMOD, ONLY : LFICOM, LFICRW
USE PARKIND1, ONLY : JPRB, JPIA, JPIB, JPIM
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Sous-programme charge des Ecritures de DOnnees du logiciel LFI,
!     *SAUF* pour les articles d'index de type caractere.
!**
!     Arguments: KREP   (Sortie) ==> Code-reponse ( zero si OK; code-
!                                    reponse du "WRITE" FORTRAN sinon);
!                KNUMER (Entree) ==> NUMERo d'unite logique FORTRAN;
!                KREC   (Entree) ==> Numero d'enregistrement a ecrire;
!                KTAB   (Sortie) ==> Zone a ecrire, de Longueur
!                                    LFI%JPLARD*KFACTM *mots*;
!                KNBECR (Entree  ==> Compteur d'ECRitures sur l'unite;
!                       +Sortie)
!                KFACTM (Entree) ==> FACteur Multiplicatif LFI de
!                                    l'unite logique;
!                KRETIN (Sortie) ==> Code-retour interne.
!
!
TYPE(LFICOM) :: LFI
INTEGER (KIND=JPLIKB) KREP, KNUMER, KREC, KNBECR, KFACTM, KRETIN
TYPE (LFICRW)         YDFIC
!
INTEGER (KIND=JPLIKB), TARGET  :: KTAB (LFI%JPLARD*KFACTM)
!
INTEGER (KIND=JPLIKB), TARGET  :: JTAB (LFI%JPLARD*KFACTM)
INTEGER (KIND=JPLIKB), POINTER :: ITAB (:)
INTEGER (KIND=JPIB)    IREP, ISIZE

!
!        ECRITURE .
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('LFIEDO_FORT',0,ZHOOK_HANDLE)

IF (YDFIC%L_C_BTSWAP) THEN
  CALL JSWAP (JTAB, KTAB, 8_JPLIKM, INT (SIZE (KTAB), JPLIKM))
  ITAB => JTAB
ELSE
  ITAB => KTAB
ENDIF
IF (KNUMER > 0) THEN
  WRITE (UNIT=KNUMER,REC=KREC,ERR=901,IOSTAT=KREP) ITAB
ELSE
  KREP=0
  CALL LFISEE (LFI, YDFIC%N_C_FPDESC, YDFIC%N_C_OFFSET, KFACTM, KREC, KREP)
  IF (KREP /= 0) GOTO 901
  ISIZE = INT (SIZE (ITAB) * 8, JPLIKB)
  CALL FI_FWRITE (IREP, ITAB, 1_JPLIKB, ISIZE, YDFIC%N_C_FPDESC)
  IF (IREP /= ISIZE) THEN
    KREP = 1
    GOTO 901
  ENDIF
  YDFIC%N_C_OFFSET = YDFIC%N_C_OFFSET + ISIZE
ENDIF

!
IF (LFI%LMISOP) THEN
  WRITE (UNIT=LFI%NULOUT,FMT=*)                             &
&          '+++++ LFIEDO - WRITE / ',KNUMER,', REC = ',KREC, &
&          ' +++++'
ENDIF
!
KNBECR=KNBECR+1
KRETIN=0
GOTO 1001
!
901 CONTINUE
KRETIN=1
!
1001 CONTINUE
!
IF (LHOOK) CALL DR_HOOK('LFIEDO_FORT',1,ZHOOK_HANDLE)
END SUBROUTINE LFIEDO_FORT
