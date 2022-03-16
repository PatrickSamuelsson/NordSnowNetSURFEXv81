! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI
SUBROUTINE LFILDO_FORT                                           &
&                     (LFI, KREP, KNUMER, KREC, KTAB, KNBLEC,  &
&                      KFACTM, YDFIC, KRETIN)
USE LFIMOD, ONLY : LFICOM, LFICRW
USE PARKIND1, ONLY : JPRB, JPIB, JPIA, JPIM
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Sous-programme charge des Lectures de DOnnees du logiciel LFI,
!     *SAUF* pour les articles d'index de type caractere.
!**
!     Arguments: KREP   (Sortie) ==> Code-reponse ( zero si OK; code-
!                                    reponse du "READ" FORTRAN sinon);
!                KNUMER (Entree) ==> NUMERo d'unite logique FORTRAN;
!                KREC   (Entree) ==> Numero d'enregistrement a lire;
!                KTAB   (Sortie) ==> Zone a lire, de Longueur
!                                    LFI%JPLARD*KFACTM *mots*;
!                KNBLEC (Entree  ==> Compteur de LECtures sur l'unite;
!                       +Sortie)
!                KFACTM (Entree) ==> FACteur Multiplicatif LFI de
!                                    l'unite logique;
!                KRETIN (Sortie) ==> Code-retour interne.
!
!
TYPE(LFICOM) :: LFI
INTEGER (KIND=JPLIKB) KREP, KNUMER, KREC, KNBLEC, KFACTM, KRETIN
TYPE (LFICRW)         YDFIC
!
INTEGER (KIND=JPLIKB)  KTAB (LFI%JPLARD*KFACTM)
INTEGER (KIND=JPIB)    IREP, ISIZE

!
!        LECTURE .
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('LFILDO_FORT',0,ZHOOK_HANDLE)

IF (KNUMER > 0) THEN
  READ (UNIT=KNUMER,REC=KREC,ERR=901,IOSTAT=KREP) KTAB
ELSE
  KREP=0
  CALL LFISEE (LFI, YDFIC%N_C_FPDESC, YDFIC%N_C_OFFSET, KFACTM, KREC, KREP)
  IF (KREP /= 0) GOTO 901
  ISIZE = INT (SIZE (KTAB) * 8, JPLIKB)
  CALL FI_FREAD (IREP, KTAB, 1_JPLIKB, ISIZE, YDFIC%N_C_FPDESC)
  IF (IREP /= ISIZE) THEN
    KREP = 1
    GOTO 901
  ENDIF
  YDFIC%N_C_OFFSET = YDFIC%N_C_OFFSET + ISIZE
ENDIF

!IF (YDFIC%L_C_BTSWAP) CALL JSWAP (KTAB, KTAB, 8_JPLIKM, INT (SIZE (KTAB), JPLIKM))

IF (LFI%LMISOP) THEN
  WRITE (UNIT=LFI%NULOUT,FMT=*)                            &
&          '+++++ LFILDO - READ / ',KNUMER,', REC = ',KREC, &
&          ' +++++'
ENDIF
!
KNBLEC=KNBLEC+1
KRETIN=0
GOTO 1001
!
901 CONTINUE
KRETIN=2
!
1001 CONTINUE
!
IF (LHOOK) CALL DR_HOOK('LFILDO_FORT',1,ZHOOK_HANDLE)
END SUBROUTINE LFILDO_FORT
