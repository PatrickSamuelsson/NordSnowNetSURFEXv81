! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FANDAI_FORT                                  &
&                     (FA,  KREP, KRANG, KDATEF, KDATXF, LDMODA )
USE FA_MOD, ONLY : FA_COM, JPNIIL, &
                 & JD_YEA, JD_MON, JD_DAY, & 
                 & JD_HOU, JD_MIN, JD_TUN, & 
                 & JD_THO,         JD_IAN, &
                 & JD_CU1, JD_CU2,         &
                 & JD_DEX,         JD_SEM, &
                 & JD_SET, JD_CE1, JD_CE2, &
                 & JD_TST, JD_FMT

USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!      Sous-programme INTERNE du logiciel de Fichiers ARPEGE:
!     Definition d'une (Nouvelle) Date.
!**
!    Arguments : KREP   (Sortie)        ==> Code-reponse du sous-programme;
!                KRANG  (Entree)        ==> Rang de l'unite logique;
!     (Tableau)  KDATEF (Entree/Sortie) ==> Date elle-meme (FA%JPLDAT mots).
!                KDATXF (Entree)        ==> Date etendue
!                LDMODA (Sortie)        ==> Vrai s'il y a modification d'une
!                                           date deja definie.
!*
!       En mode multi-taches, il doit y avoir verrouillage du fichier
!     concerne avant l'appel au sous-programme.
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KRANG
INTEGER (KIND=JPLIKB) KDATEF (FA%JPLDAT)
INTEGER (KIND=JPLIKB) KDATXF (FA%JPLDAT)
!
INTEGER (KIND=JPLIKB) IMI123, IMAX69, IMINIM
INTEGER (KIND=JPLIKB) J, ILMOIS, INIMES, INUMER
INTEGER (KIND=JPLIKB) ISECMAX
!
LOGICAL LDMODA
!
CHARACTER(LEN=FA%JPXNOM) CLACTI 
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA

!**
!     1.  -  CONTROLES DES PARAMETRES D'APPEL.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FANDAI_MT',0,ZHOOK_HANDLE)
CLACTI=''
LDMODA=.FALSE.
KREP=0
!
IF (KRANG.LE.0.OR.KRANG.GT.FA%JPNXFA) THEN
  KREP=-66
  GOTO 1001
ENDIF

IF (KDATXF (JD_DEX-FA%JPLDAT) > 0) THEN

!
!         Controle de la Date etendue.
!
  IF (ANY (KDATXF < 0)) KREP=-82

  IF (KDATEF (JD_HOU) < 0 .AND. KDATEF (JD_MIN) < 0) THEN
    KDATEF (JD_HOU) = KDATXF (JD_SEM-FA%JPLDAT) / 3600
    KDATEF (JD_MIN) = KDATXF (JD_SEM-FA%JPLDAT) / 60
  ENDIF
 
!
!         Controle de la coherence de la date et de la date etendue
!
  IF (KDATXF (JD_FMT-FA%JPLDAT) > 0) THEN
    ISECMAX = 60
  ELSE
    ISECMAX = 3600
  ENDIF

  IF (ABS (KDATEF (JD_HOU) * 3600 + KDATEF (JD_MIN) * 60 - KDATXF (JD_SEM-FA%JPLDAT)) > ISECMAX) KREP=-82

  CALL FANDAI_CMPSEC (KDATEF (JD_TUN), KDATEF (JD_THO), KDATXF (JD_SET-FA%JPLDAT))
  CALL FANDAI_CMPSEC (KDATEF (JD_TUN), KDATEF (JD_CU1), KDATXF (JD_CE1-FA%JPLDAT))
  CALL FANDAI_CMPSEC (KDATEF (JD_TUN), KDATEF (JD_CU2), KDATXF (JD_CE2-FA%JPLDAT))

  IF (KREP /= 0) GOTO 1001
ENDIF

!
!         Controle de la Date proprement dite.
!
IMI123=MIN (KDATEF(JD_YEA),KDATEF(JD_MON),KDATEF(JD_DAY))
IMAX69=MAX (KDATEF(JD_TUN),KDATEF(JD_IAN))
IMINIM=KDATEF(JD_YEA)
!
DO J=2,FA%JPLDAT
IMINIM=MIN (IMINIM,KDATEF(J))
ENDDO
!
IF (IMINIM.LT.0.OR.IMI123.LE.0.OR.KDATEF(JD_MON).GT.12.OR.                    &
&   KDATEF(JD_DAY).GT.31.OR.KDATEF(JD_HOU).GE.24.OR.KDATEF(JD_MIN).GE.60.OR.  &
&   IMAX69.GE.255.OR.                                                         &
& (KDATEF(JD_CU1).LE.KDATEF(JD_CU2).AND.(KDATEF(JD_CU1)*KDATEF(JD_CU2)).NE.0)) THEN
!
!        Erreur de syntaxe.
!
  KREP=-82
  GOTO 1001
ELSEIF ((KDATEF(JD_MON).GT.7.OR.MOD (KDATEF(JD_MON),2_JPLIKB ).EQ.0).AND.  &
&       (KDATEF(JD_MON).LE.7.OR.MOD (KDATEF(JD_MON),2_JPLIKB ).EQ.1)) THEN
!
!        Controle de coherence (annee,mois,jour).
!
  IF (KDATEF(JD_MON).EQ.2) THEN
    ILMOIS=28+MAX (0_JPLIKB ,1-MOD (KDATEF(JD_YEA),4_JPLIKB ))
  ELSE
    ILMOIS=30
  ENDIF
!
  IF (KDATEF(JD_DAY).GT.ILMOIS) THEN
    KREP=-82
    GOTO 1001
  ENDIF
!
ENDIF


IF (KDATXF (JD_DEX-FA%JPLDAT) == 0) THEN
  
!
!        Calcul de la date etendue si elle n'est pas definie
!
  KDATXF (JD_DEX-FA%JPLDAT) = 1
  KDATXF (JD_SEM-FA%JPLDAT) = KDATEF (JD_HOU) * 3600 + KDATEF (JD_MIN) * 60

  CALL FANDAI_SETSEC (KDATEF (JD_TUN), KDATEF (JD_THO), KDATXF (JD_SET-FA%JPLDAT))
  CALL FANDAI_SETSEC (KDATEF (JD_TUN), KDATEF (JD_CU1), KDATXF (JD_CE1-FA%JPLDAT))
  CALL FANDAI_SETSEC (KDATEF (JD_TUN), KDATEF (JD_CU2), KDATXF (JD_CE2-FA%JPLDAT))
  KDATXF (JD_TST-FA%JPLDAT) = 1800

ENDIF

!**
!     2.  -  SI DATE DEJA DEFINIE, COMPARAISON ANCIENNE/NOUVELLE.
!-----------------------------------------------------------------------
!
IF (.NOT. FA%FICHIER(KRANG)%LCREAF) THEN
!
  DO J=1,FA%JPLDAT
!
  IF (FA%FICHIER(KRANG)%MADATE(J).NE.KDATEF(J)) THEN
    LDMODA=.TRUE.
    GOTO 300
  ENDIF
!
  ENDDO
!
  DO J=1,FA%JPLDAT
!
  IF (FA%FICHIER(KRANG)%MADATX(J).NE.KDATXF(J)) THEN
    LDMODA=.TRUE.
    GOTO 300
  ENDIF
!
  ENDDO
!
!         Si on arrive ici, il y a redefinition a l'identique.
!
  GOTO 1001
ENDIF
!**
!     3.  -  SI NECESSAIRE, MISE A JOUR DU TABLEAU "FA%MADATE".
!-----------------------------------------------------------------------
!
300 CONTINUE
!
FA%FICHIER(KRANG)%MADATE (:) = KDATEF (:)
FA%FICHIER(KRANG)%MADATX (:) = KDATXF (:)
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
  CLNSPR='FANDAI'
  INUMER=JPNIIL
  WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KRANG='',I4,    &
&       '', KDATEF(1:5)='',I5,2(''/'',I2),I3,'':'',I2.2,    &
&       '', KDATEF(7:8)='',I6,''-'',I6,'', KDATXF='',11I6,  &
&       '', LDMODA= '',L1)') &
&     KREP,KRANG,(KDATEF(J),J=1,5),(KDATEF(J),J=7,8),KDATXF,&
&     LDMODA
  CALL FAIPAR_FORT                                      &
&               (FA, INUMER,INIMES,KREP,.FALSE.,CLMESS, &
&                CLNSPR,CLACTI, .FALSE.)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('FANDAI_MT',1,ZHOOK_HANDLE)

CONTAINS

SUBROUTINE FANDAI_SETSEC (KDATEF6, KDATEFX, KSECS)

INTEGER (KIND=JPLIKB), INTENT (IN)    :: KDATEF6
INTEGER (KIND=JPLIKB), INTENT (IN)    :: KDATEFX
INTEGER (KIND=JPLIKB), INTENT (INOUT) :: KSECS

SELECT CASE (KDATEF6)
  CASE (1)
    KSECS = KDATEFX * 3600
  CASE (2)
    KSECS = KDATEFX * 3600 * 24
  CASE DEFAULT
    KSECS = -1
END SELECT

END SUBROUTINE FANDAI_SETSEC

SUBROUTINE FANDAI_CMPSEC (KDATEF6, KDATEFX, KSECS)

INTEGER (KIND=JPLIKB), INTENT (INOUT) :: KDATEF6
INTEGER (KIND=JPLIKB), INTENT (INOUT) :: KDATEFX
INTEGER (KIND=JPLIKB), INTENT (IN)    :: KSECS

IF (KDATEF6 < 0) THEN
  IF (KSECS < 65000) THEN
    KDATEF6 = 1
  ELSE
    KDATEF6 = 2
  ENDIF
ENDIF

IF (KDATEFX < 0) THEN

  SELECT CASE (KDATEF6)
    CASE (1)
      KDATEFX = NINT (REAL (KSECS, JPDBLR) / 3600._JPDBLR)
    CASE (2)
      KDATEFX = NINT (REAL (KSECS, JPDBLR) / 3600._JPDBLR) / 24
    CASE DEFAULT
      KREP=-82
  END SELECT

ENDIF

SELECT CASE (KDATEF6)
  CASE (1)
    IF (ABS (KDATEFX * 3600 - KSECS) > 3600) THEN
      KREP=-82
    ENDIF
  CASE (2)
    IF (ABS (KDATEFX * 3600 * 24 - KSECS) > 3600 * 24) THEN
      KREP=-82
    ENDIF
  CASE DEFAULT
    KREP=-82
END SELECT

END SUBROUTINE FANDAI_CMPSEC

#include "facom2.llmoer.h"

END SUBROUTINE FANDAI_FORT

