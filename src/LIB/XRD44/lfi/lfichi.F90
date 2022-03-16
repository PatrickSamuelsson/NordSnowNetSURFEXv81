! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI
SUBROUTINE LFICHI_FORT                                   &
&                     (LFI, KREP, CDSTRU, KVAL, KPOSC2 )
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        SOUS-PROGRAMME *INTERNE* DU LOGICIEL DE FICHIERS INDEXES LFI;
!     permet de decoder une valeur entiere (CHIffres)
!     dans une chaine de caracteres.
!**
!    ARGUMENTS : KREP   (Sortie) ==> Code-Reponse du sous-programme;
!                CDSTRU (Entree) ==> Chaine a decoder;
!                KVAL   (Sortie) ==> Valeur entiere decodee;
!                KPOSC2 (Sortie) ==> Position du dernier chiffre.
!
TYPE(LFICOM) :: LFI
CHARACTER(LEN=*) CDSTRU
CHARACTER(LEN=7) CLFORM
!
INTEGER (KIND=JPLIKB) KREP, KVAL, KPOSC2
INTEGER (KIND=JPLIKB) ILUSTR, J, IPOSC1, IPOSC2

!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('LFICHI_FORT',0,ZHOOK_HANDLE)
KREP=0
ILUSTR=INT (LEN (CDSTRU), JPLIKB)
!
DO J=1,ILUSTR
!
IF (CDSTRU(J:J).NE.' ') THEN
!
  IPOSC1=INT (INDEX (LFI%LFICHI_CLCHIF,CDSTRU(J:J)), JPLIKB)
!
  IF (IPOSC1.EQ.0) THEN
    KREP=-40
    GOTO 1001
  ENDIF
!
  IPOSC1=J
  GOTO 223
!
ENDIF
!
ENDDO
!
IPOSC1=1
!
223 CONTINUE
!
DO J=IPOSC1+1,ILUSTR
!
IF (INT (INDEX (LFI%LFICHI_CLCHIF,CDSTRU(J:J)), JPLIKB).EQ.0) THEN
  IPOSC2=J-1
  GOTO 225
ENDIF
!
ENDDO
!
IPOSC2=ILUSTR
!
225 CONTINUE
!
WRITE (UNIT=CLFORM,FMT='(''(BN,I'',I1,'')'')') IPOSC2-IPOSC1+1
READ (UNIT=CDSTRU(IPOSC1:IPOSC2),FMT=CLFORM,ERR=226) KVAL
KPOSC2=IPOSC2
GOTO 1001
!
226 CONTINUE
!
KREP=-40
!
1001 CONTINUE
!
IF (LHOOK) CALL DR_HOOK('LFICHI_FORT',1,ZHOOK_HANDLE)
END SUBROUTINE LFICHI_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFICHI64                    &
&           (KREP, CDSTRU, KVAL, KPOSC2)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
CHARACTER (LEN=*)      CDSTRU                                 ! IN   
INTEGER (KIND=JPLIKB)  KVAL                                   !   OUT
INTEGER (KIND=JPLIKB)  KPOSC2                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFICHI_FORT                            &
&           (LFI, KREP, CDSTRU, KVAL, KPOSC2)

END SUBROUTINE LFICHI64

SUBROUTINE LFICHI                      &
&           (KREP, CDSTRU, KVAL, KPOSC2)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
CHARACTER (LEN=*)      CDSTRU                                 ! IN   
INTEGER (KIND=JPLIKM)  KVAL                                   !   OUT
INTEGER (KIND=JPLIKM)  KPOSC2                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFICHI_MT                              &
&           (LFI, KREP, CDSTRU, KVAL, KPOSC2)

END SUBROUTINE LFICHI

SUBROUTINE LFICHI_MT                        &
&           (LFI, KREP, CDSTRU, KVAL, KPOSC2)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
CHARACTER (LEN=*)      CDSTRU                                 ! IN   
INTEGER (KIND=JPLIKM)  KVAL                                   !   OUT
INTEGER (KIND=JPLIKM)  KPOSC2                                 !   OUT
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  IVAL                                   !   OUT
INTEGER (KIND=JPLIKB)  IPOSC2                                 !   OUT
! Convert arguments


CALL LFICHI_FORT                            &
&           (LFI, IREP, CDSTRU, IVAL, IPOSC2)

KREP       = INT (      IREP, JPLIKM)
KVAL       = INT (      IVAL, JPLIKM)
KPOSC2     = INT (    IPOSC2, JPLIKM)

END SUBROUTINE LFICHI_MT

!INTF KREP            OUT 
!INTF CDSTRU        IN    
!INTF KVAL            OUT 
!INTF KPOSC2          OUT 
