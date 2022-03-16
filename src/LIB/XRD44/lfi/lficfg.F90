! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI

SUBROUTINE LFICFG_FORT    (LFI)
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!         Sous-programme imprimant les "PARAMETER" de base definissant
!     la configuration dans laquelle tourne le logiciel de fichiers
!     indexes LFI.
!**
!         Ce sous-programme n'a pas d'arguments.
!
!
TYPE(LFICOM) :: LFI
INTEGER (KIND=JPLIKB) IREP, INUMER, INIMES
CHARACTER(LEN=LFI%JPLSPX) CLNSPR
CHARACTER(LEN=LFI%JPLMES) CLMESS
CHARACTER(LEN=LFI%JPLFTX) CLACTI
LOGICAL LLFATA

!
!**
!     1.  -  INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('LFICFG_FORT',0,ZHOOK_HANDLE)
CLACTI=''
IF (LFI%LFICFG_LLPREA) THEN
  CALL LFIINI_FORT                 &
&                 (LFI, 2_JPLIKB )
  LFI%LFICFG_LLPREA=.FALSE.
ENDIF
!
!         Envoi d'une banniere.
!
IREP=0
INUMER=LFI%JPNIL
CLNSPR='LFICFG'
WRITE (UNIT=LFI%NULOUT,FMT='(///)')
!
IF (LFI%LFRANC) THEN
  CLMESS='Configuration du logiciel LFI'// &
&             ' ("PARAMETER" de base):'
ELSE
  CLMESS='Configuration of LFI software (basic "PARAMETER"):'
ENDIF
!
INIMES=2
LLFATA=.FALSE.
CALL LFIEMS_FORT                                        &
&               (LFI, INUMER,INIMES,IREP,LLFATA,CLMESS, &
&                CLNSPR,CLACTI)
WRITE (UNIT=LFI%NULOUT,FMT='(/)')
!**
!     2.  -  IMPRESSION DES "PARAMETER" DE BASE DU LOGICIEL.
!-----------------------------------------------------------------------
!
IF (LFI%LFRANC) THEN
  WRITE (UNIT=LFI%NULOUT,FMT=9005)LFI%JPNBIM
  WRITE (UNIT=LFI%NULOUT,FMT=9007)LFI%JPNBIC
  WRITE (UNIT=LFI%NULOUT,FMT=9010)LFI%JPNCMO
  WRITE (UNIT=LFI%NULOUT,FMT=9020)LFI%JPNCPN
  WRITE (UNIT=LFI%NULOUT,FMT=9030)LFI%JPLARD,LFI%JPLARC
  WRITE (UNIT=LFI%NULOUT,FMT=9035)LFI%JPFACX
  WRITE (UNIT=LFI%NULOUT,FMT=9037)LFI%JPXUFM
  WRITE (UNIT=LFI%NULOUT,FMT=9040)LFI%JPRECL
  WRITE (UNIT=LFI%NULOUT,FMT=9050)LFI%JPNXFI
  WRITE (UNIT=LFI%NULOUT,FMT=9060)LFI%JPNPIA
  WRITE (UNIT=LFI%NULOUT,FMT=9070)LFI%JPNXPI
  WRITE (UNIT=LFI%NULOUT,FMT=9080)LFI%JPNPIS
  WRITE (UNIT=LFI%NULOUT,FMT=9090)LFI%JPNAPP
  WRITE (UNIT=LFI%NULOUT,FMT=9100)LFI%JPLDOC
  WRITE (UNIT=LFI%NULOUT,FMT=9110)LFI%JPNPDF
  WRITE (UNIT=LFI%NULOUT,FMT=9120)LFI%JPNXPR
  WRITE (UNIT=LFI%NULOUT,FMT=9130)LFI%JPNIL
  WRITE (UNIT=LFI%NULOUT,FMT=9135)LFI%JPLFTX
  WRITE (UNIT=LFI%NULOUT,FMT=9137)LFI%JPLFIX
ELSE
  WRITE (UNIT=LFI%NULOUT,FMT=9006)LFI%JPNBIM
  WRITE (UNIT=LFI%NULOUT,FMT=9008)LFI%JPNBIC
  WRITE (UNIT=LFI%NULOUT,FMT=9011)LFI%JPNCMO
  WRITE (UNIT=LFI%NULOUT,FMT=9021)LFI%JPNCPN
  WRITE (UNIT=LFI%NULOUT,FMT=9031)LFI%JPLARD,LFI%JPLARC
  WRITE (UNIT=LFI%NULOUT,FMT=9036)LFI%JPFACX
  WRITE (UNIT=LFI%NULOUT,FMT=9038)LFI%JPXUFM
  WRITE (UNIT=LFI%NULOUT,FMT=9041)LFI%JPRECL
  WRITE (UNIT=LFI%NULOUT,FMT=9051)LFI%JPNXFI
  WRITE (UNIT=LFI%NULOUT,FMT=9061)LFI%JPNPIA
  WRITE (UNIT=LFI%NULOUT,FMT=9071)LFI%JPNXPI
  WRITE (UNIT=LFI%NULOUT,FMT=9081)LFI%JPNPIS
  WRITE (UNIT=LFI%NULOUT,FMT=9091)LFI%JPNAPP
  WRITE (UNIT=LFI%NULOUT,FMT=9101)LFI%JPLDOC
  WRITE (UNIT=LFI%NULOUT,FMT=9111)LFI%JPNPDF
  WRITE (UNIT=LFI%NULOUT,FMT=9121)LFI%JPNXPR
  WRITE (UNIT=LFI%NULOUT,FMT=9131)LFI%JPNIL
  WRITE (UNIT=LFI%NULOUT,FMT=9136)LFI%JPLFTX
  WRITE (UNIT=LFI%NULOUT,FMT=9138)LFI%JPLFIX
ENDIF
!
!         Envoi d'un message terminal.
!
WRITE (UNIT=LFI%NULOUT,FMT='(/)')
!
IF (LFI%LFRANC) THEN
  CLMESS='Fin d''impression de la '//        &
&             'Configuration du logiciel LFI'
ELSE
  CLMESS='End of dump of LFI software configuration'
ENDIF
!
CALL LFIEMS_FORT                                 &
&               (LFI, INUMER,INIMES,IREP,LLFATA, &
&                CLMESS,CLNSPR,CLACTI)
WRITE (UNIT=LFI%NULOUT,FMT='(///)')
!
IF (LHOOK) CALL DR_HOOK('LFICFG_FORT',1,ZHOOK_HANDLE)
RETURN
!
 9005 FORMAT(' Nombre de Bits par mot machine.......................', &
&       I5)
!
 9006 FORMAT(' Number of Bits per computer word.....................', &
&       I5)
!
 9007 FORMAT(' Nombre de Bits par caractere machine.................', &
&       I5)
!
 9008 FORMAT(' Number of Bits per computer character................', &
&       I5)
!
 9010 FORMAT('    ... d''ou Nombre de caracteres par mot.............', &
&       I5)
!
 9011 FORMAT('    ... so Number of characters per word is...........', &
&       I5)
!
 9020 FORMAT(' Nombre maximum de Caracteres par NOM d''ARTICLE.......', &
&       I5)
!
 9021 FORMAT(' Maximum number of Characters per RECORD NAME.........', &
&       I5)
!
 9030 FORMAT(' Longueur d''Article PHYSIQUE Elementaire des Fichiers.', &
&       I5,' Mots (',I5,' Caracteres )')
!
 9031 FORMAT(' Elementary PHYSICAL Record Length of Files...........', &
&       I5,' Words (',I5,' Characters )')
!
 9035 FORMAT(' Facteur Multiplicatif Maximal de cette Longueur......', &
&       I5)
!
 9036 FORMAT(' Maximum Multiply Factor of this elementary Length....', &
&       I5)
!
 9037 FORMAT(' Nombre maximum d''associations explicites',/,           &
&       '   entre Unites Logiques et Facteurs Multiplicatifs...', &
&       I5)
!
 9038 FORMAT(' Maximum number of explicit associations',/,             &
&       '         between Logical Units and Multiply Factors...', &
&       I5)
!
 9040 FORMAT(' "RECL" Elementaire pour l''"OPEN" des Fichiers........', &
&       I5)
!
 9041 FORMAT(' Elementary "RECL" parameter for "OPEN" of Files......', &
&       I5)
!
 9050 FORMAT(' Nombre maximum de Fichiers ouverts en meme temps',/,    &
&       '        (si leur "facteur multiplicatif" vaut 1 ).....', &
&       I5)
!
 9051 FORMAT(' Maximum number of Files open at the same time',/,       &
&       '               (if their "multiply factor" is 1 ).....', &
&       I5)
!
 9060 FORMAT(' Nombre de *PAIRES* de "PAGES d''INDEX"',/,              &
&       '        (en memoire) *PREALLOUEES* par Fichier........', &
&       I5)
!
 9061 FORMAT(' Number of *PREALLOCATED PAIRS* of "INDEX PAGES"',/,     &
&       '        (in software commons) per File................', &
&       I5)
!
 9070 FORMAT(' Nombre TOTAL de *PAIRES* de "PAGES d''INDEX"',/,        &
&       '        (en memoire) ALLOUABLES.......................', &
&       I5)
!
 9071 FORMAT(' TOTAL number of *PAIRS* of "INDEX PAGES"',/,            &
&       '        (in software commons) allocatable.............', &
&       I5)
!
 9080 FORMAT('    ... d''ou Nombre de P.P.I. non preallouees.........', &
&       I5)
!
 9081 FORMAT('    ... so Number of P.I.P. not preallocated is.......', &
&       I5)
!
 9090 FORMAT(' Nombre Maxi. utile de NOMS d''ARTICLES',/,               &
&       '        par PAGE ou ARTICLE d''INDEX Elementaire.......', &
&       I5)
!
 9091 FORMAT(' Maximum number of usable RECORD NAMES',/,               &
&       '        per Elementary INDEX PAGE or RECORD...........', &
&       I5)
!
 9100 FORMAT(' Longueur de la Partie DOCUMENTAIRE du 1er Article....', &
&       I5,' Mots')
!
 9101 FORMAT(' Length of DOCUMENTARY part in 1st physical record....', &
&       I5,' Words')
!
 9110 FORMAT(' Nombre de PAGES de DONNEES par Fichier...............', &
&       I5)
!
 9111 FORMAT(' Number of DATA PAGES per FILE........................', &
&       I5)
!
 9120 FORMAT(' Nombre Maximum de PAIRES d''ARTICLES d''INDEX',/,        &
&       '        Reservables a la Creation d''un Fichier........', &
&       I5)
!
 9121 FORMAT(' Maximum number of PAIRS of INDEX RECORDS',/,            &
&       '        that may be RESERVED at File Creation.........', &
&       I5)
!
 9130 FORMAT(' Code "VALEUR ABSENTE"', /,                               &
&       '      pour certaines Tables D''ENTIERS.................', &
&       I5)
!
 9131 FORMAT(' "MISSING VALUE" code', /,                               &
&       '      for some INTEGER-type Tables....................', &
&       I5)
!
 9135 FORMAT(' Longueur Maximale des Noms de Fichiers',/,              &
&       '                 traitable proprement.................', &
&       I5,' Caracteres')
!
 9136 FORMAT(' Maximum Length of File Names',/,                        &
&       '                    carefully handled.................', &
&       I5,' Characters')
!
 9137 FORMAT(' Longueur Maximale imprimable des Noms de Fichiers....', &
&       I5,' Caracteres')
!
 9138 FORMAT(' Maximum printable Length of File Names (most cases)..', &
&       I5,' Characters')
!
END SUBROUTINE LFICFG_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFICFG64  ()
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFICFG_FORT            &
&           (LFI)

END SUBROUTINE LFICFG64

SUBROUTINE LFICFG    ()
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFICFG_MT             &
&           (LFI)

END SUBROUTINE LFICFG

SUBROUTINE LFICFG_MT    (LFI)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
! Local integers
! Convert arguments


CALL LFICFG_FORT            &
&           (LFI)


END SUBROUTINE LFICFG_MT
