! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI
SUBROUTINE LFIENG_FORT                                      &
&                     (LFI, KNUMER, KNIMES, KCODE, LDFATA,  &
&                      CDMESS, CDNSPR,CDACTI )
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE SDL_MOD   , ONLY : SDL_SRLABORT
USE LFI_PRECISION
IMPLICIT NONE
!****
!        THIS SUBROUTINE PRINTS STANDARD MESSAGES FROM LFI INDEXED-
!     FILE SOFTWARE, ABORTING PROGRAM IF REQUIRED.
!        Messages related to "debugging mode" are directly printed
!     by concerned subroutines.
!
!        This subroutine is the english version, translated from the
!     the original (french) one, and is always called through the
!     "hat" routine LFIEMS.
!     For french version see subroutine LFIEFR.
!     ( Pour la version francaise, voir LFIEFR )
!**
!  DUMMY ARGUMENTS : KNUMER ==> Logical Unit concerned, if any;
!  ( all INPUT ones )           ( if LFI%JPNIL ==> no Logical Unit )
!                    KNIMES ==> Level (0,1,2) of Message;
!                    KCODE  ==> Response code of action concerned;
!                    LDFATA ==> True if Abort of program is required;
!                    CDMESS ==> If KNIMES#0, Message to print;
!                    CDNSPR ==> Subroutine name which calls LFIEMS;
!                    CDACTI ==> Name of FORTRAN input/output action
!                               if KCODE >0, else... it depends !
!*
!     !----------------------------------------------------------------!
!     !   TABLE OF POSSIBLE VALUES FOR RESPONSE CODES OF LFI SOFTWARE  !
!     !----------------------------------------------------------------!
!
!-----------------------------------------------------------------------
!      0 ==> No error has been detected, everything is OK.
!-----------------------------------------------------------------------
!positive==> It is the (absolute value of) FORTRAN response code
!  value     from an OPEN, READ, WRITE, CLOSE or INQUIRE instruction;
!            see vendor's reference manual for exact meaning.
!-----------------------------------------------------------------------
!     -1 ==> Logical Unit currently not opened for the software.
!-----------------------------------------------------------------------
!     -2 ==> "LEVEL" value outside [0-2] range .
!-----------------------------------------------------------------------
!     -3 ==> Bad lock option (internal subroutine "LFIVER") .
!-----------------------------------------------------------------------
!     -4 ==> Explicit change for Multi-Tasking mode, but almost one unit
!            is currently open-problems may arise (subroutine "LFIINI").
!-----------------------------------------------------------------------
!     -5 ==> Logical Unit is currently opened (LFIOUV, LFIAFM, LFISFM) .
!-----------------------------------------------------------------------
!     -6 ==> Not enough space within tables to open requested Unit.
!            (LFIOUV)
!-----------------------------------------------------------------------
!     -7 ==> Invalid "STATUS" for FORTRAN instruction "OPEN" (LFIOUV) .
!-----------------------------------------------------------------------
!     -8 ==> Incompatible values given for "LDNOMM" and "CDSTTO":
!            a file which "STATUS" is 'OLD' or 'NEW' must have a name .
!            (LFIOUV) (THIS REPONSE CODE HAS CURRENTLY NO MORE SENSE)
!-----------------------------------------------------------------------
!     -9 ==> Incompatibility between "STATUS" 'NEW' or 'OLD' and (respe-
!            ctively) file existence or non-existence (LFIOUV) .
!-----------------------------------------------------------------------
!    -10 ==> The file is not a LFI one, or may not be treated through
!            this configuration or version of the software (LFIOUV) .
!-----------------------------------------------------------------------
!    -11 ==> File not closed after a modification (LFIOUV): this
!            error is not fatal if "LDERFA" is .FALSE., but in such a
!            case file integrity and data coherence are not guaranteed.
!            Note that once a file has got such problem, this response
!            code will stay even after a subsequent modification.
!-----------------------------------------------------------------------
!    -12 ==> File has a "STATUS" 'OLD' but an error occurred when
!            reading the first physical record of file (LFIOUV) .
!-----------------------------------------------------------------------
!    -13 ==> File is already open for another LFI logical unit.
!            (LFIOUV)
!-----------------------------------------------------------------------
!    -14 ==> Incorrect value for INTEGER argument (generally negative) .
!-----------------------------------------------------------------------
!    -15 ==> Incorrect CHARACTER argument (too long, for instance).
!-----------------------------------------------------------------------
!    -16 ==> Incoherence in Tables, File, internal calls, software.
!            THIS ERROR MAY NEVER BE FILTERED. ALWAYS FATAL.
!-----------------------------------------------------------------------
!    -17 ==> Too many logical records to store an extra one (LFIECR) .
!            (note that logical records consist of user-readable data
!             records, but also of holes cataloged in index... which are
!             created when existing records may not be rewritten in
!             place, or when records are suppressed; such holes may be
!             "re-cycled")
!-----------------------------------------------------------------------
!    -18 ==> A logical record name formed only with SPACES is invalid.
!            (for internal use of LFI software, holes in index are
!             described by a blank record name)
!-----------------------------------------------------------------------
!    -19 ==> File opened with "STATUS" set to 'SCRATCH', so may not be
!            kept at CLOSE time: 'KEEP' is illicit for "CDSTTC" (LFIFER)
!            if this error is not fatal, then a FORTRAN "CLOSE" without
!            "STATUS" parameter is performed, in the same manner as if
!            "CDSTTC" is neither 'KEEP' nor 'DELETE'.
!-----------------------------------------------------------------------
!    -20 ==> No logical record with such name found within logical unit.
!            (LFILEC, LFIREN, LFISUP)
!-----------------------------------------------------------------------
!    -21 ==> Requested logical record is LONGER (has more data) in file;
!            if this error is not fatal, then a PARTIAL read is
!            performed, at requested length.
!            (LFILAP, LFILAS, LFILEC)
!-----------------------------------------------------------------------
!    -22 ==> Requested logical record SHORTER (has less data) in file;
!            even if this error is not fatal, NO READING OF DATA OCCURS.
!            (LFILAP, LFILAS, LFILEC) .
!-----------------------------------------------------------------------
!    -23 ==> No or no more "NEXT" record to read (LFILAS) .
!-----------------------------------------------------------------------
!    -24 ==> The character variable given as actual output argument is
!            TOO SHORT to store the record NAME, even when suppressing
!            any spaces at the end of the name.
!            (LFICAP, LFICAS, LFILAP, LFILAS)
!-----------------------------------------------------------------------
!    -25 ==> The new name of the logical record is (already) used for
!            another logical record within the file (LFIREN).
!-----------------------------------------------------------------------
!    -26 ==> No or no more "PREVIOUS" logical record to read (LFILAP).
!-----------------------------------------------------------------------
!    -27 ==> Insufficient CONTIGUOUS space within tables to treat the
!            "multiple" file requested (LFIOUV) .
!-----------------------------------------------------------------------
!    -28 ==> Multiply factor (of elementary physical record length) too
!            big for the current configuration of the software.
!            (LFIOUV, LFIAFM, LFIFMD)
!-----------------------------------------------------------------------
!    -29 ==> Not enough space within tables to store the multiply factor
!            to be associated to logical unit (LFIAFM) .
!-----------------------------------------------------------------------
!    -30 ==> Logical unit number invalid for FORTRAN.
!-----------------------------------------------------------------------
!    -31 ==> Logical unit has no multiply factor predefined.
!            (LFISFM)
!-----------------------------------------------------------------------
!
!
TYPE(LFICOM) :: LFI
INTEGER (KIND=JPLIKB) KNUMER, KNIMES, KCODE, ILDMES 
INTEGER (KIND=JPLIKB) ILBLAN, INLNOM, INUMER
INTEGER (KIND=JPLIKB) IDECBL, IPOSBL, ILACTI, ILACT2 
INTEGER (KIND=JPLIKB) ILNSPR, ILMESU, IJL, J, IJ
INTEGER (KIND=JPLIKB) INBALO, ILMESA, INLIGN, IDECAL
!
LOGICAL LDFATA
!
CHARACTER(LEN=*)  CDNSPR
CHARACTER(LEN=6)  CLJOLI
CHARACTER(LEN=*)  CDMESS
CHARACTER(LEN=80) CLMESA
CHARACTER(LEN=*)  CDACTI
!
CHARACTER(LEN=LFI%JPLMES) CLMESS

!**
!     1.  -  INITIALISATIONS.
!-----------------------------------------------------------------------
!
!        Search for "useful" length of argument CDACTI.
!        (i.e. not taking into account any blank characters at the end)
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('LFIENG_FORT',0,ZHOOK_HANDLE)
IDECBL=0
!
101 CONTINUE
IPOSBL=IDECBL+INT (INDEX (CDACTI(IDECBL+1:),' '), JPLIKB)
!
IF (IPOSBL.LE.IDECBL) THEN
  ILACTI=INT (LEN (CDACTI), JPLIKB)
ELSEIF (CDACTI(IPOSBL:).EQ.' ') THEN
  ILACTI=IPOSBL-1
ELSE
  IDECBL=IPOSBL
  GOTO 101
ENDIF
!
ILACT2=MIN (ILACTI,LFI%JPNCPN)
ILACTI=MIN (ILACT2,8_JPLIKB )
ILNSPR=MIN (INT (LEN (CDNSPR), JPLIKB),LFI%JPLSPX)
!
!        Prefix (and possible suffix) for the message(s).
!
IF (LDFATA) THEN
  CLJOLI=' *****'
ELSEIF (KNIMES.EQ.0.OR.KCODE.NE.0) THEN
  CLJOLI=' */*/*'
ELSE
  CLJOLI=' /////'
ENDIF
!
IF (KNIMES.NE.0) THEN
!**
!     2.  -  PRINTS MESSAGE PREPARED BY SUBROUTINE WHICH CALLED LFIEMS.
!-----------------------------------------------------------------------
!
  ILMESU=MIN (INT (LEN (CLMESS), JPLIKB)-     &
&         INT (LEN (CLJOLI), JPLIKB)-ILNSPR-4, &
&         INT (LEN (CDMESS), JPLIKB))
  CLMESS=CLJOLI//' '//CDNSPR(1:ILNSPR)//' - ' &
&             //CDMESS(1:ILMESU)
  WRITE (UNIT=LFI%NULOUT,FMT='(A)') TRIM (CLMESS)
ENDIF
!
IF (KNIMES.EQ.0.OR.LDFATA) THEN
!**
!     3.  -  CONSTITUTION OF "AD HOC" MESSAGE, DEPENDING OF *KCODE*.
!-----------------------------------------------------------------------
!
!     Before, check if logical unit considered corresponds to a logical
!     unit currently opened for LFI software (or not).
!
  IF (KNUMER.EQ.LFI%JPNIL) THEN
    IJL=0
  ELSE
!
    DO J=1,LFI%NBFIOU
    IJL=LFI%NUMIND(J)
    IF (KNUMER.EQ.LFI%NUMERO(IJL)) GOTO 302
    ENDDO
!
    IJL=0
  ENDIF
!
302 CONTINUE
!
  IF (KCODE.GT.0) THEN
!
    IF ((CDACTI.EQ.'READ'.OR.CDACTI.EQ.'WRITE') &
&        .AND.LFI%NUMAPH(IJL).GT.0) THEN
      WRITE (UNIT=CLMESS,FMT='(''ERROR "'',A,''"'',I7,    &
&             '',UNIT'',I3,'',REC.NUM'',I6,'',*'',I6,      &
&             '' WORDS'')') CDACTI(1:ILACTI),KCODE,KNUMER, &
&                            LFI%NUMAPH(IJL),              &
&                            LFI%JPLARD*LFI%MFACTM(IJL)
    ELSE
      WRITE (UNIT=CLMESS,                                        &
&             FMT='(''FORTRAN "'',A,''" ERROR, CODE=''            &
&             ,I7,'', UNIT='',I3)') CDACTI(1:ILACTI),KCODE,KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-1) THEN
    WRITE (UNIT=CLMESS,FMT='(''LOGICAL UNIT'',I3,     &
&           '' NOT OPENED FOR LFI SOFTWARE'')') KNUMER
!
  ELSEIF (KCODE.EQ.-2) THEN
!
    IF (KNUMER.EQ.LFI%JPNIL) THEN
      CLMESS='ACTUAL VALUE FOR LEVEL "KNIVAU" '// &
&                 'OUTSIDE [0-2] RANGE'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                                 &
&   '(''MESSAGE LEVEL OUTSIDE [0-2] RANGE, UNIT'',I3)') KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-3) THEN
    ILDMES=MIN (8_JPLIKB ,INT (LEN (CDMESS), JPLIKB))
    CLMESS='UNKNOWN ACTION '''//CDMESS(1:ILDMES) &
&           //''' ON LOCKS'
!
  ELSEIF (KCODE.EQ.-4) THEN
    CLMESS='EXPL.CHANGE OF MULTI-TASKING MODE '// &
&               'WITH UNIT(S) OPENED'
!
  ELSEIF (KCODE.EQ.-5) THEN
    WRITE (UNIT=CLMESS,FMT='(''LOGICAL UNIT'',I3,                 &
&          '' ALREADY OPENED FOR LFI - AND SHOULD NOT.'')') KNUMER
!
  ELSEIF (KCODE.EQ.-6) THEN
    WRITE (UNIT=CLMESS,FMT='(I3,'' ENTRIES,'',         &
&    '' NOT ENOUGH PLACE WITHIN TABLES FOR UNIT'',I3)') &
&    LFI%JPNXFI,KNUMER
!
  ELSEIF (KCODE.EQ.-7) THEN
    WRITE (UNIT=CLMESS,FMT='(''FORTRAN STATUS'''''',A,          &
&           '''''' UNKNOWN, UNIT'',I3)') CDACTI(1:ILACTI),KNUMER
!
  ELSEIF (KCODE.EQ.-8) THEN
    WRITE (UNIT=CLMESS,FMT='(''UNIT'',I3,'' OF STATUS ''''''      &
&,A,'''''' MUST HAVE AN EXPLICIT NAME'')') KNUMER,CDACTI(1:ILACTI)
!
  ELSEIF (KCODE.EQ.-9) THEN
!
  IF (CDACTI.EQ.'OLD') THEN
    WRITE (UNIT=CLMESS,FMT=                             &
&'(''STATUS ''''OLD'''' BUT FILE DOES NOT EXIST, UNIT'', &
&      I3)') KNUMER
  ELSE
    ILBLAN=INT (INDEX (CDACTI(1:ILACTI),' '), JPLIKB)
    IF (ILBLAN.GT.1) ILACTI=ILBLAN-1
    WRITE (UNIT=CLMESS,FMT=                                 &
&'(''STATUS '''''',A,'''''' BUT FILE ALREADY EXISTS, UNIT'', &
&  I3)') CDACTI(1:ILACTI),KNUMER
  ENDIF
!
  ELSEIF (KCODE.EQ.-10) THEN
    WRITE (UNIT=CLMESS,FMT='(''INCOMPATIBILITY'',  &
&           '' FILE / SOFTWARE, UNIT'',I3)') KNUMER
!
  ELSEIF (KCODE.EQ.-11) THEN
    WRITE (UNIT=CLMESS,                              &
&           FMT='(''UNIT'',I3,'' NOT CLOSED AFTER '', &
&           ''ITS LAST MODIFICATION'')') KNUMER
!
  ELSEIF (KCODE.EQ.-12) THEN
    WRITE (UNIT=CLMESS,FMT='(''UNIT'',I3,                     &
&  '' OF STATUS ''''OLD'''' - READ OF FIRST RECORD FAILED'')') &
&       KNUMER
!
  ELSEIF (KCODE.EQ.-13) THEN
    INLNOM=1
    INUMER=LFI%JPNIL
!
    DO J=1,LFI%NBFIOU
    IJ=LFI%NUMIND(J)
!
    IF (CDACTI.EQ.LFI%CNOMFI(IJ)) THEN
      INUMER=LFI%NUMERO(IJ)
      INLNOM=MIN (LFI%NLNOMF(IJ),                       &
&                  INT (LEN (CLMESS), JPLIKB)-3_JPLIKB )
      GOTO 132
    ENDIF
!
    ENDDO
!
132 CONTINUE
    CLMESS=' '''//CDACTI(1:INLNOM)//''''
    WRITE (UNIT=LFI%NULOUT,FMT='(A)') TRIM (CLMESS)
    WRITE (UNIT=CLMESS,FMT='(''UNIT'',I3,'' - FILE '',    &
&           ''ALREADY OPEN WITH UNIT'',I3)') KNUMER,INUMER
!
  ELSEIF (KCODE.EQ.-14) THEN
!
    IF (CDNSPR.EQ.'LFIECR'.OR.CDNSPR.EQ.'LFILEC'.OR.   &
&        CDNSPR.EQ.'LFILAS'.OR.CDNSPR.EQ.'LFILAP') THEN
      WRITE (UNIT=CLMESS,FMT=                       &
&   '(''INCORRECT RECORD LENGTH, UNIT'',I3)') KNUMER
    ELSEIF (KNUMER.EQ.LFI%JPNIL) THEN
      CLMESS='INCORRECT ENTRY IN *LFI%NUMERO* TABLE'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                               &
&   '(''INCORRECT INTEGER TYPE ARGUMENT, UNIT'',I3)') KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-15) THEN
    WRITE (UNIT=CLMESS,FMT='(''RECORD NAME INCORRECT OR '', &
&           ''TOO LONG, UNIT'',I3)') KNUMER
!
  ELSEIF (KCODE.EQ.-16) THEN
    WRITE (UNIT=CLMESS,FMT='(''INCOHERENCE (TABLES, FILE, '', &
&           ''INTERNAL CALLS, SOFTWARE), UNIT'',I3)') KNUMER
!
  ELSEIF (KCODE.EQ.-17) THEN
!
    IF (IJL.NE.0) THEN
      INBALO=LFI%MDES1D(IXM(LFI%JPNALO,IJL))
    ELSE
      INBALO=LFI%JPNIL
    ENDIF
!
    WRITE (UNIT=CLMESS,                             &
&           FMT='(I6,'' RECORDS, INDEX FULL, UNIT'', &
&           I3)') INBALO,KNUMER
!
  ELSEIF (KCODE.EQ.-18) THEN
    WRITE (UNIT=CLMESS,FMT='(''BLANK RECORD NAME IS INVALID'', &
&           '', UNIT'',I3)') KNUMER
!
  ELSEIF (KCODE.EQ.-19) THEN
    WRITE (UNIT=CLMESS,FMT='(''UNIT'',I3,                        &
&           '' IS ''''SCRATCH'''', SO MAY NOT BE KEPT'')') KNUMER
!
  ELSEIF (KCODE.EQ.-20) THEN
    WRITE (UNIT=CLMESS,FMT='(''RECORD "'',A,                   &
&           ''" NOT FOUND, UNIT'',I3)') CDACTI(1:ILACT2),KNUMER
!
  ELSEIF (KCODE.EQ.-21) THEN
    WRITE (UNIT=CLMESS,FMT='(''RECORD "'',A,  &
&    ''" *LONGER* THAN REQUESTED, UNIT'',I3)') &
&      CDACTI(1:ILACT2),KNUMER
!
  ELSEIF (KCODE.EQ.-22) THEN
    WRITE (UNIT=CLMESS,FMT='(''RECORD "'',A,  &
&    ''" *SHORTER* THAN REQUESTED-UNIT'',I3)') &
&      CDACTI(1:ILACT2),KNUMER
!
  ELSEIF (KCODE.EQ.-23) THEN
    WRITE (UNIT=CLMESS,FMT='(''NO/NO MORE NEXT RECORD'', &
&    '' TO READ, UNIT'',I3)') KNUMER
!
  ELSEIF (KCODE.EQ.-24) THEN
    WRITE (UNIT=CLMESS,FMT='(''CHARAC. VARIABLE TOO SHORT '', &
&    ''FOR "'',A,''", UNIT'',I3)')                             &
&      CDACTI(1:ILACT2),KNUMER
!
  ELSEIF (KCODE.EQ.-25) THEN
    WRITE (UNIT=CLMESS,FMT='(''NEW RECORD NAME: "'',A, &
&    ''" ALREADY USED, UNIT'',I3)')                     &
&      CDACTI(1:ILACT2),KNUMER
!
  ELSEIF (KCODE.EQ.-26) THEN
    WRITE (UNIT=CLMESS,FMT='(''NO/NO MORE PREVIOUS RECORD '', &
&    '' TO READ, UNIT'',I3)') KNUMER
!
  ELSEIF (KCODE.EQ.-27) THEN
    WRITE (UNIT=CLMESS,                                &
&           FMT='(''INSUFFICIENT CONTIGUOUS SPACE WI'', &
&    ''THIN TABLES, UNIT'',I3)') KNUMER
!
  ELSEIF (KCODE.EQ.-28) THEN
!
    IF (KNUMER.EQ.LFI%JPNIL) THEN
      WRITE (UNIT=CLMESS,                              &
&             FMT='(''NEW DEFAULT MULTIPLY FACTOR EX'', &
&      ''CEEDS MAXIMUM ('',I3,'')'')') LFI%JPFACX
    ELSE
      WRITE (UNIT=CLMESS,FMT='(''SPECIFIED MULTIPLY FACTOR '',     &
&      ''EXCEEDS MAXIMUM ('',I3,''), UNIT'',I3)') LFI%JPFACX,KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-29) THEN
    WRITE (UNIT=CLMESS,FMT='(I3,'' ENTRIES,'',         &
&    '' NO MORE PLACE FOR MULTIPLY FACTOR, UNIT'',I3)') &
&    LFI%JPXUFM,KNUMER
!
  ELSEIF (KCODE.EQ.-30) THEN
    WRITE (UNIT=CLMESS,FMT='(''INVALID FORTRAN LOGICAL UNIT'', &
&           '' NUMBER:'',I8)') KNUMER
!
  ELSEIF (KCODE.EQ.-31) THEN
    WRITE (UNIT=CLMESS,FMT='(''LOGICAL UNIT NUMBER'',I3, &
&       '' HAS NO PREDEFINED MULTIPLY FACTOR'')') KNUMER
!
!                  For unexpected error codes...
!
  ELSEIF (KNUMER.EQ.LFI%JPNIL) THEN
    WRITE (UNIT=CLMESS,FMT='(''*UNKNOWN* GLOBAL ERROR CODE'', &
&                             I6)') KCODE
  ELSE
    WRITE (UNIT=CLMESS,FMT='(''*UNKNOWN* ERROR CODE'',I6, &
&           '' ON LOGICAL UNIT'',I3)') KCODE,KNUMER
  ENDIF
!
  ILMESA=INT (LEN (CLMESA), JPLIKB)
  ILMESU=ILMESA-1-2*INT (LEN (CLJOLI), JPLIKB)-ILNSPR-4
  CLMESA=CLJOLI//' '//CDNSPR(1:ILNSPR)//' - '// &
&         CLMESS(1:ILMESU)//CLJOLI
  WRITE (UNIT=LFI%NULOUT,FMT='(A)') CLMESA
!
!           If logical unit corresponds to a LFI logical unit
!     already opened, its name is printed.
!
  IF (IJL.NE.0) THEN
!
    IF (LFI%NLNOMF(IJL).LE.LFI%JPLFTX) THEN
      WRITE (UNIT=LFI%NULOUT,FMT='(A,/)') CLJOLI            &
&             //' NAME - APPEARENT BUT'                      &
&             //' COMPLETE - OF LFI LOGICAL UNIT CONCERNED:'
    ELSE
      WRITE (UNIT=CLMESS,FMT='(A,                          &
&             '' NAME - APPEARENT, AND TRUNCATED BY'',I4,   &
&       '' CARACTERES - OF LFI LOGICAL UNIT CONCERNED:'')') &
&      CLJOLI,LFI%NLNOMF(IJL)-LFI%JPLFTX
      WRITE (UNIT=LFI%NULOUT,FMT='(A,/)') TRIM (CLMESS)
    ENDIF
!
    INLIGN=(LFI%NLNOMF(IJL)-1)/LFI%JPLFIX
    IDECAL=0
!
    DO J=1,INLIGN
    WRITE (UNIT=LFI%NULOUT,FMT='(A)')                         &
&           LFI%CNOMFI(IJL)(IDECAL+1:IDECAL+LFI%JPLFIX)//'...'
    IDECAL=IDECAL+LFI%JPLFIX
    ENDDO
!
    IF (LFI%NLNOMF(IJL).LE.LFI%JPLFTX) THEN
      WRITE (UNIT=LFI%NULOUT,FMT='(A,/)')              &
&             LFI%CNOMFI(IJL)(IDECAL+1:LFI%NLNOMF(IJL))
    ELSE
      WRITE (UNIT=LFI%NULOUT,FMT='(A,/)')         &
&             LFI%CNOMFI(IJL)(IDECAL+1:LFI%JPLFTX) &
&             //'...'
    ENDIF
!
    IF (LFI%CNOMSY(IJL).NE.LFI%CNOMFI(IJL)) THEN
      WRITE (UNIT=LFI%NULOUT,FMT='(A,/)') CLJOLI//           &
& ' *SYSTEM* NAME (APPEARENT) OF LFI LOGICAL UNIT CONCERNED:'
      INLIGN=(LFI%NLNOMS(IJL)-1)/LFI%JPLFIX
      IDECAL=0
!
      DO J=1,INLIGN
      WRITE (UNIT=LFI%NULOUT,FMT='(A)')                         &
&             LFI%CNOMSY(IJL)(IDECAL+1:IDECAL+LFI%JPLFIX)//'...'
      IDECAL=IDECAL+LFI%JPLFIX
      ENDDO
!
      WRITE (UNIT=LFI%NULOUT,FMT='(A,/)')              &
&             LFI%CNOMSY(IJL)(IDECAL+1:LFI%NLNOMS(IJL))
    ENDIF
!
  ENDIF
!
  WRITE (UNIT=LFI%NULOUT,FMT='(A)') CLMESA
  IF (LDFATA.AND.KCODE.NE.0) THEN
!
!            Aborts program.
!
    CALL SDL_SRLABORT
  ENDIF
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('LFIENG_FORT',1,ZHOOK_HANDLE)

CONTAINS

#include "lficom2.ixm.h"

END SUBROUTINE LFIENG_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFIENG64                                       &
&           (KNUMER, KNIMES, KCODE, LDFATA, CDMESS, CDNSPR, &
&           CDACTI)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  KNIMES                                 ! IN   
INTEGER (KIND=JPLIKB)  KCODE                                  ! IN   
LOGICAL                LDFATA                                 ! IN   
CHARACTER (LEN=*)      CDMESS                                 ! IN   
CHARACTER (LEN=*)      CDNSPR                                 ! IN   
CHARACTER (LEN=*)      CDACTI                                 ! IN   

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIENG_FORT                                               &
&           (LFI, KNUMER, KNIMES, KCODE, LDFATA, CDMESS, CDNSPR, &
&           CDACTI)

END SUBROUTINE LFIENG64

SUBROUTINE LFIENG                                         &
&           (KNUMER, KNIMES, KCODE, LDFATA, CDMESS, CDNSPR, &
&           CDACTI)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKM)  KNIMES                                 ! IN   
INTEGER (KIND=JPLIKM)  KCODE                                  ! IN   
LOGICAL                LDFATA                                 ! IN   
CHARACTER (LEN=*)      CDMESS                                 ! IN   
CHARACTER (LEN=*)      CDNSPR                                 ! IN   
CHARACTER (LEN=*)      CDACTI                                 ! IN   

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIENG_MT                                                 &
&           (LFI, KNUMER, KNIMES, KCODE, LDFATA, CDMESS, CDNSPR, &
&           CDACTI)

END SUBROUTINE LFIENG

SUBROUTINE LFIENG_MT                                           &
&           (LFI, KNUMER, KNIMES, KCODE, LDFATA, CDMESS, CDNSPR, &
&           CDACTI)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKM)  KNIMES                                 ! IN   
INTEGER (KIND=JPLIKM)  KCODE                                  ! IN   
LOGICAL                LDFATA                                 ! IN   
CHARACTER (LEN=*)      CDMESS                                 ! IN   
CHARACTER (LEN=*)      CDNSPR                                 ! IN   
CHARACTER (LEN=*)      CDACTI                                 ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  INIMES                                 ! IN   
INTEGER (KIND=JPLIKB)  ICODE                                  ! IN   
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)
INIMES     = INT (    KNIMES, JPLIKB)
ICODE      = INT (     KCODE, JPLIKB)

CALL LFIENG_FORT                                               &
&           (LFI, INUMER, INIMES, ICODE, LDFATA, CDMESS, CDNSPR, &
&           CDACTI)


END SUBROUTINE LFIENG_MT

!INTF KNUMER        IN    
!INTF KNIMES        IN    
!INTF KCODE         IN    
!INTF LDFATA        IN    
!INTF CDMESS        IN    
!INTF CDNSPR        IN    
!INTF CDACTI        IN    
