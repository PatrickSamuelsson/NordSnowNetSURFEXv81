! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI

SUBROUTINE LFIIST_FORT                      &
&                     (LFI, KRANG, LDAPFE )
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        SOUS-PROGRAMME IMPRIMANT LES STATISTIQUES D'UTILISATION D'UNE
!     UNITE LOGIQUE TRAITEE PAR LE LOGICIEL DE FICHIERS INDEXES LFI.
!     CE SOUS-PROGRAMME EST ***A USAGE INTERNE*** DU LOGICIEL; CELUI
!     A APPELER PAR L'UTILISATEUR QUI DESIRE AVOIR CES STATISTIQUES
!     AUTREMENT QU'A LA FERMETURE DU FICHIER EST *LFISTA* .
!**
!    ARGUMENTS : KRANG  (ENTREE) ==> RANG ( DANS LA TABLE *LFI%NUMERO* )
!                                    DE L'UNITE LOGIQUE CONCERNEE;
!                LDAPFE (ENTREE) ==> VRAI SI L'APPEL EST FAIT LORS DE
!                                    LA FERMETURE (PAR *LFIFER*) .
!
!    Modifications:
!
!    02/06/97, Jean Clochard.
!
!              -Modification des impressions pour que l'annee puisse
!               etre imprimee avec 4 chiffres.
!
!
TYPE(LFICOM) :: LFI
CHARACTER CLOPER*(LFI%JPLSPX)
!
INTEGER (KIND=JPLIKB) IIDATE, IIHEUR
INTEGER (KIND=JPLIKB) KRANG, INUMER, IREP, IMDESC 
INTEGER (KIND=JPLIKB) IDATEM, IHEURM, IANNEE, IMOIS
INTEGER (KIND=JPLIKB) IJOUR, IHEURE, IMINUT, ISECON 
INTEGER (KIND=JPLIKB) IDECAL, IDERNI, INLNOM, J
INTEGER (KIND=JPLIKB) IDEROP, IDERCO, IDERAP, INBPIR 
INTEGER (KIND=JPLIKB) INBALO, IFACTM, ILARPH
INTEGER (KIND=JPLIKB) INALPP, ILONGF, ILDONN, INTRUA 
INTEGER (KIND=JPLIKB) INALDO, INPPIU, INUTIL
INTEGER (KIND=JPLIKB) INTRUO, ILOMIN, ILOMAX, IREESP 
INTEGER (KIND=JPLIKB) IREECO, IREELO, INALIP
INTEGER (KIND=JPLIKB) INAMAX, INMOUL, INMOUE, INMOUT 
INTEGER (KIND=JPLIKB) INMOLL, INMOLE, INMOLT
INTEGER (KIND=JPLIKB) INLECT, INECRI, INRENO, INSUPP 
INTEGER (KIND=JPLIKB) INIMES, INTPPI
!
LOGICAL LDAPFE
!
CHARACTER(LEN=LFI%JPLSPX) CLNSPR
CHARACTER(LEN=LFI%JPLMES) CLMESS
CHARACTER(LEN=LFI%JPLFTX) CLACTI
LOGICAL LLFATA

!**
!     1.  -  CONTROLES DES PARAMETRES D'APPEL.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('LFIIST_FORT',0,ZHOOK_HANDLE)
CLACTI=''
IF (KRANG.LE.0.OR.KRANG.GT.LFI%JPNXFI) THEN
  INUMER=LFI%JPNIL
ELSE
  INUMER=LFI%NUMERO(KRANG)
  IREP=0
ENDIF
!
IF (INUMER.EQ.LFI%JPNIL) THEN
  IREP=-14
  GOTO 1001
ENDIF
!**
!     2.  -  IMPRESSION DES STATISTIQUES.
!-----------------------------------------------------------------------
!
CALL LFIDAH_FORT                     &
&               (LFI, IIDATE,IIHEUR)
IDATEM=IIDATE
IHEURM=IIHEUR
IANNEE=IDATEM/10000
IJOUR =MOD (IDATEM,100_JPLIKB )
IMOIS =IDATEM/100-100*IANNEE
IHEURE=IHEURM/10000
ISECON=MOD (IHEURM,100_JPLIKB )
IMINUT=IHEURM/100-100*IHEURE
!
IF (LFI%LFRANC) THEN
  WRITE (UNIT=LFI%NULOUT,FMT=9010)INUMER,IANNEE,IMOIS,       &
&                                  IJOUR,IHEURE,IMINUT,ISECON
!
  IF (LDAPFE) THEN
    WRITE (UNIT=LFI%NULOUT,FMT=9020)
  ELSEIF (LFI%LNOUFI(KRANG)) THEN
    WRITE (UNIT=LFI%NULOUT,FMT=9025)
  ENDIF
!
ELSE
  WRITE (UNIT=LFI%NULOUT,FMT=9011)INUMER,IANNEE,IMOIS,       &
&                                  IJOUR,IHEURE,IMINUT,ISECON
!
  IF (LDAPFE) THEN
    WRITE (UNIT=LFI%NULOUT,FMT=9021)
  ELSEIF (LFI%LNOUFI(KRANG)) THEN
    WRITE (UNIT=LFI%NULOUT,FMT=9026)
  ENDIF
!
ENDIF
!
IF (.NOT.LDAPFE.OR.IXNIMS(KRANG).EQ.0) THEN
!
  IF (LFI%LFRANC) THEN
    CLMESS=' X NOM du Fichier='''
  ELSE
    CLMESS=' X File NAME='''
  ENDIF
!
  IDECAL=INT (INDEX (CLMESS,''''), JPLIKB)
  IDERNI=111
  INLNOM=MIN (LFI%NLNOMF(KRANG),LFI%JPLFIX,IDERNI-2-IDECAL)
  CLMESS(IDECAL+1:IDECAL+INLNOM+1)=   &
&    LFI%CNOMFI(KRANG)(1:INLNOM)//''''
  CLMESS(IDERNI:IDERNI)='X'
  WRITE (UNIT=LFI%NULOUT,FMT=9015) TRIM(CLMESS)
!
  IF (LFI%CNOMSY(KRANG).NE.LFI%CNOMFI(KRANG)) THEN
!
    IF (LFI%LFRANC) THEN
      CLMESS=' X NOM *SYSTEME*: '''
    ELSE
      CLMESS=' X SYSTEM NAME: '''
    ENDIF
!
    IDECAL=INT (INDEX (CLMESS,''''), JPLIKB)
    IDERNI=111
    INLNOM=MIN (LFI%NLNOMS(KRANG),LFI%JPLFIX,IDERNI-2-IDECAL)
    CLMESS(IDECAL+1:IDECAL+INLNOM+1)=       &
&          LFI%CNOMSY(KRANG)(1:INLNOM)//''''
    CLMESS(IDERNI:IDERNI)='X'
    WRITE (UNIT=LFI%NULOUT,FMT=9015) TRIM (CLMESS)
  ENDIF
!
ENDIF
!
IDEROP=LFI%NDEROP(KRANG)
!
IF (IDEROP.EQ.0) THEN
  CLOPER='LFIOUV'
ELSEIF (IDEROP.EQ.1) THEN
  CLOPER='LFIECR'
ELSEIF (IDEROP.EQ.2) THEN
  CLOPER='LFILEC'
ELSEIF (IDEROP.EQ.3) THEN
  CLOPER='LFIMST'
ELSEIF (IDEROP.EQ.4) THEN
  CLOPER='LFIERF'
ELSEIF (IDEROP.EQ.5) THEN
  CLOPER='LFINIM'
ELSEIF (IDEROP.EQ.6) THEN
  CLOPER='LFITAM'
ELSEIF (IDEROP.EQ.7) THEN
  CLOPER='LFINFO'
ELSEIF (IDEROP.EQ.8) THEN
  CLOPER='LFISTA'
ELSEIF (IDEROP.EQ.9) THEN
  CLOPER='LFIFER'
ELSEIF (IDEROP.EQ.10) THEN
  CLOPER='LFILAS'
ELSEIF (IDEROP.EQ.11) THEN
  CLOPER='LFICAS'
ELSEIF (IDEROP.EQ.12) THEN
  CLOPER='LFINAF'
ELSEIF (IDEROP.EQ.13) THEN
  CLOPER='LFIREN'
ELSEIF (IDEROP.EQ.14) THEN
  CLOPER='LFIPOS'
ELSEIF (IDEROP.EQ.15) THEN
  CLOPER='LFISUP'
ELSEIF (IDEROP.EQ.16) THEN
  CLOPER='LFILAP'
ELSEIF (IDEROP.EQ.17) THEN
  CLOPER='LFICAP'
ELSEIF (IDEROP.EQ.18) THEN
  CLOPER='LFILAF'
ELSEIF (IDEROP.EQ.19) THEN
  CLOPER='LFIOEF'
ELSEIF (IDEROP.EQ.20) THEN
  CLOPER='LFIOSF'
ELSEIF (IDEROP.EQ.21) THEN
  CLOPER='LFIOMF'
ELSEIF (IDEROP.EQ.22) THEN
  CLOPER='LFIPXF'
ELSE
  CLOPER=LFI%CHINCO(:LFI%JPLSPX)
ENDIF
!
IDERCO=LFI%NDERCO(KRANG)
IDERAP=LFI%MDES1D(IXM(LFI%JPNAPH,KRANG))
INBPIR=LFI%MDES1D(IXM(LFI%JPNPIR,KRANG))
INBALO=LFI%MDES1D(IXM(LFI%JPNALO,KRANG))
IFACTM=LFI%MFACTM(KRANG)
ILARPH=LFI%JPLARD*IFACTM
INALPP=LFI%JPNAPP*IFACTM
!
IF (LFI%LMODIF(KRANG).AND..NOT.LDAPFE) THEN
!
!         Il faut inclure, dans la longueur totale du fichier,
!     les eventuels articles physiques (de donnees, mais aussi d'index)
!     qui n'auraient pas encore ete ecrits.
!
  DO J=0,LFI%JPNPDF-1
  IDERAP=MAX (IDERAP,LFI%NUMAPD(J,KRANG))
  ENDDO
!
  INTPPI=(INBALO-1+INALPP)/INALPP
!
  IF (INTPPI.GT.INBPIR) THEN
    IMDESC=LFI%MDES1D(IXM(ILARPH+1-INTPPI+INBPIR,KRANG))
    IDERAP=MAX (IDERAP,IMDESC+1)
  ENDIF
!
ENDIF
!
ILONGF=ILARPH*IDERAP
ILDONN=LFI%MDES1D(IXM(LFI%JPLTAL,KRANG))
!
IF (LDAPFE) THEN
  INTRUA=LFI%MDES1D(IXM(LFI%JPNTRU,KRANG))
ELSE
  INTRUA=LFI%MDES1D(IXM(LFI%JPNTRU,KRANG))+LFI%NBTROU(KRANG)
ENDIF
!
INALDO=INBALO-INTRUA
!
!        NOMBRE DE P.P.I. "UTILES" (EN DECOMPTANT LES TROUS D'INDEX)
!
INPPIU=(INALDO-1+INALPP)/INALPP
INUTIL=ILONGF-ILDONN-ILARPH*(1+2*INPPIU)
INTRUO=LFI%NBTROU(KRANG)
ILOMIN=LFI%MDES1D(IXM(LFI%JPLNAL,KRANG))
ILOMAX=LFI%MDES1D(IXM(LFI%JPLXAL,KRANG))
!
IF (LFI%LFRANC) THEN
  WRITE (UNIT=LFI%NULOUT,FMT=9030)CLOPER,IDERCO
  WRITE (UNIT=LFI%NULOUT,FMT=9040)ILONGF,ILDONN,ILARPH,INUTIL
  WRITE (UNIT=LFI%NULOUT,FMT=9050)INTRUA,INTRUO
!
  IF (LDAPFE.OR..NOT.LFI%LMIMAL(KRANG)) THEN
    WRITE (UNIT=LFI%NULOUT,FMT=9060)INALDO,ILOMIN,ILOMAX
  ELSE
    WRITE (UNIT=LFI%NULOUT,FMT=9070)INALDO,ILOMIN,ILOMAX
  ENDIF
!
ELSE
  WRITE (UNIT=LFI%NULOUT,FMT=9031)CLOPER,IDERCO
  WRITE (UNIT=LFI%NULOUT,FMT=9041)ILONGF,ILDONN,ILARPH,INUTIL
  WRITE (UNIT=LFI%NULOUT,FMT=9051)INTRUA,INTRUO
!
  IF (LDAPFE.OR..NOT.LFI%LMIMAL(KRANG)) THEN
    WRITE (UNIT=LFI%NULOUT,FMT=9061)INALDO,ILOMIN,ILOMAX
  ELSE
    WRITE (UNIT=LFI%NULOUT,FMT=9071)INALDO,ILOMIN,ILOMAX
  ENDIF
!
ENDIF
!
IF (LDAPFE) THEN
  IREESP=LFI%MDES1D(IXM(LFI%JPNRES,KRANG))-LFI%NREESP(KRANG)
  IREECO=LFI%MDES1D(IXM(LFI%JPNREC,KRANG))-LFI%NREECO(KRANG)
  IREELO=LFI%MDES1D(IXM(LFI%JPNREL,KRANG))-LFI%NREELO(KRANG)
ELSE
  IREESP=LFI%MDES1D(IXM(LFI%JPNRES,KRANG))
  IREECO=LFI%MDES1D(IXM(LFI%JPNREC,KRANG))
  IREELO=LFI%MDES1D(IXM(LFI%JPNREC,KRANG))
ENDIF
!
IDATEM=LFI%MDES1D(IXM(LFI%JPDCRE,KRANG))
IHEURM=LFI%MDES1D(IXM(LFI%JPHCRE,KRANG))
IANNEE=IDATEM/10000
IJOUR =MOD (IDATEM,100_JPLIKB )
IMOIS =IDATEM/100-100*IANNEE
IHEURE=IHEURM/10000
ISECON=MOD (IHEURM,100_JPLIKB )
IMINUT=IHEURM/100-100*IHEURE
!
IF (LFI%LFRANC) THEN
  IF (.NOT.LFI%LNOUFI(KRANG))                                      &
&     WRITE (UNIT=LFI%NULOUT,FMT=9080)' AVANT',IREESP,IREECO,IREELO
  WRITE (UNIT=LFI%NULOUT,FMT=9080)'DEPUIS',LFI%NREESP(KRANG), &
&             LFI%NREECO(KRANG),LFI%NREELO(KRANG)
  WRITE (UNIT=LFI%NULOUT,FMT=9090)                  &
&        'CREATION du FICHIER (Premiere Ouverture)', &
&        IANNEE,IMOIS,IJOUR,IHEURE,IMINUT,ISECON
ELSE
  IF (.NOT.LFI%LNOUFI(KRANG))                                      &
&     WRITE (UNIT=LFI%NULOUT,FMT=9081)'BEFORE',IREESP,IREECO,IREELO
  WRITE (UNIT=LFI%NULOUT,FMT=9081)' SINCE',LFI%NREESP(KRANG), &
&         LFI%NREECO(KRANG), LFI%NREELO(KRANG)
  WRITE (UNIT=LFI%NULOUT,FMT=9091)               &
&         'FILE CREATION (Very First Opening)  ', &
&         IANNEE,IMOIS,IJOUR,IHEURE,IMINUT,ISECON
ENDIF
!
IF (.NOT.LFI%LNOUFI(KRANG).OR.LFI%LMODIF(KRANG)) THEN
  IDATEM=LFI%MDES1D(IXM(LFI%JPDMNG,KRANG))
  IHEURM=LFI%MDES1D(IXM(LFI%JPHMNG,KRANG))
  IANNEE=IDATEM/10000
  IJOUR =MOD (IDATEM,100_JPLIKB )
  IMOIS =IDATEM/100-100*IANNEE
  IHEURE=IHEURM/10000
  ISECON=MOD (IHEURM,100_JPLIKB )
  IMINUT=IHEURM/100-100*IHEURE
!
  IF (LFI%LFRANC) THEN
    WRITE (UNIT=LFI%NULOUT,FMT=9090)                       &
&               'Premiere Modification "NON GARANTIE"    ', &
&               IANNEE,IMOIS,IJOUR,IHEURE,IMINUT,ISECON
  ELSE
    WRITE (UNIT=LFI%NULOUT,FMT=9091)                    &
&               'First "not guaranteed" Modification  ', &
&               IANNEE,IMOIS,IJOUR,IHEURE,IMINUT,ISECON
  ENDIF
!
ENDIF
!
IF (LDAPFE.OR..NOT.LFI%LNOUFI(KRANG)) THEN
  IDATEM=LFI%MDES1D(IXM(LFI%JPDDMG,KRANG))
  IHEURM=LFI%MDES1D(IXM(LFI%JPHDMG,KRANG))
  IANNEE=IDATEM/10000
  IJOUR =MOD (IDATEM,100_JPLIKB )
  IMOIS =IDATEM/100-100*IANNEE
  IHEURE=IHEURM/10000
  ISECON=MOD (IHEURM,100_JPLIKB )
  IMINUT=IHEURM/100-100*IHEURE
!
  IF (LFI%LFRANC) THEN
    WRITE (UNIT=LFI%NULOUT,FMT=9090)                       &
&               'Derniere FERMETURE apres Modification   ', &
&               IANNEE,IMOIS,IJOUR,IHEURE,IMINUT,ISECON
  ELSE
    WRITE (UNIT=LFI%NULOUT,FMT=9091)                   &
&               'Last CLOSE made after a Modification', &
&               IANNEE,IMOIS,IJOUR,IHEURE,IMINUT,ISECON
  ENDIF
!
ENDIF
!
INALIP=INALPP*INBPIR
INAMAX=INALIP+INALPP*(ILARPH-LFI%JPLDOC)
INMOUL=LFI%NBMOLU(KRANG)
INMOUE=LFI%NBMOEC(KRANG)
INMOUT=INMOUL+INMOUE
INMOLL=ILARPH*LFI%NBREAD(KRANG)
INMOLE=ILARPH*LFI%NBWRIT(KRANG)
INMOLT=INMOLL+INMOLE
INLECT=LFI%NBLECT(KRANG)
INECRI=LFI%NBNECR(KRANG)+LFI%NREESP(KRANG)+ &
&       LFI%NREECO(KRANG)+LFI%NREELO(KRANG)
INRENO=LFI%NBRENO(KRANG)
INSUPP=LFI%NBSUPP(KRANG)
!
IF (LFI%LFRANC) THEN
  WRITE (UNIT=LFI%NULOUT,FMT=9100)INALIP,INAMAX
  WRITE (UNIT=LFI%NULOUT,FMT=9110)                     &
&             'UTILISATEUR','EN LECTURE','EN ECRITURE', &
&             INMOUL,INMOUE,INMOUT
  WRITE (UNIT=LFI%NULOUT,FMT=9110)                     &
&             ' LOGICIEL  ','PAR "READ"','PAR "WRITE"', &
&             INMOLL,INMOLE,INMOLT
  WRITE (UNIT=LFI%NULOUT,FMT=9120)INLECT,INECRI,INRENO,INSUPP
ELSE
  WRITE (UNIT=LFI%NULOUT,FMT=9101)INALIP,INAMAX
  WRITE (UNIT=LFI%NULOUT,FMT=9111)                      &
&             '  USER  ',' For INPUT  ',' For OUTPUT  ', &
&             INMOUL,INMOUE,INMOUT
  WRITE (UNIT=LFI%NULOUT,FMT=9111)                      &
&             'SOFTWARE','Through READ','Through WRITE', &
&             INMOLL,INMOLE,INMOLT
  WRITE (UNIT=LFI%NULOUT,FMT=9121)INLECT,INECRI,INRENO,INSUPP
ENDIF
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE INTERNE EVENTUELLE,
!            VIA LE SOUS-PROGRAMME "LFIEMS", ET ABORT EVENTUEL.
!-----------------------------------------------------------------------
!
1001 CONTINUE
LLFATA=LLMOER (IREP,KRANG)
!
IF (LFI%LMISOP.OR.LLFATA) THEN
  INIMES=2
  CLNSPR='LFIIST'
  WRITE (UNIT=CLMESS,FMT='(''IREP='',I4,'', KRANG='',I3, &
&         '', LDAPFE= '',L1)') IREP,KRANG,LDAPFE
  CALL LFIEMS_FORT                                 &
&                 (LFI, INUMER,INIMES,IREP,LLFATA, &
&                  CLMESS,CLNSPR,CLACTI)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('LFIIST_FORT',1,ZHOOK_HANDLE)
RETURN
!
 9010 FORMAT (/,/,T2,110('X'),/,' X',T111,'X',/,' X',T15,            &
& 'Statistiques d''Utilisation de l''Unite Logique Indexee',I4, &
& ' (le',I5.2,2('/',I2.2),' a',I3.2,2(':',I2.2),')',T111,'X',   &
& /,' X',T15,42('- '),'-',T111,'X',/,' X',T111,'X')
!
 9011 FORMAT (/,/,T2,110('X'),/,' X',T111,'X',/,' X',T15,           &
& 'Statistics of Use for LFI Indexed Logical Unit Number',I4,  &
& ' (on',I5.2,2('/',I2.2),' at',I3.2,2(':',I2.2),')',T111,'X', &
& /,' X',T15,42('- '),'-',T111,'X',/,' X',T111,'X')
!
 9015 FORMAT (A,/,' X',T111,'X')
!
 9020 FORMAT (' X',T29,                                             &
& '----- Cette Unite Logique est en cours de FERMETURE -----', &
& T111,'X',/,' X',T111,'X')
!
 9021 FORMAT (' X',T30,                                           &
& '----- This Logical Unit is currently being CLOSED -----', &
& T111,'X',/,' X',T111,'X')
!
 9025 FORMAT (' X',T30,                                          &
& '----- Cette Unite Logique est en mode "CREATION" -----', &
& T111,'X',/,' X',T111,'X')
!
 9026 FORMAT (' X',T31,                                       &
& '----- This Logical Unit is in "CREATION" Mode -----', &
& T111,'X',/,' X',T111,'X')
!
 9030 FORMAT (' X  Dernier Sous-Programme utilise: "',A6, &
& '", Code-Reponse correspondant a cet appel:',I4,   &
& T111,'X',/,' X',T111,'X')
!
 9031 FORMAT (' X  Name of Last Used SUBROUTINE: "',A6,   &
& '", Response Code corresponding to this call:',I4, &
& T111,'X',/,' X',T111,'X')
!
 9040 FORMAT (' X  LONGUEUR TOTALE en mots: du FICHIER=',I9,           &
& ', des DONNEES=',I9,', d''un Article "PHYSIQUE"=',I6,T111,'X',  &
& /,' X',T111,'X',/,' X  Mots "PERDUS" par l''Utili',             &
& 'sation (Articles d''Index inutiles, Trous, Fin du fichier) =', &
& I8,T111,'X',/,' X',T111,'X')
!
 9041 FORMAT (' X  TOTAL LENGTH in words: of FILE=',I9,             &
& ', of DATA=',I9,', of a "PHYSICAL" Record=',I6,T111,'X',     &
& /,' X',T111,'X',/,' X  Words currently "LOST" by previous ', &
& 'Usage (Unnecessary Index Records, Holes, End of file)=',    &
& I8,T111,'X',/,' X',T111,'X')
!
 9050 FORMAT (' X  Nombre de TROUS REPERTORIES: Actuellement',I6, &
& ', dont',I6,' CREE(S) et NON RECYCLE(S) depuis OUVERTURE', &
& T111,'X',/,' X',T111,'X')
!
 9051 FORMAT (' X  Number of HOLES within INDEX: Currently',I6,     &
& ', among which',I6,' CREATED and NOT RE-USED since OPENING', &
& T111,'X',/,' X',T111,'X')
!
 9060 FORMAT (' X  Nombre d''ARTICLES LOGIQUES de DONNEES=',I6,     &
& ', Longueur Mini/Maxi=',I7,' /',I7,T111,'X',/,' X',T111,'X')
!
 9061 FORMAT (' X  Number of LOGICAL RECORDS of DATA=',I6,              &
& ', Minimum/maximum length=',I7,' /',I7,T111,'X',/,' X',T111,'X')
!
 9070 FORMAT (' X  Nombre d''ARTICLES LOGIQUES de DONNEES=',I6, &
& ', Longueur Mini/Maxi (A PRIORI)=',I7,' /',I7,           &
& T111,'X',/,' X',T111,'X')
!
 9071 FORMAT (' X  Number of LOGICAL RECORDS of DATA=',I6, &
& ', Minimum/Maximum Length (A PRIORI)=',I7,' /',I7,  &
& T111,'X',/,' X',T111,'X')
!
 9080 FORMAT (' X  ',A6,' Ouverture, Nombre de REECRITURES Sur PLACE/', &
& 'Plus COURTES/Plus LONGUES=',I6,2(' /',I6),                      &
& T111,'X',/,' X',T111,'X')
!
 9081 FORMAT (' X  ',A6,' Opening, Number of RE-WRITE(s): In PLACE/', &
& 'SHORTER/LONGER=',I6,2(' /',I6),                               &
& T111,'X',/,' X',T111,'X')
!
 9090 FORMAT (' X  ',A40,' le',I5.2,2('/',I2.2),' a',I3.2,2(':',I2.2), &
& T111,'X',/,' X',T111,'X')
!
 9091 FORMAT (' X  ',A36,' on',I5.2,2('/',I2.2),' at',I3.2,2(':',I2.2), &
& T111,'X',/,' X',T111,'X')
!
 9100 FORMAT(' X  Nombre d''ARTICLES LOGIQUES gerables (TROUS compris)', &
&       ' SANS/AVEC DEBORDEMENT =',I7,' /',I7,                      &
& T111,'X',/,' X',T111,'X')
!
 9101 FORMAT(' X  Number of LOGICAL RECORDS STORE-able (HOLES included)' &
&       ,' WITHOUT/WITH OVERFLOW=',I7,' /',I7,                      &
& T111,'X',/,' X',T111,'X')
!
 9110 FORMAT (' X  Nombre de Mots ',A11,' demandes ',A10,'/',A11,  &
&        '/AU TOTAL=',I9,2(' /',I9),T111,'X',/,' X',T111,'X')
!
 9111 FORMAT (' X  Number of ',A8,' Words requested ',A12,'/',A13, &
&        '/TOTAL=',I9,2(' /',I9),T111,'X',/,' X',T111,'X')
!
 9120 FORMAT (' X  Nombre d''ARTICLES LOGIQUES LUS/ECRITS/RENOMMES/', &
&        'SUPPRIMES depuis Ouverture=',I6,3(' /',I6),            &
& T111,'X',/,' X',T111,'X',/,T2,110('X'),/)
!
 9121 FORMAT (' X  Number of LOGICAL RECORDS: READ/WRITTEN/RENAMED/', &
&        'SUPPRESSED since Opening = ',I6,3(' /',I6),            &
& T111,'X',/,' X',T111,'X',/,T2,110('X'),/)
!

CONTAINS

#include "lficom2.ixm.h"
#include "lficom2.ixnims.h"
#include "lficom2.llmoer.h"

END SUBROUTINE LFIIST_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFIIST64           &
&           (KRANG, LDAPFE)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KRANG                                  ! IN   
LOGICAL                LDAPFE                                 ! IN   

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIIST_FORT               &
&           (LFI, KRANG, LDAPFE)

END SUBROUTINE LFIIST64

SUBROUTINE LFIIST             &
&           (KRANG, LDAPFE)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KRANG                                  ! IN   
LOGICAL                LDAPFE                                 ! IN   

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIIST_MT                 &
&           (LFI, KRANG, LDAPFE)

END SUBROUTINE LFIIST

SUBROUTINE LFIIST_MT             &
&           (LFI, KRANG, LDAPFE)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
INTEGER (KIND=JPLIKM)  KRANG                                  ! IN   
LOGICAL                LDAPFE                                 ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IRANG                                  ! IN   
! Convert arguments

IRANG      = INT (     KRANG, JPLIKB)

CALL LFIIST_FORT               &
&           (LFI, IRANG, LDAPFE)


END SUBROUTINE LFIIST_MT

!INTF KRANG         IN    
!INTF LDAPFE        IN    
