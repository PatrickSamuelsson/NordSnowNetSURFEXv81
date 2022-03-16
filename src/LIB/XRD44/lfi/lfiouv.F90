! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI
! Sep-2012 P. Marguinaud Fix uninitialized variables

SUBROUTINE LFIOUV_FORT                                     &
&                     (LFI, KREP, KNUMER, LDNOMM, CDNOMF,  &
&                      CDSTTO, LDERFA,                     &
&                      LDIMST, KNIMES, KNBARP, KNBARI )
USE LFIMOD, ONLY : LFICOM, LFICRW
USE PARKIND1, ONLY : JPRB, JPIA, JPIM, JPIB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        SOUS-PROGRAMME D'OUVERTURE D'UNE UNITE LOGIQUE DEVANT ETRE
!     TRAITEE COMME UN FICHIER INDEXE, PAR LE LOGICIEL LFI.
!**
!     ARGUMENTS : KREP   (SORTIE) ==> CODE-REPONSE DU SOUS-PROGRAMME;
!                 KNUMER (ENTREE) ==> LFI%NUMERO DE L'UNITE LOGIQUE;
!                 LDNOMM (ENTREE) ==> VRAI SI L'UNITE LOGIQUE DOIT ETRE
!                                     ASSOCIEE A UN NOM DE FICHIER EXP-
!                                     LICITE LORS DE L'"OPEN" FORTRAN;
!                 CDNOMF (ENTREE) ==> NOM DE FICHIER EXPLICITE, SI
!                                     *LDNOMM* EST VRAI - MEME SI CE
!                                     N'EST PAS LE CAS, CE *DOIT* ETRE
!                                     UN OBJET DE TYPE "CHARACTER" .
!                 CDSTTO (ENTREE) ==> "STATUS" POUR L'"OPEN" FORTRAN
!                                     ('OLD','NEW','UNKNOWN','SCRATCH')
!                                     PAR DEFAUT, METTRE 'UNKNOWN';
!                 LDERFA (ENTREE) ==> OPTION D'ERREUR FATALE;
!                 LDIMST (ENTREE) ==> OPTION IMPRESSION DE STATISTIQUES
!                                     AU MOMENT DE LA FERMETURE;
!                 KNIMES (ENTREE) ==> NIVEAU DE LA MESSAGERIE (0,1 OU 2)
!                                     ( 0==>RIEN, 2==>TOUT )
!                 KNBARP (ENTREE) ==> NOMBRE D'ARTICLES LOGIQUES PREVUS,
!                                     CE QUI N'EST UTILISE QUE LORS DE
!                                     LA CREATION DU FICHIER,
!                                     ET QUI N'EMPECHE QUAND MEME PAS
!                                     D'AVOIR PLUS D'ARTICLES LOGIQUES;
!                 KNBARI (SORTIE) ==> NOMBRE D'ARTICLES LOGIQUES DE DON-
!                                     NEES SUR LE FICHIER, INITIALEMENT.
!                                     (ZERO SI CREATION)
CHARACTER CPNOMD*(*)
PARAMETER ( CPNOMD='%%%%% FICHIER SANS NOM %%%%%' )
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
INTEGER (KIND=JPLIKB) KREP, KNUMER, KNIMES, KNBARP, KNBARI
INTEGER (KIND=JPLIKB) IDATE, IHEURE
INTEGER (KIND=JPLIKB) ILSTTU, IREPX, IRANG, IRANMS, INBARI 
INTEGER (KIND=JPLIKB) IDECBL, IPOSBL, J
INTEGER (KIND=JPLIKB) ILNOMF, INLNOM, INIMES, IREP, ILNOMS 
INTEGER (KIND=JPLIKB) IFACTM, ILSTTO, IJ
INTEGER (KIND=JPLIKB) IRANFM, ILACTI, ICOMPT, ITAILS, ICRITS
INTEGER (KIND=JPLIKB) IPOFIN, ICRITG
INTEGER (KIND=JPLIKB) ICRITD, ICRITR, IPOSCA, INREAD, INWRIT
INTEGER (KIND=JPLIKB) IBASE, ILOREC
INTEGER (KIND=JPLIKB) INAPHY, JREC, ILARPH, INALPP, IFACPH 
INTEGER (KIND=JPLIKB) IFACPP, INBPIR
INTEGER (KIND=JPLIKB) IRANGD, IREC, INBALO, ILUTIL, IRGPIF 
INTEGER (KIND=JPLIKB) IRETIN
TYPE (LFICRW)         YLFIC
INTEGER (KIND=JPIB)   IISLEN
!
LOGICAL LDNOMM, LDERFA, LDIMST, LLEXFI, LLNOUF, LLNOMS
LOGICAL LLVERG, LLEXUL
LOGICAL LLISLE
LOGICAL LLRQLE
!
CHARACTER CDNOMF*(*), CDSTTO*(*)
CHARACTER*(LFI%JPLSTX) CLSTTO
CHARACTER*(LFI%JPLFTX) CLNOMF, CLNOMS

!
CHARACTER(LEN=LFI%JPLSPX) CLNSPR
CHARACTER(LEN=LFI%JPLMES) CLMESS
CHARACTER(LEN=LFI%JPLFTX) CLACTI
CHARACTER(LEN=32)         CLENDI
LOGICAL LLFATA

!
!     1.  -  CONTROLES DIVERS, ET INITIALISATIONS.
!-----------------------------------------------------------------------
!*
!     1.0 - PARTIE "ELEMENTAIRE".
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('LFIOUV_FORT',0,ZHOOK_HANDLE)
CLACTI=''
ILSTTU=MIN (INT (LEN (CLSTTO), JPLIKB),  &
&            INT (LEN (CDSTTO), JPLIKB))
IREPX=0
IRANG=0
IRANMS=0
INBARI=0
LLVERG=.FALSE.
LLISLE=.FALSE.
LLRQLE=.FALSE.
YLFIC%L_C_BTSWAP=.FALSE.
!
CLENDI=''
CALL GET_ENVIRONMENT_VARIABLE ("LFI_BYTE_ORDER", CLENDI)
CALL ISWAP_ISLE (IISLEN)
LLISLE=IISLEN.NE.0      
SELECT CASE (CLENDI)
  CASE ('LITTLE_ENDIAN')
    LLRQLE=.TRUE.
  CASE ('BIG_ENDIAN')
    LLRQLE=.FALSE.
  CASE ('NATIVE_ENDIAN')
    LLRQLE=LLISLE
  CASE DEFAULT
    LLRQLE=.FALSE.
END SELECT
!
YLFIC%L_C_BTSWAP=(LLISLE.AND.(KNUMER<0)).NEQV.LLRQLE
!
YLFIC%N_C_FPDESC=0
YLFIC%N_C_OFFSET=0
!
!        Appel legerement anticipe a LFINUM, permettant une initialisa-
!     tion des variables globales du logiciel a la 1ere utilisation.
!
CALL LFINUM_FORT                    &
&               (LFI, KNUMER, IRANG)
!        Si KNUMER est nul, alors le numero d'unite logique est
!     attribué automatiquement
IF (KNUMER == 0) THEN
  CALL LFIUTO_FORT    (LFI, KNUMER)
  IRANG=0
ENDIF
!
IF (LDNOMM) THEN
!
!        Recherche de la longueur "utile" du nom de fichier specifie.
!        (c'est-a-dire sans tenir compte des blancs terminaux eventuels)
!
  IDECBL=0
!
101 CONTINUE
  IPOSBL=IDECBL+INT (INDEX (CDNOMF(IDECBL+1:),' '), JPLIKB)
!
  IF (IPOSBL.LE.IDECBL) THEN
    ILNOMF=INT (LEN (CDNOMF), JPLIKB)
  ELSEIF (CDNOMF(IPOSBL:).EQ.' ') THEN
    ILNOMF=MAX (IPOSBL-1,1_JPLIKB )
  ELSE
    IDECBL=IPOSBL
    GOTO 101
  ENDIF
!
  IF (ILNOMF.GT.LFI%JPLFTX) THEN
    INLNOM=LFI%JPLFTX
    INIMES=LFI%NIMESG
!
    IF (INIMES.GE.1) THEN
!
!        Message preventif, car le controle de non ouverture d'un meme
!     fichier via deux unites logiques differentes risque de "sauter"
!     artificiellement... et pas forcement a cet appel.
!
!        Le code-reponse ci-dessous est bidon, mais permet de mettre
!     en relief le message via LFIEMS.
!
      IREP=LFI%JPNIL
      CLNSPR='LFIOUV'
!
      IF (LFI%LFRANC) THEN
        WRITE (UNIT=CLMESS,FMT=                              &
&               '(''ATTENTION: NOM DE FICHIER TRONQUE A'',I4, &
&                 '' CARACTERES...'')') LFI%JPLFTX
      ELSE
        WRITE (UNIT=CLMESS,FMT=                               &
&               '(''WARNING: FILE NAME TRUNCATED TO ONLY'',I4, &
&                 '' CHARACTERS...'')') LFI%JPLFTX
      ENDIF
!
      CALL LFIEMS_FORT                                  &
&                     (LFI, KNUMER,INIMES,IREP,.FALSE., &
&                      CLMESS,CLNSPR,                   &
&                      CLACTI)
    ENDIF
!
  ELSE
    INLNOM=ILNOMF
  ENDIF
!
  CLNOMF=CDNOMF(:INLNOM)
ELSE
  ILNOMF=INT (LEN (CPNOMD), JPLIKB)
  CLNOMF=CPNOMD
  INLNOM=ILNOMF
ENDIF
!
!       Ci-dessous, initialisations destinees a forcer l'impression
!     du nom du fichier en cas de problemes.
!
CLNOMS=CLNOMF
ILNOMS=INLNOM
IFACTM=0
!
!        Controle de validite FORTRAN du Numero d'Unite Logique.
!
IF (KNUMER > 0) THEN
  INQUIRE (UNIT=KNUMER,EXIST=LLEXUL,ERR=901,IOSTAT=IREP)
ELSE
  LLEXUL=.TRUE.
ENDIF
!
IF (.NOT.LLEXUL) THEN
  IREP=-30
  GOTO 1001
ENDIF
!
!        CONTROLE DE L'ARGUMENT D'APPEL "KNIMES"
!
IF (KNIMES.LT.0.OR.KNIMES.GT.2) THEN
  IREP=-2
  GOTO 1001
ENDIF
!
!        CONTROLE DE L'ARGUMENT D'APPEL "CDSTTO"
!
ILSTTO=INT (INDEX (CDSTTO,' '), JPLIKB)-1
DO J=1,LFI%JPNBST
IF (CDSTTO(1:ILSTTO).EQ.LFI%LFIOUV_CLSTEX(J)(1:ILSTTO)) GOTO 104
ENDDO
!
ILACTI=MIN (INT (LEN (CDSTTO), JPLIKB),INT (LEN (CLACTI), JPLIKB))
CLACTI=CDSTTO(:ILACTI)
IREP=-7
GOTO 1001
!
104 CONTINUE
IF (ILSTTO.GT.0) ILSTTU=ILSTTO
CLSTTO=CDSTTO(:ILSTTU)
!
!               CONTROLE DE NON-OUVERTURE PREALABLE.
!
IF (IRANG.NE.0) THEN
  IREP=-5
  GOTO 1001
ENDIF
!
IF (LFI%LMULTI) CALL LFIVER_FORT                       &
&                               (LFI, LFI%VERGLA,'ON')
LLVERG=LFI%LMULTI
!
!        Recherche d'un eventuel facteur multiplicatif predefini pour
!     l'unite logique en question.
!
CALL LFIFMP_FORT                     &
&               (LFI, KNUMER,IRANFM)
IFACTM=LFI%MFACTU(IRANFM)
!
IF (LDNOMM) THEN
!
!        SI LE FICHIER EST NOMME, ON VERIFIE QU'IL N'A PAS ETE
!        DEJA OUVERT POUR UNE AUTRE UNITE LOGIQUE.
!
  DO J=1,LFI%NBFIOU
  IJ=LFI%NUMIND(J)
!
  IF (CLNOMF.EQ.LFI%CNOMFI(IJ)(:MIN (LFI%JPLFTX,LFI%NLNOMF(IJ)))) &
&  THEN
    ILACTI=MIN(INT (LEN (CLNOMF), JPLIKB), &
&               INT (LEN (CLACTI), JPLIKB))
    CLACTI=CLNOMF(:ILACTI)
    IRANMS=IJ
    IREP=-13
    GOTO 1001
  ENDIF
!
  ENDDO
!
ENDIF
!
110 CONTINUE
!*
!     1.1 - RECHERCHE D'UN EMPLACEMENT DISPONIBLE DANS LA TABLE DES
!           NUMEROS D'UNITES LOGIQUES *LFI%NUMERO* .
!           (Il faut IFACTM emplacements CONSECUTIFS)
!-----------------------------------------------------------------------
!
IF ((LFI%NFACTM+IFACTM).GT.LFI%JPNXFI) THEN
  IREP=-6
  GOTO 1001
ENDIF
!
ICOMPT=0
ITAILS=LFI%JPNXFI+1
ICRITS=0
!
DO J=1,LFI%JPNXFI
!
IF (LFI%NUMERO(J).EQ.LFI%JPNIL) THEN
  ICOMPT=ICOMPT+1
  IF (J.NE.LFI%JPNXFI.OR.ICOMPT.LT.IFACTM.OR.ICOMPT.GT.ITAILS) &
&    CYCLE
  IPOFIN=LFI%JPNXFI
ELSEIF (ICOMPT.LT.IFACTM.OR.ICOMPT.GT.ITAILS) THEN
  ICOMPT=0
!
  IF ((LFI%JPNXFI-J).LT.IFACTM) THEN
    GOTO 112
  ELSE
    CYCLE
  ENDIF
!
ELSE
  IPOFIN=J-1
ENDIF
!
!       Les lignes qui suivent sont atteintes si on a trouve un espace
!     contigu suffisant dans la table LFI%NUMERO, et de taille inferieure
!     ou egale a ce qu'on aurait pu trouver precedemment.
!       On calcule alors un critere de cadrage (a gauche ou a droite)
!     dans cet espace, en privilegiant une occupation decentree.
!
ICRITG=ABS (LFI%JPNXFI+1-2*(IPOFIN-ICOMPT+1))
ICRITD=ABS (LFI%JPNXFI+1-2*IPOFIN)
!
IF (ICRITG.GE.ICRITD) THEN
  ICRITR=ICRITG
  IPOSCA=IPOFIN-ICOMPT+1
ELSE
  ICRITR=ICRITD
  IPOSCA=IPOFIN-IFACTM+1
ENDIF
!
!       On retient l'espace trouve s'il est plus petit que ce qu'on
!     avait pu trouver precedemment, ou en cas d'egalite de taille
!     s'il est plus decentre.
!
IF (ICOMPT.LT.ITAILS.OR.ICRITR.GT.ICRITS) THEN
  ITAILS=ICOMPT
  IRANG=IPOSCA
  ICRITS=ICRITR
ENDIF
!
ICOMPT=0
IF ((LFI%JPNXFI-J).LT.IFACTM) GOTO 112
!
ENDDO   
!
112 CONTINUE
!
IF (ITAILS.GT.LFI%JPNXFI) THEN
!
!         On n'a pas trouve d'espace ad hoc.
!
  IF (IFACTM.GT.1) THEN
    IREP=-27
  ELSE
    IREP=-16
  ENDIF
!
  GOTO 1001
!
ENDIF
!
IRANMS=IRANG
IF (LFI%LMISOP) WRITE (UNIT=LFI%NULOUT,FMT=*)           &
&  '====> LFIOUV - IRANG = ',IRANG, ', IFACTM = ',IFACTM
LFI%LERFAT(IRANG)=LDERFA
LFI%NIVMES(IRANG)=KNIMES
INREAD=0
INWRIT=0
!
!        CETTE INITIALISATION QUI PEUT PARAITRE BIEN COMPLIQUEE SERT
!     DE PARADE AU MAUVAIS COMPORTEMENT DU "READ" SUR UN FICHIER VIDE,
!     sur CRAY-2 sous UNICOS 4.0 et 5.0... ( Debut )
!
CALL LFIDAH_FORT                    &
&               (LFI, IDATE,IHEURE)
IBASE=IHEURE+LFI%JPNIL
!
DO J=1,LFI%JPLDOC
LFI%MDES1D(IXM(J,IRANG))=IBASE-J
ENDDO
!**
!     2.  -  OUVERTURE DU FICHIER AU SENS FORTRAN DU TERME (*OPEN*).
!-----------------------------------------------------------------------
!
ILOREC=LFI%JPRECL*IFACTM
!
IF (LDNOMM) THEN
!*
!     2.1 - CAS OU L'UNITE LOGIQUE DOIT ETRE ASSOCIEE A UN FICHIER
!           DONT LE NOM EST EXPLICITEMENT DONNE.
!-----------------------------------------------------------------------
!
  INQUIRE (FILE=CDNOMF,EXIST=LLEXFI,IOSTAT=IREP,ERR=901)
!
  IF (LLEXFI.AND.CLSTTO.EQ.'NEW'                &
&      .OR..NOT.LLEXFI.AND.CLSTTO.EQ.'OLD') THEN
    CLACTI=CLSTTO
    IREP=-9
    IRANG=0
    IRANMS=0
    GOTO 1001
  ENDIF
!
  LLNOUF=CLSTTO.EQ.'NEW'.OR.CLSTTO.EQ.'SCRATCH'.OR..NOT.LLEXFI
!
!     APRES TOUS CES CONTROLES DE BASE, ON TENTE L'"OPEN" DU FICHIER .
!
  IF (KNUMER < 0) THEN
    CALL OPENC (CLNOMS, LLNOMS, IREP)
    IF (IREP /= 0) GOTO 902
  ELSE
    OPEN (UNIT=KNUMER,FILE=CDNOMF,STATUS=CLSTTO,                 &
&         ERR=902,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=ILOREC, &
&         IOSTAT=IREP)
  ENDIF
!
ELSE
!*
!     2.2 - CAS OU L'UNITE LOGIQUE N'A PAS DE NOM DE FICHIER ASSOCIE
!           EXPLICITE; ON TENTE DIRECTEMENT L'"OPEN" .
!-----------------------------------------------------------------------
!
  IF (KNUMER < 0) THEN
    CALL OPENC (CLNOMS, LLNOMS, IREP)
    IF (IREP /= 0) GOTO 902
  ELSEIF (CLSTTO.NE.'OLD'.AND.CLSTTO.NE.'NEW') THEN
    OPEN (UNIT=KNUMER,STATUS=CLSTTO,FORM='UNFORMATTED',    &
&          ACCESS='DIRECT',RECL=ILOREC,ERR=902,IOSTAT=IREP)
  ELSE
    OPEN (UNIT=KNUMER,FORM='UNFORMATTED',                  &
&          ACCESS='DIRECT',RECL=ILOREC,ERR=902,IOSTAT=IREP)
  ENDIF
!
  LLNOUF=CLSTTO.EQ.'SCRATCH'
!
ENDIF
!*
!     2.3 - L'"OPEN" S'EST BIEN PASSE... ON ESSAIE DE RECUPERER LE NOM
!           *SYSTEME* EVENTUEL ASSOCIE A L'UNITE LOGIQUE.
!-----------------------------------------------------------------------
!

IF (KNUMER > 0) THEN
  INQUIRE (UNIT=KNUMER,NAMED=LLNOMS,NAME=CLNOMS,ERR=901, &
  &        IOSTAT=IREP)
ENDIF
!
IF (LLNOMS) THEN
!
!        Recherche de la longueur "utile" du nom systeme du fichier.
!        (c'est-a-dire sans tenir compte des blancs terminaux eventuels)
!
  IDECBL=0
!
231 CONTINUE
  IPOSBL=IDECBL+INT (INDEX (CLNOMS(IDECBL+1:),' '), JPLIKB)
!
  IF (IPOSBL.LE.IDECBL) THEN
    ILNOMS=INT (LEN (CLNOMS), JPLIKB)
  ELSEIF (CLNOMS(IPOSBL:).EQ.' ') THEN
    ILNOMS=MAX (IPOSBL-1,1_JPLIKB )
  ELSE
    IDECBL=IPOSBL
    GOTO 231
  ENDIF
!
  IF (.NOT.LDNOMM) THEN
    ILNOMF=ILNOMS
    INLNOM=ILNOMS
    CLNOMF=CLNOMS
  ENDIF
!
  DO J=1,LFI%NBFIOU
  IJ=LFI%NUMIND(J)
!
  IF (CLNOMS.EQ.LFI%CNOMSY(IJ)(:LFI%NLNOMS(IJ))) THEN
    ILACTI=MIN(INT (LEN (CLNOMS), JPLIKB), &
&               INT (LEN (CLACTI), JPLIKB))
    CLACTI=CLNOMS(:ILACTI)
    IREP=-13
    IRANG=0
    IRANMS=0
    GOTO 1001
  ENDIF
!
  ENDDO
!
ELSE
  ILNOMS=INT (LEN (CPNOMD), JPLIKB)
  CLNOMS=CPNOMD
ENDIF
!
IF (CLSTTO.EQ.'OLD'.OR..NOT.LLNOUF) THEN
!**
!     3.  -  DANS LE CAS OU LE FICHIER DEVAIT OU POUVAIT EXISTER AVANT
!            OUVERTURE, ON ESSAIE DE LIRE LES PREMIERS ARTICLES.
!-----------------------------------------------------------------------
!        ( L'ARTICLE DOCUMENTAIRE ET UNE PAIRE D'ARTICLES D'INDEX;
!          ON COMMENCE PAR L'ARTICLE NO. 3 CAR IL Y A PLUS DE CHANCES
!          D'AVOIR UNE MAUVAISE LECTURE POUR CELUI-CI )
!
!          DANS LE CAS DU "STATUS" 'UNKNOWN', IL S'AGIT DE LEVER
!       L'AMBIGUITE: FICHIER DEJA ECRIT PAR LE LOGICIEL, OU DEVANT ETRE
!       CREE PAR LUI ?
!

  DO JREC=3,1,-2
  INAPHY=JREC
  CALL LFILDO_FORT                                     &
&                 (LFI, IREP,KNUMER,JREC,            &
&                  LFI%MDES1D(IXM(1_JPLIKB ,IRANG)), &
&                  INREAD,IFACTM,                    &
&                  YLFIC,IRETIN)
!
  IF (IRETIN.NE.0) THEN
    GOTO 302
  ENDIF
!
  ENDDO

! Si la longueur max des articles depasse 128, alors il faut faire du
! byte-swapping
  IF (LFI%MDES1D(IXM(2_JPLIKB, IRANG)) > 128) THEN
    YLFIC%L_C_BTSWAP = .NOT. YLFIC%L_C_BTSWAP
    CALL JSWAP (LFI%MDES1D(IXM(1_JPLIKB, IRANG)), LFI%MDES1D(IXM(1_JPLIKB, IRANG)), &
              & 8_JPLIKM, INT (LFI%JPLARD*IFACTM, JPLIKM))
  ENDIF

!
302 CONTINUE
!
  IF (IREP.EQ.0) THEN
!
!           LECTURE OK... ON CONTROLE QUELQUES VALEURS "DOCUMENTAIRES"
!
!            Fin de la parade sur CRAY2, sous UNICOS 4.0 et 5.0 .
!
    DO J=1,LFI%JPLDOC
    IF (LFI%MDES1D(IXM(J,IRANG)).NE.(IBASE-J)) GOTO 304
    ENDDO
!
    LLNOUF=.TRUE.
    GOTO 390
!
304 CONTINUE
    LLNOUF=.FALSE.
    ILARPH=LFI%MDES1D(IXM(LFI%JPLPAR,IRANG))
    INALPP=LFI%MDES1D(IXM(LFI%JPXAPI,IRANG))
    IFACPH=ILARPH/LFI%JPLARD
    IFACPP=INALPP/LFI%JPNAPP
!
    IF (MIN (ILARPH,INALPP).LE.0.OR.MOD (ILARPH,LFI%JPLARD).NE.0   &
&        .OR.LFI%MDES1D(IXM(LFI%JPLMNA,IRANG)).NE.LFI%JPNCPN        &
&        .OR.LFI%MDES1D(IXM(LFI%JPLLDO,IRANG)).NE.LFI%JPLDOC        &
&        .OR.MOD (INALPP,LFI%JPNAPP).NE.0.OR.IFACPP.NE.IFACPH) THEN
      IREP=-10
      IRANG=0
      IRANMS=0
      GOTO 1001
    ELSEIF (LFI%MDES1D(IXM(LFI%JPFEAM,IRANG)).NE.0) THEN
      IREP=-11
      LLFATA=LLMOER (IREP,IRANG)
!
      IF (LLFATA) THEN
        IRANG=0
        IRANMS=0
        GOTO 1001
      ENDIF
!
!        SI L'ERREUR (-11) N'A PAS ETE FATALE, ON DONNE LA POSSIBILITE
!       DE TRAITER LE FICHIER DONT LA DERNIERE MODIFICATION N'A PAS ETE
!       "ENREGISTREE" . MAIS SANS AUCUNE GARANTIE ...
!
    ENDIF
!
    IF (IFACPH.NE.IFACTM) THEN
!
!     Messagerie de Niveau 1 pour prevenir de l'incident...
!
      INIMES=IXNIMS (IRANMS)
!
      IF (INIMES.GE.1) THEN
        CLNSPR='LFIOUV'
!
        IF (LFI%LFRANC) THEN
          WRITE (UNIT=CLMESS,FMT='(''Unite logique'',I3,        &
& '', facteur multiplicatif lu sur fichier='',I3,'', attendu='', &
&                 I3)')KNUMER,IFACPH,IFACTM
        ELSE
          WRITE (UNIT=CLMESS,FMT='(''Logical Unit'',I3,       &
& '', multiply factor read on file='',I3,'', expected='',I3)') &
&          KNUMER,IFACPH,IFACTM
        ENDIF
!
        IREPX=IREP
        IREP=0
        CALL LFIEMS_FORT                                  &
&                       (LFI, KNUMER,INIMES,IREP,.FALSE., &
&                        CLMESS,CLNSPR,CLACTI)
!
        IF (LFI%LFRANC) THEN
          ILUTIL=MIN (INLNOM,LFI%JPLFIX,            &
&                      INT (LEN (CLMESS), JPLIKB)-6)
          CLMESS='Nom='''//CLNOMF(:ILUTIL)//''''
        ELSE
          ILUTIL=MIN (INLNOM,LFI%JPLFIX,            &
&                      INT (LEN (CLMESS), JPLIKB)-7)
          CLMESS='Name='''//CLNOMF(:ILUTIL)//''''
        ENDIF
!
        CALL LFIEMS_FORT                                  &
&                       (LFI, KNUMER,INIMES,IREP,.FALSE., &
&                        CLMESS,CLNSPR,CLACTI)
!
        IF (LDNOMM.AND.CLNOMS.NE.CLNOMF) THEN
!
          IF (LFI%LFRANC) THEN
            ILUTIL=MIN (ILNOMS,LFI%JPLFIX,             &
&                        INT (LEN (CLMESS), JPLIKB)-14)
            CLMESS='Nom SYSTEME='''//CLNOMS(:ILUTIL)//''''
          ELSE
            ILUTIL=MIN (ILNOMS,LFI%JPLFIX,             &
&                        INT (LEN (CLMESS), JPLIKB)-14)
            CLMESS='SYSTEM Name='''//CLNOMS(:ILUTIL)//''''
          ENDIF
!
          CALL LFIEMS_FORT                                  &
&                         (LFI, KNUMER,INIMES,IREP,.FALSE., &
&                          CLMESS,CLNSPR,CLACTI)
        ENDIF
!
        IF (LFI%LFRANC) THEN
          CLMESS='On essaie de s''adapter au facteur ' &
&               //'multiplicatif lu sur le fichier...'
        ELSE
          CLMESS='One tries to adapt to multiply ' &
&               //'factor read on the file...'
        ENDIF
!
        CALL LFIEMS_FORT                                  &
&                       (LFI, KNUMER,INIMES,IREP,.FALSE., &
&                        CLMESS,CLNSPR,CLACTI)
        IREP=IREPX
      ENDIF
!
!        On va essayer de traiter le fichier avec la longueur d'Article
!     Physique lue sur le fichier. Pour cela, on doit d'abord le fermer,
!     puis on va recommencer le traitement depuis le paragraphe 1.1 .
!
      IRANG=0
      IRANMS=0
      IF (KNUMER > 0) THEN
        CLOSE (UNIT=KNUMER,STATUS='KEEP',ERR=905,IOSTAT=IREP)
      ELSE
        CALL CLOSEC (IREP)
        IF (IREP /= 0) GOTO 905
      ENDIF
!
      IF (IFACPH.GT.LFI%JPFACX) THEN
        IREP=-28
        GOTO 1001
      ENDIF
!
      IFACTM=IFACPH
      GOTO 110
    ENDIF
!
  ELSEIF (CLSTTO.EQ.'OLD') THEN
    IREP=-12
    IRANG=0
    IRANMS=0
    GOTO 1001
  ELSE
    IREP=0
    LLNOUF=.TRUE.
  ENDIF
!
ENDIF
!
390 CONTINUE
!
!         Controle ultime avant le paragraphe suivant, dans la mesure
!     ou, contrairement au FORTRAN, on autorise les "STATUS" 'OLD'
!     et 'NEW' pour une unite logique sans nom de fichier explicite...
!     puisque le logiciel a sa propre mecanique de discrimination entre
!     un fichier "existant" ou "en mode creation".
!
IF (LLNOUF.AND.CLSTTO.EQ.'OLD'                &
&    .OR..NOT.LLNOUF.AND.CLSTTO.EQ.'NEW') THEN
  CLACTI=CLSTTO
  IREP=-9
  IRANG=0
  IRANMS=0
  GOTO 1001
ENDIF
!**
!     4.  -  L'OUVERTURE FORTRAN EST OK, ON SAIT SI ON EST EN MODE
!            CREATION DU FICHIER INDEXE OU NON... ON COMMENCE A GARNIR
!            LES VARIABLES EN COMMON, MAIS SANS INCREMENTER *LFI%NBFIOU*
!            CAR ON PEUT ENCORE AVOIR DE (MAUVAISES) SURPRISES.
!-----------------------------------------------------------------------
!
IREPX=IREP
LFI%CNOMFI(IRANG)=CLNOMF
LFI%NLNOMF(IRANG)=ILNOMF
LFI%CNOMSY(IRANG)=CLNOMS
LFI%NLNOMS(IRANG)=ILNOMS
LFI%NDEROP(IRANG)=0
LFI%CSTAOP(IRANG)=CLSTTO
LFI%LNOUFI(IRANG)=LLNOUF
LFI%LMODIF(IRANG)=.FALSE.
LFI%NDERCO(IRANG)=IREP
LFI%NTRULZ(IRANG)=0
LFI%NRFPTZ(IRANG)=0
LFI%NRFDTZ(IRANG)=0
LFI%NBMOLU(IRANG)=0
LFI%NBMOEC(IRANG)=0
LFI%NDERGF(IRANG)=LFI%JPNIL
LFI%CNDERA(IRANG)=' '
LFI%MFACTM(IRANG)=IFACTM
LFI%NSUIVF(IRANG)=LFI%JPNIL
LFI%NPRECF(IRANG)=LFI%JPNIL
!
!     N.B.: LES PAGES D'INDEX DE RANG "IRANG" SONT AUTOMATIQUEMENT
!        "AFFECTEES" A L'UNITE LOGIQUE AYANT CE RANG, ET SERVENT
!        A Y STOCKER LA PREMIERE P.A.I. EN RANG DANS LE FICHIER.
!
!     ( LES PAGES D'INDEX DE RANG "IRANG+(J-1)*LFI%JPNXFI" OU J VARIE
!       DE 1 A LFI%JPNPIA, SONT AUTOMATIQUEMENT AFFECTEES A L'UNITE
!       LOGIQUE DE RANG "IRANG" )
!
LFI%NBLECT(IRANG)=0
LFI%NBNECR(IRANG)=0
LFI%NREESP(IRANG)=0
LFI%NREECO(IRANG)=0
LFI%NREELO(IRANG)=0
LFI%NBTROU(IRANG)=0
LFI%NBRENO(IRANG)=0
LFI%NBSUPP(IRANG)=0
LFI%LISTAT(IRANG)=LDIMST
LFI%LMIMAL(IRANG)=.FALSE.
 IF (LFI%LMULTI) CALL LFIVER_FORT                                &
&                                (LFI, LFI%VERRUE(IRANG),'ASGN')
!
IF (LLNOUF) THEN
!*
!     4.1 - CAS DE CREATION DU FICHIER INDEXE - INITIALISATIONS DIVERSES
!-----------------------------------------------------------------------
!
  ILARPH=LFI%JPLARD*IFACTM
  INALPP=LFI%JPNAPP*IFACTM
!
  DO J=1,ILARPH
  LFI%MLGPOS(IXM(J,IRANG))=0
  ENDDO
!
  DO J=1,LFI%JPNXNA
  LFI%CNOMAR(IXC(J,IRANG))=' '
  ENDDO
!
  DO J=1,ILARPH
  LFI%MDES1D(IXM(J,IRANG))=0
  ENDDO
!
!         NOMBRE DE PAIRES D'ARTICLES D'INDEX RESERVES,
!         (ELLES OCCUPERONT LES ARTICLES 2 A (2*INBPIR+1) DU FICHIER)
!         ET REMPLISSAGE DE CERTAINS MOTS DE L'ARTICLE DOCUMENTAIRE.
!
  INBPIR=MAX (1_JPLIKB ,MIN (LFI%JPNXPR,1+(KNBARP-1)/INALPP))
  LFI%MDES1D(IXM(LFI%JPNPIR,IRANG))=INBPIR
  LFI%MDES1D(IXM(LFI%JPNAPH,IRANG))=1+2*INBPIR
  LFI%MDES1D(IXM(LFI%JPLPAR,IRANG))=ILARPH
  LFI%MDES1D(IXM(LFI%JPLMNA,IRANG))=LFI%JPNCPN
  LFI%MDES1D(IXM(LFI%JPLLDO,IRANG))=LFI%JPLDOC
  LFI%MDES1D(IXM(LFI%JPXAPI,IRANG))=INALPP
  LFI%MDES1D(IXM(LFI%JPFEAM,IRANG))=1
  LFI%NPODPI(IRANG)=1
  LFI%NALDPI(IRANG)=0
  LFI%NPPIMM(IRANG)=1
  IRANGD=IRANG
  CALL LFIDAH_FORT                                           &
&                 (LFI, LFI%MDES1D(IXM(LFI%JPDCRE,IRANG)), &
&                  LFI%MDES1D(IXM(LFI%JPHCRE,IRANG)))
!
!          ECRITURE DU PREMIER ARTICLE (DESCRIPTIF)
!
  IREC=1
  INAPHY=IREC
  CALL LFIEDO_FORT                                     &
&                 (LFI, IREP,KNUMER,IREC,            &
&                  LFI%MDES1D(IXM(1_JPLIKB ,IRANG)), &
&                  INWRIT,  IFACTM, YLFIC, IRETIN)
!
  IF (IRETIN.NE.0) THEN
    GOTO 904
  ENDIF
!
!
!     Remise a zero du descripteur en vue d'une fermeture normale.
!
  LFI%MDES1D(IXM(LFI%JPFEAM,IRANG))=0
!
!          ECRITURE DES ARTICLES CONTENANT LES PAIRES D'ARTICLES D'INDEX
!          "RESERVES".
!
  DO J=1,INBPIR
  IREC=IREC+1
  INAPHY=IREC
  CALL LFIECC_FORT                                     &
&                 (LFI, IREP,KNUMER,IREC,            &
&                  LFI%CNOMAR(IXC(1_JPLIKB ,IRANG)), &
&                  INWRIT,IFACTM, YLFIC, IRETIN)
!
  IF (IRETIN.NE.0) THEN
    GOTO 903
  ENDIF
!
  IREC=IREC+1
  INAPHY=IREC
  CALL LFIEDO_FORT                                     &
&                 (LFI, IREP,KNUMER,IREC,            &
&                  LFI%MLGPOS(IXM(1_JPLIKB ,IRANG)), &
&                  INWRIT,  IFACTM, YLFIC, IRETIN)
!
  IF (IRETIN.NE.0) THEN
    GOTO 904
  ENDIF
!
  ENDDO
!
ELSE
!*
!     4.2 - LE FICHIER EXISTAIT DEJA... ON LIT LA 1ERE PAIRE D'ARTICLES
!           D'INDEX ( + LA DERNIERE S'IL Y EN A AU MOINS 2 *UTILISEES* )
!-----------------------------------------------------------------------
!
  INBALO=LFI%MDES1D(IXM(LFI%JPNALO,IRANG))
  INBPIR=LFI%MDES1D(IXM(LFI%JPNPIR,IRANG))
  IREC=2
  INAPHY=IREC
  CALL LFILCC_FORT                                     &
&                 (LFI, IREP,KNUMER,IREC,            &
&                  LFI%CNOMAR(IXC(1_JPLIKB ,IRANG)), &
&                  INREAD,IFACTM,YLFIC,IRETIN)
!
  IF (IRETIN.NE.0) THEN
    GOTO 904
  ENDIF
!
  IREC=3
  INAPHY=IREC
  CALL LFILDO_FORT                                     &
&                 (LFI, IREP,KNUMER,IREC,            &
&                  LFI%MLGPOS(IXM(1_JPLIKB ,IRANG)), &
&                  INREAD,IFACTM,                    &
&                  YLFIC,IRETIN)
!
  IF (IRETIN.NE.0) THEN
    GOTO 904
  ENDIF
!
  IF (INBALO.LE.INALPP) THEN
    LFI%NALDPI(IRANG)=INBALO
    LFI%NPODPI(IRANG)=1
    LFI%NPPIMM(IRANG)=1
    IRANGD=IRANG
  ELSE
!
!          CAS OU IL Y A AU MOINS 2 PAIRES D'ARTICLES D'INDEX UTILISEES.
!
    IRGPIF=1+(INBALO-1)/INALPP
    CALL LFIREC_FORT                         &
&                   (LFI, IRGPIF,IRANG,IREC)
    IRANGD=IRANG+LFI%JPNXFI
    INAPHY=IREC
    CALL LFILCC_FORT                                      &
&                   (LFI, IREP,KNUMER,IREC,             &
&                    LFI%CNOMAR(IXC(1_JPLIKB ,IRANGD)), &
&                    INREAD,IFACTM,YLFIC,IRETIN)
!
    IF (IRETIN.NE.0) THEN
      GOTO 904
    ENDIF
!
    IREC=IREC+1
    INAPHY=IREC
    CALL LFILDO_FORT                                      &
&                   (LFI, IREP,KNUMER,IREC,             &
&                    LFI%MLGPOS(IXM(1_JPLIKB ,IRANGD)), &
&                    INREAD,IFACTM,                     &
&                    YLFIC,IRETIN)
!
    IF (IRETIN.NE.0) THEN
      GOTO 904
    ENDIF
!
    LFI%NALDPI(IRANG)=1+MOD (INBALO-1,INALPP)
    LFI%NPODPI(IRANG)=2
    LFI%NPPIMM(IRANG)=2
    LFI%MRGPIM(2,IRANG)=IRANGD
    LFI%MRGPIF(IRANGD)=IRGPIF
  ENDIF
!
ENDIF
!**
!     5.  -  L'OUVERTURE AU SENS DU LOGICIEL DE FICHIERS INDEXES LFI
!            EST COMPLETE; ON MET DONC A JOUR LES DERNIERES VARIABLES
!            EN COMMON, DONT *LFI%NBFIOU*.
!-----------------------------------------------------------------------
!
!           REMARQUE: LA PREMIERE ET LA DERNIERE P.P.I. SONT TOUJOURS
!                     "PHASEES".
!
DO J=IRANG,IRANGD,LFI%JPNXFI
LFI%LECRPI(J,1)=.FALSE.
LFI%LECRPI(J,2)=.FALSE.
LFI%LPHASP(J)=.TRUE.
ENDDO
!
DO J=0,LFI%JPNPDF-1
LFI%NUMAPD(J,IRANG)=LFI%JPNIL
LFI%NLONPD(J,IRANG)=0
LFI%LECRPD(J,IRANG)=.FALSE.
ENDDO
!
DO J=1,IFACTM
LFI%NUMERO(IRANG+J-1)=KNUMER
ENDDO
!
YLFIC%CNOMFI => LFI%CNOMFI (IRANG)
LFI%YLFIC (IRANG)=YLFIC
!
LFI%NDERPD(IRANG)=LFI%JPNPDF-1
LFI%NBFIOU=LFI%NBFIOU+1
LFI%NFACTM=LFI%NFACTM+IFACTM
LFI%NUMIND(LFI%NBFIOU)=IRANG
INBARI=LFI%MDES1D(IXM(LFI%JPNALO,IRANG))- &
&       LFI%MDES1D(IXM(LFI%JPNTRU,IRANG))
LFI%NBREAD(IRANG)=INREAD
LFI%NBWRIT(IRANG)=INWRIT
LFI%LTAMPL(IRANG)=LFI%LTAMLG
LFI%LTAMPE(IRANG)=LFI%LTAMEG
LFI%NEXPOR(IRANG)=LFI%JPNIL
LFI%NIMPOR(IRANG)=LFI%JPNIL
!
IREP=IREPX
GOTO 1001
!**
!     9.  - CI-DESSOUS, ETIQUETTES DE BRANCHEMENT EN CAS D'ERREUR E/S.
!-----------------------------------------------------------------------
!
901 CONTINUE
CLACTI='INQUIRE'
INAPHY=0
GOTO 909
!
902 CONTINUE
CLACTI='OPEN'
IRANG=0
IRANMS=0
INAPHY=0
GOTO 909
!
903 CONTINUE
CLACTI='WRITE'
GOTO 909
!
904 CONTINUE
CLACTI='READ'
GOTO 909
!
905 CONTINUE
CLACTI='CLOSE'
INAPHY=0
!
909 CONTINUE
!
!      AU CAS OU, ON FORCE LE CODE-REPONSE ENTREE/SORTIE A ETRE POSITIF.
!
IREP=ABS (IREP)
LFI%NUMAPH(IRANG)=INAPHY
IF (IRANG.EQ.0) LFI%MFACTM(0)=IFACTM
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE, AVEC "ABORT" EVENTUEL,
!            VIA LE SOUS-PROGRAMME "LFIEMS" .
!-----------------------------------------------------------------------
!
1001 CONTINUE
KREP=IREP
KNBARI=INBARI
LLFATA=LLMOER (IREP,IRANG)
!
IF (LLFATA) THEN
  INIMES=2
ELSE
  INIMES=IXNIMS (IRANMS)
ENDIF
!
 IF (LFI%LMULTI.AND.LLVERG) CALL LFIVER_FORT                        &
&                                           (LFI, LFI%VERGLA,'OFF')
!
IF (.NOT.LLFATA.AND.INIMES.EQ.0)  THEN 
  IF (LHOOK) CALL DR_HOOK('LFIOUV_FORT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='LFIOUV'
!
IF (INIMES.GE.1) THEN
!
!           Impression du nom du fichier.
!
  IF (LFI%LFRANC) THEN
    ILUTIL=MIN (INLNOM,LFI%JPLFIX,            &
&                INT (LEN (CLMESS)-6, JPLIKB))
    CLMESS='Nom='''//CLNOMF(:ILUTIL)//''''
  ELSE
    ILUTIL=MIN (INLNOM,LFI%JPLFIX,            &
&                INT (LEN (CLMESS)-7, JPLIKB))
    CLMESS='Name='''//CLNOMF(:ILUTIL)//''''
  ENDIF
!
  CALL LFIEMS_FORT                                  &
&                 (LFI, KNUMER,INIMES,IREP,.FALSE., &
&                  CLMESS,CLNSPR,CLACTI)
!
  IF (LDNOMM.AND.CLNOMS(:ILNOMS).NE.CLNOMF(:INLNOM)) THEN
!
    IF (LFI%LFRANC) THEN
      ILUTIL=MIN (ILNOMS,LFI%JPLFIX,             &
&                  INT (LEN (CLMESS)-14, JPLIKB))
      CLMESS='Nom SYSTEME='''//CLNOMS(:ILUTIL)//''''
    ELSE
      ILUTIL=MIN (ILNOMS,LFI%JPLFIX,             &
&                  INT (LEN (CLMESS)-14, JPLIKB))
      CLMESS='SYSTEM Name='''//CLNOMS(:ILUTIL)//''''
    ENDIF
!
    CALL LFIEMS_FORT                                  &
&                   (LFI, KNUMER,INIMES,IREP,.FALSE., &
&                    CLMESS,CLNSPR,CLACTI)
  ENDIF
!
ENDIF
!
IF (INIMES.EQ.2) THEN
  WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KNUMER='',I3,          &
&     '', LDNOMM= '',L1,'', CDSTTO='''''',A,'''''', LDERFA= '',L1,  &
&     '',  LDIMST= '',L1,                                           &
&         '', KNIMES='',I2,'', KNBARP='',I6,'' KNBARI='',I6)')      &
&   KREP,KNUMER,LDNOMM,CDSTTO(:ILSTTU),LDERFA,LDIMST,KNIMES,KNBARP, &
&   KNBARI
  CALL LFIEMS_FORT                                 &
&                 (LFI, KNUMER,INIMES,IREP,LLFATA, &
&                  CLMESS,CLNSPR,CLACTI)
ENDIF
!
!        LA MESSAGERIE QUI SUIT N'EST PAS EMISE EN CAS D'ERREUR FATALE.
!
IF (INIMES.GE.1.AND.(IREP.EQ.0.OR.IREP.EQ.-11)) THEN
!
  IF (LLNOUF) THEN
!
    IF (LFI%LFRANC) THEN
      WRITE (UNIT=CLMESS,FMT='(''Unite'',I3,                       &
&   '' OUVERTE, CREATION de Fichier,'',I7,'' Articles prevus,'',I7, &
&   '' Articles gerables sans debordement'')')                      &
&      KNUMER,KNBARP,INALPP*INBPIR
    ELSE
      WRITE (UNIT=CLMESS,FMT='(''Unit'',I3,                  &
&   '' OPENED, File CREATION,'',I7,'' expected Records,'',I7, &
&   '' Records may be handled without overflow'')')           &
&      KNUMER,KNBARP,INALPP*INBPIR
    ENDIF
!
  ELSE
!
    IF (LFI%LFRANC) THEN
      WRITE (UNIT=CLMESS,FMT='(''Unite'',I3,                      &
&       '' OUVERTE, derniere Modification OK a'',I9.6,''_'',I6.6,  &
&       '','',I7,'' Articles de donnees,'',I9,'' mots en tout'')') &
&     KNUMER,LFI%MDES1D(IXM(LFI%JPDDMG,IRANG)),                    &
&     LFI%MDES1D(IXM(LFI%JPHDMG,IRANG)),                           &
&            KNBARI,ILARPH*LFI%MDES1D(IXM(LFI%JPNAPH,IRANG))
    ELSE
      WRITE (UNIT=CLMESS,FMT='(''Unit'',I3,                  &
&       '' OPENED, last Modification OK at'',I9.6,''_'',I6.6, &
&       '','',I7,'' data Records,'',I9,'' words in file'')')  &
&     KNUMER,LFI%MDES1D(IXM(LFI%JPDDMG,IRANG)),               &
&     LFI%MDES1D(IXM(LFI%JPHDMG,IRANG)),                      &
&            KNBARI,ILARPH*LFI%MDES1D(IXM(LFI%JPNAPH,IRANG))
    ENDIF
!
  ENDIF
!
  CALL LFIEMS_FORT                                  &
&                 (LFI, KNUMER,INIMES,IREP,.FALSE., &
&                  CLMESS,CLNSPR,CLACTI)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('LFIOUV_FORT',1,ZHOOK_HANDLE)

CONTAINS

#include "lficom2.ixc.h"
#include "lficom2.ixm.h"
#include "lficom2.ixnims.h"
#include "lficom2.llmoer.h"

SUBROUTINE OPENC (CDNOMS, LDNOMS, KREP)

IMPLICIT NONE

CHARACTER (LEN=*)     CDNOMS
LOGICAL               LDNOMS
INTEGER (KIND=JPLIKB) KREP

INTEGER (KIND=JPLIKB) I
INTEGER (KIND=JPIM) IREP4

KREP=0

LDNOMS=.TRUE.

IF (LDNOMM) THEN

  CDNOMS = CDNOMF

ELSE

  DO I = 1, LEN (CDNOMS)
    CDNOMS (I:I) = ' '
  ENDDO
  WRITE (CDNOMS, '(I4.4)') -KNUMER
  DO I = 1, 3
    IF (CDNOMS (1:1) == '0') CDNOMS (1:4) = CDNOMS (2:5)
  ENDDO

  CDNOMS = "fort."//TRIM (CDNOMS)

ENDIF

SELECT CASE (CLSTTO)
  CASE ('NEW')
    CALL FI_FOPEN (YLFIC%N_C_FPDESC, CDNOMS, "w+")
  CASE ('OLD')
    CALL FI_FOPEN (YLFIC%N_C_FPDESC, CDNOMS, "r+")
    IF (YLFIC%N_C_FPDESC == 0) &
   &CALL FI_FOPEN (YLFIC%N_C_FPDESC, CDNOMS, "r")
  CASE DEFAULT
    CALL FI_FOPEN (YLFIC%N_C_FPDESC, CDNOMS, "r+")
    IF (YLFIC%N_C_FPDESC == 0) &
   &CALL FI_FOPEN (YLFIC%N_C_FPDESC, CDNOMS, "w+")
    IF (YLFIC%N_C_FPDESC == 0) &
   &CALL FI_FOPEN (YLFIC%N_C_FPDESC, CDNOMS, "r")
END SELECT

IF (YLFIC%N_C_FPDESC == 0) THEN
  CALL FI_ERRNO (IREP4)
  KREP=INT (IREP4, JPLIKB)
ENDIF

END SUBROUTINE OPENC

SUBROUTINE CLOSEC (KREP)
USE PARKIND1, ONLY : JPIM

INTEGER (KIND=JPLIKB) KREP

INTEGER (KIND=JPIM) IREP4

KREP=0

CALL FI_FCLOSE (IREP4, YLFIC%N_C_FPDESC)
IF (IREP4 /= 0) THEN
  CALL FI_ERRNO (IREP4)
  KREP=IREP4
ENDIF

END SUBROUTINE CLOSEC

END SUBROUTINE LFIOUV_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFIOUV64                                      &
&           (KREP, KNUMER, LDNOMM, CDNOMF, CDSTTO, LDERFA, &
&           LDIMST, KNIMES, KNBARP, KNBARI)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
LOGICAL                LDNOMM                                 ! IN   
CHARACTER (LEN=*)      CDNOMF                                 ! IN   
CHARACTER (LEN=*)      CDSTTO                                 ! IN   
LOGICAL                LDERFA                                 ! IN   
LOGICAL                LDIMST                                 ! IN   
INTEGER (KIND=JPLIKB)  KNIMES                                 ! IN   
INTEGER (KIND=JPLIKB)  KNBARP                                 ! IN   
INTEGER (KIND=JPLIKB)  KNBARI                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIOUV_FORT                                               &
&           (LFI, KREP, KNUMER, LDNOMM, CDNOMF, CDSTTO, LDERFA, &
&           LDIMST, KNIMES, KNBARP, KNBARI)

END SUBROUTINE LFIOUV64

SUBROUTINE LFIOUV                                        &
&           (KREP, KNUMER, LDNOMM, CDNOMF, CDSTTO, LDERFA, &
&           LDIMST, KNIMES, KNBARP, KNBARI)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
LOGICAL                LDNOMM                                 ! IN   
CHARACTER (LEN=*)      CDNOMF                                 ! IN   
CHARACTER (LEN=*)      CDSTTO                                 ! IN   
LOGICAL                LDERFA                                 ! IN   
LOGICAL                LDIMST                                 ! IN   
INTEGER (KIND=JPLIKM)  KNIMES                                 ! IN   
INTEGER (KIND=JPLIKM)  KNBARP                                 ! IN   
INTEGER (KIND=JPLIKM)  KNBARI                                 !   OUT

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFIOUV_MT                                                &
&           (LFI, KREP, KNUMER, LDNOMM, CDNOMF, CDSTTO, LDERFA, &
&           LDIMST, KNIMES, KNBARP, KNBARI)

END SUBROUTINE LFIOUV

SUBROUTINE LFIOUV_MT                                          &
&           (LFI, KREP, KNUMER, LDNOMM, CDNOMF, CDSTTO, LDERFA, &
&           LDIMST, KNIMES, KNBARP, KNBARI)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
LOGICAL                LDNOMM                                 ! IN   
CHARACTER (LEN=*)      CDNOMF                                 ! IN   
CHARACTER (LEN=*)      CDSTTO                                 ! IN   
LOGICAL                LDERFA                                 ! IN   
LOGICAL                LDIMST                                 ! IN   
INTEGER (KIND=JPLIKM)  KNIMES                                 ! IN   
INTEGER (KIND=JPLIKM)  KNBARP                                 ! IN   
INTEGER (KIND=JPLIKM)  KNBARI                                 !   OUT
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  INIMES                                 ! IN   
INTEGER (KIND=JPLIKB)  INBARP                                 ! IN   
INTEGER (KIND=JPLIKB)  INBARI                                 !   OUT
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)
INIMES     = INT (    KNIMES, JPLIKB)
INBARP     = INT (    KNBARP, JPLIKB)

CALL LFIOUV_FORT                                               &
&           (LFI, IREP, INUMER, LDNOMM, CDNOMF, CDSTTO, LDERFA, &
&           LDIMST, INIMES, INBARP, INBARI)

KREP       = INT (      IREP, JPLIKM)
KNBARI     = INT (    INBARI, JPLIKM)

IF (KNUMER == 0) THEN
  KNUMER = INT (    INUMER, JPLIKM)
ENDIF

END SUBROUTINE LFIOUV_MT

!INTF KREP            OUT 
!INTF KNUMER        IN    
!INTF LDNOMM        IN    
!INTF CDNOMF        IN    
!INTF CDSTTO        IN    
!INTF LDERFA        IN    
!INTF LDIMST        IN    
!INTF KNIMES        IN    
!INTF KNBARP        IN    
!INTF KNBARI          OUT 
