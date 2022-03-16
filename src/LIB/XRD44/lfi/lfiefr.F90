! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI
! Sep-2012 P. Marguinaud Add FLUSH stdout
SUBROUTINE LFIEFR_FORT                                      &
&                     (LFI, KNUMER, KNIMES, KCODE, LDFATA,  &
&                      CDMESS, CDNSPR, CDACTI )
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE SDL_MOD   , ONLY : SDL_SRLABORT
USE LFI_PRECISION
IMPLICIT NONE
!****
!        CE SOUS-PROGRAMME EST CHARGE DE FAIRE L'IMPRESSION DES MESSAGES
!     STANDARD EMIS PAR LE LOGICIEL DE FICHIERS INDEXES LFI, EN FAISANT
!     SI BESOIN EST L'"ABORT" DU PROGRAMME .
!        Les messages lies au mode "mise au point" sont emis directement
!     par les sous-programmes concernes.
!
!        Ce sous-programme est la V.O. (Version Originale, francaise),
!     et est appele par le sous-programme "chapeau" LFIEMS.
!     Pour la version anglaise, voir LFIENG.
!     ( For english version see subroutine LFIENG )
!**
!        ARGUMENTS : KNUMER ==> Numero eventuel de l'Unite Logique;
!        ( tous                 ( si LFI%JPNIL ==> pas d'Unite Logique )
!         d'Entree ) KNIMES ==> Niveau (0,1,2) du Message;
!                    KCODE  ==> Code correspondant a l'action en cause;
!                    LDFATA ==> Vrai si on doit avorter le programme;
!                    CDMESS ==> Si KNIMES#0, Message a emettre;
!                    CDNSPR ==> Nom du sous-programme appelant LFIEMS;
!                    CDACTI ==> Nom de l'action d'entree/sortie FORTRAN
!                               si KCODE >0), sinon fourre-tout (!) .
!*
!     !----------------------------------------------------------------!
!     ! TABLE DES VALEURS POSSIBLES DES CODES-REPONSES DU LOGICIEL LFI !
!     !----------------------------------------------------------------!
!
!-----------------------------------------------------------------------
!      0 ==> Aucune erreur n'a ete detectee, tout est OK.
!-----------------------------------------------------------------------
! valeur ==> Il s'agit (de la valeur absolue) du code-reponse FORTRAN
! positive   d'une instruction OPEN, READ, WRITE, CLOSE ou INQUIRE; pour
!            le sens exact voir le manuel de reference du constructeur.
!-----------------------------------------------------------------------
!     -1 ==> Unite Logique non ouverte pour le logiciel.
!-----------------------------------------------------------------------
!     -2 ==> Valeur d'un "NIVEAU" hors plage [0-2] .
!-----------------------------------------------------------------------
!     -3 ==> Option de verrou erronee (s/p a usage interne "LFIVER") .
!-----------------------------------------------------------------------
!     -4 ==> Changement explicite de mode Multi-Taches avec au moins une
!            unite logique ouverte-risque de problemes (s/p "LFIINI") .
!-----------------------------------------------------------------------
!     -5 ==> Unite Logique deja ouverte (LFIOUV, LFIAFM, LFISFM) .
!-----------------------------------------------------------------------
!     -6 ==> Pas assez de place dans les tables pour ouvrir l'Unite
!            Logique demandee (LFIOUV) .
!-----------------------------------------------------------------------
!     -7 ==> Argument illicite de "STATUS" pour l'instruction FORTRAN
!            "OPEN" (LFIOUV) .
!-----------------------------------------------------------------------
!     -8 ==> Incompatibilite entre "LDNOMM" et "CDSTTO" (LFIOUV) :
!            un fichier de "STATUS" 'OLD' ou 'NEW' doit etre nomme .
!            (CE CODE-REPONSE N'A PLUS DE SENS ACTUELLEMENT)
!-----------------------------------------------------------------------
!     -9 ==> Incompatibilite entre le "STATUS" 'NEW' ou 'OLD' et (respe-
!            ctivement) l'existence ou non du fichier (LFIOUV) .
!-----------------------------------------------------------------------
!    -10 ==> Le fichier considere n'est pas un fichier de type LFI, ou
!            ne peut pas etre traite par cette version du logiciel.
!            (LFIOUV)
!-----------------------------------------------------------------------
!    -11 ==> Fichier non ferme apres une modification (LFIOUV): cette
!            erreur n'est pas fatale si "LDERFA" est .FALSE., mais alors
!            integrite et coherence des donnees ne sont pas garanties.
!            Noter qu'une fois qu'un fichier a ce type de probleme, ce
!            code-reponse restera meme apres modification ulterieure.
!-----------------------------------------------------------------------
!    -12 ==> Fichier de "STATUS" 'OLD' mais erreur sur la lecture du
!            premier article physique du fichier (LFIOUV) .
!-----------------------------------------------------------------------
!    -13 ==> Fichier deja ouvert pour une autre unite logique LFI.
!            (LFIOUV)
!-----------------------------------------------------------------------
!    -14 ==> Argument d'appel de type ENTIER incorrect (souvent negatif)
!-----------------------------------------------------------------------
!    -15 ==> Argument d'appel de type CARACTERE incorrect (longueur).
!-----------------------------------------------------------------------
!    -16 ==> Incoherence Tables, Fichier, appels s/p internes, logiciel.
!            CETTE ERREUR NE PEUT PAS ETRE FILTREE. EST TOUJOURS FATALE.
!-----------------------------------------------------------------------
!    -17 ==> Trop d'articles logiques sur le fichier pour un de plus.
!            (par articles logiques on entend ceux lisibles par l'utili-
!             sateur, mais aussi les trous reperes dans l'index... qui
!             sont crees lors de reecritures d'articles de donnees ne
!             pouvant se faire sur place, et lors de suppression d'arti-
!             cles; ces trous peuvent etre "recycles" - LFIECR)
!-----------------------------------------------------------------------
!    -18 ==> Nom d'Article logique compose uniquement de BLANCS illicite
!            (pour le fonctionnement interne du logiciel LFI,
!             les trous d'index sont reperes par un nom d'article blanc)
!-----------------------------------------------------------------------
!    -19 ==> Un fichier ouvert avec le "STATUS" 'SCRATCH' ne peut pas
!            etre conserve: "CDSTTC" a 'KEEP' est illicite (LFIFER) .
!            si cette erreur n'est pas fatale, alors on execute un
!            "CLOSE" FORTRAN sans parametre "STATUS", de la meme maniere
!            que lorsque "CDSTTC" n'est ni a 'KEEP' ni a 'DELETE'.
!-----------------------------------------------------------------------
!    -20 ==> L'article logique demande n'existe pas dans le fichier.
!            (LFILEC, LFIREN, LFISUP)
!-----------------------------------------------------------------------
!    -21 ==> L'article logique demande est PLUS LONG sur le fichier;
!            si cette erreur n'est pas fatale, le resultat est une
!            lecture PARTIELLE de l'article, a la longueur demandee.
!            (LFILAP, LFILAS, LFILEC)
!-----------------------------------------------------------------------
!    -22 ==> L'article logique demande est PLUS COURT sur le fichier;
!            meme si cette erreur n'est pas fatale, AUCUNE LECTURE DE
!            DONNEES N'EST FAITE (LFILAP, LFILAS, LFILEC) .
!-----------------------------------------------------------------------
!    -23 ==> Il n'y a pas ou plus d'article "SUIVANT" a lire (LFILAS) .
!-----------------------------------------------------------------------
!    -24 ==> La variable caractere donnee en argument d'appel de sortie
!            est TROP COURTE pour y stocker le NOM de l'article, meme en
!            supprimant d'eventuels caracteres blancs en fin de nom.
!            (LFICAP, LFICAS, LFILAP, LFILAS)
!-----------------------------------------------------------------------
!    -25 ==> Le nouveau nom de l'article logique est (deja) celui d'un
!            autre article logique du fichier (LFIREN).
!-----------------------------------------------------------------------
!    -26 ==> Il n'y a pas ou plus d'article "PRECEDENT" a lire (LFILAP).
!-----------------------------------------------------------------------
!    -27 ==> Espace CONTIGU insuffisant dans les tables pour gerer le
!            fichier "multiple" demande (LFIOUV) .
!-----------------------------------------------------------------------
!    -28 ==> Facteur multiplicatif (de la longueur d'article physique
!            elementaire) trop grand pour la configuration du logiciel.
!            (LFIOUV, LFIAFM, LFIFMD)
!-----------------------------------------------------------------------
!    -29 ==> Pas assez de place dans les tables pour definir le facteur
!            multiplicatif a associer a l'Unite Logique (LFIAFM) .
!-----------------------------------------------------------------------
!    -30 ==> Numero d'Unite Logique FORTRAN illicite.
!-----------------------------------------------------------------------
!    -31 ==> Numero d'Unite Logique sans facteur multiplicatif predefini
!            (LFISFM)
!-----------------------------------------------------------------------
!
!
TYPE(LFICOM) :: LFI
INTEGER (KIND=JPLIKB) KNUMER, KNIMES, KCODE, ILDMES 
INTEGER (KIND=JPLIKB) ILBLAN, INLNOM, INUMER
INTEGER (KIND=JPLIKB) ILACTI, ILACT2 
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
!        Recherche de la longueur "utile" de l'argument CDACTI.
!        (c'est-a-dire sans tenir compte des blancs terminaux eventuels)
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('LFIEFR_FORT',0,ZHOOK_HANDLE)
ILACTI=INT (LEN (TRIM (CDACTI)), JPLIKB)
!
ILACT2=MIN (ILACTI,LFI%JPNCPN)
ILACTI=MIN (ILACT2,8_JPLIKB )
ILNSPR=MIN (INT (LEN (CDNSPR), JPLIKB),LFI%JPLSPX)
!
!        Prefixe (et eventuellement suffixe) pour le(s) message(s).
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
!     2.  -  ON IMPRIME LE MESSAGE PREPARE PAR LE S/P APPELLANT LFIEMS.
!-----------------------------------------------------------------------
!
  ILMESU=MIN (INT (LEN (CLMESS), JPLIKB)  &
&             -INT (LEN (CLJOLI), JPLIKB)  &
&             -ILNSPR-4,                   &
&              INT (LEN (CDMESS), JPLIKB))
  CLMESS=CLJOLI//' '//CDNSPR(1:ILNSPR)//' - '// &
&             CDMESS(1:ILMESU)
  WRITE (UNIT=LFI%NULOUT,FMT='(A)') TRIM (CLMESS)
ENDIF
!
IF (KNIMES.EQ.0.OR.LDFATA) THEN
!**
!     3.  -  CONSTITUTION D'UN MESSAGE "AD HOC", EN FONCTION DE *KCODE*.
!-----------------------------------------------------------------------
!
!     En preambule, on cherche si l'unite logique concernee correspond
!     ou non a une unite logique ouverte pour le logiciel LFI.
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
      WRITE (UNIT=CLMESS,FMT='(''ERREUR "'',A,''"'',I7,  &
&             '',UNITE'',I3,'',NUM.ART'',I6,'',*'',I6,    &
&             '' MOTS'')') CDACTI(1:ILACTI),KCODE,KNUMER, &
&                           LFI%NUMAPH(IJL),              &
&                           LFI%JPLARD*LFI%MFACTM(IJL)
    ELSE
      WRITE (UNIT=CLMESS,                                         &
&             FMT='(''ERREUR "'',A,''" FORTRAN, CODE=''            &
&             ,I7,'', UNITE='',I3)') CDACTI(1:ILACTI),KCODE,KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-1) THEN
    WRITE (UNIT=CLMESS,FMT='(''UNITE LOGIQUE'',I3,         &
&           '' NON OUVERTE POUR LE LOGICIEL LFI'')') KNUMER
!
  ELSEIF (KCODE.EQ.-2) THEN
!
    IF (KNUMER.EQ.LFI%JPNIL) THEN
      CLMESS='PARAMETRE DE NIVEAU "KNIVAU" HORS PLAGE [0-2]'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                                      &
&   '(''NIVEAU DE MESSAGERIE HORS PLAGE [0-2], UNITE'',I3)') KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-3) THEN
    ILDMES=MIN (8_JPLIKB ,INT (LEN (CDMESS), JPLIKB))
    CLMESS='ACTION '''//CDMESS(1:ILDMES)   &
&           //''' INCONNUE SUR LES VERROUS'
!
  ELSEIF (KCODE.EQ.-4) THEN
    CLMESS='CHANGEMENT MODE MULTI-TACHES AVEC ' &
&               //'UNITE(S) OUVERTE(S)'
!
  ELSEIF (KCODE.EQ.-5) THEN
    WRITE (UNIT=CLMESS,FMT='(''UNITE LOGIQUE'',I3,                &
&           '' DEJA OUVERTE POUR LFI - NE DEVRAIT PAS.'')') KNUMER
!
  ELSEIF (KCODE.EQ.-6) THEN
    WRITE (UNIT=CLMESS,FMT='(I3,'' ENTREES,'',            &
&    '' PLUS ASSEZ DE PLACE DANS LES TABLES, UNITE'',I3)') &
&    LFI%JPNXFI,KNUMER
!
  ELSEIF (KCODE.EQ.-7) THEN
    WRITE (UNIT=CLMESS,FMT='(''STATUS FORTRAN '''''',A,          &
&           '''''' INCONNU, UNITE'',I3)') CDACTI(1:ILACTI),KNUMER
!
  ELSEIF (KCODE.EQ.-8) THEN
    WRITE (UNIT=CLMESS,                                            &
&           FMT='(''L''''UNITE'',I3,'' DE STATUS ''''''             &
&,A,'''''' DOIT AVOIR UN NOM EXPLICITE'')') KNUMER,CDACTI(1:ILACTI)
!
  ELSEIF (KCODE.EQ.-9) THEN
!
  IF (CDACTI.EQ.'OLD') THEN
    WRITE (UNIT=CLMESS,FMT=                                      &
&'(''STATUS ''''OLD'''' MAIS LE FICHIER N''''EXISTE PAS, UNITE'', &
&      I3)') KNUMER
  ELSE
    ILBLAN=INT (INDEX (CDACTI(1:ILACTI),' '), JPLIKB)
    IF (ILBLAN.GT.1) ILACTI=ILBLAN-1
    WRITE (UNIT=CLMESS,FMT=                                      &
&'(''STATUS '''''',A,'''''' MAIS LE FICHIER EXISTE DEJA, UNITE'', &
&  I3)') CDACTI(1:ILACTI),KNUMER
  ENDIF
!
  ELSEIF (KCODE.EQ.-10) THEN
    WRITE (UNIT=CLMESS,FMT='(''INCOMPATIBILITE'',      &
&           '' FICHIER / LOGICIEL, UNITE'',I3)') KNUMER
!
  ELSEIF (KCODE.EQ.-11) THEN
    WRITE (UNIT=CLMESS,                               &
&           FMT='(''UNITE'',I3,'' NON FERMEE APRES '', &
&           ''LA DERNIERE MODIFICATION'')') KNUMER
!
  ELSEIF (KCODE.EQ.-12) THEN
    WRITE (UNIT=CLMESS,FMT='(''UNITE'',I3,                       &
&  '' DE STATUS ''''OLD'''' - ERREUR LECTURE PREMIER ARTICLE'')') &
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
      INLNOM=MIN (LFI%NLNOMF(IJ),INT (LEN (CLMESS), JPLIKB)-3)
      GOTO 132
    ENDIF
!
    ENDDO
!
132 CONTINUE
    CLMESS=' '''//CDACTI(1:INLNOM)//''''
    WRITE (UNIT=LFI%NULOUT,FMT='(A)') TRIM(CLMESS)
    WRITE (UNIT=CLMESS,FMT='(''UNITE'',I3,'' - FICHIER '',     &
&           ''DEJA OUVERT POUR L''''UNITE'',I3)') KNUMER,INUMER
!
  ELSEIF (KCODE.EQ.-14) THEN
!
    IF (CDNSPR.EQ.'LFIECR'.OR.CDNSPR.EQ.'LFILEC'.OR.   &
&        CDNSPR.EQ.'LFILAS'.OR.CDNSPR.EQ.'LFILAP') THEN
      WRITE (UNIT=CLMESS,FMT=                                 &
&   '(''LONGUEUR D''''ARTICLE INCORRECTE, UNITE'',I3)') KNUMER
    ELSEIF (KNUMER.EQ.LFI%JPNIL) THEN
      CLMESS='RANG DANS LA TABLE *LFI%NUMERO* INCORRECT'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                                  &
&   '(''ARGUMENT DE TYPE ENTIER INCORRECT, UNITE'',I3)') KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-15) THEN
    WRITE (UNIT=CLMESS,                              &
&           FMT='(''NOM D''''ARTICLE INCORRECT OU '', &
&           ''TROP LONG, UNITE'',I3)') KNUMER
!
  ELSEIF (KCODE.EQ.-16) THEN
    WRITE (UNIT=CLMESS,                                      &
&           FMT='(''INCOHERENCE (TABLES, FICHIER, '',         &
&           ''APPELS S/P INT, LOGICIEL), UNITE'',I3)') KNUMER
!
  ELSEIF (KCODE.EQ.-17) THEN
!
    IF (IJL.NE.0) THEN
      INBALO=LFI%MDES1D(IXM(LFI%JPNALO,IJL))
    ELSE
      INBALO=LFI%JPNIL
    ENDIF
!
    WRITE (UNIT=CLMESS,                                &
&           FMT='(I6,'' ARTICLES, INDEX PLEIN, UNITE'', &
&           I3)') INBALO,KNUMER
!
  ELSEIF (KCODE.EQ.-18) THEN
    WRITE (UNIT=CLMESS,                             &
&           FMT='(''ARTICLE DE NOM BLANC ILLICITE'', &
&           '', UNITE'',I3)') KNUMER
!
  ELSEIF (KCODE.EQ.-19) THEN
    WRITE (UNIT=CLMESS,FMT='(''UNITE'',I3,                        &
&           '' ''''SCRATCH'''', NE PEUT ETRE CONSERVEE'')') KNUMER
!
  ELSEIF (KCODE.EQ.-20) THEN
    WRITE (UNIT=CLMESS,FMT='(''ARTICLE "'',A,                    &
&           ''" NON TROUVE, UNITE'',I3)') CDACTI(1:ILACT2),KNUMER
!
  ELSEIF (KCODE.EQ.-21) THEN
    WRITE (UNIT=CLMESS,FMT='(''ARTICLE "'',A, &
&    ''" + *LONG* QUE DEMANDE, UNITE'',I3)')   &
&      CDACTI(1:ILACT2),KNUMER
!
  ELSEIF (KCODE.EQ.-22) THEN
    WRITE (UNIT=CLMESS,FMT='(''ARTICLE "'',A, &
&    ''" + *COURT* QUE DEMANDE, UNITE'',I3)')  &
&      CDACTI(1:ILACT2),KNUMER
!
  ELSEIF (KCODE.EQ.-23) THEN
    WRITE (UNIT=CLMESS,                                &
&           FMT='(''PAS OU PLUS D''''ARTICLE SUIVANT'', &
&    '' A LIRE, UNITE'',I3)') KNUMER
!
  ELSEIF (KCODE.EQ.-24) THEN
    WRITE (UNIT=CLMESS,FMT='(''VARIABLE CAR.TROP COURTE '', &
&    ''POUR "'',A,''", UNITE'',I3)')                         &
&      CDACTI(1:ILACT2),KNUMER
!
  ELSEIF (KCODE.EQ.-25) THEN
    WRITE (UNIT=CLMESS,                             &
&           FMT='(''NOUVEAU NOM D''''ARTICLE: "'',A, &
&    ''" DEJA UTILISE, UNITE'',I3)')                 &
&      CDACTI(1:ILACT2),KNUMER
!
  ELSEIF (KCODE.EQ.-26) THEN
    WRITE (UNIT=CLMESS,FMT='(''PAS OU PLUS D''''ARTICLE '', &
&    '' PRECEDENT A LIRE, UNITE'',I3)') KNUMER
!
  ELSEIF (KCODE.EQ.-27) THEN
    WRITE (UNIT=CLMESS,                                &
&           FMT='(''ESPACE CONTIGU INSUFFISANT DANS '', &
&    '' LES TABLES, UNITE'',I3)') KNUMER
!
  ELSEIF (KCODE.EQ.-28) THEN
!
    IF (KNUMER.EQ.LFI%JPNIL) THEN
      WRITE (UNIT=CLMESS,FMT='(''FACTEUR MULTIPLICATIF PAR '', &
&      ''DEFAUT SUPERIEUR AU MAXIMUM('',I3,'')'')') LFI%JPFACX
    ELSE
      WRITE (UNIT=CLMESS,FMT='(''FACTEUR MULTIPLICATIF '',     &
&      ''DEMANDE SUPERIEUR AU MAXIMUM ('',I3,''), UNITE'',I3)') &
&        LFI%JPFACX,KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-29) THEN
    WRITE (UNIT=CLMESS,FMT='(I3,'' ENTREES,'',            &
&    '' PAS DE PLACE POUR FACTEUR MULTIPLIC, UNITE'',I3)') &
&    LFI%JPXUFM,KNUMER
!
  ELSEIF (KCODE.EQ.-30) THEN
    WRITE (UNIT=CLMESS,                                    &
&           FMT='(''LFI%NUMERO D''''UNITE LOGIQUE FORTRAN'' &
&           ,I8,'' ILLICITE'')') KNUMER
!
  ELSEIF (KCODE.EQ.-31) THEN
    WRITE (UNIT=CLMESS,FMT='(''LFI%NUMERO UNITE LOGIQ'',I3, &
&       '' SANS FACTEUR MULTIPLICATIF PREDEFINI'')') KNUMER
!
!                  Pour les codes d'erreur non prevus...
!
  ELSEIF (KNUMER.EQ.LFI%JPNIL) THEN
    WRITE (UNIT=CLMESS,                                    &
&           FMT='(''ERREUR GLOBALE *INCONNUE* LFI%NUMERO'', &
&                             I6)') KCODE
  ELSE
    WRITE (UNIT=CLMESS,                               &
&           FMT='(''ERREUR *INCONNUE* LFI%NUMERO'',I6, &
&           '' SUR UNITE LOGIQUE'',I3)') KCODE,KNUMER
  ENDIF
!
  ILMESA=INT (LEN (CLMESA), JPLIKB)
  ILMESU=ILMESA-1-2*INT (LEN (CLJOLI), JPLIKB)-ILNSPR-4
  CLMESA=CLJOLI//' '//CDNSPR(1:ILNSPR)//' - ' &
&         //CLMESS(1:ILMESU)//CLJOLI
  WRITE (UNIT=LFI%NULOUT,FMT='(A)') CLMESA
!
!           Si l'unite logique correspond a une unite logique LFI
!     deja ouverte, on en imprime le nom.
!
  IF (IJL.NE.0) THEN
!
    IF (LFI%NLNOMF(IJL).LE.LFI%JPLFTX) THEN
      WRITE (UNIT=LFI%NULOUT,FMT='(A,/)') CLJOLI               &
&             //' NOM - APPARENT MAIS'                          &
&             //' COMPLET - DE L''UNITE LOGIQUE LFI CONCERNEE:'
    ELSE
      WRITE (UNIT=CLMESS,FMT='(A,                                &
&             '' NOM - APPARENT, ET TRONQUE DE'',I4,              &
&       '' CARACTERES - DE L''''UNITE LOGIQUE LFI CONCERNEE:'')') &
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
      WRITE (UNIT=LFI%NULOUT,FMT='(A,/)')             &
&            LFI%CNOMFI(IJL)(IDECAL+1:LFI%NLNOMF(IJL))
    ELSE
      WRITE (UNIT=LFI%NULOUT,FMT='(A,/)')                &
&             LFI%CNOMFI(IJL)(IDECAL+1:LFI%JPLFTX)//'...'
    ENDIF
!
    IF (LFI%CNOMSY(IJL).NE.LFI%CNOMFI(IJL)) THEN
      WRITE (UNIT=LFI%NULOUT,FMT='(A,/)') CLJOLI//              &
& ' NOM *SYSTEME* (APPARENT) DE L''UNITE LOGIQUE LFI CONCERNEE:'
      INLIGN=(LFI%NLNOMS(IJL)-1)/LFI%JPLFIX
      IDECAL=0
!
      DO J=1,INLIGN
      WRITE (UNIT=LFI%NULOUT,FMT='(A)')                         &
&             LFI%CNOMSY(IJL)(IDECAL+1:IDECAL+LFI%JPLFIX)//'...'
      IDECAL=IDECAL+LFI%JPLFIX
      ENDDO
!
      WRITE (UNIT=LFI%NULOUT,FMT='(A,/)')             &
&            LFI%CNOMSY(IJL)(IDECAL+1:LFI%NLNOMS(IJL))
    ENDIF
!
  ENDIF
!
  WRITE (UNIT=LFI%NULOUT,FMT='(A)') CLMESA
  IF (LDFATA.AND.KCODE.NE.0) THEN
!
!            Saborde le programme.
!
    CALL FLUSH (INT (LFI%NULOUT))
    CALL SDL_SRLABORT
  ENDIF
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('LFIEFR_FORT',1,ZHOOK_HANDLE)

CONTAINS

#include "lficom2.ixm.h"

END SUBROUTINE LFIEFR_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFIEFR64                                       &
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

CALL LFIEFR_FORT                                               &
&           (LFI, KNUMER, KNIMES, KCODE, LDFATA, CDMESS, CDNSPR, &
&           CDACTI)

END SUBROUTINE LFIEFR64

SUBROUTINE LFIEFR                                         &
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

CALL LFIEFR_MT                                                 &
&           (LFI, KNUMER, KNIMES, KCODE, LDFATA, CDMESS, CDNSPR, &
&           CDACTI)

END SUBROUTINE LFIEFR

SUBROUTINE LFIEFR_MT                                           &
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

CALL LFIEFR_FORT                                               &
&           (LFI, INUMER, INIMES, ICODE, LDFATA, CDMESS, CDNSPR, &
&           CDACTI)


END SUBROUTINE LFIEFR_MT

!INTF KNUMER        IN    
!INTF KNIMES        IN    
!INTF KCODE         IN    
!INTF LDFATA        IN    
!INTF CDMESS        IN    
!INTF CDNSPR        IN    
!INTF CDACTI        IN    
