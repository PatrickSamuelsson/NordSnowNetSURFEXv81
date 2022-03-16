! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FAIPAR_FORT                                            &
&                     (FA,  KNUMER, KNIMES, KCODE, LDFATA, CDMESS,  &
&                      CDNSPR, CDACTI, LDRLFI )
USE FA_MOD, ONLY : FA_COM, JPNIIL
USE PARKIND1, ONLY : JPRB
USE YOMHOOK, ONLY : LHOOK, DR_HOOK
USE SDL_MOD, ONLY : SDL_SRLABORT
USE OML_MOD, ONLY : OML_MY_THREAD, OML_IN_PARALLEL
USE LFI_PRECISION
IMPLICIT NONE
!**** 
!        Ce sous-programme est charge de FAIre PARt des Messages
!     emis par le logiciel de Fichiers ARPEGE, en faisant si besoin est
!     l'"ABORT" du programme .
!**
!        Arguments : KNUMER ==> Numero de l'unite logique eventuelle;
!                               ( JPNIIL ==> pas d'unite logique )
!                    KNIMES ==> Niveau (0,1,2) du message;
!                    KCODE  ==> Code correspondant a l'action;
!                    LDFATA ==> Vrai si on doit avorter le programme;
!                    CDMESS ==> si KNIMES#0, Message a emettre;
!                    CDNSPR ==> Nom du Sous-Programme appelant;
!                    CDACTI ==> Nom de l'action d'Entree/Sortie FORTRAN
!                               (si KCODE >0), sinon fourre-tout !) .
!                    LDRLFI ==> Vrai si le code-reponse different de 0
!                               a ete detecte par le logiciel LFI.
!*
!     VALEURS POSSIBLES DES CODES-REPONSES DU LOGICIEL FICHIERS ARPEGE
!
!      0 ==> Aucune erreur detectee, message informatif.
!     >0 ==> Il s'agit du code-reponse d'une erreur detectee lors d'un
!            OPEN, READ, WRITE, CLOSE, ou INQUIRE FORTRAN; voir manuels.
! -(1a50)==> Code-reponse renvoye par le logiciel LFI.
!    -51 ==> Unite logique non ouverte pour le logiciel,
!             ou bien cadre non defini .
!    -52 ==> Valeur d'un "NIVEAU" hors plage [0-2] .
!    -53 ==> *** code-reponse inutilise pour le moment ***.
!    -54 ==> Changement explicite de mode Multi-Tasking avec au moins 1
!            unite logique ouverte-risque de problemes (s/p "FARINE") .
!    -55 ==> Unite logique deja ouverte ou cadre non redefinissable.
!    -56 ==> Trop d'unites logiques deja ouvertes ou de cadres definis.
!    -57 ==> Nouveau fichier, et cadre reference non defini au prealable
!    -58 ==> Fichier preexistant, et cadre reference non compatible .
!    -59 ==> Redefinition de cadre avec modification de parametre(s),
!            impossible car il y a au moins un fichier qui s'y rattache.
!    -60 ==> Un des articles definissant le cadre est manquant.
!    -61 ==>  "  "      "        "       "  " a une longueur inattendue.
!    -62 ==> L'article DATE est manquant.
!    -63 ==> L'article DATE a une longueur inattendue.
!    -64 ==> Argument d'appel de type ENTIER incorrect .
!    -65 ==> Argument d'appel de type CARACTERE incorrect .
!    -66 ==> INCOHERENCE tables, fichier, appels s/p internes, logiciel;
!            CE CODE EST LE SEUL QUE *FA%NRFAGA* VALANT 2 NE PEUT MASQUER,
!            ET PROVOQUE DONC UNE ERREUR FATALE DANS TOUS LES CAS.
!    -67 ==> Le cadre reference a au moins un fichier associe,
!            et ne peut donc pas etre supprime.
!    -68 ==> Nom d'article compose uniquement de BLANCS, non accepte
!            ( a cause du fonctionnement interne du logiciel LFI ),
!            ou cadre de nom blanc non accepte (les cadres non definis
!             sont reperes ainsi dans les tables du logiciel)
!    -69 ==> La variable CARACTERE donnee en argument est TROP COURTE
!            pour recevoir le NOM du cadre ou d'identificateur, meme en
!            supprimant les eventuels caracteres blancs en fin de nom .
!    -70 ==> Troncature hors plage [1-FA%NXTRON].
!    -71 ==> Nombre de latitudes de pole a pole hors plage [1-FA%NXLATI].
!    -72 ==> Nombre de niveaux verticaux hors plage [1-FA%NXNIVV].
!    -73 ==> Coefficient de dilatation inferieur a 1.
!    -74 ==> Un des nombres de points par parallele est <= 0 ou depasse
!            le nombre maximum annonce dans la description du cadre.
!    -75 ==> Un des nombres de points par parallele est inferieur
!            a celui qui le precede.
!    -76 ==> Un des nombres d'onde zonal maxi par parallele depasse
!            la troncature ou n'est pas positif.
!    -77 ==> Un des nombres d'onde zonal maxi par parallele est
!            inferieur a celui qui le precede.
!    -78 ==> Sur une latitude au moins, le nombre de points est insuffi-
!            sans vis-a-vis du nombre d'onde zonal maximum.
!    -79 ==> Inconsistance: le nombre de latitudes (de pole a pole)
!            est insuffisant vis-a-vis de la troncature.
!    -80 ==> Une valeur d'une des fonctions de la coordonnee hybride
!            est en-dehors de l'intervalle [0,1] .
!    -81 ==> Pour une couche, valeurs inadaptees des fonctions de la
!            coordonnee hybride.
!    -82 ==> L'article DATE a un contenu incorrect.
!    -83 ==> Le nombre maximum de points par parallele est en-dehors de
!            l'intervalle [1,FA%NXLONG]
!    -84 ==> Le nombre maximum de points par parallele est insuffisant
!            vis-a-vis de la troncature.
!    -85 ==> Fichier en mode creation, donc vide d'articles vis-a-vis
!            du logiciel (a la description du Cadre pres):
!            il faut le munir d'une date...
!    -86 ==> Prefixe et/ou suffixe "blanc" interdit.
!    -87 ==> Prefixe de champ trop long.
!    -88 ==> Troncature effective inferieure ou egale a
!            la sous-troncature "non compactee".
!    -89 ==> Article de type champ demande inexistant dans le fichier.
!    -90 ==> L'article de type champ existe, mais est trop long.
!    -91 ==> En-tete d'article champ incorrect.
!    -92 ==> L'article demande est en points-de-grille au lieu de
!            coefficients spectraux, ou vice-versa.
!    -93 ==> L'article demande est trop COURT sur le fichier.
!    -94 ==> L'article demande est trop LONG sur le fichier ( si cette
!            erreur n'est pas fatale, on traite le debut de l'article ).
!    -95 ==> Incoherence entre entete d'article champ et zone GRIB.
!    -96 ==> Niveau de codage GRIB trop grand.
!    -97 ==> Nombre de bits de codage trop grand.
!    -98 ==> Puissance de laplacien trop grande.
!    -99 ==> Sous-troncature superieure ou egale a la troncature
!   -100 ==> Une des lignes trigonometriques definissant le pole de
!            projection n'est pas dans [-1,1] .
!   -101 ==> Cosinus et Sinus de la longitude du pole de projection
!            incoherents.
!   -102 ==> Le Sinus d'une latitude de la grille n'est pas dans [-1,1].
!   -103 ==> Le Sinus d'une latitude est superieur ou egal a celui de
!            la latitude precedente.
!   -104 ==> Troncature maxi. incompatible avec cadre(s) deja defini(s).
!   -105 ==> Nombre maxi. de niveaux verticaux "  "       "      "     .
!   -106 ==> Nombre maxi. de latitudes pole a pole"       "      "     .
!   -107 ==> Nombre maxi. de longitudes "      "  "       "      "     .
!   -108 ==> Pression de reference aberrante.
!   -109 ==> Type de transformation horizontale hors plage [1-FA%NTYPTX]
!   -110 ==> Pas d'article "identificateur" (apres ceux du cadre)
!   -111 ==> Article de nom reserve, a ne pas ecrire ou lire ainsi
!   -112 ==> Caracteristiques des fichiers vraiment incompatibles,
!            recopie d'article "champ" impossible.
!   -113 ==> Sous-troncature implicite superieure ou egale a la
!            limite usager de troncature.
!   -114 ==> Ratio des troncatures (version LAM) superieur a 3.
!            Garde-fou, modifiable dans FARCIS, FACSIM et FAPULA.
!   -115 ==> (LAM) Le nombre maximum de points par parallele est insuffisant
!            vis-a-vis de la troncature.
!   -116 ==> (LAM) Le nombre maximum de points par meridien est insuffisant
!            vis-a-vis de la troncature.
!   -117 ==> (LAM) L'indicateur NDOMFP a une valeur hors de [-1,1]
!   -118 ==> (LAM) L'indice de depart (C+I) en longitude est hors plage [1-KNXLON].
!   -119 ==> (LAM) L'indice de fin (C+I) en longitude est errone.
!   -120 ==> (LAM) L'indice de depart (C+I) en latitude est hors plage [1-KNLATI].
!   -121 ==> (LAM) L'indice de fin (C+I) en latitude est errone.
!   -122 ==> (LAM) La largeur de la zone de relaxation est trop large en longitude.
!   -123 ==> (LAM) La largeur de la zone de relaxation est trop large en latitude.
!   -124 ==> Nombre de bits de codage nul alors qu'un codage est demande
!   -125 ==> Argument ayant une valeur incorrecte
!   -126 ==> Detection d'une incoherence dans un controle interne
!   -127 ==> (LAM) Description incoherente de l'ellipse contenant les troncatures
!   -128 ==> Message GRIB incorrect dans l'appel a DECF10
!   -129 ==> Impossible d'encoder ce champ avec ces reglages
!   -130 ==> Tableau trop court pour encoder ce champ
! <-1000 ==> Erreur rapportee par GRIBEX
!


!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KNUMER, KNIMES, KCODE
!
INTEGER (KIND=JPLIKB) ILNSPR, ILACTI, ILACT2
INTEGER (KIND=JPLIKB) ILMESU, ILMESA, INUME1, INUME2
!
LOGICAL LDFATA, LDRLFI
!
CHARACTER(LEN=*)  CDNSPR
CHARACTER(LEN=6)  CLJOLI
CHARACTER(LEN=6)  CLITID
CHARACTER(LEN=*)  CDMESS
CHARACTER(LEN=80) CLMESA
CHARACTER(LEN=*)  CDACTI
CHARACTER(LEN=FA%JPLMES) CLMESS 

!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FAIPAR_MT',0,ZHOOK_HANDLE)
ILNSPR=MIN (INT (LEN (CDNSPR), JPLIKB),6_JPLIKB )
ILACTI=MIN (INT (LEN (CDACTI), JPLIKB),8_JPLIKB )
ILACT2=MIN (INT (LEN (CDACTI), JPLIKB),FA%NCPCAD)
!
IF (LDFATA) THEN
  CLJOLI=' *****'
ELSEIF (KNIMES.EQ.0.OR.KCODE.NE.0) THEN
  CLJOLI=' */*/*'
ELSE
  CLJOLI=' /////'
ENDIF
IF (OML_IN_PARALLEL ()) THEN
  WRITE (CLITID, '(" @",I4.4)') OML_MY_THREAD ()
ELSE
  CLITID=''
ENDIF
!
IF (KNIMES.NE.0) THEN
  ILMESU=MIN (INT (LEN (CLMESS), JPLIKB)-          &
&              INT (LEN (CLJOLI), JPLIKB)-ILNSPR-4, &
&              INT (LEN (CDMESS), JPLIKB))
  CLMESS=CLJOLI//' '//CDNSPR(1:ILNSPR)//' - '//TRIM (CDMESS(1:ILMESU))//CLITID
  WRITE (UNIT=FA%NULOUT,FMT='(A)') TRIM (CLMESS)
ENDIF
!
IF (KNIMES.EQ.0.OR.LDFATA) THEN
!
!        CONSTITUTION D'UN MESSAGE D'ERREUR AD HOC, EN FONCTION DE KCODE
!
  IF (LDRLFI) THEN
    WRITE (UNIT=CLMESS,FMT='(''CODE-REPONSE DE *'',A6,''* ='',  &
&           I4,'', UNITE ='',I3)') CDACTI(1:ILACTI),KCODE,KNUMER
!
  ELSEIF (KCODE.GT.0) THEN
    WRITE (UNIT=CLMESS,                                         &
&           FMT='(''ERREUR "'',A,''" FORTRAN, CODE ='',          &
&           I4,'', UNITE ='',I3)') CDACTI(1:ILACTI),KCODE,KNUMER
!
  ELSEIF (KCODE.EQ.-51) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
      CLMESS='CADRE "'//CDACTI(1:ILACT2)//'" NON DEFINI'
    ELSE
      WRITE (UNIT=CLMESS,FMT='(''UNITE LOGIQUE'',I3, &
&             '' NON OUVERTE'')') KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-52) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
      CLMESS='PARAMETRE DE NIVEAU "KNIVAU" HORS PLAGE [0-2]'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                                      &
&   '(''NIVEAU DE MESSAGERIE HORS PLAGE [0-2], UNITE'',I3)') KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-53) THEN
    CLMESS='CADRE "'//CDACTI(1:ILACT2)//  &
&           '", PARAMETRE(S) INCORRECT(S)'
!
  ELSEIF (KCODE.EQ.-54) THEN
    CLMESS=                                                  &
&      'CHANGEMENT EXPLICITE MODE MULTI AVEC UNITES OUVERTES'
!
  ELSEIF (KCODE.EQ.-55) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
      CLMESS='CADRE "'//CDACTI(1:ILACT2)// &
&                '" NON REDEFINISSABLE'
    ELSE
      WRITE (UNIT=CLMESS,FMT='(''UNITE LOGIQUE'',I3, &
&             '' DEJA OUVERTE'')') KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-56) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
      WRITE (UNIT=CLMESS,FMT=                                     &
&         '(I2,'' CADRES DEFINIS, PAS DE PLACE POUR "'',A,''"'')') &
&      FA%JPNXCA,CDACTI(1:ILACT2)
    ELSE
      WRITE (UNIT=CLMESS,FMT='(I2,                                &
&           '' UNITES LOGIQUES OUVERTES, PAS DE PLACE POUR'',I3)') &
&      FA%JPNXFA,KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-57) THEN
    WRITE (UNIT=CLMESS,                                &
&           FMT='(''UNITE'',I3,'' A CREER, CADRE "'',A, &
&           ''" NON DEFINI'')') KNUMER,CDACTI(1:ILACT2)
!
  ELSEIF (KCODE.EQ.-58) THEN
    WRITE (UNIT=CLMESS,                                     &
&           FMT='(''UNITE'',I3,'' /CADRE PREDEFINI "'',      &
&           A,''" INCOMPATIBLES'')') KNUMER,CDACTI(1:ILACT2)
!
  ELSEIF (KCODE.EQ.-59) THEN
    CLMESS='CADRE "'//CDACTI(1:ILACT2)//   &
&           '"  NON MODIFIABLE CAR UTILISE'
!
  ELSEIF (KCODE.EQ.-60) THEN
    WRITE (UNIT=CLMESS,FMT=                                       &
&       '(''UN ARTICLE DU CADRE EST MANQUANT, UNITE'',I3)') KNUMER
!
  ELSEIF (KCODE.EQ.-61) THEN
!***** FAZZZZ - un article du cadre a une longueur inattendue, uniteiii *****
    WRITE (UNIT=CLMESS,FMT=                                        &
&  '(''UN ARTICLE DU CADRE A UNE LONGUEUR INATTENDUE, UNITE'',I3)') &
&    KNUMER
!
  ELSEIF (KCODE.EQ.-62) THEN
    WRITE (UNIT=CLMESS,FMT=                                    &
&       '(''PAS D''''ARTICLE DATE SUR L''''UNITE'',I3)') KNUMER
!
  ELSEIF (KCODE.EQ.-63) THEN
    WRITE (UNIT=CLMESS,FMT=                                        &
&'(''L''''ARTICLE DATE A UNE LONGUEUR INATTENDUE SUR L'''' UNITE'', &
&    I3)')  KNUMER
!
  ELSEIF (KCODE.EQ.-64) THEN
!
    IF (CDNSPR.EQ.'FAIENC') THEN
      WRITE (UNIT=CLMESS,FMT=                             &
&   '(''NIVEAU VERTICAL HORS LIMITES, UNITE'',I3)') KNUMER
    ELSEIF (CDNSPR.EQ.'FAISAN'.OR.CDNSPR.EQ.'FALAIS') THEN
      WRITE (UNIT=CLMESS,FMT=                          &
&   '(''LONGUEUR D''''ARTICLE <=0, UNITE'',I3)') KNUMER
    ELSEIF (KNUMER.EQ.JPNIIL) THEN
      CLMESS='RANG DANS UNE TABLE GLOBALE INCORRECT'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                          &
&   '(''ARGUMENT ENTIER INCORRECT, UNITE'',I3)') KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-65) THEN
!
    IF (CDNSPR.EQ.'FAIENC'.OR.CDNSPR.EQ.'FACILE'.OR.   &
&        CDNSPR.EQ.'FAISAN'.OR.CDNSPR.EQ.'FALAIS') THEN
      WRITE (UNIT=CLMESS,                              &
&             FMT='(''NOM D''''ARTICLE INCORRECT OU '', &
&           ''TROP LONG, UNITE'',I3)') KNUMER
    ELSEIF (KNUMER.EQ.JPNIIL) THEN
      CLMESS='NOM DE CADRE INCORRECT OU TROP LONG: "' &
&             //CDACTI(1:ILACT2)//'"'
    ELSE
      WRITE (UNIT=CLMESS,                              &
&             FMT='(''CHAINE DE CARACTERES INCORRECT'', &
&           ''E OU TROP LONGUE, UNITE'',I3)') KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-66) THEN
    WRITE (UNIT=CLMESS,                                        &
&           FMT='(''INCOHERENCE (TABLES, FICHIER, '',           &
&           ''APPELS S/P INT. OU FICHIER), UNITE'',I3)') KNUMER
!
  ELSEIF (KCODE.EQ.-67) THEN
    CLMESS='FICHIER ASSOCIE, CADRE"'//CDACTI(1:ILACT2) &
&           //'" DEVANT SUBSISTER'
!
  ELSEIF (KCODE.EQ.-68) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
      CLMESS='CADRE DE NOM BLANC NON ACCEPTE'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                                      &
&        '(''ARTICLE DE NOM BLANC NON ACCEPTE, UNITE'',I3)') KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-69) THEN
    WRITE (UNIT=CLMESS,FMT='(''VARIABLE CARACT. TROP COURTE '', &
&    ''POUR "'',A,''", UNITE'',I3)')                             &
&      CDACTI(1:ILACT2),KNUMER
!
  ELSEIF (KCODE.EQ.-70) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
      WRITE (UNIT=CLMESS,FMT='(''TRONCATURE HORS PLAGE [1-'', &
&                               I4,''], CADRE "'',A,''"'')')   &
&      FA%NXTRON,CDACTI(1:ILACT2)
    ELSE
      WRITE (UNIT=CLMESS,FMT='(''TRONCATURE HORS PLAGE [1-'', &
&                      I4,''], UNITE'',I4)') FA%NXTRON,KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-71) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
      WRITE (UNIT=CLMESS,FMT='(''NOMBRE LATITUDES HORS [1-'', &
&             I4,''], CADRE "'',A,''"'')')                     &
&             FA%NXLATI,CDACTI(1:ILACT2)
    ELSE
      WRITE (UNIT=CLMESS,FMT='(''NOMBRE LATITUDES HORS [1-'', &
&             I4,''], UNITE'',I3)') FA%NXLATI,KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-72) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
      WRITE (UNIT=CLMESS,FMT='(''NB. NIV. VERTICAUX HORS [1-'', &
&             I3,''], CADRE "'',A,''"'')')                       &
&             FA%NXNIVV,CDACTI(1:ILACT2)
    ELSE
      WRITE (UNIT=CLMESS,FMT='(''NB. NIV. VERTICAUX HORS [1-'', &
&             I3,''], UNITE'',I3)') FA%NXNIVV,KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-73) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
      CLMESS='COEFF. DILATATION INFERIEUR A 1, CADRE "' &
&             //CDACTI(1:ILACT2)//'"'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                                      &
&         '(''COEFF. DILATATION INFERIEUR A 1, UNITE'',I3)') KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-74) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
      CLMESS='1 NB. POINTS/PARALL. HORS PLAGE, CADRE "' &
&             //CDACTI(1:ILACT2)//'"'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                                      &
&         '(''1 NB. POINTS/PARALL. HORS PLAGE, UNITE'',I3)') KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-75) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
      CLMESS='LISTE POINTS/PARALL. INCORRECTE, CADRE "' &
&             //CDACTI(1:ILACT2)//'"'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                                      &
&         '(''LISTE POINTS/PARALL. INCORRECTE, UNITE'',I3)') KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-76) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
      CLMESS='1 N.O.Z. MAX/PARALL. HORS PLAGE, CADRE "' &
&             //CDACTI(1:ILACT2)//'"'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                                      &
&         '(''1 N.O.Z. MAX/PARALL. HORS PLAGE, UNITE'',I3)') KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-77) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
      CLMESS='LISTE NOZMAX/PARALL. INCORRECTE, CADRE "' &
&             //CDACTI(1:ILACT2)//'"'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                                      &
&         '(''LISTE NOZMAX/PARALL. INCORRECTE, UNITE'',I3)') KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-78) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
      CLMESS='INCONSISTANCE NOZMAX/NB. POINTS, CADRE "' &
&             //CDACTI(1:ILACT2)//'"'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                                      &
&         '(''INCONSISTANCE NOZMAX/NB. POINTS, UNITE'',I3)') KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-79) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
      CLMESS='INCONSISTANCE NB.LAT/TRONCATURE, CADRE "' &
&             //CDACTI(1:ILACT2)//'"'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                                      &
&         '(''INCONSISTANCE NB.LAT/TRONCATURE, UNITE'',I3)') KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-80) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
      CLMESS='FONC. COORD. HYBRIDE HORS [0,1], CADRE "' &
&             //CDACTI(1:ILACT2)//'"'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                                      &
&         '(''FONC. COORD. HYBRIDE HORS [0,1], UNITE'',I3)') KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-81) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
!***** FAZZZZ - fonc. coord. hybride inadaptees, CADRE "0123456789ABCDEF" *****
      CLMESS='FONC. COORD. HYBRIDE INADAPTEES, CADRE "' &
&             //CDACTI(1:ILACT2)//'"'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                                      &
&         '(''FONC. COORD. HYBRIDE INADAPTEES, UNITE'',I3)') KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-82) THEN
!
    IF (CDNSPR.EQ.'FANDAR') THEN
!***** FAZZZZ - La DATE proposee a un contenu incorrect, UNITEiii *****
      WRITE (UNIT=CLMESS,FMT=                                     &
&       '(''LA DATE PROPOSEE A UN CONTENU INCORRECT, UNITE'',I3)') &
&      KNUMER
    ELSE
      WRITE (UNIT=CLMESS,FMT=                                      &
&       '(''L''''ARTICLE DATE A UN CONTENU INCORRECT, UNITE'',I3)') &
&      KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-83) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
!***** FAZZZZ - Nb. Max. longitu. hors [1-iiii], CADRE "0123456789abcdef" *****
      WRITE (UNIT=CLMESS,FMT='(''NB. MAX. LONGITU. HORS [1-'', &
&             I4,''], CADRE "'',A,''"'')')                      &
&             FA%NXLONG,CDACTI(1:ILACT2)
    ELSE
      WRITE (UNIT=CLMESS,FMT='(''NB. MAX. LONGITU. HORS [1-'', &
&             I4,''], UNITE'',I3)') FA%NXLONG,KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-84) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
      CLMESS='INCONSISTANCE KNXLON/TRONCATURE, CADRE "' &
&             //CDACTI(1:ILACT2)//'"'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                                      &
&         '(''INCONSISTANCE KNXLON/TRONCATURE, UNITE'',I3)') KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-85) THEN
    WRITE (UNIT=CLMESS,FMT='(''UNITE'',I3,               &
&           '' PAS ENCORE MUNIE D''''UNE DATE'')') KNUMER
!
  ELSEIF (KCODE.EQ.-86) THEN
    WRITE (UNIT=CLMESS,FMT=                                       &
&         '(''PREFIXE/SUFFIXE BLANC INTERDIT, UNITE'',I3)') KNUMER
!
  ELSEIF (KCODE.EQ.-87) THEN
    WRITE (UNIT=CLMESS,FMT=                                   &
&         '(''PREFIXE DE CHAMP TROP LONG, UNITE'',I3)') KNUMER
!
  ELSEIF (KCODE.EQ.-88) THEN
    WRITE (UNIT=CLMESS,FMT=                                        &
&  '(''TRONCATURE <= SOUS-TRONCATURE "NON COMPACTEE", UNITE'',I3)') &
&       KNUMER
!
  ELSEIF (KCODE.EQ.-89) THEN
    WRITE (UNIT=CLMESS,FMT='(''ARTICLE-CHAMP "'',A,              &
&           ''" INEXISTANT, UNITE'',I3)') CDACTI(1:ILACT2),KNUMER
!
  ELSEIF (KCODE.EQ.-90) THEN
    WRITE (UNIT=CLMESS,FMT='(''ARTICLE-CHAMP "'',A,             &
&           ''" TROP LONG, UNITE'',I3)') CDACTI(1:ILACT2),KNUMER
!
  ELSEIF (KCODE.EQ.-91) THEN
!***** FAZZZZ - Entete d'article "0123456789abcdef" incorrect, uniteiii *****
    WRITE (UNIT=CLMESS,FMT='(''ENTETE D''''ARTICLE "'',A,       &
&           ''" INCORRECT, UNITE'',I3)') CDACTI(1:ILACT2),KNUMER
!
  ELSEIF (KCODE.EQ.-92) THEN
!***** FAZZZZ - Desaccord Csp./pdg., Article "0123456789abcdef", uniteiii *****
    WRITE (UNIT=CLMESS,                                &
&           FMT='(''DESACCORD CSP./PDG., ARTICLE "'',A, &
&           ''" , UNITE'',I3)') CDACTI(1:ILACT2),KNUMER
!
  ELSEIF (KCODE.EQ.-93) THEN
    WRITE (UNIT=CLMESS,FMT='(''ARTICLE "'',A,      &
&           ''" TROP *COURT* SUR L''''UNITE'',I3)') &
&        CDACTI(1:ILACT2),KNUMER
!
  ELSEIF (KCODE.EQ.-94) THEN
    WRITE (UNIT=CLMESS,FMT='(''ARTICLE "'',A,     &
&           ''" TROP *LONG* SUR L''''UNITE'',I3)') &
&        CDACTI(1:ILACT2),KNUMER
!
  ELSEIF (KCODE.EQ.-95) THEN
!***** FAZZZZ - inconsis entete/GRIB article "0123456789abcdef", uniteiii *****
    WRITE (UNIT=CLMESS,                                &
&           FMT='(''INCONSIS ENTETE/GRIB ARTICLE "'',A, &
&           ''", UNITE'',I3)')                          &
&        CDACTI(1:ILACT2),KNUMER
!
  ELSEIF (KCODE.EQ.-96) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
      CLMESS='NIVEAU IMPLICITE DE CODAGE GRIB TROP GRAND'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                                     &
&       '(''NIVEAU DE CODAGE GRIB TROP GRAND, UNITE'',I3)') KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-97) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
      WRITE (UNIT=CLMESS,FMT=                                  &
&  '(''NOMBRE(S) IMPLICITE(S) DE BITS PAR VALEUR > MAXI ('',I2, &
&      '')'')')   FA%NBIMAX
    ELSE
      WRITE (UNIT=CLMESS,FMT=                                  &
&  '(''NOMBRE(S) DE BITS PAR VALEUR > MAXI ('',I2,''), UNITE'', &
&      I3)')   FA%NBIMAX,KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-98) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
      CLMESS=                                         &
&       'PUISSANCE IMPLICITE DE LAPLACIEN TROP GRANDE'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                              &
&     '(''PUISSANCE DE LAPLACIEN TROP GRANDE, UNITE'',I3)') &
&         KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-99) THEN
!
    IF (CDNSPR.EQ.'FARFLU') THEN
      CLMESS=                                                     &
&       'TRONCATURE MAX INFERIEURE OU = SOUS-TRONCATURE IMPLICITE'
    ELSEIF (KNUMER.EQ.JPNIIL) THEN
      CLMESS=                                                     &
&       'SS-TRONCATURE IMPLICITE SUPERIEURE OU = A UNE TRONCATURE'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                                      &
&     '(''SOUS-TRONCATURE SUPERIEURE OU = A LA TRONCATURE, UNITE'', &
&       I3)')  KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-100) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
      CLMESS='SIN/COS POLE PROJEC. HORS [-1,1], CADRE "' &
&             //CDACTI(1:ILACT2)//'"'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                                      &
&     '(''SINUS/COSINUS DU POLE DE PROJECTION HORS [-1,1], UNITE'', &
&       I3)')  KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-101) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
      CLMESS='INCOHERENCE COS/SIN LONGIT. POLE, CADRE "' &
&             //CDACTI(1:ILACT2)//'"'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                                    &
&     '(''INCOHERENCE COS/SIN LONGITUDE POLE PROJECTION, UNITE'', &
&       I3)')  KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-102) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
      CLMESS='SINUS D''UNE LATITUDE HORS [-1,1], CADRE "' &
&             //CDACTI(1:ILACT2)//'"'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                                      &
&    '(''SINUS D''''UNE LATITUDE DE LA GRILLE HORS [-1,1], UNITE'', &
&       I3)')  KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-103) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
!***** FAZZZZ - sinus latitudes non decroissants, cadre '0123456789abcdef" *****
      CLMESS='SINUS LATITUDES NON DECROISSANTS, CADRE "' &
&             //CDACTI(1:ILACT2)//'"'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                          &
&    '(''SINUS DES LATITUDES NON DECROISSANTS, UNITE'', &
&       I3)')  KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-104) THEN
!
    CLMESS=                                                      &
&     'TRONCATURE MAXI INCOMPATIBLE AVEC CADRE(S) DEJA DEFINI(S)'
!
  ELSEIF (KCODE.EQ.-105) THEN
!
    CLMESS=                                                      &
&     'NB MAX NIVEAUX VERTICAUX/CADRE(S) DEFINI(S) INCOMPATIBLES'
!
  ELSEIF (KCODE.EQ.-106) THEN
!
    CLMESS=                                                      &
&     'NOMBRE MAXI DE LATITUDES/CADRE(S) DEFINI(S) INCOMPATIBLES'
!
  ELSEIF (KCODE.EQ.-107) THEN
!
    CLMESS=                                                      &
&     'NOMBRE MAX DE LONGITUDES/CADRE(S) DEFINI(S) INCOMPATIBLES'
!
  ELSEIF (KCODE.EQ.-108) THEN
!
!***** FAZZZZ - Pression de reference aberrante, cadre "0123456789abcdef" *****
    IF (KNUMER.EQ.JPNIIL) THEN
      CLMESS='PRESSION DE REFERENCE ABERRANTE, CADRE "' &
&             //CDACTI(1:ILACT2)//'"'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                                   &
&             '(''PRESSION DE REFERENCE ABERRANTE, UNITE'',I3)') &
&          KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-109) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
      WRITE (UNIT=CLMESS,                            &
&             FMT='(''TYPE TRANSFO HORIZ. HORS [1-'', &
&             I2,''], CADRE "'',A,''"'')')            &
&             FA%NTYPTX,CDACTI(1:ILACT2)
    ELSE
      WRITE (UNIT=CLMESS,                            &
&             FMT='(''TYPE TRANSFO HORIZ. HORS [1-'', &
&             I2,''], UNITE'',I3)') FA%NTYPTX,KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-110) THEN
    WRITE (UNIT=CLMESS,FMT=                                    &
&    '(''PAS D''''ARTICLE DERRIERE CEUX DU CADRE, UNITE'',I3)') &
&           KNUMER
!
  ELSEIF (KCODE.EQ.-111) THEN
    WRITE (UNIT=CLMESS,FMT=                                    &
&    '(''NOM RESERVE, NE PEUT ETRE UTILISE AINSI, UNITE'',I3)') &
&           KNUMER
!
  ELSEIF (KCODE.EQ.-112) THEN
    INUME1=KNUMER/1000
    INUME2=MOD (KNUMER,1000_JPLIKB )
    WRITE (UNIT=CLMESS,                                          &
&           FMT='(''COPIE DE '''''',A,'''''': UNITES'',           &
&           I3,'' ET'',I3,'' INCOMPATIBLES'')') CDACTI(1:ILACT2), &
&      INUME1,INUME2
!
  ELSEIF (KCODE.EQ.-113) THEN
    CLMESS=                                                        &
&       'SS-TRONCATURE IMPLICITE SUPERIEURE OU = A TRONCATURE MAXI'
!
  ELSEIF (KCODE.EQ.-114) THEN
    CLMESS=                                                       &
&       'RATIO DES TRONCATURES HORIZ. (VERSION LAM) SUPERIEUR A 3: &
&        GARDE-FOU, MODIFIABLE DANS FARCIS+FACSIM'
!
  ELSEIF (KCODE.EQ.-115) THEN
    IF (KNUMER.EQ.JPNIIL) THEN
      CLMESS='KNXLON<(-2*KTYPTR+1), CADRE "' &
&             //CDACTI(1:ILACT2)//'"'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                           &
&         '(''KNXLON<(-2*KTYPTR+1), UNITE'',I3)') KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-116) THEN
    IF (KNUMER.EQ.JPNIIL) THEN
      CLMESS='KNXLON<(2*KTRONC+1), CADRE "' &
&             //CDACTI(1:ILACT2)//'"'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                          &
&         '(''KNXLON<(2*KTRONC+1), UNITE'',I3)') KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-117) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
      CLMESS='INDICATEUR DE DOMAINE HORS [-1,1], CADRE "' &
&             //CDACTI(1:ILACT2)//'"'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                       &
&    '(''INDICATEUR DE DOMAINE HORS [-1,1], UNITE'', &
&       I3)')  KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-118) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
      CLMESS=                                                    &
&       'INDICE DE DEPART EN LONGITUDE HORS [1,KNXLON], CADRE "'  &
&             //CDACTI(1:ILACT2)//'"'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                                   &
&    '(''INDICE DE DEPART EN LONGITUDE HORS [1,KNXLON], UNITE'', &
&       I3)')  KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-119) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
      CLMESS='INDICE DE FIN EN LONGITUDE ERRONE, CADRE "' &
&             //CDACTI(1:ILACT2)//'"'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                       &
&    '(''INDICE DE FIN EN LONGITUDE ERRONE, UNITE'', &
&       I3)')  KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-120) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
      CLMESS=                                                  &
&       'INDICE DE DEPART EN LATITUDE HORS [1,KNLATI], CADRE "' &
&             //CDACTI(1:ILACT2)//'"'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                                  &
&    '(''INDICE DE DEPART EN LATITUDE HORS [1,KNLATI], UNITE'', &
&       I3)')  KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-121) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
      CLMESS='INDICE DE FIN EN LATITUDE ERRONE, CADRE "' &
&             //CDACTI(1:ILACT2)//'"'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                      &
&    '(''INDICE DE FIN EN LATITUDE ERRONE, UNITE'', &
&       I3)')  KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-122) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
      CLMESS='ZONE DE RELAXATION TROP LARGE '// &
&                'EN LONGITUDE, CADRE "'         &
&                //CDACTI(1:ILACT2)//'"'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                                &
&    '(''ZONE DE RELAXATION TROP LARGE EN LONGITUDE, UNITE'', &
&       I3)')  KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-123) THEN
!
    IF (KNUMER.EQ.JPNIIL) THEN
      CLMESS='ZONE DE RELAXATION TROP LARGE '// &
&                'EN LATITUDE, CADRE "'          &
&                //CDACTI(1:ILACT2)//'"'
    ELSE
      WRITE (UNIT=CLMESS,FMT=                               &
&    '(''ZONE DE RELAXATION TROP LARGE EN LATITUDE, UNITE'', &
&       I3)')  KNUMER
    ENDIF
!
  ELSEIF (KCODE.EQ.-124) THEN
    CLMESS=                        &
&       'NB DE BITS POUR CODER NUL'
!
  ELSEIF (KCODE.EQ.-125) THEN
    CLMESS=                                   &
&       'ARGUMENT AYANT UNE VALEUR INCORRECTE'
!
  ELSEIF (KCODE.EQ.-126) THEN
    CLMESS=                                   &
&       'INCOHERENCE DANS UN CONTROLE INTERNE'
!
  ELSEIF (KCODE.EQ.-127) THEN
    CLMESS=                                         &
&       'INCOHERENCE DANS LE CONTROLE DE L''ELLIPSE'
!
  ELSEIF (KCODE.EQ.-128) THEN
    CLMESS=                                 &
&       'MESSAGE GRIB INCORRECT POUR DECF10'
!
  ELSEIF (KCODE.EQ.-129) THEN
    CLMESS=                                 &
&       'IMPOSSIBLE D''ENCODER CE CHAMP AVEC CES REGLAGES'
!
  ELSEIF (KCODE.EQ.-130) THEN
    CLMESS=                                 &
&       'TABLEAU TROP COURT POUR ENCODER CE CHAMP'
!
  ELSEIF (KCODE.LT.-1000) THEN
    CLMESS=                                                       &
&  'ERREUR RAPPORTEE PAR GRIBEX (cf manuel, avec kret=-krep-1000)'
!
  ELSEIF (KNUMER.EQ.JPNIIL) THEN
    WRITE (UNIT=CLMESS,FMT='(''ERREUR GLOBALE NUMERO'',I6)')  &
&            KCODE
!
  ELSE
    WRITE (UNIT=CLMESS,FMT='(''ERREUR GRIB'',I6,         &
&           '' SUR UNITE LOGIQUE'',I3)') 200+KCODE,KNUMER
  ENDIF
!
  ILMESA=INT (LEN (CLMESA), JPLIKB)
  ILMESU=ILMESA-1-2*INT (LEN (CLJOLI), JPLIKB)-ILNSPR-4
  CLMESA=CLJOLI//' '//CDNSPR(1:ILNSPR)//' - '//CLMESS(1:ILMESU) &
&         //CLJOLI
  CLMESA=TRIM (CLMESA)//CLITID
  WRITE (UNIT=FA%NULOUT,FMT='(A)') CLMESA
!
  WRITE (UNIT=FA%NULOUT,FMT=*) CLMESA
  IF (LDFATA.AND.KCODE.NE.0) THEN
    CALL FLUSH(INT (FA%NULOUT))
    CALL SDL_SRLABORT
  ENDIF
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('FAIPAR_MT',1,ZHOOK_HANDLE)
END SUBROUTINE FAIPAR_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FAIPAR64                                       &
&           (KNUMER, KNIMES, KCODE, LDFATA, CDMESS, CDNSPR, &
&           CDACTI, LDRLFI)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
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
LOGICAL                LDRLFI                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAIPAR_FORT                                              &
&           (FA, KNUMER, KNIMES, KCODE, LDFATA, CDMESS, CDNSPR, &
&           CDACTI, LDRLFI)

END SUBROUTINE FAIPAR64

SUBROUTINE FAIPAR                                         &
&           (KNUMER, KNIMES, KCODE, LDFATA, CDMESS, CDNSPR, &
&           CDACTI, LDRLFI)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
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
LOGICAL                LDRLFI                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAIPAR_MT                                                &
&           (FA, KNUMER, KNIMES, KCODE, LDFATA, CDMESS, CDNSPR, &
&           CDACTI, LDRLFI)

END SUBROUTINE FAIPAR

SUBROUTINE FAIPAR_MT                                          &
&           (FA, KNUMER, KNIMES, KCODE, LDFATA, CDMESS, CDNSPR, &
&           CDACTI, LDRLFI)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
INTEGER (KIND=JPLIKM)  KNIMES                                 ! IN   
INTEGER (KIND=JPLIKM)  KCODE                                  ! IN   
LOGICAL                LDFATA                                 ! IN   
CHARACTER (LEN=*)      CDMESS                                 ! IN   
CHARACTER (LEN=*)      CDNSPR                                 ! IN   
CHARACTER (LEN=*)      CDACTI                                 ! IN   
LOGICAL                LDRLFI                                 ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
INTEGER (KIND=JPLIKB)  INIMES                                 ! IN   
INTEGER (KIND=JPLIKB)  ICODE                                  ! IN   
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)
INIMES     = INT (    KNIMES, JPLIKB)
ICODE      = INT (     KCODE, JPLIKB)

CALL FAIPAR_FORT                                              &
&           (FA, INUMER, INIMES, ICODE, LDFATA, CDMESS, CDNSPR, &
&           CDACTI, LDRLFI)


END SUBROUTINE FAIPAR_MT

!INTF KNUMER        IN    
!INTF KNIMES        IN    
!INTF KCODE         IN    
!INTF LDFATA        IN    
!INTF CDMESS        IN    
!INTF CDNSPR        IN    
!INTF CDACTI        IN    
!INTF LDRLFI        IN    
