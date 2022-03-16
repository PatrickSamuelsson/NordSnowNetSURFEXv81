! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe LFI

SUBROUTINE LFILAF_FORT                             &
&                     (LFI, KREP, KNUMER, LDTOUT )
USE LFIMOD, ONLY : LFICOM
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Sous-programme donnant, pour une unite logique ouverte au sens
!     du logiciel de fichiers indexes *LFI*, la Liste des Articles logi-
!     ques de donnees presents dans le Fichier, liste donnee toutefois
!     dans l'ordre PHYSIQUE ou ceux-ci figurent dans le fichier.
!        Sur option on donne aussi des renseignements sur les articles
!     (physiques) de gestion propres au logiciel, ainsi que sur les
!     trous repertories dans l'index.
!**
!    Arguments : KREP   (Sortie) ==> Code-reponse du sous-programme;
!                KNUMER (Entree) ==> Numero de l'unite logique;
!                LDTOUT (Entree) ==> Vrai si on doit donner les rensei-
!                                    gnements optionnels (qui ne concer-
!                                    nent pas directement les articles
!                                    logiques de donnees).
!
!
TYPE(LFICOM) :: LFI
INTEGER (KIND=JPLIKB) KREP, KNUMER, IMDESC, IREP, IRANG 
INTEGER (KIND=JPLIKB) INTROU, INBPIR, INBALO
INTEGER (KIND=JPLIKB) INALDO, IFACTM, ILARPH, INALPP 
INTEGER (KIND=JPLIKB) INTPPI, INPPIM, INIMES, J
INTEGER (KIND=JPLIKB) INAGES, IRESER, INUTIL, IPERTE 
INTEGER (KIND=JPLIKB) IPOSFI, IPOSDE, INEXCE
INTEGER (KIND=JPLIKB) INABAL, INALDI, INTROI, INPIMD 
INTEGER (KIND=JPLIKB) INPIMF, INPILE, JRGPIF
INTEGER (KIND=JPLIKB) IRGPFS, IRGPIM, IRANGM, IRPIMS 
INTEGER (KIND=JPLIKB) INALPI, ILONGA, IRECPI
INTEGER (KIND=JPLIKB) IDERPU, IREC, IRETIN
!
LOGICAL LDTOUT
!
CHARACTER(LEN=LFI%JPLSPX) CLNSPR
CHARACTER(LEN=LFI%JPLMES) CLMESS
CHARACTER(LEN=LFI%JPLFTX) CLACTI
LOGICAL LLFATA

!**
!     1.  -  CONTROLES DES PARAMETRES D'APPEL, PUIS INITIALISATIONS.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('LFILAF_FORT',0,ZHOOK_HANDLE)
CLACTI=''
IREP=0
IRANG=0
CLNSPR='LFILAF'
CALL LFINUM_FORT                    &
&               (LFI, KNUMER,IRANG)
!
IF (IRANG.EQ.0) THEN
  IREP=-1
  GOTO 1001
ENDIF
!
IF (LFI%LMULTI) CALL LFIVER_FORT                              &
&                               (LFI, LFI%VERRUE(IRANG),'ON')
INTROU=LFI%MDES1D(IXM(LFI%JPNTRU,IRANG))+LFI%NBTROU(IRANG)
INBPIR=LFI%MDES1D(IXM(LFI%JPNPIR,IRANG))
INBALO=LFI%MDES1D(IXM(LFI%JPNALO,IRANG))
INALDO=INBALO-INTROU
IFACTM=LFI%MFACTM(IRANG)
ILARPH=LFI%JPLARD*IFACTM
INALPP=LFI%JPNAPP*IFACTM
INTPPI=(INBALO-1+INALPP)/INALPP
INPPIM=LFI%NPPIMM(IRANG)
!
!         Envoi d'une banniere.
!
WRITE (UNIT=LFI%NULOUT,FMT='(///)')
!
IF (LFI%LFRANC) THEN
  WRITE (UNIT=CLMESS,                                             &
&         FMT='(''Catalogue de l''''Unite Logique LFI''            &
& ,I3,'' dans l''''ordre *PHYSIQUE* (sequentiel) des articles'')') &
&     KNUMER
ELSE
  WRITE (UNIT=CLMESS,FMT='(''Catalog of LFI Logical Unit'',I3,  &
&         '' in *PHYSICAL* (sequential) record order'')') KNUMER
ENDIF
!
INIMES=2
LLFATA=.FALSE.
CALL LFIEMS_FORT                                 &
&               (LFI, KNUMER,INIMES,IREP,LLFATA, &
&                CLMESS,CLNSPR,CLACTI)
!**
!     2.  -  SUR OPTION, RENSEIGNEMENTS SUR LES ARTICLES "DE GESTION".
!            (ARTICLE DOCUMENTAIRE, PAIRES D'ARTICLES D'INDEX)
!-----------------------------------------------------------------------
!
IF (LDTOUT) THEN
  INAGES=1+2*INBPIR
  IRESER=ILARPH*INAGES
!
  IF (LFI%LFRANC) THEN
    WRITE (UNIT=LFI%NULOUT,FMT='(//,TR1,I6,                        &
&           '' article(s) "physique(s)" de gestion,'',I6,           &
&           '' mots chacun, occupant donc'',I7,'' mots; detail:'',  &
& /,TR10,''Article documentaire de la position 1 a'',I6,/,TR10,I6,  &
&'' paire(s) d''''articles d''''index prereserves, de la position'' &
&           ,I6,'' a'',I7)')                                        &
&         INAGES,ILARPH,IRESER,ILARPH,INBPIR,ILARPH+1,IRESER
  ELSE
    WRITE (UNIT=LFI%NULOUT,FMT='(//,TR1,I6,                        &
&           '' "physical" records for file handling,'',I6,          &
&           '' words each, occupying then'',I7,'' words; detail:'', &
& /,TR10,''Documentary record from position 1 to'',I6,/,TR10,I6,    &
&'' pair(s) of pre-reserved index records, from position''          &
&           ,I6,'' to'',I7)')                                       &
&         INAGES,ILARPH,IRESER,ILARPH,INBPIR,ILARPH+1,IRESER
  ENDIF
!
  IF (INTPPI.LT.INBPIR) THEN
    INUTIL=INBPIR-INTPPI
    IPERTE=ILARPH*INUTIL*2
!
    IF (LFI%LFRANC) THEN
      WRITE (UNIT=LFI%NULOUT,                                     &
&             FMT='(/,TR10,5(''=''),''> Il y a'',I3,               &
& '' paire(s) d''''articles d''''index inutilises, representant'', &
&             I8,'' mots'')') INUTIL,IPERTE
    ELSE
      WRITE (UNIT=LFI%NULOUT,                                &
&             FMT='(/,TR10,5(''=''),''> There is (are)'',I3,  &
& '' pair(s) of unused index records, leading to a loss of'', &
&             I8,'' words'')') INUTIL,IPERTE
    ENDIF
!
  ELSEIF (INTPPI.EQ.INBPIR) THEN
!
    IF (LFI%LFRANC) THEN
      WRITE (UNIT=LFI%NULOUT,                                     &
&             FMT='(TR15,5(''-''),TR3,''pas de paire '',           &
&        ''d''''articles d''''index inutilises ni excedentaires'', &
&          TR3,5(''-''))')
    ELSE
      WRITE (UNIT=LFI%NULOUT,                         &
&             FMT='(TR15,5(''-''),TR3,''no pair of '', &
&        ''unused or overflow pages'',                 &
&          TR3,5(''-''))')
    ENDIF
!
  ELSEIF (INTPPI.EQ.(INBPIR+1)) THEN
    IPOSFI=ILARPH*(LFI%MDES1D(IXM(ILARPH,IRANG))+1)
    IPOSDE=IPOSFI-2*ILARPH+1
!
    IF (LFI%LFRANC) THEN
      WRITE (UNIT=LFI%NULOUT,                              &
&             FMT='(TR10,''une paire d''''articles '',      &
&             ''d''''index excedentaires, de la position'', &
&             I9,'' a'',I9)')                               &
&      IPOSDE,IPOSFI
    ELSE
      WRITE (UNIT=LFI%NULOUT,                            &
&             FMT='(TR10,''one pair of overflow index '', &
&             ''pages ,from position'',                   &
&             I9,'' to'',I9)')                            &
&      IPOSDE,IPOSFI
    ENDIF
!
  ELSE
    INEXCE=INTPPI-INBPIR
!
    IF (LFI%LFRANC) THEN
      WRITE (UNIT=LFI%NULOUT,                                     &
&             FMT='(TR10,I6,'' paires d''''articles '',            &
&           ''d''''index excedentaires, des positions:'')') INEXCE
!
      DO J=1,INEXCE
      IPOSFI=ILARPH*(LFI%MDES1D(IXM(ILARPH+1-J,IRANG))+1)
      IPOSDE=IPOSFI-2*ILARPH+1
      WRITE (UNIT=LFI%NULOUT,FMT='(TR20,I9,'' a'',I9)')  &
&             IPOSDE,IPOSFI
      ENDDO
!
    ELSE
      WRITE (UNIT=LFI%NULOUT,                             &
&             FMT='(TR10,I6,'' pairs of overflow index '', &
&           ''pages, from positions:'')') INEXCE
!
      DO J=1,INEXCE
      IPOSFI=ILARPH*(LFI%MDES1D(IXM(ILARPH+1-J,IRANG))+1)
      IPOSDE=IPOSFI-2*ILARPH+1
      WRITE (UNIT=LFI%NULOUT,FMT='(TR20,I9,'' to'',I9)')  &
&             IPOSDE,IPOSFI
      ENDDO
!
    ENDIF
!
  ENDIF
!
ENDIF
!
WRITE (UNIT=LFI%NULOUT,FMT='(//)')
!**
!     3.  -  RENSEIGNEMENTS INDIVIDUALISES SUR LES ARTICLES LOGIQUES.
!            (DONNEES, ET SUR OPTION TROUS REPERTORIES DANS L'INDEX)
!-----------------------------------------------------------------------
!
IF (LFI%LFRANC) THEN
!
  IF (INBALO.EQ.0) THEN
    WRITE (UNIT=LFI%NULOUT,                                        &
&           FMT='(/,TR10,5(''=''),''> L''''unite logique'',         &
& I3,'' ne contient AUCUN ARTICLE LOGIQUE (ni donnees, ni trous)'', &
&           //)') KNUMER
    GOTO 1001
  ELSEIF (INBALO.EQ.INTROU) THEN
    WRITE (UNIT=LFI%NULOUT,                                      &
&           FMT='(/,TR10,5(''=''),''> L''''unite logique'',       &
& I3,'' ne contient QUE DES TROUS, pas de donnees)'',//)') KNUMER
    IF (.NOT.LDTOUT) GOTO 1001
  ENDIF
!
ELSE
!
  IF (INBALO.EQ.0) THEN
    WRITE (UNIT=LFI%NULOUT,                                        &
&           FMT='(/,TR10,5(''=''),''> The logical unit'',I3,        &
& '' contains NO LOGICAL RECORD AT ALL (neither data, nor holes)'', &
&           //)') KNUMER
    GOTO 1001
  ELSEIF (INBALO.EQ.INTROU) THEN
    WRITE (UNIT=LFI%NULOUT,                                 &
&           FMT='(/,TR10,5(''=''),''> The logical unit'',I3, &
& '' contains ONLY HOLES, no dat)'',//)') KNUMER
    IF (.NOT.LDTOUT) GOTO 1001
  ENDIF
!
ENDIF
!*
!     3.1 -  BALAYAGE DES PAIRES D'ARTICLES D'INDEX, PAR ORDRE CROISSANT
!-----------------------------------------------------------------------
!
INABAL=0
INALDI=0
INTROI=0
INPIMD=2
INPIMF=INPPIM
IF (LFI%NPODPI(IRANG).EQ.2) INPIMD=3
IF (LFI%NPODPI(IRANG).EQ.INPPIM) INPIMF=INPPIM-1
INPILE=2
!
DO JRGPIF=1,INTPPI
IRGPFS=JRGPIF+1
!
!        On fait en sorte que la P.A.I. concernee, ainsi que sa suivante
!     eventuelle, soient toutes les deux en memoire.
!
IF (JRGPIF.EQ.INTPPI) THEN
  IRGPIM=LFI%MRGPIM(LFI%NPODPI(IRANG),IRANG)
  GOTO 314
!
ELSEIF (JRGPIF.NE.1) THEN
!
!       Recherche de la P.A.I. dans les Paires de Pages d'Index memoire.
!
  DO J=INPIMD,INPIMF
  IRGPIM=LFI%MRGPIM(J,IRANG)
!
  IF (LFI%MRGPIF(IRGPIM).EQ.JRGPIF) THEN
!
    IF (.NOT.LFI%LPHASP(IRGPIM)) THEN
!
      CALL LFIPHA_FORT                                &
&                     (LFI, IREP,IRANG,IRGPIM,IRETIN)
!
      IF (IRETIN.EQ.1) THEN
        GOTO 903
      ELSEIF (IRETIN.EQ.2) THEN
        GOTO 904
      ELSEIF (IRETIN.NE.0) THEN
        GOTO 1001
      ENDIF
!
    ENDIF
!
    GOTO 312
!
  ENDIF
!
  ENDDO
!
!          Mise en memoire de la Paire d'Articles d'Index cherchee.
!
  CALL LFIPIM_FORT                                              &
&                 (LFI, IREP,IRANG,IRANGM,IRGPIM,JRGPIF,IRGPFS, &
&                  INPILE,IRETIN)
!
  IF (IRETIN.EQ.1) THEN
    GOTO 903
  ELSEIF (IRETIN.EQ.2) THEN
    GOTO 904
  ELSEIF (IRETIN.NE.0) THEN
    GOTO 1001
  ELSEIF (IRANGM.GT.INPPIM) THEN
    INPPIM=IRANGM
    INPIMF=INPPIM
  ENDIF
!
ELSE
  IRGPIM=LFI%MRGPIM(1,IRANG)
!
ENDIF
!
312 CONTINUE
!
IF (IRGPFS.EQ.INTPPI) THEN
  IRPIMS=LFI%MRGPIM(LFI%NPODPI(IRANG),IRANG)
!
ELSE
!
!       Recherche de la P.A.I. dans les Paires de Pages d'Index memoire.
!
  DO J=INPIMD,INPIMF
  IRPIMS=LFI%MRGPIM(J,IRANG)
!
  IF (LFI%MRGPIF(IRPIMS).EQ.IRGPFS) THEN
!
    IF (.NOT.LFI%LPHASP(IRPIMS)) THEN
!
      CALL LFIPHA_FORT                                &
&                     (LFI, IREP,IRANG,IRPIMS,IRETIN)
!
      IF (IRETIN.EQ.1) THEN
        GOTO 903
      ELSEIF (IRETIN.EQ.2) THEN
        GOTO 904
      ELSEIF (IRETIN.NE.0) THEN
        GOTO 1001
      ENDIF
!
    ENDIF
!
    GOTO 314
!
  ENDIF
!
  ENDDO
!
!          Mise en memoire de la Paire d'Articles d'Index cherchee.
!
  CALL LFIPIM_FORT                                              &
&                 (LFI, IREP,IRANG,IRANGM,IRPIMS,IRGPFS,JRGPIF, &
&                  INPILE,IRETIN)
!
  IF (IRETIN.EQ.1) THEN
    GOTO 903
  ELSEIF (IRETIN.EQ.2) THEN
    GOTO 904
  ELSEIF (IRETIN.NE.0) THEN
    GOTO 1001
  ELSEIF (IRANGM.GT.INPPIM) THEN
    INPPIM=IRANGM
    INPIMF=INPPIM
  ENDIF
!
ENDIF
!
314 CONTINUE
INALPI=MIN (INALPP,INBALO-INABAL)
!
!        Balayage de la Paire d'Article d'Index concernee.
!
DO J=1,INALPI
!
IF (LFI%CNOMAR(IXC(J,IRGPIM)).NE.' ') THEN
!
!              Il s'agit d'un article logique de donnees; en plus de ses
!         caracteristiques tabulees, on verifie s'il n'y a pas de la
!         place "perdue" juste derriere les donnees, place recuperable
!         eventuellement en cas de reecriture plus longue de l'article
!         logique.
!
  INALDI=INALDI+1
  ILONGA=LFI%MLGPOS(IXM(2*J-1,IRGPIM))
  IPOSDE=LFI%MLGPOS(IXM(2*J  ,IRGPIM))
  IPOSFI=IPOSDE+ILONGA-1
!
  IF (J.EQ.1.AND.JRGPIF.GT.INBPIR) THEN
!
!          Cas du premier article logique d'une P.A.I. excedentaire;
!     dans ce cas, la P.A.I. est situee derriere l'article logique,
!     en occupant deux articles physiques.
!
    IRECPI=LFI%MDES1D(IXM(ILARPH+1-(JRGPIF-INBPIR),IRANG))
    IDERPU=ILARPH*(IRECPI-1)
!
  ELSEIF (J.EQ.INALPI.AND.JRGPIF.EQ.INTPPI) THEN
!
!          Cas du dernier article logique du fichier, sans P.A.I. situee
!     derriere: la derniere position utilisable sans modifier le nombre
!     d'articles physiques du fichier correspond a la fin du dernier
!     article physique contenant des donnees, ou a la fin du dernier
!     article physique ecrit sur le fichier.
!
    IMDESC=LFI%MDES1D(IXM(LFI%JPNAPH,IRANG))
    IREC=MAX (1+(IPOSFI-1)/ILARPH,IMDESC)
    IDERPU=ILARPH*IREC
!
!          Si on arrive au test ci-dessous, on est sur que l'article lo-
!     gique n'est pas le dernier du fichier.
!
  ELSEIF (J.NE.INALPP) THEN
!
!          Cas general, ou l'article logique n'est pas le dernier de sa
!     (Paire de) Page(s) d'Index.
!
    IDERPU=LFI%MLGPOS(IXM(2*J+2,IRGPIM))-1
!
  ELSE
!
!          Cas particulier ou l'article logique est le dernier de sa
!     (Paire de) Page(s) d'Index.
!
    IDERPU=LFI%MLGPOS(IXM(2_JPLIKB ,IRPIMS))-1
  ENDIF
!
  IF (IDERPU.EQ.IPOSFI) THEN
!
    IF (LFI%LFRANC) THEN
      WRITE (UNIT=LFI%NULOUT,                                &
&             FMT='(I7,''-eme article de donnees: "'',A,      &
&             ''",'',I7,'' mots, position'',I9,'' a'',I9)')   &
&       INALDI,LFI%CNOMAR(IXC(J,IRGPIM)),ILONGA,IPOSDE,IPOSFI
    ELSE
      WRITE (UNIT=LFI%NULOUT,                                &
&             FMT='(I7,''-th data record: "'',A,''",'',I7,    &
&             '' words, position'',I9,'' to'',I9)')           &
&       INALDI,LFI%CNOMAR(IXC(J,IRGPIM)),ILONGA,IPOSDE,IPOSFI
    ENDIF
!
  ELSE
!
!           On visualise en plus la place "perdue" derriere l'article.
!
    IF (LFI%LFRANC) THEN
      WRITE (UNIT=LFI%NULOUT,                                      &
&             FMT='(I7,''-eme article de donnees: "'',A,            &
&             ''",'',I7,'' mots, position'',I9,'' a'',I9,'' <'',SP, &
&             I8,'' >'')')                                          &
&   INALDI,LFI%CNOMAR(IXC(J,IRGPIM)),ILONGA,IPOSDE,                 &
&   IPOSFI,IDERPU-IPOSFI
    ELSE
      WRITE (UNIT=LFI%NULOUT,                                  &
&       FMT='(I7,''-th data record: '''''',A,                   &
&       '''''','',I7,'' words, position'',I9,'' to'',I9,'' <'', &
&       SP,I8,'' >'')')                                         &
&   INALDI,LFI%CNOMAR(IXC(J,IRGPIM)),ILONGA,IPOSDE,             &
&   IPOSFI,IDERPU-IPOSFI
    ENDIF
!
  ENDIF
!
ELSEIF (LDTOUT) THEN
  INTROI=INTROI+1
  ILONGA=LFI%MLGPOS(IXM(2*J-1,IRGPIM))
  IPOSDE=LFI%MLGPOS(IXM(2*J  ,IRGPIM))
  IPOSFI=IPOSDE+ILONGA-1
!
  IF (LFI%LFRANC) THEN
    WRITE (UNIT=LFI%NULOUT,FMT='(TR1,5(''=''),''>'',T10,I6,        &
& ''-eme TROU repertorie dans l''''index, longueur reutilisable:'', &
&         I7,'' mots, position'',I9,'' a'',I9)')                    &
&   INTROI,ILONGA,IPOSDE,IPOSFI
  ELSE
    WRITE (UNIT=LFI%NULOUT,FMT='(TR1,5(''=''),''>'',T10,I6, &
& ''-th HOLE cataloged within index, re-usable length:'',    &
&         I7,'' words, position'',I9,'' to'',I9)')           &
&   INTROI,ILONGA,IPOSDE,IPOSFI
  ENDIF
!
ENDIF
!
ENDDO
!
INABAL=INABAL+INALPI
ENDDO
!*
!     3.2 -  ENVOI DE MESSAGES RECAPITULATIFS.
!-----------------------------------------------------------------------
!
IF (LFI%LFRANC) THEN
!
  IF (LDTOUT) THEN
    WRITE (UNIT=LFI%NULOUT,FMT='(//,T5,8(''-''),TR3,I7,     &
&           '' articles logiques de donnees et'',I6,         &
&           '' trous repertories listes'',TR3,8(''-''),//)') &
&    INALDI,INTROI
  ELSE
    WRITE (UNIT=LFI%NULOUT,FMT='(//,T5,8(''-''),TR3,I7,            &
&       '' articles logiques de donnees listes'',TR3,8(''-''),//)') &
&    INALDI
  ENDIF
!
ELSE
!
  IF (LDTOUT) THEN
    WRITE (UNIT=LFI%NULOUT,FMT='(//,T5,8(''-''),TR3,I7,      &
&           '' logical records of data and'',I6,              &
&           '' holes within index listed'',TR3,8(''-''),//)') &
&    INALDI,INTROI
  ELSE
    WRITE (UNIT=LFI%NULOUT,FMT='(//,T5,8(''-''),TR3,I7,       &
&       '' logical records of data listed'',TR3,8(''-''),//)') &
&    INALDI
  ENDIF
!
ENDIF
!
IF (INALDI.EQ.INALDO.AND.(.NOT.LDTOUT.OR.INTROI.EQ.INTROU)) THEN
!
  IF (LFI%LFRANC) THEN
    WRITE (UNIT=CLMESS,FMT=                                       &
&     '(''Fin du catalogue de l''''Unite Logique'',I3,'' ---'',I7, &
&       '' Articles logiques en tout'')') KNUMER,INBALO
  ELSE
    WRITE (UNIT=CLMESS,FMT=                                 &
&     '(''End of catalog of Logical Unit'',I3,'' ---'',I7,   &
&       '' logical Records for whole file'')') KNUMER,INBALO
  ENDIF
!
  CALL LFIEMS_FORT                                 &
&                 (LFI, KNUMER,INIMES,IREP,LLFATA, &
&                  CLMESS,CLNSPR,CLACTI)
  WRITE (UNIT=LFI%NULOUT,FMT='(///)')
ELSE
  IREP=-16
ENDIF
!
GOTO 1001
!**
!     9.  - CI-DESSOUS, ETIQUETTES DE BRANCHEMENT EN CAS D'ERREUR E/S.
!-----------------------------------------------------------------------
!
903 CONTINUE
CLACTI='WRITE'
GOTO 909
!
904 CONTINUE
CLACTI='READ'
!
909 CONTINUE
!
!      AU CAS OU, ON FORCE LE CODE-REPONSE ENTREE/SORTIE A ETRE POSITIF.
!
IREP=ABS (IREP)
!**
!    10.  -  PHASE TERMINALE : MESSAGERIE, AVEC "ABORT" EVENTUEL,
!            VIA LE SOUS-PROGRAMME "LFIEMS" .
!-----------------------------------------------------------------------
!
1001 CONTINUE
KREP=IREP
LLFATA=LLMOER (IREP,IRANG)
!
IF (IRANG.NE.0) THEN
  LFI%NDEROP(IRANG)=18
  LFI%NDERCO(IRANG)=IREP
  IF (LFI%LMULTI) CALL LFIVER_FORT                               &
&                                 (LFI, LFI%VERRUE(IRANG),'OFF')
ENDIF
!
IF (LLFATA.OR.IXNIMS (IRANG).EQ.2) THEN
  INIMES=2
ELSE
  IF (LHOOK) CALL DR_HOOK('LFILAF_FORT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KNUMER='',I3, &
&    '', LDTOUT= '',L1)') KREP,KNUMER,LDTOUT
CALL LFIEMS_FORT                                 &
&               (LFI, KNUMER,INIMES,IREP,LLFATA, &
&                CLMESS,CLNSPR,CLACTI)
!
IF (LHOOK) CALL DR_HOOK('LFILAF_FORT',1,ZHOOK_HANDLE)

CONTAINS

#include "lficom2.ixc.h"
#include "lficom2.ixm.h"
#include "lficom2.ixnims.h"
#include "lficom2.llmoer.h"

END SUBROUTINE LFILAF_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE LFILAF64              &
&           (KREP, KNUMER, LDTOUT)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
LOGICAL                LDTOUT                                 ! IN   

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFILAF_FORT                       &
&           (LFI, KREP, KNUMER, LDTOUT)

END SUBROUTINE LFILAF64

SUBROUTINE LFILAF                &
&           (KREP, KNUMER, LDTOUT)
USE LFIMOD, ONLY : LFI => LFICOM_DEFAULT, &
&                   LFICOM_DEFAULT_INIT,   &
&                   NEW_LFI_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
LOGICAL                LDTOUT                                 ! IN   

IF (.NOT. LFICOM_DEFAULT_INIT) CALL NEW_LFI_DEFAULT ()

CALL LFILAF_MT                        &
&           (LFI, KREP, KNUMER, LDTOUT)

END SUBROUTINE LFILAF

SUBROUTINE LFILAF_MT                  &
&           (LFI, KREP, KNUMER, LDTOUT)
USE LFIMOD, ONLY : LFICOM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (LFICOM)          LFI                                    ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
LOGICAL                LDTOUT                                 ! IN   
! Local integers
INTEGER (KIND=JPLIKB)  IREP                                   !   OUT
INTEGER (KIND=JPLIKB)  INUMER                                 ! IN   
! Convert arguments

INUMER     = INT (    KNUMER, JPLIKB)

CALL LFILAF_FORT                       &
&           (LFI, IREP, INUMER, LDTOUT)

KREP       = INT (      IREP, JPLIKM)

END SUBROUTINE LFILAF_MT

!INTF KREP            OUT 
!INTF KNUMER        IN    
!INTF LDTOUT        IN    
