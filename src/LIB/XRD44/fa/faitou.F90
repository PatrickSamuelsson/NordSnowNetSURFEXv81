! Jan-2013 P. Marguinaud Use JNGEOM & JNEXPL parameters
! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FAITOU_FORT                                             &
&                     (FA,  KREP, KNUMER, LDNOMM, CDNOMF, CDSTTU,  &
&                      LDERFA, LDIMST, KNIMES, KNBARP, KNBARI,     &
&                      CDNOMC)
USE FA_MOD, ONLY : FA_COM, NEW_FICHIER, JPNIIL, JNGEOM, JNEXPL
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!        Sous-programme d'OUVERTURE d'une unite logique "Fichier ARPEGE"
!     Il s'agit d'un fichier indexe, traite par le logiciel LFI.
!
!**
!     ARGUMENTS : Ce sont les memes que pour "LFIOUV", avec CDNOMC comme
!                 argument supplementaire.
!
!                 KREP   (Sortie) ==> Code-reponse du sous-programme;
!                 KNUMER (Entree) ==> Numero de l'unite logique;
!                 LDNOMM (Entree) ==> Vrai si l'unite logique doit etre
!                                     associee a un NOM de Fichier EXP-
!                                     LICITE lors de l'"OPEN" FORTRAN;
!                 CDNOMF (Entree) ==> Nom de fichier explicite, si
!                                     *LDNOMM* est VRAI - Meme si ce
!                                     n'est pas le cas, ce *DOIT* ETRE
!                                     UN OBJET DE TYPE "CHARACTER" .
!                 CDSTTU (Entree) ==> "STATUS" pour l'"OPEN" FORTRAN
!                                     ('OLD','NEW','UNKNOWN','SCRATCH')
!                                     par defaut, mettre 'UNKNOWN';
!                 LDERFA (Entree) ==> Option d'erreur fatale;
!                 LDIMST (Entree) ==> Option impression de Statistiques
!                                     au moment de la fermeture;
!                 KNIMES (Entree) ==> Niveau de la Messagerie (0,1 ou 2)
!                                     ( 0==>Rien, 2==>Tout )
!                 KNBARP (Entree) ==> Nombre d'articles logiques prevus,
!                                     ce qui n'est utilise que lors de
!                                     la Creation du fichier,
!                                     et qui n'empeche quand meme pas
!                                     d'avoir plus d'articles logiques;
!                 KNBARI (Sortie) ==> Nombre d'articles logiques de don-
!                                     nees sur le fichier, initialement.
!                                     (zero si creation)
!                 CDNOMC (Entree) ==> Nom du CADRE associe au fichier.
!*
!     N.B. :  Pour un fichier en mode creation, ce cadre doit avoir ete
!          defini au prealable (via le sous-programme FACADE, ou par
!          l'ouverture d'un fichier preexistant).
!             Pour un fichier ARPEGE preexistant, le cadre est lu sur le
!          fichier; s'il etait deja defini auparavant, il y a controle
!          de coherence entre les deux versions du cadre.
!
!
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KNUMER, KNIMES, KNBARP, KNBARI
!
INTEGER (KIND=JPLIKB) IRANG, IRANMS
INTEGER (KIND=JPLIKB) IREPOU, ILNOMC, ILOMIN, IREP, J
INTEGER (KIND=JPLIKB) INBARP, IRANER, IRANGC
INTEGER (KIND=JPLIKB) INPAHE, INLATI, ISULEI, INPIND
INTEGER (KIND=JPLIKB) INIVER, ILONGA
INTEGER (KIND=JPLIKB) ITRONC, ILACTI, INIMES, INXLON
INTEGER (KIND=JPLIKB) ITYPTR, IPHASE, IGARDE, IPOSEX, IPUILA
!
INTEGER (KIND=JPLIKB) IDIMEN (FA%JPCADI)
INTEGER (KIND=JPLIKB) IRDPOL (FA%JPXPAH+FA%JPXIND)
INTEGER (KIND=JPLIKB) IDATEF (FA%JPLDAT), IDATXF (FA%JPLDAT)
INTEGER (KIND=JPLIKB) ILDIMEN(FA%JPCADI),          &
&                      ILRDPOL(FA%JPXPAH+FA%JPXIND)
INTEGER (KIND=JPLIKB) ILPNVER
!
REAL (KIND=JPDBLR) ZCHMID (FA%JPCAFS), ZSINLA (FA%JPXGEO)
REAL (KIND=JPDBLR) ZHYBRI (0:(1+FA%JPXNIV)*2)
!
LOGICAL LDNOMM, LDERFA, LDIMST, LLVERG, LLNOUF, LLNOUC, LLRLFI
LOGICAL LLMODC, LLREDF, LLMODA, LLMLAM
!
CHARACTER CDNOMF*(*), CDSTTU*(*), CDNOMC*(*)
!
CHARACTER(LEN=FA%JPXNOM) CLACTI 
CHARACTER(LEN=FA%JPXNOM) CLNOMA
CHARACTER(LEN=FA%JPLMES) CLMESS 
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL                  LLFATA
LOGICAL                  LLNEWF

!**
!     1.  -  CONTROLES DIVERS, ET OUVERTURE DU FICHIER AU SENS "LFI".
!-----------------------------------------------------------------------
!
!     Controle sommaire sur les arguments...le reste est "sous-traite"
!     au sous-programme LFIOUV.
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FAITOU_MT',0,ZHOOK_HANDLE)
CLACTI=''
LLNEWF=.FALSE.
IRANG=0
IRANER=0
IRANMS=0
IREPOU=JPNIIL
LLRLFI=.FALSE.
LLVERG=.FALSE.
ILNOMC=INT (LEN (CDNOMC), JPLIKB)
ILOMIN=MIN ( INT (LEN (CDNOMF), JPLIKB),         &
&             INT (LEN (CDSTTU), JPLIKB), ILNOMC)
!
!        L'appel ci-dessous est legerement anticipe, de maniere a
!     initialiser les variables globales du logiciel s'il s'agit
!     du premier appel a un sous-programme de ce logiciel.
!
CALL FANUMU_FORT                 &
&               (FA, KNUMER,IRANG)
!        Si KNUMER est nul, alors le numero d'unite logique est
!     attribué automatiquement
IF (KNUMER == 0) THEN
  CALL FAAUTO_FORT  (FA, KNUMER, .TRUE.)
  IRANG=0
ENDIF
!
IF (ILOMIN.LE.0) THEN
  IREP=-65
  GOTO 1001
ELSEIF (IRANG.NE.0) THEN
!
!            Controle de non-ouverture prealable (au sens du logiciel)
!
  IREP=-55
  IRANMS=IRANG
  GOTO 1001
ENDIF
!
!             Verrouillage global, si necessaire.
!
IF (FA%LFAMUL) CALL LFIVER_FORT                        &
&                              (FA%LFI, FA%VRGLAS,'ON')
LLVERG=FA%LFAMUL
!
!        A-t-on deja atteint le nombre limite de fichiers ARPEGE
!     ouverts simultanement ? Si non, on cherche un emplacement libre
!     dans la table FA%NULOGI (logiquement, il devrait en exister un)
!
IF (FA%NFIOUV.GE.FA%JPNXFA) THEN
  IREP=-56
  GOTO 1001
ELSE
!
  DO J=1,FA%JPNXFA
!
  IF (FA%FICHIER(J)%NULOGI.EQ.JPNIIL) THEN
    IRANG=J
    GOTO 102
  ENDIF
!
  ENDDO
!
  IREP=-66
  GOTO 1001
!
102 CONTINUE
!
ENDIF
!
!              Ouverture du fichier au sens du logiciel LFI.
!     (on ajoute au nombre d'articles prevus par l'utilisateur les
!      articles constituant le cadre, la date et l'identificateur)
!
INBARP=KNBARP+7
CALL LFIOUV_FORT                                             &
&               (FA%LFI, IREPOU,KNUMER,LDNOMM,CDNOMF,CDSTTU, &
&             LDERFA,LDIMST,                                 &
&             KNIMES,INBARP,KNBARI)
!
IF (IREPOU.NE.0.AND.IREPOU.NE.-11) THEN
  IREP=IREPOU
  LLRLFI=.TRUE.
  GOTO 1001
ENDIF
!**
!     2.  -  CONTROLES SPECIFIQUES AU LOGICIEL DE FICHIERS ARPEGE.
!-----------------------------------------------------------------------
!
LLNOUF=KNBARI.EQ.0
CALL FANUCA_FORT                          &
&               (FA, CDNOMC,IRANGC,.FALSE.)
LLNOUC=IRANGC.EQ.0
!
IF (LLNOUF) THEN
!
  IF (LLNOUC) THEN
    IREP=-57
    GOTO 1001
  ELSE
!
!         Fichier en mode creation et cadre predefini... OK a ce niveau.
!
!       On ecrit les articles definissant le cadre sur le fichier,
!     ainsi qu'un article ayant pour nom l'identificateur "par defaut",
!     (en fait, le nom du cadre) de maniere a ce que cet article soit
!     sequentiellement celui qui suit le dernier article du cadre.
!
    LLMLAM=FA%CADRE(IRANGC)%LIMLAM
!          
    IDIMEN(1)=FA%CADRE(IRANGC)%MTRONC
    INLATI=FA%CADRE(IRANGC)%NLATIT
    IF (.NOT.LLMLAM) THEN
       INPAHE=(1+INLATI)/2
    ELSE
       ISULEI=FA%CADRE(IRANGC)%NOZPAR(1)
!
       INPIND=2*ISULEI+4
    ENDIF                   
    IDIMEN(2)=INLATI
    IDIMEN(3)=FA%CADRE(IRANGC)%NXLOPA
    INIVER=FA%CADRE(IRANGC)%NNIVER
    IDIMEN(4)=INIVER
    IDIMEN(5)=FA%CADRE(IRANGC)%NTYPTR
    ZCHMID(1)=FA%CADRE(IRANGC)%SSLAPO
    ZCHMID(2)=FA%CADRE(IRANGC)%SCLOPO
    ZCHMID(3)=FA%CADRE(IRANGC)%SSLOPO
    ZCHMID(4)=FA%CADRE(IRANGC)%SCODIL
    ZHYBRI(0)=FA%CADRE(IRANGC)%SPREFE
    ILNOMC=FA%CADRE(IRANGC)%NLCCAD
    CLNOMA=CDNOMC
!
    IF (.NOT.LLMLAM) THEN
!
       DO J=1,INPAHE
       IRDPOL(J)=FA%CADRE(IRANGC)%NLOPAR(J)
       IRDPOL(INPAHE+J)=FA%CADRE(IRANGC)%NOZPAR(J)
       ZSINLA(J)=FA%CADRE(IRANGC)%SINLAT(J)
       ENDDO
!
    ELSE
       DO J=1,JNGEOM
          ZSINLA(J)=FA%CADRE(IRANGC)%SINLAT(J)
       ENDDO
       DO J=1,JNEXPL
          IRDPOL(J)=FA%CADRE(IRANGC)%NLOPAR(J)
       ENDDO
       DO J=1,INPIND
          IRDPOL(JNEXPL+J)=FA%CADRE(IRANGC)%NOZPAR(J)
       ENDDO
!
    ENDIF
!
    DO J=0,INIVER
    ZHYBRI(J+1)=FA%CADRE(IRANGC)%SFOHYB(1,J)
    ZHYBRI(J+2+INIVER)=FA%CADRE(IRANGC)%SFOHYB(2,J)
    ENDDO
!
    LLRLFI=.TRUE.
    ILDIMEN=IDIMEN
    CALL LFIECR_FORT (FA%LFI, IREP, KNUMER, FA%CPCADI, ILDIMEN, FA%JPCADI)
    IDIMEN=ILDIMEN
    IF (IREP.NE.0) GOTO 1001
!
    CALL LFIECR_RD (FA%CPCAFS, ZCHMID, FA%JPCAFS)
    IF (IREP.NE.0) GOTO 1001
!
   IF (.NOT.LLMLAM) THEN
!
       ILONGA=INPAHE*2
       ILRDPOL=IRDPOL
       CALL LFIECR_FORT (FA%LFI, IREP, KNUMER, FA%CPCARP, ILRDPOL, ILONGA)
       IRDPOL=ILRDPOL
       IF (IREP.NE.0) GOTO 1001
!
       ILONGA=INPAHE
       CALL LFIECR_RD (FA%CPCASL, ZSINLA, ILONGA)
       IF (IREP.NE.0) GOTO 1001
!
    ELSE
!
       ILONGA=JNEXPL+INPIND
       ILRDPOL=IRDPOL
       CALL LFIECR_FORT (FA%LFI, IREP, KNUMER, FA%CPCARP, ILRDPOL, ILONGA)
       IRDPOL=ILRDPOL
       IF (IREP.NE.0) GOTO 1001
!
       ILONGA=JNGEOM
       CALL LFIECR_RD (FA%CPCASL, ZSINLA, ILONGA)
       IF (IREP.NE.0) GOTO 1001
!
    ENDIF
!           
    ILONGA=1+(1+INIVER)*2
    CALL LFIECR_RD (FA%CPCACH, ZHYBRI, ILONGA)
    IF (IREP.NE.0) GOTO 1001
!
    ILPNVER=FA%JPNVER
    CALL LFIECR_FORT (FA%LFI, IREP, KNUMER, CLNOMA(1:ILNOMC), ILPNVER, 1_JPLIKB)
    IF (IREP.NE.0) GOTO 1001
!
    LLRLFI=.FALSE.
    GOTO 300
  ENDIF
!
ENDIF
!*
!     2.1 - Fichier preexistant...lecture et controle du Cadre "Fichier"
!-----------------------------------------------------------------------
!
CALL LFINFO_FORT                                             &
&               (FA%LFI, IREP,KNUMER,FA%CPCADI,ILONGA,IPOSEX)
!
IF (IREP.NE.0) THEN
  LLRLFI=.TRUE.
  GOTO 1001
ELSEIF (ILONGA.EQ.0) THEN
  IREP=-60
  GOTO 1001
ELSEIF (ILONGA.NE.FA%JPCADI) THEN
  IREP=-61
  GOTO 1001
ENDIF
!
ILDIMEN=IDIMEN
CALL LFILEC_FORT (FA%LFI, IREP, KNUMER, FA%CPCADI, ILDIMEN, FA%JPCADI)
IDIMEN=ILDIMEN
!
IF (IREP.NE.0) THEN
  LLRLFI=.TRUE.
  GOTO 1001
ENDIF
!
CALL LFINFO_FORT (FA%LFI, IREP, KNUMER, FA%CPCAFS, ILONGA, IPOSEX)
!
IF (IREP.NE.0) THEN
  LLRLFI=.TRUE.
  GOTO 1001
ELSEIF (ILONGA.EQ.0) THEN
  IREP=-60
  GOTO 1001
ELSEIF (ILONGA.NE.FA%JPCAFS) THEN
  IREP=-61
  GOTO 1001
ENDIF
!
CALL LFILEC_DR (FA%CPCAFS, ZCHMID, FA%JPCAFS)
!
IF (IREP.NE.0) THEN
  LLRLFI=.TRUE.
  GOTO 1001
ENDIF
!
!        Coherence des dimensions par rapport aux valeurs "licites",
!     que l'on doit faire avant de poursuivre les lectures.
!
IF(IDIMEN(5).LE.0) LLMLAM = .TRUE.
ITRONC=IDIMEN(1)
INLATI=IDIMEN(2)
INPAHE=(1+INLATI)/2
INXLON=IDIMEN(3)
INIVER=IDIMEN(4)
ITYPTR=IDIMEN(5)
IPHASE=1
IGARDE=1
CALL FACADI_FORT                                                    &
&              (FA, IREP,CDNOMC,ITYPTR,ZCHMID(1),ZCHMID(2),         &
&               ZCHMID(3),ZCHMID(4),ITRONC,INLATI,INXLON,IRDPOL(1), &
&               IRDPOL(FA%JPXPAH+1),ZSINLA,                         &
&               INIVER,ZHYBRI(0),ZHYBRI(1),ZHYBRI(FA%JPXNIV+2),     &
&               LLMODC,LLREDF,IPHASE,IRANGC,ILNOMC,IGARDE)
IF (IREP.NE.0) GOTO 1001
!
CALL LFINFO_FORT                                             &
&               (FA%LFI, IREP,KNUMER,FA%CPCARP,ILONGA,IPOSEX)
!
IF (IREP.NE.0) THEN
  LLRLFI=.TRUE.
  GOTO 1001
ELSEIF (ILONGA.EQ.0) THEN
  IREP=-60
  GOTO 1001
ELSEIF (ILONGA.NE.INPAHE*2) THEN
  IF (.NOT.LLMLAM) THEN
     IREP=-61
     GOTO 1001
  ENDIF    
ENDIF
!
ILRDPOL=IRDPOL
CALL LFILEC_FORT (FA%LFI, IREP, KNUMER, FA%CPCARP, ILRDPOL, ILONGA)
IRDPOL=ILRDPOL
!
IF (IREP.NE.0) THEN
  LLRLFI=.TRUE.
  GOTO 1001
ENDIF
!
CALL LFINFO_FORT                                             &
&               (FA%LFI, IREP,KNUMER,FA%CPCASL,ILONGA,IPOSEX)
!
IF (IREP.NE.0) THEN
  LLRLFI=.TRUE.
  GOTO 1001
ELSEIF (ILONGA.EQ.0) THEN
  IREP=-60
  GOTO 1001
ELSEIF (ILONGA.NE.INPAHE) THEN
  IF (.NOT.LLMLAM) THEN
     IREP=-61
     GOTO 1001
  ENDIF      
ENDIF
!
CALL LFILEC_DR (FA%CPCASL, ZSINLA, ILONGA)
!
IF (IREP.NE.0) THEN
  LLRLFI=.TRUE.
  GOTO 1001
ENDIF
!
CALL LFINFO_FORT                                             &
&               (FA%LFI, IREP,KNUMER,FA%CPCACH,ILONGA,IPOSEX)
!
IF (IREP.NE.0) THEN
  LLRLFI=.TRUE.
  GOTO 1001
ELSEIF (ILONGA.EQ.0) THEN
  IREP=-60
  GOTO 1001
ELSEIF (ILONGA.NE.1+(1+INIVER)*2) THEN
  IF (.NOT.LLMLAM) THEN
     IREP=-61
     GOTO 1001
  ENDIF      
ENDIF
!
CALL LFILEC_DR (FA%CPCACH, ZHYBRI, ILONGA)
!
IF (IREP.NE.0) THEN
  LLRLFI=.TRUE.
  GOTO 1001
ENDIF
!
!        Tests complementaires sur les valeurs lues.
!
IPHASE=2
CALL FACADI_FORT                                                    &
&              (FA, IREP,CDNOMC,ITYPTR,ZCHMID(1),ZCHMID(2),         &
&               ZCHMID(3),ZCHMID(4),ITRONC,INLATI,INXLON,IRDPOL(1), &
&               IRDPOL(INPAHE+1),ZSINLA,                            &
&               INIVER,ZHYBRI(0),ZHYBRI(1),ZHYBRI(INIVER+2),        &
&               LLMODC,LLREDF,IPHASE,IRANGC,ILNOMC,IGARDE)
IF (IREP.NE.0) GOTO 1001
!*
!     2.2 - Fichier preexistant...l'identificateur du fichier est le
!           premier article suivant les articles du cadre.
!-----------------------------------------------------------------------
!
CALL LFICAS_FORT                                   &
&               (FA%LFI, IREP,KNUMER,CLNOMA,ILONGA, &
&                IPOSEX,.FALSE.)
!
IF (IREP.NE.0) THEN
  LLRLFI=.TRUE.
  GOTO 1001
ELSEIF (ILONGA.EQ.0) THEN
  IREP=-110
  GOTO 1001
ENDIF
!
!*
!     2.3 - Fichier preexistant...lecture et controle de l'article DATE.
!-----------------------------------------------------------------------
!
CALL LFINFO_FORT                                             &
&               (FA%LFI, IREP,KNUMER,FA%CPDATE,ILONGA,IPOSEX)
!
IF (IREP.NE.0) THEN
  LLRLFI=.TRUE.
  GOTO 1001
ELSEIF (ILONGA.EQ.0) THEN
  IREP=-62
  GOTO 1001
ELSEIF (ILONGA.NE.FA%JPLDAT) THEN
  IREP=-63
  GOTO 1001
ENDIF
!
CALL LFILEC_FORT (FA%LFI, IREP, KNUMER, FA%CPDATE, IDATEF, FA%JPLDAT)
!
IF (IREP.NE.0) THEN
  LLRLFI=.TRUE.
  GOTO 1001
ENDIF

!
!*
!           Fichier preexistant...lecture et controle de l'article DATX.
!-----------------------------------------------------------------------
!
CALL LFINFO_FORT                                             &
&               (FA%LFI, IREP,KNUMER,FA%CPDATX,ILONGA,IPOSEX)
!
IF (IREP.NE.0) THEN
  LLRLFI=.TRUE.
  GOTO 1001
ELSEIF (ILONGA.EQ.0) THEN
  IDATXF (:) = 0
  GOTO 103
ELSEIF (ILONGA.NE.FA%JPLDAT) THEN
  IREP=-63
  GOTO 1001
ENDIF
!
CALL LFILEC_FORT (FA%LFI, IREP, KNUMER, FA%CPDATX, IDATXF, FA%JPLDAT)
!
IF (IREP.NE.0) THEN
  LLRLFI=.TRUE.
  GOTO 1001
ENDIF

103 CONTINUE

CALL NEW_FICHIER (FA, FA%FICHIER(IRANG), FA%JPLDAT, ITRONC, ITYPTR)
LLNEWF = .TRUE.

!
!        La ligne ci-dessous evite a FANDAI de croire, eventuellement,
!     a une redefinition de date.
!
FA%FICHIER(IRANG)%LCREAF=.TRUE.
!
!        Controle de la Date fichier, et stockage dans FA%MADATE.
!
CALL FANDAI_FORT                             &
&               (FA, IREP,IRANG,IDATEF,IDATXF,LLMODA)
IF (IREP.NE.0) GOTO 1001
!
!         Definition du Cadre proprement dite.
!
IPHASE=3
CALL FACADI_FORT                                                  &
&              (FA, IREP,CDNOMC,ITYPTR,ZCHMID(1),ZCHMID(2),         &
&               ZCHMID(3),ZCHMID(4),ITRONC,INLATI,INXLON,IRDPOL(1), &
&               IRDPOL(INPAHE+1),ZSINLA,                            &
&               INIVER,ZHYBRI(0),ZHYBRI(1),ZHYBRI(INIVER+2),        &
&               LLMODC,LLREDF,IPHASE,IRANGC,ILNOMC,IGARDE)
IF (IREP.NE.0) GOTO 1001
!**
!     3.  -  ON MET A JOUR LES TABLES RELATIVES AUX FICHIERS.
!-----------------------------------------------------------------------
!
300 CONTINUE
!
ITRONC=FA%CADRE(IRANGC)%MTRONC
ITYPTR=FA%CADRE(IRANGC)%NTYPTR

IF (.NOT. LLNEWF) THEN
  CALL NEW_FICHIER (FA, FA%FICHIER(IRANG), FA%JPLDAT, ITRONC, ITYPTR)
  LLNEWF = .TRUE.
ENDIF

FA%NFIOUV=FA%NFIOUV+1
FA%NULIND(FA%NFIOUV)=IRANG
FA%FICHIER(IRANG)%NULOGI=KNUMER
FA%FICHIER(IRANG)%NUCADR=IRANGC
!
FA%FICHIER(IRANG)%LNOMME=LDNOMM
FA%FICHIER(IRANG)%NIVOMS=KNIMES
FA%FICHIER(IRANG)%LERRFA=LDERFA
FA%FICHIER(IRANG)%LCREAF=LLNOUF
FA%FICHIER(IRANG)%NBFPDG=FA%NBIPDG
FA%FICHIER(IRANG)%NBFCSP=FA%NBICSP
FA%FICHIER(IRANG)%NPUFLA=FA%NPUILA
FA%FICHIER(IRANG)%NMFDPL=FA%NMIDPL
FA%FICHIER(IRANG)%NFGRIB=FA%NIGRIB
FA%FICHIER(IRANG)%CIDENT=CLNOMA
!
IF (ITYPTR.LT.0) THEN
  FA%FICHIER(IRANG)%NSTROF=MIN (FA%NSTROI,ITRONC-1,-ITYPTR-1)
ELSE
  FA%FICHIER(IRANG)%NSTROF=MIN (FA%NSTROI,ITRONC-1)
ENDIF
!
! Appel a FAINOC pour interpreter les eventuels defauts
! de -1 pris par FA%NBFPDG, FA%NBFCSP, FA%NSTROF et FA%NPUFLA en
! IRANG-ieme position.
!
CALL FAINOC_FORT            &
&               (FA,  IRANG )
!
IRANER=IRANG
IRANMS=IRANG
IPUILA=FA%FICHIER(IRANG)%NPUFLA
!
FA%FICHIER(IRANG)%NCOGRIF(:)=FA%NCODGRI(:)
FA%FICHIER(IRANG)%NRASHO = 0
FA%FICHIER(IRANG)%NRASVE = 0
!
!   L'initialisation de FLAP1Dx sera faite dans FACSIM
!
FA%FICHIER(IRANG)%LIFLAP=.TRUE.
!
!
IF (FA%LFAMUL) CALL LFIVER_FORT                                 &
&                              (FA%LFI, FA%FICHIER(IRANG)%VRFICH,'ASGN')
!
!        On incremente le nombre de fichiers attaches au cadre specifie.
!
FA%CADRE(IRANGC)%NULCAD=FA%CADRE(IRANGC)%NULCAD+1
IREP=IREPOU
GOTO 1001
!**
!     9.  - CI-DESSOUS, ETIQUETTES DE BRANCHEMENT EN CAS D'ERREUR E/S.
!-----------------------------------------------------------------------
!
CLACTI='INQUIRE'
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
LLFATA=LLMOER (IREP,IRANER)
!
IF (LLFATA) THEN
  INIMES=2
ELSE
  INIMES=IXNVMS (IRANMS)
ENDIF
!
!           Deverrouillage global eventuel.
!
IF (LLVERG) CALL LFIVER_FORT                         &
&                           (FA%LFI, FA%VRGLAS,'OFF')
!
IF (.NOT.LLFATA.AND.INIMES.EQ.0)  THEN 
  IF (LHOOK) CALL DR_HOOK('FAITOU_MT',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
CLNSPR='FAITOU'
!
IF (INIMES.EQ.2) THEN
!
  IF (ILNOMC.GT.0) THEN
    ILACTI=MIN (INT (LEN (CLACTI), JPLIKB),ILNOMC)
    CLACTI(1:ILACTI)=CDNOMC(1:ILNOMC)
  ELSE
    ILACTI=8
    CLACTI=FA%CHAINC(:ILACTI)
  ENDIF
!
  WRITE (UNIT=CLMESS,FMT='(''KREP='',I4,'', KNUMER='',I3,          &
&     '', LDNOMM= '',L1,'', CDSTTU='''''',A7,'''''', LDERFA= '',L1, &
&     '',  LDIMST= '',L1,                                           &
&         '', KNIMES='',I2,'', KNBARP='',I6,'' KNBARI='',I6)')      &
&   KREP,KNUMER,LDNOMM,CDSTTU,LDERFA,LDIMST,KNIMES,KNBARP,KNBARI
  CALL FAIPAR_FORT                                      &
&                 (FA, KNUMER,INIMES,IREP,.FALSE.,CLMESS, &
&                  CLNSPR,CLACTI(1:ILACTI),LLRLFI)
  CLMESS='CDNOMC='''//CLACTI(1:ILACTI)//''''
  CALL FAIPAR_FORT                                     &
&                 (FA, KNUMER,INIMES,IREP,LLFATA,CLMESS, &
&               CLNSPR,CLACTI(1:ILACTI),LLRLFI)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('FAITOU_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"
#include "facom2.ixnvms.h"

SUBROUTINE LFILEC_DR (CDNOMA, PDONNE, KLONGA)

CHARACTER(LEN=*)      :: CDNOMA
INTEGER (KIND=JPLIKB) :: KLONGA
REAL (KIND=JPDBLR)    :: PDONNE (KLONGA)
REAL (KIND=JPDBLD)    :: ZDONNE (KLONGA)

CALL LFILEC_FORT (FA%LFI, IREP, KNUMER, CDNOMA, ZDONNE, KLONGA)

PDONNE = REAL (ZDONNE, JPDBLR)

END SUBROUTINE LFILEC_DR

SUBROUTINE LFIECR_RD (CDNOMA, PDONNE, KLONGA)

CHARACTER(LEN=*)      :: CDNOMA
INTEGER (KIND=JPLIKB) :: KLONGA
REAL (KIND=JPDBLR)    :: PDONNE (KLONGA)
REAL (KIND=JPDBLD)    :: ZDONNE (KLONGA)

ZDONNE = REAL (PDONNE, JPDBLD)

CALL LFIECR_FORT (FA%LFI, IREP, KNUMER, CDNOMA, ZDONNE, KLONGA)

END SUBROUTINE LFIECR_RD

END SUBROUTINE FAITOU_FORT



! Oct-2012 P. Marguinaud 64b LFI
SUBROUTINE FAITOU64                                      &
&           (KREP, KNUMER, LDNOMM, CDNOMF, CDSTTU, LDERFA, &
&           LDIMST, KNIMES, KNBARP, KNBARI, CDNOMC)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
LOGICAL                LDNOMM                                 ! IN   
CHARACTER (LEN=*)      CDNOMF                                 ! IN   
CHARACTER (LEN=*)      CDSTTU                                 ! IN   
LOGICAL                LDERFA                                 ! IN   
LOGICAL                LDIMST                                 ! IN   
INTEGER (KIND=JPLIKB)  KNIMES                                 ! IN   
INTEGER (KIND=JPLIKB)  KNBARP                                 ! IN   
INTEGER (KIND=JPLIKB)  KNBARI                                 !   OUT
CHARACTER (LEN=*)      CDNOMC                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAITOU_FORT                                             &
&           (FA, KREP, KNUMER, LDNOMM, CDNOMF, CDSTTU, LDERFA, &
&           LDIMST, KNIMES, KNBARP, KNBARI, CDNOMC)

END SUBROUTINE FAITOU64

SUBROUTINE FAITOU                                        &
&           (KREP, KNUMER, LDNOMM, CDNOMF, CDSTTU, LDERFA, &
&           LDIMST, KNIMES, KNBARP, KNBARI, CDNOMC)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                   FA_COM_DEFAULT_INIT,  &
&                   NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
LOGICAL                LDNOMM                                 ! IN   
CHARACTER (LEN=*)      CDNOMF                                 ! IN   
CHARACTER (LEN=*)      CDSTTU                                 ! IN   
LOGICAL                LDERFA                                 ! IN   
LOGICAL                LDIMST                                 ! IN   
INTEGER (KIND=JPLIKM)  KNIMES                                 ! IN   
INTEGER (KIND=JPLIKM)  KNBARP                                 ! IN   
INTEGER (KIND=JPLIKM)  KNBARI                                 !   OUT
CHARACTER (LEN=*)      CDNOMC                                 ! IN   

IF (.NOT. FA_COM_DEFAULT_INIT) CALL NEW_FA_DEFAULT ()

CALL FAITOU_MT                                                 &
&           (FA, KREP, KNUMER, LDNOMM, CDNOMF, CDSTTU, LDERFA, &
&            LDIMST, KNIMES, KNBARP, KNBARI, CDNOMC)

END SUBROUTINE FAITOU

SUBROUTINE FAITOU_MT                                           &
&           (FA, KREP, KNUMER, LDNOMM, CDNOMF, CDSTTU, LDERFA, &
&            LDIMST, KNIMES, KNBARP, KNBARI, CDNOMC)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
LOGICAL                LDNOMM                                 ! IN   
CHARACTER (LEN=*)      CDNOMF                                 ! IN   
CHARACTER (LEN=*)      CDSTTU                                 ! IN   
LOGICAL                LDERFA                                 ! IN   
LOGICAL                LDIMST                                 ! IN   
INTEGER (KIND=JPLIKM)  KNIMES                                 ! IN   
INTEGER (KIND=JPLIKM)  KNBARP                                 ! IN   
INTEGER (KIND=JPLIKM)  KNBARI                                 !   OUT
CHARACTER (LEN=*)      CDNOMC                                 ! IN   
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

CALL FAITOU_FORT                                              &
&          (FA, IREP, INUMER, LDNOMM, CDNOMF, CDSTTU, LDERFA, &
&           LDIMST, INIMES, INBARP, INBARI, CDNOMC)

KREP       = INT (      IREP, JPLIKM)
KNBARI     = INT (    INBARI, JPLIKM)

IF (KNUMER == 0) THEN
  KNUMER = INT (    INUMER, JPLIKM)
ENDIF

END SUBROUTINE FAITOU_MT

!INTF KREP            OUT 
!INTF KNUMER        IN    
!INTF LDNOMM        IN    
!INTF CDNOMF        IN    
!INTF CDSTTU        IN    
!INTF LDERFA        IN    
!INTF LDIMST        IN    
!INTF KNIMES        IN    
!INTF KNBARP        IN    
!INTF KNBARI          OUT 
!INTF CDNOMC        IN    
