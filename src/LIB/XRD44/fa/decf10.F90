! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Interface to thread-safe FA
FUNCTION DECF10               &
&                           (KGRIB,  KLENG, KDECAL,  &
&                           KFAORI, KFAMOD, KNBIMO)
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!     Fonction "DECF10"
!
!     But.
!     ---
!
!     Effectuer dans un message GRIB au niveau d'edition 1 le decalage
!     (en valeur) du descripteur "facteur d'echelle decimal".
!
!
!**   Interface.
!     ----------
!
!
!      Arguments d'appel:
!
!     KGRIB  (Entree+ ===> Tableau receptacle pour le message GRIB
!             Sortie)
!     KLENG  (Entree) ===> Longueur utile (mots) de KGRIB
!     KDECAL (Entree) ===> DECALage (additif) a effectuer sur la valeur
!                          courante du facteur d'echelle decimal.
!     KFAORI (Sortie) ===> FActeur decimal d'ORIgine
!     KFAMOD (Sortie) ===> FActeur decimal MODifie
!     KNBIMO (Entree) ===> Nombre de BIts par mot (entier)
!
!     Le code-retour de la fonction est:
!
!      0 si tout s'est bien passe
!     -1 si KGRIB ne commence pas par un entete GRIB
!     -2 si l'edition du GRIB ne convient pas
!     >0 code-retour (erreur) INXBIT
!
!
!     Methode.
!     --------
!
!     Verifie le niveau d'edition du message GRIB, decode le facteur
!     d'echelle decimal courant, y ajoute KDECAL, et remplace la valeur
!     par insertion dans le message.
!
!
!     Sous-programmes/fonctions externes utilises.
!     --------------------------------------------
!
!     INXBIT - INsertion d'une chaine binaire
!
!     Reference.
!     ----------
!
!     Aucune.
!
!
!     Commentaires.
!     -------------
!
!     Cettte fonction a ete developpee dans le cadre du pretraitement de
!     modeles etrangers arrivant sous forme de fichiers, pour DIAPASON
!     et SYNERGIE. Mais elle n'en depend pas vraiment, et peut donc
!     etre reutilisee dans un autre contexte.
!
!     Dans sa mouture initiale, seule l'edition 1 de GRIB est geree.
!
!     On utilise une primitive de bas niveau de la bibliotheque
!     "emos" du CEPMMT.
!
!     La convention de codage du signe du facteur d'echelle est
!     explicitement geree par la fonction courante.
!
!
!     Auteur.
!     -------
!
!     J. Clochard, Meteo France, DSI/OP/D - Juin 2001.
!
!
!     Modifications.
!     --------------
!
!     Aucune.
!
!
!
!     Constantes symboliques.
!
!     Signification des PARAMETER "locaux" au sous-programme :
!
!     JPDGRB => Nombre d'octets a decoder pour controler le message GRIB
!     JPGRIB => Nombre d'octets a decoder pour controler l'entete GRIB
!
INTEGER (KIND=JPLIKB) DECF10
!
INTEGER JPDGRB, JPGRIB
!
PARAMETER ( JPDGRB = 8, JPGRIB = 4 )
!
!     Arguments d'appel.
!
INTEGER (KIND=JPLIKB) KLENG, KDECAL, KFAORI, KFAMOD, KNBIMO
!
INTEGER KGRIB (KLENG)
!
!     Variables locales.
!
INTEGER IAUXIL, IREPON, J, IDECAL, ILODES, IPIVOT, IFACOD, IFAC10
!
INTEGER IDGRIB (JPDGRB)
INTEGER ILENG, INBIMO
!
!     Caracteres G, R, I, B en code CCITT IA-5
!
INTEGER, PARAMETER :: IBLOCD (JPGRIB) = (/ 71, 82, 73, 66 /)
!
CHARACTER(LEN=1) CLOPER
!**
!     1.  -  INITIALISATIONS ET CONTROLES.
!-----------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('DECF10',0,ZHOOK_HANDLE)
IREPON = 0
KFAORI = -9999
KFAMOD = KFAORI
ILENG  = INT (KLENG,  JPLIKM)
INBIMO = INT (KNBIMO, JPLIKM)
!
IF ( KDECAL .EQ. 0 ) THEN
!
  GO TO 900
!
ENDIF
!
!     Deocdage des premiers octets du message GRIB.
!
CLOPER = 'D'
IDECAL = 0
IAUXIL = JPDGRB
ILODES = 8
!
CALL INXBIT( KGRIB,  ILENG, IDECAL, IDGRIB, IAUXIL, INBIMO, &
&             ILODES, CLOPER, IREPON )
!
IF ( IREPON .NE. 0 ) THEN
!
  GO TO 900
!
ENDIF
!
!     KGRIB commence-t-il bien par les 4 lettres GRIB ?
!
DO J = 1, JPGRIB
!
  IF( IDGRIB(J) .NE. IBLOCD(J) ) THEN
!
    IREPON = -1
!
    GO TO 900
!
  ENDIF
!
ENDDO
!
!     Controle du niveau d'edition de GRIB.
!
IF( IDGRIB(JPDGRB) .NE. 1 ) THEN
!
  IREPON = -2
!
  GO TO 900
!
ENDIF
!
IDECAL = (8+26)*8
IAUXIL = 1
ILODES = 16
IPIVOT = 2**15
!**
!     2.  -  DECODAGE DU FACTEUR D'ECHELLE DECIMAL COURANT.
!-----------------------------------------------------------------------
!
!
CALL INXBIT( KGRIB,  ILENG, IDECAL, IFACOD, IAUXIL, INBIMO, &
&             ILODES, CLOPER, IREPON )
IDECAL = IDECAL-ILODES*IAUXIL
!
IF ( IREPON .NE. 0 ) THEN
!
  GO TO 900
!
ELSEIF ( IFACOD .LE. IPIVOT ) THEN
!
  IFAC10 = IFACOD
!
ELSE
!
  IFAC10 = IPIVOT-IFACOD
!
ENDIF
!
KFAORI = INT (IFAC10, JPLIKB)
!**
!     3.  -  MODIFICATION DU FACTEUR D'ECHELLE DECIMAL COURANT.
!-----------------------------------------------------------------------
!
!
IFAC10 = IFAC10+ INT (KDECAL, JPLIKM)
KFAMOD = INT (IFAC10, JPLIKB)
CLOPER = 'C'
!
IF ( IFAC10 .GE. 0 ) THEN
!
  IFACOD = IFAC10
!
ELSE
!
  IFACOD = IPIVOT-IFAC10
!
ENDIF
!
CALL INXBIT( KGRIB,  ILENG, IDECAL, IFACOD, IAUXIL, INBIMO, &
&             ILODES, CLOPER, IREPON )
!**
!     9.  -  MESSAGERIE EVENTUELLE, RETOUR A L'APPLICATIF APPELANT.
!-----------------------------------------------------------------------
!
900 CONTINUE
!
DECF10 = INT (IREPON, JPLIKB)
!
IF (LHOOK) CALL DR_HOOK('DECF10',1,ZHOOK_HANDLE)
END FUNCTION DECF10
