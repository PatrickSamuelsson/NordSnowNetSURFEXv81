!
!       FONCTION DONNANT LE PLUS HAUT NIVEAU DE MESSAGERIE ACCEPTABLE
!       POUR L'UNITE LOGIQUE DE RANG "I3456" .
!       (UTILISATION DES NIVEAUX DE MESSAGERIE GLOBAL ET PROPRE AU
!        FICHIER - MEME REMARQUE QUE CI-DESSUS SI I3456=0, POUR NIVMES)
!
INTEGER (KIND=JPLIKB) FUNCTION IXNIMS (I3456)
INTEGER (KIND=JPLIKB) :: I3456
IXNIMS =MIN (2_JPLIKB ,2_JPLIKB *LFI%NIMESG, MAX (2_JPLIKB *LFI%NIMESG-2_JPLIKB ,LFI%NIVMES(I3456)))
END FUNCTION