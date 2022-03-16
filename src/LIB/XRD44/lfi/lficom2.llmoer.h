!       FONCTION SERVANT A RENDRE FATALE OU NON UNE ERREUR DETECTEE,
!       A L'AIDE DU CODE-REPONSE COURANT, DU NIVEAU DE FILTRAGE GLOBAL,
!       ET DE L'OPTION D'ERREUR FATALE PROPRE AU FICHIER.
!       S'IL N'Y A PAS DE FICHIER (I5678=0, D'OU DIMENSIONNEMENT DE
!          *LERFAT*), LE NIVEAU DE FILTRAGE JOUE LE ROLE PRINCIPAL.
LOGICAL FUNCTION LLMOER (I1234,I5678)
INTEGER (KIND=JPLIKB) :: I1234,I5678
LLMOER = (I1234.EQ.-16_JPLIKB) .OR. (I1234.NE.0_JPLIKB .AND.(LFI%NERFAG.EQ.0_JPLIKB &
       & .OR.(LFI%NERFAG.EQ.1_JPLIKB .AND.LFI%LERFAT(I5678)))) 
END FUNCTION