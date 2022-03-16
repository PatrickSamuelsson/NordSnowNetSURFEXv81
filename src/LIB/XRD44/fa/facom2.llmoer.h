!
!       Fonction servant a rendre fatale ou non une erreur detectee,
!       a l'aide du code reponse courant, du niveau de filtrage global,
!       et de l'option d'erreur fatale propre au fichier.
!       s'il n'y a pas de fichier (I5678=0, d'ou dimensionnement de
!          *LERRFA*), le niveau de filtrage joue le role principal.
!
LOGICAL FUNCTION LLMOER (I1234,I5678)
INTEGER (KIND=JPLIKB) :: I1234,I5678
LLMOER=I1234.EQ.-66_JPLIKB .OR. (I1234.NE.0_JPLIKB .AND.(FA%NRFAGA.EQ.0_JPLIKB &
     & .OR. (FA%NRFAGA.EQ.1_JPLIKB .AND.FA%FICHIER(I5678)%LERRFA)))
END FUNCTION
