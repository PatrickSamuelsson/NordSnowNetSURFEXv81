!*
!       Fonction "en ligne" donnant le plus haut niveau de messagerie
!       acceptable pour l'unite logique de rang "I3456"
!       (utilisation des niveaux de messagerie global et propre au
!        fichier; s'il n'y a pas de fichier - I3456=0, d'ou le dimensio-
!        nnement de *NIVOMS* a partir de zero, le niveau de filtrage
!        global joue seul)
!
INTEGER (KIND=JPLIKB) FUNCTION IXNVMS (I3456)
INTEGER (KIND=JPLIKB) :: I3456
IXNVMS =MIN (2_JPLIKB ,2_JPLIKB *FA%NIMSGA,MAX (2_JPLIKB *FA%NIMSGA-2_JPLIKB ,FA%FICHIER(I3456)%NIVOMS))
END FUNCTION
