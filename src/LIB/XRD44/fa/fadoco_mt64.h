INTERFACE

SUBROUTINE FADOCO_FORT                                              &
&                     (FA,  KREP,   KNUMER, CDPREF, KNIVAU, CDSUFF, &
&                      LDCOSP, CDNOMA, KLNOMA, PVALCO, KLONGD,      &
&                      PCHAMP, LDUNDF, PUNDF)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
TYPE(FA_COM)           FA
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDPREF                                 ! IN   
INTEGER (KIND=JPLIKB)  KNIVAU                                 ! IN   
CHARACTER (LEN=*)      CDSUFF                                 ! IN   
LOGICAL                LDCOSP                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 !   OUT
INTEGER (KIND=JPLIKB)  KLNOMA                                 !   OUT
REAL (KIND=JPDBLR)     PVALCO     (*)                         ! IN   
INTEGER (KIND=JPLIKB)  KLONGD                                 ! IN   
REAL (KIND=JPDBLR)     PCHAMP     (*)                         !   OUT
LOGICAL,               OPTIONAL :: LDUNDF                     !   OUT
REAL (KIND=JPDBLR),    OPTIONAL :: PUNDF                      !   OUT
END SUBROUTINE

END INTERFACE

