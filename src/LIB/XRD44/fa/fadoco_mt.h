INTERFACE

SUBROUTINE FADOCO_MT                                           &
&           (FA, KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, LDCOSP, &
&            CDNOMA, KLNOMA, PVALCO, KLONGD, PCHAMP,           &
&            LDUNDF, PUNDF)
USE FA_MOD, ONLY : FA_COM
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
TYPE (FA_COM)          FA                                     ! INOUT
INTEGER (KIND=JPLIKM)  KREP                                   !   OUT
INTEGER (KIND=JPLIKM)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDPREF                                 ! IN   
INTEGER (KIND=JPLIKM)  KNIVAU                                 ! IN   
CHARACTER (LEN=*)      CDSUFF                                 ! IN   
LOGICAL                LDCOSP                                 ! IN   
CHARACTER (LEN=*)      CDNOMA                                 !   OUT
INTEGER (KIND=JPLIKM)  KLNOMA                                 !   OUT
REAL (KIND=JPDBLR)     PVALCO     (*)                         ! IN   
INTEGER (KIND=JPLIKM)  KLONGD                                 ! IN   
REAL (KIND=JPDBLR)     PCHAMP     (*)                         !   OUT
LOGICAL,               OPTIONAL :: LDUNDF                     !   OUT
REAL (KIND=JPDBLR),    OPTIONAL :: PUNDF                      !   OUT
END SUBROUTINE

END INTERFACE

