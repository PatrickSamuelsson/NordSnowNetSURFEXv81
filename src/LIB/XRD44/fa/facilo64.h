INTERFACE

SUBROUTINE FACILO64                                        &
&           (KREP, KNUMER, CDPREF, KNIVAU, CDSUFF, PCHAMP, &
&           LDCOSP, LDUNDF, PUNDF)
USE FA_MOD, ONLY : FA => FA_COM_DEFAULT, &
&                  FA_COM_DEFAULT_INIT,  &
&                  NEW_FA_DEFAULT
USE LFI_PRECISION
IMPLICIT NONE
! Arguments
INTEGER (KIND=JPLIKB)  KREP                                   !   OUT
INTEGER (KIND=JPLIKB)  KNUMER                                 ! IN   
CHARACTER (LEN=*)      CDPREF                                 ! IN   
INTEGER (KIND=JPLIKB)  KNIVAU                                 ! IN   
CHARACTER (LEN=*)      CDSUFF                                 ! IN   
REAL (KIND=JPDBLR)     PCHAMP     (*)                         !   OUT
LOGICAL                LDCOSP                                 ! IN   
LOGICAL,               OPTIONAL :: LDUNDF                     !   OUT
REAL (KIND=JPDBLR),    OPTIONAL :: PUNDF                      !   OUT
END SUBROUTINE

END INTERFACE
