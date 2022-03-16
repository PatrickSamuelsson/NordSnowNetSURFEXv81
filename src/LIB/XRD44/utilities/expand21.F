!OPTIONS NODOUBLE
      SUBROUTINE EXPAND21(KWRDS,PPACKED,POUT,KDUM1,KDUM2)

!**** *PACK21  *  - Unpack data by a factor 2 to 1

!     Purpose.
!     --------
!           Unpack data by a factor 2 to 1. Packing is done
!           in order to save memory.
!           Simple version to be used on Fujitsu VPP.

!**   Interface.
!     ----------
!        *CALL* *EXPAND21(...)*

!        Explicit arguments : KWRDS   - number of words to unpack (copy)
!        -------------------- PPACKED - buffer containing (packed) data
!                             POUT    - array containing unpacked data
!                             KDUM1   - not used
!                             KDUM2   - not used

!        Implicit arguments : None
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.
!     ----------
!        None

!     Reference.
!     ----------
!        ECMWF Research Department documentation of the IFS

!     Author.
!     -------
!        Lars Isaksen *ECMWF*

!     Modifications.
!     --------------
!        Original : 98-01-01
!     ------------------------------------------------------------------

      USE PARKIND1, ONLY : JPIM
!      IMPLICIT LOGICAL (L)
      IMPLICIT NONE
      INTEGER(KIND=JPIM), INTENT(IN)    :: KWRDS
      INTEGER(KIND=JPIM), INTENT(IN)    :: KDUM1
      INTEGER(KIND=JPIM), INTENT(IN)    :: KDUM2
      REAL (KIND=4) :: PPACKED(KWRDS)
      REAL (KIND=8) :: POUT(KWRDS)

!     ------------------------------------------------------------------

      POUT(:)=REAL(PPACKED(:),KIND=8)

      RETURN
      END SUBROUTINE EXPAND21
