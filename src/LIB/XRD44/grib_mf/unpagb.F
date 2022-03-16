      SUBROUTINE UNPAGB ( KPDATA, PFDATA, PMIN, PMAX, KBITS, PSCALE,    &
     &                    KLENG, LDARPE )
      USE PARKIND1, ONLY : JPRB
      USE YOMHOOK , ONLY : LHOOK, DR_HOOK
      USE LFI_PRECISION
!
!
!********************************************************************
!*
!*    NAME      : UNPAGB
!*
!*    FUNCTION  : COMPUTES INDIVIDUAL "UNPACKED" VALUES (FIELD FROM GRIB
!*                ), THE INPUT CONSISTING OF ONE DATA JUST UNPACKED
!*                FROM A BIT STRING PER COMPUTER WORD.
!*                   This subroutine has been designed to avoid explicit
!*                mixed use of REAL and INTEGER type values within the
!*                dummy-argument array PFDATA of DECOGA, this explicit
!*                use leading to non-standard code. The following code
!*                enables use of the same actual argument for the 2
!*                dummy-argument arrays.
!*
!*    INPUT     : KPDATA - (POSITIVE) INTEGER VALUES "JUST UNPACKED"
!*                PMIN   - MINIMUM VALUE, OR AN "UNDER-APPROXIMATION"
!*                         OF THE MINIMUM VALUE).
!*                PMAX   - MAXIMUM VALUE, OR A "OVER-APPROXIMATION"
!*                         OF THE MAXIMUM VALUE).
!*                KBITS  - NUMBER OF BITS PER CODED VALUE.
!*                PSCALE - SCALE FACTOR TO APPLY.
!*                KLENG  - NUMBER OF VALUES TO BE TREATED.
!*                LDARPE  - .TRUE., modifications for ARPEGE coding
!*                                  have been included when coding data;
!*                          .FALSE., no such modifications.
!*
!*    PMAX and KBITS are used only if LDARPE is .TRUE. .
!*
!*
!*    OUTPUT    : PFDATA - FLOATING-POINT VALUES.
!*
!*    AUTHOR    : J.CLOCHARD, FRENCH WEATHER SERVICE, 01/03/90.
!*
!********************************************************************
!*
      IMPLICIT NONE
!
!     JP_STRIDE= pas permettant la correspondance entre les elements
!                d'un tableau de reels (KIND=JPDBLD): PFDATA(J) et
!                les elements d'un tableau d'entiers KPDATA(JP_STRIDE*J)
!                defini comme un tableau d'entiers representes sur
!                autant de bits que les reels.
!
      INTEGER (KIND=JPLIKM), PARAMETER :: 
     &              JP_STRIDE=JPDBLD/JP_SIMPLE_ENTIER
!
! If integers are on 32 bits, don't be afraid by the number of bits of the
! real argument which is real and on 64 bits ... it's a trick : we unpack
! KPDATA to PFDATA in the same area, but we are kind enought to have a
! stride of 2 to access KPDATA ... ! (see also packgb.F)
!
      INTEGER (KIND=JPLIKM) :: KLENG
      INTEGER (KIND=JPLIKM) :: KBITS
!
      INTEGER (KIND=JPLIKM) :: KPDATA (JP_STRIDE*KLENG)
!
      REAL (KIND=JPDBLD) :: PMIN
      REAL (KIND=JPDBLD) :: PMAX
      REAL (KIND=JPDBLD) :: PSCALE
!
      REAL (KIND=JPDBLD) :: PFDATA (KLENG)
!
      INTEGER (KIND=JPLIKM) :: J, II, IAUXI1, IAUXI2
!
      LOGICAL :: LDARPE
!
      REAL(KIND=JPRB) :: ZHOOK_HANDLE
      IF (LHOOK) CALL DR_HOOK('UNPAGB',0,ZHOOK_HANDLE)
      IF (LDARPE) THEN
!**
!     1.  -  DIRECT COMPUTING WITH 2 CASES, WHICH ENABLES PERFECT
!            RESPECT OF MINIMUM AND MAXIMUM PROVIDED THAT PMIN AND PMAX
!            ARE THESE VALUES.
!
        IAUXI1=2**(KBITS-1)
        IAUXI2=2*IAUXI1-1
!
!            Here, PSCALE is (PMAX-PMIN)/(FLOAT(IAUXI2).
!
!$OMP PARALLEL DO PRIVATE(J,II) SCHEDULE(STATIC,4096)
        DO 101 J=KLENG,1,-1
!
#if defined(LITTLE)
          II=JP_STRIDE*J -1
#else
          II=JP_STRIDE*J
#endif
          IF (KPDATA(II).LT.IAUXI1) THEN
            PFDATA(J)=PMIN+PSCALE*REAL (KPDATA(II),JPDBLD)
          ELSE
            PFDATA(J)=PMAX-PSCALE*REAL (IAUXI2-KPDATA(II),JPDBLD)
          ENDIF
!
101     CONTINUE
!$OMP END PARALLEL DO
!
      ELSE
!**
!     2.  -  DIRECT COMPUTING, WHICH ENABLES PERFECT RESPECT
!            OF MINIMUM PROVIDED THAT PMIN IS THIS VALUE.
!       (in standard GRIB, there is no estimation of the field maximum,
!        just a "gross" over-approximation can be given)
!
!
!$OMP PARALLEL DO PRIVATE(J,II) SCHEDULE(STATIC,4096)
        DO 201 J=KLENG,1,-1
#if defined(LITTLE)
          II=JP_STRIDE*J -1
#else
          II=JP_STRIDE*J
#endif
          PFDATA(J)=PMIN+PSCALE*REAL (KPDATA(II),JPDBLD)
201     CONTINUE
!$OMP END PARALLEL DO
!
      ENDIF
!
      IF (LHOOK) CALL DR_HOOK('UNPAGB',1,ZHOOK_HANDLE)
      ENDSUBROUTINE UNPAGB
