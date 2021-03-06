SUBROUTINE FADCPL_FORT                                          &
&                     (FA, KREP, KRANG, CDNOMA, KVALCO, KLONGA, &
&                      PCHAMP, LDCOSP, LDUNDF, PUNDF)
USE FA_MOD, ONLY : FA_COM, JPNIIL
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
USE GRIB_API_INTERFACE
USE GRIB_API
IMPLICIT NONE
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) KREP, KRANG, KLONGA
!
INTEGER (KIND=JPLIKB) KVALCO(*)
REAL (KIND=JPDBLR)    PCHAMP(*)
REAL (KIND=JPDBLR)    PUNDF
!
LOGICAL LDCOSP, LDUNDF
!
CHARACTER CDNOMA*(*)
!
!
REAL (KIND=JPDBLR) :: Z1, Z2, Z3, Z4
INTEGER (KIND=JPLIKB) IRANGC
INTEGER (KIND=JPLIKB) INLATI, INXLON, IDLUXG, IDGUXG, IDZONL, IDZONG
INTEGER (KIND=JPLIKB) ILCHAM
INTEGER (KIND=JPLIKB) ICPLSIZE, INBITS
INTEGER (KIND=JPLIKB) ILAT, ILON, IPACK
INTEGER (KIND=JPLIKB) ILAT1, ILAT2, ILAT3, ILAT4
INTEGER (KIND=JPLIKB) ILON1, ILON2, ILON3, ILON4
INTEGER (KIND=JPLIKB) ILATMIN, ILATMAX, ILONMIN, ILONMAX
INTEGER (KIND=JPLIKB) INIMES, INUMER
!
CHARACTER(LEN=FA%JPLMES) CLMESS
CHARACTER(LEN=FA%JPLSPX) CLNSPR
LOGICAL :: LLFATA
!
REAL (KIND=JPDBLR) :: ZUNDF, ZMULTI
INTEGER (KIND=JPLIKM) :: ILGRIB, IGRIBH, IRET
INTEGER (KIND=JPLIKB) :: INOD, INOV
CHARACTER, ALLOCATABLE :: CLGRIB (:)
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('FADCPL_MT',0,ZHOOK_HANDLE)

IF (LDCOSP) THEN
  KREP=-200
  GOTO 1001
ENDIF

KREP=0

IRANGC=FA%FICHIER(KRANG)%NUCADR
INLATI=FA%CADRE(IRANGC)%NLATIT
INXLON=FA%CADRE(IRANGC)%NXLOPA

ILCHAM = INLATI * INXLON
IDLUXG  = FA%CADRE(IRANGC)%NLOPAR (4)  ! lon
IDGUXG  = FA%CADRE(IRANGC)%NLOPAR (6)  ! lat
IDZONL  = FA%CADRE(IRANGC)%NLOPAR (7)  
IDZONG  = FA%CADRE(IRANGC)%NLOPAR (8)

ICPLSIZE = KVALCO (3)
INBITS   = KVALCO (4)

ILONMIN=IDZONL+ICPLSIZE
ILONMAX=IDLUXG-ICPLSIZE-IDZONL+1
ILATMIN=IDZONG+ICPLSIZE
ILATMAX=IDGUXG-ICPLSIZE-IDZONG+1


ILGRIB = (KLONGA-3)*8

ALLOCATE (CLGRIB (ILGRIB))
CLGRIB = TRANSFER (KVALCO (5:KLONGA), CLGRIB)
CALL GRIB_NEW_FROM_MESSAGE_CHAR (IGRIBH, CLGRIB, STATUS=IRET)
DEALLOCATE (CLGRIB)
IF (IRET /= GRIB_SUCCESS) THEN
  KREP=-1000-IRET
  GOTO 1001
ENDIF

CALL IGRIB_GET_VALUE (IGRIBH, 'FMULTI', ZMULTI)
CALL IGRIB_GET_VALUE (IGRIBH, 'numberOfValues', INOV) 
CALL IGRIB_GET_VALUE (IGRIBH, 'numberOfDataPoints', INOD) 
CALL IGRIB_GET_VALUE (IGRIBH, 'values', PCHAMP (1:ILCHAM))
CALL IGRIB_GET_VALUE (IGRIBH, 'missingValue',  ZUNDF)
CALL IGRIB_RELEASE (IGRIBH)


! Basic check on dimensions

IF ((INOD < ILCHAM) .OR. &
  & (INOV < (ILCHAM-(ILATMAX-ILATMIN-1)*(ILONMAX-ILONMIN-1)))) THEN
  KREP=-93
  GOTO 1001
ELSEIF ((INOD > ILCHAM) .OR. &
  & (INOV > (ILCHAM-(ILATMAX-ILATMIN-1)*(ILONMAX-ILONMIN-1)))) THEN
  KREP=-94
  GOTO 1001
ENDIF

! Apply scaling factor

IF (ZMULTI /= REAL (1._4, JPDBLR)) THEN
  PCHAMP (1:ILCHAM) = PCHAMP (1:ILCHAM) / ZMULTI
  ZUNDF             = ZUNDF             / ZMULTI
ENDIF

DO ILAT = ILATMIN+1, ILATMAX-1
  DO ILON = ILONMIN+1, ILONMAX-1

    IF (LDUNDF) THEN
      PCHAMP ((ILAT-1)*INXLON+ILON) = PUNDF
    ELSE
      ILAT1=ILAT
      ILON1=ILONMIN

      ILAT2=ILAT
      ILON2=ILONMAX

      ILAT3=ILATMIN
      ILON3=ILON

      ILAT4=ILATMAX
      ILON4=ILON

      Z1 = 1.0_JPRB / (ILON-ILON1)
      Z2 = 1.0_JPRB / (ILON2-ILON)
      Z3 = 1.0_JPRB / (ILAT-ILAT3)
      Z4 = 1.0_JPRB / (ILAT4-ILAT)

      PCHAMP ((ILAT-1)*INXLON+ILON) = &
                    & (Z1*PCHAMP((ILAT1-1)*INXLON+ILON1)   &
                    & +Z2*PCHAMP((ILAT2-1)*INXLON+ILON2)   &
                    & +Z3*PCHAMP((ILAT3-1)*INXLON+ILON3)   &
                    & +Z4*PCHAMP((ILAT4-1)*INXLON+ILON4))  &
                    & /(Z1+Z2+Z3+Z4) 
    ENDIF

  ENDDO
ENDDO

1001 CONTINUE

LLFATA=LLMOER (KREP,KRANG)

IF (FA%LFAMOP.OR.LLFATA) THEN
  INIMES=2
  CLNSPR='FADCPL'
  INUMER=JPNIIL

  WRITE (UNIT=CLMESS,FMT="('KREP=',I4,', KRANG=',I4,', CDPREF=''',A,'''')") &
&             KREP, KRANG, CDNOMA
  CALL FAIPAR_FORT                                       &
&                 (FA,INUMER,INIMES,KREP,.FALSE.,CLMESS, &
&                  CLNSPR, '',.FALSE.)
ENDIF

!
IF (LHOOK) CALL DR_HOOK('FADCPL_MT',1,ZHOOK_HANDLE)

CONTAINS

#include "facom2.llmoer.h"

END SUBROUTINE









 



