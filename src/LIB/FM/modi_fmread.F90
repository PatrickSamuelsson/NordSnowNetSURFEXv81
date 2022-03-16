!     ######spl
MODULE MODI_FMREAD
!##################
!
INTERFACE FMREAD
      SUBROUTINE FMREADX0(HFILEM,HRECFM,HFIPRI,KLENG,PFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
CHARACTER(LEN=*)          ,INTENT(IN) ::HFILEM   ! file name
CHARACTER(LEN=*)          ,INTENT(IN) ::HRECFM   ! name of the article to be written

CHARACTER(LEN=*)          ,INTENT(IN) ::HFIPRI   ! file for prints in FM

INTEGER,                   INTENT(IN) ::KLENG  ! length of the data field
REAL, &
                           INTENT(OUT)::PFIELD ! array containing the data field
INTEGER,                   INTENT(OUT)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(OUT)::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(OUT)::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
END SUBROUTINE FMREADX0
!
      SUBROUTINE FMREADX1(HFILEM,HRECFM,HFIPRI,KLENG,PFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
CHARACTER(LEN=*)          ,INTENT(IN) ::HFILEM   ! file name
CHARACTER(LEN=*)          ,INTENT(IN) ::HRECFM   ! name of the article to be written

CHARACTER(LEN=*)          ,INTENT(IN) ::HFIPRI   ! file for prints in FM

INTEGER,                   INTENT(IN) ::KLENG  ! length of the data field
REAL, DIMENSION(:), &
                           INTENT(OUT)::PFIELD ! array containing the data field
INTEGER,                   INTENT(OUT)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(OUT)::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(OUT)::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
END SUBROUTINE FMREADX1
!
!
      SUBROUTINE FMREADX2(HFILEM,HRECFM,HFIPRI,KLENG,PFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
CHARACTER(LEN=*)          ,INTENT(IN) ::HFILEM   ! file name
CHARACTER(LEN=*)          ,INTENT(IN) ::HRECFM   ! name of the article to be written

CHARACTER(LEN=*)          ,INTENT(IN) ::HFIPRI   ! file for prints in FM

INTEGER,                   INTENT(IN) ::KLENG  ! length of the data field
REAL, DIMENSION(:,:), &
                           INTENT(OUT)::PFIELD ! array containing the data field
INTEGER,                   INTENT(OUT)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(OUT)::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(OUT)::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
END SUBROUTINE FMREADX2
!
!
      SUBROUTINE FMREADX3(HFILEM,HRECFM,HFIPRI,KLENG,PFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
CHARACTER(LEN=*)          ,INTENT(IN) ::HFILEM   ! file name
CHARACTER(LEN=*)          ,INTENT(IN) ::HRECFM   ! name of the article to be written

CHARACTER(LEN=*)          ,INTENT(IN) ::HFIPRI   ! file for prints in FM

INTEGER,                   INTENT(IN) ::KLENG  ! length of the data field
REAL, DIMENSION(:,:,:), &
                           INTENT(OUT)::PFIELD ! array containing the data field
INTEGER,                   INTENT(OUT)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(OUT)::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(OUT)::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
END SUBROUTINE FMREADX3
!
!
      SUBROUTINE FMREADX4(HFILEM,HRECFM,HFIPRI,KLENG,PFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
CHARACTER(LEN=*)          ,INTENT(IN) ::HFILEM   ! file name
CHARACTER(LEN=*)          ,INTENT(IN) ::HRECFM   ! name of the article to be written

CHARACTER(LEN=*)          ,INTENT(IN) ::HFIPRI   ! file for prints in FM

INTEGER,                   INTENT(IN) ::KLENG  ! length of the data field
REAL, DIMENSION(:,:,:,:), &
                           INTENT(OUT)::PFIELD ! array containing the data field
INTEGER,                   INTENT(OUT)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(OUT)::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(OUT)::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
END SUBROUTINE FMREADX4
!
!
      SUBROUTINE FMREADX5(HFILEM,HRECFM,HFIPRI,KLENG,PFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
CHARACTER(LEN=*)          ,INTENT(IN) ::HFILEM   ! file name
CHARACTER(LEN=*)          ,INTENT(IN) ::HRECFM   ! name of the article to be written

CHARACTER(LEN=*)          ,INTENT(IN) ::HFIPRI   ! file for prints in FM

INTEGER,                   INTENT(IN) ::KLENG  ! length of the data field
REAL, DIMENSION(:,:,:,:,:), &
                           INTENT(OUT)::PFIELD ! array containing the data field
INTEGER,                   INTENT(OUT)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(OUT)::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(OUT)::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
END SUBROUTINE FMREADX5
!
!
      SUBROUTINE FMREADX6(HFILEM,HRECFM,HFIPRI,KLENG,PFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
CHARACTER(LEN=*)          ,INTENT(IN) ::HFILEM   ! file name
CHARACTER(LEN=*)          ,INTENT(IN) ::HRECFM   ! name of the article to be written

CHARACTER(LEN=*)          ,INTENT(IN) ::HFIPRI   ! file for prints in FM

INTEGER,                   INTENT(IN) ::KLENG  ! length of the data field
REAL, DIMENSION(:,:,:,:,:,:), &
                           INTENT(OUT)::PFIELD ! array containing the data field
INTEGER,                   INTENT(OUT)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(OUT)::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(OUT)::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
END SUBROUTINE FMREADX6
!
      SUBROUTINE FMREADN0(HFILEM,HRECFM,HFIPRI,KLENG,KFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
CHARACTER(LEN=*)          ,INTENT(IN) ::HFILEM   ! file name
CHARACTER(LEN=*)          ,INTENT(IN) ::HRECFM   ! name of the article to be written

CHARACTER(LEN=*)          ,INTENT(IN) ::HFIPRI   ! file for prints in FM

INTEGER,                   INTENT(IN) ::KLENG  ! length of the data field
INTEGER, &
                           INTENT(OUT)::KFIELD ! array containing the data field
INTEGER,                   INTENT(OUT)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(OUT)::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(OUT)::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
END SUBROUTINE FMREADN0
!
      SUBROUTINE FMREADN1(HFILEM,HRECFM,HFIPRI,KLENG,KFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
CHARACTER(LEN=*)          ,INTENT(IN) ::HFILEM   ! file name
CHARACTER(LEN=*)          ,INTENT(IN) ::HRECFM   ! name of the article to be written

CHARACTER(LEN=*)          ,INTENT(IN) ::HFIPRI   ! file for prints in FM

INTEGER,                   INTENT(IN) ::KLENG  ! length of the data field
INTEGER, DIMENSION(:), &
                           INTENT(OUT)::KFIELD ! array containing the data field
INTEGER,                   INTENT(OUT)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(OUT)::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(OUT)::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
END SUBROUTINE FMREADN1
!
      SUBROUTINE FMREADN2(HFILEM,HRECFM,HFIPRI,KLENG,KFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
CHARACTER(LEN=*)          ,INTENT(IN) ::HFILEM   ! file name
CHARACTER(LEN=*)          ,INTENT(IN) ::HRECFM   ! name of the article to be written

CHARACTER(LEN=*)          ,INTENT(IN) ::HFIPRI   ! file for prints in FM

INTEGER,                   INTENT(IN) ::KLENG  ! length of the data field
INTEGER, DIMENSION(:,:), &
                           INTENT(OUT)::KFIELD ! array containing the data field
INTEGER,                   INTENT(OUT)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(OUT)::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(OUT)::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
END SUBROUTINE FMREADN2
!
      SUBROUTINE FMREADL0(HFILEM,HRECFM,HFIPRI,KLENG,OFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
CHARACTER(LEN=*)          ,INTENT(IN) ::HFILEM   ! file name
CHARACTER(LEN=*)          ,INTENT(IN) ::HRECFM   ! name of the article to be written

CHARACTER(LEN=*)          ,INTENT(IN) ::HFIPRI   ! file for prints in FM

INTEGER,                   INTENT(IN) ::KLENG  ! length of the data field
LOGICAL, &
                           INTENT(OUT)::OFIELD ! array containing the data field
INTEGER,                   INTENT(OUT)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(OUT)::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(OUT)::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
END SUBROUTINE FMREADL0
!
      SUBROUTINE FMREADL1(HFILEM,HRECFM,HFIPRI,KLENG,OFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
CHARACTER(LEN=*)          ,INTENT(IN) ::HFILEM   ! file name
CHARACTER(LEN=*)          ,INTENT(IN) ::HRECFM   ! name of the article to be written

CHARACTER(LEN=*)          ,INTENT(IN) ::HFIPRI   ! file for prints in FM

INTEGER,                   INTENT(IN) ::KLENG  ! length of the data field
LOGICAL, DIMENSION(:), &
                           INTENT(OUT)::OFIELD ! array containing the data field
INTEGER,                   INTENT(OUT)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(OUT)::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(OUT)::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
END SUBROUTINE FMREADL1
!
      SUBROUTINE FMREADC0(HFILEM,HRECFM,HFIPRI,KLENG,HFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
CHARACTER(LEN=*)          ,INTENT(IN) ::HFILEM   ! file name
CHARACTER(LEN=*)          ,INTENT(IN) ::HRECFM   ! name of the article to be written

CHARACTER(LEN=*)          ,INTENT(IN) ::HFIPRI   ! file for prints in FM

INTEGER,                   INTENT(IN) ::KLENG  ! length of the data field
CHARACTER(LEN=*), &
                           INTENT(OUT)::HFIELD ! array containing the data field
INTEGER,                   INTENT(OUT)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(OUT)::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(OUT)::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
END SUBROUTINE FMREADC0
!
      SUBROUTINE FMREADT0(HFILEM,HRECFM,HFIPRI,KLENG,TFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
USE MODD_TYPE_DATE_SURF
!
CHARACTER(LEN=*)          ,INTENT(IN) ::HFILEM   ! file name
CHARACTER(LEN=*)          ,INTENT(IN) ::HRECFM   ! name of the article to be written

CHARACTER(LEN=*)          ,INTENT(IN) ::HFIPRI   ! file for prints in FM

INTEGER,                   INTENT(IN) ::KLENG  ! length of the data field
TYPE (DATE_TIME), &
                           INTENT(OUT)::TFIELD ! array containing the data field
INTEGER,                   INTENT(OUT)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(OUT)::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(OUT)::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
END SUBROUTINE FMREADT0
!
END INTERFACE
!
END MODULE MODI_FMREAD
