!     ######spl
MODULE MODI_FMWRIT
!##################
!
INTERFACE FMWRIT
      SUBROUTINE FMWRITX0(HFILEM,HRECFM,HFIPRI,KLENG,PFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
CHARACTER(LEN=*)          ,INTENT(IN) ::HFILEM   ! file name
CHARACTER(LEN=*)          ,INTENT(IN) ::HRECFM   ! name of the article to be written

CHARACTER(LEN=*)          ,INTENT(IN) ::HFIPRI   ! file for prints in FM

INTEGER,                   INTENT(IN) ::KLENG  ! length of the data field
REAL, &
                           INTENT(IN) ::PFIELD ! array containing the data field
INTEGER,                   INTENT(IN) ::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(IN) ::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(IN) ::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
END SUBROUTINE FMWRITX0
!
      SUBROUTINE FMWRITX1(HFILEM,HRECFM,HFIPRI,KLENG,PFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
CHARACTER(LEN=*)          ,INTENT(IN) ::HFILEM   ! file name
CHARACTER(LEN=*)          ,INTENT(IN) ::HRECFM   ! name of the article to be written

CHARACTER(LEN=*)          ,INTENT(IN) ::HFIPRI   ! file for prints in FM

INTEGER,                   INTENT(IN) ::KLENG  ! length of the data field
REAL, DIMENSION(:), &
                           INTENT(IN) ::PFIELD ! array containing the data field
INTEGER,                   INTENT(IN) ::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(IN) ::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(IN) ::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
END SUBROUTINE FMWRITX1
!
!
      SUBROUTINE FMWRITX2(HFILEM,HRECFM,HFIPRI,KLENG,PFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
CHARACTER(LEN=*)          ,INTENT(IN) ::HFILEM   ! file name
CHARACTER(LEN=*)          ,INTENT(IN) ::HRECFM   ! name of the article to be written

CHARACTER(LEN=*)          ,INTENT(IN) ::HFIPRI   ! file for prints in FM

INTEGER,                   INTENT(IN) ::KLENG  ! length of the data field
REAL, DIMENSION(:,:), &
                           INTENT(IN) ::PFIELD ! array containing the data field
INTEGER,                   INTENT(IN) ::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(IN) ::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(IN) ::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
END SUBROUTINE FMWRITX2
!
!
      SUBROUTINE FMWRITX3(HFILEM,HRECFM,HFIPRI,KLENG,PFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
CHARACTER(LEN=*)          ,INTENT(IN) ::HFILEM   ! file name
CHARACTER(LEN=*)          ,INTENT(IN) ::HRECFM   ! name of the article to be written

CHARACTER(LEN=*)          ,INTENT(IN) ::HFIPRI   ! file for prints in FM

INTEGER,                   INTENT(IN) ::KLENG  ! length of the data field
REAL, DIMENSION(:,:,:), &
                           INTENT(IN) ::PFIELD ! array containing the data field
INTEGER,                   INTENT(IN) ::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(IN) ::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(IN) ::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
END SUBROUTINE FMWRITX3
!
!
      SUBROUTINE FMWRITX4(HFILEM,HRECFM,HFIPRI,KLENG,PFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
CHARACTER(LEN=*)          ,INTENT(IN) ::HFILEM   ! file name
CHARACTER(LEN=*)          ,INTENT(IN) ::HRECFM   ! name of the article to be written

CHARACTER(LEN=*)          ,INTENT(IN) ::HFIPRI   ! file for prints in FM

INTEGER,                   INTENT(IN) ::KLENG  ! length of the data field
REAL, DIMENSION(:,:,:,:), &
                           INTENT(IN) ::PFIELD ! array containing the data field
INTEGER,                   INTENT(IN) ::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(IN) ::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(IN) ::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
END SUBROUTINE FMWRITX4
!
!
      SUBROUTINE FMWRITX5(HFILEM,HRECFM,HFIPRI,KLENG,PFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
CHARACTER(LEN=*)          ,INTENT(IN) ::HFILEM   ! file name
CHARACTER(LEN=*)          ,INTENT(IN) ::HRECFM   ! name of the article to be written

CHARACTER(LEN=*)          ,INTENT(IN) ::HFIPRI   ! file for prints in FM

INTEGER,                   INTENT(IN) ::KLENG  ! length of the data field
REAL, DIMENSION(:,:,:,:,:), &
                           INTENT(IN) ::PFIELD ! array containing the data field
INTEGER,                   INTENT(IN) ::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(IN) ::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(IN) ::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
END SUBROUTINE FMWRITX5
!
!
      SUBROUTINE FMWRITX6(HFILEM,HRECFM,HFIPRI,KLENG,PFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
CHARACTER(LEN=*)          ,INTENT(IN) ::HFILEM   ! file name
CHARACTER(LEN=*)          ,INTENT(IN) ::HRECFM   ! name of the article to be written

CHARACTER(LEN=*)          ,INTENT(IN) ::HFIPRI   ! file for prints in FM

INTEGER,                   INTENT(IN) ::KLENG  ! length of the data field
REAL, DIMENSION(:,:,:,:,:,:), &
                           INTENT(IN) ::PFIELD ! array containing the data field
INTEGER,                   INTENT(IN) ::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(IN) ::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(IN) ::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
END SUBROUTINE FMWRITX6
!
      SUBROUTINE FMWRITN0(HFILEM,HRECFM,HFIPRI,KLENG,KFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
CHARACTER(LEN=*)          ,INTENT(IN) ::HFILEM   ! file name
CHARACTER(LEN=*)          ,INTENT(IN) ::HRECFM   ! name of the article to be written

CHARACTER(LEN=*)          ,INTENT(IN) ::HFIPRI   ! file for prints in FM

INTEGER,                   INTENT(IN) ::KLENG  ! length of the data field
INTEGER, &
                           INTENT(IN) ::KFIELD ! array containing the data field
INTEGER,                   INTENT(IN) ::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(IN) ::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(IN) ::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
END SUBROUTINE FMWRITN0
!
      SUBROUTINE FMWRITN1(HFILEM,HRECFM,HFIPRI,KLENG,KFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
CHARACTER(LEN=*)          ,INTENT(IN) ::HFILEM   ! file name
CHARACTER(LEN=*)          ,INTENT(IN) ::HRECFM   ! name of the article to be written

CHARACTER(LEN=*)          ,INTENT(IN) ::HFIPRI   ! file for prints in FM

INTEGER,                   INTENT(IN) ::KLENG  ! length of the data field
INTEGER, DIMENSION(:), &
                           INTENT(IN) ::KFIELD ! array containing the data field
INTEGER,                   INTENT(IN) ::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(IN) ::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(IN) ::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
END SUBROUTINE FMWRITN1
!
      SUBROUTINE FMWRITN2(HFILEM,HRECFM,HFIPRI,KLENG,KFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
CHARACTER(LEN=*)          ,INTENT(IN) ::HFILEM   ! file name
CHARACTER(LEN=*)          ,INTENT(IN) ::HRECFM   ! name of the article to be written

CHARACTER(LEN=*)          ,INTENT(IN) ::HFIPRI   ! file for prints in FM

INTEGER,                   INTENT(IN) ::KLENG  ! length of the data field
INTEGER, DIMENSION(:,:), &
                           INTENT(IN) ::KFIELD ! array containing the data field
INTEGER,                   INTENT(IN) ::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(IN) ::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(IN) ::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
END SUBROUTINE FMWRITN2
!
      SUBROUTINE FMWRITL0(HFILEM,HRECFM,HFIPRI,KLENG,OFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
CHARACTER(LEN=*)          ,INTENT(IN) ::HFILEM   ! file name
CHARACTER(LEN=*)          ,INTENT(IN) ::HRECFM   ! name of the article to be written

CHARACTER(LEN=*)          ,INTENT(IN) ::HFIPRI   ! file for prints in FM

INTEGER,                   INTENT(IN) ::KLENG  ! length of the data field
LOGICAL, &
                           INTENT(IN) ::OFIELD ! array containing the data field
INTEGER,                   INTENT(IN) ::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(IN) ::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(IN) ::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
END SUBROUTINE FMWRITL0
!
      SUBROUTINE FMWRITL1(HFILEM,HRECFM,HFIPRI,KLENG,OFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
CHARACTER(LEN=*)          ,INTENT(IN) ::HFILEM   ! file name
CHARACTER(LEN=*)          ,INTENT(IN) ::HRECFM   ! name of the article to be written

CHARACTER(LEN=*)          ,INTENT(IN) ::HFIPRI   ! file for prints in FM

INTEGER,                   INTENT(IN) ::KLENG  ! length of the data field
LOGICAL,DIMENSION(:),  &
                           INTENT(IN) ::OFIELD ! array containing the data field
INTEGER,                   INTENT(IN) ::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(IN) ::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(IN) ::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
END SUBROUTINE FMWRITL1
!
      SUBROUTINE FMWRITC0(HFILEM,HRECFM,HFIPRI,KLENG,HFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
CHARACTER(LEN=*)          ,INTENT(IN) ::HFILEM   ! file name
CHARACTER(LEN=*)          ,INTENT(IN) ::HRECFM   ! name of the article to be written

CHARACTER(LEN=*)          ,INTENT(IN) ::HFIPRI   ! file for prints in FM

INTEGER,                   INTENT(IN) ::KLENG  ! length of the data field
CHARACTER(LEN=*), &
                           INTENT(IN) ::HFIELD ! array containing the data field
INTEGER,                   INTENT(IN) ::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(IN) ::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(IN) ::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
END SUBROUTINE FMWRITC0
!
      SUBROUTINE FMWRITT0(HFILEM,HRECFM,HFIPRI,KLENG,TFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
USE MODD_TYPE_DATE_SURF
!
CHARACTER(LEN=*)          ,INTENT(IN) ::HFILEM   ! file name
CHARACTER(LEN=*)          ,INTENT(IN) ::HRECFM   ! name of the article to be written

CHARACTER(LEN=*)          ,INTENT(IN) ::HFIPRI   ! file for prints in FM

INTEGER,                   INTENT(IN) ::KLENG  ! length of the data field
TYPE (DATE_TIME), &
                           INTENT(IN) ::TFIELD ! array containing the data field
INTEGER,                   INTENT(IN) ::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(IN) ::KLENCH ! length of comment string

CHARACTER(LEN=*)     ,INTENT(IN) ::HCOMMENT ! comment string

INTEGER,                   INTENT(OUT)::KRESP  ! return-code if problems araised
!
END SUBROUTINE FMWRITT0
!
END INTERFACE
!
END MODULE MODI_FMWRIT
