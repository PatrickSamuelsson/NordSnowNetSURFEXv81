MODULE MODI_gltools_readnam
INTERFACE
SUBROUTINE gltools_readnam(hmandatory,kluout,YGLTPARAM)
!
USE modd_glt_param, only: t_glt_param
USE modd_glt_const_thm
USE modi_gltools_nextval
USE modi_gltools_nwords
USE modi_gltools_strsplit
!
IMPLICIT NONE
!
LOGICAL, INTENT(IN),OPTIONAL           :: &
  hmandatory         ! Is a gltpar file mandatory ?
INTEGER, INTENT(IN),OPTIONAL           :: &
 kluout              ! imposed output logical unit ?
TYPE(t_glt_param),INTENT(INOUT)    :: YGLTPARAM
END SUBROUTINE gltools_readnam
END INTERFACE
END MODULE MODI_gltools_readnam
!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!GLT_LIC The GELATO model is a seaice model used in stand-alone or embedded mode. 
!GLT_LIC  It has been developed by Meteo-France. The holder of GELATO is Meteo-France.
!GLT_LIC  
!GLT_LIC  This software is governed by the CeCILL-C license under French law and biding
!GLT_LIC  by the rules of distribution of free software. See the CeCILL-C_V1-en.txt
!GLT_LIC  (English) and CeCILL-C_V1-fr.txt (French) for details. The CeCILL is a free
!GLT_LIC  software license, explicitly compatible with the GNU GPL
!GLT_LIC  (see http://www.gnu.org/licenses/license-list.en.html#CeCILL)
!GLT_LIC  
!GLT_LIC  The CeCILL-C licence agreement grants users the right to modify and re-use the
!GLT_LIC  software governed by this free software license. The exercising of this right
!GLT_LIC  is conditional upon the obligation to make available to the community the
!GLT_LIC  modifications made to the source code of the software so as to contribute to
!GLT_LIC  its evolution.
!GLT_LIC  
!GLT_LIC  In consideration of access to the source code and the rights to copy, modify
!GLT_LIC  and redistribute granted by the license, users are provided only with a limited
!GLT_LIC  warranty and the software's author, the holder of the economic rights, and the
!GLT_LIC  successive licensors only have limited liability. In this respect, the risks
!GLT_LIC  associated with loading, using, modifying and/or developing or reproducing the
!GLT_LIC  software by the user are brought to the user's attention, given its Free
!GLT_LIC  Software status, which may make it complicated to use, with the result that its
!GLT_LIC  use is reserved for developers and experienced professionals having in-depth
!GLT_LIC  computer knowledge. Users are therefore encouraged to load and test the
!GLT_LIC  suitability of the software as regards their requirements in conditions enabling
!GLT_LIC  the security of their systems and/or data to be ensured and, more generally, to
!GLT_LIC  use and operate it in the same conditions of security. 
!GLT_LIC  
!GLT_LIC  The GELATO sofware is cureently distibuted with the SURFEX software, available at 
!GLT_LIC  http://www.cnrm.meteo.fr/surfex. The fact that you download the software deemed that
!GLT_LIC  you had knowledge of the CeCILL-C license and that you accept its terms.
!GLT_LIC  Attempts to use this software in a way not complying with CeCILL-C license
!GLT_LIC  may lead to prosecution. 
!GLT_LIC 
! =======================================================================
! ====================== MODULE modi_gltools_readnam ======================
! =======================================================================
!
! Goal:
! -----
!   Reads model parameters from a "namelist" and compute other parameters
! from those read.
!
! Created : 2004/01 (D. Salas y Melia)
! Modified: 2015/07 (S. Senesi & D. Salas y Melia) Insertion in Surfex
!
! -------------------- BEGIN MODULE modi_gltools_readnam ------------------
!
!THXS_SFX!MODULE modi_gltools_readnam
!THXS_SFX!INTERFACE
!THXS_SFX!!
!THXS_SFX!SUBROUTINE gltools_readnam(hmandatory,kluout)
!THXS_SFX!LOGICAL, INTENT(IN),OPTIONAL           :: &
!THXS_SFX!  hmandatory         ! Is a gltpar file mandatory ?
!THXS_SFX!INTEGER, INTENT(IN),OPTIONAL           :: &
!THXS_SFX! kluout              ! imposed output logical unit ?
!THXS_SFX!END SUBROUTINE gltools_readnam
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_gltools_readnam
!
! ------------------- END MODULE modi_gltools_readnam ---------------------
!
!
! -----------------------------------------------------------------------
! ------------------------- SUBROUTINE gltools_readnam --------------------------
!
SUBROUTINE gltools_readnam(hmandatory,kluout,YGLTPARAM)
!
USE modd_glt_param, only: t_glt_param
USE modd_glt_const_thm
USE modi_gltools_nextval
USE modi_gltools_nwords
USE modi_gltools_strsplit
!
IMPLICIT NONE
!
LOGICAL, INTENT(IN),OPTIONAL           :: &
  hmandatory         ! Is a gltpar file mandatory ?
INTEGER, INTENT(IN),OPTIONAL           :: &
 kluout              ! imposed output logical unit ?
TYPE(t_glt_param),INTENT(INOUT)    :: YGLTPARAM
CHARACTER(1) ::  &
  ytag
CHARACTER(6), PARAMETER ::  &
  ypinpfile='gltpar'
CHARACTER(80) ::  &
  ypar,yval,yinsfld
CHARACTER(80), DIMENSION(:), ALLOCATABLE ::  &
  ylistfld 
CHARACTER(1000) ::  &
  yfldin
INTEGER ::  &
  iparlu,iok,infld,jl,icount, ierr
LOGICAL :: &
  gmandatory,gread
INTEGER, DIMENSION(:), ALLOCATABLE ::  &
  ilistfound 
REAL :: zjl
!
!
!
! 1. Initializations 
! ===================
!
!  Init default values
!
YGLTPARAM%dttave= 365. ! days 
gread=.TRUE.
!
!
! .. Try to open the parameter file
!
iparlu = 1
!
IF (PRESENT(hmandatory)) THEN
  gmandatory = hmandatory
ELSE
  gmandatory = .TRUE.
ENDIF
!
OPEN( UNIT=iparlu, FILE=TRIM(ADJUSTL(ypinpfile)), STATUS='OLD', &
  FORM='FORMATTED' , ERR= 230, IOSTAT=ierr)
230 CONTINUE
IF (ierr /= 0 ) THEN  ! File not found , or any other issue
  IF ( gmandatory ) THEN
    WRITE(*,*) "*** GELATO/readnam : issue opening gltpar "
    WRITE(*,*) 'We stop.'
    STOP
  ELSE
    gread=.FALSE.
  ENDIF
ENDIF
!
IF (gread) THEN
!
!   WRITE(*,*) "*** GELATO/readnam : gltpar found"
! Code below is purposely not indented , for easy comparison with previous version
!
! .. Introductory message
!
  IF (YGLTPARAM%lp1) WRITE(*,*) '       ----------------------------------------'
  IF (YGLTPARAM%lp1) WRITE(*,*) '        glt_gelato : READING NAMELIST ' // TRIM(ADJUSTL(ypinpfile))
  IF (YGLTPARAM%lp1) WRITE(*,*) '       ----------------------------------------'
!
! .. The list of all parameters available from glt_gelato 
! (for definitions of these parameters, see dmod_param)
!
  yfldin=  &
    'nmkinit nrstout nrstgl4 nthermo ndynami  &
&  nadvect ntimers ndyncor ncdlssh niceage  &
&  nicesal nmponds nsnwrad nleviti nsalflx  &
&  nextqoc nicesub cnflxin' 
  yfldin=TRIM(yfldin) // ' ' //  &
    'cfsidmp chsidmp ccsvdmp &
&  xfsidmpeft xhsidmpeft'
  yfldin=TRIM(yfldin) // ' ' //  &
    'cdiafmt cdialev navedia ninsdia ndiamax  &
&  cinsfld nsavinp nsavout nupdbud nprinto nprlast'
  yfldin=TRIM(yfldin) // ' ' //  &
    'nidate niter dtt'
  yfldin=TRIM(yfldin) // ' ' //  &
    'nt thick'
  yfldin=TRIM(yfldin) // ' ' //  &
    'nilay nslay xh0 xh1 xh2 xh3 xh4'
  yfldin=TRIM(yfldin) // ' ' //  &
    'ntstp ndte'
  yfldin=TRIM(yfldin) // ' ' //  &
    'xfsimax xicethcr xhsimin'
  yfldin=TRIM(yfldin) // ' ' //  &
    'alblc xlmelt xswhdfr albyngi albimlt albsmlt albsdry'
  yfldin=TRIM(yfldin) // ' ' //  &
    'ngrdlu nsavlu nrstlu n0vilu n0valu n2vilu n2valu nxvilu nxvalu  &
&  nibglu nspalu noutlu ntimlu'
  yfldin=TRIM(yfldin) // ' ' //  &
    'ciopath'
!
! .. Initialize control arrays
!
  infld=gltools_nwords( yfldin )
  ALLOCATE( ylistfld(infld) )
  ALLOCATE( ilistfound(infld) )
  ylistfld = gltools_strsplit(yfldin,infld)
  ilistfound = 0
!
!
!
! 2. Read all model parameter
! ============================
!
  iok=0
  icount=0
!
  DO WHILE ( iok==0 ) 
    CALL gltools_nextval( iparlu,ylistfld,ilistfound,iok,ypar,yval,YGLTPARAM%lwg )
!
!
! 2.1. Options to run glt_gelato
! --------------------------
!
    SELECT CASE ( TRIM(ADJUSTL(ypar)) )
!
    CASE('nmkinit') ; READ( yval,* ) YGLTPARAM%nmkinit
    CASE('nrstout') ; READ( yval,* ) YGLTPARAM%nrstout
    CASE('nrstgl4') ; READ( yval,* ) YGLTPARAM%nrstgl4
    CASE('nthermo') ; READ( yval,* ) YGLTPARAM%nthermo
    CASE('ndynami') ; READ( yval,* ) YGLTPARAM%ndynami
    CASE('nadvect') ; READ( yval,* ) YGLTPARAM%nadvect
    CASE('ntimers') ; READ( yval,* ) YGLTPARAM%ntimers
    CASE('ndyncor') ; READ( yval,* ) YGLTPARAM%ndyncor
    CASE('ncdlssh') ; READ( yval,* ) YGLTPARAM%ncdlssh
    CASE('niceage') ; READ( yval,* ) YGLTPARAM%niceage
    CASE('nicesal') ; READ( yval,* ) YGLTPARAM%nicesal
    CASE('nmponds') ; READ( yval,* ) YGLTPARAM%nmponds
    CASE('nsnwrad') ; READ( yval,* ) YGLTPARAM%nsnwrad
    CASE('nleviti') ; READ( yval,* ) YGLTPARAM%nleviti
    CASE('nsalflx') ; READ( yval,* ) YGLTPARAM%nsalflx
    CASE('nextqoc') ; READ( yval,* ) YGLTPARAM%nextqoc
    CASE('nicesub') ; READ( yval,* ) YGLTPARAM%nicesub
    CASE('cnflxin') ; READ( yval,* ) YGLTPARAM%cnflxin
!
!
! 2.2. Damping and restoring
! ---------------------------
!
    CASE('cfsidmp')    ; READ( yval,*) YGLTPARAM%cfsidmp
    CASE('xfsidmpeft') ; READ( yval,*) YGLTPARAM%xfsidmpeft
    CASE('chsidmp')    ; READ( yval,*) YGLTPARAM%chsidmp
    CASE('xhsidmpeft') ; READ( yval,*) YGLTPARAM%xhsidmpeft
!
!
! 2.3. Diagnostics glt_output
! ------------------------
!
    CASE('cdiafmt') ; READ( yval,* ) YGLTPARAM%cdiafmt
    CASE('cdialev') ; READ( yval,* ) YGLTPARAM%cdialev
    CASE('dttave')  ; READ( yval,* ) YGLTPARAM%dttave
    CASE('navedia') ; READ( yval,* ) YGLTPARAM%navedia
    CASE('ninsdia') ; READ( yval,* ) YGLTPARAM%ninsdia
    CASE('ndiamax') ; READ( yval,* ) YGLTPARAM%ndiamax 
      ! Caller program (e.g. Surfex) may have already allocate cinsfld  
      ! differently when settings Gelato hard defaults 
      IF (ALLOCATED(YGLTPARAM%cinsfld)) DEALLOCATE(YGLTPARAM%cinsfld)
      ALLOCATE( YGLTPARAM%cinsfld(YGLTPARAM%ndiamax) )
      YGLTPARAM%cinsfld(:) = ''
    CASE('cinsfld') ; READ( yval,* ) yinsfld
      IF ( ALLOCATED(YGLTPARAM%cinsfld) ) THEN
          icount = icount + 1
          YGLTPARAM%cinsfld(icount) = yinsfld
        ELSE
          GOTO 200
      ENDIF
    CASE('nsavinp') ; READ( yval,* ) YGLTPARAM%nsavinp
    CASE('nsavout') ; READ( yval,* ) YGLTPARAM%nsavout
    CASE('nupdbud') ; READ( yval,* ) YGLTPARAM%nupdbud
    CASE('nprinto') ; READ( yval,* ) YGLTPARAM%nprinto
    CASE('nprlast') ; READ( yval,* ) YGLTPARAM%nprlast
!
!
! 2.4. Run date position and time step
! ------------------------------------
!
    CASE('nidate') ; READ( yval,* ) YGLTPARAM%nidate
    CASE('niter')  ; READ( yval,* ) YGLTPARAM%niter
    CASE('dtt')    ; READ( yval,* ) YGLTPARAM%dtt
!
!
! 2.5. Number of ice categories
! -----------------------------
!
    CASE('nt')    ; 
       READ( yval,* ) YGLTPARAM%nt ; 
       ! Caller program (e.g. Surfex) may have already allocate thick  
       ! differently when settings Gelato hard defaults 
       IF (ALLOCATED(YGLTPARAM%thick)) DEALLOCATE(YGLTPARAM%thick)  
       ALLOCATE( YGLTPARAM%thick(YGLTPARAM%nt+1) )
    CASE('thick') 
      IF ( ALLOCATED(YGLTPARAM%thick) ) THEN
          READ( yval,*,END=100 ) YGLTPARAM%thick
        ELSE
          GOTO 100
      ENDIF
!
!
! 2.6. Number of layers in the ice-snow slab
! ------------------------------------------
!
    CASE('nilay') ; READ( yval,* ) YGLTPARAM%nilay
    CASE('nslay') ; READ( yval,* ) YGLTPARAM%nslay
    CASE('xh0')   ; READ( yval,* ) YGLTPARAM%xh0
    CASE('xh1')   ; READ( yval,* ) YGLTPARAM%xh1
    CASE('xh2')   ; READ( yval,* ) YGLTPARAM%xh2
    CASE('xh3')   ; READ( yval,* ) YGLTPARAM%xh3
    CASE('xh4')   ; READ( yval,* ) YGLTPARAM%xh4
!
!
! 2.7. Elastic Viscous-Plastic sea ice rheology parameters
! ---------------------------------------------------------
!
    CASE('ntstp')    ; READ( yval,* ) YGLTPARAM%ntstp
    CASE('ndte')     ; READ( yval,* ) YGLTPARAM%ndte
!
!
! 2.8. Limit Values for sea ice
! ------------------------------
!
    CASE('xfsimax')  ; READ( yval,* ) YGLTPARAM%xfsimax
    CASE('xicethcr') ; READ( yval,* ) YGLTPARAM%xicethcr
    CASE('xhsimin')  ; READ( yval,* ) YGLTPARAM%xhsimin
!
!
! 2.9. Parameterizations
! -----------------------
!
    CASE('alblc')    ; READ( yval,* ) YGLTPARAM%alblc
    CASE('xlmelt')   ; READ( yval,* ) YGLTPARAM%xlmelt
    CASE('xswhdfr')  ; READ( yval,* ) YGLTPARAM%xswhdfr
    CASE('albyngi')  ; READ( yval,* ) YGLTPARAM%albyngi
    CASE('albimlt')  ; READ( yval,* ) YGLTPARAM%albimlt
    CASE('albsmlt')  ; READ( yval,* ) YGLTPARAM%albsmlt
    CASE('albsdry')  ; READ( yval,* ) YGLTPARAM%albsdry
!
!
! 2.10. Logical units
! --------------------
!
    CASE('ngrdlu') ; READ( yval,* ) YGLTPARAM%ngrdlu
    CASE('nsavlu') ; READ( yval,* ) YGLTPARAM%nsavlu
    CASE('nrstlu') ; READ( yval,* ) YGLTPARAM%nrstlu
    CASE('n0vilu') ; READ( yval,* ) YGLTPARAM%n0vilu
    CASE('n0valu') ; READ( yval,* ) YGLTPARAM%n0valu
    CASE('n2vilu') ; READ( yval,* ) YGLTPARAM%n2vilu
    CASE('n2valu') ; READ( yval,* ) YGLTPARAM%n2valu
    CASE('nxvilu') ; READ( yval,* ) YGLTPARAM%nxvilu
    CASE('nxvalu') ; READ( yval,* ) YGLTPARAM%nxvalu
    CASE('nibglu') ; READ( yval,* ) YGLTPARAM%nibglu
    CASE('nspalu') ; READ( yval,* ) YGLTPARAM%nspalu
    CASE('noutlu') ; READ( yval,* ) YGLTPARAM%noutlu ; IF (PRESENT(KLUOUT)) YGLTPARAM%noutlu=kluout
    CASE('ntimlu') ; READ( yval,* ) YGLTPARAM%ntimlu
!
!
! 2.11. Path to keep Gelato I/O fields
! -------------------------------------
!
    CASE('ciopath') ; READ( yval,* ) YGLTPARAM%ciopath
!
!
! 2.12. End of case select
! -------------------------
!
    CASE DEFAULT 
      IF (YGLTPARAM%lwg) WRITE(*,110) TRIM(ypar)
!
    END SELECT 
  END DO
!
!
! 2.13. Close the parameter file
! -------------------------------
!
  CLOSE(1)
  DEALLOCATE( ilistfound )
  DEALLOCATE( ylistfld )
!
ENDIF
!
!
! 3. Parameters computed from previous set
! ========================================
!
! 3.1. Dimensions of exchanged variables
! ---------------------------------------
!
SELECT CASE ( TRIM(YGLTPARAM%cnflxin) )
!
CASE('mixed')  ; YGLTPARAM%nnflxin = 0 
  if (YGLTPARAM%lp1) WRITE(*,*) 'We are using single physics (one input flux)'
CASE('double') ; YGLTPARAM%nnflxin = 1
  IF (YGLTPARAM%lp1) WRITE(*,*) 'We are using double physics (two input fluxes)'  
CASE('multi')  ; YGLTPARAM%nnflxin = YGLTPARAM%nt
  IF (YGLTPARAM%lp1) WRITE(*,*) 'We are using multiple physics (one flux per ice cat + water)'
CASE DEFAULT
  IF (YGLTPARAM%lp1) WRITE(*,*) 'We stop. Invalid parameter cnflxin = ' // TRIM(YGLTPARAM%cnflxin) ; STOP
!
END SELECT
!
!
! 3.2. Number of layers in an ice/snow slab
! -----------------------------------------
!
!  - nl         : number of grid points in the ice+snow slab along the
! vertical
!
YGLTPARAM%nl = YGLTPARAM%nilay + YGLTPARAM%nslay
!
! .. thickness of the different levels at t and w points (from top to bottom)
!
! Caller program (e.g. Surfex) may have already allocate some   
! arrays differently when settings Gelato hard defaults 
IF (ALLOCATED(YGLTPARAM%sf3t)) DEALLOCATE(YGLTPARAM%sf3t)
ALLOCATE( YGLTPARAM%sf3t(YGLTPARAM%nilay) )
IF (ALLOCATED(YGLTPARAM%e3w)) DEALLOCATE(YGLTPARAM%e3w)
ALLOCATE( YGLTPARAM%e3w(YGLTPARAM%nilay) )
!
! .. thickness of the different levels at t points (from bottom to top)
!
IF (ALLOCATED(YGLTPARAM%sf3tinv)) DEALLOCATE(YGLTPARAM%sf3tinv)
ALLOCATE( YGLTPARAM%sf3tinv(YGLTPARAM%nilay) )
!
! .. depth of the different inter-levels (from top to bottom)
!
IF (ALLOCATED(YGLTPARAM%depth)) DEALLOCATE(YGLTPARAM%depth)
ALLOCATE( YGLTPARAM%depth(YGLTPARAM%nilay+1) )
!
! .. height of the different inter-levels (from bottom to top)
!
IF (ALLOCATED(YGLTPARAM%height)) DEALLOCATE(YGLTPARAM%height)
ALLOCATE( YGLTPARAM%height(YGLTPARAM%nilay+1) )
!
! .. compute vertical scale factors at t points (sf3t) and w points (e3w)
!
DO jl=1,YGLTPARAM%nilay
  zjl=FLOAT(jl)
!  depth(jl)=-xh0+zjl*xh1+xh2*xh3*log( cosh((zjl-xh4)/xh3) )
  YGLTPARAM%sf3t(jl)=YGLTPARAM%xh1+YGLTPARAM%xh2*tanh( (zjl+0.5-YGLTPARAM%xh4)/YGLTPARAM%xh3 )
  YGLTPARAM%e3w(jl)=YGLTPARAM%xh1+YGLTPARAM%xh2*tanh( (zjl-YGLTPARAM%xh4)/YGLTPARAM%xh3 )
END DO
!
! Ensure perfect normalization of sf3t (e3w is not necessarily normalized)
YGLTPARAM%sf3t(:)=YGLTPARAM%sf3t(:)/SUM(YGLTPARAM%sf3t)
!
! Vertical scale factor at t points, but from bottom to top
DO jl=1,YGLTPARAM%nilay
  YGLTPARAM%sf3tinv(jl) = YGLTPARAM%sf3t(YGLTPARAM%nilay+1-jl)
END DO
!
! Level boundaries height from inferior water/ice interface
YGLTPARAM%height(1)=0.
DO jl=2,YGLTPARAM%nilay+1
  YGLTPARAM%height(jl) = YGLTPARAM%height(jl-1) + YGLTPARAM%sf3tinv(jl-1)
END DO
!
! Ensure perfect normalization of height
YGLTPARAM%height(:)=YGLTPARAM%height(:)/YGLTPARAM%height(YGLTPARAM%nilay+1)
!
! Depth of level boundaries from top air/ice interface or snow/ice interface
!zjl=FLOAT(nilay+1)
!depth(nilay+1)=-xh0+zjl*xh1+xh2*xh3*log( cosh((zjl-xh4)/xh3) )
YGLTPARAM%depth(:) = 1.-YGLTPARAM%height(:)
!
!
! 3.3. Get diagnostics options
! ----------------------------
!
SELECT CASE ( TRIM(YGLTPARAM%cdiafmt) )
!
  CASE('GELATO','VMAR5')
    YGLTPARAM%ndiap1=0 ; YGLTPARAM%ndiap2=0 ; YGLTPARAM%ndiap3=0 ; YGLTPARAM%ndiapx=0
    DO jl=1,LEN( TRIM(YGLTPARAM%cdialev) ) 
      ytag=YGLTPARAM%cdialev(jl:jl)
      SELECT CASE ( ytag )
        CASE('1') ; YGLTPARAM%ndiap1=1
        CASE('2') ; YGLTPARAM%ndiap2=1
        CASE('3') ; YGLTPARAM%ndiap3=1
        CASE('x') ; YGLTPARAM%ndiapx=1
        CASE DEFAULT
          IF (YGLTPARAM%lwg) THEN
            WRITE(*,*) ' '
            WRITE(*,*) '              glt_gelato FATAL ERROR' 
            WRITE(*,*) '            **********************' 
            WRITE(*,*) ' Diagnostic code = '//ytag//' in cdialev ignored.'
            WRITE(*,*) '  (illegal with glt_output format = '//TRIM(YGLTPARAM%cdiafmt)//')'
            WRITE(*,*) ' We stop.'
          ENDIF
          STOP
      END SELECT
    END DO
  CASE('XIOS')
    YGLTPARAM%ndiap1=1 ; YGLTPARAM%ndiap2=1 ; YGLTPARAM%ndiap3=1 ; YGLTPARAM%ndiapx=1
    IF (YGLTPARAM%lwg) THEN
      WRITE(*,*) ' '
      WRITE(*,*) '              Using XIOS for diagnostics output' 
      WRITE(*,*) ' '
    ENDIF
  CASE DEFAULT
    IF (YGLTPARAM%lwg) THEN
      WRITE(*,*) ' '
      WRITE(*,*) '              glt_gelato FATAL ERROR' 
      WRITE(*,*) '            **********************' 
      WRITE(*,*) ' cdiafmt was set to '//YGLTPARAM%cdiafmt//' in gltpar.' 
      WRITE(*,*) ' Only GELATO and VMAR5 are legal. We stop.'
    ENDIF
    STOP
END SELECT  
!
!
! 3.4. Parameters for sea ice incremental remapping
! --------------------------------------------------
!
! .. Number of categories
!
YGLTPARAM%ncat = YGLTPARAM%nt
!
! .. Number of ice layers
!
YGLTPARAM%nilyr = YGLTPARAM%nilay
!
! .. Number of categories by number of ice layers
!
YGLTPARAM%ntilay = YGLTPARAM%ncat*YGLTPARAM%nilyr
!
! .. Number of advected tracers
!
YGLTPARAM%na = YGLTPARAM%nt*( YGLTPARAM%niceage+YGLTPARAM%nicesal+YGLTPARAM%nmponds )
!
!
! 3.5. Parameters for constraint
! -------------------------------
!
YGLTPARAM%ntd=0        ! Will disable sit_d array allocation and use of constraint data
IF ( TRIM(YGLTPARAM%cfsidmp)=='DAMP' .OR. TRIM(YGLTPARAM%cfsidmp)=='PRESCRIBE' ) THEN
  YGLTPARAM%ntd=1      ! Will enable sit_d array allocation and trigger constraint
ELSE IF ( TRIM(YGLTPARAM%cfsidmp)/='NONE' ) THEN
  WRITE(*,*) "cfsidmp must be 'DAMP' or 'PRESCRIBE'" 
  WRITE(*,*) " - You specified cfsidmp=" // TRIM(YGLTPARAM%cfsidmp)
  STOP
ENDIF
!
IF ( TRIM(YGLTPARAM%chsidmp)=='DAMP_ADD' .OR. TRIM(YGLTPARAM%chsidmp)=='DAMP_FAC' .OR.  &
     TRIM(YGLTPARAM%chsidmp)=='PRESCRIBE' ) THEN
  YGLTPARAM%ntd=1
ELSE IF ( TRIM(YGLTPARAM%chsidmp)/='NONE' ) THEN
  WRITE(*,*) "chsidmp must be 'DAMP_ADD', 'DAMP_FAC'' or 'PRESCRIBE'" 
  WRITE(*,*) " - You specified chsidmp=" // TRIM(YGLTPARAM%chsidmp)
  STOP
ENDIF
RETURN
!
!
!
! 4. Error messages
! ==================
!
IF (YGLTPARAM%lp1) WRITE(*,*) '        ---------------------------------------------------'
IF (YGLTPARAM%lp1) WRITE(*,*) '          gelato : READING OF gltpar INPUT FILE COMPLETED '
IF (YGLTPARAM%lp1) WRITE(*,*) '        ---------------------------------------------------'
!
100 CONTINUE
!
IF (YGLTPARAM%lwg) THEN
  WRITE(*,*) "*** GELATO/readnam : &
& dimension of 'thick' not consistent with nt, &
& or you declared 'thick' before 'nt' in gltpar"
  WRITE(*,*) 'We stop.'
ENDIF
STOP
!
200 CONTINUE
! 
IF (YGLTPARAM%lwg) THEN
  WRITE(*,*) "*** GELATO/readnam : &
& dimension of 'cinsfld' not consistent with nt, &
& or you declared 'cinsfld' before 'ndiamax' in gltpar"
  WRITE(*,*) 'We stop.'
ENDIF
STOP
!
110 FORMAT("* GELATO/readnam : parameter '",A,"' ignored.")
!
END SUBROUTINE gltools_readnam
!
! ---------------------- END SUBROUTINE gltools_readnam -------------------------
! -----------------------------------------------------------------------
