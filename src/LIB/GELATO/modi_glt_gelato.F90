MODULE MODI_glt_gelato
INTERFACE
SUBROUTINE glt_gelato( tpglt,ygltparam,ygltvhd)
!
!
! 1. Declarations
! ================
!
! 1.1. Module use declarations
! -----------------------------
!
!#if ! defined in_surfex
!USE mpi
!#endif
!
! * Contains physical constants.
USE modd_glt_const_thm 
!
#if ! defined in_surfex
! * Contains physical constants for elastic viscous-plastic dynamics 
USE modd_glt_const_evp
#endif
!
! * Contains types_glt definitions.
USE modd_types_glt 
!
! * To send an error message
USE modi_gltools_glterr
!
! * To initialize the control of energy budget
USE modi_glt_inibud
!
! * Sea ice and leads thermodynamics.
USE modi_glt_thermo
!
! * To print statistics in glt_gelato glt_output file.
USE mode_glt_info
!
USE modi_glt_updbud
USE modi_gltools_timers
!
USE MODD_GLT_PARAM, ONLY : t_glt_param
USE MODD_GLT_VHD, ONLY : t_glt_vhd
!
USE mode_glt_init

! * No variables are implicitely declared. 
IMPLICIT NONE
!
!
! 1.2. Dummy arguments
! ---------------------
!
TYPE(t_glt), INTENT(inout)       ::  tpglt
TYPE(t_glt_param), INTENT(inout) ::  ygltparam
TYPE(t_glt_vhd), INTENT(inout)   ::  ygltvhd
END SUBROUTINE glt_gelato
END INTERFACE
END MODULE MODI_glt_gelato
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
! ========================== MODULE modi_glt_gelato =========================
! =======================================================================
!
!                                                               
!             glt_gelato Sea Ice Dynamics and Thermodynamics 
!                          @(#)5.18 version
!                                                                       
!                        David Salas y Melia
!                                                                       
!                                                                       
! * glt_gelato determines the evolution of sea ice state variables,
! considering both dynamics and thermodynamics on a specified
! gridded region and within given time boundaries.
!
! Created : 1999 (D. Salas y Melia) 
!          Can be called from glt.f90 main program (Gelato in stand-alone
!          mode) or from an ocean model
! Modified: 2000-2008 (D. Salas y Melia) 
!          Many changes due to changes in thermodynamics, dynamics, 
!          diagnostics...
! Modified: 2009 (D. Salas y Melia)
!          Substantial interface change. Use of structured, optional
!          arguments to handle double or single physics. 
! Modified: 2012/05 (D. Salas y Melia)
!          Parallel dynamics, advection and redistribution
! Modified: 2012/11 (D. Salas y Melia)
!          Changes to introduce sea ice damping + total parallelism
!
! ----------------------- BEGIN MODULE modi_glt_gelato ----------------------
!
!THXS_SFX!MODULE modi_glt_gelato
!THXS_SFX!INTERFACE 
!THXS_SFX!!
!THXS_SFX!SUBROUTINE glt_gelato( tpglt )
!THXS_SFX!!
!THXS_SFX!  USE modd_types_glt
!THXS_SFX!  TYPE(t_glt), INTENT(inout) ::  &
!THXS_SFX!    tpglt
!THXS_SFX!END SUBROUTINE glt_gelato
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_gelato
!
! ----------------------- END MODULE modi_glt_gelato ------------------------
!
!
! -----------------------------------------------------------------------
! ----------------------- SUBROUTINE glt_gelato -------------------------
!
SUBROUTINE glt_gelato( tpglt,ygltparam,ygltvhd)
!
!
! 1. Declarations
! ================
!
! 1.1. Module use declarations
! -----------------------------
!
!#if ! defined in_surfex
!USE mpi
!#endif
!
! * Contains physical constants.
USE modd_glt_const_thm 
!
#if ! defined in_surfex
! * Contains physical constants for elastic viscous-plastic dynamics 
USE modd_glt_const_evp
#endif
!
! * Contains types_glt definitions.
USE modd_types_glt 
!
! * To send an error message
USE modi_gltools_glterr
!
! * To initialize the control of energy budget
USE modi_glt_inibud
!
! * Sea ice and leads thermodynamics.
USE modi_glt_thermo
!
! * To print statistics in glt_gelato glt_output file.
USE mode_glt_info
!
USE modi_glt_updbud
USE modi_gltools_timers
!
USE MODD_GLT_PARAM, ONLY : t_glt_param
USE MODD_GLT_VHD, ONLY : t_glt_vhd
!
USE mode_glt_init

! * No variables are implicitely declared. 
IMPLICIT NONE
!
!
! 1.2. Dummy arguments
! ---------------------
!
TYPE(t_glt), INTENT(inout)       ::  tpglt
TYPE(t_glt_param), INTENT(inout) ::  ygltparam
TYPE(t_glt_vhd), INTENT(inout)   ::  ygltvhd
!
!
!
! 2. Initialization (every time step)
! ====================================
!
! 2.1. Local variables
! ---------------------
!
IF (ygltparam%lp1) THEN
  WRITE(ygltparam%noutlu,*) ' '
  WRITE(ygltparam%noutlu,*) '  ** LEVEL 2 - SUBROUTINE GELATO'
  WRITE(ygltparam%noutlu,*) ' '
ENDIF
!
!
! 2.2. Initialize lead temperature and sea ice-ocean fluxes
! ----------------------------------------------------------
!

CALL initfl( tpglt%tfl,ygltparam%nx,ygltparam%ny )

!
! 2.3. Initialize diagnostics
! ----------------------------
!
CALL inidia( tpglt%ind,tpglt%dia,tpglt%cdia0,tpglt%cdia,&
    ygltparam%ndiamax,ygltparam%nx,ygltparam%ny )
CALL gltools_timers(ygltparam%ntimers,ygltparam%ntimlu,ygltparam%ntimnum,&
ygltparam%xtime,ygltparam%clabel,ygltparam%lwg,'end inidia') 
!
!
! 2.4. Budgets initialization
! ----------------------------
!
CALL glt_inibud( tpglt%bud )
!

CALL glt_info_si( 'Initial conditions:',tpglt%dom,&
ygltparam%niceage,ygltparam%nicesal,ygltparam%nl,ygltparam%nmponds,ygltparam%noutlu,ygltparam%nt,ygltparam%nx,ygltparam%ny,&
ygltparam%lp2,ygltparam%lp3,&
tpsit=tpglt%sit )
CALL gltools_timers(ygltparam%ntimers,ygltparam%ntimlu,ygltparam%ntimnum,&
ygltparam%xtime,ygltparam%clabel,ygltparam%lwg,'end inibud')
!
!
!
! 3. Input data
! ==============
!
! .. Print level
!
IF ( tpglt%ind%cur==tpglt%ind%end ) ygltparam%nprinto = ygltparam%nprlast 
ygltparam%lp1 = (ygltparam%lwg.AND.ygltparam%nprinto>=1)
ygltparam%lp2 = (ygltparam%lwg.AND.ygltparam%nprinto>=2)
ygltparam%lp3 = (ygltparam%lwg.AND.ygltparam%nprinto>=3)
ygltparam%lp4 = (ygltparam%lwg.AND.ygltparam%nprinto>=4)
ygltparam%lp5 = (ygltparam%lwg.AND.ygltparam%nprinto>=5)
!
!
! 3.1. Sea ice salinity initialization
! -------------------------------------
!
! .. This is done only at first time step. If no proper salinity 
! field was found in the restart, the bulk salinity is deduced from
! sea surface salinity (hence this routine must be invoked after 
! ocean surface forcing fields are obtained !)
!

IF ( tpglt%ind%cur==tpglt%ind%beg ) CALL inisal(tpglt%dom,tpglt%tml,tpglt%sit,&
    ygltparam%nicesal,ygltparam%nt,ygltparam%nx,ygltparam%ny )
!

!
! 3.2. First energy update budget
! --------------------------------
!
! .. This is done even if glt_updbud flag is off, to allow the computation
! of certain diagnostics if wished by the user.
!

CALL glt_updbud( 1,'Initial conditions:',  &
  tpglt%dom,tpglt%tml,tpglt%tfl,tpglt%atm_all,tpglt%blkw,tpglt%blki,  &
  tpglt%sit,tpglt%sil,tpglt%bud,&
  ygltparam%niceage,ygltparam%nicesal,ygltparam%nilay,ygltparam%nmponds,ygltparam%nl,ygltparam%noutlu,ygltparam%nprinto,&
  ygltparam%nslay,ygltparam%nsnwrad,ygltparam%nt,ygltparam%nx,ygltparam%ny,&
  ygltparam%dtt,ygltparam%xdomsrf_g,ygltparam%lp1,ygltparam%lp2,ygltparam%lp3,ygltparam%lwg,ygltparam%sf3tinv)
!

!
!
! 4. Sea ice and leads thermodynamics
! ====================================
!
IF ( ygltparam%ntd==0 ) THEN
!
! .. Thermodynamics without sea ice constraint
!

  CALL glt_thermo  &
      ( ygltparam,ygltvhd,&
    tpglt%dom,tpglt%ust,tpglt%tml,tpglt%atm_all,tpglt%blkw,tpglt%blki,  &
    tpglt%bud,tpglt%dia,tpglt%tfl,tpglt%sit,tpglt%sil )

ELSE
! 
! .. Thermodynamics with sea ice constraint (no energy conservation)
!
  CALL glt_thermo  &
      ( ygltparam,ygltvhd,&
    tpglt%dom,tpglt%ust,tpglt%tml,tpglt%atm_all,tpglt%blkw,tpglt%blki,  &
    tpglt%bud,tpglt%dia,tpglt%tfl,tpglt%sit,tpglt%sil,tpsit_d=tpglt%sit_d )
        
ENDIF
!
CALL gltools_timers(ygltparam%ntimers,ygltparam%ntimlu,ygltparam%ntimnum,&
ygltparam%xtime,ygltparam%clabel,ygltparam%lwg,'end thermo')
!
!
IF (ygltparam%lp1) THEN
  WRITE(ygltparam%noutlu,*) ' '
  WRITE(ygltparam%noutlu,*) '  ** LEVEL 2 - END SUBROUTINE GELATO'
  WRITE(ygltparam%noutlu,*) ' '
ENDIF
!
END SUBROUTINE glt_gelato
!
! ====================== END SUBROUTINE glt_gelato ======================
! =======================================================================
