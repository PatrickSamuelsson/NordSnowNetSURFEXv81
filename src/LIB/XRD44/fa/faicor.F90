! Oct-2012 P. Marguinaud 64b LFI
! Jan-2011 P. Marguinaud Thread-safe FA
SUBROUTINE FAICOR_FORT           &
&                     (FA)
USE FA_MOD, ONLY : FA_COM, FAGR1TAB
USE PARKIND1, ONLY : JPRB
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE LFI_PRECISION
IMPLICIT NONE
!****
!      Sous-programme INTERNE du logiciel de Fichiers ARPEGE:
!      Initialisation des tableaux permettant la correspondance
!      entre nom du champ dans FA et ses descripteurs GRIB.
!
!      **  FA%NBPARC ne doit pas depasser FA%JPXPAR !! **
!**
!
TYPE(FA_COM) :: FA
INTEGER (KIND=JPLIKB) I, J
REAL (KIND=JPDBLR), PARAMETER :: ZONOVG = 1.0_JPDBLR / 9.80665_JPDBLR, &
                               & Z100   = 100._JPDBLR
LOGICAL :: LLALLOC

!**
!     1.  -  TRAITEMENT DES CHAMPS MONO NIVEAU
!-----------------------------------------------------------------------
!
!     1.1 -  Champs "profond"
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('FAICOR_MT',0,ZHOOK_HANDLE)

DO I = 1, 2

LLALLOC = I == 2

IF (LLALLOC) THEN
  ALLOCATE (FA%YGR1TAB (FA%NBPARC))
ENDIF

FA%NBPARC = 0

! 1 : Table                                                               
! 2 : Parameter                                                           
! 3 : LevelType                                                           
! 4 : FirstLevel                                                          
! 5 : SecondLevel                                                         
! 6 : 2 => min/max, 4 => sum since last term, 8 => sum since the beginning
! 7 : ScalingFactor                                                       
! 8 : BitsPerValue (usual value)                                          

CALL T ('PROF', 'TEMPERATURE ', (/   1,  11,   111,  10,   0, 0,  0,  12 /))
CALL T ('PROF', 'RESERV.GLACE', (/   1, 152,   112,   0, 250, 0,  0,  16 /))
CALL T ('PROF', 'RESERV.EAU' ,  (/   1, 153,   112,   0, 250, 0,  0,  16 /))
CALL T ('PROF', 'RUISSELLEMEN', (/ 149,   2,   112,  10, 300, 4,  0,  12 /))
CALL T ('PROF', 'FLUX DE GEL ', (/ 149,  73,   112,  10, 300, 4,  0,  12 /))
CALL T ('PROF', 'PROP.RMAX.EA', (/ 149,  77,   112,  10, 300, 0,  0,  12 /))
CALL T ('PROF', 'XRUISSELLEME', (/ 149,  85,   112,  10, 300, 0,  0,  12 /))

!
!     1.2 -  Champs de surface
!
CALL T ('SURF', 'PRESSION    ', (/   1,   1,   1,     0,   0, 0,  0,  16 /))
CALL T ('SURF', 'GEOPOTENTIEL', (/   1,   8,   1,     0,   0, 0,  0,  24 /), PMULTI=ZONOVG)
CALL T ('SURF', 'TEMPERATURE ', (/   1,  11,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'FLU.MSUBL.NE', (/   1,  57,   1,     0,   0, 8,  0,  16 /))
CALL T ('SURF', 'FLU.MEVAP.EA', (/   1,  57,   1,     0,   0, 8,  0,  16 /))
CALL T ('SURF', 'FLU.MTOTA.NE', (/   1,  57,   1,     0,   0, 8,  0,  16 /))
CALL T ('SURF', 'PREC.EAU.GEC', (/   1,  62,   1,     0,   0, 8,  0,  16 /))
CALL T ('SURF', 'PREC.EAU.CON', (/   1,  63,   1,     0,   0, 8,  0,  16 /))
CALL T ('SURF', 'RESERV.NEIGE', (/   1,  65,   1,     0,   0, 0,  0,  16 /))
CALL T ('SURF', 'NEBUL.TOTALE', (/   1,  71,   1,     0,   0, 0, -2,   8 /), PMULTI=Z100)
CALL T ('SURF', 'NEBUL.CONVEC', (/   1,  72,   1,     0,   0, 0, -2,   8 /), PMULTI=Z100)
CALL T ('SURF', 'NEBUL.BASSE ', (/   1,  73,   1,     0,   0, 0, -2,   8 /), PMULTI=Z100)
CALL T ('SURF', 'NEBUL.MOYENN', (/   1,  74,   1,     0,   0, 0, -2,   8 /), PMULTI=Z100)
CALL T ('SURF', 'NEBUL.HAUTE ', (/   1,  75,   1,     0,   0, 0, -2,   8 /), PMULTI=Z100)
CALL T ('SURF', 'PREC.NEI.CON', (/   1,  78,   1,     0,   0, 8,  0,  16 /))
CALL T ('SURF', 'PREC.NEI.GEC', (/   1,  79,   1,     0,   0, 8,  0,  16 /))
CALL T ('SURF', 'IND.TERREMER', (/   1,  81,   1,     0,   0, 0,  0,   1 /))
CALL T ('SURF', 'Z0.FOIS.G   ', (/   1,  83,   1,     0,   0, 0,  0,  24 /), PMULTI=ZONOVG)
CALL T ('SURF', 'ALBEDO      ', (/   1,  84,   1,     0,   0, 0, -2,  12 /), PMULTI=Z100)
CALL T ('SURF', 'PROP.VEGETAT', (/   1,  87,   1,     0,   0, 0, -2,  12 /), PMULTI=Z100)
CALL T ('SURF', 'FLU.RAY.SOLA', (/ 128, 111,   1,     0,   0, 8,  0,  16 /))
CALL T ('SURF', 'FLU.RAY.THER', (/ 128, 112,   1,     0,   0, 8,  0,  16 /))
CALL T ('SURF', 'FLU.LAT.MEVA', (/ 128, 121,   1,     0,   0, 8,  0,  16 /))
CALL T ('SURF', 'FLU.LAT.MSUB', (/ 128, 121,   1,     0,   0, 8,  0,  16 /))
CALL T ('SURF', 'FLU.LAT.MTOT', (/ 128, 121,   1,     0,   0, 8,  0,  16 /))
CALL T ('SURF', 'FLU.CHA.SENS', (/ 128, 122,   1,     0,   0, 8,  0,  16 /))
CALL T ('SURF', 'ETA.GEOPOTEN', (/   1, 128,   1,     0,   0, 0,  0,  16 /))
CALL T ('SURF', 'ETP.GEOPOTEN', (/   1, 129,   1,     0,   0, 0,  0,  16 /))
CALL T ('SURF', 'TENS.DMOG.ZO', (/   1, 130,   1,     0,   0, 8,  0,  16 /))
CALL T ('SURF', 'TENS.TURB.ZO', (/   1, 130,   1,     0,   0, 8,  0,  16 /))
CALL T ('SURF', 'TENS.TOTA.ZO', (/   1, 130,   1,     0,   0, 8,  0,  16 /))
CALL T ('SURF', 'TENS.DMOG.ME', (/   1, 131,   1,     0,   0, 8,  0,  16 /))
CALL T ('SURF', 'TENS.TURB.ME', (/   1, 131,   1,     0,   0, 8,  0,  16 /))
CALL T ('SURF', 'TENS.TOTA.ME', (/   1, 131,   1,     0,   0, 8,  0,  16 /))
CALL T ('SURF', 'RESERV.GLACE', (/   1, 152, 112,     0,   1, 0,  0,  16 /))
CALL T ('SURF', 'RESERV.EAU  ', (/   1, 153, 112,     0,   1, 0,  0,  16 /))
CALL T ('SURF', 'RAYT.LUNE.DE', (/   1, 158,   1,     0,   0, 8,  0,  16 /))
CALL T ('SURF', 'CAPE.MOD.XFU', (/ 159, 154,   1,     0,   0, 0,  0,  16 /))
CALL T ('SURF', 'TOT.WAT.VAPO', (/   1, 167,   1,     0,   0, 0,  0,  12 /))
! Debut des champs pour Mocage :
!DP: champ TCO3 remplace par O3_TOT le 18 oct 2004 par coherence
! avec la BDAP qui venait de proceder au changement.
CALL T ('SURF', 'O3_TOT      ', (/   1,  10,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'CH4_TOT     ', (/ 159,  21,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'NO_TOT      ', (/ 159,  22,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'NO2_TOT     ', (/ 159,  23,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'CO_TOT      ', (/ 159,  24,   1,     0,   0, 0,  0,  12 /))
! Fin des champs pour Mocage
CALL T ('SURF', 'ALBEDO NEIGE', (/ 149,   5,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'ALBEDO HISTO', (/ 149,  48,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'ALBEDO.SOLNU', (/ 149,  49,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'ALBEDO.VEG  ', (/ 149,  50,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'EMISSIVITE  ', (/ 149,  60,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'ET.GEOPOTENT', (/ 149,  93,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'VAR.GEOP.ANI', (/ 128, 161,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'VAR.GEOP.DIR', (/ 128, 162,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'IND.VEG.DOMI', (/ 149,  61,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'RESI.STO.MIN', (/ 149,  62,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'PROP.ARGILE ', (/ 149,  63,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'PROP.SABLE  ', (/ 149,  64,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'EPAIS.SOL   ', (/ 149,  51,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'IND.FOLIAIRE', (/ 149,  65,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'RES.EVAPOTRA', (/ 149,  66,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'GZ0.THERM   ', (/ 149,  67,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'RESERV.INTER', (/ 149,  68,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'FL.Q TURBUL ', (/ 149,  69,   1,     0,   0, 4,  0,  12 /))
CALL T ('SURF', 'FL.CT TURBUL', (/ 149,  70,   1,     0,   0, 4,  0,  12 /))
CALL T ('SURF', 'FONTE NEIGE ', (/   1,  99,   1,     0,   0, 4,  0,  12 /))
CALL T ('SURF', 'CHAL. DS SOL', (/ 149,   8,   1,     0,   0, 4,  0,  12 /))
CALL T ('SURF', 'EAU DANS SOL', (/   1, 150,   1,     0,   0, 4,  0,  12 /))
CALL T ('SURF', 'RUISSELLEMEN', (/ 149,   2,   1,     0,   0, 4,  0,  12 /))
CALL T ('SURF', 'EVAPOTRANSPI', (/   1, 168,   1,     0,   0, 4,  0,  12 /))
CALL T ('SURF', 'TRANSPIRATIO', (/ 149,  71,   1,     0,   0, 4,  0,  12 /))
CALL T ('SURF', 'RUISS. INTER', (/ 149,  72,   1,     0,   0, 4,  0,  12 /))
CALL T ('SURF', 'FLUX DE GEL ', (/ 149,  73,   1,     0,   0, 4,  0,  12 /))
CALL T ('SURF', 'RAYT DIR SUR', (/ 128, 137,   1,     0,   0, 8,  0,  16 /))
CALL T ('SURF', 'RAYT DIFF DE', (/ 149,  75,   1,     0,   0, 4,  0,  12 /))
CALL T ('SURF', 'RAYT THER DE', (/ 149, 104,   1,     0,   0, 8,  0,  16 /))
CALL T ('SURF', 'PROP.RMAX.EA', (/ 149,  77,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'DENSIT.NEIGE', (/ 149,   3,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'CFU.Q.TURBUL', (/ 149,  78,   1,     0,   0, 4,  0,  12 /))
CALL T ('SURF', 'CFU.CT.TURBU', (/ 149,  79,   1,     0,   0, 4,  0,  12 /))
CALL T ('SURF', 'TIME.PREC.TO', (/ 149,  80,   1,     0,   0, 4,  0,  12 /))
CALL T ('SURF', 'XFONTE NEIGE', (/ 149,  81,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'XCHAL. DS SO', (/ 149,  82,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'XEAU DANS SO', (/ 149,  83,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'XFLU.MEVAP.E', (/ 149,  84,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'XRUISSELLEME', (/ 149,  85,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'XEVAPOTRANSP', (/ 149,  86,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'XTRANSPIRATI', (/ 149,  87,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'XRUISS. INTE', (/ 149,  88,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'ISOTPW0.MALT', (/ 128,   8,  20, 27315,   0, 0,  0,  16 /))
CALL T ('SURF', 'ISOTPW1.MALT', (/ 128,   8,  20, 27415,   0, 0,  0,  16 /))
CALL T ('SURF', 'CAPE.POS.F00', (/   1, 160,   1,     0,   0, 0,  0,  16 /))
CALL T ('SURF', 'CAPE.POS.F04', (/ 159, 154,   1,     0,   0, 0,  0,  16 /))
CALL T ('SURF', 'HUMI.RELATIV', (/   1,  52,   1,     0,   0, 0, -2,  12 /), PMULTI=Z100)
CALL T ('SURF', 'HUMI.SPECIFI', (/   1,  51,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'ISOT0.MALTIT', (/ 128,   8, 115, 27315,   0, 0,  0,  16 /))
CALL T ('SURF', 'ISOTM10.MALT', (/ 128,   8, 115, 26315,   0, 0,  0,  16 /))
CALL T ('SURF', 'EQUILIBRLEV ', (/ 159, 155,   1,     0,   0, 0,  0,  16 /))
CALL T ('SURF', 'RAYT SOL CL ', (/ 128, 168,   1,     0,   0, 8,  0,  16 /))
CALL T ('SURF', 'RAYT THER CL', (/ 128, 169,   1,     0,   0, 8,  0,  16 /))
CALL T ('SURF', 'WIND_VELOC  ', (/   1,  32,   1,     0,   0, 0,  0,  24 /))
CALL T ('SURF', 'TCO3        ', (/   1,  10,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'TCNOX       ', (/ 159,  22,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'TCCO        ', (/ 159,  24,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'TCCH4       ', (/ 159,  21,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'TCPAN       ', (/ 159,  21,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'TC_DUST     ', (/ 159,  94,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'OD_DUST     ', (/ 159,  95,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'DSEC_DUST   ', (/ 159,  96,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'DHUM_DUST   ', (/ 159,  97,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'DSEDI_DUST  ', (/ 159,  98,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'EMISS_DUST  ', (/ 159,  99,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'ANAMOCON    ', (/   1, 166,   1,     0,   0, 0,  0,  16 /))
CALL T ('SURF', 'RAYT SOLA DE', (/ 149, 105,   1,     0,   0, 8,  0,  16 /))
CALL T ('SURF', 'ACCPLUIE    ', (/   2, 150,   1,     0,   0, 8,  0,  16 /))
CALL T ('SURF', 'INSPLUIE    ', (/   2, 150,   1,     0,   0, 0,  0,  16 /))
CALL T ('SURF', 'ACCNEIGE    ', (/   1,  99,   1,     0,   0, 8,  0,  16 /))
CALL T ('SURF', 'INSNEIGE    ', (/   1,  99,   1,     0,   0, 0,  0,  16 /))
CALL T ('SURF', 'ACCGRAUPEL  ', (/ 159,  29,   1,     0,   0, 8,  0,  16 /))
CALL T ('SURF', 'INSGRAUPEL  ', (/ 159,  29,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'ACCGRELE    ', (/ 159,  30,   1,     0,   0, 8,  0,  16 /))
CALL T ('SURF', 'INSGRELE    ', (/ 159,  30,   1,     0,   0, 0,  0,  16 /))
CALL T ('SURF', 'DIAGHAIL    ', (/ 159, 248,   1,     0,   0, 0,  0,  16 /))
CALL T ('SURF', 'VERT.VELOC  ', (/   1,  40,   1,     0,   0, 0,  0,  16 /))
CALL T ('SURF', 'TC_BLACKC   ', (/ 159, 196,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'EMISS_BLACKC', (/ 159, 200,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'DHUM_BLACKC ', (/ 159, 198,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'DSEC_BLACKC ', (/ 159, 197,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'DSEDI_BLACKC', (/ 159, 199,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'TC_DYNSAL   ', (/ 159, 190,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'EMISS_DYNSAL', (/ 159, 194,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'DHUM_DYNSAL ', (/ 159, 192,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'DSEC_DYNSAL ', (/ 159, 191,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'DSEDI_DYNSAL', (/ 159, 193,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'TC_PMANTH   ', (/ 159, 202,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'EMISS_BLACKC', (/ 159, 200,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'DHUM_BLACKC ', (/ 159, 198,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'DSEC_BLACKC ', (/ 159, 197,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'DSEDI_BLACKC', (/ 159, 199,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'TC_DYNSAL   ', (/ 159, 190,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'TC_BLACKC   ', (/ 159, 196,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'EMISS_PMANTH', (/ 159, 206,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'DHUM_PMANTH ', (/ 159, 204,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'DSEC_PMANTH ', (/ 159, 203,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'DSEDI_PMANTH', (/ 159, 205,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'TC_PM25     ', (/ 159, 208,   1,     0,   0, 0,  0,  12 /))
CALL T ('SURF', 'TC_PM10     ', (/ 159, 210,   1,     0,   0, 0,  0,  12 /))
!
!  Geopotentiel de surface en spectral (relief)
!
CALL T ('SPECSURF', 'GEOPOTEN', (/   1,   8,   1,     0,   0, 0,  0,  12 /), PMULTI=ZONOVG)
!
!     1.3 -  Champs au sommet
!
CALL T ('SOMM', 'FLU.RAY.SOLA', (/ 128, 113,   8,     0,   0, 8,  0,  16 /))
CALL T ('SOMM', 'FLU.RAY.THER', (/ 128, 114,   8,     0,   0, 8,  0,  16 /))
CALL T ('SOMM', 'TB_OZ_CLEAR ', (/   1, 170,   8,     0,   0, 0,  0,  12 /))
CALL T ('SOMM', 'TB_OZ_CLOUDY', (/   1, 171,   8,     0,   0, 0,  0,  12 /))
CALL T ('SOMM', 'TB_IR_CLEAR ', (/   1, 172,   8,     0,   0, 0,  0,  12 /))
CALL T ('SOMM', 'TB_IR_CLOUDY', (/   1, 173,   8,     0,   0, 0,  0,  12 /))
CALL T ('SOMM', 'TB_WV_CLEAR ', (/   1, 174,   8,     0,   0, 0,  0,  12 /))
CALL T ('SOMM', 'TB_WV_CLOUDY', (/   1, 175,   8,     0,   0, 0,  0,  12 /))
!
!     1.4 -  Champs de la Couche Limite de Surface
!
CALL T ('CLS', 'TEMPERATURE  ', (/   1,  11, 105,     2,   0, 0,  0,  12 /))
CALL T ('CLS', 'MAXI.TEMPERAT', (/   1,  15, 105,     2,   0, 2,  0,  12 /))
CALL T ('CLS', 'MINI.TEMPERAT', (/   1,  16, 105,     2,   0, 2,  0,  12 /))
CALL T ('CLS', 'VENT.ZONAL   ', (/   1,  33, 105,    10,   0, 0,  0,  12 /))
CALL T ('CLS', 'VENT.MERIDIEN', (/   1,  34, 105,    10,   0, 0,  0,  12 /))
CALL T ('CLS', 'HUMI.SPECIFIQ', (/   1,  51, 105,     2,   0, 0,  0,  12 /))
CALL T ('CLS', 'HUMI.RELATIVE', (/   1,  52, 105,     2,   0, 0, -2,  12 /), PMULTI=Z100)
CALL T ('CLS', 'MAXI.HUMI.REL', (/ 149,  90, 105,     2,   0, 2,  0,  12 /))
CALL T ('CLS', 'MINI.HUMI.REL', (/ 149,  91, 105,     2,   0, 2,  0,  12 /))
CALL T ('CLS', 'U.RAF.MOD.XFU', (/   1, 163, 105,    10,   0, 0,  0,  12 /))
CALL T ('CLS', 'V.RAF.MOD.XFU', (/   1, 164, 105,    10,   0, 0,  0,  12 /))
!
!     1.5 -  Champs au niveau tropopause
!
CALL T ('ICAO', 'TROP.PRESSUR', (/   1,   1,   7,     0,   0, 0,  0,  16 /))
CALL T ('ICAO', 'TROP.TEMPERA', (/   1,  11,   7,     0,   0, 0,  0,  12 /))
!
!     1.6 -  Champs au niveau jet
!
CALL T ('JET', 'PRESSURE     ', (/   1,   1,   6,     0,   0, 0,  0,  16 /))
CALL T ('JET', 'VENT_ZONAL   ', (/   1,  33,   6,     0,   0, 0,  0,  12 /))
CALL T ('JET', 'VENT_MERIDIEN', (/   1,  34,   6,     0,   0, 0,  0,  12 /))
!
!     1.7 -  Champs pour la Couche Limite Planetaire
!
CALL T ('CLP', 'MHAUT.MOD.XFU', (/   1, 165,   1,     0,   0, 0,  0,  16 /))
CALL T ('CLP', 'MOCON.MOD.XFU', (/   1, 166,   1,     0,   0, 0,  0,  16 /))
CALL T ('CLP', 'GEOPO.MOD.XFU', (/   1, 165,   1,     0,   0, 0,  0,  16 /))
!
!     1.8 -  Champs au niveau mer
!
CALL T ('MSL', 'PRESSURE',      (/   1,   2, 102,     0,   0, 0,  0,  12 /))
!
!     1.9 - Relief
!
CALL T ('INT', 'SURFGEOPOTENT', (/   1,   8,   1,     0,   0, 0,  0,  16 /), PMULTI=ZONOVG)
!
!     1.10- ATMO
!
CALL T ('ATMO', 'NEBUL.CONVEC', (/ 128,  72,   1,     0,   0, 4,  0,  12 /))
CALL T ('ATMO', 'NEBUL.BASSE ', (/ 128,  73,   1,     0,   0, 4,  0,  12 /))
CALL T ('ATMO', 'NEBUL.MOYENN', (/ 128,  74,   1,     0,   0, 4,  0,  12 /))
CALL T ('ATMO', 'NEBUL.HAUTE ', (/ 128,  75,   1,     0,   0, 4,  0,  12 /))
!
!     1.11- TOP
!
CALL T ('TOP', 'RAYT DIR SOM',  (/ 149, 174,   8,     0,   0, 4,  0,  12 /))
!**
!     2.  -  TRAITEMENT DES CHAMPS SUR NIVEAUX
!-----------------------------------------------------------------------
!
!     2.1 -  Champs sur un niveau isobare
!
CALL T ('P', 'POT_VORTICIT',    (/   1,   4, 100,     0,   0, 0,  0,  16 /))
CALL T ('P', 'GEOPOTENTIEL',    (/   1,   6, 100,     0,   0, 0,  0,  16 /))
CALL T ('P', 'TEMPERATURE ',    (/   1,  11, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'TEMPE_POTENT',    (/   1,  13, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'THETA_PRIM_W',    (/   1,  14, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'VENT_ZONAL  ',    (/   1,  33, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'VENT_MERIDIE',    (/   1,  34, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'VITESSE_VERT',    (/   1,  39, 100,     0,   0, 0,  0,  16 /))
CALL T ('P', 'ABS_VORTICIT',    (/   1,  41, 100,     0,   0, 0,  0,  16 /))
CALL T ('P', 'DIVERGENCE  ',    (/   1,  44, 100,     0,   0, 0,  0,  16 /))
CALL T ('P', 'HUMI.SPECIFI',    (/   1,  51, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'HUMI_RELATIV',    (/   1,  52, 100,     0,   0, 0, -2,  12 /), PMULTI=Z100)
CALL T ('P', 'PRESSURE    ',    (/   1,   1, 105,     0,   0, 0,  0,  12 /))
CALL T ('P', 'WIND_VELOCIT',    (/   1,  32, 100,     0,   0, 0,  0,  24 /))
CALL T ('P', 'VORTICITY   ',    (/   1,  43, 100,     0,   0, 0,  0,  16 /))
CALL T ('P', 'FONC.COURANT',    (/   1,  35, 100,     0,   0, 0,  0,  24 /))
CALL T ('P', 'POT.VITESSE ',    (/   1,  36, 100,     0,   0, 0,  0,  24 /))
! Debut des champs pour Mocage :
CALL T ('P', 'O3        ',      (/ 159,   1, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'NO2       ',      (/ 159,   2, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'NO        ',      (/ 159,   3, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'PAN       ',      (/ 159,   4, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'HNO3      ',      (/ 159,   5, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'SO2       ',      (/ 159,   6, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'CO        ',      (/ 159,   7, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'CH4       ',      (/ 159,   8, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'C2H6      ',      (/ 159,   9, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'NC4H10    ',      (/ 159,  10, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'C2H4      ',      (/ 159,  11, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'C3H6      ',      (/ 159,  12, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'OXYL      ',      (/ 159,  13, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'C5H8      ',      (/ 159,  14, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'APINEN    ',      (/ 159,  15, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'HCHO      ',      (/ 159,  16, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'CH3CHO    ',      (/ 159,  17, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'GLYOX     ',      (/ 159,  18, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'MGLYOX    ',      (/ 159,  19, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'CH3COE    ',      (/ 159,  20, 100,     0,   0, 0,  0,  12 /))
! Fin des champs pour Mocage
CALL T ('P', 'RAD_LIQUID_W',    (/ 159,  32, 100,     0,   0, 0,  0,  16 /))
CALL T ('P', 'RAD_SOLID_WA',    (/ 128, 247, 100,     0,   0, 0,  0,  16 /))
CALL T ('P', 'CLOUD_FRACTI',    (/ 159,  36, 100,     0,   0, 0,  0,  16 /))
CALL T ('P', 'TKE         ',    (/ 159,  37, 100,     0,   0, 0,  0,  16 /))
CALL T ('P', 'SURFMOCON   ',    (/   1, 166,   1,     0,   0, 0,  0,  16 /), LDNIVA=.TRUE.)
CALL T ('P', 'MAX_O3      ',    (/ 159,  23, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'MAX_NO      ',    (/ 159,  25, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'MAX_NO2     ',    (/ 159,  26, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'MAX_IUVCC   ',    (/ 159,  27, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'MAX_IUVCN   ',    (/ 159,  28, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'VERT.VELOCIT',    (/   1,  40, 100,     0,   0, 0,  0,  16 /))
CALL T ('P', 'SIM_REFLECTI',    (/ 159,  31, 100,     0,   0, 0,  0,  16 /))
CALL T ('P', 'CLOUD_WATER ',    (/ 159,  32, 100,     0,   0, 0,  0,  16 /))
CALL T ('P', 'RAIN        ',    (/ 159,  33, 100,     0,   0, 0,  0,  16 /))
CALL T ('P', 'ICE_CRYSTAL ',    (/ 128, 247, 100,     0,   0, 0,  0,  16 /))
CALL T ('P', 'SNOW        ',    (/ 159,  34, 100,     0,   0, 0,  0,  16 /))
CALL T ('P', 'GRAUPEL     ',    (/ 159,  35, 100,     0,   0, 0,  0,  16 /))
CALL T ('P', 'THETA_VIRTUA',    (/ 159,  38, 100,     0,   0, 0,  0,  16 /))
CALL T ('P', 'DUST        ',    (/ 159,  93, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'HAIL        ',    (/ 159, 100, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'BLACKC      ',    (/ 159, 195, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'DYNSAL      ',    (/ 159, 189, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'PMANTH      ',    (/ 159, 201, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'PM25        ',    (/ 159, 207, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'PM10        ',    (/ 159, 209, 100,     0,   0, 0,  0,  12 /))
CALL T ('P', 'EDR         ',    (/ 128, 135, 100,     0,   0, 0,  0,  12 /))
!
!     2.2 -  Champs sur un niveau hauteur
!
CALL T ('H', 'POT_VORTICIT',    (/   1,   4, 105,     0,   0, 0,  0,  16 /))
CALL T ('H', 'GEOPOTENTIEL',    (/   1,   6, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'TEMPERATURE ',    (/   1,  11, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'TEMPE_POTENT',    (/   1,  13, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'THETA_PRIM_W',    (/   1,  14, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'VENT_ZONAL  ',    (/   1,  33, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'VENT_MERIDIE',    (/   1,  34, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'VITESSE_VERT',    (/   1,  39, 105,     0,   0, 0,  0,  16 /))
CALL T ('H', 'ABS_VORTICIT',    (/   1,  41, 105,     0,   0, 0,  0,  16 /))
CALL T ('H', 'DIVERGENCE  ',    (/   1,  44, 105,     0,   0, 0,  0,  16 /))
CALL T ('H', 'HUMI.SPECIFI',    (/   1,  51, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'HUMI_RELATIV',    (/   1,  52, 105,     0,   0, 0, -2,  12 /), PMULTI=Z100)
CALL T ('H', 'PRESSURE    ',    (/   1,   1, 105,     0,   0, 0,  0,  16 /))
CALL T ('H', 'WIND_VELOCIT',    (/   1,  32, 105,     0,   0, 0,  0,  24 /))
CALL T ('H', 'VORTICITY   ',    (/   1,  43, 105,     0,   0, 0,  0,  16 /))
CALL T ('H', 'FONC.COURANT',    (/   1,  35, 105,     0,   0, 0,  0,  24 /))
CALL T ('H', 'POT.VITESSE ',    (/   1,  36, 105,     0,   0, 0,  0,  24 /))
! Debut des champs pour Mocage :
CALL T ('H', 'O3        ',      (/ 159,   1, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'NO2       ',      (/ 159,   2, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'NO        ',      (/ 159,   3, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'PAN       ',      (/ 159,   4, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'HNO3      ',      (/ 159,   5, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'SO2       ',      (/ 159,   6, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'CO        ',      (/ 159,   7, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'CH4       ',      (/ 159,   8, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'C2H6      ',      (/ 159,   9, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'NC4H10    ',      (/ 159,  10, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'C2H4      ',      (/ 159,  11, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'C3H6      ',      (/ 159,  12, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'OXYL      ',      (/ 159,  13, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'C5H8      ',      (/ 159,  14, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'APINEN    ',      (/ 159,  15, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'HCHO      ',      (/ 159,  16, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'CH3CHO    ',      (/ 159,  17, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'GLYOX     ',      (/ 159,  18, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'MGLYOX    ',      (/ 159,  19, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'CH3COE    ',      (/ 159,  20, 105,     0,   0, 0,  0,  12 /))
! Fin des champs pour Mocage
CALL T ('H', 'RAD_LIQUID_W',      (/ 159,  32, 105,     0,   0, 0,  0,  16 /))
CALL T ('H', 'RAD_SOLID_WA',      (/ 128, 247, 105,     0,   0, 0,  0,  16 /))
CALL T ('H', 'CLOUD_FRACTI',      (/ 159,  36, 105,     0,   0, 0,  0,  16 /))
CALL T ('H', 'TKE         ',      (/ 159,  37, 105,     0,   0, 0,  0,  16 /))
CALL T ('H', 'SURFMOCON   ',      (/   1, 166, 105,     0,   0, 0,  0,  12 /), LDNIVA=.TRUE.)
CALL T ('H', 'MAX_O3      ',      (/ 159,  23, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'MAX_NO      ',      (/ 159,  25, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'MAX_NO2     ',      (/ 159,  26, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'MAX_IUVCC   ',      (/ 159,  27, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'MAX_IUVCN   ',      (/ 159,  28, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'VERT.VELOCIT',      (/   1,  40, 105,     0,   0, 0,  0,  16 /))
CALL T ('H', 'SIM_REFLECTI',      (/ 159,  31, 105,     0,   0, 0,  0,  16 /))
CALL T ('H', 'CLOUD_WATER ',      (/ 159,  32, 105,     0,   0, 0,  0,  16 /))
CALL T ('H', 'RAIN        ',      (/ 159,  33, 105,     0,   0, 0,  0,  16 /))
CALL T ('H', 'ICE_CRYSTAL ',      (/ 128, 247, 105,     0,   0, 0,  0,  16 /))
CALL T ('H', 'SNOW        ',      (/ 159,  34, 105,     0,   0, 0,  0,  16 /))
CALL T ('H', 'GRAUPEL     ',      (/ 159,  35, 105,     0,   0, 0,  0,  16 /))
CALL T ('H', 'THETA_VIRTUA',      (/ 159,  38, 105,     0,   0, 0,  0,  16 /))
CALL T ('H', 'DUST        ',      (/ 159,  93, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'HAIL        ',      (/ 159, 100, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'BLACKC      ',      (/ 159, 195, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'DYNSAL      ',      (/ 159, 189, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'PMANTH      ',      (/ 159, 201, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'PM25        ',      (/ 159, 207, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'PM10        ',      (/ 159, 209, 105,     0,   0, 0,  0,  12 /))
CALL T ('H', 'EDR         ',      (/ 128, 135, 105,     0,   0, 0,  0,  12 /))
!
!     2.3 -  Champs sur un niveau iso-tourbillon-potentiel
!
CALL T ('V', 'POT_VORTICIT',    (/   1,   4, 117,     0,   0, 0,  0,  16 /))
CALL T ('V', 'GEOPOTENTIEL',    (/   1,   6, 117,     0,   0, 0,  0,  16 /))
CALL T ('V', 'TEMPERATURE ',    (/   1,  11, 117,     0,   0, 0,  0,  12 /))
CALL T ('V', 'TEMPE_POTENT',    (/   1,  13, 117,     0,   0, 0,  0,  12 /))
CALL T ('V', 'THETA_PRIM_W',    (/   1,  14, 117,  1500,   0, 0,  0,  12 /))
CALL T ('V', 'VENT_ZONAL  ',    (/   1,  33, 117,     0,   0, 0,  0,  12 /))
CALL T ('V', 'VENT_MERIDIE',    (/   1,  34, 117,     0,   0, 0,  0,  12 /))
CALL T ('V', 'VITESSE_VERT',    (/   1,  39, 117,  1500,   0, 0,  0,  16 /))
CALL T ('V', 'ABS_VORTICIT',    (/   1,  41, 117,     0,   0, 0,  0,  16 /))
CALL T ('V', 'DIVERGENCE  ',    (/   1,  44, 117,     0,   0, 0,  0,  16 /))
CALL T ('V', 'HUMI.SPECIFI',    (/   1,  51, 117,     0,   0, 0,  0,  12 /))
CALL T ('V', 'HUMI_RELATIV',    (/   1,  52, 117,  1500,   0, 0, -2,  12 /), PMULTI=Z100)
CALL T ('V', 'PRESSURE    ',    (/   1,   1, 117,  1500,   0, 0,  0,  16 /))
CALL T ('V', 'WIND_VELOCIT',    (/   1,  32, 117,  1500,   0, 0,  0,  24 /))
CALL T ('V', 'VORTICITY   ',    (/   1,  43, 117,     0,   0, 0,  0,  16 /))
CALL T ('V', 'FONC.COURANT',    (/   1,  35, 117,     0,   0, 0,  0,  24 /))
CALL T ('V', 'POT.VITESSE ',    (/   1,  36, 117,     0,   0, 0,  0,  24 /))
CALL T ('V', 'TKE         ',    (/ 159,  37, 117,  1500,   0, 0,  0,  16 /))
CALL T ('V', 'CLOUD_FRACTI',    (/ 159,  36, 117,  1500,   0, 0,  0,  16 /))
CALL T ('V', 'CLOUD_WATER ',    (/ 159,  32, 117,  1500,   0, 0,  0,  16 /))
CALL T ('V', 'RAD_LIQUID_W',    (/ 159,  32, 117,  1500,   0, 0,  0,  16 /))
CALL T ('V', 'ICE_CRYSTAL ',    (/ 128, 247, 117,  1500,   0, 0,  0,  16 /))
CALL T ('V', 'RAD_SOLID_WA',    (/ 128, 247, 117,  1500,   0, 0,  0,  16 /))
!
!     2.4 -  Champs sur un niveau isentrope
!
CALL T ('T', 'POT_VORTICIT',    (/   1,   4, 107,     0,   0, 0,  0,  16 /))
CALL T ('T', 'GEOPOTENTIEL',    (/   1,   6, 107,     0,   0, 0,  0,  16 /))
CALL T ('T', 'TEMPERATURE ',    (/   1,  11, 107,     0,   0, 0,  0,  12 /))
CALL T ('T', 'TEMPE_POTENT',    (/   1,  13, 107,     0,   0, 0,  0,  12 /))
CALL T ('T', 'THETA_PRIM_W',    (/   1,  14, 107,  1500,   0, 0,  0,  12 /))
CALL T ('T', 'VENT_ZONAL  ',    (/   1,  33, 107,     0,   0, 0,  0,  12 /))
CALL T ('T', 'VENT_MERIDIE',    (/   1,  34, 107,     0,   0, 0,  0,  12 /))
CALL T ('T', 'VITESSE_VERT',    (/   1,  39, 107,     0,   0, 0,  0,  16 /))
CALL T ('T', 'ABS_VORTICIT',    (/   1,  41, 107,     0,   0, 0,  0,  12 /))
CALL T ('T', 'DIVERGENCE  ',    (/   1,  44, 107,     0,   0, 0,  0,  16 /))
CALL T ('T', 'HUMI.SPECIFI',    (/   1,  51, 107,     0,   0, 0,  0,  12 /))
CALL T ('T', 'HUMI_RELATIV',    (/   1,  52, 107,     0,   0, 0, -2,  12 /), PMULTI=Z100)
CALL T ('T', 'PRESSURE    ',    (/   1,   1, 107,  1500,   0, 0,  0,  16 /))
CALL T ('T', 'WIND_VELOCIT',    (/   1,  32, 107,  1500,   0, 0,  0,  12 /))
CALL T ('T', 'VORTICITY   ',    (/   1,  43, 107,     0,   0, 0,  0,  16 /))
CALL T ('T', 'FONC.COURANT',    (/   1,  35, 107,     0,   0, 0,  0,  24 /))
CALL T ('T', 'POT.VITESSE ',    (/   1,  36, 107,     0,   0, 0,  0,  24 /))
CALL T ('T', 'TKE         ',    (/ 159,  37, 107,  1500,   0, 0,  0,  12 /))
CALL T ('T', 'CLOUD_FRACTI',    (/ 159,  36, 107,  1500,   0, 0,  0,  12 /))
CALL T ('T', 'CLOUD_WATER ',    (/ 159,  32, 107,  1500,   0, 0,  0,  12 /))
CALL T ('T', 'RAD_LIQUID_W',    (/ 159,  32, 107,  1500,   0, 0,  0,  12 /))
CALL T ('T', 'ICE_CRYSTAL ',    (/ 128, 247, 107,  1500,   0, 0,  0,  12 /))
CALL T ('T', 'RAD_SOLID_WA',    (/ 128, 247, 107,  1500,   0, 0,  0,  12 /))
!
!     2.5 -  Champs sur un niveau modele
!
!
CALL T ('S', 'TEMPERATURE ',    (/   1,  11, 109,     0,   0, 0,  0,  12 /))
CALL T ('S', 'WIND.U.PHYS ',    (/   1,  33, 109,     0,   0, 0,  0,  12 /))
CALL T ('S', 'VENT_ZONAL  ',    (/   1,  33, 109,     0,   0, 0,  0,  12 /))
CALL T ('S', 'WIND.V.PHYS ',    (/   1,  34, 109,     0,   0, 0,  0,  12 /))
CALL T ('S', 'VENT_MERIDIE',    (/   1,  34, 109,     0,   0, 0,  0,  12 /))
CALL T ('S', 'FONC.COURANT',    (/   1,  35, 109,     0,   0, 0,  0,  24 /))
CALL T ('S', 'POT.VITESSE ',    (/   1,  36, 109,     0,   0, 0,  0,  24 /))
CALL T ('S', 'VORTICITY   ',    (/   1,  43, 109,     0,   0, 0,  0,  16 /))
CALL T ('S', 'DIVERGENCE  ',    (/   1,  44, 109,     0,   0, 0,  0,  16 /))
CALL T ('S', 'HUMI.SPECIFI',    (/   1,  51, 109,     0,   0, 0,  0,  12 /))
CALL T ('S', 'VERTIC.DIVER',    (/ 149,  91, 109,     0,   0, 0,  0,  12 /))
CALL T ('S', 'PRESS.DEPART',    (/ 149,  92, 109,     0,   0, 0,  0,  12 /))
CALL T ('S', 'PRESSURE    ',    (/   1,   1, 109,     1,   0, 0,  0,  16 /))
CALL T ('S', 'GEOPOTENTIEL',    (/   1,   8, 109,     1,   0, 0,  0,  24 /), PMULTI=ZONOVG)
CALL T ('S', 'HUMI_RELATIV',    (/   1,  52, 109,     1,   0, 0, -2,  12 /), PMULTI=Z100)
CALL T ('S', 'VITESSE_VERT',    (/   1,  39, 109,     1,   0, 0,  0,  16 /))
CALL T ('S', 'O3          ',    (/ 159,   1, 109,     1,   0, 0,  0,  12 /))
CALL T ('S', 'NO2         ',    (/ 159,   2, 109,     1,   0, 0,  0,  12 /))
CALL T ('S', 'NO          ',    (/ 159,   3, 109,     1,   0, 0,  0,  12 /))
CALL T ('S', 'PAN         ',    (/ 159,   4, 109,     1,   0, 0,  0,  12 /))
CALL T ('S', 'HNO3        ',    (/ 159,   5, 109,     1,   0, 0,  0,  12 /))
CALL T ('S', 'SO2         ',    (/ 159,   6, 109,     1,   0, 0,  0,  12 /))
CALL T ('S', 'CO          ',    (/ 159,   7, 109,     1,   0, 0,  0,  12 /))
CALL T ('S', 'CH4         ',    (/ 159,   8, 109,     1,   0, 0,  0,  12 /))
CALL T ('S', 'C2H6        ',    (/ 159,   9, 109,     1,   0, 0,  0,  12 /))
CALL T ('S', 'NC4H10      ',    (/ 159,  10, 109,     1,   0, 0,  0,  12 /))
CALL T ('S', 'C2H4        ',    (/ 159,  11, 109,     1,   0, 0,  0,  12 /))
CALL T ('S', 'C3H6        ',    (/ 159,  12, 109,     1,   0, 0,  0,  12 /))
CALL T ('S', 'OXYL        ',    (/ 159,  13, 109,     1,   0, 0,  0,  12 /))
CALL T ('S', 'C5H8        ',    (/ 159,  14, 109,     1,   0, 0,  0,  12 /))
CALL T ('S', 'APINEN      ',    (/ 159,  15, 109,     1,   0, 0,  0,  12 /))
CALL T ('S', 'HCHO        ',    (/ 159,  16, 109,     1,   0, 0,  0,  12 /))
CALL T ('S', 'CH3CHO      ',    (/ 159,  17, 109,     1,   0, 0,  0,  12 /))
CALL T ('S', 'GLYOX       ',    (/ 159,  18, 109,     1,   0, 0,  0,  12 /))
CALL T ('S', 'MGLYOX      ',    (/ 159,  19, 109,     1,   0, 0,  0,  12 /))
CALL T ('S', 'CH3COE      ',    (/ 159,  20, 109,     1,   0, 0,  0,  12 /))
CALL T ('S', 'PLUIE STRATI',    (/   1,  62,   1,     0,   0, 0,  0,  16 /), LDNIVA=.TRUE.)
CALL T ('S', 'PLUIE CONVEC',    (/   1,  63,   1,     0,   0, 0,  0,  16 /), LDNIVA=.TRUE.)
CALL T ('S', 'VERT.VELOCIT',    (/   1,  40, 109,     1,   0, 0,  0,  16 /))
CALL T ('S', 'SIM_REFLECTI',    (/ 159,  31, 109,     1,   0, 0,  0,  16 /))
CALL T ('S', 'CLOUD_WATER ',    (/ 159,  32, 109,     1,   0, 0,  0,  16 /))
CALL T ('S', 'RAD_LIQUID  ',    (/ 159,  32, 109,     1,   0, 0,  0,  16 /))
CALL T ('S', 'RAIN        ',    (/ 159,  33, 109,     1,   0, 0,  0,  16 /))
CALL T ('S', 'ICE_CRYSTA  ',    (/ 128, 247, 109,     1,   0, 0,  0,  16 /))
CALL T ('S', 'RAD_SOLID_WA',    (/ 128, 247, 109,     1,   0, 0,  0,  16 /))
CALL T ('S', 'SNOW        ',    (/ 159,  34, 109,     1,   0, 0,  0,  16 /))
CALL T ('S', 'GRAUPEL     ',    (/ 159,  35, 109,     1,   0, 0,  0,  16 /))
CALL T ('S', 'CLOUD_FRACTI',    (/ 159,  36, 109,     1,   0, 0,  0,  16 /))
CALL T ('S', 'TKE         ',    (/ 159,  37, 109,     1,   0, 0,  0,  16 /))
CALL T ('S', 'THETA_VIRTUA',    (/ 159,  38, 109,     1,   0, 0,  0,  16 /))
CALL T ('S', 'HAIL        ',    (/ 159, 100, 109,     1,   0, 0,  0,  12 /))
CALL T ('S', 'RAYT THER CL',    (/ 128, 169, 109,     0,   0, 8,  0,  16 /))
!
CALL T ('SPEC', 'SURFGEOPOTEN', (/   1,   8, 109,     1,   0, 0,  0,  24 /), PMULTI=ZONOVG)
!
CALL T ('KT', 'ISOT_ALTIT ',    (/ 128,   8, 115,     0,   0, 0,  0,  16 /))
CALL T ('KT', 'ISOT_PRESS ',    (/ 254,   1, 115, 27315,   0, 0,  0,  16 /)) 
CALL T ('KB', 'ISOT_ALTIT ',    (/ 128,   8, 115, 27315,   0, 0,  0,  16 /)) 
CALL T ('KB', 'ISOT_PRESS ',    (/ 254,   1, 115, 27315,   0, 0,  0,  16 /)) 
!
CALL T ('C001', '_METEOSAT_07', (/ 129,   1, 100,    64,   0, 0,  0,  12 /))
CALL T ('C002', '_METEOSAT_07', (/ 129,   1, 100,   115,   0, 0,  0,  12 /))
!
CALL T ('C001', '_METEOSAT_09', (/ 129,   1, 100,    39,   0, 0,  0,  12 /))
CALL T ('C002', '_METEOSAT_09', (/ 129,   1, 100,    62,   0, 0,  0,  12 /))
CALL T ('C003', '_METEOSAT_09', (/ 129,   1, 100,    73,   0, 0,  0,  12 /))
CALL T ('C004', '_METEOSAT_09', (/ 129,   1, 100,    87,   0, 0,  0,  12 /))
CALL T ('C005', '_METEOSAT_09', (/ 129,   1, 100,    97,   0, 0,  0,  12 /))
CALL T ('C006', '_METEOSAT_09', (/ 129,   1, 100,   108,   0, 0,  0,  12 /))
CALL T ('C007', '_METEOSAT_09', (/ 129,   1, 100,   120,   0, 0,  0,  12 /))
CALL T ('C008', '_METEOSAT_09', (/ 129,   1, 100,   134,   0, 0,  0,  12 /))
!
CALL T ('C001', '_GOES_11_IMA', (/ 129,   1, 100,    39,   0, 0,  0,  12 /))
CALL T ('C002', '_GOES_11_IMA', (/ 129,   1, 100,    67,   0, 0,  0,  12 /))
CALL T ('C003', '_GOES_11_IMA', (/ 129,   1, 100,   107,   0, 0,  0,  12 /))
CALL T ('C004', '_GOES_11_IMA', (/ 129,   1, 100,   110,   0, 0,  0,  12 /))
!
CALL T ('C001', '_GOES_12_IMA', (/ 129,   1, 100,    39,   0, 0,  0,  12 /))
CALL T ('C002', '_GOES_12_IMA', (/ 129,   1, 100,    67,   0, 0,  0,  12 /))
CALL T ('C003', '_GOES_12_IMA', (/ 129,   1, 100,   107,   0, 0,  0,  12 /))
CALL T ('C004', '_GOES_12_IMA', (/ 129,   1, 100,   110,   0, 0,  0,  12 /))
!
CALL T ('C001', '_MTSAT_01_IM', (/ 129,   1, 100,    39,   0, 0,  0,  12 /))
CALL T ('C002', '_MTSAT_01_IM', (/ 129,   1, 100,    67,   0, 0,  0,  12 /))
CALL T ('C003', '_MTSAT_01_IM', (/ 129,   1, 100,   107,   0, 0,  0,  12 /))
CALL T ('C004', '_MTSAT_01_IM', (/ 129,   1, 100,   110,   0, 0,  0,  12 /))
!

ENDDO

IF (LHOOK) CALL DR_HOOK('FAICOR_MT',1,ZHOOK_HANDLE)

CONTAINS

SUBROUTINE T (CDPREF, CDSUFF, KCODPA, LDNIVA, PMULTI)

CHARACTER (LEN=*),              INTENT (IN) :: CDPREF, CDSUFF
INTEGER,                        INTENT (IN) :: KCODPA (8)
LOGICAL,            OPTIONAL,   INTENT (IN) :: LDNIVA
REAL (KIND=JPDBLR), OPTIONAL,   INTENT (IN) :: PMULTI

LOGICAL :: LLNIVA

LLNIVA = .FALSE.
IF (PRESENT (LDNIVA)) LLNIVA = LDNIVA

FA%NBPARC = FA%NBPARC + 1

IF (LLALLOC) THEN
  FA%YGR1TAB (FA%NBPARC)%CIPREF = CDPREF
  FA%YGR1TAB (FA%NBPARC)%CISUFF = CDSUFF
  FA%YGR1TAB (FA%NBPARC)%NCODPA = INT (KCODPA, JPLIKB)
  FA%YGR1TAB (FA%NBPARC)%LFNIVA = LLNIVA
  IF (PRESENT (PMULTI)) THEN
    FA%YGR1TAB (FA%NBPARC)%FMULTI = PMULTI
    FA%YGR1TAB (FA%NBPARC)%LMULTI = .TRUE.
  ENDIF
ENDIF

END SUBROUTINE T
! 
END SUBROUTINE FAICOR_FORT
