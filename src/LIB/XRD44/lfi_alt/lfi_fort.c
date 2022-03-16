/**** *lfi_fort.c* - Definition of lficb_fort
 *
 *    Author. 
 *    ------- 
 *     Philippe Marguinaud *METEO-FRANCE*
 *     Original : 12-08-2013
 *
 */


#include <stdlib.h>
#include "lfi_fort.h"

lficb_t lficb_fort = 
{
  lfiouv_fort_,
  lficas_fort_,
  lfiecr_fort_,
  lfifer_fort_,
  lfilec_fort_,
  lfinfo_fort_,
  lfipos_fort_,
  lfiver_fort_,
  lfiofm_fort_,
  lfineg_fort_,
  lfilaf_fort_,
  lfiosg_fort_,
  lfinum_fort_,
  lfisup_fort_,
  lfiopt_fort_,
  lfinmg_fort_,
  lficap_fort_,
  lfifra_fort_,
  lficfg_fort_,
  lfierf_fort_,
  lfilas_fort_,
  lfiren_fort_,
  lfiini_fort_,
  lfipxf_fort_,
  lfioeg_fort_,
  lfinaf_fort_,
  lfiofd_fort_,
  lfiomf_fort_,
  lfiafm_fort_,
  lfista_fort_,
  lfiosf_fort_,
  lfilap_fort_,
  lfioef_fort_,
  lfimst_fort_,
  lfinim_fort_,
  lfisfm_fort_,
  lfinsg_fort_,
  lfideb_fort_,
  lfiomg_fort_,
  lfifmd_fort_,
};

/* Only free the memory allocated for the handle; the underlying LFICOM object
 * is freed in lfimod.F90
 */

static void lfi_del_fort_hndl (lfi_hndl_t * lfi)
{
  free (lfi);
}

/* Check the unit is not already open; we use the lfinum routine */

static int lfi_opn_fort_hndl (lfi_hndl_t * lfi, integer64 * KNUMER)
{
  integer64 KRANG;
  lfi->cb->lfinum (lfi->data, KNUMER, &KRANG);
  return KRANG > 0 ? 1 : 0;
}

/* Messages are handled by the Fortran LFI library itself; hence we return false */

static int lfi_vrb_fort_hndl (lfi_hndl_t * lfi, integer64 * KNUMER)
{
  return 0;
}

/* Fatal errors are handled by the Fortran LFI library itself; hence we return false */

static int lfi_fat_fort_hndl (lfi_hndl_t * lfi, integer64 * KNUMER)
{
  return 0;
}

/* Allocate and initialize a new LFI handle base on a LFICOM structure */

lfi_hndl_t * lfi_get_fort_hndl (void * data)
{
  lfi_hndl_t * lfi = (lfi_hndl_t *)malloc (sizeof (lfi_hndl_t));
  lfi->cb = &lficb_fort;
  lfi->data = data;
  lfi->destroy = lfi_del_fort_hndl;
  lfi->is_open = lfi_opn_fort_hndl;
  lfi->is_verb = lfi_vrb_fort_hndl;
  lfi->is_fatl = lfi_fat_fort_hndl;
  lfi->next = NULL;
  return lfi;
}

