#ifndef _LFI_FORT_H
#define _LFI_FORT_H

/**** *lfi_fort.h* - Declaration of Fortran LFI routines; these routines are packed in a lficb_t structure
 *
 *    Author. 
 *    ------- 
 *     Philippe Marguinaud *METEO-FRANCE*
 *     Original : 12-08-2013
 *
 * Description :
 * Fortran routines may be handled by the lfi_intf layer. Therefore, it is necessary to define a such a callback
 * structure
 */


#include "lfi_type.h"
#include "lfi_args.h"
#include "lfi_call.h"
#include "lfi_hndl.h"

extern void lfiouv_fort_ (LFIOUV_ARGS_DECL);
extern void lficas_fort_ (LFICAS_ARGS_DECL);
extern void lfiecr_fort_ (LFIECR_ARGS_DECL);
extern void lfifer_fort_ (LFIFER_ARGS_DECL);
extern void lfilec_fort_ (LFILEC_ARGS_DECL);
extern void lfinfo_fort_ (LFINFO_ARGS_DECL);
extern void lfipos_fort_ (LFIPOS_ARGS_DECL);
extern void lfiver_fort_ (LFIVER_ARGS_DECL);
extern void lfiofm_fort_ (LFIOFM_ARGS_DECL);
extern void lfineg_fort_ (LFINEG_ARGS_DECL);
extern void lfilaf_fort_ (LFILAF_ARGS_DECL);
extern void lfiosg_fort_ (LFIOSG_ARGS_DECL);
extern void lfinum_fort_ (LFINUM_ARGS_DECL);
extern void lfisup_fort_ (LFISUP_ARGS_DECL);
extern void lfiopt_fort_ (LFIOPT_ARGS_DECL);
extern void lfinmg_fort_ (LFINMG_ARGS_DECL);
extern void lficap_fort_ (LFICAP_ARGS_DECL);
extern void lfifra_fort_ (LFIFRA_ARGS_DECL);
extern void lficfg_fort_ (LFICFG_ARGS_DECL);
extern void lfierf_fort_ (LFIERF_ARGS_DECL);
extern void lfilas_fort_ (LFILAS_ARGS_DECL);
extern void lfiren_fort_ (LFIREN_ARGS_DECL);
extern void lfiini_fort_ (LFIINI_ARGS_DECL);
extern void lfipxf_fort_ (LFIPXF_ARGS_DECL);
extern void lfioeg_fort_ (LFIOEG_ARGS_DECL);
extern void lfinaf_fort_ (LFINAF_ARGS_DECL);
extern void lfiofd_fort_ (LFIOFD_ARGS_DECL);
extern void lfiomf_fort_ (LFIOMF_ARGS_DECL);
extern void lfiafm_fort_ (LFIAFM_ARGS_DECL);
extern void lfista_fort_ (LFISTA_ARGS_DECL);
extern void lfiosf_fort_ (LFIOSF_ARGS_DECL);
extern void lfilap_fort_ (LFILAP_ARGS_DECL);
extern void lfioef_fort_ (LFIOEF_ARGS_DECL);
extern void lfimst_fort_ (LFIMST_ARGS_DECL);
extern void lfinim_fort_ (LFINIM_ARGS_DECL);
extern void lfisfm_fort_ (LFISFM_ARGS_DECL);
extern void lfinsg_fort_ (LFINSG_ARGS_DECL);
extern void lfideb_fort_ (LFIDEB_ARGS_DECL);
extern void lfiomg_fort_ (LFIOMG_ARGS_DECL);
extern void lfifmd_fort_ (LFIFMD_ARGS_DECL);

extern lficb_t lficb_fort;

/* This structure is the head of LFICOM (see lfimod.F90) */

typedef struct lficom_t
{
  character cmagic[8];  /* "LFI_FORT"                */
  void * lfihl;         /* Linked list of lfi_hndl_t */
}
lficom_t;

/* Create a LFI handle from a Fortran LFICOM */

extern lfi_hndl_t * lfi_get_fort_hndl (void *);

#endif
