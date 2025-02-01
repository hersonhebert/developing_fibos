#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdlib.h>
#include <R_ext/Visibility.h>

/* Declarações das funções Fortran */
extern void F77_NAME(main_intermediate)(double *, double *, double *, int *, int *, int *);
extern void F77_NAME(main_intermediate01)(double *, double *, double *, int *, int *, int *);
extern void F77_NAME(main_intermediate02)(int *);
extern void F77_NAME(runsims)(int *);
extern void F77_NAME(surfcal)(void);
extern void F77_NAME(renum)(void);
extern void F77_NAME(main)(int *resnum, int *natm, double *x, double *y, double *z,
                     char *atype, char *restype, char *chain, char *aarestype,
                     int *iresf, int *iresl, int *atype_len, int *restype_len,
                     int *chain_len, int *aarestype_len);
extern void F77_NAME(respak)(void);

/* Tabela de registro das funções Fortran */
static const R_FortranMethodDef FortranEntries[] = {
  {"main_intermediate",   (DL_FUNC) &F77_NAME(main_intermediate),   6},
  {"main_intermediate01", (DL_FUNC) &F77_NAME(main_intermediate01), 6},
  {"main_intermediate02", (DL_FUNC) &F77_NAME(main_intermediate02), 1},
  {"runSIMS",             (DL_FUNC) &F77_NAME(runsims),            1},
  {"surfcal",             (DL_FUNC) &F77_NAME(surfcal),            0},
  {"renum",               (DL_FUNC) &F77_NAME(renum),              0},
  {"main",                (DL_FUNC) &F77_NAME(main),               15},
  {"respak",              (DL_FUNC) &F77_NAME(respak),             0},
  {NULL, NULL, 0}
};

/* Inicialização do pacote */
void attribute_visible R_init_fibos(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
  R_useDynamicSymbols(dll, TRUE);
  R_forceSymbols(dll, FALSE);
}
