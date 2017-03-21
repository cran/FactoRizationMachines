#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP FactoRizationMachines_predictFM(SEXP);
extern SEXP FactoRizationMachines_trainFM(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"FactoRizationMachines_predictFM", (DL_FUNC) &FactoRizationMachines_predictFM, 1},
    {"FactoRizationMachines_trainFM",   (DL_FUNC) &FactoRizationMachines_trainFM,   1},
    {NULL, NULL, 0}
};

void R_init_FactoRizationMachines(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
