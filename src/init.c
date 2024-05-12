#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>


/* .Call calls */
extern SEXP row_from_y_C(SEXP, SEXP, SEXP);
static const R_CallMethodDef CallEntries[] = {
  {"row_from_y_C",  (DL_FUNC) &row_from_y_C,  3},
  {NULL, NULL, 0}
};

void R_init_vaster(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
