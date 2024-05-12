#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP col_from_x_(SEXP, SEXP, SEXP);
extern SEXP row_from_y_(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"col_from_x_", (DL_FUNC) &col_from_x_, 3},
    {"row_from_y_", (DL_FUNC) &row_from_y_, 3},
    {NULL, NULL, 0}
};

void R_init_vaster(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
