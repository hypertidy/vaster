#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP coord_centre_(SEXP, SEXP);
extern SEXP coord_from_index_(SEXP, SEXP, SEXP);
extern SEXP index_from_coord_(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"coord_centre_",     (DL_FUNC) &coord_centre_,     2},
    {"coord_from_index_", (DL_FUNC) &coord_from_index_, 3},
    {"index_from_coord_", (DL_FUNC) &index_from_coord_, 3},
    {NULL, NULL, 0}
};

void R_init_vaster(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
