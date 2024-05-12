# include <R.h>
# include <Rinternals.h>

SEXP row_from_y_C(SEXP dimension, SEXP extent, SEXP py)
{
  // FIXME: long vectors, and values > intmax
  int nn = LENGTH(py);
  double y_res = (REAL(extent)[3] - REAL(extent)[2])/REAL(dimension)[1];
  double y_max = REAL(extent)[3];
  double rownr;
  SEXP out = PROTECT(Rf_allocVector(REALSXP, nn));
  for (int i = 0; i < nn; i++){
    if (REAL(py)[i] == REAL(extent)[2]) {
      rownr = REAL(dimension)[1] - 1;
    } else if (REAL(py)[i] > REAL(extent)[3] | REAL(py)[i] < REAL(extent)[2]) {
      rownr = R_NaReal;
    } else {
      rownr =   1 + trunc(y_max - REAL(py)[i])/y_res;
    }
    REAL(out)[i] = rownr;
  }
  UNPROTECT(1);
  return out;
}
