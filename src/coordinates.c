# include <R.h>
# include <Rinternals.h>
# include "vaster.h"

SEXP bin_from_float(SEXP bins, SEXP range, SEXP coord) {
 int nn = LENGTH(coord);
  double scl = (REAL(range)[1] - REAL(range)[0])/INTEGER(bins)[0];
  SEXP out;
  out = PROTECT(Rf_allocVector(REALSXP, nn));

  for (int i = 0; i < nn; i++) {
    if (REAL(coord)[i] == REAL(range)[1]) {
      REAL(out)[i] = INTEGER(bins)[0] - 1;
    } else if ((REAL(coord)[i] > REAL(range)[1]) | (REAL(coord)[i] < REAL(range)[0])) {
      REAL(out)[i] = R_NaReal;
    } else {
      REAL(out)[i] =    trunc((REAL(range)[1] - REAL(coord)[i])/scl);
    }
  }
  UNPROTECT(1);
  return out;
}
SEXP col_from_x_(SEXP ncol, SEXP xlim, SEXP px)
{
  check_size(ncol);
  check_range(xlim);

 return bin_from_float(ncol, xlim, px);
}
SEXP row_from_y_(SEXP nrow, SEXP ylim, SEXP py)
{
  check_size(nrow);
  check_range(ylim);


  return bin_from_float(nrow, ylim, py);

}
