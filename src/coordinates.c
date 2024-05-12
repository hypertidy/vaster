# include <R.h>
# include <Rinternals.h>
# include "vaster.h"

SEXP bin_from_float(SEXP bins, SEXP range, SEXP coord) {
 int nn = LENGTH(coord);
  double scl = (REAL(range)[1] - REAL(range)[0])/INTEGER(bins)[0];
  SEXP out;
  out = PROTECT(Rf_allocVector(REALSXP, nn));

  // save the index, as per https://github.com/hadley/r-internals/blob/master/vectors.md#get-and-set-values
  double* rout = REAL(out);
  double* rcoord = REAL(coord);
  double cmax = REAL(range)[1];
  double cmin = REAL(range)[0];
  double rbin = INTEGER(bins)[0];
  for (int i = 0; i < nn; i++) {
    if (rcoord[i] == cmax) {
      rout[i] = rbin - 1;
    } else if ((rcoord[i] > cmax) || (rcoord[i] < cmin)) {
      rout[i] = R_NaReal;
    } else {
      rout[i] =    trunc((cmax - rcoord[i])/scl);
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
