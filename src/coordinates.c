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

SEXP coord_from_bin(SEXP bins, SEXP range, SEXP index) {
  int nn = LENGTH(index);
  SEXP out;
  out = PROTECT(Rf_allocVector(REALSXP, nn));

  int nbins = INTEGER(bins)[0];
  double res = (REAL(range)[1] - REAL(range)[0])/nbins;
  double cmin = REAL(range)[0];
  double* rout = REAL(out);
  int* rindex = INTEGER(index);
  for (int i = 0; i < nn; i++) {
    if ((rindex[i] >= 0) && (rindex[i] < nbins)) {
      rout[i] = cmin + res/2 + rindex[i] * res;
    } else {
      rout[i] = R_NaReal;
    }
  }
  UNPROTECT(1);
  return out;
}
SEXP x_from_col_(SEXP ncol, SEXP xlim, SEXP col) {
  return coord_from_bin(ncol, xlim, col);
}

SEXP y_from_row_(SEXP nrow, SEXP ylim, SEXP row) {
  return coord_from_bin(nrow, ylim, row);
}
