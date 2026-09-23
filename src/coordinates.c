# include <R.h>
# include <Rinternals.h>
# include "vaster.h"

// 0-based bin index of coord within range; from_max = 0 counts up from range[0]
// (columns), from_max = 1 counts down from range[1] (rows, top first). A coord on
// the far edge goes into the last bin, outside the range is NA.
SEXP index_from_coord_(SEXP bins, SEXP range, SEXP coord, SEXP from_max) {
  check_size(bins);
  check_range(range);
  int nn = LENGTH(coord);
  int down = Rf_asLogical(from_max) == 1;
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
    if ((rcoord[i] > cmax) || (rcoord[i] < cmin)) {
      rout[i] = R_NaReal;
    } else if (rcoord[i] == (down ? cmin : cmax)) {
      rout[i] = rbin - 1;
    } else {
      rout[i] = trunc((down ? (cmax - rcoord[i]) : (rcoord[i] - cmin))/scl);
    }
  }
  UNPROTECT(1);
  return out;
}


SEXP coord_from_index_(SEXP bins, SEXP range, SEXP index) {
  check_size(bins);
  check_range(range);
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

SEXP coord_centre_(SEXP bins, SEXP range) {
  int nn = INTEGER(bins)[0];
  SEXP out;
  out = PROTECT(Rf_allocVector(REALSXP, nn));
  double cmin = REAL(range)[0];
  double scl = (REAL(range)[1] - cmin) / nn;
  double* rout = REAL(out);
  for (int i = 0; i < nn; i++) {
      rout[i] = cmin + scl/2 + i * scl;
  }
  UNPROTECT(1);
  return out;
}



