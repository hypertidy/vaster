# include <R.h>
# include <Rinternals.h>

void check_size(SEXP size) {
  if ((INTEGER(size)[0] == R_NaInt) | (INTEGER(size)[0] < 1)) {
    Rf_error("%s", "bad dimension ncol or nrow is < 1 or missing");
  }
}
void check_range(SEXP range) {
  double c_max = REAL(range)[1];
  double c_min = REAL(range)[0];
  if (!R_finite(c_max) | !R_finite(c_min) | (c_max <= c_min)) {
    Rf_error("%s", "bad extent, xmax <= xmin, ymax <= ymin, or missing values");
  }
}

void check_extent(SEXP extent) {
  int nn = LENGTH(extent);
  if (nn != 4) {
    Rf_error("%s", "extent must be numeric length 4");
  }
 double y_max = REAL(extent)[3];
 double y_min = REAL(extent)[2];
 double x_max = REAL(extent)[1];
 double x_min = REAL(extent)[0];
 if (!R_finite(x_max) | !R_finite(x_min) | (x_max <= x_min)) {
   Rf_error("%s", "bad extent, xmax <= xmin or missing values");
 }
 if (!R_finite(y_max) | !R_finite(y_min) | (y_max <= y_min)) {
   Rf_error("%s", "bad extent, ymax <= ymin or missing values");
 }
}

void check_dimension(SEXP dimension) {
  int nn = LENGTH(dimension);
  if (nn != 2) {
    Rf_error("%s", "dimension must be numeric length 2");
  }

  if (INTEGER(dimension)[0] < 1) {  // or missing or bad length or bad type
    Rf_error("%s", "bad dimension ncol is < 1 or missing");

  }
  if (INTEGER(dimension)[1] < 1) {
    Rf_error("%s", "bad dimension nrow is < 1 or missing");

  }
}
