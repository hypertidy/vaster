% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gdal-utils-helpers.R
\name{ts_te}
\alias{ts_te}
\alias{te}
\alias{ts}
\title{Target size, extent}
\usage{
te(extent)

ts(dimension)

ts_te(dimension, extent)
}
\arguments{
\item{extent}{xmin,xmax,ymin,ymax}

\item{dimension}{ncol, nrow}
}
\value{
string formatted for GDAL command line (-te -ts)
}
\description{
Format properties for the GDAL options.
}
\examples{
ts_te(c(10, 100), 1:4)
ts(c(10, 100))
te(1:4)
}
