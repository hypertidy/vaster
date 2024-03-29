% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vaster.R
\name{origin}
\alias{origin}
\title{Origin of grid alignment}
\usage{
origin(dimension, extent = NULL)
}
\arguments{
\item{dimension}{integer ncol, nrow}

\item{extent}{numeric extent xmin,xmax,ymin,ymax}
}
\value{
coordinate of grid origin
}
\description{
Origin of grid alignment
}
\examples{
origin(c(10, 5), c(0, 10, 0, 5))
}
