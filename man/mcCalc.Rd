% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mcCalc.R
\name{mcCalc}
\alias{mcCalc}
\title{Multicore implementation of the raster::\code{\link{calc}} function.}
\usage{
mcCalc(x, minrows = 1, fun, mc.cores = 1, logfile = NULL, ...)
}
\arguments{
\item{x}{Raster* object}

\item{minrows}{Integer}

\item{fun}{Function to be applied to the raster object.}

\item{mc.cores}{Integer Numvber of corrse, see \code{\link{mclapply}}.}

\item{...}{Arguments to be passed to \code{\link{writeRaster}}.}
}
\value{
a Raster* object
}
\description{
Allows functions to be applied to raster objects, with multicore support, controlling the chunksize with minrows.
}
\details{
For further help, see \code{\link{calc}}. Warnings of the parallel package (see \code{\link{mclapply}} for instance) apply to this function.
}
\author{
Johannes Eberenz, adapted from Loic Dutrieux
}
\seealso{
\code{\link{calc}}
}
