% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/processProbaVbatch.R
\name{processProbaVbatch}
\alias{processProbaVbatch}
\title{Wrapper/batcher to pre-process Proba-V in batch mode}
\usage{
processProbaVbatch(x, pattern = "RADIOMETRY", tiles = NULL,
  start_date = NULL, end_date = NULL, QC_val = QC_val, fill = NULL,
  as.is = FALSE, outdir, ncores = 1, overwrite = FALSE)
}
\arguments{
\item{x}{Character. Directory where the data is located. Or list of file names.}

\item{pattern}{Character. if more folders are supplied, they must all follow the YYYYMMDD folder structure. Default is 'RADIOMETRY'. See \link{list.files} for more details}

\item{tiles}{Character. Tiles to process, format "X10Y06",}

\item{QC_val}{Numeric. See \code{\link{cleanProbaV}}}

\item{fill}{Numeric. See \code{\link{cleanProbaV}}}

\item{as.is}{Logical. See \code{\link{cleanProbaV}}}

\item{outdir}{Character. Directory where the output should be written.}

\item{ncores}{Numeric. Number of cores to use for processing. See \link{registerDoParallel}}

\item{start_d}{Date. Starting date.}

\item{end_d}{Date. End date.}

\item{overwritw}{Logical. Wether to overwrite existing files using the same naming convention in outdir. ault is \code{FALSE})}
}
\value{
This function is used for its side effect of producing cleaned data, hence written to disk at \code{outdir} location
}
\description{
Processes Proba-V data for subsequentent use in time-series analysis. Performs Proba-V cleaning and operations with parallel support using foreach
}
\seealso{
\code{\link{cleanProbaV}}, and \code{\link{timeStackProbaV}} for stacking.
}

