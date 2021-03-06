% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smoothLoess.R
\name{smoothLoess}
\alias{smoothLoess}
\title{Smooth time series using loess}
\usage{
smoothLoess(tsx, QC_good = NULL, dates = NULL, threshold = c(-50, Inf),
  res_type = c("distance", "sd_distance", "all", "filled", "omit", "QC"), ...)
}
\arguments{
\item{tsx}{ts or numeric vector. Time series data.}

\item{QC_good}{Integer or Logical vector. Optional time series of ts quality: 0=bad, 1=good}

\item{dates}{Dates or Numeric. Vector of observation times. Overwrites index of tsx.}

\item{threshold}{Numeric vector of length 2. Upper and lower threshold for non-outlier distances from smoothed time series.}

\item{res_type}{Character. Determins the returned object , see \link[=dest]{details}.}

\item{...}{additional arguments to \code{\link{loess}}}
}
\value{
a ts, numeric vector or data.frame, see \link[=dest]{details}
}
\description{
Temporal outliers are marked or removed based on their distanced from a smoothing model derived with \code{\link{loess}}.
}
\details{
"distance" gives the distance, "sd_distance" the distance devided by the sd,
"filled", a ts with outleirs replaced by smoothed values, "omit" the input ts
with outliers removed,
"QC" the status of observations: 0=Missing/band input QC, 1=good, 2=temporal outlier.
"all" returns a data.frame with all these vectors.
}

