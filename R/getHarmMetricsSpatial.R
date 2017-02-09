#' Calculate time series metrics for a tile
#' @description Spatial version of \code{\link{getHarmMetrics}}, using \code{\link{mcCalc}}.
#' @param x RasterBrick or RasterStack Input time series. Layer names must follow proba_v pattern.
#' @param df_probav_sm dataframe of info on Proba-V tiles. See \code{\link{getProbaVinfo}}
#' @param n_years Integer. See \code{\link{getHarmMetrics}}
#' @param order Integer. See \code{\link{getHarmMetrics}}
#' @param robust Logical. See \code{\link{getHarmMetrics}}
#' @param qc RasterBrick or RasterStack. Quality control mask from temporal cloud filtering, where value 1 indicates cloud-free.
#' @param cf_bands Integer. Which bands to use for temporal cloud filter (See \code{\link{smoothLoess}})
#' @param thresholds threshold Numeric vector of length 2. See \code{\link{smoothLoess}}.
#' @param scale_f Numeric of length bands. Scaling factor per band.
#' @param minrows Numeric. Min block size for  \code{\link{mcCalc}}
#' @param mc.cores Numeric. Processing cores, see\code{\link{mcCalc}}
#' @param logfile Character. See \code{\link{mcCalc}}
#' @param filename Character. location and name of output file.
#' @param ... additional arguments to \code{\link{mcCalc}}.
#'
#' @return rasterBrick with metrics as bands.
#' @export
#'

getHarmMetricsSpatial <- function(x, df_probav_sm, n_years=NULL, order=1, robust=FALSE,
                                     qc=NULL, cf_bands=NULL, thresholds=c(-80, Inf, -120, 120) , span=0.3, scale_f=NULL,
                                     minrows=1, mc.cores=1, logfile, filename, ...) {
  # Copy dataframe
  s_info <- df_probav_sm
  # Assign metadata to variables
  bands <- s_info[s_info$date == s_info$date[1], 'band']
  dates <- s_info[s_info$band == bands[1], 'date']
  ydays <- s_info[s_info$band == bands[1], 'yday']
  if (nlayers(x) != length(bands) * length(dates)) {
    stop("Number of input layers differs from time series length.")
  }
  if (!is.null(qc) && nlayers(qc) != nlayers(x)) {
    stop("Quality control mask length differs from time series length.")
  }
  thresholds <- matrix(thresholds, nrow=2)
  len_res <- (4 + (order*2)) * length(bands)

  cat("\nOutput layers:", len_res, "\n")


  fun <- function(x){
    # smooth loess and getHarmMetrics
    m <- matrix(x, nrow= length(bands), ncol=length(dates))

    if (!all(is.na(m[1,]))) {
      res <- try({
        if (!is.null(cf_bands)) {
          # smooth loess on all cf bands, then combine
          qc <- foreach(bn = 1:length(cf_bands), .combine='&') %do% {
            qcb <-   smoothLoess(m[cf_bands[bn],], dates = dates, threshold = thresholds[,bn],
                                 res_type = "QC", span=span)
            qcb == 1
          }
        }

        #get metrics
        coefs <- apply(m, 1, FUN=getHarmMetrics, yday=ydays, QC_good=qc, order=order, robust=robust, dates = dates)

        if (!is.null(scale_f)){
          # scaling
          res_1 <- as.integer(t(round((scale_f) * t(coefs))))
        } else res_1 <- c(coefs)

        if (length(res_1) != len_res) {
          res_1 <- rep(-9999, len_res)
        }
        res_1
      })

      if(class(res) == 'try-error') {
        res <- rep(NA_integer_, len_res)
      }

    } else {
      res <- rep(NA_integer_, len_res)
    }

    # no names, because they get lost in mc.calc anyway
    return(res)
  }

  # use mcCalc rather than mc.calc (controll minrows)
  out <- mcCalc(x=x, fun=fun, minrows = minrows, mc.cores = mc.cores, logfile=logfile, out_name = filename)

  return(out)
}
