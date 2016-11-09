#' @title Build time stack from probav images
#'
#' @description Stacks Prova-V layers.
#' @author J Eberenz
#' @param x Character. Directory of a Proba-V geotiffs or list of filenames.
#' @param pattern Character. As in \code{\link{list.files}}
#' @param order_chrono Logical. Wether to order the stack chronologically. Defatult \code{TRUE}.
#' @param tile Character. Which tile to process. Format: "X00Y00".
#' @param quick Logical. See \code{raster::stack}
#' @param start_date Date. first date to process.
#' @param end_date Date. Last date to process.
#' @param ... Additional arguments to \code{raster::writeRaster}
#'
#' @return a RasterStack or rasterBrick.
#'
#' @export
#'
#' @import raster

timeStackProbaV <- function(x, pattern, order_chrono=TRUE, tile=NULL, quick=FALSE, start_date = NULL, end_date=NULL, ...){

  df_info <- getProbaVinfo(x, pattern)
  if(order_chrono){
    df_info <- df_info[order(df_info$date),]
  }

  if (!is.null(tile)) df_info  <- df_info[df_info$tile==tile, ]
  if (!is.null(end_date) & !is.null(start_date)){
    df_info  <- df_info[(df_info$date) <= end_date & (df_info$date) >= start_date,]
  } else if (is.null(end_date)) {
    end_date <- max(df_info$date)
  } else if (is.null(start_date)){
    start_date <- min(df_info$date)
  }

  if (!is.null(end_date) & !is.null(start_date)) df_info  <- df_info[(df_info$date) <= end_date & (df_info$date) >= start_date,]

  s <- raster::stack(file.path(x, df_info$fpath), quick=quick)

  #cat("build brick ... ")
  #s <- brick(s)
  names(s) <- row.names(df_info)

  s <- setZ(x=s, z=format(df_info$date, "%Y%j"))

  if(hasArg(filename)) {
    cat("writing...")
    out <- writeRaster(s, progress="bar", ... )
    return(out)
  }

  return(s)
}
