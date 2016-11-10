#' Wrapper/batcher to pre-process Proba-V in batch mode
#'
#' @description Processes Proba-V data for subsequentent use in time-series analysis. Performs Proba-V cleaning and operations with parallel support using foreach
#'
#' @param x Character. Directory where the data is located. Or list of file names.
#' @param pattern Character. if more folders are supplied, they must all follow the YYYYMMDD folder structure. Default is 'RADIOMETRY'. See \link{list.files} for more details
#' @param tiles Character. Tiles to process, format "X10Y06",
#' @param start_d Date. Starting date.
#' @param end_d Date. End date.
#' @param QC_val Numeric. See \code{\link{cleanProbaV}}
#' @param fill Numeric. See \code{\link{cleanProbaV}}
#' @param as.is Logical. See \code{\link{cleanProbaV}}
#' @param ncores Numeric. Number of cores to use for processing. See \link{registerDoParallel}
#' @param outdir Character. Directory where the output should be written.
#' @param overwritw Logical. Wether to overwrite existing files using the same naming convention in outdir. ault is \code{FALSE})
#'
#' @seealso \code{\link{cleanProbaV}}, and \code{\link{timeStackProbaV}} for stacking.
#'
#' @return This function is used for its side effect of producing cleaned data, hence written to disk at \code{outdir} location
#'
#' @export
#'
#' @import raster
#' @import doParallel
#' @import foreach
#' @import stringr

processProbaVbatch <- function(x, pattern = "RADIOMETRY", tiles=NULL, start_date=NULL, end_date=NULL, QC_val = QC_val, fill=NULL, as.is=FALSE, outdir, ncores=1, overwrite=FALSE) {

  if (!is.character(x)) {
    stop('x needs to be of class character')
  }
  # Copy folder structure for name output
  x2 <- x

  if(length(x) == 1) {

    info <- getProbaVinfo(x, pattern=pattern, tiles = tiles)

    # x <- list.files(path=x, pattern=pattern, full.names=TRUE,  recursive = T, include.dirs = F, no.. = T)
  } else {

    df = list()
    for (i in 1:length(x)){
      dat <- getProbaVinfo(x[i], pattern = pattern, tiles = tiles)
      df[[i]] <- dat # add it to your list
    }
    info <- do.call(rbind, df)
  }
  if (!is.null(tiles)) {
    info <- subset(info, info$tile %in% tiles)
    # x <- x[info$tile %in% tiles]
  }
  if (!is.null(end_date) & !is.null(start_date)) {
    info <- subset(info, info$date >= start_date & info$date <= end_date)
  } else if (!is.null(end_date) & is.null(start_date)){
    info <- subset(info, info$date <= end_date)
  } else if (!is.null(start_date) & is.null(end_date)){
    info <- subset(info, info$date >= start_date)
  }

  # When you only insert 1 directory
  if (length(x) == 1){
    x <- paste0(x2,'/',info$fpath)
  } else {
    # When you have multiple directories following YYYYMMDD
    f <- gsub("-", "", info$date)
    gg <- subset(x2, str_sub(x2,-8,-1) %in% f)
    x <- paste0(gg,'/',info$fpath)
  }

  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

  if (pattern == "NDVI.tif$"){
    type <- "FLT4S"
  } else {
    type <- dataType(raster(x[1]))
  }

  outnames <- file.path(outdir, gsub("\\.tif", "_sm.tif", basename(x)))
  outnames <- gsub("RADIOMETRY_sm\\.tif", "RED0_sm.tif", outnames)

  if(!overwrite){
    x <- x[!file.exists(outnames)]
    outnames <- outnames[!file.exists(outnames)]
  }

  outnames <- gsub("_RED0_sm\\.tif", ".tif", outnames)

  cat("Processing", length(x), "files. Names: ", length(outnames), "\n")

  if (ncores > 1){
    registerDoParallel(ncores)
  } else registerDoSEQ()

  xprocessed <- foreach(i=x, o=outnames, .combine = c, .multicombine = T, .inorder = F, .packages = c("raster", "rgdal"), .verbose = T ) %dopar% {
    cat("...out:", o)
    r <- cleanProbaV(i, filename=o, QC_val = QC_val, fill=fill, datatype = type, as.is = as.is, overwrite = overwrite )
    print(r)
    o
  }

  registerDoSEQ()

  if (length(xprocessed) == 0){
    cat(length(xprocessed), " files processed, files allready exists or are out of range")
  } else {
    cat(length(xprocessed), " files processed")
  }


  # # delete if files exists
  # f_exist <- subset(outnames, file.exists(outnames) == T)
  # if (length(f_exist) > 0 & pattern != "NDVI.tif$"){
  #   cat("\n","deleting temp files")
  #   file.remove(f_exist)
  #   cat(length(f_exist), " files removed")
  # }

  return(xprocessed)

  # old...
  #     mcmapply(FUN=cleanProbaV, f_data=x, filename = outnames,
  #          MoreArgs = list(QC_val = QC_val, fill=fill, datatype = type, read_gdal=read_gdal), mc.cores=mc.cores)

}
