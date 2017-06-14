calc_N_N2_Max <- function(x) {
  N <- apply(x>0, 1, sum)
  mx <- apply(x, 1, max)
  x <- sweep(x, 1, rowSums(x), "/")
  N2 <- exp(-log(apply(x^2, 1, sum)))
  res <- cbind(N, N2, max=mx)
  colnames(res) <- c("N", "N2", "Max")
  res
}

calc_all <- function(d, metric) {
  res <- list()
  if (metric=="TDILM") {
    x.tdi <- calc_TDI(d, "TDI3")
    res[[1]] <- calc_EQR(x.tdi)
    x.tdi <- calc_TDI(d, "TDI4")
    res[[2]] <- calc_EQR(x.tdi)
    x.tdi <- calc_TDI(d, "TDI5LM")
    res[[3]] <- calc_EQR(x.tdi)
    names(res) <- c("TDI3", "TDI4", "TDI5LM")
  } else if (metric=="LTDILM") {
    x.tdi <- calc_TDI(d, "LTDI1")
    res[[1]] <- calc_EQR(x.tdi)
    x.tdi <- calc_TDI(d, "LTDI2")
    res[[2]] <- calc_EQR(x.tdi)
    names(res) <- c("LTDI1", "LTDI2")
  } else if (metric=="DAMLM") {
    x.tdi <- calc_TDI(d, "DAM")
    res[[1]] <- calc_EQR(x.tdi)
    names(res) <- "DAM"
  } else if (metric=="TDINGS") {
    x.tdi <- calc_TDI(d, "TDI5NGS")
    res[[1]] <- calc_EQR(x.tdi)
    names(res) <- "TDI5NGS"
  }
  class(res) <- "DARLEQ3_EQR"
  res
}

darleq3 <- function(inFile=NULL, sheet=NULL, metric.type=NULL, outFile=NULL) {
  if (!interactive() & (is.null(inFile) | is.null(sheet)))
    return()
  if (!require(openxlsx, quietly=TRUE))
    stop("This function needs the package openxlsx, please install it")
  metric.types <- c("TDILM", "LTDILM", "DAMLM", "TDINGS")
  metric <- ""
  if (!is.null(metric.type)) {
     res <- pmatch(metric.type, metric.types)
     if(is.na(res))
       stop("Invalid diatom metric")
     if(res == -1)
        stop("Ambiguous metric.type")
     metric <- metric.types[res]
  }

  fn <- get_file_sheet_name(inFile, sheet)
  if (mode(fn) == "character")
    stop(fn)
  if (is.null(metric.type)) {
     opts <- c("River diatom TDI", "Lake diatom LTDI", "River Diatom Acidification Index", "River NGS TDI")
     metric.type <- menu(opts)
     if (metric.type==0)
        stop("Operation cancelled")
     metric <- metric.types[metric.type]
  }
  d <- tryCatch(read_DARLEQ(fn$fn, fn$sheet), error=function(e) { e })
  if ("error" %in% class(d))
    stop(d$message)
  res <- calc_all(d, metric)
  save_darleq3(res, outFile)
}

save_darleq3 <- function(d, outFile) {
   wb <- openxlsx::createWorkbook("Temp")
   for (i in names(d)) {
     addWorksheet(wb, i)
     openxlsx::writeDataTable(wb, i, d[[i]], withFilter=FALSE, keepNA=FALSE)
   }
   if (is.null(outFile)) {
      cat("Results are ready to save, please choose a file name\n")
      Filt <- matrix(c("Excel (*.xlsx)", "*.xlsx"), nrow=1)
      outFile <- choose.files(multi=FALSE, filters=Filt)
     if (length(outFile) < 1) {
        stop("Operation cancelled")
     }
   }
   saveWorkbook(wb, outFile, overwrite=TRUE)

}
