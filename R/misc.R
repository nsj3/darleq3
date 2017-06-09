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
  res
}

darleq3 <- function(inFile=NULL, sheet=NULL, metric.type="TDILM", outFile="Results.xlsx") {
  if (!interactive() & (is.null(inFile) | is.null(sheet)))
    return(NULL)
  if (!require(openxlsx, quietly=TRUE))
    stop("This function needs the package openxlsx, please install it")
  fn <- tryCatch(get_file_sheet_name(inFile, sheet), error=function(e) { cat(e); return(NULL) })
  if (is.null(fn))
    return(NULL)
  if (is.null(metric.type)) {
     opts <- c("River diatom TDI", "Lake diatom LTDI", "River Diatom Acidification Index", "River NGS TDI")
     metric <- menu(opts)
     if (tdi==0)
        return(NULL)
     metric <- c("TDILM", "LTDILM", "DAMLM", "TDINGS")[metric]
  } else {
     metric <- metric.type
  }
  d <- tryCatch(read_DARLEQ(fn$fn, fn$sheet), error=function(e) { cat(e$message); return(NULL) })
  if (is.null(d))
    return(NULL)
  res <- calc_all(d, metric)
  wb <- openxlsx::createWorkbook("Temp")
  for (i in names(res)) {
    addWorksheet(wb, i)
    openxlsx::writeDataTable(wb, i, res[[i]], withFilter=FALSE, keepNA=FALSE)
  }
  saveWorkbook(wb, outFile, overwrite=TRUE)
}
