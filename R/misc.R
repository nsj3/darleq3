#' Calculate summaries for diatom samples
#'
#' @param x data frame of diatom count or relative abundance data
#' @return A matrix with 3 columns listing the number of taxa (N), Hill's N2, the effective number of taxa, and the maximum abundance (Max) of a taxon in each sample (row) of \code{x}.
#'
#' @author Steve Juggins \email{Stephen.Juggins@@ncl.ac.uk}
#' @references Hill, M.O. (1973). Diversity and evenness: a unifying notation and its consequences. \emph{Ecology} 54, 427--473.
#' @note This is a helper function and is not meant to be called by the user directly.

calc_N_N2_Max <- function(x) {
  N <- apply(x>0, 1, sum)
  mx <- apply(x, 1, max)
  x <- sweep(x, 1, rowSums(x), "/")
  N2 <- exp(-log(apply(x^2, 1, sum)))
  res <- cbind(N, round(N2, 2), max=round(mx, 2))
  colnames(res) <- c("N", "N2", "Max")
  res
}

#' Calculate diatom water quality metrics for metric \"families\"
#'
#' @param x Data frame of diatom count or relative abundance data
#' @param  metric.type Metric family for calculation.  One of either "TDILM" for river LM TDI metrics (TDI3, TDI4 and TDI5LM), "TDINGS" for river NGS TDI metrics (TDI5NGS), "LTDILM" for lake TDI metrics (LTDI1 and LTDI2) or "DAMLM" for river diatom acidification metric.
#' @return A matrix with 3 columns listing the number of taxa, the N2 effective number of taxa, and the maximum abundance for each sample (row) of \code{x}.
#' @description This is a wrapper function to \code{\link{calc_Metric}} that calculates all metrics in a given family.  Metric families and their constituent metrics are as follows: TDILM (TDI3, TDI4, TDI5LM - river TDI metrics for LM data); TDINGS (TDI5NGS - river TDI metric for NGS data); LTDILM (LTDI1, LTDI2 - lake TDI metrics for LM data); DAMLM (DAM - river acidification metric for LM data).  For example, choosing metric family "TDILM" will calculate metrics TDI3, TDI4 and TDI5LM.

#' @author Steve Juggins \email{Stephen.Juggins@@ncl.ac.uk}
#' @references Kelly, M., S. Juggins, R. Guthrie, S. Pritchard, J. Jamieson, B. Rippey, H. Hirst, and M. Yallop, Assessment of ecological status in UK rivers using diatoms. \emph{Freshwater Biology}, 2008. 403-422.
#' @references Juggins, S., M. Kelly, T. Allott, M. Kelly-Quinn, and D. Monteith, A Water Framework Directive-compatible metric for assessing acidification in UK and Irish rivers using diatoms. \emph{Science of The Total Environment}, 2016. 671-678.
#' @references Bennion, H., M.G. Kelly, S. Juggins, M.L. Yallop, A. Burgess, J. Jamieson, and J. Krokowski, Assessment of ecological status in UK lakes using benthic diatoms. \emph{Freshwater Science}, 2014. 639-654.

calc_Metric_EQR <- function(x, metrics="TDI5LM", verbose=TRUE) {
  metrics2 <- darleq3_data$metric.codes
  if (!is.null(metrics)) {
    mt <- metrics %in% metrics2
    if(any(is.na(mt)))
      .errMessage("Invalid diatom metric")
  } else {
    .errMessage("metrics missing with no default", verbose)
  }
  res <- vector("list", length=length(metrics))

  for (i in 1:length(metrics)) {
    x.tdi <- tryCatch(calc_Metric(x$diatom_data, metrics[i], verbose=FALSE), error=function(e) { e } )
    if (inherits(x.tdi, "error"))
        .errMessage(x.tdi$message, verbose)
    res[[i]] <- calc_EQR(x.tdi, x$header)
    res[[i]]$Job_Summary <- x.tdi$Job_Summary
  }
  names(res) <- metrics
  class(res) <- "DARLEQ3_EQR"
  res
}

save_darleq3 <- function(d, outFile=NULL, fn="", sheet="", verbose=TRUE) {
  if (is.null(outFile))
    .errMessage("outfile missing with no default", verbose)
  save_Job <- function(x, i, fn, sheet) {
    nm <- paste0(i, "_Job_Summary")
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, startCol = 1, startRow = 1, x=paste("File:", fn))
    openxlsx::writeData(wb, nm, startCol = 1, startRow = 2, x=paste("Sheet:", fn))
    openxlsx::writeData(wb, nm, startCol = 1, startRow = 4, x=paste("Metric: ", x$Metric))
    openxlsx::writeData(wb, nm, startCol = 1, startRow = 5, x=paste("Number of taxa: ", x$N_taxa))
    openxlsx::writeData(wb, nm, startCol = 1, startRow = 6, x=paste("Number of taxa with no occurrences: ", x$N_taxa - x$N_taxa_gt_zero))
    openxlsx::writeData(wb, nm, startCol = 1, startRow = 7, x=paste("Number of samples: ", x$N_samples))
    openxlsx::writeData(wb, nm, startCol = 1, startRow = 8, x=paste("Number of samples with no taxa: ", x$N_samples - x$N_samples_gt_zero))
    if (!is.null(x$MissingTaxa)) {
      openxlsx::writeData(wb, nm, startCol = 1, startRow = 10, x=paste("The following taxa do not have", x$Metric, "scores in the DARLEQ3 database:"))
      openxlsx::writeDataTable(wb, nm, startCol = 1, startRow = 10, x=x$MissingTaxa, withFilter=FALSE, keepNA=FALSE)
    }
  }
  wb <- openxlsx::createWorkbook("Temp")
  nms <- names(d)
  for (i in 1:length(nms)) {
    if (!is.null(d[[i]]$Job_Summary)) {
      save_Job(d[[i]]$Job_Summary, nms[i], fn, sheet)
    }
    if (!is.null(d[[i]]$EQR)) {
      nm <- paste0(nms[i], "_Sample Summary")
      openxlsx::addWorksheet(wb, nm)
      openxlsx::writeDataTable(wb, nm, d[[i]]$EQR, withFilter=FALSE, keepNA=FALSE)
    }
    if (!is.null(d[[i]]$Uncertainty)) {
      nm <- paste0(nms[i], "_Uncertainty")
      openxlsx::addWorksheet(wb, nm)
      openxlsx::writeDataTable(wb, nm, d[[i]]$Uncertainty, withFilter=FALSE, keepNA=FALSE)
    }
  }
  openxlsx::saveWorkbook(wb, outFile, overwrite=TRUE)
}

darleq3 <- function(inFile, sheet=NULL, metrics=c("TDI3", "TDI4", "TDI5LM"), outfile=NULL, verbose=TRUE) {
  metrics2 <- darleq3_data$metric.codes
  if (!is.null(metrics)) {
    mt <- metrics %in% metrics2
    if(any(is.na(mt)))
       .errMessage("Invalid diatom metric", verbose)
  } else {
    .errMessage("metrics missing with no default", verbose)
  }
  d <- tryCatch(read_DARLEQ(inFile, sheet, FALSE), error=function(e) { e })
  if (inherits(d, "error"))
    .errMessage(d$message, verbose)

  res <- tryCatch(calc_Metric_EQR(d, metrics, FALSE))
  if (inherits(res, "error"))
    .errMessage(res$message, verbose)

  if (is.null(outfile)) {
    tmp <- basename(inFile)
    pth <- dirname(inFile)
    fn <- strsplit(tmp, "\\.")[[1]][1]
    if (is.null(sheet))
      sheet <- d$sheet
    fn <- paste0("DARLEQ3_Results_", fn, "_", sheet, "_", Sys.Date(), ".xlsx")
    outfile <- file.path(pth, fn)
    outfile <- gsub(" ", "_", outfile)
  }
  retval <- tryCatch(save_darleq3(res, outfile, fn=inFile, sheet=sheet, FALSE))
  if (inherits(retval, "error"))
    .errMessage(retval$message, verbose)
}

