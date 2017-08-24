#' Saves results of diatom EQR and class claculation to an Excel file
#'
#' @param d list of sample and site EQR WFD class results, usually the output from \code{\link{calc_Metric_EQR}}
#' @param outFile name of Excel file to save results.
#' @param fn name of in the input file for inclusion in the job summary page of output.
#' @param sheet name of the input worksheet for inclusion in the job summary page of output.
#' @param verbose logical to indicate if function should stop immediately on error (TRUE) or return a \code{simpleError} (FALSE).  Defaults to TRUE.
#'
#' @author Steve Juggins \email{Stephen.Juggins@@ncl.ac.uk}
#'
#' @examples
#' fn <- system.file("extdata/DARLEQ2TestData.xlsx", package="darleq3")
#' d <- read_DARLEQ(fn, "Rivers TDI Test Data")
#' x <- calc_Metric_EQR(d)
#' save_DARLEQ(x, outFile="results.xlsx")
#'
#' @export save_DARLEQ
#'

save_DARLEQ <- function(d, outFile=NULL, fn="", sheet="", verbose=TRUE) {
  if (is.null(outFile))
    errMessage("outFile missing with no default", verbose)
  save_Job <- function(x, i, fn, sheet, wb) {
    nm <- paste0(i, "_Job_Summary")
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, startCol = 1, startRow = 1, x=paste("File:", fn))
    openxlsx::writeData(wb, nm, startCol = 1, startRow = 2, x=paste("Sheet:", sheet))
    openxlsx::writeData(wb, nm, startCol = 1, startRow = 4, x=paste("Metric: ", x$Metric))
    openxlsx::writeData(wb, nm, startCol = 1, startRow = 5, x=paste("Number of taxa: ", x$Job_Summary$N_taxa))
    openxlsx::writeData(wb, nm, startCol = 1, startRow = 6, x=paste("Number of taxa with no occurrences: ", x$Job_Summary$N_taxa - x$Job_Summary$N_taxa_gt_zero))
    openxlsx::writeData(wb, nm, startCol = 1, startRow = 7, x=paste("Number of samples: ", x$Job_Summary$N_samples))
    openxlsx::writeData(wb, nm, startCol = 1, startRow = 8, x=paste("Number of samples with no taxa: ", x$Job_Summary$N_samples - x$Job_Summary$N_samples_gt_zero))
    startRow <- 10

    if (i %in% c("TDI4", "TDI3", "LTDI1", "LTDI2") & !is.null(x$warnings)) {
      openxlsx::writeData(wb, nm, startCol = 1, startRow = startRow, x="Warnings")
      str <- strsplit(x$warnings, "\n")
      openxlsx::writeData(wb, nm, startCol = 1, startRow = startRow, x=str[[1]])
      startRow <- startRow + length(str[[1]])
    }

    if (!is.null(x$Job_Summary$MissingTaxa) & is.null(x$Job_Summary$MissingTaxa2)) {
      openxlsx::writeData(wb, nm, startCol = 1, startRow = startRow+1, x=paste("The following taxa do not have", x$Metric, "indicator values in the DARLEQ3 database:"))
      openxlsx::writeDataTable(wb, nm, startCol = 1, startRow=startRow+3, x=x$Job_Summary$MissingTaxa, withFilter=FALSE, keepNA=FALSE)
    }
    if (is.null(x$Job_Summary$MissingTaxa) & !is.null(x$Job_Summary$MissingTaxa2)) {
      openxlsx::writeData(wb, nm, startCol = 1, startRow = startRow+1, x=paste("The following taxa have", x$Metric, "indicator values in the DARLEQ3 but not DARLEQ2 database:"))
      openxlsx::writeDataTable(wb, nm, startCol = 1, startRow=startRow+3, x=x$Job_Summary$MissingTaxa2, withFilter=FALSE, keepNA=FALSE)
    }
    if (!is.null(x$Job_Summary$MissingTaxa) & !is.null(x$Job_Summary$MissingTaxa2)) {
      MissingTaxa1 <- x$Job_Summary$MissingTaxa
      MissingTaxa2 <- x$Job_Summary$MissingTaxa2
      nm1 <- colnames(MissingTaxa1)[1:2]
      MissingTaxa <- merge(MissingTaxa1, MissingTaxa2, by=nm1, all=TRUE)
      colnames(MissingTaxa)[-(1:2)] <- c("N.D3", "N2.D3", "Max.D3", "N.D2", "N2.D2", "Max.D2")
      MissingTaxaD3D2 <- MissingTaxa[!is.na(MissingTaxa[, "N.D3"]) & !is.na(MissingTaxa[, "N.D2"]), 1:5]
      colnames(MissingTaxaD3D2)[-(1:2)] <- c("N", "N2", "Max")
      if (nrow(MissingTaxaD3D2) > 0) {
        openxlsx::writeData(wb, nm, startCol = 1, startRow = startRow+1, x=paste("The following taxa do not have", x$Metric, "indicator values in the DARLEQ2 and DARLEQ3 database:"))
        openxlsx::writeDataTable(wb, nm, startCol = 1, startRow=startRow+3, x=MissingTaxaD3D2, withFilter=FALSE, keepNA=FALSE)
        startRow <- startRow + 4 + nrow(MissingTaxaD3D2)
      }
      MissingTaxaD3 <- MissingTaxa[!is.na(MissingTaxa[, "N.D3"]) & is.na(MissingTaxa[, "N.D2"]), 1:5]
      colnames(MissingTaxaD3)[-(1:2)] <- c("N", "N2", "Max")
      if (nrow(MissingTaxaD3) > 0) {
        openxlsx::writeData(wb, nm, startCol = 1, startRow = startRow+1, x=paste("The following taxa have", x$Metric, "indicator values in the DARLEQ2 but not DARLEQ3 database:"))
        openxlsx::writeDataTable(wb, nm, startCol = 1, startRow=startRow+2, x=MissingTaxaD3, withFilter=FALSE, keepNA=FALSE)
        startRow <- startRow + 4 + nrow(MissingTaxaD3)
      }
      MissingTaxaD2 <- MissingTaxa[is.na(MissingTaxa[, "N.D3"]) & !is.na(MissingTaxa[, "N.D2"]), c(1, 2, 6:8)]
      colnames(MissingTaxaD2)[-(1:2)] <- c("N", "N2", "Max")
      if (nrow(MissingTaxaD2) > 0) {
        openxlsx::writeData(wb, nm, startCol = 1, startRow = startRow+1, x=paste("The following taxa have", x$Metric, "indicator values in the DARLEQ3 but not DARLEQ2 database:"))
        openxlsx::writeDataTable(wb, nm, startCol = 1, startRow=startRow+2, x=MissingTaxaD2, withFilter=FALSE, keepNA=FALSE)
      }
    }
  }
  wb <- openxlsx::createWorkbook("Temp")
  nms <- names(d)
  cf <- openxlsx::createStyle(numFmt = "0.00")
  for (i in 1:length(nms)) {
    if (!is.null(d[[i]]$Job_Summary)) {
      save_Job(d[[i]], nms[i], fn, sheet, wb)
    }
    if (!is.null(d[[i]]$EQR)) {
      nm <- paste0(nms[i], "_Sample Summary")
      openxlsx::addWorksheet(wb, nm)
      mt <- match("Total_count", colnames(d[[i]]$EQR))
      openxlsx::writeDataTable(wb, nm, d[[i]]$EQR, withFilter=FALSE, keepNA=FALSE)
      if (!is.na(mt[1])) {
        cc <- c(1, 3:7, 9:12)
        openxlsx::addStyle(wb, sheet=nm, cf, cols=mt+cc, rows=2:(1+nrow(d[[i]]$EQR)), gridExpand=TRUE)
        mt <- match(c("TDI4_D2_Sum", "TDI3_D2_Sum", "LTDI1_D2_Sum", "LTDI2_D2_Sum"), colnames(d[[i]]$EQR))
        mt <- na.omit(mt)
        if (length(mt) > 0) {
           openxlsx::addStyle(wb, sheet=nm, cf, cols=mt+1:3, rows=2:(1+nrow(d[[i]]$EQR)), gridExpand=TRUE)
        }
      }
    }
    if (!is.null(d[[i]]$Uncertainty)) {
      nm <- paste0(nms[i], "_Uncertainty")
      openxlsx::addWorksheet(wb, nm)
      openxlsx::writeDataTable(wb, nm, d[[i]]$Uncertainty, withFilter=FALSE, keepNA=FALSE)
      openxlsx::addStyle(wb, sheet=nm, cf, cols=3:13, rows=2:(1+nrow(d[[i]]$Uncertainty)), gridExpand=TRUE)
    }
  }
  openxlsx::saveWorkbook(wb, outFile, overwrite=TRUE)
}
