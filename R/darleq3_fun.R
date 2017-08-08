#' Calculate diatom water quality metrics, EQRs and WFD class for samples, and uncertainties of site classification
#'
#' @param inFile Excel file name containing diatom and sample environmental data. See \code{\link{read_DARLEQ}} for acceptible formats for these data.
#' @param  sheet name of the worksheet in the Excel file to import.
#' @param metrics character vector of metric codes.  Currently one or more of the following: "TDI3", "TDI4", "TDI5LM" (for river LM TDI calculations), "TDI5NGS" for river NGS metric, "LTDI1", "LTDI2" for lake LM TDI metric or "DAMLM" for river diatom acidification metric.
#' @param outFile name of Excel file to save results.  If not given the function will generate a name by concatenating "DARLEQ3_Results_" with the original filename, the sheet name and the current date.
#' @param verbose logical to indicate should function stop immediately on error (TRUE) or return a \code{simpleError} (FALSE).  Defaults to TRUE.
#'
#' @details This is a wrapper function to \code{\link{read_DARLEQ}} \code{\link{calc_Metric_EQR}} and \code{\link{save_DARLEQ}} that imports data form an Excel file, calculates multiple metrics, EQRs and WFD classes and saves the results to another Excel file in one step.
#'
#' @author Steve Juggins \email{Stephen.Juggins@@ncl.ac.uk}
#' @references Kelly, M., S. Juggins, R. Guthrie, S. Pritchard, J. Jamieson, B. Rippey, H. Hirst, and M. Yallop, Assessment of ecological status in UK rivers using diatoms. \emph{Freshwater Biology}, 2008. 403-422.
#' @references Juggins, S., M. Kelly, T. Allott, M. Kelly-Quinn, and D. Monteith, A Water Framework Directive-compatible metric for assessing acidification in UK and Irish rivers using diatoms. \emph{Science of The Total Environment}, 2016. 671-678.
#' @references Bennion, H., M.G. Kelly, S. Juggins, M.L. Yallop, A. Burgess, J. Jamieson, and J. Krokowski, Assessment of ecological status in UK lakes using benthic diatoms. \emph{Freshwater Science}, 2014. 639-654.
#'
#' @examples
#' fn <- system.file("example_datasets/DARLEQ2TestData.xlsx", package="darleq3")
#' darleq(fn, outFile="Results.xlsx")
#'
#' @export darleq
#'

darleq <- function(inFile, sheet=NULL, metrics=c("TDI3", "TDI4", "TDI5LM"), outFile=NULL, verbose=TRUE) {
  metrics2 <- darleq3::darleq3_data$metric.codes
  if (!is.null(metrics)) {
    mt <- metrics %in% metrics2
    if(any(is.na(mt)))
       errMessage("Invalid diatom metric", verbose)
  } else {
    errMessage("metrics missing with no default", verbose)
  }
  d <- tryCatch(read_DARLEQ(inFile, sheet, FALSE), error=function(e) { e })
  if (inherits(d, "error"))
    errMessage(d$message, verbose)

  res <- tryCatch(calc_Metric_EQR(d, metrics, FALSE))
  if (inherits(res, "error"))
    errMessage(res$message, verbose)

  if (is.null(outFile)) {
    tmp <- basename(inFile)
    pth <- dirname(inFile)
    fn <- strsplit(tmp, "\\.")[[1]][1]
    if (is.null(sheet))
      sheet <- d$sheet
    fn <- paste0("DARLEQ3_Results_", fn, "_", sheet, "_", Sys.Date(), ".xlsx")
    outFile <- gsub(" ", "_", outFile)
  }
  retval <- tryCatch(save_DARLEQ(res, outFile, fn=inFile, sheet=sheet, FALSE))
  if (inherits(retval, "error"))
    errMessage(retval$message, verbose)
}

