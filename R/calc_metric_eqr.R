#' Calculate diatom water quality metrics, EQRs and WFD class for samples, and uncertainties of site classification
#'
#' @param x list containing diatom and header (environmental) data. This will usually be the data structure imported by \code{\link{read_DARLEQ}}.
#' @param  metrics character vector of metric codes.  Currently one or more of the following: "TDI3", "TDI4", "TDI5LM" (for river LM TDI calculations), "TDI5NGS" for river NGS metric, "LTDI1", "LTDI2" for lake LM TDI metric or "DAMLM" for river diatom acidification metric.
#' @param verbose logical to indicate should function stop immediately on error (TRUE) or return a \code{simpleError} (FALSE).  Defaults to TRUE.
#'
#' @details This is a wrapper function to \code{\link{calc_Metric}} and \code{\link{calc_EQR}} that calculates multiple metrics, EQRs and WFD classes.  The output can be saved to an Excel file using function \code{\link{save_DARLEQ}}.
#'
#' @return A list with a named element for each metric calculated. Each element in the list is itself a list containing the output from \code{\link{calc_EQR}} (ie. the sample and site metrics, EQRs and WFD classes), and the job summary produced by \code{\link{calc_Metric}}.
#'
#' @author Steve Juggins \email{Stephen.Juggins@@ncl.ac.uk}
#' @references Kelly, M., S. Juggins, R. Guthrie, S. Pritchard, J. Jamieson, B. Rippey, H. Hirst, and M. Yallop, Assessment of ecological status in UK rivers using diatoms. \emph{Freshwater Biology}, 2008. 403-422.
#' @references Juggins, S., M. Kelly, T. Allott, M. Kelly-Quinn, and D. Monteith, A Water Framework Directive-compatible metric for assessing acidification in UK and Irish rivers using diatoms. \emph{Science of The Total Environment}, 2016. 671-678.
#' @references Bennion, H., M.G. Kelly, S. Juggins, M.L. Yallop, A. Burgess, J. Jamieson, and J. Krokowski, Assessment of ecological status in UK lakes using benthic diatoms. \emph{Freshwater Science}, 2014. 639-654.
#'
#' @examples
#' fn <- system.file("extdata/DARLEQ2TestData.xlsx", package="darleq3")
#' d <- read_DARLEQ(fn, "Rivers TDI Test Data")
#' x <- calc_Metric_EQR(d)
#' save_DARLEQ(x, outFile="results.xlsx")
#'
#' @export calc_Metric_EQR
#'

calc_Metric_EQR <- function(x, metrics="TDI5LM", verbose=TRUE) {
  metrics2 <- darleq3::darleq3_data$metric.codes
  if (!is.null(metrics)) {
    mt <- metrics %in% metrics2
    if(any(is.na(mt)))
      errMessage("Invalid diatom metric")
  } else {
    errMessage("metrics missing with no default", verbose)
  }
  res <- vector("list", length=length(metrics))

  for (i in 1:length(metrics)) {
    x.tdi <- tryCatch(calc_Metric(x$diatom_data, metrics[i], verbose=FALSE), error=function(e) { e } )
    if (inherits(x.tdi, "error"))
        errMessage(x.tdi$message, verbose)
    res[[i]] <- calc_EQR(x.tdi, x$header, verbose=FALSE)
    res[[i]]$Job_Summary <- x.tdi$Job_Summary
    if (!is.null(x.tdi$warnings)) {
      if (is.null(res[[i]]$warnings)) {
         res[[i]]$warnings <- x.tdi$warnings
      } else {
         res[[i]]$warnings <- paste0(res[[i]]$warnings, "\n", x.tdi$warnings)
      }
    }
    if (verbose & !is.null(res[[i]]$warnings))
      warning(res[[i]]$warnings, call.=FALSE)
  }
  names(res) <- metrics
  class(res) <- "DARLEQ3_EQR"
  res
}

