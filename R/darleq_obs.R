#' Print an object of class DARLEQ_Data
#'
#' @param x object of class DARLEQ_DATA
#' @param ... additional arguments to \code{print}
#'
#' @method print DARLEQ_DATA
#' @export
print.DARLEQ_DATA <- function(x, ...) {
  cat("File name  :", x$file, "\n")
  cat(paste("Sheet name :", x$sheet, "\n"))
  cat(paste("No. samples:", nrow(x$diatom_data), "\n"))
  cat(paste("No. taxa:", ncol(x$diatom_data), "\n"))
}

