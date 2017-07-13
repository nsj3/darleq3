#' Get taxon coding from diatom data
#' @param x data frame of diatom count or relative abundance data
#' @param dictionary Diatom dictionary, a data frame with diatom taxon codes and indicator values for different metrics.  Defaults to the built-in DARLEQ3 dictionary.
#' @return Column name of dictionary containing the matched taxon codes.  For the DARLEQ3 dictionary this is either "NBSCode" for NBS codes or "TaxonID" for alphanumeric RB codes used in the UCL DIATCODE taxon list.  Returns NULL if column names in \code{x} in do not match any entries in first or second column of the dictionary.
#'
#' @author Steve Juggins \email{Stephen.Juggins@@ncl.ac.uk}
#' @note This is a helper function and is not meant to be called by the user directly.

.get_Taxon_Coding <- function(x, dictionary=darleq3::darleq3_taxa) {
  nms <- colnames(x)
  mt1 <- match(nms, dictionary[, 1])
  nM1 <- sum(!is.na(mt1))
  mt2 <- match(nms, dictionary[, 2])
  nM2 <- sum(!is.na(mt2))
  if (sum(nM1 + nM2) < 1)
    return(NULL)
  else
    return(ifelse(nM1 > nM2, colnames(dictionary)[1], colnames(dictionary)[2]))
}

.mono_predict <- function(mod, newdata)
{
  .Predict.matrix <- function (object, data)
  {
    x <- data[[object$term]]
    if (length(x) < 1)
      stop("no data to predict at")
    nx <- length(x)
    nk <- object$bs.dim
    X <- rep(0, nx * nk)
    S <- 1
    F.supplied <- 1
    if (is.null(object$F))
      stop("F is missing from cr smooth - refit model with current mgcv")
    oo <- .C(C_crspl, x = as.double(x), n = as.integer(nx), xk = as.double(object$xp),
             nk = as.integer(nk), X = as.double(X), S = as.double(S),
             F = as.double(object$F), Fsupplied = as.integer(F.supplied))
    X <- matrix(oo$X, nx, nk)
    X
  }
  .Predict.matrix(mod$sm, data.frame(x = newdata)) %*% mod$p
}

.errMessage <- function(txt, verbose) {
  if (verbose)
    stop(txt, call.=FALSE)
  else
    simpleError(txt)
}

## Function copied Predict.matrix.cr.smooth in package mgcv, for running in shiny app on computer that doesn't have mgcv installed

