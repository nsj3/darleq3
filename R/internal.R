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
  require(mgcv, quietly=TRUE)
  Predict.matrix(mod$sm, data.frame(x = newdata)) %*% mod$p
}

.errMessage <- function(txt, verbose) {
  if (verbose)
    stop(txt, call.=FALSE)
  else
    simpleError(txt)
}

