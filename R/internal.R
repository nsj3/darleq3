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
  mgcv::Predict.matrix(mod$sm, data.frame(x = newdata)) %*% mod$p
}

.errMessage <- function(txt, verbose) {
  if (verbose)
    stop(txt, call.=FALSE)
  else
    simpleError(txt)
}

