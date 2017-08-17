#' Calculate water quality metrics from diatom data
#'
#' @param x data frame of diatom counts or relative abundance data
#' @param metric diatom metric, one of "TDI3", "TDI4", "TDI5LM", "TDI5NGS", "LTDI1", "LTDI2", or "DAM".  Defaults to "TDI5LM".
#' @param dictionary diatom dictionary, a data frame with diatom taxon codes and indicator values for different metrics.  Defaults to the built-in DARLEQ3 dictionary.
#' @param verbose logical to indicate should function stop immediately on error (TRUE) or return a \code{simpleError} (FALSE).  Defaults to TRUE.
#'
#' @details \code{calc_Metric} takes as arguments a data frame of diatom counts or relative abundances, a metric code and a "dictionary" of diatom metric indicator values. The function will like the diatom taxon codes from the column names in the diatom data to those listed in the dictionary and calculate the relevant metric, along with some useful summary statistics.Diatoms data should be coded with either NBS codes or 6-character DiatCode codes. See \code{\link{darleq3_taxa}} for the current DARLEQ3 dictionary.
#'
#' @return A object of class \code{DIATOM_METRIC}, a list with the following named elements:
#' \item{Metric_Code}{metric code}
#' \item{CodingID}{taxon coding type - the column name containing taxon codes in the taxon dictionary}
#' \item{Metric}{data frame with one column listing the value of the metric for each sample}
#' \item{Summary}{data frame summaring the input data with the following columns:}
#' \itemize{
#' \item{Total_count: total diatom count for each sample}
#' \item{Percent_in_Metric, percentage of count included in metric calculations}
#' \item{N_Metric, Number of taxa included in metric calculations}
#' \item{N2_Metric, Hill's N2 effective number of taxa included in metric calculations}
#' \item{Max_Metric, maximum abundance of any taxon included in metric calculations}
#' }
#' \item{EcolGroup}{data frame containing a list of the percentage of motile, organic tolerant, planktic and saline tolerant taxa in each sample}
#' \item{Job_Summary}{list containing elements giving the total number of samples, number of samples with data, total number of taxa, number of taxa with occurrences, diatom metric and list of taxa that do not have a metric indicator value in the taxon dictionary}
#' \item{TDI4.D2}{for TDI4 caluclations, a data frame containing TDI4 calculated using the DARLEQ2 taxon list, the sum of taxa included in the calculation, and the difference between TDI4 calculated by DARLEQ3 and DARLEQ2 software}
#'
#'
#' @author Steve Juggins \email{Stephen.Juggins@@ncl.ac.uk}
#'
#' @references Kelly, M., S. Juggins, R. Guthrie, S. Pritchard, J. Jamieson, B. Rippey, H. Hirst, and M. Yallop, Assessment of ecological status in UK rivers using diatoms. \emph{Freshwater Biology}, 2008. 403-422.
#' @references Juggins, S., M. Kelly, T. Allott, M. Kelly-Quinn, and D. Monteith, A Water Framework Directive-compatible metric for assessing acidification in UK and Irish rivers using diatoms. \emph{Science of The Total Environment}, 2016. 671-678.
#' @references Bennion, H., M.G. Kelly, S. Juggins, M.L. Yallop, A. Burgess, J. Jamieson, and J. Krokowski, Assessment of ecological status in UK lakes using benthic diatoms. \emph{Freshwater Science}, 2014. 639-654.
#'
#' @examples
#' fn <- system.file("example_datasets/DARLEQ2TestData.xlsx", package="darleq3")
#' d <- read_DARLEQ(fn, "Rivers TDI Test Data")
#' x <- calc_Metric(d$diatom_data, metric="TDI4")
#' head(x$Metric)
#'
#' @export calc_Metric
#'

calc_Metric <- function(x, metric="TDI5LM", dictionary=darleq3::darleq3_taxa, verbose=TRUE) {
  wm <- function(w, x) {
    stats::weighted.mean(x, w, na.rm=TRUE)
  }
  calc_N_N2_Max <- function(x) {
    N <- apply(x>0, 1, sum)
    mx <- apply(x, 1, max)
    x <- sweep(x, 1, rowSums(x), "/")
    N2 <- exp(-log(apply(x^2, 1, sum)))
    res <- cbind(N, round(N2, 2), max=round(mx, 2))
    colnames(res) <- c("N", "N2", "Max")
    res
  }
  get_Taxon_Coding <- function(x, dictionary=darleq3::darleq3_taxa) {
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
  metric.codes <- darleq3::darleq3_data$metric.codes
  method <- match(metric, metric.codes)
  if(is.na(method))
    stop("Invalid diatom metric")
  if(method == -1)
    stop("Ambiguous diatom metric")

  codingID <- get_Taxon_Coding(x, dictionary)
  if (is.null(codingID)) {
    errMessage("No taxon codes found, are you sure this is a DARLEQ diatom data file?", verbose)
  }

  totals <- rowSums(x)
  diat.pc <- x / totals * 100
  diat.pc[is.na(diat.pc)] <- 0

  Job_Summary <- list()
  Job_Summary[[1]] <- length(totals)
  Job_Summary[[2]] <- sum(totals > 0)
  Job_Summary[[3]] <- ncol(diat.pc)
  diat.pc <- diat.pc[, colSums(diat.pc) > 0, drop=FALSE]
  Job_Summary[[4]] <- ncol(diat.pc)
  Job_Summary[[5]] <- metric
  names(Job_Summary) <- c("N_samples", "N_samples_gt_zero", "N_taxa", "N_taxa_gt_zero", "Metric")

  nms <- colnames(diat.pc)
  mt <- match(nms, dictionary[, codingID])

  tdi.sp.all <- dictionary[stats::na.omit(mt), metric]
  names(tdi.sp.all) <- dictionary[stats::na.omit(mt), codingID]
  tdi.sp <- stats::na.omit(tdi.sp.all)
  tdi.sp.nms <- names(tdi.sp)
  diat.pc2 <- diat.pc[, tdi.sp.nms, drop=FALSE]

  planktic <- as.logical(dictionary[stats::na.omit(mt), "Planktic"])
  saline <- as.logical(dictionary[stats::na.omit(mt), "Saline"])
  motile <- as.logical(dictionary[stats::na.omit(mt), "Motile"])
  organic <- as.logical(dictionary[stats::na.omit(mt), "OrganicTolerant"])
  planktic[is.na(planktic)] <- FALSE
  saline[is.na(saline)] <- FALSE
  motile[is.na(motile)] <- FALSE
  organic[is.na(organic)] <- FALSE
  planktic_T <- names(tdi.sp.all)[planktic]
  saline_T <- names(tdi.sp.all)[saline]
  motile_T <- names(tdi.sp.all)[motile]
  organic_T <- names(tdi.sp.all)[organic]
  nsam <- nrow(diat.pc)
  if (any(planktic))
     pc.planktic <- rowSums(diat.pc[, planktic_T, drop=FALSE])
  else
    pc.planktic <- rep(0.0, nsam)
  if (any(saline))
    pc.saline <- rowSums(diat.pc[, saline_T, drop=FALSE])
  else
    pc.saline <- rep(0.0, nsam)
  if (any(motile))
     pc.motile <- rowSums(diat.pc[, motile_T, drop=FALSE])
  else
     pc.motile <- rep(0.0, nsam)
  if (any(organic))
    pc.organic <- rowSums(diat.pc[, organic_T, drop=FALSE])
  else
    pc.organic <- rep(0.0, nsam)

  missingTaxa <- setdiff(nms, tdi.sp.nms)
  if (length(missingTaxa) > 0) {
    tmp <- diat.pc[, missingTaxa]
    tmp2 <- calc_N_N2_Max(t(tmp))
    mt <- match(missingTaxa, dictionary[, codingID])
    names <- dictionary[mt, "TaxonName"]
    missingTaxonSummary <- data.frame(TaxonID=missingTaxa, Name=names, tmp2)
  }

  tdi.sam <- apply(diat.pc2, 1, wm, x=tdi.sp)
  tdi.sam <- (tdi.sam * 25) - 25

  if (metric == "TDI5NGS") {
    mono.mod <- darleq3::darleq3_data$mono.mod
    ma.coef <- darleq3::darleq3_data$ma.coef
    tdi.sam <- mgcv::Predict.matrix(mono.mod$sm, data.frame(x = tdi.sam)) %*% mono.mod$p
    tdi.sam <- (tdi.sam - ma.coef[1]) / ma.coef[2]
  }

  rSum <- rowSums(diat.pc2)

  res <- list()
  res$CodingID <- codingID
  res$Metric_Code <- metric
  res$Metric <- data.frame(Metric=tdi.sam)
  colnames(res$Metric) <- metric
  res$Summary <- data.frame(Total.count=totals, total.TDI=rSum, calc_N_N2_Max(diat.pc2))
  colnames(res$Summary) <- c("Total_count", paste0(c("Percent_in_", "N_", "N2_", "Max_"), metric))
  res$EcolGroup <- round(data.frame(Motile=pc.motile, OrganicTolerant=pc.organic, Planktic=pc.planktic, Saline=pc.saline), 2)
  res$Job_Summary <- Job_Summary

  if (codingID=="NBSCode" & metric=="TDI4") {
    mt <- match(nms, dictionary[, codingID])
    tdi.sp.all2 <- dictionary[stats::na.omit(mt), "TDI4.D2"]
    names(tdi.sp.all2) <- dictionary[stats::na.omit(mt), codingID]
    tdi.sp2 <- stats::na.omit(tdi.sp.all2)
    tdi.sp.nms2 <- names(tdi.sp2)
    diat.pc3 <- diat.pc[, tdi.sp.nms2, drop=FALSE]
    tdi4.D2 <- apply(diat.pc3, 1, wm, x=tdi.sp2)
    tdi4.D2 <- (tdi4.D2 * 25) - 25
    tdi4.D2.rSum <- rowSums(diat.pc3) * totals / 100
    res$TDI4.D2 <- data.frame(TDI4.D2.Sum=tdi4.D2.rSum, TDI4.D2=tdi4.D2, TDI4.Diff=tdi.sam-tdi4.D2)
    nWarn <- sum(abs(res$TDI4.D2$TDI4.Diff)>2.0, na.rm=TRUE)
    if (nWarn > 0) {
      res$warnings <- paste0(nWarn, " samples have a difference of more than 2 units in TDI4 between DARLEQ versions 2 and 3.")
      if (verbose)
        warning(res$warning, call.=FALSE)
    }
  }

  if (length(missingTaxa)>0) {
    res$Job_Summary$MissingTaxa <- missingTaxonSummary
  }
  class(res) <- c("Diatom_METRIC")
  res
}

