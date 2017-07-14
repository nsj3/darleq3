#' Calculate water quality metrics from diatom data
#'
#' @param x Data frame of diatom counts or relative abundance data
#' @param metric Diatom metric, one of "TDI3", "TDI4", "TDI5LM", "TDI5NGS", "LTDI1", "LTDI2", "DAM".
#' @param dictionary Diatom dictionary, a data frame with diatom taxon codes and indicator values for different metrics.  Defaults to the built-in DARLEQ3 dictionary.
#' @return A object of class \code{DIATOM_METRIC}, a list with the following named elements:
#'
#' @author Steve Juggins \email{Stephen.Juggins@@ncl.ac.uk}

calc_Metric <- function(x, metric, dictionary=darleq3::darleq3_taxa, verbose=TRUE) {
  wm <- function(w, x) {
    weighted.mean(x, w, na.rm=TRUE)
  }
  metric.codes <- darleq3::darleq3_data$metric.codes
  method <- match(metric, metric.codes)
  if(is.na(method))
    stop("Invalid diatom metric")
  if(method == -1)
    stop("Ambiguous diatom metric")

  codingID <- .get_Taxon_Coding(x, dictionary)
  if (is.null(codingID)) {
    .errMessage("No taxon codes found, are you sure this is a DARLEQ diatom data file?", verbose)
  }

  totals <- rowSums(x)
  diat.pc <- x / totals * 100
  diat.pc[is.na(diat.pc)] <- 0

  Job_Summary <- list()
  Job_Summary[[1]] <- length(totals)
  Job_Summary[[2]] <- sum(totals > 0)
  Job_Summary[[3]] <- ncol(diat.pc)
  diat.pc <- diat.pc[, colSums(diat.pc) > 0]
  Job_Summary[[4]] <- ncol(diat.pc)
  Job_Summary[[5]] <- metric
  names(Job_Summary) <- c("N_samples", "N_samples_gt_zero", "N_taxa", "N_taxa_gt_zero", "Metric")

  nms <- colnames(diat.pc)
  mt <- match(nms, dictionary[, codingID])

  tdi.sp.all <- dictionary[na.omit(mt), metric]
  names(tdi.sp.all) <- dictionary[na.omit(mt), codingID]
  tdi.sp <- na.omit(tdi.sp.all)
  tdi.sp.nms <- names(tdi.sp)
  diat.pc2 <- diat.pc[, tdi.sp.nms]

  planktic <- as.logical(dictionary[na.omit(mt), "Planktic"])
  saline <- as.logical(dictionary[na.omit(mt), "Saline"])
  motile <- as.logical(dictionary[na.omit(mt), "Motile"])
  organic <- as.logical(dictionary[na.omit(mt), "OrganicTolerant"])
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
     pc.planktic <- rowSums(diat.pc[, planktic_T])
  else
    pc.planktic <- rep(0.0, nsam)
  if (any(saline))
    pc.saline <- rowSums(diat.pc[, saline_T])
  else
    pc.saline <- rep(0.0, nsam)
  if (any(motile))
     pc.motile <- rowSums(diat.pc[, motile_T])
  else
     pc.motile <- rep(0.0, nsam)
  if (any(organic))
    pc.organic <- rowSums(diat.pc[, organic_T])
  else
    pc.organic <- rep(0.0, nsam)

  missingTaxa <- setdiff(nms, tdi.sp.nms)
  if (length(missingTaxa) > 0) {
    tmp <- diat.pc[, missingTaxa]
    err <<- tmp
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
    tdi.sam <- .mono_predict(mono.mod, tdi.sam)
    tdi.sam <- (tdi.sam - ma.coef[1]) / ma.coef[2]
  }

  rSum <- rowSums(diat.pc2)

  res <- list()
  res$Metric_Name <- metric
  res$Metric <- data.frame(Metric=round(tdi.sam, 2))
  colnames(res$Metric) <- metric
  res$Summary <- data.frame(Total.count=round(totals, 2), total.TDI=round(rSum, 2), calc_N_N2_Max(diat.pc2))
  colnames(res$Summary) <- c("Total_count", paste0(c("Percent_in_", "N_", "N2_", "Max_"), metric))
  res$EcolGroup <- round(data.frame(Motile=pc.motile, OrganicTolerant=pc.organic, Planktic=pc.planktic, Saline=pc.saline), 2)
  res$Job_Summary <- Job_Summary
  if (length(missingTaxa)>0) {
    res$Job_Summary$MissingTaxa <- missingTaxonSummary
  }
  class(res) <- c("Diatom_METRIC")
  res
}

