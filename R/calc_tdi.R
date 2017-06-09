calc_N_N2_Max <- function(x) {
  suppressPackageStartupMessages(require(vegan, quietly=TRUE))
  N <- apply(x>0, 2, sum)
  N2 <- renyi(t(x), scales=c(0, 2), hill=TRUE)
  mx <- apply(x, 2, max)
  res <- cbind(N2, max=mx)
  colnames(res) <- c("N", "N2", "Max")
  res
}

calc_TDI <- function(x, TDI.code, dict=darleq3_taxa) {
  wm <- function(w, x) {
    weighted.mean(x, w, na.rm=TRUE)
  }
  TDI.codes <- darleq3_data$TDI.codes
  method <- pmatch(TDI.code, TDI.codes)
  if(is.na(method))
    stop("Invalid diatom metric")
  if(method == -1)
    stop("Ambiguous diatom metric")

  code <- "NBSCode"
  if(!(class(x)=="DARLEQ_DATA"))
    simpleError("Data not of class DARLEQ_DATA in CalcTDILM")
  if(!(class(x)=="NBSCode"))
    code <- "TaxonId"
  diat <- x$diatom_data
  totals <- rowSums(diat)
  diat.pc <- diat / totals * 100
  nms <- colnames(diat)
  mt <- match(nms, dict[, code])

  tdi.sp.all <- dict[na.omit(mt), TDI.code]
  names(tdi.sp.all) <- dict[na.omit(mt), code]
  tdi.sp <- na.omit(tdi.sp.all)
  tdi.sp.nms <- names(tdi.sp)
  diat.pc2 <- diat.pc[, tdi.sp.nms]

  planktic <- as.logical(dict[dict[, code] %in% names(tdi.sp.all), "Planktic"])
  saline <- as.logical(dict[dict[, code] %in% names(tdi.sp.all), "Saline"])
  motile <- as.logical(dict[dict[, code] %in% names(tdi.sp.all), "Motile"])
  organic <- as.logical(dict[dict[, code] %in% names(tdi.sp.all), "OrganicTolerant"])
  planktic[is.na(planktic)] <- FALSE
  saline[is.na(saline)] <- FALSE
  motile[is.na(motile)] <- FALSE
  organic[is.na(organic)] <- FALSE

  pc.planktic <- rowSums(diat.pc[, planktic])
  pc.saline <- rowSums(diat.pc[, saline])
  pc.motile <- rowSums(diat.pc[, motile])
  pc.organic <- rowSums(diat.pc[, organic])

  tdi.sam <- apply(diat.pc2, 1, wm, x=tdi.sp)
  tdi.sam <- (tdi.sam * 25) - 25

  rSum <- rowSums(diat.pc2)
  res <- list()
  res$metric <- data.frame(metric=tdi.sam)
  colnames(res$metric) <- TDI.code
  res$Summary <- data.frame(Total.count=totals, total.TDI=rSum, calc_N_N2_Max(t(diat.pc2)))
  colnames(res$Summary) <- c(paste0(c("Total_count_", "Percent_in_", "N_", "N2_", "Max_"), TDI.code))
  res$EcolGroup <- data.frame(Motile=pc.motile, OrganicTolerant=pc.organic, Planktic=pc.planktic, Saline=pc.saline)
  res$header <- x$header
  class(res) <- c("DiatomMetric", TDI.code)
  res
}


CalcTDI5NGS <- function(diat, dict, TDI5LM=NULL, code="inv", mono.ds=TRUE) {
  wm <- function(w, x) {
    weighted.mean(x, w, na.rm=TRUE)
  }
  nms <- colnames(diat)
  mt <- match(nms, rownames(dict))
  tdi <- dict[mt, code]
  tdi5 <- apply(diat, 1, wm, x=tdi)
  tdi5 <- (tdi5 * 25) - 25
  if (!is.null(TDI5LM)) {
    if (mono.ds) {
      mod <- rioja::.mono.fit(tdi5, TDI5LM)
      tdi5 <- rioja::.mono.predict(mod, tdi5)
    } else {
      mod <- glm(TDI5LM/100 ~ tdi5, family=quasibinomial(link = "logit"))
      tdi5 <- predict(mod, type="response") * 100
    }
  }
  summ <-  matrix(as.numeric(!is.na(tdi)), nrow=1) %*% t(diat)
  data.frame(TDI5NGS=tdi5, total.TDI5NGS=summ[1, ])
}

