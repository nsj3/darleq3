#' error message handler
#' @param txt error message
#' @param verbose logical to indicate should function stop immediately on error (TRUE) or return a \code{simpleError} (FALSE).  Defaults to TRUE.
#'
#' @details This is an internal function and is not meant to be called directly.

errMessage <- function(txt, verbose) {
  if (verbose)
    stop(txt, call.=FALSE)
  else
    simpleError(txt)
}

#' Calculate WFD quality class
#' @param EQR sample EQR
#' @param metric metric to calculate
#' @param lake_Type lake type following GB lake typology
#'
#' @details This is an internal function and is not meant to be called directly.

calc_WFDClass <- function(EQR, metric, lake_Type=NA) {
  ddd <- darleq3::darleq3_data$defaults
  WFD.classes <- c("High", "Good", "Moderate", "Poor", "Bad")
  metric2 <- substring(metric, 1, 3)
  if (metric2 == "TDI") {
    boundaries <- switch(metric, TDI3=ddd$boundariesTDI3, TDI4=ddd$boundariesTDI4, TDI5LM=ddd$boundariesTDI5LM, TDI5NGS=ddd$boundariesTDI5NGS)
    class <- cut(EQR, c(10, boundaries, 0), labels=rev(WFD.classes), right=FALSE)
  } else if (metric2=="LTD") {
    HA_boundaries <- switch(metric, LTDI1=ddd$boundariesLTDI1_HA, LTDI2=ddd$boundariesLTDI2_HA)
    MA_boundaries <- switch(metric, LTDI1=ddd$boundariesLTDI1_MA, LTDI2=ddd$boundariesLTDI2_MA)
    LA_boundaries <- switch(metric, LTDI1=ddd$boundariesLTDI1_LA, LTDI2=ddd$boundariesLTDI2_LA)
    class <- vector(mode="character", length=length(EQR))
    class[lake_Type=="HA"] <- as.character(cut(EQR[lake_Type=="HA"], c(10, HA_boundaries, 0), labels=rev(WFD.classes), right=FALSE))
    class[lake_Type=="MA"] <- as.character(cut(EQR[lake_Type=="MA"], c(10, MA_boundaries, 0), labels=rev(WFD.classes), right=FALSE))
    class[lake_Type=="LA"] <- as.character(cut(EQR[lake_Type=="LA"], c(10, LA_boundaries, 0), labels=rev(WFD.classes), right=FALSE))
  } else if (metric2=="DAM") {
    boundaries <- ddd$boundariesDAM
    class <- cut(EQR, c(10, boundaries, 0), labels=rev(WFD.classes), right=FALSE)
  }
  class
}

#' Calculate site EQRs
#' @param EQR sample EQR
#' @param SiteID site ID
#' @param lake_Type lake type following GB lake typology
#'
#' @details This is an internal function and is not meant to be called directly.

calc_SiteEQR <- function(EQR, SiteID, lake_Type=NULL) {
  mean_EQR <- stats::aggregate(EQR, list(SiteID=SiteID), function(x) round(mean(x, na.rm=TRUE), 5) )
  N_EQR <- stats::aggregate(EQR, list(SiteID=SiteID), function(x) sum(!is.na(x)) )
  res <- data.frame(SiteID=N_EQR[, 1], N=N_EQR[, 2], EQR=mean_EQR[, 2])
  if (!is.null(lake_Type)) {
    Type2 <- stats::aggregate(lake_Type, list(SiteID=SiteID), function(x) return(x[1]) )
    res$lake_TYPE <- Type2[, 2]
  }
  SiteID2 <- unique(SiteID)
  mt <- match(SiteID2, res$SiteID)
  res[mt, ]
}

#' Calculate classification uncertainties
#' @param x data frame outfdrom from function \code{\link{calc_SiteEQR}}
#' @param metric diatom metric
#' @param lake_Type lake type following GB lake typology
#'
#' @details This is an internal function and is not meant to be called directly.

calc_Uncertainty <- function(x, metric, lake_Type=NULL) {
  ddd <- darleq3::darleq3_data$defaults
  if (substring(metric, 1, 3) == "LTD") {
    HA_boundaries <- switch(metric, LTDI1=ddd$boundariesLTDI1_HA, LTDI2=ddd$boundariesLTDI2_HA)
    MA_boundaries <- switch(metric, LTDI1=ddd$boundariesLTDI1_MA, LTDI2=ddd$boundariesLTDI2_MA)
    LA_boundaries <- switch(metric, LTDI1=ddd$boundariesLTDI1_LA, LTDI2=ddd$boundariesLTDI2_LA)
    nsam <- length(x$EQR)
    HG <- rep(HA_boundaries["HG"], nsam); HG[lake_Type=="MA"] <- MA_boundaries["HG"]; HG[lake_Type=="LA"] <- LA_boundaries["HG"]
    GM <- rep(HA_boundaries["GM"], nsam); GM[lake_Type=="MA"] <- MA_boundaries["GM"]; GM[lake_Type=="LA"] <- LA_boundaries["GM"]
    MP <- rep(HA_boundaries["MP"], nsam); MP[lake_Type=="MA"] <- MA_boundaries["MP"]; MP[lake_Type=="LA"] <- LA_boundaries["MP"]
    PB <- rep(HA_boundaries["PB"], nsam); PB[lake_Type=="MA"] <- MA_boundaries["PB"]; PB[lake_Type=="LA"] <- LA_boundaries["PB"]
    A0 <- ddd$CoC_LTDI["A0"]
    B1 <- ddd$CoC_LTDI["B1"]
    B2 <- ddd$CoC_LTDI["B2"]
    Power <- ddd$CoC_LTDI["Power"]
  } else {
    boundaries <- switch(metric, TDI3=ddd$boundariesTDI3, TDI4=ddd$boundariesTDI4, TDI5LM=ddd$boundariesTDI5LM, TDI5NGS=ddd$boundariesTDI5NGS)
    HG <- boundaries["HG"]
    GM <- boundaries["GM"]
    MP <- boundaries["MP"]
    PB <- boundaries["PB"]
    A0 <- ddd$CoC_TDI["A0"]
    B1 <- ddd$CoC_TDI["B1"]
    B2 <- ddd$CoC_TDI["B2"]
    Power <- ddd$CoC_TDI["Power"]
  }
  EQR <- x$EQR
  EQR[EQR > 0.999] <- 0.999
  nSam <- x$N
  stdev <- (A0 + B1*EQR + B2 * (EQR^Power)) / sqrt(nSam)
  TMean <- log(EQR / (1-EQR))
  Tstdev <- stdev/(EQR * (1-EQR))
  if (substring(metric, 1, 3) == "LTD") {
    Tstdev[Tstdev > 1.5] <- 1.5
  } else {
    Tstdev[Tstdev > 1.0] <- 1.0
  }
  tHG <- log(HG/(1-HG))
  tGM <- log(GM/(1-GM))
  tMP <- log(MP/(1-MP))
  tPB <- log(PB/(1-PB))
  N1 <- stats::pnorm((tPB-TMean) / Tstdev)
  N2 <- stats::pnorm((tMP-TMean) / Tstdev)
  N3 <- stats::pnorm((tGM-TMean) / Tstdev)
  N4 <- stats::pnorm((tHG-TMean) / Tstdev)
  CoCB <- 100 * N1
  CoCP <- 100 * (N2 - N1)
  CoCM <- 100 * (N3 - N2)
  CoCG <- 100 * (N4 - N3)
  CoCH <- 100 * (1 - N4)
  ROM <- 100 - CoCH
  sel <- is.na(EQR)
  EQR[sel] <- 0
  ROM[EQR <= PB] <- 100 - CoCB[EQR <= PB]
  ROM[EQR <= MP] <- 100 - CoCP[EQR <= MP]
  ROM[EQR <= GM] <- 100 - CoCM[EQR <= GM]
  ROM[EQR <= HG] <- 100 - CoCG[EQR <= HG]
  CoCHG <- CoCH + CoCG
  CoCMPB <- CoCP + CoCB + CoCM
  ROM_GM <- 100-CoCHG
  ROM_GM[EQR <= GM] <- 100-CoCMPB[EQR <= GM]
  ROM[sel]  <- NA
  ROM_GM[sel] <- NA
  round(data.frame(CoCH=CoCH, CoCG=CoCG, CoCM=CoCM, CoCP=CoCP, CoCB=CoCB, ROM=ROM, CoCHG=CoCHG, CoCMPB=CoCMPB, ROM_GM=ROM_GM), 2)
}
