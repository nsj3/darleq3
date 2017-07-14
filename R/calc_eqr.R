#' Calculate EQRs and WFD status classes from diatom metric and sample environmental data
#'
#' @param x An object of class \code{DIATOM_METRIC}, usually the output from function \code{\link{calc_Metric}}.
#' @param header Data frame containing sample and site environmental information for claculating the expected value of the metric.
#' @return A object of class \code{DIATOM_EQR}, a list with the following named elements:
#'
#' @author Steve Juggins \email{Stephen.Juggins@@ncl.ac.uk}

calc_EQR <- function(x, header) {
  metric.codes <- darleq3::darleq3_data$metric.codes
  if (!inherits(x, "DIATOM_METRIC")) {
    simpleError("Input is not of class DIATOM_METRIC")
  }

  if (nrow(x$Metric) != nrow(header))
    simpleError("Diatom and environmental data have different number of samples")
  metric <- x$Metric_Name
  method <- match(metric, metric.codes)
  if(is.na(method))
    stop("Invalid diatom metric")
  metric2 <- substring(metric, 1, 3)
  comments <- data.frame(SampleId=header[, 1])
  comments$comment <- ""
  ddd <- darleq3::darleq3_data$defaults

  SiteID <- NULL
  mt <- grep("SITE", toupper(colnames(header)))
  if (length(mt)>0)
    SiteID <- header[, mt[1]]

  if (metric2 == "TDI") {
    minAlk <- switch(metric, TDI3=ddd$minAlkTDI3, TDI4=ddd$minAlkTDI4, TDI5LM=ddd$minAlkTDI5LM, TDI5NGS=ddd$minAlkTDI5NGS)
    maxAlk <- switch(metric, TDI3=ddd$maxAlkTDI3, TDI4=ddd$maxAlkTDI4, TDI5LM=ddd$maxAlkTDI5LM, TDI5NGS=ddd$maxAlkTDI5NGS)
    mult_Factor <- ddd$TDI_Norm_Factor[metric]
    if (!("ALKALINITY" %in% toupper(colnames(header))))
      header$ALKALINITY <- NA
    if (!("SAMPLE_DATE" %in% toupper(colnames(header))))
      header$SAMPLE_DATE <- NA
    env.vars <- c("ALKALINITY", "SAMPLE_DATE")
    mt <- match(env.vars, toupper(colnames(header)))
    env <- header[, mt]
    colnames(env) <- env.vars
    comments$missingAlk <- is.na(env$ALKALINITY)
    env$ALKALINITY[comments$missingAlk] <- ddd$defaultAlkalinity
    comments$minAlk <- env$ALKALINITY < minAlk
    env$ALKALINITY[comments$minAlk] <- minAlk
    comments$maxAlk <- env$ALKALINITY > maxAlk
    env$ALKALINITY[comments$maxAlk] <- maxAlk
    comments[comments$missingAlk, 2] <- paste0(comments[comments$missingAlk, 2], "Missing alkalinity, value set to ", ddd$defaultAlkalinity)
    comments[comments$minAlk, 2] <- paste0(comments[comments$minAlk, 2], "Alkalinity < ", minAlk, ", value set to ", minAlk)
    comments[comments$maxAlk, 2] <- paste0(comments[comments$maxAlk, 2], "Alkalinity > ", maxAlk, ", value set to ", maxAlk)
    if (metric=="TDI3") {
      comments$missingDate <- is.na(env$SAMPLE_DATE)
      SAMPLE_DATE <- rep(as.Date("01/01/2000", format="%d/%m/%Y"), nrow(env))
      SAMPLE_DATE[!is.na(env$SAMPLE_DATE)] <- na.omit(env$SAMPLE_DATE)
      season <- as.numeric(format(SAMPLE_DATE, "%m"))
      season <- ifelse(season > 6, 1, 0)
      lAlk <- log10(env$ALKALINITY)
      eTDI = -25.36 + 56.83 * lAlk - 12.96 * lAlk^2 + 3.21 * season
    } else if (metric=="TDI4") {
      lAlk <- log10(env$ALKALINITY)
      eTDI = 9.933 * exp(lAlk * 0.81)
    } else if (metric=="TDI5LM") {
      lAlk <- log10(env$ALKALINITY)
      eTDI = 9.933 * exp(lAlk * 0.81)
    } else if (metric=="TDI5NGS") {
      lAlk <- log10(env$ALKALINITY)
      eTDI = 9.933 * exp(lAlk * 0.81)
    }
    EQR <- (100 - x$Metric) / (100 - eTDI) * mult_Factor
    EQR[EQR > 1.0] <- 1.0
    class <- calc_WFDClass(EQR[, 1], metric)
    if (!is.null(SiteID)) {
      mean_EQR <- calc_SiteEQR(EQR[, 1], SiteID)
      class.site <- calc_WFDClass(mean_EQR$EQR, metric)
      uncert <- calc_Uncertainty(mean_EQR, metric)
    }

  } else if (metric2 == "LTD") {
    medians <- switch(metric, LTDI1=ddd$medianTDI_LTDI1, LTDI2=ddd$medianTDI_LTDI2)
    mt <- grep("TYPE", toupper(colnames(header)))
    if (length(mt)==0)
      header$TYPE <- NA
    mt <- grep("TYPE", toupper(colnames(header)))
    env <- header[, mt[1], drop=FALSE]
    colnames(env) <- "TYPE"
    comments$missingType <- is.na(env$TYPE)
    comments[comments$missingType, 2] <- paste0(comments[comments$missingType, 2], "Missing lake Type, value set to ", ddd$defaultsLakeType)

    env$TYPE[is.na(env$TYPE)] <- ddd$defaultLakeType
    eTDI <- apply(env[, "TYPE", drop=FALSE], 1, function(x) switch(x, HA=medians[1], MA=medians[2], LA=medians[3]))
    EQR <- (100 - x$Metric) / (100 - eTDI)
    EQR[EQR > 1.0] <- 1.0
    class <- calc_WFDClass(EQR[, 1], metric, env$TYPE)
    if (!is.null(SiteID)) {
      mean_EQR <- calc_SiteEQR(EQR, SiteID, env$TYPE)
      class.site <- calc_WFDClass(mean_EQR$EQR, metric, mean_EQR$TYPE)
      uncert <- calc_Uncertainty(mean_EQR, metric, mean_EQR$TYPE)
    }
  } else if (metric2 == "DAM") {
    minCa <- ddd$minCa
    maxCa <- ddd$maxCa
    minDOC <- ddd$minDOC
    maxDOC <- ddd$maxDOC
    if (!("CALCIUM" %in% toupper(colnames(header))))
      header$Calcium <- NA
    if (!("DOC" %in% toupper(colnames(header))))
      header$DOC <- NA
    env.vars <- c("CALCIUM", "DOC")
    mt <- match(env.vars, toupper(colnames(header)))
    env <- header[, mt]
    colnames(env) <- env.vars
    comments$missingCa <- is.na(env$CALCIUM)
    env$CALCIUM[comments$missingCa] <- ddd$defaultCa
    comments$minCa <- env$CALCIUM < minCa
    env$CALCIUM[comments$minCa] <- minCa
    comments$maxCa <- env$CALCIUM > maxCa
    env$CALCIUM[comments$maxCa] <- maxCa
    comments$missingDOC <- is.na(env$DOC)
    env$DOC[comments$missingDOC] <- ddd$defaultDOC
    comments$minDOC <- env$DOC < minDOC
    env$DOC[comments$minDOC] <- minDOC
    comments$maxDOC <- env$DOC > maxDOC
    env$DOC[comments$maxDOC] <- maxDOC
    comments[comments$missingCa, 2] <- paste0(comments[comments$missingCa, 2], "Missing Ca, value set to ", ddd$defaultCa)
    comments[comments$minCa, 2] <- paste0(comments[comments$minCa, 2], "Ca < ", minCa, ", value set to ", minCa)
    comments[comments$maxCa, 2] <- paste0(comments[comments$maxCa, 2], "Ca > ", maxCa, ", value set to ", maxCa)
    comments[comments$missingDOC, 2] <- paste0(comments[comments$missingDOC, 2], "Missing DOC, value set to ", ddd$defaultDOC)
    comments[comments$minDOC, 2] <- paste0(comments[comments$minDOC, 2], "Ca < ", minDOC, ", value set to ", minDOC)
    comments[comments$maxDOC, 2] <- paste0(comments[comments$maxDOC, 2], "Ca > ", maxDOC, ", value set to ", maxDOC)
    eTDI = -5.5 + 33 * log10(env$CALCIUM) - 1.9 * env$DOC;
    EQR <- x$Metric / eTDI
    EQR[EQR > 1.0] <- 1.0
    class <- calc_WFDClass(EQR[, 1], metric)
    if (!is.null(SiteID)) {
      mean_EQR <- calc_SiteEQR(EQR, SiteID)
      class.site <- calc_WFDClass(mean_EQR$EQR, metric)
    }
  }
  res <- list()
  res2 <- data.frame(round(eTDI, 2), round(EQR, 2), class)
  colnames(res2) <- paste0(c("e", "EQR_", "Class_"), metric)
  res$EQR <- data.frame(header, x$Summary, x$Metric, res2, x$EcolGroup, Comments=comments[, 2])
  if (!is.null(SiteID)) {
    if (metric == "DAM") {
      colnames(mean_EQR)[3] <- "mean_EQR"
      res$Uncertainty <- data.frame(mean_EQR, WFDClass=class.site)
    } else {
      res$Uncertainty <- data.frame(mean_EQR, WFDClass=class.site, uncert)
    }
  }
  res$Metric <- metric
  class(res) <- "DARLEQ_EQR"
  res
}

calc_WFDClass <- function(EQR, metric, lake.type=NA) {
  ddd <- darleq3::darleq3_data$defaults
  WFD.classes <- c("High", "Good", "Moderate", "Poor", "Bad")
  metric2 <- substring(metric, 1, 3)
  if (metric2 == "TDI") {
    boundaries <- switch(metric, TDI3=ddd$boundariesTDI3, TDI4=ddd$boundariesTDI4, TDI5LM=ddd$boundariesTDI5LM, TDI5NGS=ddd$boundariesTDI5NGS)
    class <- cut(EQR, c(10, boundaries, 0), labels=rev(WFD.classes))
  } else if (metric2=="LTD") {
    HA_boundaries <- switch(metric, LTDI1=ddd$boundariesLTDI1_HA, LTDI2=ddd$boundariesLTDI2_HA)
    MA_boundaries <- switch(metric, LTDI1=ddd$boundariesLTDI1_MA, LTDI2=ddd$boundariesLTDI2_MA)
    LA_boundaries <- switch(metric, LTDI1=ddd$boundariesLTDI1_LA, LTDI2=ddd$boundariesLTDI2_LA)
    class <- vector(mode="character", length=length(EQR))
    class[lake.type=="HA"] <- as.character(cut(EQR[lake.type=="HA"], c(10, HA_boundaries, 0), labels=rev(WFD.classes)))
    class[lake.type=="MA"] <- as.character(cut(EQR[lake.type=="MA"], c(10, MA_boundaries, 0), labels=rev(WFD.classes)))
    class[lake.type=="LA"] <- as.character(cut(EQR[lake.type=="LA"], c(10, LA_boundaries, 0), labels=rev(WFD.classes)))
  } else if (metric2=="DAM") {
    boundaries <- ddd$boundariesDAM
    class <- cut(EQR, c(10, boundaries, 0), labels=rev(WFD.classes))
  }
  class
}

calc_SiteEQR <- function(EQR, SiteID, Type=NULL) {
  mean_EQR <- aggregate(EQR, list(SiteID=SiteID), function(x) round(mean(x, na.rm=TRUE), 2) )
  N_EQR <- aggregate(EQR, list(SiteID=SiteID), function(x) sum(!is.na(x)) )
  res <- data.frame(SiteID=N_EQR[, 1], N=N_EQR[, 2], EQR=mean_EQR[, 2])
  if (!is.null(Type)) {
    Type2 <- aggregate(Type, list(SiteID=SiteID), function(x) return(x[1]) )
    res$TYPE <- Type2[, 2]
  }
  SiteID2 <- unique(SiteID)
  mt <- match(SiteID2, res$SiteID)
  res[mt, ]
}

#summary.DARLEQ_EQR <- function(x, ...) {
#  cat(paste("No. samples:", nrow(x$EQR), "\n"))
#  if (!is.null(x$Uncertainty)) {
#     cat(paste("No. sites:", ncol(x$Uncertainty), "\n"))
#  } else {
#    cat("Site ID not found, uncertainty not calculated\n")
#  }
#  cat(paste("Metric:", x$metric, "\n"))
#}

calc_Uncertainty <- function(x, metric, lake_Type=NULL) {
  ddd <- darleq3::darleq3_data$defaults
  if (substring(metric, 1, 3) == "LTD") {
    HA_boundaries <- switch(metric, LTDI1=ddd$boundariesLTDI1_HA, LTDI2=ddd$boundariesLTDI2_HA)
    MA_boundaries <- switch(metric, LTDI1=ddd$boundariesLTDI1_MA, LTDI2=ddd$boundariesLTDI2_MA)
    LA_boundaries <- switch(metric, LTDI1=ddd$boundariesLTDI1_LA, LTDI2=ddd$boundariesLTDI2_LA)
    nsam <- length(x$EQR)
    HG <- rep(HA_boundaries["HG"], nsam); HG[lake_Type=="MA"] <- MA_boundaries["HG"]; HG[lake_Type=="LA"] <- LA_boundaries["HG"]
    GM <- rep(HA_boundaries["GM"], nsam); GM[lake_Type=="MA"] <- MA_boundaries["GM"]; GM[lake_Type=="LA"] <- LA_boundaries["GM"]
    MP <- rep(HA_boundaries["HG"], nsam); MP[lake_Type=="MA"] <- MA_boundaries["MP"]; MP[lake_Type=="LA"] <- LA_boundaries["MP"]
    PB <- rep(HA_boundaries["HG"], nsam); PB[lake_Type=="MA"] <- MA_boundaries["PB"]; PB[lake_Type=="LA"] <- LA_boundaries["PB"]
    A0 <- darleq3_data$defaults$CoC_LTDI["A0"]
    B1 <- darleq3_data$defaults$CoC_LTDI["B1"]
    B2 <- darleq3_data$defaults$CoC_LTDI["B2"]
    Power <- darleq3_data$defaults$CoC_LTDI["Power"]
  } else {
    boundaries <- switch(metric, TDI3=ddd$boundariesTDI3, TDI4=ddd$boundariesTDI4, TDI5LM=ddd$boundariesTDI5LM, TDI5NGS=ddd$boundariesTDI5NGS)
    HG <- boundaries["HG"]
    GM <- boundaries["GM"]
    MP <- boundaries["MP"]
    PB <- boundaries["PB"]
    A0 <- darleq3_data$defaults$CoC_TDI["A0"]
    B1 <- darleq3_data$defaults$CoC_TDI["B1"]
    B2 <- darleq3_data$defaults$CoC_TDI["B2"]
    Power <- darleq3_data$defaults$CoC_TDI["Power"]
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
  N1 <- pnorm((tPB-TMean) / Tstdev)
  N2 <- pnorm((tMP-TMean) / Tstdev)
  N3 <- pnorm((tGM-TMean) / Tstdev)
  N4 <- pnorm((tHG-TMean) / Tstdev)
  CoCB <- 100 * N1
  CoCP <- 100 * (N2 - N1)
  CoCM <- 100 * (N3 - N2)
  CoCG <- 100 * (N4 - N3)
  CoCH <- 100 * (1 - N4)
  ROM <- 100 - CoCH
  ROM[EQR <= PB] <- 100 - CoCB[EQR <= PB]
  ROM[EQR <= MP] <- 100 - CoCP[EQR <= MP]
  ROM[EQR <= GM] <- 100 - CoCM[EQR <= GM]
  ROM[EQR <= HG] <- 100 - CoCG[EQR <= HG]
  CoCHG <- CoCH + CoCG
  CoCMPB <- CoCP + CoCB + CoCM
  ROM_GM <- 100-CoCHG
  ROM_GM[EQR <= GM] <- 100-CoCMPB[EQR <= GM]
  round(data.frame(CoCH=CoCH, CoCG=CoCG, CoCM=CoCM, CoCP=CoCP, CoCB=CoCB, ROM=ROM, CoCHG=CoCHG, CoCMPB=CoCMPB, RMO_GM=ROM_GM), 2)
}
