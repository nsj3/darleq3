calc_EQR <- function(x) {
  TDI.codes <- darleq3_data$TDI.codes
  WFD.classes <- c("High", "Good", "Moderate", "Poor", "Bad")
  if (!any(class(x) %in% "DiatomMetric")) {
    simpleError("Input is not of class DiatomMetric")
  }
  metric <- TDI.codes[TDI.codes %in% class(x)]
  metric2 <- substring(metric, 1, 3)
  comments <- data.frame(SampleId=x$header$SampleID)
  comments$comment <- ""
  ddd <- darleq3_data$defaults
  if (metric2 == "TDI") {
    minAlk <- switch(metric, TDI3=ddd$minAlkTDI3, TDI4=ddd$minAlkTDI4, TDI5LM=ddd$minAlkTDI5LM, TDI5NGS=ddd$minAlkTDI5NGS)
    maxAlk <- switch(metric, TDI3=ddd$maxAlkTDI3, TDI4=ddd$maxAlkTDI4, TDI5LM=ddd$maxAlkTDI5LM, TDI5NGS=ddd$minAlkTDI5NGS)
    boundaries <- switch(metric, TDI3=ddd$boundariesTDI3, TDI4=ddd$boundariesTDI4, TDI5LM=ddd$boundariesTDI5LM, TDI5NGS=ddd$boundariesTDI5NGS)
    if (!("ALKALINITY" %in% toupper(colnames(x$header))))
      x$header$ALKALINITY <- NA
    if (!("SAMPLE_DATE" %in% toupper(colnames(x$header))))
      x$header$SAMPLE_DATE <- NA
    env.vars <- c("ALKALINITY", "SAMPLE_DATE")
    mt <- match(env.vars, toupper(colnames(x$header)))
    env <- x$header[, mt]
    colnames(env) <- env.vars
    comments$missingAlk <- is.na(env$ALKALINITY)
    env$ALKALINITY[comments$missingAlk] <- ddd$defaultAlkalinity
    comments$minAlk <- env$ALKALINITY < minAlk
    env$ALKALINITY[comments$minAlk] <- minAlk
    comments$maxAlk <- env$ALKALINITY > maxAlk
    env$ALKALINITY[comments$maxAlk] <- maxAlk
    if (metric=="TDI3") {
      comments$missingDate <- is.na(env$SAMPLE_DATE)
      season <- as.numeric(format(env$SAMPLE_DATE, "%m"))
      season[is.na(season)] <- 1
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
    EQR <- (100 - x$metric) / (100 - eTDI)
    EQR <- ifelse(EQR[, 1] > 1.25, 1.25, EQR[, 1])
    class <- cut(EQR, c(10, boundaries, 0), labels=rev(WFD.classes))

    cut(EQR, c(10, boundaries, 0))

  } else if (metric2 == "LTD") {
    HA_boundaries <- switch(metric, LTDI1=ddd$boundariesLTDI1_HA, LTDI2=ddd$boundariesLTDI2_HA)
    MA_boundaries <- switch(metric, LTDI1=ddd$boundariesLTDI1_MA, LTDI2=ddd$boundariesLTDI2_MA)
    LA_boundaries <- switch(metric, LTDI1=ddd$boundariesLTDI1_LA, LTDI2=ddd$boundariesLTDI2_LA)
    medians <- switch(metric, LTDI1=ddd$medianTDI_LTDI1, LTDI2=ddd$medianTDI_LTDI2)
    if (!("TYPE" %in% toupper(colnames(x$header))))
      x$header$Type <- NA
# check for unknown lake types

    env.vars <- c("TYPE")
    mt <- match(env.vars, toupper(colnames(x$header)))
    env <- x$header[, mt, drop=FALSE]
    colnames(env) <- env.vars
    comments$missingType <- is.na(env$TYPE)
    eTDI <- apply(env[, "TYPE", drop=FALSE], 1, function(x) switch(x, HA=medians[1], MA=medians[2], LA=medians[3]))
    EQR <- (100 - x$metric) / (100 - eTDI)
    class <- vector(mode="character", length=nrow(EQR))
    class[env$TYPE=="HA"] <- as.character(cut(EQR[env$TYPE=="HA", 1], c(10, HA_boundaries, 0), labels=rev(WFD.classes)))
    class[env$TYPE=="MA"] <- as.character(cut(EQR[env$TYPE=="MA", 1], c(10, MA_boundaries, 0), labels=rev(WFD.classes)))
    class[env$TYPE=="LA"] <- as.character(cut(EQR[env$TYPE=="LA", 1], c(10, LA_boundaries, 0), labels=rev(WFD.classes)))

  } else if (metric2 == "DAM") {
    minCa <- ddd$minCa
    maxCa <- ddd$maxCa
    minDOC <- ddd$minDOC
    maxDOC <- ddd$maxDOC
    boundaries <- ddd$boundariesDAM
    if (!("CALCIUM" %in% toupper(colnames(x$header))))
      x$header$Calcium <- NA
    if (!("DOC" %in% toupper(colnames(x$header))))
      x$header$DOC <- NA
    env.vars <- c("CALCIUM", "DOC")
    mt <- match(env.vars, toupper(colnames(x$header)))
    env <- x$header[, mt]
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
    eTDI = -5.5 + 33 * log10(env$CALCIUM) - 1.9 * env$DOC;
    EQR <- x$metric / eTDI
    class <- cut(EQR[, 1], c(10, boundaries, 0), labels=rev(WFD.classes))
  }

  res2 <- data.frame(eTDI, EQR, class)
  colnames(res2) <- paste0(c("e", "EQR_", "Class_"), metric)
  res <- data.frame(x$header, x$Summary, x$metric, res2, x$EcolGroup, comments[, -1])
  res
}


