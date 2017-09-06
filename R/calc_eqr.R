#' Calculate EQRs and WFD status classes from diatom metric and sample environmental data
#'
#' @param x an object of class \code{DIATOM_METRIC}, usually the output from function \code{\link{calc_Metric}}.
#' @param header data frame containing sample and site environmental information for calculating the expected value of the metric.
#' @param verbose logical to indicate should function stop immediately on error (TRUE) or return a \code{simpleError} (FALSE).  Defaults to TRUE.
#' @return A object of class \code{DIATOM_EQR}, a list with the following named elements:
#' \item{EQR}{data frame containing, for each sample, sample codes, water chemistry data and other columns from the header in original Excel file, metric and summary information from function \code{calc_Metric}, expected EQRs (eEQR), calculated EQRs, predicted WFD class, percentage diatoms in diagnostic ecological groups, and a flag to indicate missing or out of range environmental data.}
#' \item{Uncertainty}{data frame containing, for each site, mean EQRS, predicted WFD class, and confidence of class (CoC) for each WFD class and HG/MPB boundary (CoCCHG, COCMPB), and risk of misclassification for the predicted class (ROM) and for the G/M boundary (ROM_GM)}.
#'
#' @author Steve Juggins \email{Stephen.Juggins@@ncl.ac.uk}
#'
#' @references Kelly, M., S. Juggins, R. Guthrie, S. Pritchard, J. Jamieson, B. Rippey, H. Hirst, and M. Yallop, Assessment of ecological status in UK rivers using diatoms. \emph{Freshwater Biology}, 2008. 403-422.
#' @references Juggins, S., M. Kelly, T. Allott, M. Kelly-Quinn, and D. Monteith, A Water Framework Directive-compatible metric for assessing acidification in UK and Irish rivers using diatoms. \emph{Science of The Total Environment}, 2016. 671-678.
#' @references Bennion, H., M.G. Kelly, S. Juggins, M.L. Yallop, A. Burgess, J. Jamieson, and J. Krokowski, Assessment of ecological status in UK lakes using benthic diatoms. \emph{Freshwater Science}, 2014. 639-654.
#'
#' @examples
#' fn <- system.file("extdata/DARLEQ2TestData.xlsx", package="darleq3")
#' d <- read_DARLEQ(fn, "Rivers TDI Test Data")
#' x <- calc_Metric(d$diatom_data, metric="TDI4")
#' eqr <- calc_EQR(x, d$header)
#' head(eqr$EQR)
#' head(eqr$Uncertainty)
#'
#' @export calc_EQR
#'

calc_EQR <- function(x, header, verbose=TRUE) {
  metric.codes <- darleq3::darleq3_data$metric.codes
  if (!inherits(x, "DIATOM_METRIC")) {
    simpleError("Input is not of class DIATOM_METRIC")
  }
  if (nrow(x$Metric) != nrow(header))
    simpleError("Diatom and environmental data have different number of samples")
  metric <- x$Metric_Code
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
      SAMPLE_DATE[!is.na(env$SAMPLE_DATE)] <- stats::na.omit(env$SAMPLE_DATE)
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
    if (length(mt)==0) {
      header$lake_TYPE <- NA
      mt <- grep("lake_TYPE", colnames(header))
    }
    env <- header[, mt[1], drop=FALSE]
    colnames(env) <- "lake_TYPE"
    comments$missingType <- is.na(env$lake_TYPE)
    env$lake_TYPE[is.na(env$lake_TYPE)] <- ddd$defaultLakeType
    comments[comments$missingType, 2] <- paste0(comments[comments$missingType, 2], "Missing lake Type, value set to ", ddd$defaultsLakeType)
    eTDI <- apply(env[, "lake_TYPE", drop=FALSE], 1, function(x) switch(x, HA=medians[1], MA=medians[2], LA=medians[3]))
    EQR <- (100 - x$Metric) / (100 - eTDI)
    EQR[EQR > 1.0] <- 1.0
    class <- calc_WFDClass(EQR[, 1], metric, env$lake_TYPE)
    if (!is.null(SiteID)) {
      mean_EQR <- calc_SiteEQR(EQR, SiteID, env$lake_TYPE)
      class.site <- calc_WFDClass(mean_EQR$EQR, metric, mean_EQR$lake_TYPE)
      uncert <- calc_Uncertainty(mean_EQR, metric, mean_EQR$lake_TYPE)
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
  res2 <- data.frame(eTDI, EQR, class)
  colnames(res2) <- paste0(c("e", "EQR_", "Class_"), metric)
  res$EQR <- data.frame(header, x$Summary, x$Metric, res2, x$EcolGroup, Comments=comments[, 2])
#  if (x$CodingID=="NBSCode" & (metric %in% c("TDI4", "TDI3", "LTDI1", "LTDI2"))) {
#     metID <- paste0(metric, "_D2")
#     if (metric2=="TDI") {
#        EQR2 <- (100 - x$Metric.D2[, metID]) / (100 - eTDI) * mult_Factor
#        EQR2[EQR2 > 1.0] <- 1.0
#        Class2 <- calc_WFDClass(EQR2, metric)
#     } else {
#        EQR2 <- (100 - x$Metric.D2[, metID]) / (100 - eTDI)
#        EQR[EQR > 1.0] <- 1.0
#        Class2 <- calc_WFDClass(EQR2, metric, env$lake_TYPE)
#     }
#     mt <- grep("CLASS", toupper(colnames(res2)))
#     nClass <- sum(res2[, mt] != Class2, na.rm=TRUE)
#     tmp <- data.frame(eqr2=EQR2, class2=Class2, diff=ifelse(res2[, mt] != Class2, "Changed", NA))
#     colnames(tmp) <- c(paste(metric, c("D2_EQR", "D2_Class"), sep="_"), "Class_Diff")
#     res$EQR <- cbind(res$EQR, x$Metric.D2, tmp)
#     if (nClass > 0)
#        res$warnings <- paste0(nClass, " sample(s) have different ", metric,  " classes in DARLEQ versions 2 and 3.")
#        if (verbose)
#          warnings(res$warnings, call.=FALSE)
#  }
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

