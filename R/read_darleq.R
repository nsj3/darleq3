#' Read diatom data from an Excel file in DARLEQ2 format
#'
#' @param filename NAme of Excel file.
#' @param sheet Name of sheet within Excel file.
#' @return A list with the following named elements:
#'
#' @author Steve Juggins \email{Stephen.Juggins@@ncl.ac.uk}

read_DARLEQ <- function(filename, sheet=NULL, verbose=TRUE) {
  is_empty <- function(x) {
    ifelse (!is.na(x) & length(stringr::str_trim(d[i, 2])) > 0, FALSE, TRUE)
  }

# if sheet is null we just read the first sheet
  if (is.null(sheet)) {
     d <- readxl::read_excel(filename, col_types = "text")
     sheet <- get_Sheets(filename)[1]
  } else {
     d <- readxl::read_excel(filename, col_types = "text", sheet=sheet)
  }

# Now split data into header with site and water chemistry information, and diatom data
# find start of data.  Column 2 should be empty in header but have taxon names in block of diatom data
  iEndCol <- ncol(d); iEndRow <- nrow(d)
  for (i in 1:20) {
    if (!is_empty(d[i, 2]))
        break
  }

# find rightmost column of data.  It should have a sample ID in row 1.
  iStartRow <- i; iStartCol <- 3
  for (i in iStartCol:iEndCol) {
    if (is_empty(d[1, i])) {
      iEndCol <- i-1
      break
    }
  }

# Extract header and remove any rows with no variable ID in column 1.
  header <- as.data.frame(d[1:(iStartRow-1), 1:iEndCol])
  iEndRowHeader <- nrow(header)
  for (i in 1:iEndRowHeader) {
    if (is_empty(header[i, 1])) {
      iEndRowHeader <- i-1
      break
    }
  }
  header <- header[1:iEndRowHeader, ]
  rownames(header) <- header[, 1]
  header <- as.data.frame(t(header[, -c(1:2)]), stringsAsFactors=FALSE)

#  Convert any columns with date or chemistry data to numeric
  sel <- c("ALKALINITY", "CALCIUM", "DOC")
  mt <- toupper(colnames(header)) %in% sel
  suppressWarnings(header[, mt] <- sapply(header[, mt], as.numeric))

# Convert SAMPLEDATE field to dates
  mt <- grep("DATE", toupper(colnames(header)))
  if (length(mt) > 0) {
    for (i in 1:length(mt)) {
      suppressWarnings(dt <- sapply(header[, mt], as.numeric))
      suppressWarnings(header[, mt[i]] <- as.Date(dt, origin = "1899-12-30"))
      if (any(nchar(dt)<5, na.rm=TRUE)) {
         sel <- which(nchar(dt)<5)
         suppressWarnings(header[sel, mt[i]] <- as.Date(paste0(dt[sel < 5], "-01-01"), format="%Y-%d-%xx <- c(", origin = "1899-12-30"))
      }
    }
    colnames(header)[mt[1]] <- "SAMPLE_DATE"
  }
  header <- data.frame(SampleID=rownames(header), header)

# Extract diatom data and remove any rows without taxon code in column 1

  d2 <- as.data.frame(d[iStartRow:iEndRow, 1:iEndCol], stringsAsFactors=FALSE)

  haveCode <- is_empty(d2[, 1])
  if (any(haveCode))
    d2 <- d2[!haveCode, ]
  if (nrow(d2) < 1) {
    .errMessage("No diatom data found, are you sure this is a DARLEQ data file?", verbose)
  }

# Merge duplicate rows and replace missing values with zero
  nms <- d2[, 1]
  d2 <- d2[, -c(1:2)]
  d2[is.na(d2)] <- 0
  # check for errors

  suppressWarnings(d2 <- sapply(d2, as.numeric))
  non_numeric <- sum(is.na(d2))
  if (non_numeric > 0) {
    .errMessage(paste0(non_numeric, " non-numeric values found in diatom data. Please correct and try again."), verbose)
  }
  d2 <- aggregate(d2, list(nms), sum)
  rownames(d2) <- d2[, 1]
  d2 <- as.data.frame(t(d2[, -1]), stringsAsFactors=FALSE)
  d2[is.na(d2)] <- 0

#  d2 <- d2[, apply(d2, 2, sum) > 0]
  if (sum(apply(d2, 2, sum) > 0) < 1) {
    .errMessage("No taxa found or all taxa have zero abundance.", verbose)
  }
  coding <- .get_Taxon_Coding(d2)
  if (is.null(coding)) {
     .errMessage("No taxon codes found, are you sure this is a DARLEQ dirom data file?", verbose)
  }
  res <- list(header=header, diatom_data=d2, filename=basename(filename), filepath=filename, sheet=sheet, coding=coding)
  class(res) <- "DARLEQ_DATA"
  res
}

get_Sheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  sheets
}

print.DARLEQ_DATA <- function(x, ...) {
  cat("File name  :", x$filename, "\n")
  cat(paste("Sheet name :", x$sheet, "\n"))
  cat(paste("No. samples:", nrow(x$diatom_data), "\n"))
  cat(paste("No. species:", ncol(x$diatom_data), "\n"))
  cat(paste("Coding:", x$coding, "\n"))
}

