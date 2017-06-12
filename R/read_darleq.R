# Functions to read DARLEQ data in excel format

read_DARLEQ <- function(fn, sheet=NULL, verbose=FALSE) {
#
  is_empty <- function(x) {
    ifelse (!is.na(x) & length(str_trim(d[i, 2])) > 0, FALSE, TRUE)
  }

  if (!require(readxl, quietly=TRUE)) {
    simpleError("Package readxl not installed - please install it.")
  }
  if (!require(stringr, quietly=TRUE)) {
    simpleError("Package stringr not installed - please install it.")
  }

  # to do - deal with multiple sheets and sheet names
  if (is.null(sheet))
    d <- read_excel(fn, col_types = "text")
  else
    d <- read_excel(fn, col_types = "text", sheet=sheet)
  iEndCol <- ncol(d)
  iEndRow <- nrow(d)
  # find start of data
  for (i in 1:20) {
    if (!is_empty(d[i, 2]))
        break
  }
  iStartRow <- i
  iStartCol <- 3

  for (i in iStartCol:iEndCol) {
    if (is_empty(d[1, i])) {
      iEndCol <- i-1
      break
    }
  }
#  for (i in iStartRow:iEndRow) {
#    if (is_empty(d[i, 1])) {
#      iEndRow <- i-1
#      break
#    }
#  }
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
  d2 <- as.data.frame(d[iStartRow:iEndRow, 1:iEndCol], stringsAsFactors=FALSE)

#  deal with numeric columns
  sel <- c("ALKALINITY", "CALCIUM", "DOC", "SAMPLEDATE", "SAMPLE_DATE")
  mt <- toupper(colnames(header)) %in% sel
  suppressWarnings(header[, mt] <- sapply(header[, mt], as.numeric))

# deal with dates
  mt <- match("SAMPLEDATE", toupper(colnames(header)))
  if (is.na(mt))
    mt <- match("SAMPLE_DATE", toupper(colnames(header)))
  if (!is.na(mt)) {
     suppressWarnings(header[, mt] <- as.Date(header[, mt], origin = "1899-12-30"))
     colnames(header[mt]) <- "SAMPLE_DATE"
  }
  header <- data.frame(SampleID=rownames(header), header)

# check for errors

  haveCode <- ifelse(is.na(d2[, 1]) | nchar(d2[, 1]) < 1, FALSE, TRUE)
  if (any(!haveCode))
    d2 <- d2[haveCode, ]

# deal with duplicate row names

  nms <- d2[, 1]
  d2 <- d2[, -c(1:2)]
  d2[is.na(d2)] <- 0
  # check for errors
  suppressWarnings(d2 <- sapply(d2, as.numeric))
  d2 <- aggregate(d2, list(nms), sum)
  rownames(d2) <- d2[, 1]
  d2 <- as.data.frame(t(d2[, -1]), stringsAsFactors=FALSE)
  nms <- colnames(d2)
  mt1 <- match(nms, darleq3_taxa$NBSCode)
  nM1 <- sum(!is.na(mt1))
  mt2 <- match(nms, darleq3_taxa$TaxonId)
  nM2 <- sum(!is.na(mt2))
  if (sum(nM1 + nM2) < 1)
    simpleError("No taxon codes found, are you sure this is a DARLEQ data file?")
  res <- list(header=header, diatom_data=d2, filename=basename(fn), sheet=sheet)
  class(res) <- "DARLEQ_DATA"
  if (nM1 > nM2)
    class(res) <- c(class(res), "NBSCode")
  res
}

get_sheets <- function(fn) {
  if (!require(readxl, quietly=TRUE)) {
    simpleError("package readxl not installed - please install it.")
  }
  sheets <- excel_sheets(fn)
  sheets
}

get_file_sheet_name <- function(fn=NULL, sheet=NULL) {
  if (!require(readxl, quietly=TRUE)) {
    simpleError("package readxl not installed - please install it.")
  }
  if (is.null(fn)) {
     Fil <- matrix(c("Excel files (*.xls, *.xlsx)", "*.xls;*.xlsx"), nrow=1)
     fn <- tryCatch(choose.files(multi=FALSE, filters=Fil))
     if (length(fn) < 1) {
       return("Operation cancelled")
     }
  }
  sheets <- tryCatch(excel_sheets(fn))
  if ("error" %in% class(sheets)) {
    cat(sheets$message)
    return(NULL)
  }
  if (length(sheets) == 1) {
    return(list(fn=fn, sheet=sheets[1]))
  } else {
    if (is.null(sheet)) {
       n_sheet <- menu(sheets)
       if (n_sheet > 0) {
         return(list(fn=fn, sheet=sheets[n_sheet]))
       } else {
         return(NULL)
       }
    } else {
      if (sheet %in% sheets)
        return(list(fn=fn, sheet=sheet))
      else {
        return(paste0("Sheet ", sheet, " not found in file ", basename(fn)))
      }
    }
  }
}

print.DARLEQ_DATA <- function(x, ...) {
        cat("File name  :", x$filename, "\n")
  cat(paste("Sheet name :", x$sheet, "\n"))
  cat(paste("No. samples:", ncol(x$diatom_data), "\n"))
  cat(paste("No. species:", nrow(x$diatom_data), "\n"))
}

