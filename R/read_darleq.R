#' Read DARLEQ diatom data from an Excel file
#'
#' @description
#' \code{read_DARLEQ} imports DARLEQ-formatted diatom data from an Excel file.

#' @param file Name of Excel file.  See Details below for guidelines on formatting the diatom data.
#' @param sheet Name of sheet within Excel file.  If blank the function will import the first sheet in the Excel file.
#' @param verbose logical to indicate should function stop immediately on error (TRUE) or return a \code{simpleError} (FALSE).  Defaults to TRUE.
#'
#' @return A list with the following named elements:
#' \item{header}{data frame containing the rows of environmental data from the top of the Excel file (ie. site, sample, water chemistry and data information)}
#' \item{diatom_data}{data frame containing the diatom data}
#' \item{file}{name of the Excel file}
#' \item{filepath}{full path to the Excel file}
#' \item{sheet}{name of the Excel worksheet}
#'
#' @author Steve Juggins \email{Stephen.Juggins@@ncl.ac.uk}
#'
#' @details \code{\link{read_DARLEQ}} imports diatom data from an Excel file in either .xls or .xlsx format.
#' An example Excel file is included in this package.  See examples below to view it.  The required data and layout are slightly different for river and lake samples.  Figure 1 below shows the required format for performing TDI calculations for river samples.
#'
#' The first four header rows are mandatory and must contain the following information:
#' * Row 1: Sample identifier – a short numerical or alphanumeric code to uniquely identify the sample. This field cannot be empty (an empty cell indicates the end of data).
#' * Row 2:	Site identifier – a short numerical or alphanumeric code to uniquely identify the site. This code will be used to aggregate multiple samples when calculating confidence of class for a site.
#' * Row 3:	Sample Date in Day/Month/Year format. Missing dates are set to “Spring” for the purposes of classification using TDI3 and samples flagged with a warning.
#' * Row 4:	Mean annual alkalinity (or best available estimate) in mg l-1 (CaCO3). Missing values are set to 100 mg l-1 for the purposes of classification and samples flagged with a warning.  Alkalinity values outside the range of the site prediction algorithm are set to the appropriate limit (6 or 150 mg l-1 for TDI3 and 5 or 250 mg l-1 for TDI4 and TDI5LM / TDI5NGS).
#' * Rows 5+: Further option sample descriptors such as river name, reach name etc. These data are not used by the program but will be reproduced in the output.
#' Note that the second column of the header information must be left blank.
#'
#' \if{html}{\figure{DARLEQ_River_Data.png}{options: width="60\%" alt=""}}
#' \if{latex}{\figure{DARLEQRiverData.png}{options: width=9cm}}
#'
#' ***Figure 1: Example format for river diatom samples***
#'
#' Identifiers for each row of the sample header information should be listed in column 1.  Diatom data then follow the header information and may be in count or percentage format. The first column must contain the taxon code in either NBS or DiatCode (http://www.ecrc.ucl.ac.uk/?q=databases/diatcode) format. The codes in this column are used to link the data to the DARLEQ3 taxon list and ecological information and cannot be empty (an empty cell indicates the end of the data). The second column must include either the taxon name or code (ie. a repeat of column 1). Empty (blank) cells in the count or percentage data matrix will be read as zero. Character data in the diatom matrix will generate an error. A full list of diatom codes (either NBS or DiatCodes) are available in the dataframe \code{darleq3_taxa}.
#'
#' If the Diatom Acidification Metric (DAM) is to be calculated, rows 5 and 6 must contain estimates of mean annual Calcium and DOC concentrations, in ueq l-1 and mg l-1 respectively. Figure 2 shows an example formatted for calculation of TDI and DAM.  Note that if only DAM scores are required the Alkalinity field may be left blank.  Sample Date is not used for calculating DAM and may be left blank.
#'
#' \if{html}{\figure{DARLEQDAMData.png}{options: width="60\%" alt=""}}
#' \if{latex}{\figure{DARLEQDAMData.png}{options: width=9cm}}
#'
#' ***Figure 2: Example format for river diatom TDI and DAM samples***
#'
#' The required format for lake samples is shown in Figure 3. This is exactly the same as for river data except that the fourth row must contain a code indicating lake type according to the GB lake typology alkalinity classes.  Marl lakes are included in the high alkalinity (HA) group. Peat and brackish lakes are not covered by the tool.  Sample date for lake samples is not used in the class calculations and can contain missing values.
#'
#' \if{html}{\figure{DARLEQLakeData.png}{options: width="60\%" alt=""}}
#' \if{latex}{\figure{DARLEQLakeData.png}{options: width=9cm}}
#'
#' ***Figure 3: Example format for lake diatom LTDI samples***
#'
#' @examples
#' fn <- system.file("example_datasets/DARLEQ2TestData.xlsx", package="darleq3")
#' d <- read_DARLEQ(fn, "Rivers TDI Test Data")
#' head(d$diatom_data)
#' head(d$header)
#' \dontrun{
#' # view the example dataset in Excel
#' # note running the following lines will open the file in Excel (if installed)
#' fn <- system.file("example_datasets/DARLEQ2TestData.xlsx", package="darleq3")
#' shell.exec(fn)
#' }
#'
#' @export read_DARLEQ
#'

read_DARLEQ <- function(file, sheet=NULL, verbose=TRUE) {
  is_empty <- function(x) {
    ifelse (!is.na(x) & length(stringr::str_trim(d[i, 2])) > 0, FALSE, TRUE)
  }
  get_Sheets <- function(file) {
    sheets <- readxl::excel_sheets(file)
    sheets
  }

# if sheet is null we just read the first sheet
  if (is.null(sheet)) {
     d <- readxl::read_excel(file, col_types = "text")
     sheet <- get_Sheets(file)[1]
  } else {
     d <- readxl::read_excel(file, col_types = "text", sheet=sheet)
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
    errMessage("No diatom data found, are you sure this is a DARLEQ data file?", verbose)
  }

# Merge duplicate rows and replace missing values with zero
  nms <- d2[, 1]
  d2 <- d2[, -c(1:2)]
  d2[is.na(d2)] <- 0

# check for errors
  suppressWarnings(d2 <- sapply(d2, as.numeric))
  non_numeric <- sum(is.na(d2))
  if (non_numeric > 0) {
    errMessage(paste0(non_numeric, " non-numeric values found in diatom data. Please correct and try again."), verbose)
  }
  d2 <- stats::aggregate(d2, list(nms), sum)
  rownames(d2) <- d2[, 1]
  d2 <- as.data.frame(t(d2[, -1]), stringsAsFactors=FALSE)
  d2[is.na(d2)] <- 0

#  d2 <- d2[, apply(d2, 2, sum) > 0]
  if (sum(apply(d2, 2, sum) > 0) < 1) {
    errMessage("No taxa found or all taxa have zero abundance.", verbose)
  }
  res <- list(header=header, diatom_data=d2, file=basename(file), filepath=file, sheet=sheet)
  class(res) <- "DARLEQ_DATA"
  res
}

