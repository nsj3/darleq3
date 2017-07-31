#' Run DARLEQ3 as an interactive shiny app
#'
#' @details
#' \code{runDARLEQ} runs darleq3 as an interactive shiny app. When running the function will open a web browser displaying the shiny app:
#'
#' \if{html}{\figure{darleq3shiny.png}{options: width="60\%" alt=""}}
#' \if{latex}{\figure{darleq3shiny.png}{options: width=9cm}}
#'
#' To use the app follow these simple steps:
#' * 1: Click the Browse... button to select and upload a DARLEQ diatom file (see function \code{\link{read_DARLEQ}} for guidelines on how to format this file).
#' * 2: Once uploaded, select a sheet and click import.  A summary (number of samples & taxa) will be displayed in the Data summary box when upload is complete.
#' * 3: Select the metric type. "TDI for LM" will calculate TDI3, TDI4 and TDI5LM for river LM diatom data, TDI for NGS will calculate TDI5NGS for river NGS diatom data, "LTDI for LM" will calculate LTDI1 and LTDI2 for lake LM data, and "DAM for LM" will calculate the diatom acidification metric for river LM data.  A summary of results will appear in the Results summary box when the calculations are complete.
#' * 4: Click Download Results to save the results in an Excel file.  The default name for this file will be the "DARLEQ3_Results_" concatenated with the original data filename, worksheet name, and date.
#'
#' To quit the app simple close the browser and or hit Escape in the RStudio Console window.
#'
#' @examples
#' \dontrun{
#' library(darleq3)
#' runDARLEQ()
#' }
#'
#' @export runDARLEQ
#'
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar
#' @importFrom shinyjs disable enable

runDARLEQ <- function() {
  fn <- system.file("shiny_app/app.R", package="darleq3")
  shiny::shinyAppFile(fn)
}

