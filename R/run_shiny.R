runDARLEQ3 <- function() {
  if (!require(shiny, quietly=TRUE))
    stop("This function needs the package shiny, please install it.")
#  if (!require(shinyjs, quietly=TRUE))
#    stop("This function needs the package shinyjs, please install it.")
#  if (!require(shinydashboard, quietly=TRUE))
#    stop("This function needs the package shinydashboard, please install it.")
  appDir <- system.file("shiny_app/darleq3_shiny.R", package = "darleq3")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `darleq3`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}

