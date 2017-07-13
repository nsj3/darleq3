suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(darleq3))
suppressPackageStartupMessages(library(mgcv))


header <- dashboardHeader(title = paste0("DARLEQ3 for diatom-based water quality assessment, version ", utils::packageDescription("darleq3", fields="Version")), titleWidth=750)

D_ui <- dashboardPage(header, dashboardSidebar(disable = TRUE),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(shinyjs::useShinyjs(),
      column(width=4,
        box(fileInput("fn", "Select input file:", accept=c(".xlsx", ".xls"), width="100%"), width=200),
        box(selectInput("sheet", "Select worksheet:", ""), width=200),
        box(disabled(actionButton("importButton", "Import data")), width="80%"),
        box(radioButtons("metric", "Select metric:", c(`TDI for LM`="TDILM", `TDI for NGS`="TDINGS", `LTDI for LM`="LTDILM", `DAM for LM`="DAMLM")), width=100),
        box(disabled(actionButton("calculateButton", "Calculate!")), width="80%")
      ),
      column(width=8,
        box(verbatimTextOutput("table1"), width=900, title="Data summary", status="primary"),
        box(verbatimTextOutput("table2"), width=900, title="Results summary", status="primary"),
        box(disabled(downloadButton("downloadResults", "Download results")), width="80%")
      )
    )
  )
)

fn <- ""
fn2 <- ""
fn_display <- ""
darleq_data <- NULL
sheet <- ""
#outFile <- ""

summarise_data <- function(fn, sheet, data) {
  if (nchar(fn_display) > 0) {
    nsam <- nrow(darleq_data$diatom_data)
    nsp <- ncol(darleq_data$diatom_data)
    p <- capture.output(print.DARLEQ_DATA(darleq_data))
    paste(p, collapse="\n")
#    paste("File name: ", fn, "\n\rSheet:", sheet, "\n\nNumber of samples: ", nsam, "\nNumber of taxa: ", nsp )
  } else {
    paste("Select worksheet and click import data...")
  }
}

D_server <- function(input, output, session) {
  outFile <- ""
  output$table1 <- renderText(summarise_data(fn, sheet, darleq_data))
  res <- NULL
  observeEvent(input$sheet, {
    darleq_data <<- NULL
    sheet <<- ""
    fn_display <<- ""
    output$table1 <- renderText(summarise_data(fn, sheet, darleq_data))
    if (nchar(input$sheet) > 0) {
      shinyjs::enable("importButton")
    } else {
      shinyjs::disable("importButton")
    }
    shinyjs::disable("downloadResults")
    shinyjs::disable("calculateButton")
    output$table2 <- renderText("")
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(input$fn$name, {
    fn1 <- input$fn$name
    if (is.null(fn1))
      return(NULL)
    if (fn1 != fn) {
      dn <- dirname(input$fn$datapath)
      fn2 <<- file.path(dn, input$fn$name)
      if (file.exists(fn2))
        file.remove(fn2)
      file.rename(input$fn$datapath, fn2)
      fn <<- fn1
      sheets.nms <<- tryCatch(get_Sheets(fn2), error=function(e) return (e))
      if (inherits(sheets.nms, "error")) {
        output$table1 <- renderText(sheets.nms$message, quoted=TRUE)
        sheet <<- ""
        darleq_data <<- NULL
        fn1 <- input$fn$name
        return()
      }
      updateSelectInput(session, "sheet", choices=sheets.nms)
      sheet <<- input$sheet
      if (nchar(input$sheet) > 0) {
        shinyjs::enable("importButton")
      } else {
        shinyjs::disable("importButton")
        shinyjs::disable("downloadResults")
        output$table2 <- renderText("")
      }
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(input$importButton, {
    if (nchar(fn2[1]) > 4) {
      sheet <<- input$sheet
      if (nchar(sheet) < 1)
        return(NULL)
      fn_display <<- fn
      darleq_data <<- tryCatch(read_DARLEQ(fn2, sheet=sheet), error=function(e) return(e))
      if (inherits(darleq_data, "error")) {
        output$table1 <- renderText(darleq_data$message, quoted=TRUE)
        darleq_data <<- NULL
        outFile <<- ""
        sheet <<- ""
      } else {
        output$table1 <- renderText(summarise_data(fn, sheet, darleq_data))
      }
      if(!is.null(darleq_data)) {
        shinyjs::enable("calculateButton")
      } else {
        shinyjs::disable("calculateButton")
        shinyjs::disable("downloadResults")
        output$table2 <- renderText("")
      }
    }
  })
  observeEvent(input$calculateButton, {
    if (!require(openxlsx, quietly=TRUE)) {
      output$table1 <- renderText("This function needs the package openxlsx, please install it")
    }
    metric <- input$metric
    if (is.null(darleq_data)) {
      shinyjs::disable("downloadResults")
      shinyjs::disable("calculateButton")
    }
    metrics <- NULL
    if (metric=="TDILM") {
      metrics <- c("TDI3", "TDI4", "TDI5LM")
    } else if (metric == "LTDILM") {
      metrics <- c("LTDI1", "LTDI2")
    } else if (metric == "TDINGS") {
      metrics <- c("TDI5NGS")
    } else if (metric == "DAMLM") {
      metrics <- c("DAM")
    }
    res <<- tryCatch(calc_Metric_EQR(darleq_data, metrics, verbose=FALSE), error=function(e) return(e))
    if (inherits(res, "error")) {
      output$table2 <- renderText(paste0("Error calculating metrics: \n", res$message), quoted=TRUE)
      res <<- NULL
      return()
    }
    shinyjs::enable("downloadResults")
    output$table2 <- renderText("Results are ready to download")
  })
  output$downloadResults <- downloadHandler(
    filename = function() {
      tmp <- basename(fn)
      tmp <- strsplit(tmp, "\\.")[[1]][1]
      if (is.null(sheet))
        sheet <- d$sheet
      outFile <- paste0("DARLEQ3_Results_", tmp, "_", sheet, "_", Sys.Date(), ".xlsx")
      outFile <- gsub(" ", "_", outFile)
      outFile
    },
    content <- function(file) {
      retval <- tryCatch(save_darleq3(res, file, fn=fn, sheet=sheet, FALSE))
      if (inherits(retval, "error")) {
        output$table1 <- renderText(res$message)
        shinyjs::disable("downloadResults")
        return()
      }
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
}

shinyApp(D_ui, D_server)
