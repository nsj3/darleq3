suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(darleq3))

D_ui <- dashboardPage(
  dashboardHeader(title = "DARLEQ3 for diatom-based water quality assessment", titleWidth=600),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(shinyjs::useShinyjs(),
             column(width=4,
                    box(fileInput("fn", "Select input file:", accept=c(".xlsx"), width="100%"), width=200),
                    box(selectInput("sheet", "Select worksheet:", ""), width=200),
                    box(disabled(actionButton("importButton", "Import data")), width="80%"),
                    box(radioButtons("metric", "Select metric:", c(`TDI for LM`="TDILM", `TDI for NGS`="TDINGS", LTDI="LTDILM", DAM="DAMLM")), width=100),
                    box(disabled(actionButton("calculateButton", "Calculate!")), width="80%")
             ),
             column(width=8,
                    box(verbatimTextOutput("table1"), width=900, title="Data summary", status="primary"),
                    box(verbatimTextOutput("output1"), width=900, title="Results summary", status="primary"),
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
outFile <- ""

summarise_data <- function(fn, sheet, data) {
  if (nchar(fn_display) > 0) {
    nsam <- nrow(darleq_data$diatom_data)
    nsp <- ncol(darleq_data$diatom_data)
    p <- capture.output(print(darleq_data))
    paste(p, collapse="\n")
#    paste("File name: ", fn, "\n\rSheet:", sheet, "\n\nNumber of samples: ", nsam, "\nNumber of taxa: ", nsp )
  } else {
    paste("Select worksheet and click import data...")
  }
}

D_server <- function(input, output, session) {
  output$table1 <- renderText(summarise_data(fn, sheet, darleq_data))
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
    output$output1 <- renderText("")
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
      sheets.nms <<- tryCatch(get_sheets(fn2), error=function(e) return (e))
      if ("error" %in% class(sheets.nms)) {
        output$table1 <- renderText(sheets.nms$message)
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
        output$output1 <- renderText("")
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
      if (any(class(darleq_data) == "error")) {
#        output$table1 <- renderText(darleq_data$message)
        output$table1 <- renderText("darleq_data$message")
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
        output$output1 <- renderText("")
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
    res <- calc_all(darleq_data, metric)
    wb <- openxlsx::createWorkbook("Temp")
    outFile <<- file.path(tempdir(), "Results.xlsx")
    for (i in names(res)) {
      addWorksheet(wb, i)
      openxlsx::writeDataTable(wb, i, res[[i]], withFilter=FALSE, keepNA=FALSE)
    }
    saveWorkbook(wb, outFile, overwrite=TRUE)
    shinyjs::enable("downloadResults")
    output$output1 <- renderText("Results are ready to download")
  })
  output$downloadResults <- downloadHandler(
    filename <- function() {
      paste("Results", ".xlsx", sep="")
    },
    content <- function(file) {
      file.copy(outFile, file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
}

shinyApp(D_ui, D_server)
