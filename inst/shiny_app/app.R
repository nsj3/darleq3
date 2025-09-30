##
## Copyright (c) 2019, Steve Juggins
##
## License GPL-2
##
## Permission is hereby granted to use, copy, modify and distribute the software in accordance with
## the GPL-2 license and subject to the following condition:
##
## The above copyright notice and this permission notice shall be
## included in all copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
## EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
## MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
## NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
## LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
## OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
## WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
##

suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(darleq3))
suppressPackageStartupMessages(library(mgcv))

header <- dashboardHeader(title = paste0("DARLEQ3 for diatom-based water quality assessment"), titleWidth=750)

email <- tags$html( tags$body( a(href="mailto:Stephen.Juggins@ncl.ac.uk")))

D_ui <- dashboardPage(header, dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(tags$style(HTML('.skin-blue .main-header .logo {
                              background-color: #3c8dbc;
                              text-align: left; }
                              .skin-blue .main-header .logo:hover {
                              background-color: #3c8dbc; } '))),

    # Boxes need to be put in a row (or column)
    fluidRow(shinyjs::useShinyjs(),
      column(width=4,
        box(fileInput("fn", "Select input file:",
              accept=c("application/vnd.ms-excel",
                       "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                        ".xlsx", ".xls"),
              width="100%"), width=200),
        box(verbatimTextOutput("message1"),
            selectInput("sheet", "Select worksheet:", ""), width=200),
        box(actionButton("importButton", "Import data"), width="80%"),
        box(radioButtons("metric", "Select metric:",
                         c(`TDI5 for LM`="TDI5LM", `TDI 3 & 4 for LM`="TDI34LM",
                           `TDI5 for NGS`="TDI5NGS", `TDI5.1 for NGS`="TDI5.1NGS",
                           `LTDI 1 & 2 for LM`="LTDILM",
                           `DAM for LM`="DAMLM")), width=100),
        box(actionButton("calculateButton", "Calculate!"), width="80%")
      ),
      column(width=8,
        box(verbatimTextOutput("table1"), width=900, title="Data summary", status="primary"),
        box(verbatimTextOutput("table2"), width=900, title="Results summary", status="primary"),
        box(downloadButton("downloadResults", "Download results"), width="80%"),
        box(p(paste0("This is DARLEQ3 Version ",
                     utils::packageDescription("darleq3", fields="Version"), " (",
                     utils::packageDescription("darleq3", fields="Date"), ")")),
            "Please email comments, bug reports etc to ", a("Stephen.Juggins@ncl.ac.uk", href="mailto:Stephen.Juggins@ncl.ac.uk"), width="80%"),
        helpText(p(a("darleq3 User Guide", target="_blank", href="darleq3UserGuidePDF.pdf")),
            p(a("Guide to interpreting TDI5 NGS data", target="_blank", href="Interpreting_NGS_data.pdf")),
            p(a("Download darleq3 taxon list", target="_blank", href="DarleqTaxonList2017_Master.xlsx")),
            p(a("Download Diat.barcode lookup table", target="_blank", href="Diat_barcode_to_darleq_lookup_09_2025.xlsx")),
            width="80%")
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
id <- NULL


summarise_data <- function(fn, sheet, data) {
  if (nchar(fn_display) > 0) {
    nsam <- nrow(darleq_data$diatom_data)
    nsp <- ncol(darleq_data$diatom_data)
    p <- capture.output(print(darleq_data))
    return(paste(p, collapse="\n"))
#    paste("File name: ", fn, "\n\rSheet:", sheet, "\n\nNumber of samples: ", nsam, "\nNumber of taxa: ", nsp )
  } else {
    return("")
  }
}

D_server <- function(input, output, session) {
  outFile <- ""
  output$table1 <- renderText(summarise_data(fn, sheet, darleq_data))
  res <- NULL
  shinyjs::disable("downloadResults")
  shinyjs::disable("calculateButton")
  shinyjs::disable("importButton")

  observeEvent(input$metric, {
    if (!is.null(id))
      removeNotification((id))
    if (input$metric %in% c("TDI34LM", "LTDILM")) {
      id <<- showNotification("Only use these metrics with diatom samples analysed optically prior to the 7th of March 2017", duration=NULL, type="error")
    }
  })

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
    res <<- NULL
    output$table2 <- renderText("")
#  }, ignoreSysqInit = TRUE, ignoreNULL = TRUE)
}, ignoreNULL = TRUE)

  observeEvent(input$fn$name, {
    get_Sheets <- function(file) {
      sheets <- readxl::excel_sheets(file)
      sheets
    }
    fn1 <- input$fn$name
    cat(paste("Input file = ", input$fn$name, "\n"))
    cat(paste("fn =", input$fn$type, "\n"))
    cat(paste("fn =", input$fn$datapath, "\n"))
    if (is.null(fn1)) {
      return(NULL)
    }
#    if (fn1 != fn) {
      dn <- dirname(input$fn$datapath)
      fn2 <<- file.path(dn, input$fn$name)
      if (file.exists(fn2))
        file.remove(fn2)
      file.rename(input$fn$datapath, fn2)
      fn <<- fn1
      output$table2 <- renderText("")
      res <<- NULL
      sheet <<- ""
      darleq_data <<- NULL
      sheets.nms <<- tryCatch(get_Sheets(fn2), error=function(e) return (e))
      if (inherits(sheets.nms, "error")) {
        updateSelectInput(session, "sheet", choices="")
        output$message1 <- renderText(paste0("Cannot open Excel file.\nReason: ", sheets.nms$message), quoted=TRUE)
        fn1 <- input$fn$name
        sheets.nms <<- ""
        output$table1 <- renderText("")
        shinyjs::disable("importButton")
        shinyjs::disable("downloadResults")
        return()
      }
      sheet <<- input$sheet
      if (nchar(input$sheet) > 0) {
        shinyjs::enable("importButton")
      }
      output$message1 <- renderText("Select worksheet and click import data...")
      updateSelectInput(session, "sheet", choices=sheets.nms)
#    }
#  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  }, ignoreNULL = TRUE)

  observeEvent(input$importButton, {
    if (nchar(fn2[1]) > 4) {
      sheet <<- input$sheet
      if (nchar(sheet) < 1)
        return(NULL)
      fn_display <<- fn
      darleq_data <<- tryCatch(read_DARLEQ(fn2, sheet=sheet), error=function(e) return(e))
      if (inherits(darleq_data, "error")) {
        output$table1 <- renderText(paste0("Cannot import data\nReason: ", darleq_data$message), quoted=TRUE)
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
        res <<- NULL
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
    if (metric=="TDI5LM") {
      metrics <- c("TDI5LM")
    } else if (metric == "TDI34LM") {
      metrics <- c("TDI3", "TDI4")
    } else if (metric == "LTDILM") {
      metrics <- c("LTDI1", "LTDI2")
    } else if (metric == "TDI5NGS") {
      metrics <- c("TDI5NGS")
    } else if (metric == "TDI5.1NGS") {
      metrics <- c("TDI5.1NGS")
    } else if (metric == "DAMLM") {
      metrics <- c("DAM")
    }
    res <<- tryCatch(calc_Metric_EQR(darleq_data, metrics, verbose=FALSE), error=function(e) return(e))
    if (inherits(res, "error")) {
      output$table2 <- renderText(paste0("Error calculating metrics: \n", res$message), quoted=TRUE)
      res <<- NULL
      return()
    } else {
      mm <- paste(names(res), collapse=", ")
      msg <- paste0("Results ready for ", mm, ".\nClick button below to download.")
      for (i in 1:length(res)) {
         if (!is.null(res[[i]]$warnings)) {
            msg <- paste0(msg, "\nWarning: ", res[[i]]$warnings)
         }
      }
      output$table2 <- renderText(msg)
    }
    shinyjs::enable("downloadResults")
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
      retval <- tryCatch(save_DARLEQ(res, file, fn=fn, sheet=sheet, FALSE))
      if (inherits(retval, "error")) {
        output$table2 <- renderText(res$message, quoted=TRUE)
        return()
      }
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
}

shinyApp(D_ui, D_server)
