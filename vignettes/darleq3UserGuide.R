## ----setup1, include=FALSE--------------------------------------------------------------------------------------------
knitr::opts_chunk$set(out.width="100%", warning=FALSE, prompt=TRUE)
knitr::opts_chunk$set(cache=TRUE, fig.path="R_Figures/", cache.path="Cache/")
options(width=120)
library(darleq3)

## ----devtools, eval=FALSE---------------------------------------------------------------------------------------------
#  install.packages("devtools")

## ----github, eval=FALSE-----------------------------------------------------------------------------------------------
#  library(devtools)
#  install_github("nsj3/darleq3", build_vignettes=TRUE)

## ----example, eval=FALSE----------------------------------------------------------------------------------------------
#  library(darleq3)
#  fn <- system.file("extdata/DARLEQ2TestData.xlsx", package="darleq3")

## ----Excel, eval=FALSE------------------------------------------------------------------------------------------------
#  # note running the following lines will open the file in Excel (if installed)
#  shell.exec(fn)

## ----shiny, eval=FALSE------------------------------------------------------------------------------------------------
#  library(darleq3)
#  runDARLEQ()

## ----darleq, eval=FALSE-----------------------------------------------------------------------------------------------
#  fn <- system.file("extdata/DARLEQ2TestData.xlsx", package="darleq3")
#  darleq(fn)

## ----darleq2, eval=FALSE----------------------------------------------------------------------------------------------
#  fn <- system.file("extdata/DARLEQ2TestData.xlsx", package="darleq3")
#  darleq(fn, sheet="Lakes LTDI Test Data", metrics="LTDI2", outFile="Results.xlsx")

## ----darleq3, eval=FALSE----------------------------------------------------------------------------------------------
#  fn <- system.file("extdata/DARLEQ2TestData.xlsx", package="darleq3")
#  darleq(fn, sheet="Lakes LTDI Test Data", metrics=c("LTDI1", "LTDI2"), outFile="Results.xlsx")

## ----read-------------------------------------------------------------------------------------------------------------
fn <- system.file("extdata/DARLEQ2TestData.xlsx", package="darleq3")
d <- read_DARLEQ(fn, "Rivers TDI Test Data")
head(d$diatom_data[, 1:8])
head(d$header)

## ----meqr-------------------------------------------------------------------------------------------------------------
fn <- system.file("extdata/DARLEQ2TestData.xlsx", package="darleq3")
d <- read_DARLEQ(fn, "Rivers TDI Test Data")
results <- calc_Metric_EQR(d, metrics=c("TDI4", "TDI5LM"))
head(results$TDI5LM$EQR[, 9:15])
head(results$TDI5LM$Uncertainty)

## ----save, eval=FALSE-------------------------------------------------------------------------------------------------
#  fn <- system.file("extdata/DARLEQ2TestData.xlsx", package="darleq3")
#  d <- read_DARLEQ(fn, "Rivers TDI Test Data")
#  results <- calc_Metric_EQR(d, metrics=c("TDI4", "TDI5LM"))
#  save_DARLEQ(results, outFile="Results.xlsx")

## ----metric-----------------------------------------------------------------------------------------------------------
fn <- system.file("extdata/DARLEQ2TestData.xlsx", package="darleq3")
d <- read_DARLEQ(fn, "Rivers TDI Test Data")
x <- calc_Metric(d$diatom_data, metric="TDI4")
head(x$Metric)

## ----eqr--------------------------------------------------------------------------------------------------------------
fn <- system.file("extdata/DARLEQ2TestData.xlsx", package="darleq3")
d <- read_DARLEQ(fn, "Rivers TDI Test Data")
x <- calc_Metric(d$diatom_data, metric="TDI4")
eqr <- calc_EQR(x, d$header)
head(eqr$EQR[, 9:15])
head(eqr$Uncertainty)

