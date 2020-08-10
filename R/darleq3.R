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
#' darleq3: Diatom Assessment of River and Lake Ecological Quality
#'
#' @description
#' darleq3 is an R package for the assessment of river and lake ecological status using diatom data obtained by light microscopy (LM) or Next Generation Sequencing (NGS). The package contains functions for importing data from Excel worksheets, calculating various water quality metrics, EQRs and Water Framework Directive quality classes.
#'
#' Additonal help on using the package and interpreting the results can be found in the following vignettes:
#'
#' \href{../doc/darleq3UserGuide.html}{darleq3 User Guide (html version)}
#'
#' \href{../doc/darleq3UserGuidePDF.pdf}{darleq3 User Guide (pdf version)}
#'
#' \href{../doc/darleq3.pdf}{darleq3 Manual (pdf version)}
#'
#' \href{../doc/interpreting_NGS_data.pdf}{Guide to interpreting TDI5 NGS data (pdf version)}
#'
#' @section darleq3 functions:
#'
#' * \code{\link{darleq}} import diatom data from an Excel file, calculate matrics, EQRs and WFD quality classes, and save results in Excel format
#' * \code{\link{read_DARLEQ}} import diatom data from an Excel file
#' * \code{\link{save_DARLEQ}} save metric and EQR results in an Excel file
#' * \code{\link{calc_Metric}} calculate various diatom water quality metrics
#' * \code{\link{calc_EQR}} calculate sample and site EQRs and WFD quality classes
#' * \code{\link{calc_Metric_EQR}} calculate EQRS, WFD quality classes and summary diagnostic measures for multiple metrics
#' * \code{\link{runDARLEQ}} run DARLEQ3 as an interactive shiny app in a web browser
#'
#' @section Acknowledgements:
#' DARLEQ was developed and funded as part of the following projects:
#' * Diatoms as Monitors of Ecological Status of Rivers (Diatom Assessment of River Ecological Status - DARES) Project No: EMC/WP04/078 Funded by the Environment Agency and SNIFFER (The Scotland and Northern Ireland Forum for Environmental Research).
#' * Development of a phytobenthos classification tool for lakes and lochs of UK (DALES - Diatom assessment of lake and loch ecological status) Project Code: EMC/WP09/079. Funded by The Environment Agency Science Programme.
#' * Development of the Diatom classification tool (DARLEQ) for lakes and rivers. Funded by The Environment Agency Science Programme.
#'
#' @references Kelly, M., S. Juggins, R. Guthrie, S. Pritchard, J. Jamieson, B. Rippey, H. Hirst, and M. Yallop, Assessment of ecological status in UK rivers using diatoms. \emph{Freshwater Biology}, 2008. 403-422.
#' @references Juggins, S., M. Kelly, T. Allott, M. Kelly-Quinn, and D. Monteith, A Water Framework Directive-compatible metric for assessing acidification in UK and Irish rivers using diatoms. \emph{Science of The Total Environment}, 2016. 671-678.
#' @references Bennion, H., M.G. Kelly, S. Juggins, M.L. Yallop, A. Burgess, J. Jamieson, and J. Krokowski, Assessment of ecological status in UK lakes using benthic diatoms. \emph{Freshwater Science}, 2014. 639-654.
#'
#'
#' @docType package
#' @name darleq3
"_PACKAGE"
