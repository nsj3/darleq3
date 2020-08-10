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
#' Print an object of class DARLEQ_Data
#'
#' @param x object of class DARLEQ_DATA
#' @param ... additional arguments to \code{print}
#'
#' @method print DARLEQ_DATA
#' @export
print.DARLEQ_DATA <- function(x, ...) {
  cat("File name  :", x$file, "\n")
  cat(paste("Sheet name :", x$sheet, "\n"))
  cat(paste("No. samples:", nrow(x$diatom_data), "\n"))
  cat(paste("No. taxa:", ncol(x$diatom_data), "\n"))
}

