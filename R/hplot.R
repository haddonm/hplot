#' hplot: a set of functions providing aid for case graphics plus custom plots
#'
#' The hplot package provides an array of utility functions and custom graphics
#' functions.
#'
#' @section plotutils:
#' \describe{
#'   \item{newplot}{bare-bones plotprep, opens a new device + default par}
#'   \item{parsyn}{prints par command syntax to the console to be copied}
#'   \item{plotprep}{sets up a plotting device external to Rstudio}
#'   \item{parset}{simplifies the definition of main par parameters}
#'   \item{setplot}{writes a base graphics template to the console}
#' }
#' 
#' @docType package
#' @name hplot
NULL

#' @importFrom grDevices dev.cur dev.new dev.off png palette
#' @importFrom graphics par grid plot axis mtext polygon title hist lines text
#' @importFrom graphics points
#' @importFrom utils tail head str write.table write.csv
#' @importFrom stats quantile loess sd 
NULL
