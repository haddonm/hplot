#' hplot: a set of functions providing aid for case graphics plus custom plots
#'
#' The hplot package provides an array of utility functions and custom graphics
#' functions.
#'
#' @seealso{
#'    \link{plot1}, \link{plotprep}, \link{parset}, \link{addnorm}, 
#'    \link{getmax}, \link{getmin}, \link{pickbound}, \link{yearBubble}, 
#'    \link{inthist}, \link{plotnull}, \link{plotxyy}, \link{uphist}
#' 
#' }
#' 
#' @docType package
#' @name hplot
NULL

#' @importFrom grDevices dev.cur dev.new dev.off png palette rgb col2rgb
#' @importFrom graphics par grid plot axis mtext polygon title hist lines text
#' @importFrom graphics points abline symbols
#' @importFrom utils tail head str write.table write.csv
#' @importFrom stats quantile loess sd coef lm dnorm
NULL
