% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hplot_funs.R
\name{plotcompdata}
\alias{plotcompdata}
\title{plotcompdata makes a series of barplots to compare composition data}
\usage{
plotcompdata(
  compdata,
  sau,
  ylabel = "",
  console = TRUE,
  outdir = "",
  barcol = "red",
  bordercol = "black",
  horizline = NULL
)
}
\arguments{
\item{compdata}{matrix of sizes or ages by years, row and column names should
be numbers, as in ages or sizeclasses for rows, and years for columns.}

\item{sau}{name of spatial assessment unit or origin of samples}

\item{ylabel}{what outer name to be used for the Y-axis, default=''}

\item{console}{should the graph be plotted to the console or saved as a file.
default=TRUE ie it goes to the console, if set to FALSE it goes to 
paste0(rundir,/)}

\item{outdir}{if saving a png file this is the directory to which a filename
is added, default=""}

\item{barcol}{what colour should the bars be, default = red}

\item{bordercol}{what colour should the bar borders be? default = black}

\item{horizline}{the size or age against which to draw a reference line. 
default=NULL. To draw a line at 136mm then horizline=136}
}
\value{
invisibly returns a list of the filename and caption
}
\description{
plotcompdata generates a horizontal array of up to 20 barplots
    and is used to represent time-series of age- or size-composition data in
    a manner that makes interannual comparisons simple. Currently designed 
    for use with makehtml
}
\examples{
\dontrun{
  x <- matrix(rnorm(600,mean=20,sd=3),nrow=20,ncol=30,
              dimnames=list(seq(122,160,2),1991:2020))
  ylabel="Random Composition"
  numcol <- ncol(x)
  if (numcol > 20) x1 <- x[,1:20]
  plotcompdata(compdata=x1,sau="sauX",ylabel="Random Composition Data",
               console=TRUE,barcol="red",bordercol="black",horizline=136)
}              
}
