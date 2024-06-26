% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hplot_funs.R
\name{addnorm}
\alias{addnorm}
\title{addnorm adds a normal distribution to a histogram of a data set.}
\usage{
addnorm(inhist, xdata, inc = 0.01)
}
\arguments{
\item{inhist}{is the output from a call to 'hist' (see examples)}

\item{xdata}{is the data that is being plotted in the histogram.}

\item{inc}{defaults to a value of 0.01; is the fine grain increment used to
define the normal curve. The histogram will be coarse grained relative to
this.}
}
\value{
a list with a vector of 'x' values and a vector of 'y' values (to be
   used to plot the fitted normal probability density function), and a vector
   of four called 'stats' containing the mean, standard deviation, number of
   observations and median of the input data
}
\description{
addnorm adds a normal distribution to a histogram of a data
   set. This is generally to be used to illustrate whether log-transformation
   normalizes a set of catch or cpue data.
}
\examples{
x <- rnorm(1000,mean=5,sd=1)
dev.new(height=6,width=4,noRStudioGD = TRUE)
par(mfrow= c(1,1),mai=c(0.5,0.5,0.3,0.05))
par(cex=0.85, mgp=c(1.5,0.35,0), font.axis=7)
outH <- hist(x,breaks=25,col=3,main="")
nline <- addnorm(outH,x)
lines(nline$x,nline$y,lwd=3,col=2)
print(nline$stats)
}
