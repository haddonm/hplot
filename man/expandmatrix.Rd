% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hplot_funs.R
\name{expandmatrix}
\alias{expandmatrix}
\title{expandmatrix reshapes a matrix of values into a 3 column data.frame}
\usage{
expandmatrix(x)
}
\arguments{
\item{x}{a matrix of values}
}
\value{
a 3-column matrix of (rows x cols) rows from the input matrix
}
\description{
expandmatrix takes an oblong matrix of values and expands it
    into a three column data.frame of row, column, value. This is then easier 
    to plot as a scattergram or is used within categoryplot. It expects to 
    have the year values in the columns = xvalues
}
\examples{
x <- matrix(rnorm(25,5,1),nrow=5,ncol=5,dimnames=list(1:5,1:5))
res <- expandmatrix(x)
res
}
