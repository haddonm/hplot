% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hplot_funs.R
\name{pythag}
\alias{pythag}
\title{pythag calculates Pythagorus' theorum on a vector of two values}
\usage{
pythag(x)
}
\arguments{
\item{x}{a vector of two numbers or a matrix of pairs of numbers}
}
\value{
a single number or a vector depending on input
}
\description{
pythag Pythagorus' theorum states that the length of the
    hypotheneuse between two lines at right angels to each other (that
    is in cartesian coordinates) is the sqrt of the sum of their squares.
}
\examples{
\dontrun{
 pythag(c(3,4))  # should be 5
 dat <- matrix(c(3,4,5,7),nrow=2,ncol=2,byrow=TRUE)
 print(dat)
 pythag(dat)     # should be 5 and 10
}
}
