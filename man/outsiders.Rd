% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/invictor.R
\name{outsiders}
\alias{outsiders}
\alias{\%][\%}
\alias{\%)(\%}
\alias{\%](\%}
\alias{\%)[\%}
\alias{\%].[\%}
\alias{\%).(\%}
\alias{\%].(\%}
\alias{\%).[\%}
\title{Outside interval}
\usage{
x \%][\% j

x \%)(\% j

x \%](\% j

x \%)[\% j

x \%].[\% j

x \%).(\% j

x \%].(\% j

x \%).[\% j
}
\arguments{
\item{x}{A vector}

\item{j}{A range}
}
\value{
logical vector of length x, or, values of x outside the range j
}
\description{
Decide if a value \code{x} falls outside an interval \code{j[1],j[2]} that can be open or closed on the left and/or the right. Either a logical vector equal to \code{x}, or the actual values are extracted,
}
\note{
Package `DescTools` provides similar functions
}
\examples{

# Closed interval
5\%][\%c(1,5)
5\%].[\%c(1,5)

# Open interval
5\%)(\%c(1,5)
5\%).(\%c(1,5)

# Half-losed interval left
5\%](\%c(1,5)
5\%].(\%c(1,5)

# Half-losed interval right
5\%)[\%c(1,5)
5\%).[\%c(1,5)


}
