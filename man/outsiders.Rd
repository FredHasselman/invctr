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
\title{Values outside interval}
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
Values outside interval

Not in closed interval: \verb{\%][\%}

Not in open interval: \verb{\%)(\%}

Not in half-closed interval (left): \verb{\%](\%}

Not in half-closed interval (right): \verb{\%)[\%}

Return x not in closed interval: \verb{\%].[\%}

Return x not in open interval: \verb{\%).(\%}

Return x not in half-closed interval (left): \verb{\%].(\%}

Return x not in half-closed interval (right): \verb{\%).[\%}
}
\note{
Package \code{DescTools} provides similar functions
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
