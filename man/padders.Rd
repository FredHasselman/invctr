% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/invictor.R
\name{padders}
\alias{padders}
\alias{\%[+\%}
\alias{\%+]\%}
\alias{\%[+]\%}
\title{Padd vector by index}
\usage{
x \%[+\% j

x \%+]\% j

x \%[+]\% j
}
\arguments{
\item{x}{A vector}

\item{j}{A one, or two element vector. One element: Pad \code{front} or \code{rear} by \code{j} \code{0}s, or, \code{front} by \code{floor(j/2)} and rear by \code{ceiling(j/2)}. Two elements: Pad \code{j[1]} times the value passed in \code{j[2]}.}
}
\value{
A padded version of \code{x}
}
\description{
Padd vector by index

Pad vector front \verb{\%[+\%}

Pad vector rear \verb{\%+]\%}

Pad vector front + rear \verb{\%[+]\%}
}
\examples{

x <- rnorm(100)

# Pad front with 10 zeros
x\%[+\%10
# Same as
x\%[+\%c(10,0)

# Pad rear with zeros
x\%+]\%10
# Same as
x\%+]\%c(10,0)

# Pad front + rear with NA
x\%[+]\%c(NA,10)

# Pad front + rear of a character vector
"yes"\%[+]\%c(2,"no")
"yes"\%[+]\%c(1,"no")
"yes"\%[+]\%c(0,"no")

}
