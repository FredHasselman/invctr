% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/invictor.R
\name{\%00\%}
\alias{\%00\%}
\title{Rose tinted infix}
\usage{
x \%00\% y
}
\arguments{
\item{x}{If (an element of) \code{x} is any of \code{Inf,-Inf,NA,NaN,NULL,length(x)==0}, it will return/replace the value of \code{y}; otherwise \code{x}.}

\item{y}{The value to return/replace for \code{x} in case of catastrophe \code{>00<}}
}
\description{
When your functions wear these rose tinted glasses, the world will appear to be a nicer, fluffier place.
}
\examples{

Inf \%00\% NA

numeric(0) \%00\% ''

NA \%00\% 0

NaN \%00\% NA
c(1, NaN) \%00\% NA

NULL \%00\% NA
c(1, NULL) \%00\% NA # can't see second element

}
\seealso{
purrrr::\%||\%
}
\author{
Fred Hasselman
}
