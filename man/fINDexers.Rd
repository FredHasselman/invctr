% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/invictor.R
\name{fINDexers}
\alias{fINDexers}
\alias{\%ci\%}
\alias{\%ri\%}
\alias{\%mi\%}
\alias{\%ai\%}
\alias{\%e\%}
\title{Find row or column by name or index}
\usage{
c \%ci\% d

r \%ri\% d

rc \%mi\% d

nv \%ai\% d

x \%e\% y
}
\arguments{
\item{c}{Column name or index}

\item{d}{A named vector, list, matrix, or data frame}

\item{r}{Row name or index}

\item{rc}{A 2-element numeric or character vector representing \code{c(r,c)}. Names (character) and indices (numeric) vectors can be mixed if \code{rc} is passed as a 2-element list object.}

\item{nv}{A numeric value, or vector of values of which you want to know the indices in \code{d}.}

\item{x}{A vector, data frame or list containing numbers and/or characters that could be elements of y}

\item{y}{An object that could contain values in x}
}
\value{
If \code{r/c/rc} is numeric, the name corresponding to the row/column index of \code{d}, if \code{r/c/rc} is a character vector, the row/column index corresponding to the row/column name. If \code{dimnames(d) == NULL}, but \code{names(d) != NULL} then \code{\%ci\%} and \code{\%ri\%} will look up \code{r/c} in \code{names(d)}

Logical vector indicating which x are an element of y
}
\description{
Find row or column by name or index

Column by name or index: \verb{\%ci\%}

Row by name or number: \verb{\%ri\%}

Matrix cell index by name or number: \verb{\%mi\%}

Return all indices of a (range of) values: \verb{\%ai\%}

Is element of... with multiple input types: \verb{\%e\%}
}
\examples{

# data frame
d <- data.frame(x=1:5,y=6,row.names=paste0("ri",5:1))

"y" \%ci\% d # y is the 2nd column of d
  2 \%ci\% d # the name of the second column of d is "y"

    2 \%ri\% d
"ri5" \%ri\% d

# change column name
 colnames(d)["y" \%ci\% d] <- "Yhat"

# mi works on data frames, matrices, tiblles, etc.
 c(5,2) \%mi\% d
 list(r="ri1",c=2) \%mi\% d

# matrix row and column indices
m <- matrix(1:10,ncol=2, dimnames = list(paste0("ri",0:4),c("xx","yy")))

 1 \%ci\% m
 5 \%ci\% m # no column 5

 1 \%ri\% m
 5 \%ri\% m

 c(5,1)\%mi\%m
 c(1,5)\%mi\%m

# For list and vector objects ri and ci return the same values
l <- list(a=1:100,b=LETTERS)

  2 \%ci\% l
"a" \%ci\% l

  2 \%ri\% l
"a" \%ri\% l

# named vector
v <- c("first" = 1, "2nd" = 1000)

"2nd" \%ci\% v
    1 \%ci\% v

"2nd" \%ri\% v
    1 \%ri\% v

# get all indices of the number 1 in v
 1 \%ai\% v

# get all indices of the number 3 and 6 in d
 c(3,6) \%ai\% d

# get all indices of values: Z < -1.96 and Z > 1.96
 Z <- rnorm(100)
 Z[Z\%)(\%c(-1.96,1.96)] \%ai\% Z


}
\author{
Fred Hasselman
}
