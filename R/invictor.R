#' Rose tinted infix
#'
#' @param x If (an element of) \code{x} is any of \code{Inf,-Inf,NA,NaN,NULL,length(x)==0}, it will return/replace the value of \code{y}; otherwise \code{x}.
#' @param y The value to return/replace for \code{x} in case of catastrophy \code{>00<}
#'
#' @export
#' @author Fred Hasselman
#' @description When your functions wear these rose tinted glasses, the world will appear to be a nicer, fluffier place.
#'
#' @seealso purrrr::%||%
#'
#' @examples
#'
#' Inf %00% NA
#'
#' numeric(0) %00% ''
#'
#' NA %00% 0
#'
#' NaN %00% NA
#' c(1, NaN) %00% NA
#'
#' NULL %00% NA
#' c(1, NULL) %00% NA # can't see second element
#'
`%00%` <- function(x, y) {
  if (length(x) == 0) {
    x <- y
  } else {
      for (i in seq_along(x)) {
        l0 <- isna <- isnan <- isinf <- isnull <- isTryError <- FALSE
        if (length(x[i]) == 0) {
          l0 <- TRUE
        } else {
          if (all(is.na(x[i]))) {isna <- TRUE}
          if (all(is.nan(x[i]))) {isnan <- TRUE}
          if (all(is.infinite(x[i]))) {isinf <- TRUE}
          if (all(is.null(x[i]))) {isnull <- TRUE}
          if (all(class(x[i]) %in% "try-error")) {isTryError <- TRUE}
        }
        if (any(l0, isna, isnan, isinf, isnull, isTryError)) {
          x[i] <- y
        }
      }
  }
  return(x)
}

# Extractors -----

#' Extract vectors by index or value occurrence
#'
#' @description Extract front or rear of vector `x` up and untill an index `i`, the first or last occurence of a value `v`, or, extract values based on quantile `q`, first, middle, or, last index `j`.
#'
#' @param x A vector
#' @param i An index or two element vector \code{c(lo,hi)} indicating a range to extract
#' @param j A character indicating to extract the first `f`, middle `m` or last `l` value of `x`.
#' @param v A value of which the first or last occurrence in \code{x} will be used as an index
#' @param q A percentile value (between `0` and `1`)
#'
#' @name extractors
#'
#' @return A vector extracted from the front, rear, or, range of `x`. Either based on an index or the first or last occurrence of a value or the first, middle, or, ;ast element of a vector.
#'
#' @examples
#'
#' z <- letters
#'
#' # Extract front by first occurrence of value
#' z %[f% "n"
#'
#' # Extract front by index
#' x <- rnorm(100)
#' x %[% 10
#'
#' # Extract rear by index
#' x %]% 90
#'
#' # Extract rear by index
#' x %]% 90
#'
#' # Extract by indices if a range is provided
#' x %]% c(4,30)
#' z %[% c(6,10)
#'
#' # Extract last/middle value of x
#' x %:% "l"
#' z %:% "m"
#'
#' # Extract by percentile
#' seq(1,10,.5) %(q% .5 # infix
#' seq(1,10,.5)[seq(1,10,.5) < quantile(seq(1,10,.5),.5)] # regular syntax
#'
#' seq(1,10,.5) %q]% .5 # infix
#' seq(1,10,.5)[seq(1,10,.5) >= quantile(seq(1,10,.5),.5)] # regular syntax
#'
#'
NULL
# > NULL


#' Extract front by first occurrence of a value
#'
#'
#' @export
#'
#' @rdname extractors
#'
`%[f%` <- function(x, v) {
  if (is.vector(x)) {
    i <- which(x %in% v) %00% NA
    if (all(is.na(i))) {
      stop("v was not found in x")
    } else {
      return(x[1:i[1]])
    }
  }
}

#' Extract front by last occurrence of a value
#'
#' @export
#'
#' @rdname extractors
#'
`%[l%` <- function(x, v) {
  if (is.vector(x)) {
    i <- which(x %in% v) %00% NA
    if (all(is.na(i))) {
      stop("v was not found in x")
    } else {
      return(x[1:i[length(i)]])
    }
  }
}

#' Extract vector front by index
#'
#' @rdname extractors
#' @export
#'
#' @note The function provided for symmetry, character lengths of \code{x\%]\%i} and \code{x[1:i]} are equal.
#'
`%[%` <- function(x, i) {
  if (all(is.vector(x))){
    if (all(i %[]% c(1, NROW(x)))){
      if(length(i)==2){
        return(x[i[1]:i[2]])
      } else {
        return(x[1:i])
      }
    }
  }
}

#' Extract vector rear
#'
#' @export
#' @rdname extractors
#'
`%]%` <- function(x, i){
  if (all(is.vector(x))){
    if (all(i %[]% c(1, NROW(x)))){
      if(length(i)==2){
          return(x[i[1]:i[2]])
        } else {
          return(x[i:NROW(x)])
      }
    }
  }
}

#' Extract first occurrence of a value to vector rear
#'
#' @export
#' @rdname extractors
#'
`%f]%` <- function(x, v) {
  if (is.vector(x)) {
    i <- which(x %in% v) %00% NA
    if (all(is.na(i))) {
      stop("v was not found in x")
    } else {
      return(x[i[1]:NROW(x)])
    }
  }
}

#' Extract last occurrence of a value to vector rear
#'
#' @export
#' @rdname extractors
#'
`%l]%` <- function(x, v) {
  if (is.vector(x)) {
    i <- which(x %in% v) %00% NA
    if (all(is.na(i))) {
      stop("v was not found in x")
    } else {
      return(x[i[length(i)]:NROW(x)])
    }
  }
}

#' Extract values at percentile and higher
#'
#' @export
#' @rdname extractors
#'
`%q]%` <- function(x, q) {
  if (is.vector(x)) {
    i <- which(x>=stats::quantile(x=x, probs=q,na.rm = TRUE)) %00% NA
    if (all(is.na(i))) {
      stop("q was not found in x")
    } else {
      return(x[i])
    }
  }
}

#' Extract values higher than  percentile
#'
#' @export
#' @rdname extractors
#'
`%q)%` <- function(x, q) {
  if (is.vector(x)) {
    i <- which(x>stats::quantile(x=x,probs=q,na.rm = TRUE)) %00% NA
    if (all(is.na(i))) {
      stop("q was not found in x")
    } else {
      return(x[i])
    }
  }
}

#' Extract values at percentile and smaller
#'
#' @export
#' @rdname extractors
#'
`%[q%` <- function(x, q) {
  if (is.vector(x)) {
    i <- which(x<=stats::quantile(x=x,probs=q,na.rm = TRUE)) %00% NA
    if (all(is.na(i))) {
      stop("q was not found in x")
    } else {
      return(x[i])
    }
  }
}

#' Extract values smaller than percentile
#'
#' @export
#' @rdname extractors
#'
`%(q%` <- function(x, q) {
  if (is.vector(x)) {
    i <- which(x<stats::quantile(x=x,probs=q,na.rm = TRUE)) %00% NA
    if (all(is.na(i))) {
      stop("q was not found in x")
    } else {
      return(x[i])
    }
  }
}

#' Extract first, middle or last value of vector
#'
#' @rdname extractors
#' @export
#'
#'
`%:%` <- function(x, j){
  switch(j,
         f = x[1],
         m = x[round(NROW(x)/2)],
         l = x[NROW(x)]
  )
}


# Trimmers ----

#' Trim vector by index
#'
#' @param x A vector
#' @param i A 1 element vector by which the rear of \code{x} will be trimmed
#' @param j A one, or two element numeric vector. One element: Trim front by \code{floor(i/2)} and rear by \code{ceiling(i/2)}. Two elements: Trim \code{i[1]} from the front and \code{i[2]} from the rear.
#'
#' @name trimmers
#'
#' @return A trimmed version of \code{x}
#'
#' @examples
#'
#' x <- rnorm(100)
#'
#' # Trim front
#' x%[-%5
#'
#' # Trim rear
#' x%-]%5
#'
#' # Trim front + rear
#' x%[-]%c(2,10)
#'
#' x%[-]%7
#'
NULL
# > NULL


#' Trim vector front
#'
#' @rdname trimmers
#' @export
#'
#'
`%[-%` <- function(x, i) {
  if (all(is.vector(x), is.wholenumber(i[1]))) {
    return(x[-c(1:i)])
  }
}

#' Trim vector rear
#'
#' @rdname trimmers
#' @export
#'
`%-]%` <- function(x, i) {
  if (all(is.vector(x), is.wholenumber(i[1]))) {
    return(c(x[1:(length(x) - i)]))
  }
}

#' Trim vector front + rear
#'
#' @rdname trimmers
#' @export
#'
`%[-]%` <- function(x, j) {
  front <- rear <- 0
  if (all(is.vector(x), is.wholenumber(j))) {
    if (length(j) == 2) {
      front <- j[1]
      rear  <- j[2]
    }
    if (length(j) == 1) {
      front <- floor(j/2)
      rear  <- ceiling(j/2)
    }
  }
  return(c(x[-c(1:front, (NROW(x)-rear+1):NROW(x))]))
}


# Padders ----

#' Padd vector by index
#'
#' @param x A vector
#' @param j A one, or two element vector. One element: Pad \code{front} or \code{rear} by \code{j} \code{0}s, or, \code{front} by \code{floor(j/2)} and rear by \code{ceiling(j/2)}. Two elements: Pad \code{j[1]} times the value passed in \code{j[2]}.
#'
#' @name padders
#'
#' @return A padded version of \code{x}
#'
#' @examples
#'
#' x <- rnorm(100)
#'
#' # Pad front with 10 zeros
#' x%[+%10
#' # Same as
#' x%[+%c(10,0)
#'
#' # Pad rear with zeros
#' x%+]%10
#' # Same as
#' x%+]%c(10,0)
#'
#' # Pad front + rear with NA
#' x%[+]%c(NA,10)
#'
#' # Pad front + rear of a character vector
#' "yes"%[+]%c(2,"no")
#' "yes"%[+]%c(1,"no")
#' "yes"%[+]%c(0,"no")
#'
NULL

#' Pad vector front
#'
#' @rdname padders
#' @export
#'
`%[+%` <- function(x,j) {
  if (all(is.vector(x), is.wholenumber(j[1]))) {
    if (length(j) == 2) {
      return(c(rep(j[2], j[1]), x))
    } else {
      return(c(rep(0, j[1]), x))
    }
  }
}

#' Pad vector rear
#'
#' @rdname padders
#' @export
#'
`%+]%` <- function(x, j) {
  if (all(is.vector(x), is.wholenumber(j[1]))) {
    if (length(j) == 2) {
      return(c(x, rep(j[2], j[1])))
    } else {
      return(c(x, rep(0, j[1])))
    }
  }
}

#' Pad vector front + rear
#'
#' @rdname padders
#' @export
#'
`%[+]%` <- function(x, j) {
  rep <- as.numeric(j[1])
  if (all(is.vector(x), is.wholenumber(rep))) {
    front <- floor(rep / 2)
    rear <- ceiling(rep / 2)
    if (length(j) == 2) {
      return(c(rep(j[2], front), x, rep(j[2], rear)))
    } else {
      return(c(rep(0, front), x, rep(0, rear)))
    }
  }
}

#' Regress vectors
#'
#' @param x Numeric vectors
#' @param y Numeric vector
#' @param yn List of length 2, first element is a vector \code{y}, the second element an integer denoting the order of the polynomial regression.
#' @name regressors
#'
#' @examples
#'
#' x <- rnorm(100)
#' y <- x + x^2 + x^3
#'
#' # Correlate x with y
#' x%/r%y
#'
#' # Polynomial regression degree 1 .. 4
#' x%/1%y
#' x%/2%y
#' x%/3%y
#' x%/4%y
#'
#' anova(x%/1%y,x%/2%y,x%/3%y,x%/4%y)
#'
#' # Order n
#'
#' x%/n%list(y,10)
#'
NULL
# > NULL


# Regressors ----

#' Correlate x and y
#' @rdname regressors
#' @export
#'
`%/r%` <- function(x, y) {
  if (all(is.vector(x), is.vector(y))) {
    stats::cor(x, y)
  }
}

#' Polynomial regression of degree 1
#' @rdname regressors
#' @export
#'
`%/1%` <- function(x, y) {
  if (all(is.vector(x), is.vector(y))) {
    stats::lm(y ~ stats::poly(x, order = 1))
  }
}

#' Polynomial regression of degree 2
#' @rdname regressors
#' @export
#'
`%/2%` <- function(x, y) {
  if (all(is.vector(x), is.vector(y))) {
    stats::lm(y ~ stats::poly(x, order = 2))
  }
}

#' Polynomial regression of degree 3
#' @rdname regressors
#' @export
#'
`%/3%` <- function(x, y) {
  if (all(is.vector(x), is.vector(y))) {
    stats::lm(y ~ stats::poly(x, order = 3))
  }
}

#' Polynomial regression of degree 4
#' @rdname regressors
#' @export
#'
`%/4%` <- function(x, y) {
  if (all(is.vector(x), is.vector(y))) {
    stats::lm(y ~ stats::poly(x, order = 4))
  }
}

#' `%/n%` Polynomial regression of degree n
#'
#' @rdname regressors
#' @export
#'
`%/n%` <- function(x, yn = list(y = x, n = 1)) {
  if (length(yn) == 2) {
    if (all(is.vector(x), is.vector(yn[[1]]), is.wholenumber(yn[[2]]))) {
      stats::lm(yn[[1]] ~ stats::poly(x, order = yn[[2]]))
    }
  }
}

# Counters ----

#' Counters
#'
#' @param counter If \code{counter} and \code{increment} are both (signed/positive) integers \code{counter} will change by the value of \code{increment}.
#' @param increment An integer value \eqn{\neq 0} to add to \code{counter}
#'
#' @name Counters
#' @examples
#' \dontrun{
#' # Signed increment
#' # Notice the difference between passing an object and a value for counter
#'
#' # Value
#' (10 %+-% -5)
#' (10 %+-% -5)
#'
#' # Object
#' i <- 10
#' (i %+-% -5)
#' (i %+-% -5)
#'
#' # This means we can use the infix in a while ... statement
#' # WARNING: As is the case for any while ... statement, be careful not to create an infinite loop!
#'
#' i <- 10
#' while(i > -5){
#'   i %+-% -5
#'   print(i)
#' }
#'
#'
#' # Non-negative increment
#' # Notice the difference between passing an object and a value for counter
#'
#' # Value
#' (0 %++% 5)
#' (0 %++% 5)
#'
#' # Object
#' i <- 0
#' (i %++% 5)
#' (i %++% 5)
#'
#' # This means we can use the infix in a while ... statement
#' # WARNING: As is the case for any while ... statement, be careful not to create an infinite loop!
#'
#' i <- 0
#' while(i < 20){
#' i %++% 5
#' print(i)
#' }
#'}
#'
NULL

#' Signed increment
#'
#' @export
#' @rdname Counters
#'
`%+-%` <- function(counter, increment) {
  if (any(is.na(counter %00% NA) | is.na(increment %00% NA) | !is.wholenumber(counter) | !is.wholenumber(increment) | increment == 0)) {
    stop("Don't know how to work with counter and/or increment argument.\n Did you use integers?")
  } else {
    result <- counter + increment
    if (counter > 0 & result <= 0) {
      warning("Positive valued counter changed sign (counter <= 0)!")
    }
    if (counter < 0 & result >= 0) {
      warning("Negative valued counter changed sign (counter >= 0)!")
    }
    obj <- suppressWarnings(as.numeric(deparse(substitute(counter)))%00%NA)
    if (is.na(obj)) {
      eval(parse(text = paste(deparse(substitute(counter)), " <<- result")))
    } else {
      return(result)
    }
  }
}

#'  Non-negative increment
#'
#' @export
#' @rdname Counters
#'
`%++%` <- function(counter, increment) {
  if (any(is.na(counter %00% NA) | is.na(increment %00% NA) | !is.wholenumber(counter) | !is.wholenumber(increment) | increment <= 0 | counter < 0)) {
    stop("Don't know how to work with counter and/or increment argument.\n Did you use integers?")
  } else {
    result <- counter + increment
    obj <- suppressWarnings(as.numeric(deparse(substitute(counter)))%00%NA) #try_CATCH(as.numeric(deparse(substitute(counter))))
    if (is.na(obj)) {
      eval(parse(text = paste(deparse(substitute(counter)), " <<- result")))
    } else {
      return(result)
    }
  }
}


#' Inside interval
#'
#' Decide if a value \code{x} falls inside an interval \code{j[1],j[2]} that can be open or closed on the left and/or the right. Either a logical vector equal to \code{x}, or the actual values are extracted, when the `.`-versions are used.
#'
#' @param x A vector
#' @param j A 2-element numeric vector indicating a range
#'
#' @note Package `DescTools` provides similar functions
#'
#' @name insiders
#'
#' @return Logical vector of length \code{x}, or, values in the range \code{j}
#'
#' @examples
#'
#' # Closed interval
#' 0:5 %[]% c(1,5)  # logical vector
#' 0:5 %[.]% c(1,5) # extract values
#'
#' # Open interval
#' 0:5 %()% c(1,5)
#' 0:5 %(.)% c(1,5)
#'
#' # Closed interval left
#' 0:5 %[)% c(1,5)
#' 0:5 %[.)% c(1,5)
#'
#' # Closed interval right
#' 0:5 %(]% c(1,5)
#' 0:5 %(.]% c(1,5)
#'
#'
NULL
# >NULL


# Insiders ----

#'  In closed interval
#'
#' @rdname insiders
#' @export
#'
#'
`%[]%` <- function(x, j) {
  if(all(length(j) == 2, is.numeric(x), is.numeric(j))){
    rng <- sort(j)
    x >= rng[1] & x <= rng[2]
  }
}

#'  In open interval
#'
#' @rdname insiders
#' @export
#'
#'
`%()%` <- function(x, j) {
  if (all(length(j) == 2, is.numeric(x), is.numeric(j))) {
    rng <- sort(j)
    x > rng[1] & x < rng[2]
  }
}

#'  In half-closed interval (left)
#'
#' @rdname insiders
#' @export
#'
#'
`%[)%` <- function(x, j) {
  if (all(length(j) == 2, is.numeric(x), is.numeric(j))) {
    rng <- sort(j)
    x >= rng[1] & x < rng[2]
  }
}

#'  In half-closed interval (right)
#'
#' @rdname insiders
#' @export
#'
#'
`%(]%` <- function(x, j) {
  if (all(length(j) == 2, is.numeric(x), is.numeric(j))) {
    rng <- sort(j)
    x > rng[1] & x <= rng[2]
  }
}


#'  Return x in closed interval
#'
#' @rdname insiders
#' @export
#'
#'
`%[.]%` <- function(x, j) {
  x[x%[]%j]
}

#'  Return x in open interval
#'
#' @rdname insiders
#' @export
#'
#'
`%(.)%` <- function(x, j) {
    x[x%()%j]
}

#'  Return x in half-closed interval (left)
#'
#' @rdname insiders
#' @export
#'
#'
`%[.)%` <- function(x, j) {
  x[x%[)%j]
}

#'  Return x in half-closed interval (right)
#'
#' @rdname insiders
#' @export
#'
#'
`%(.]%` <- function(x, j) {
  x[x%(]%j]
}



# Outsiders -----

#' Outside interval
#'
#' Decide if a value \code{x} falls outside an interval \code{j[1],j[2]} that can be open or closed on the left and/or the right. Either a logical vector equal to \code{x}, or the actual values are extracted,
#'
#' @param x A vector
#' @param j A range
#'
#' @note Package `DescTools` provides similar functions
#'
#' @name outsiders
#'
#' @return logical vector of length x, or, values of x outside the range j
#'
#' @examples
#'
#' # Closed interval
#' 5%][%c(1,5)
#' 5%].[%c(1,5)
#'
#' # Open interval
#' 5%)(%c(1,5)
#' 5%).(%c(1,5)
#'
#' # Half-losed interval left
#' 5%](%c(1,5)
#' 5%].(%c(1,5)
#'
#' # Half-losed interval right
#' 5%)[%c(1,5)
#' 5%).[%c(1,5)
#'
#'
NULL
# >NULL


#'  Not in closed interval
#'
#' @rdname outsiders
#' @export
#'
`%][%` <- function(x, j) {
  return(!x%()%j)
}

#'  Not in open interval
#'
#' @rdname outsiders
#' @export
#'
#'
`%)(%` <- function(x, j) {
  return(!x%[]%j)
}

#'  Not in half-closed interval (left)
#'
#' @rdname outsiders
#' @export
#'
#'
`%](%` <- function(x, j) {
  return(!x%(]%j)
}

#'  Not in half-closed interval (right)
#'
#' @rdname outsiders
#' @export
#'
#'
`%)[%` <- function(x, j) {
  return(!x%[)%j)
}


#'  Return x not in closed interval
#'
#' @rdname outsiders
#' @export
#'
`%].[%` <- function(x, j) {
  return(x[!x%()%j])
}

#'  Return x not in open interval
#'
#' @rdname outsiders
#' @export
#'
#'
`%).(%` <- function(x, j) {
  return(x[!x%[]%j])
}

#'  Return x not in half-closed interval (left)
#'
#' @rdname outsiders
#' @export
#'
#'
`%].(%` <- function(x, j) {
  return(x[!x%(]%j])
}

#'  Return x not in half-closed interval (right)
#'
#' @rdname outsiders
#' @export
#'
#'
`%).[%` <- function(x, j) {
  return(x[!x%[)%j])
}


# fINDexers -----

#' Find row or column by name or index
#'
#' @param c Column name or index
#' @param r Row name or index
#' @param rc A 2-element numeric or character vector representing \code{c(r,c)}. Names (character) and indices (numeric) vectors can be mixed if \code{rc} is passed as a 2-element list object.
#' @param nv A numeric value, or vector of values of which you want to know the indices in \code{d}.
#' @param d A named vector, list, matrix, or data frame
#'
#' @return If \code{r/c/rc} is numeric, the name corresponding to the row/column index of \code{d}, if \code{r/c/rc} is a character vector, the row/column index corresponding to the row/column name. If \code{dimnames(d) == NULL}, but \code{names(d) != NULL} then \code{\%ci\%} and \code{\%ri\%} will look up \code{r/c} in \code{names(d)}
#'
#' @name fINDexers
#'
#' @author Fred Hasselman
#'
#' @examples
#'
#' # data frame
#' d <- data.frame(x=1:5,y=6,row.names=paste0("ri",5:1))
#'
#' "y" %ci% d # y is the 2nd column of d
#'   2 %ci% d # the name of the second column of d is "y"
#'
#'     2 %ri% d
#' "ri5" %ri% d
#'
#' # change column name
#'  colnames(d)["y" %ci% d] <- "Yhat"
#'
#' # mi works on data frames, matrices, tiblles, etc.
#'  c(5,2) %mi% d
#'  list(r="ri1",c=2) %mi% d
#'
#' # matrix row and column indices
#' m <- matrix(1:10,ncol=2, dimnames = list(paste0("ri",0:4),c("xx","yy")))
#'
#'  1 %ci% m
#'  5 %ci% m # no column 5
#'
#'  1 %ri% m
#'  5 %ri% m
#'
#'  c(5,1)%mi%m
#'  c(1,5)%mi%m
#'
#' # For list and vector objects ri and ci return the same values
#' l <- list(a=1:100,b=LETTERS)
#'
#'   2 %ci% l
#' "a" %ci% l
#'
#'   2 %ri% l
#' "a" %ri% l
#'
#' # named vector
#' v <- c("first" = 1, "2nd" = 1000)
#'
#' "2nd" %ci% v
#'     1 %ci% v
#'
#' "2nd" %ri% v
#'     1 %ri% v
#'
#' # get all indices of the number 1 in v
#'  1 %ai% v
#'
#' # get all indices of the number 3 and 6 in d
#'  c(3,6) %ai% d
#'
#' # get all indices of values: Z < -1.96 and Z > 1.96
#'  Z <- rnorm(100)
#'  Z[Z%)(%c(-1.96,1.96)] %ai% Z
#'
#'
NULL

#' Column by name or index
#'
#' @rdname fINDexers
#' @export
#'
`%ci%` <- function(c, d) {
  if (all(!is.null(dimnames(d)[[2]]), any(is.numeric(c), is.character(c)))){
    if(is.character(c)){
      return(which(dimnames(d)[[2]]%in%c))
    } else {
      return(dimnames(d)[[2]][c])
    }
  } else {
    if(!is.null(names(d))){
      if(is.character(c)){
        return(which(names(d)%in%c))
      } else {
        return(names(d)[c])
      }
    }
  }
  return(NA)
}

#' Row by name or number
#'
#' @rdname fINDexers
#' @export
#'
`%ri%` <- function(r, d) {
  if (all(!is.null(dimnames(d)[[1]]), any(is.numeric(r), is.character(r)))){
    if(is.character(r)){
      return(which(dimnames(d)[[1]]%in%r))
    } else {
      return(dimnames(d)[[1]][r])
    }
   } else {
    if(!is.null(names(d))){
      if(is.character(r)){
        return(which(names(d)%in%r))
      } else {
        return(names(d)[r])
      }
    }
   }
  return(NA)
}

#' Matrix cell index by name or number
#'
#' @rdname fINDexers
#' @export
#'
`%mi%` <- function(rc,d) {
  if (all(!is.null(dimnames(d)[[1]]), any(is.numeric(unlist(rc)), is.character(unlist(rc))),length(rc)==2)){
      rr <- rc[[1]]%ri%d
      cc <- rc[[2]]%ci%d
      if(is.list(rc)){
        out <- list(rr,cc)
        names(out) <- names(rc)
        return(out)
      }
      return(c(rr,cc))
  } else {
    message("d is a vector or list object")
  }
  return(NA)
}


#' Return all indices of a (range of) values
#'
#' @rdname fINDexers
#' @export
#'
`%ai%` <- function(nv,d) {
  if(all(is.numeric(unlist(nv)),dim(data.frame(nv))[2]==1)){
    names(nv) <- paste0(nv)
    out <- plyr::ldply(nv, function(n) which(d==n,arr.ind = TRUE), .id = "nv")
    return(out)
    } else {
    message("nv must be a numeric vector.")
      return(NA)
  }
}


#' Is element of... with multiple input types
#'
#' @param x A vector, data frame or list containing numbers and/or characters that could be elements of y
#' @param y An object that could contain values in x
#'
#' @return Logical vector indicating which x are an element of y
#'
#' @rdname fINDexers
#'
#' @export
#'
`%e%` <- function(x,y){
  outTable <- list()
  if(any(is.list(x), is.numeric(x), is.character(x))){
    if(!is.null(dim(x))){
      outTable <- coliter(cin = x, table = y)
    } else {
      if(!is.list(x)){x<-list(x=x)}
      outTable <- sapply(x, coliter, table = y)
    }
  }
 return(outTable)
}


# Helpers ----

coliter <- function(cin,table){

  if(is.null(dim(cin))){
    if(is.list(cin)){stop("Input structure too complex: list containing a list.")}
    cin <- data.frame(element=cin, stringsAsFactors = FALSE)
  }

  out <- list()
  #names(table) <- paste0("e.",unlist(table))
  #table <- as.data.frame(table)
  for(c in 1:NCOL(cin)){
    if(all(is.numeric(cin[,c]%00%NaN))){
      cin[,c] <- cin[,c]%00%NaN
      }
    if(all(is.character(cin[,c]%00%NA_character_))){
      cin[,c] <- cin[,c]%00%NaN
    }
    elements <- as.list(cin[,c])
    names(elements) <- paste0(cin[,c])
    out[[c]] <- plyr::ldply(elements, function(n){
      outTable <- as.data.frame(table)
      for(ct in 1:NCOL(table)){
        outTable[,ct] <- table[,ct]%in%n
      }
      return(outTable)
      })
  }
  names(out) <- colnames(cin)
  plyr::ldply(out,.id="variable")
  return(out)
}



#' Wholenumber check
#'
#' @param x Number to check
#' @param tol Tolerance
#'
#' @return TRUE if \code{x} is a signed integer.
#' @export
#'
#' @keywords internal
#'
#' @examples
#'
#' is.wholenumber(1.2)
#' is.wholenumber(1)
#'
#' @note This code was found in the examples of \link[base]{is.integer}.
#'
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  if(!is.numeric(x)|all(is.na(x%00%NA))){
    return(FALSE)
  } else {
    # NAs to FALSE
    NAind <- is.na(x%00%NA)
    if(sum(NAind)>0){
      x[NAind] <- 0.5
    }
    return(abs(x - round(x)) < tol)
  }
}


