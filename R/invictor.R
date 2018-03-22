#' Rose tinted infix
#'
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
#' Inf %00% NA
#'
#' numeric(0) %00% ''
#'
#' NA %00% 0
#'
#' NaN %00% NA
#'
#' NULL %00% NA
#'
`%00%` <- function(x, y) {
  if (length(x) == 0) {
    x <- y
  } else {
    for (i in seq_along(x)) {
      l0 <- isna <- isnan <- isinf <- isnll <- isTryError <- FALSE
      if (length(x[i]) == 0) {
        l0 <- TRUE
      } else {
        if (all(is.na(x[i]))) isna <- TRUE
        if (all(is.nan(x[i]))) isnan <- TRUE
        if (all(is.infinite(x[i]))) isinf <- TRUE
        if (all(is.null(x[i]))) isnll <- TRUE
        if (all(class(x[i]) %in% "try-error")) isTryError <- TRUE
      }
      if (any(l0, isna, isnan, isinf, isnll, isTryError)) {
        x[i] <- y
      }
    }
  }
  return(x)
}


#' Extract vectors by index
#'
#' @param x A vector
#' @param i An index
#' @param j A two element vector \code{}  indicating a range to extract
#'
#' @name extractors
#'
#' @return A vector extracted from the front, rear, or, center of \code{x}. Either based on an index or the occurrence of a value.
#'
#' @examples
#'
#' # Extract front by value
#' z <- letters
#'
#' z%[1%"n"
#'
#' # Extract front by index
#' x <- rnorm(100)
#' x%[%10
#'
#' # Extract rear by index
#' 90%]%x
#'
#' # Extract center by indices
#' x%[.]%c(4,30)
#' z%[.]%c(6,10)
#'
NULL
# > NULL


#' Extract front by 1st occurrence
#'
#' Extract vector front up and untill first occurence of \code{i} in \code{x}
#'
#' @return The contents of vector \code{x} in the range \code{x[1:i]}
#' @export
#'
#' @rdname extractors
#'
`%[1%` <- function(x, i) {
  if (all(is.vector(x), is.wholenumber(i))) {
    ind <- which(x %in% i) %00% NA
    if (is.na(ind)) {
      stop("i was not found in x")
    } else {
      return(x[1:ind])
    }
  }
}

#' Extract vector front by index
#'
#' @rdname extractors
#' @export
#' @note This is provided for symmetry, character lengths of \code{x\%]\%i} and \code{x[1:i]} are equal
#'
`%[%` <- function(x, i) {
  if (all(is.vector(x), is.wholenumber(i))) {
    return(x[1:i])
  }
}

#' Extract vector rear
#'
#' @param i An index in the range \code{[1,length(x)]}
#' @param x A vector
#'
#' @return The contents of vector \code{x} in the range \code{x[i:length(x)]}
#'
#' @export
#'
#' @family vector extractors
#'
`%]%` <- function(i, x) {
  if (all(is.vector(x), is.wholenumber(i))) {
    if (i %[]% c(1, length(x))) {
      return(x[i:length(x)])
    }
  }
}


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
#' 5%[-%x
#'
#' # Trim rear
#' 5%-]%x
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
`%[-%` <- function(i, x) {
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
  if (length(j) == 2) {
    if (all(is.vector(x), is.wholenumber(j[1]), is.wholenumber(j[2]))) {
      front <- floor(j[2] / 2)
      rear <- ceiling(j[2] / 2)
      return(c(x[-c(front, rear)]))
    }
  }
}


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
#' 10%[+%x
#' # Same as
#' c(10,0)%[+%x
#'
#' # Pad rear with zeros
#' x%+]%10
#' # Same as
#' x%+]%c(10,0)
#'
#' # Pad front + rear with NA
#' x%[+]%c(NA,10)
#'
#' # Pad front + rear of character vector
#' "yes"%[+]%c(2,"no")
#' "yes"%[+]%c(1,"no")
#' "yes"%[+]%c(0,"no")

#' Pad vector front
#'
#' @rdname padders
#' @export
#'
`%[+%` <- function(j, x) {
  if (all(is.vector(x), is.wholenumber(j[1]))) {
    if (length(j) == 2) {
      return(c(rep(j[2], j[1]), x))
    } else {
      return(c(x, rep(0, j[1])))
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

#' Correlate x and y
#' @rdname regressors
#' @export
#'
`%/r%` <- function(x, y) {
  if (all(is.vector(x), is.vector(y))) {
    cor(x, y)
  }
}

#' Polynomial regression of degree 1
#' @rdname regressors
#' @export
#'
`%/1%` <- function(x, y) {
  if (all(is.vector(x), is.vector(y))) {
    lm(y ~ poly(x, order = 1))
  }
}

#' Polynomial regression of degree 2
#' @rdname regressors
#' @export
#'
`%/2%` <- function(x, y) {
  if (all(is.vector(x), is.vector(y))) {
    lm(y ~ poly(x, order = 2))
  }
}

#' Polynomial regression of degree 3
#' @rdname regressors
#' @export
#'
`%/3%` <- function(x, y) {
  if (all(is.vector(x), is.vector(y))) {
    lm(y ~ poly(x, order = 3))
  }
}

#' Polynomial regression of degree 4
#' @rdname regressors
#' @export
#'
`%/4%` <- function(x, y) {
  if (all(is.vector(x), is.vector(y))) {
    lm(y ~ poly(x, order = 4))
  }
}

#' `%/n%` Polynomial regression of degree n
#' @rdname regressors
#' @export
#'
`%/n%` <- function(x, yn = list(y = x, n = 1)) {
  if (length(y) == 2) {
    if (all(is.vector(x), is.vector(yn[[1]]), is.wholenumber(yn[[2]]))) {
      lm(yn[[1]] ~ poly(x, order = yn[[2]]))
    }
  }
}


#' Signed increment
#'
#' Increment an integer counter by an arbitrary (signed) interval.
#'
#' @param counter If \code{counter} and \code{increment} are both a (signed) integers \code{counter} will change by the value of \code{increment}.
#' @param increment An integer value \eqn{\neq 0} to add to \code{counter}
#'
#' @export
#' @author Fred Hasselman
#'
#' @seealso %++%
#'
#' @family auto counters
#'
#' @examples
#'
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
#' while(i > -3) i %+-% -5
#'
`%+-%` <- function(counter, increment) {
  if (is.na(counter %00% NA) | is.na(increment %00% NA) | !is.wholenumber(counter) | !is.wholenumber(increment) | increment == 0) {
    stop("Don't know how to work with counter and/or increment argument.\n Did you use integers?")
  } else {
    result <- counter + increment
    if (counter > 0 & result <= 0) {
      warning("Positive valued counter changed sign (counter <= 0)!")
    }
    if (counter < 0 & result >= 0) {
      warning("Negative valued counter changed sign (counter >= 0)!")
    }
    obj <- try_CATCH(as.numeric(deparse(substitute(counter))))
    if (is.na(obj$value)) {
      eval(parse(text = paste(deparse(substitute(counter)), " <<- result")))
    } else {
      return(result)
    }
  }
}

#'  Non-negative increment
#'
#' Increment a counter by an arbitrary interval greater than 0.
#'
#' @param counter If \code{counter} \eqn{\ge 0} and \code{increment} \eqn{> 0} and are both integers, \code{counter} will change by the value of \code{increment}.
#' @param increment An integer value \eqn{> 0} to add to \code{counter}
#'
#' @export
#' @author Fred Hasselman
#'
#' @seealso %+-%
#'
#' @family auto counters
#'
#' @examples
#'
#' # Notice the difference between passing an object and a value for counter
#'
#' # Value
#' (0 %++% 5)
#' (0 %++% 5)
#'
#' # Object
#' i <- 0
#' (i %+-% 5)
#' (i %+-% 5)
#'
#' # This means we can use the infix in a while ... statement
#' # WARNING: As is the case for any while ... statement, be careful not to create an infinite loop!
#'
#' while(i < 20) i %+-% 5
#'
`%++%` <- function(counter, increment) {
  if (is.na(counter %00% NA) | is.na(increment %00% NA) | !is.wholenumber(counter) | !is.wholenumber(increment) | increment <= 0 | counter < 0) {
    stop("Don't know how to work with counter and/or increment argument.\n Did you use integers?")
  } else {
    result <- counter + increment
    obj <- try_CATCH(as.numeric(deparse(substitute(counter))))
    if (is.na(obj$value)) {
      eval(parse(text = paste(deparse(substitute(counter)), " <<- result")))
    } else {
      return(result)
    }
  }
}


#' Inside interval
#'
#' Decide if a value \code{x} falls within an interval \code{j[1],j[2]}
#'
#' @param x A vector
#' @param j A range
#'
#' @name outsiders
#'
#' @return True or false
#'
#' @examples
#'
#' # Closed interval
#' 5%[]%c(1,5)
#'
#' # Open interval
#' 5%()%c(1,5)
#'
#' # Closed interval left
#' 5%[)%c(1,5)
#'
#' # Closed interval right
#' 5%(]%c(1,5)
#'
NULL
# >NULL


#'  Closed interval
#'
#' @rdname outsiders
#' @export
#'
#' @seealso \link{[DescTools]}
#'
`%[]%` <- function(x, j) {
  if (all(length(j) == 2, is.numeric(x), is.numeric(j))) {
    rng <- sort(j)
    x >= rng[1] & x <= rng[2]
  }
}

#'  Open interval
#'
#' @rdname outsiders
#' @export
#'
#' @seealso \link{[DescTools]}
#'
`%()%` <- function(x, j) {
  if (all(length(j) == 2, is.numeric(x), is.numeric(j))) {
    rng <- sort(j)
    x > rng[1] & x < rng[2]
  }
}

#'  Closed interval (left)
#'
#' @rdname outsiders
#' @export
#'
#' @seealso \link{[DescTools]}
#'
`%[)%` <- function(x, j) {
  if (all(length(j) == 2, is.numeric(x), is.numeric(j))) {
    rng <- sort(j)
    x >= rng[1] & x < rng[2]
  }
}

#'  Closed interval (right)
#'
#' @rdname outsiders
#' @export
#'
#' @seealso \link{[DescTools]}
#'
`%(]%` <- function(x, j) {
  if (all(length(j) == 2, is.numeric(x), is.numeric(j))) {
    rng <- sort(j)
    x > rng[1] & x <= rng[2]
  }
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
#' @note This code was found in the examples of \link[base]{is.integer}.
#'
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  return(abs(x - round(x)) < tol)
}


#' TRY ... CATCH
#'
#' @description
#'  In longer simulations, aka computer experiments,
#'  you may want to
#'  1) catch all errors and warnings (and continue)
#'  2) store the error or warning messages
#'
#'  Here's a solution  (see \R-help mailing list, Dec 9, 2010):
#'
#' Catch *and* save both errors and warnings, and in the case of
#' a warning, also keep the computed result.
#'
#' @title tryCatch both warnings (with value) and errors
#' @param expr an \R expression to evaluate
#' @return a list with 'value' and 'warning', where value' may be an error caught.
#' @author Martin Maechler; Copyright (C) 2010-2012  The R Core Team
#' @export
#' @keywords internal
#'
try_CATCH <- function(expr) {
  W <- NULL
  w.handler <- function(w) { # warning handler
    W <<- w
    invokeRestart("muffleWarning")
  }
  list(
    value = withCallingHandlers(tryCatch(expr, error = function(e) e),
      warning = w.handler
    ),
    warning = W
  )
}
