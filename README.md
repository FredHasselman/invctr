---
**INVCTR**
---

# Vector operations between grapes <img src="man/figures/logo.png" align="right" alt="" width="120" />

This package contains only `infix` functions (`grapes-function-grapes`), most of them shorten the syntax for common vector operations, such as finding indices and extracting values from vectors.

The main function classes are:

* _Extractors_ - Extract indices or values from the front or the rear of a vector
* _Insiders & Outsiders_ - Determine whether values in a vector are inside or outside a specific range and/or exctract those values
* _Counters_ - Automatically increment a value or a numerical object (e.g. in a while loop)
* _Padders & Trimmers_ - Padd or Trim the front and/or rear of a vector
* _Regressors_ - Perform simple polynomial regression on vectors

In addition, some infix functions for very specific tasks are provided, e.g., _the rose tinted infix_ `%00%` provides a means to deal with functions that return `NA`, `Inf`, `NULL`or other error/exception values by turning them into a user specified value, or, `x %e% y` which will check whether elements of `x` are in `y` irrespective of the object type of `x` ND `Y`.

