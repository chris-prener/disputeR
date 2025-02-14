
<!-- README.md is generated from README.Rmd. Please edit that file -->

# disputeR

<!-- badges: start -->
<!-- badges: end -->

The goal of `disputeR` is to easily implement unit tests within
functions and Shiny modules. Unlike other packages that provide
frameworks for developing these tests, `disputeR` takes a different
approach, offering pre-built unit tests that can easily be dropped into
functions and test for basic requirements, like class, length, and
common values used in function writing (such as `NULL`). These tests
return informative error messages that follow the ‘tidyverse’ style
guide’s recommendations.

## Installation

You can install the development version of disputeR from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("chris-prener/disputeR")
```

## Basic Usage

The following example creates a simple function for squaring a numeric
value, which is passsed to the function using the parameter `x`. Two
`disputeR` functions are added before squaring `x`. The first,
`dis_not_missing()`, evaluates `x` to ensure the parameter is not
skipped by the user. The second, `dis_numeric()`, applies a number of
checks to `x`.

``` r
## load package
library(disputeR)

## define example function
example <- function(x){

   ## check inputs with disputeR
   dis_not_missing(.f = missing(x))
   dis_numeric(x, null_valid = FALSE)

   ## square
   out <- x^2

   ## return output
   return(out)

}
```

The checks `dis_numeric()` applies include default settings that require
a scalar input (i.e. `length(x) == 1`) and do not allow `NA`, `NaN`,
`Inf`, or `-Inf`. Each of these can be enabled using the parameters for
`dis_numeric()` at the function author’s discretion. In addition, we
enforce a requirement that `x` cannot be `NULL`.

In the case of the following example, `2` is a valid input to
`example()` and the function executes without error:

``` r
example(x = 2)
#> [1] 4
```

However, if the user does not specify the `x` argument, an informative
error message is returned along with instructions for how to rectify the
issue:

``` r
example()
#> Error in example(): argument "x" is missing, with no default
```

Likewise, if `x` is incorrectly specified (such as with a
`class(x) == character` value), a similar message along with
instructions for fixing the error is returned:

``` r
example(x = "test")
#> Error in `example()`:
#> ! `x` must be a <numeric> scalar, not a <character> scalar.
#> ℹ Provide a <numeric> scalar for `x`, such as `x = 1`.
```

A similar approach can be taken for both character and logical
arguments. For example :

``` r
example <- function(x, round = FALSE){

   ## check inputs with disputeR
   dis_not_missing(.f = missing(x))
   dis_numeric(x, null_valid = FALSE)
   dis_logical(round, null_valid = FALSE)

   ## square
   out <- x^2
   
   ## optionally round
   if (isTRUE(round)){
     out <- round(out, digits = 2)
   }

   ## return output
   return(out)

}
```

The use of `dis_logical()` ensures that a non-`NULL`, logical argument
(either `TRUE` or `FALSE`) is passed to the `round` parameter. This
allows us to validate or dispute user inputs:

``` r
## no issues
example(x = 2.25)
#> [1] 5.0625
example(x = 2.25, round = TRUE)
#> [1] 5.06

## error found
example(x = 2.25, round = "yes")
#> Error in `example()`:
#> ! `yes` must be a <logical> scalar, not a <character> scalar.
#> ℹ Provide a <logical> scalar for `yes`, such as `round = TRUE` or `round =
#>   FALSE`.
```

Both `dis_numeric()` and `dis_character()` also accept strings of valid
values. The following function allows for only certain words to be
passed to the `x` parameter:

``` r
example <- function(x){

   ## check inputs with disputeR
   dis_not_missing(.f = missing(x))
   dis_character(x, valid = c("ham", "eggs", "bacon"), null_valid = FALSE,)

   ## create output
   out <- paste0(x, " is delicious!")

   ## return output
   return(out)

}
```

The value for `x` will be checked against the vector passed to the
`valid` parameter in addition to the other checks that `dis_character()`
performs:

``` r
## no issues
example(x = "bacon")
#> [1] "bacon is delicious!"

## error found
example(x = "spam")
#> Error in `example()`:
#> ! `x` must be a valid value for the parameter `x`, not `spam`.
#> ℹ Valid arguments for `x` are: `ham`, `eggs`, and `bacon`.
```

The `valid` argument works the same way with `dis_numeric()`, passing
either integer or numeric values to `valid` instead of characters.

## Code of Conduct

Please note that the disputeR project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
