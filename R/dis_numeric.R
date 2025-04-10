#' Validate Numeric Arguments
#'
#' @description This function runs standard unit tests on numeric and integer
#'     parameters for functions.
#'
#' @usage dis_numeric(x, valid = NULL, null_valid = TRUE, na_valid = FALSE,
#'     nan_valid = FALSE, infinite_valid = FALSE, integer = FALSE, scalar = TRUE,
#'     param = NULL, call = NULL, fact_check = "global")
#'
#' @param x Required object; a parameter argument to test.
#' @param valid Optional numeric scalar or vector; valid values for the parameter.
#' @param null_valid Required logical scalar; whether the parameter can be \code{NULL}.
#'     If \code{FALSE}, the function will throw an error if \code{x} is \code{NULL}.
#'     Default is \code{TRUE}.
#' @param na_valid Required logical scalar; whether the parameter can be \code{NA}.
#'     If \code{FALSE}, the function will throw an error if \code{x} is \code{NA}.
#'     Default is \code{FALSE}.
#' @param nan_valid Required logical scalar; whether the parameter can be \code{NaN}.
#'     If \code{FALSE}, the function will throw an error if \code{x} is \code{NaN}.
#'     Default is \code{FALSE}.
#' @param infinite_valid Required logical scalar; whether the parameter can be \code{Inf}
#'     or \code{-Inf}. If \code{FALSE}, the function will throw an error if \code{x}
#'     is infinite. Default is \code{FALSE}.
#' @param integer Required logical scalar; whether the parameter must be an integer.
#'     If \code{TRUE}, the function will throw an error if \code{x} is not an integer.
#'     Default is \code{FALSE}.
#' @param scalar Required logical scalar; whether the parameter must be a scalar.
#'     If \code{TRUE} (default), the function will throw an error if \code{x} is
#'     not a scalar.
#' @param param Optional character scalar; the parameter name. If \code{NULL} (default),
#'     the function will attempt to determine the parameter name from the calling
#'     environment. If nesting functions, it is recommended to provide the parameter
#'     name to ensure the correct parameter is referenced using \code{rlang::caller_arg()}.
#' @param call Optional environment; the environment in which the function was called.
#'     If \code{NULL} (default), the function will attempt to determine the calling
#'     environment. If nesting functions, it is recommended to provide the calling
#'     environment to ensure the correct environment is referenced using
#'     \code{rlang::caller_env()}.
#' @param fact_check Required character scalar; whether to override fact checking
#'     environment setting. If \code{"global"} (default), \code{dis_character} will
#'     follow the global setting. If \code{"always"}, \code{dis_character} will
#'     ignore any global setting and will always check \code{x}. This argument is
#'     primarily intended for Shiny developers who wish to use \code{disputeR} in
#'     modules. See the vignette on \code{vignette("developing", package = "disputeR")}
#'     for details on how to use this function.
#'
#' @return This function will return either \code{TRUE} (if the input passes
#'     all validation checks) or an error message. Note that, if the input is \code{NULL},
#'     \code{NA}, \code{NaN}, or infinite and the appropriate arguments are set
#'     to \code{TRUE}, the detailed unit tests are skipped and the function will
#'     return \code{TRUE}.
#'
#' @details See the vignette on \code{vignette("developing", package = "disputeR")}
#'     for details about internal validation of arguments for this function.
#'
#' @examples
#' # create example function that uses dis_numeric()
#' example <- function(x){
#'
#'   ## check inputs with disputeR
#'   dis_not_missing(.f = rlang::is_missing(x))
#'   dis_numeric(x, null_valid = FALSE)
#'
#'   ## square
#'   out <- x^2
#'
#'   ## return output
#'   return(out)
#'
#' }
#'
#' # test example function
#' example(x = 2)
#'
#' @export
dis_numeric <- function(x, valid = NULL, null_valid = TRUE, na_valid = FALSE,
                        nan_valid = FALSE, infinite_valid = FALSE,
                        integer = FALSE, scalar = TRUE, param = NULL,
                        call = NULL, fact_check = "global"){

  ## store environment if not provided
  if (is.null(call)){
    call <- rlang::caller_env()
  }

  ## check fact_check and set path
  path <- dis_checker(fact_check = fact_check, call = call)

  ## check x if path == TRUE, return TRUE if all checks pass
  if (isTRUE(path)){

    ## store parameter name if not provided
    if (is.null(param)){
      param <- rlang::caller_arg(x)
    }

    ## check inputs
    if (isTRUE(Sys.getenv(x = "DISPUTER_DEV_CHECK"))){

      ### check call
      dis_environment(call)

      ### check param
      dis_param(param)

      ### check x
      dis_not_missing(.f = rlang::is_missing(x))

      ### check valid
      if (!is.null(valid)){
        if (integer == TRUE & !is.integer(valid)){
          cli::cli_abort(
            message = c(
              paste0("{.code valid} must be an integer, not ", dis_indefinite(x), " {.cls {class(valid)}}."),
              "i" = "Provide an integer for {.code valid}, such as {.code valid = 1L} or {.code valid = c(1L:5L)}."
            ),
            call = call
          )
        } else if (integer == FALSE & !is.numeric(valid)){
          cli::cli_abort(
            message = c(
              paste0("{.code valid} must be a numeric, not ", dis_indefinite(x), " {.cls {class(valid)}}."),
              "i" = "Provide a numeric for {.code valid}, such as {.code valid = 1} or {.code valid = c(1:5)}."
            ),
            call = call
          )
        }
      }

      ### check remaining logical parameters
      dis_logical(null_valid)
      dis_logical(na_valid)
      dis_logical(nan_valid)
      dis_logical(infinite_valid)
      dis_logical(integer)
      dis_logical(scalar)

    }

    ## check x
    ### set initial values
    if (isTRUE(integer)){
      class <- "integer"
    } else {
      class <- "numeric"
    }

    if (isTRUE(scalar)){
      type <- "scalar"

      if (isTRUE(integer)){
        stem <- "{.code {param} = 1L}"
      } else {
        stem <- "{.code {param} = 1}"
      }

    } else if (isFALSE(scalar)){
      type <- "vector"

      if (isTRUE(integer)){
        stem <- "{.code {param} = 1L} or {.code {param} = c(1L:5L)}"
      } else {
        stem <- "{.code {param} = 1} or {.code {param} = c(1:5)}"
      }

    }

    if (length(x) > 1 & isFALSE(scalar)){
      stem_a <- "contain"
      stem_b <- "a vector of non-missing values"
      stem_c <- "a vector of possible values"
      stem_d <- "a vector of non-infinite values"
    } else if (length(x) == 1){
      stem_a <- "be"
      stem_b <- "a non-missing value"
      stem_c <- "a possible value"
      stem_d <- "a non-infinite value"
    }

    ### test that x is a scalar
    if (isTRUE(scalar) & length(x) != 1){
      cli::cli_abort(
        message = dis_msg_scalar(class = class, stem = stem), call = call
      )
    }

    ### check that x is not NULL, NA, NaN, or Inf/-Inf
    dis_null(x = x, class = class, null_valid = null_valid, param = param, call = call)

    if (!nan_valid & any(is.nan(x))){

      cli::cli_abort(
        message = c(
          paste0("{.arg {param}} must not ", stem_a, " {.code NaN}."),
          "i" = paste0("Provide ", stem_c, " (i.e. do not divide by zero) for {.arg {param}}.")
        ),
        call = call
      )
    }

    if (!na_valid & any(is.na(x))){

      cli::cli_abort(
        message = c(
          paste0("{.arg {param}} must not ", stem_a, " {.code NA}."),
          "i" = paste0("Provide ", stem_b, " for {.arg {param}}.")
        ),
        call = call
      )
    }

    if (!infinite_valid & any(is.infinite(x))){

      cli::cli_abort(
        message = c(
          paste0("{.arg {param}} must not ", stem_a, " {.code Inf} or {.code -Inf}."),
          "i" = paste0("Provide ", stem_d, " for {.arg {param}}.")
        ),
        call = call
      )
    }

    ### tests for x as long as it is not NULL, NA, or NaN
    if (!is.null(x)){

      ### test that x is numeric
      if (!is.numeric(x)){
        cli::cli_abort(
          message = dis_msg_class(x = x, class = class, type = type, stem = stem),
          call = call
        )
      }

      ### test that x is an integer
      if (isTRUE(integer) & !is.integer(x)){
        cli::cli_abort(
          message = dis_msg_class(x = x, class = class, type = type, stem = stem),
          call = call
        )
      }

      ### test that x is in valid
      if (!is.null(valid) & !x %in% valid){
        cli::cli_abort(message = dis_msg_valid(), call = call)
      }

    }

    ## create output
    out <- TRUE

  ## do not check x if path == FALSE, return FALSE
  } else {
    out <- FALSE
  }

  ## return output
  return(out)

}
