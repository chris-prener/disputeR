#' Validate Character Arguments
#'
#' @description This function runs standard unit tests on character parameters for
#'     functions.
#'
#' @usage dis_character(x, valid = NULL, null_valid = TRUE, empty_valid = FALSE,
#'     scalar = TRUE, param = NULL, call = NULL, fact_check = "global")
#'
#' @param x Required object; a parameter argument to test.
#' @param valid Optional character scalar or vector; valid values for the parameter.
#' @param null_valid Required logical scalar; whether the parameter can be \code{NULL}.
#'     If \code{FALSE}, the function will throw an error if \code{x} is \code{NULL}.
#'     Default is \code{TRUE}.
#' @param empty_valid Required logical scalar; whether the parameter can be an empty
#'     string. If \code{FALSE} (default), the function will throw an error if \code{x}
#'     is an empty string. If \code{TRUE}, the function will allow an empty string.
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
#' @return This function will return either \code{TRUE} (if \code{x} passes
#'     all validation checks) or \code{FALSE} (if the validation checks are
#'     skipped). If \code{x} fails validation checks, an error message will
#'     be returned. Note that, if the input is \code{NULL} and \code{null_valid}
#'     is set to \code{TRUE}, the detailed unit tests are skipped and the
#'     function will return \code{TRUE}.
#'
#' @details See the vignette on \code{vignette("developing", package = "disputeR")}
#'     for details about internal validation of arguments for this function.
#'
#' @examples
#' # create example function that uses dis_character()
#' example <- function(x){
#'
#'   ## check inputs with disputeR
#'   dis_not_missing(.f = rlang::is_missing(x))
#'   dis_character(x, null_valid = FALSE)
#'
#'   ## modify string
#'   out <- paste0(x, " rules!")
#'
#'   ## return output
#'   return(out)
#'
#' }
#'
#' # test example function
#' example(x = "R")
#'
#' @export
dis_character <- function(x, valid = NULL, null_valid = TRUE, empty_valid = FALSE,
                          scalar = TRUE, param = NULL, call = NULL,
                          fact_check = "global"){

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

    ## check inputs if DISPUTER_DEV_CHECK == TRUE
    if (Sys.getenv(x = "DISPUTER_DEV_CHECK") == "TRUE"){

      ### check call
      dis_environment(call)

      ### check param
      dis_param(param)

      ### check x
      dis_not_missing(.f = rlang::is_missing(x))

      ### check valid
      if (!is.null(valid) & !is.character(valid)){
        cli::cli_abort(
          message = c(
            paste0("{.code valid} must be a character, not ", dis_indefinite(valid), " {.cls {class(valid)}}."),
            "i" = "Provide a character vector for {.code valid}, such as {.code valid = 'value'} or {.code valid = c('value1', 'value2')}."
          ),
          call = call
        )
      }

      ### check remaining logical parameters
      dis_logical(null_valid)
      dis_logical(empty_valid)
      dis_logical(scalar)

    }

    ## unit tests
    ### test whether x is NULL
    dis_null(x = x, class = "character", null_valid = null_valid)

    ### tests for x as long as it is not NULL
    if (!is.null(x)){

      ### set error message info text
      if (isTRUE(scalar)){
        type <- "scalar"
        stem <- "{.code {param} = 'value'}."
      } else if (isFALSE(scalar)){
        type <- "vector"
        stem <- "{.code {param} = 'value'} or {.code {param} = c('value1', 'value2')}."
      }

      ### test that x is character
      if (!is.character(x)){
        cli::cli_abort(
          message = dis_msg_class(x = x, class = "character", type = type, stem = stem, param = "x", call = call),
          call = call
        )
      }

      ### test that x is a scalar
      if (isTRUE(scalar) & length(x) != 1){
        cli::cli_abort(
          message = dis_msg_scalar(class = "character", stem = stem), call = call
        )
      }

      ### test that x is not empty
      if (!empty_valid & x == ""){
        cli::cli_abort(
          message = c(
            "{.arg {param}} must not be an empty {.cls character} value (i.e. {.code {param} = ''}).",
            "i" = paste0("Provide a {.cls character} ", type, " for {.arg {param}}, such as ", stem)
          ),
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
