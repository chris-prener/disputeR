#' Validate Logical Arguments
#'
#' @description This function runs standard unit tests on logical parameters for
#'     functions.
#'
#' @usage dis_logical(x, null_valid = TRUE, scalar = TRUE, param = NULL,
#'     call = NULL, fact_check = "global")
#'
#' @param x Required object; a parameter argument to test.
#' @param null_valid Required logical scalar; whether the parameter can be \code{NULL}.
#'     If \code{FALSE}, the function will throw an error if \code{x} is \code{NULL}.
#'     Default is \code{TRUE}.
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
#'     all validation checks) or an error message. Note that, if the input is \code{NULL}
#'     and \code{null_valid} is set to \code{TRUE}, the detailed unit tests are
#'     skipped and the function will return \code{TRUE}.
#'
#' @details See the vignette on \code{vignette("developing", package = "disputeR")}
#'     for details about internal validation of arguments for this function.
#'
#' @examples
#' # create example function that uses dis_logical()
#' example <- function(x){
#'
#'   ## check inputs with disputeR
#'   dis_not_missing(.f = rlang::is_missing(x))
#'   dis_logical(x, null_valid = FALSE)
#'
#'   ## flip logical input
#'   out <- !x
#'
#'   ## return output
#'   return(out)
#'
#' }
#'
#' # test example function
#' example(x = TRUE)
#'
#' @export
dis_logical <- function(x, null_valid = TRUE, scalar = TRUE, param = NULL,
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
    if (Sys.getenv(x = "DISPUTER_DEV_CHECK") == "TRUE"){

      ### check call
      dis_environment(call)

      ### check param
      dis_param(param)

      ### check x
      dis_not_missing(.f = rlang::is_missing(x))

      ### check null_valid
      stem <- "{.code {null_valid} = TRUE} or {.code {null_valid} = FALSE}"

      if (!is.logical(null_valid)){
        cli::cli_abort(
          message = dis_msg_class(
            x = null_valid,
            class = "logical",
            type = "scalar",
            stem = stem,
            param = "null_valid"
          ),
          call = call
        )
      }

      if (length(null_valid) != 1){
        cli::cli_abort(
          message = dis_msg_scalar(
            class = "logical",
            stem = stem,
            param = "null_valid"
          ),
          call = call
        )
      }

      ### check scalar
      stem <- "{.code {scalar} = TRUE} or {.code {scalar} = FALSE}"

      if (!is.logical(scalar)){
        cli::cli_abort(
          message = dis_msg_class(
            x = scalar,
            class = "logical",
            type = "scalar",
            stem = stem,
            param = "scalar"
          ),
          call = call
        )
      }

      if (length(scalar) != 1){
        cli::cli_abort(
          message = dis_msg_scalar(
            class = "logical",
            stem = stem,
            param = "scalar"
          ),
          call = call
        )
      }

    }

    ### test whether x is NULL
    dis_null(x = x, class = "logical", null_valid = null_valid, param = param, call = call)

    ### tests for x as long as it is not NULL
    if (!is.null(x)){

      ### set error message info text
      if (isTRUE(scalar)){
        type <- "scalar"
        stem <- "{.code {param} = TRUE} or {.arg {param} = FALSE}"
      } else if (isFALSE(scalar)){
        type <- "vector"
        stem <- "{.code {param} = TRUE} or {.code {param} = c(TRUE, FALSE)}"
      }

      if (!is.logical(x)){
        cli::cli_abort(
          message = dis_msg_class(
            x,
            class = "logical",
            type = type,
            stem = stem
          ),
          call = call
        )
      }

      if (length(x) != 1){
        cli::cli_abort(
          message = dis_msg_scalar(
            class = "logical",
            stem = stem,
            param = "x"
          ),
          call = call
        )
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
