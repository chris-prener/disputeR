#' Validate Data Frame Arguments
#'
#' @description This function runs standard unit tests on data frame parameters for
#'     functions.
#'
#' @usage dis_df(x, tibble = FALSE, param = NULL, call = NULL)
#'
#' @param x Required object; a parameter argument to test.
#' @param tibble Required logical scalar; whether the parameter must be a tibble.
#'     If \code{TRUE}, the function will throw an error if \code{x} is not a tibble.
#'     Default is \code{FALSE}.
#' @param param Optional character scalar; the parameter name. If \code{NULL} (default),
#'     the function will attempt to determine the parameter name from the calling
#'     environment. If nesting functions, it is recommended to provide the parameter
#'     name to ensure the correct parameter is referenced using \code{rlang::caller_arg()}.
#' @param call Optional environment; the environment in which the function was called.
#'     If \code{NULL} (default), the function will attempt to determine the calling
#'     environment. If nesting functions, it is recommended to provide the calling
#'     environment to ensure the correct environment is referenced using
#'     \code{rlang::caller_env()}.
#'
#' @return This function will return either \code{TRUE} (if the input passes
#'     all validation checks) or an error message.
#'
#' @export
dis_df <- function(x, tibble = FALSE, param = NULL, call = NULL){

  ## store environment and parameter name if not provided
  if (is.null(param)){
    param <- rlang::caller_arg(x)
  } else {
    dis_param(param)
  }

  if (is.null(call)){
    call <- rlang::caller_env()
  } else {
    dis_environment(call)
  }

  ## check inputs
  dis_not_missing(.f = missing(x), call = call)

  if (missing(param)){
    cli::cli_abort(message = c("x" = "A value must be passed to the 'param' argument."))
  }

  gmod_validate_param(param = param)
  dis_logical(tibble)

  ## create root of error
  root <- paste0("The '", param, "' argument")

  ## unit tests
  ### test that x is a data frame
  if (!is.data.frame(x)){
    cli::cli_abort(message = c("x" = paste0(root, " must be a data frame. Use 'class(", param, ")' to identify the type of input you have.")))
  }

  ### test that x is a tibble
  if (isTRUE(tibble) & !inherits(x, what = "tbl_df")){
    cli::cli_abort(message = c("x" = paste0(root, " must be a tibble.")))
  }

  ## return output if valid
  return(TRUE)

}
