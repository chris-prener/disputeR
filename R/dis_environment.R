#' Validate Call Argument
#'
#' @description This function returns a standard error message for invalid \code{call}
#'     values. The \code{call} argument is used throughout \code{disputeR} to
#'     pass the environment of the function being checked.
#'
#' @usage dis_environment(x, param = "call", call = rlang::caller_env(),
#'     fact_check = "global")
#'
#' @param x Required object; a parameter argument to test.
#' @param param Required character scalar; the parameter name. The default value
#'     is \code{"call"}, though this can be modified if you are using a different
#'     name for your environment argument.
#' @param call Required environment; the environment in which the function was called.
#'     If nesting \code{dis_param} within another function, it is recommended to provide
#'     the calling environment to ensure the correct environment is referenced using
#'     \code{rlang::caller_env()} (default). If there are multiple levels of nesting,
#'     the \code{call} argument should be used to pass an environment created in the
#'     outermost function.
#' @param fact_check Required character scalar; whether to override fact checking
#'     environment setting. If \code{"global"} (default), \code{dis_character} will
#'     follow the global setting. If \code{"always"}, \code{dis_character} will
#'     ignore any global setting and will always check \code{x}. This argument is
#'     primarily intended for Shiny developers who wish to use \code{disputeR} in
#'     modules. See the vignette on \code{vignette("developing", package = "disputeR")}
#'     for details on how to use this function.
#'
#' @return This function will return either \code{TRUE} (if the input passes
#'     all validation checks) or an error message.
#'
#' @details See the vignette on \code{vignette("developing", package = "disputeR")}
#'     for details about internal validation of arguments for this function. Unlike
#'     the core functions, \code{dis_param} does not have a \code{fact_check}
#'     argument. The \code{vignette("developing", package = "disputeR")} vignette
#'     includes details on how to implement that functionality around \code{dis_param}.
#'
#' @examples
#' # create example function that uses dis_param()
#' example <- function(x = rlang::caller_env()){
#'   dis_environment(x)
#' }
#'
#' # example function
#' example()
#'
#' @export
dis_environment <- function(x, param = "call", call = rlang::caller_env(),
                            fact_check = "global"){

  ## check inputs if FACT_CHECK is not FALSE
  if (isTRUE(Sys.getenv(x = "DISPUTER_DEV_CHECK"))){

    ### check call
    if (!is.environment(call)){
      cli::cli_abort(
        message = dis_msg_env(x = call, param = param)
      )
    }

    ### check param
    if (!is.character(param) | length(param) != 1){
      cli::cli_abort(
        message = dis_msg_class(
          x = param,
          class = "character",
          type = "scalar",
          stem = "{.code {param} = 'call'}"
        ),
        call = call
      )
    }

    ### check x
    if (rlang::is_missing(x)){
      cli::cli_abort(
        message = dis_msg_miss(),
        call = call
      )
    }

  }

  ## return error if not environment
  if (!is.environment(x)){
    cli::cli_abort(
      message = dis_msg_env(x = x, param = param),
      call = call
    )
  }

  ## return output if not missing
  return(TRUE)

}
