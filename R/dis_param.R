#' Validate Param Argument
#'
#' @description This function returns a standard error message for invalid \code{param}
#'     values. The \code{param} argument is used throughout \code{disputeR} to
#'     pass the name of the parameter being checked.
#'
#' @usage dis_param(x, param = "param", call = rlang::caller_env())
#'
#' @param x Required object; a parameter argument to test.
#' @param param Required character scalar; the parameter name. The default value
#'     is \code{"param"}, though this can be modified if you are using a different
#'     name for your \code{param} argument.
#' @param call Required environment; the environment in which the function was called.
#'     If nesting \code{dis_param} within another function, it is recommended to provide
#'     the calling environment to ensure the correct environment is referenced using
#'     \code{rlang::caller_env()} (default). If there are multiple levels of nesting,
#'     the \code{call} argument should be used to pass an environment created in the
#'     outermost function.
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
#' example <- function(param){
#'   dis_param(param)
#' }
#'
#' # example function
#' example(param = "test")
#'
#' @export
dis_param <- function(x, param = "param", call = rlang::caller_env()){

  ## check inputs if DISPUTER_DEV_CHECK == TRUE
  if (isTRUE(Sys.getenv(x = "DISPUTER_DEV_CHECK"))){

    ### check call
    if (!is.environment(call)){
      cli::cli_abort(
        message = dis_msg_env(x = call)
      )
    }

    ### check param
    if (!is.character(param) | length(param) != 1){
      cli::cli_abort(
        message = dis_msg_class(
          x = param,
          class = "character",
          type = "scalar",
          stem = "{.code {param} = 'value'}"
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

  ## return error if not character scalar
  if (!is.character(x) | length(x) != 1){
    cli::cli_abort(
      message = dis_msg_class(
        x = x,
        class = "character",
        type = "scalar",
        stem = "{.code {param} = 'value'}"
      ),
      call = call
    )
  }

  ## return output if not missing
  return(TRUE)

}
