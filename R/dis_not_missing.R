#' Validate Whether Parameter is Missing
#'
#' @description This function returns a standard error message for missing parameters.
#'
#' @usage dis_not_missing(.f, param = "x", call = rlang::caller_env(),
#'     fact_check = "global")
#'
#' @param .f Required logical scalar; the output of \code{missing(x))} or
#'     \code{rlang::is_missing(x)}, substituting the parameter name passed to
#'     either function as needed.
#' @param param Required character scalar; the parameter name. The default uses
#'     the variable name \code{x}, but this value can be set to a different
#'     static character scalar. Alternatively, you can use \code{rlang::caller_arg()}
#'     to pull the parameter name passed by the function \code{dis_not_missing()}
#'     is nested within. Regardless of the approach taken here, the resulting
#'     character scalar should match the object name passed to \code{.f}.
#' @param call Required environment; the environment in which the function was called.
#'     If nesting \code{dis_not_missing} within another function, it is recommended to
#'     provide the calling environment to ensure the correct environment is referenced
#'     using \code{rlang::caller_env()} (default). If there are multiple levels of
#'     nesting, the \code{call} argument can be used to pass an environment created
#'     in the outermost function.
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
#'     for details about internal validation of arguments for this function.
#'
#' @examples
#' # create example function that uses dis_param()
#' example <- function(x){
#'   dis_not_missing(.f = rlang::is_missing(x))
#' }
#'
#' # example function
#' example(x = "test")
#'
#' # create example function that uses dis_param() with different variable name
#' example <- function(var){
#'   dis_not_missing(.f = rlang::is_missing(var), param = "var")
#' }
#'
#' # example function
#' example(var = "test")
#'
#' @export
dis_not_missing <- function(.f, param = "x", call = rlang::caller_env(),
                            fact_check = "global"){

  ### check call
  dis_environment(call)

  ## check fact_check and set path
  path <- dis_checker(fact_check = fact_check, call = call)

  ## check x if path == TRUE, return TRUE if all checks pass
  if (isTRUE(path)){

    ## check inputs
    if (isTRUE(Sys.getenv(x = "DISPUTER_DEV_CHECK"))){

      ### check param
      dis_param(param)

      ### check .f
      if (rlang::is_missing(.f)){
        cli::cli_abort(
          message = c(
            "{.code {.f}} must be provided but is missing.",
            "i" = "Add an argument for {.code {.f}} to the function call."
          ),
          call = call
        )
      }

    }

    ## return error if missing
    if (isTRUE(.f)){
      cli::cli_abort(
        message = c(
          "{.code {param}} must be provided but is missing.",
          "i" = "Add an argument for {.code {param}} to the function call."
        ),
        call = call
      )
    }

    ## return output if not missing
    out <- TRUE

  ## do not check x if path == FALSE, return FALSE
  } else {
    out <- FALSE
  }

  ## return output
  return(out)

}
