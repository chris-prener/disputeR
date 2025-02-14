#' Validate Whether Parameter is Null
#'
#' @description This function returns a standard error message for \code{NULL} parameters
#'     that are not allowed to be \code{NULL}, i.e. if \code{null_valid = FALSE}.
#'
#' @usage dis_null(x, class, null_valid, param = rlang::caller_arg(x),
#'     call = rlang::caller_env())
#'
#' @param x Required object; a parameter argument to test.
#' @param class Required character scalar; the expected type of the parameter.
#' @param null_valid Required logical scalar; whether the parameter can be \code{NULL}.
#'     If \code{FALSE}, the function will throw an error if \code{x}
#'     is \code{NULL}. If \code{TRUE}, the function will not throw an error if
#'     \code{x} is \code{NULL}.
#' @param param Required character scalar; the parameter name. The default uses
#'     \code{rlang::caller_arg(x)} to pull the parameter name passed to \code{x}
#'     by the function \code{dis_null()} is nested within. This value can also
#'     be set to a static character scalar.
#' @param call Required environment; the environment in which the function was called.
#'     If nesting \code{dis_not_missing} within another function, it is recommended to
#'     provide the calling environment to ensure the correct environment is referenced
#'     using \code{rlang::caller_env()} (default). If there are multiple levels of
#'     nesting, the \code{call} argument can be used to pass an environment created
#'     in the outermost function.
#'
#' @return This function will return an error message for \code{NULL} parameters
#'     that are not allowed to be \code{NULL}. Otherwise, no output is returned.
#'
#' @export
dis_null <- function(x, class, null_valid, param = rlang::caller_arg(x),
                     call = rlang::caller_env()){

  ## check inputs if FACT_CHECK is not FALSE
  if (!isFALSE(Sys.getenv(x = "FACT_CHECK"))){

    ### check call
    dis_environment(call)

    ### check param
    dis_param(param)

    ### check x
    dis_not_missing(.f = rlang::is_missing(x), param = "x")

    ### check class
    dis_not_missing(.f = rlang::is_missing(class), param = "class")

    if (!is.character(class) | length(class) != 1){
      cli::cli_abort(
        message = dis_msg_class(
          x = class,
          class = "character",
          type = "scalar",
          stem = "{.code {class} = 'character'}"
        ),
        call = call
      )
    }

    ### check null_valid
    dis_not_missing(.f = rlang::is_missing(null_valid), param = "null_valid")

    if (!is.logical(null_valid) | length(null_valid) != 1){
      cli::cli_abort(
        message = dis_msg_class(
          x = null_valid,
          class = "logical",
          type = "scalar",
          stem = "{.code {null_valid} = TRUE} or {.code {null_valid} = FALSE}"
        ),
        call = call
      )
    }
  }

  ## return error if NULL and NULL is not allowed
  if (!null_valid & is.null(x)){

    ## set stem of error message based on type
    if (class == "character"){
      stem <- "{.code {param} = 'value'}"
    } else if (class == "logical"){
      stem <- "{.code {param} = TRUE}"
    } else if (class == "numeric"){
      stem <- "{.code {param} = 1}"
    } else if (class == "integer"){
      stem <- "{.code {param} = 1L}"
    } else if (class == "data frame"){
      stem <- "{.code {param} = df}"
    }

    ## return error
    cli::cli_abort(
      message = c(
        "{.arg {param}} must not be {.code NULL}.",
        "i" = paste0("Provide a non-{.code NULL} value for {.arg {param}}, such as ", stem, ".")
      ),
      call = call
    )
  }

}
