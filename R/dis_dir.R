#' Validate Directory Arguments
#'
#' @description This function runs standard unit tests on a character argument
#'     that should resolve to a specific directory.
#'
#' @usage dis_dir(x, null_valid = TRUE, error = TRUE, param = NULL,
#'     call = NULL, fact_check = "global")
#'
#' @param x Required object; a parameter argument to test.
#' @param null_valid Required logical scalar; whether the parameter can be \code{NULL}.
#'     If \code{FALSE}, the function will throw an error if \code{x} is \code{NULL}.
#'     Default is \code{TRUE}.
#' @param error Required logical scalar; whether the function should error if the
#'     path is not found. If \code{TRUE} (default), the function will throw an
#'     error if \code{x} is not found. If \code{FALSE}, the function will create
#'     a directory at the given path instead of erroring.
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
#'     all validation checks). If it does not pass all validation tests, it will
#'     return an error message if \code{error = TRUE} or a message about the
#'     creation of a directory if \code{error = FALSE}. Note that, if the input
#'     is \code{NULL} and \code{null_valid} is set to \code{TRUE}, the detailed
#'     unit tests are skipped and the function will return \code{TRUE}.
#'
#' @details See the vignette on \code{vignette("developing", package = "disputeR")}
#'     for details about internal validation of arguments for this function.
#'
#' @export
dis_dir <- function(x, null_valid = TRUE, error = TRUE, param = NULL,
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

      ### check remaining logical parameters
      dis_logical(error)

    }

    ## check x
    ### test whether x is NULL
    dis_null(x = x, class = "character", null_valid = null_valid, param = param, call = call)

    ### tests for x as long as it is not NULL
    if (!is.null(x)){

      ### verify x is a character
      dis_character(x)

      ### verify directory exists and error if it does not
      if (isTRUE(error) & !dir.exists(x)){
        cli::cli_abort(
          message = c(
            "{.code {param}} must be a valid path to a directory but is not.",
            "i" = "Update {.code {param}} to a valid path, or create a directory using {.code dir.create(path = '{x}')}."
          ),
          call = call
        )

      ### verify directory exists and create it if it does not
      } else if (isFALSE(error) & !dir.exists(x)){

        ### attempt to create directory
        result <- tryCatch(
          dir.create(path = x),
          error = function(e) e,
          warning = function(w) w
        )

        ### return error if an error or warning is generated when attempting to create directory
        if (inherits(result, "warning") | inherits(result, "error")){
          cli::cli_abort(
            message = c(
              "{.code {param}} must be a valid path to a directory but is not.",
              "x" = "Attempted to create a directory at {.file {x}} but could not create it successfully.",
              "i" = "This can happen when you attempt to create a subdirectory within a parent directory, and that parent diretory does not exist."
            ),
            call = call
          )
        }

        ### return update if folder created successfully
        cli::cli_alert_warning(
          text = "{.code {param}} must be a valid path to a directory but is not."
        )

        cli::cli_alert_success(
          text = "Directory created at {.file {x}}."
        )

      } else {
        return(TRUE)
      }

    }

  ## do not check x if path == FALSE, return FALSE
  } else {
    return(FALSE)
  }

}
