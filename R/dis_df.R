#' Validate Data Frame Arguments
#'
#' @description This function runs standard unit tests on data frame parameters for
#'     functions.
#'
#' @usage dis_df(x, valid_class = c("data.frame", "tibble", "data.table"), null_valid = TRUE,
#'     param = NULL, call = NULL, fact_check = "global")
#'
#' @param x Required object; a parameter argument to test.
#' @param valid_class Required character vector; list of possible variations of
#'     data frames. If a given type is passed to \code{valid_class}, it will not
#'     trigger a validation error. As of this time, \code{dis_df()} supports
#'     \code{tibble} and \code{data.table}  objects in addition to
#'     \code{base R} data frames. If an object class is omitted from this argument,
#'     objects of that class will result in a validation error.
#' @param null_valid Required logical scalar; whether the parameter can be \code{NULL}.
#'     If \code{FALSE}, the function will throw an error if \code{x} is \code{NULL}.
#'     Default is \code{TRUE}.
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
#' # create example function that uses dis_df()
#' example <- function(x){
#'
#'   ## check inputs with disputeR
#'   dis_not_missing(.f = rlang::is_missing(x))
#'   dis_df(x, valid_class = "data.frame")
#'
#'   ## return output
#'   return(x)
#'
#' }
#'
#' # test example function
#' example(x = mtcars)
#'
#' @export
dis_df <- function(x, valid_class = c("data.frame", "tibble", "data.table"),
                   null_valid = TRUE, param = NULL, call = NULL,
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

      ### check valid_class
      dis_character(valid_class, valid = c("data.frame", "tibble", "data.table"),
                    scalar = FALSE, null_valid = FALSE)

      ### check null_valid
      dis_logical(null_valid)

    }

    ## unit tests
    ### test whether x is NULL
    dis_null(x = x, class = "data.frame", null_valid = null_valid, param = param, call = call)

    ### tests for x as long as it is not NULL
    if (!is.null(x)){

      ### test that x is any type of data.frame
      if (!is.data.frame(x)){
        cli::cli_abort(
          message = c(
            "{.arg {param}} must be a {.cls data.frame}.",
            "i" = "Valid classes for {.arg {param}} are: {.code {valid_class}}."
          ),
          call = call
        )
      }

      ### create conversion bullets
      if ("data.frame" %in% valid_class){
        df_msg <- c("*" = "Use {.code as.data.frame({param})} to convert {.arg {param}} to a {.cls data.frame}.")
      } else {
        df_msg <- NULL
      }

      if ("tibble" %in% valid_class){
        tbl_msg <- c("*" = "Use {.code tibble::tibble({param})} to convert {.arg {param}} to a {.cls tbl_df} (tibble).")
      } else {
        tbl_msg <-  NULL
      }

      if ("data.table" %in% valid_class){
        datatable_msg <- c("*" = "Use {.code data.table::data.table({param})} to convert {.arg {param}} to a {.cls data.table}.")
      } else {
        datatable_msg <- NULL
      }

      bullets <- c(df_msg, tbl_msg, datatable_msg)

      ### test that x is a base R data.frame
      if ((!"data.frame" %in% valid_class) & length(class(x)) > 1){
        cli::cli_abort(
          message = c(
            "{.arg {param}} must not be a base {.code R} {.cls data.frame}.",
            "i" = "Valid classes for {.arg {param}} are: {.code {valid_class}}.",
            bullets
          ),
          call = call
        )
      }

      ### test that x is a tibble
      if ((!"tibble" %in% valid_class) & inherits(x, what = "tbl_df")){
        cli::cli_abort(
          message = c(
            "{.arg {param}} must not be a {.cls tbl_df} (tibble).",
            "i" = "Valid classes for {.arg {param}} are: {.code {valid_class}}.",
            bullets
          ),
          call = call
        )
      }

      ### test that x is a data.table
      if ((!"data.table" %in% valid_class) & inherits(x, what = "data.table")){
        cli::cli_abort(
          message = c(
            "{.arg {param}} must not be a {.cls data.table}.",
            "i" = "Valid classes for {.arg {param}} are: {.code {valid_class}}.",
            bullets
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
