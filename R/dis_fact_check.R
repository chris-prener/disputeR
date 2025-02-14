#' Set the Fact Check Environment Variable
#'
#' @description The \code{DISPUTER_FACT_CHECK} environment variable is used
#'     determine whether \code{disputeR} checks are run globally within a package
#'     or Shiny app.
#'
#' @usage dis_fact_check(fact_check = TRUE)
#'
#' @param fact_check Required logical scalar; should \code{disputeR} fact checking
#'     be conducted? If \code{TRUE} (default), all parameter checks within a project or
#'     Shiny app will be run. When set to \code{FALSE}, all checks will be skipped
#'     unless they are specifically overridden with a function's \code{fact_check}
#'     argument. See the vignette on \code{vignette("developing", package = "disputeR")}
#'     for details on how to use this function.
#'
#' @return This function writes to your session's environment variables, but
#'     does not explicitly return any value.
#'
#' @details Since this function is meant to be used interactively, internal validation
#'     of its arguments cannot be skipped. See the vignette on
#'     \code{vignette("developing", package = "disputeR")} for details.
#'
#' @examples
#' dis_fact_check()
#'
#' @export
dis_fact_check <- function(fact_check = TRUE){

  ## check inputs
  dis_logical(fact_check, null_valid = FALSE)

  ## set global environment
  Sys.setenv(DISPUTER_FACT_CHECK = fact_check)

}

#' Check and Set fact_check Within a Function
#'
#' @description This is designed to be used within functions to check the
#'     status of the \code{DISPUTER_FACT_CHECK} environment variable as well
#'     as the \code{fact_check} argument. Both are evaluated and the output
#'     can be used to determine the execution path for the remainder of the
#'     function.
#'
#' @usage dis_checker(fact_check, call = rlang::caller_env())
#'
#' @param fact_check Required character scalar; one of either \code{"global"} or
#'     \code{"always"}, passed from the parent function's \code{fact_check}
#'     parameter.
#' @param call Required environment; the environment in which the function was called.
#'     If \code{NULL} (default), the function will attempt to determine the calling
#'     environment. If nesting functions, it is recommended to provide the calling
#'     environment to ensure the correct environment is referenced using
#'     \code{rlang::caller_env()}.
#'
#' @return Either \code{TRUE} if fact checking should be completed or \code{FALSE}
#'     if it should be skipped. If the value of \code{fact_check} is not either
#'     \code{"global"} or \code{"always"}, an error will be returned.
#'
#' @details Since this function is meant to be used prior to most other checks,
#'     internal validation of its arguments cannot be skipped. See the vignette on
#'     \code{vignette("developing", package = "disputeR")} for details.
#'
#' @examples
#' dis_checker(fact_check = "global")
#'
#' @export
dis_checker <- function(fact_check, call = rlang::caller_env()){

  ## set valid values for fact_check
  fact_check_valid <- c("global", "always")

  ## check fact_check
  if (!fact_check %in% fact_check_valid | length(fact_check) > 1){
    cli::cli_abort(
      message = c(
        "{.arg fact_check} must be a valid value for the parameter, not {.code {fact_check}}.",
        "i" = "Valid arguments for {.arg fact_check} are: {.code {fact_check_valid}}."
      ),
      call = call
    )
  }

  ## create output
  if (Sys.getenv("DISPUTER_FACT_CHECK") != "FALSE" | fact_check == "always"){
    out <- TRUE
  } else if (Sys.getenv("DISPUTER_FACT_CHECK") == "FALSE" & fact_check == "global"){
    out <- FALSE
  }

  ## return output
  return(out)

}

#' Set the Developer Check Environment Variable
#'
#' @description The \code{DISPUTER_DEV_CHECK} environment variable is used
#'     determine whether parameter checks on \code{disputeR} functions are run
#'     during a session.
#'
#' @usage dis_dev_check(dev_check = FALSE)
#'
#' @param dev_check Required logical scalar; should parameter checks on
#'     \code{disputeR} functions be conducted? If \code{FALSE} (default), all
#'     checks for \code{disputeR} function parameters will be skipped. This is
#'     the ideal situation for end users who are using a function with properly
#'     configured \code{disputeR} functions within it. For developers who are
#'     checking the configuration of \code{disputeR} functions, setting
#'     \code{dev_check} to \code{TRUE} can aid with troubleshooting. See the
#'     vignette on \code{vignette("developing", package = "disputeR")}
#'     for details on how to use this function.
#'
#' @return This function writes to your session's environment variables, but
#'     does not explicitly return any value.
#'
#' @details Since this function is meant to be used interactively, internal validation
#'     of its arguments cannot be skipped. See the vignette on
#'     \code{vignette("developing", package = "disputeR")} for details.
#'
#' @export
dis_dev_check <- function(dev_check = FALSE){

  ## check inputs
  dis_logical(dev_check, null_valid = FALSE)

  ## set global environment
  Sys.setenv(DISPUTER_DEV_CHECK = dev_check)

}
