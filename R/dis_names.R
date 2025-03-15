#' Validate Names Within Objects
#'
#' @description For any object that can have names, this function can be used to
#'     enforce requirements either for names that must be present or names that
#'     cannot be present.
#'
#' @usage dis_names(x, names, present = FALSE, warn = FALSE, null_valid = TRUE,
#'     param = NULL, call = NULL, fact_check = "global")
#'
#' @param x Required object; a parameter argument to test.
#' @param names Require character vector; the names to test \code{x} for.
#' @param present Required logical scalar; if \code{FALSE} (default), an error
#'     or warning will be generated if the names in \code{names} in the names
#'     argument are not present. If \code{TRUE}, an error or warning will be
#'     generated if the names in the \code{names} argument are present.
#'
#'     For example, if you wish to enforce a rule that a column named \code{outcome}
#'     is reserved for your function and any object passed to the function therefore
#'     cannot contain \code{outcome}, you would set \code{present} to \code{TRUE}.
#'
#'     Alternatively, if you wish to enforce a rule that a column named \code{outcome}
#'     must be present in any object passed to the function, you would set
#'     \code{present} to \code{FALSE} (default).
#'
#' @param warn Required logical scalar; if \code{FALSE} (default), the function
#'     will throw an error based on the condition specified with the \code{present}
#'     argument. If \code{TRUE}, the function will return a warning instead.
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
#' @details See the vignette on \code{vignette("developing", package = "disputeR")}
#'     for details about internal validation of arguments for this function.
#'
#' @examples
#' # create sample data frame
#' x <- data.frame(
#'   a = c(1:5)
#' )
#'
#' # define function that requires a column named a and does not allow a column
#' # named b in the input object
#' square_a <- function(x){
#'
#'   ## check inputs with disputeR
#'   dis_not_missing(.f = rlang::is_missing(x))
#'   dis_df(x, valid_class = "data.frame")
#'   dis_names(x, names = "a")
#'   dis_names(x, names = "b", present = TRUE)
#'
#'   ## square a
#'   x$b <- x$a^2
#'
#'   ## return output
#'   return(x)
#'
#' }
#'
#' # test function
#' square_a(x)
#'
#' @export
dis_names <- function(x, names, present = FALSE, warn = FALSE, null_valid = TRUE,
                      param = NULL, call = NULL, fact_check = "global"){

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

      ### check names
      dis_not_missing(.f = rlang::is_missing(names))
      dis_character(names, scalar = FALSE, null_valid = FALSE)

      ### check logical arguments
      dis_logical(present)
      dis_logical(warn)

    }

    ## unit tests
    ### test whether x is NULL
    dis_null(x = x, class = "data.frame", null_valid = null_valid)

    ### tests for x as long as it is not NULL
    if (!is.null(x)){

      ### if present is FALSE
      if (isFALSE(present)){
        if (all(names %in% names(x))){
          fail <- FALSE
        } else {
          fail <- TRUE
          present_text <- "have"
        }

      ### if present is TRUE
      } else if (isTRUE(present)){
        if (any(names %in% names(x))){
          fail <- TRUE
          present_text <- "not have"
        } else {
          fail <- FALSE
        }
      }

      ### create output if logic test is failed
      if (isTRUE(fail)){

        ### create object name
        if (inherits(x, what = "data.frame")){
          if (length(names) == 1){
            type <- " a column"
          } else {
            type <- " columns"
          }
        } else if (is.list(x)){
          if (length(names) == 1){
            type <- " an element"
          } else {
            type <- " elements"
          }
        }

        ### create stem
        if (isTRUE(present)){

          present_names <- names[names %in% names(x)]
          present_index <- match(present_names[1], names(x))

          if (inherits(x, what = "data.frame")){
            if (length(present_names) == 1){
              type2 <- " a column"

              if (length(names) == 1){
                stem <- "."
              } else {
                stem <- paste0(", but has ", type2, " named {.code {present_names}}.")
              }

              info_lead <- c("i" = "You can this error by modifying {.arg {param}} in one of the following ways:")
            } else {
              type2 <- " columns"
              stem <- paste0(", but has ", type2, " named {.code {present_names}}.")
              info_lead <- c("i" = "You can this error by modifying each problematic column in {.arg {param}} in one of the following ways:")
            }

            info_text <- c(
              info_lead,
              "*" = "Use {.code names({param})[{present_index}] <- 'new_name'} or {.code dplyr::rename({param}, new_name = {present_names[1]})} to rename column {.code {present_names[1]}}.",
              "*" = "Use {.code {param}[-{present_index}]} or {.code dplyr::select({param}, -{present_names[1]})} to remove column {.code {present_names[1]}}."
            )

          } else if (is.list(x)){
            if (length(present_names) == 1){
              type2 <- " an element"

              if (length(names) == 1){
                stem <- "."
              } else {
                stem <- paste0(", but has ", type2, " named {.code {present_names}}.")
              }

              info_lead <- c("i" = "You can address this error by modifying {.arg {param}} in one of the following ways:")
            } else {
              type2 <- " elements"
              stem <- paste0(", but has ", type2, " named {.code {present_names}}.")
              info_lead <- c("i" = "You can address this error by modifying each problematic element in {.arg {param}} in one of the following ways:")
            }

            info_text <- c(
              info_lead,
              "*" = "Use {.code names({param})[{present_index}] <- 'new_name'} to rename element {.code {present_names[1]}}.",
              "*" = "Use {.code {param}[-{present_index}]} to remove element {.code {present_names[1]}}."
            )
          }

        } else {
          stem <- "."
          info_text <- c("i" = "Check your input for {param}.")
        }

        ### if warn is FALSE
        if (isFALSE(warn)){
          cli::cli_abort(
            message = c(
              paste0("{.arg {param}} must ", present_text, type, " named", " {.code {names}}", stem),
              info_text
            ),
            call = call
          )

        ### if warn is TRUE
        } else if (isTRUE(warn)){
          cli::cli_warn(
            message = c(
              paste0("{.arg {param}} must ", present_text, type, " named", " {.code {names}}", stem),
              "i" = info_text
            ),
            call = call
          )
        }

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
