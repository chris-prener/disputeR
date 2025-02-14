#' Determine Whether an Indefinite Article is Needed for a Particular Class
#'
#' @description This function returns the correct indefinite article based on the
#'    class of the input.
#'
#' @usage dis_indefinite(x, indefinite_class = NULL)
#'
#' @param x Required object; a parameter argument to test.
#' @param indefinite_class Optional character vector; if the class of an object
#'     is listed here, and \code{class(x)} matches this class, the function
#'     will return \code{"an"}.
#'
#' @return If the input is an integer or the class of the input is included in
#'     \code{indefinite_class}, the function will return \code{"an"}. Otherwise,
#'     the function will return \code{"a"}.
#'
#' @examples
#' dis_indefinite(x = "test")
#' dis_indefinite(x = 2)
#'
#' @export
dis_indefinite <- function(x, indefinite_class = NULL){

  ## check inputs if FACT_CHECK is not FALSE
  if (!isFALSE(Sys.getenv(x = "FACT_CHECK"))){

    ### check x
    if (rlang::is_missing(x)){
      cli::cli_abort(
        message = dis_msg_miss(),
        arg = "x", call = call
      )
    }

    ### check param
    if (!is.null(indefinite_class)){
      if (!is.character(indefinite_class)){
        cli::cli_abort(
          message = c(
            "{.arg {indefinite_class}} must be a {.cls character} vector, not a {.cls {class(x)}} vector.",
            "i" = "Provide a {.cls character} vector for {.arg {param}}, such as {.code {indefinite_class} = 'value'} or {.code {indefinite_class} = c('value1', 'value2')}."
          ),
          arg = indefinite_class, call = call
        )
      }
    }

  }

  ## determine if indefinite article is appropriate
  if (is.integer(x) | class(x) %in% indefinite_class){
    out <- "an"
  } else {
    out <- "a"
  }

  ## return output
  return(out)

}
