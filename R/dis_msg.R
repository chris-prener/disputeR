#' Default Message for Missing Parameters
#'
#' @description Create the default message used when a parameter is found to
#'     be missing.
#'
#' @usage dis_msg_miss()
#'
#' @return A character string with the default message.
#'
#' @examples
#' dis_msg_miss()
#'
#' @export
dis_msg_miss <- function(){

  ## create output
  out <- c(
    "{.code x} must be provided but is missing.",
    "i" = "Add an argument for {.code x} to the function call."
  )

  ## return output
  return(out)

}

#' Default Message for Parameters of Incorrect Class
#'
#' @description Create the default message used when a parameter is found to
#'     be the wrong class.
#'
#' @usage dis_msg_class(x, class, type, stem, param = "param", call = rlang::caller_env())
#'
#' @param x Required object; a parameter argument to test.
#' @param class Required character scalar; the expected class of an object.
#' @param type Required character scalar; one of either \code{"scalar"},
#'     \code{"vector"}, or \code{"object"}.
#' @param stem Required character scalar; the end of the error message containing
#'     an example of how to correctly format the input. For example, if you require
#'     a numeric scalar, you should include \code{stem = "{.code {param} = 1}."}.
#'     See the documentation for the \code{cli} package to find other formatting
#'     examples.
#' @param param Required character scalar; the parameter name. The default
#'     matches the argument named for environments used through \code{disputeR}.
#' @param call Required environment; the environment in which the function was called.
#'     If nesting \code{dis_msg_class()} within another function, it is recommended to
#'     provide the calling environment to ensure the correct environment is referenced
#'     using \code{rlang::caller_env()} (default). If there are multiple levels of
#'     nesting, the \code{call} argument can be used to pass an environment created
#'     in the outermost function.
#'
#' @return A character string with the default message.
#'
#' @details See the vignette on \code{vignette("performance", package = "disputeR")}
#'     for details about how to skip internal validation of arguments for this
#'     function.
#'
#' @examples
#' dis_msg_class(x = "test", class = "integer", type = "scalar",
#'               stem = "{.code {param} = 1}")
#'
#' @export
dis_msg_class <- function(x, class, type, stem, param = "param", call = rlang::caller_env()){

  ## check inputs if FACT_CHECK is not FALSE
  if (!isFALSE(Sys.getenv(x = "FACT_CHECK"))){

    ### check call
    if (!is.environment(call)){
      cli::cli_abort(
        message = dis_msg_env(x = call)
      )
    }

    ### check param
    dis_not_missing(.f = rlang::is_missing(param), param = "param")

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
    dis_not_missing(.f = rlang::is_missing(x))

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

    ### check valid inputs for type
    dis_not_missing(.f = rlang::is_missing(type), param = "type")

    valid <- c("scalar", "vector", "object")

    if (!type %in% valid){
      cli::cli_abort(
        message = c(
          "{.arg {type}} must be a valid value for the parameter {.arg {type}}, not {.code {type}}.",
          "i" = "Valid arguments for {.arg {type}} are: {.code {valid}}."
        ),
        call = call
      )
    }

    ### check stem
    dis_not_missing(.f = rlang::is_missing(stem), param = "stem")

    if (!is.character(stem) | length(stem) != 1){
      cli::cli_abort(
        message = dis_msg_class(
          x = stem,
          class = "character",
          type = "scalar",
          stem = "{.code {param} = 1}"
        ),
        call = call
      )
    }

  }

  ## create output
  out <- c(
    paste0("{.arg {", param, "}} must be a {.cls ", class, "} ", type, ", not ", dis_indefinite(x), " {.cls {class(x)}} ", type, "."),
    "i" = paste0("Provide a {.cls ", class, "} ", type, " for {.arg {", param, "}}, such as ", stem, ".")
  )

  ## return output
  return(out)

}

#' Default Message for Parameters of Incorrect Length
#'
#' @description Create the default message used when a parameter is found to
#'     be a length greater than 1 (i.e. not a scalar).
#'
#' @usage dis_msg_scalar(class, stem, param = "param", call = rlang::caller_env())
#'
#' @param class Required character scalar; the expected class of an object.
#' @param stem Required character scalar; the end of the error message containing
#'     an example of how to correctly format the input. For example, if you require
#'     a numeric scalar, you should include \code{stem = "{.code {param} = 1}."}.
#'     See the documentation for the \code{cli} package to find other formatting
#'     examples.
#' @param param Required character scalar; the parameter name. The default
#'     matches the argument named for environments used through \code{disputeR}.
#' @param call Required environment; the environment in which the function was called.
#'     If nesting \code{dis_msg_class()} within another function, it is recommended to
#'     provide the calling environment to ensure the correct environment is referenced
#'     using \code{rlang::caller_env()} (default). If there are multiple levels of
#'     nesting, the \code{call} argument can be used to pass an environment created
#'     in the outermost function.
#'
#' @return A character string with the default message.
#'
#' @details See the vignette on \code{vignette("performance", package = "disputeR")}
#'     for details about how to skip internal validation of arguments for this
#'     function.
#'
#' @examples
#' dis_msg_scalar(class = "integer", stem = "{.code {param} = 1}")
#'
#' @export
dis_msg_scalar <- function(class, stem, param = "param", call = rlang::caller_env()){

  ## check inputs if FACT_CHECK is not FALSE
  if (!isFALSE(Sys.getenv(x = "FACT_CHECK"))){

    ### check call
    if (!is.environment(call)){
      cli::cli_abort(
        message = dis_msg_env(x = call)
      )
    }

    ### check param
    dis_not_missing(.f = rlang::is_missing(param), param = "param")

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

    ### check stem
    dis_not_missing(.f = rlang::is_missing(stem), param = "stem")

    if (!is.character(stem) | length(stem) != 1){
      cli::cli_abort(
        message = dis_msg_class(
          x = stem,
          class = "character",
          type = "scalar",
          stem = "{.code {param} = 1}"
        ),
        call = call
      )
    }

  }

  ## create output
  out <- c(
    "{.arg {", param, "}} must have a length of {.code 1} (i.e. a scalar), not a length of {.code {length(x)}}.",
    "i" = paste0("Provide a {.cls ", class, "} scalar for {.arg {", param, "}}, such as ", stem, ".")
  )

  ## return output
  return(out)

}

#' Default Message for Invalid Parameters
#'
#' @description Create the default message used when a parameter is found to
#'     be have an invalid value.
#'
#' @usage dis_msg_valid()
#'
#' @return A character string with the default message.
#'
#' @examples
#' dis_msg_valid()
#'
#' @export
dis_msg_valid <- function(){

  ## create output
  out <- c(
    "{.arg {param}} must be a valid value for the parameter {.arg {param}}, not {.code {x}}.",
    "i" = "Valid arguments for {.arg {param}} are: {.code {valid}}."
  )

  ## return output
  return(out)

}

#' Default Message for Environments
#'
#' @description Create the default message used when an object passed to an
#'     environment parameter does not have the correct class.
#'
#' @usage dis_msg_env(x, param = "call", call = rlang::caller_env())
#'
#' @param x Required object; a parameter argument to test.
#' @param param Required character scalar; the parameter name. The default
#'     matches the argument named for environments used through \code{disputeR}.
#' @param call Required environment; the environment in which the function was called.
#'     If nesting \code{dis_msg_class()} within another function, it is recommended to
#'     provide the calling environment to ensure the correct environment is referenced
#'     using \code{rlang::caller_env()} (default). If there are multiple levels of
#'     nesting, the \code{call} argument can be used to pass an environment created
#'     in the outermost function.
#'
#' @return A character string with the default message.
#'
#' @examples
#' dis_msg_env(x = "character", param = "call")
#'
#' @export
dis_msg_env <- function(x, param = "call", call = rlang::caller_env()){

  ## check inputs if FACT_CHECK is not FALSE
  if (!isFALSE(Sys.getenv(x = "FACT_CHECK"))){

    ### check call
    if (!is.environment(call)){
      cli::cli_abort(
        message = dis_msg_env(x = call)
      )
    }

    ### check param
    dis_not_missing(.f = rlang::is_missing(x))

    ### check param
    dis_not_missing(.f = rlang::is_missing(param), param = "class")

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

  }

  ## create output
  out <- c(
    paste0("{.arg {", param, "}} must be an environment object, not ", dis_indefinite(x), " {.cls {class(", param, ")}}."),
    "i" = "Provide an environment object for {.arg {", param, "}}."
  )

  ## return output
  return(out)

}
