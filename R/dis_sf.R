#' Validate sf Object Arguments
#'
#' @description This function runs standard unit tests on sf object parameters for
#'     functions. If the suggested \code{sf} package is not installed, it will only
#'     test that \code{x} is an object of class \code{sf}. The remaining checks for
#'     geometry column name, geometry type, crs, and whether or not a geographic
#'     coordinate system is acceptable all require that the \code{sf} package
#'     is installed.
#'
#' @usage dis_sf(x, valid_geometry_name = NULL, valid_geometry_type = NULL,
#'     valid_crs = NULL, valid_longlat = NULL, null_valid = TRUE, param = NULL,
#'     call = NULL, fact_check = "global")
#'
#' @param x Required object; a parameter argument to test.
#' @param valid_geometry_name Optional character scalar; required name for the
#'     geometry column. If \code{NULL} (default), any name is permissible. If a
#'     character string is included (i.e. \code{"geometry"}, the typical name of
#'     a geometry column), the \code{sf} object must have a geometry column
#'     whose name matches the argument. This test requires that \code{sf} is
#'     installed.
#' @param valid_geometry_type Optional character scalar; required geometry type(s)
#'     for \code{x}. The most common are \code{"POINT"}, \code{"LINESTRING"}, and
#'     \code{"POLYGON"} as well as the \code{"MULTI"} versions of each
#'     (\code{"MULTIPOINT"}, \code{"MULTILINESTRING"}, and \code{"MULTIPOLYGON"}).
#'
#'     Other valid geometry types are \code{"GEOMETRY"} and \code{"GEOMETRYCOLLECTION"},
#'     which is used for "geometry collections." However, you should note that
#'     geometry collections will cause errors with some spatial data operations.
#'
#'     Less common geometry types include \code{"CIRCULARSTRING"}, \code{"COMPOUNDCURVE"},
#'     \code{"CURVEPOLYGON"}, \code{"MULTICURVE"}, \code{"MULTISURFACE"},
#'     \code{"CURVE"}, \code{"SURFACE"}, \code{"POLYHEDRALSURFACE"},
#'     \code{"TIN"}, and \code{"TRIANGLE"}.
#'
#'     If multiple values are given, \code{x} can be any one  of those values.
#'     If \code{NULL} (default), this test is skipped. This test requires that
#'     \code{sf} is installed.
#'
#' @param valid_crs Optional character scalar; the required CRS value that
#'     \code{x} must use. If multiple values are given, \code{x} can be any one
#'     of those values. If \code{NULL} (default), this test is skipped. This test
#'     requires that \code{sf} is installed.
#' @param valid_longlat Optional logical scalar; if \code{TRUE}, \code{x} must
#'     use a geographic coordinate system. If \code{FALSE}, \code{x} must use a
#'     projected coordinate system. If \code{NULL} (default), this test is skipped.
#'     This test requires that \code{sf} is installed.
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
#' # create example function that uses dis_sf()
#' example <- function(x){
#'
#'   ## check inputs with disputeR
#'   dis_not_missing(.f = rlang::is_missing(x))
#'   dis_sf(x)
#'
#'   ## return output
#'   return(x)
#'
#' }
#'
#' # test example function
#' # example(x = sf)
#'
#' @export
dis_sf <- function(x, valid_geometry_name = NULL, valid_geometry_type = NULL,
                   valid_crs = NULL, valid_longlat = NULL,
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

      ### check character inputs
      dis_character(valid_geometry_name)
      dis_character(valid_geometry_type)
      dis_character(valid_crs)

      ### check null_valid
      dis_logical(null_valid)
      dis_logical(valid_longlat)

      ### check that valid_longlat and valid_crs cannot both be non-NULL
      if (!is.null(valid_crs) & !is.null(valid_longlat)){
        cli::cli_abort(
          message = c(
            "Only one of {.arg valid_crs} or {.arg valid_longlat} may be specified in a test.",
            "i" = "The {.arg valid_crs} test is more restrictive, allowing only those {.code crs} values listed.",
            "i" = "The {.arg valid_longlat} test is more flexible, allowing any coordinate system as long as it meets the test condition ({.code TRUE} for geographic coordinate systems or {.code FALSE} for projected coordinate systems."
          ),
          call = call
        )
      }

    }

    ## unit tests
    ### test whether x is NULL
    dis_null(x = x, class = "data.frame", null_valid = null_valid)

    ### tests for x as long as it is not NULL
    if (!is.null(x)){

      ### test that x is a sf object
      if (!inherits(x, what = "sf")){
        cli::cli_abort(
          message = dis_msg_class(x = x, class = "sf", type = "object", stem = "{.code {param} = sf_obj}"),
          call = call
        )
      }

      ### more detailed tests on sf objects
      if (rlang::is_installed("sf")) {

        ### test that the expected geometry column name is used
        if (!is.null(valid_geometry_name)){
          if (attr(x, "sf_column") != valid_geometry_name){
            cli::cli_abort(
              message = c(
                "{.arg {param}} must have a geometry column named {.arg {valid_geometry_name}}, not {.code {attr(x, 'sf_column')}}.",
                "i" = "Change the {.code geometry} column name with {.code sf::st_geometry({param}) <- '{valid_geometry_name}'}."
              ),
              call = call
            )
          }
        }

        ### test that object as valid geometry type
        if (!is.null(valid_geometry_type)){
          sfc <- unique(sf::st_geometry_type(x))

          if (isFALSE(all(sfc %in% valid_geometry_type))){
            cli::cli_abort(
              message = c(
                "{.arg {param}} must be a valid geometry type, not {.code {sfc}}.",
                "i" = "Valid geometry types for {.arg {param}} are: {.code {valid_geometry_type}}."
              ),
              call = call
            )
          }
        }

        ### test that the expected CRS is used
        if (!is.null(valid_crs)){
          if (!sf::st_crs(x)$epsg %in% valid_crs){
            cli::cli_abort(
              message = c(
                "{.arg {param}} must have a valid {.code crs}, not {.code {sf::st_crs(x)$epsg}} ({.code {sf::st_crs(x)$input}}).",
                "i" = "Valid {.code crs} values for {.arg {param}} are: {.code {valid_crs}}.",
                "i" = "Use {.code sf::st_transform({param}, crs = {valid_crs})} to re-project your data."
              ),
              call = call
            )
          }
        }

        ### test that x is in a geographic coordinate system
        if (!is.null(valid_longlat)){
          if (sf::st_is_longlat(x) != valid_longlat){
            if (isTRUE(valid_longlat)){
              cli::cli_abort(
                message = c(
                  "{.arg {param}} must use a geographic coordinate system, not a projected coordinate system.",
                  "i" = "Use {.code sf::st_crs({param})} to inspect your data's coordinate system.",
                  "i" = "Use {.code sf::st_transform({param})} to re-project your data to a geographic coordinate system that is appropriate for your data.",
                  "*" = "For example, if you wanted to use WGS84, you would use {.code {param} <- sf::st_transform({param}, crs = 4326)} to re-project your data.",
                  "*" = "Websites like {.url https://epsg.io} are helpful resources for finding appropriate coordinate systems."
                ),
                call = call
              )
            } else {
              cli::cli_abort(
                message = c(
                  "{.arg {param}} must use a projected coordinate system, not a geographic coordinate system.",
                  "i" = "Use {.code sf::st_crs({param})} to inspect your data's coordinate system.",
                  "i" = "Use {.code sf::st_transform()} to re-project your data to a projected coordinate system that is appropriate for your data.",
                  "*" = "For example, if you wanted to use Winkel Tripel, you would use {.code {param} <- sf::st_transform({param}, crs = 'ESRI:53042')} to re-project your data.",
                  "*" = "Websites like {.url https://epsg.io} are helpful resources for finding appropriate coordinate systems."
                ),
                call = call
              )
            }
          }
        }

      } else {
        cli::cli_inform(message = c(
          "i" = "The {.pkg sf} package is required for more detailed tests of {.code sf} objects beyond verifying {param} is a {.cls sf} object.",
          "i" = "Use {.code install.packages('sf')} to install {.pkg sf}."
          )
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
