#' Calculate Biomass Index
#'
#' Calculates a biomass index by multiplying CPUE by area swept. Returns
#' `NA` for any row where either input is `NA`.
#'
#' @param cpue Numeric vector of CPUE values (units: kg/hr).
#' @param area_swept Numeric vector of area swept in **km^2^**.
#' @return A numeric vector the same length as `cpue`.
#' @inheritParams cpue
#' @inheritDotParams cpue -catch -effort
#'
#' @param data A data frame. See [dplyr::filter()] for subsetting rows.
#'
#' @details
#' Two modes of use:
#'
#' - Provide `cpue` directly for a simple calculation.
#' - Provide `catch` and `effort` to compute CPUE first, then scale by area.
#'
#' Any additional arguments in `...` are forwarded to [cpue()].
#'
#' Returns the same structure as [the CPUE function][cpue()].
#'
#' @seealso [cpue()] to compute CPUE values from raw catch and effort.
#' @return A numeric vector the same length as `cpue`.
#' @export
#'
#' @examples
#' salmon_cpue <- cpue(catch = 2, effort = 2)
#' biomass_index(salmon_cpue, 5)
#'
biomass_index <- function(cpue = NULL,
                          area_swept,
                          catch = NULL,
                          effort = NULL,
                          verbose = getOption("fishr.verbose", default = FALSE),
                          ...
                          ) {
  rlang::check_dots_used()

  if(is.null(cpue) && (!is.null(catch) && !is.null(effort))) {
    cpue <- cpue(catch, effort, verbose = verbose, ...)
  }

  if (is.null(cpue)) {
    stop("must provide either 'cpue' or both 'catch' and 'effort'.")
  }

  validate_numeric_inputs(cpue = cpue, area_swept = area_swept)

  if (verbose) {
    message("calculating biomass index for ", length(area_swept), " records")
  }

  cpue * area_swept
}
