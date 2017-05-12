#' Calculate lambda (biplot)
#'
#' Calculate the lambda factor to scale biplot dimensions.
#' The code is a snippet of \code{\link[stats]{biplot.princomp}}).
#'
#' @param sdev Data frame containing the stardand deviations
#'             along n dimensions
#' @param n.obs number of observations (rows) in the original data
#' @param dimensions The number of dimensions
#' @param scale Numeric, the value defining the
#'    degree in which distances between observations have
#'    priority over distances between variables
#'    (0 = observation-focused, 1 = variable-focused).
#'    It corresponds to the argument \code{scale} in
#'    \code{\link[stats]{biplot.princomp}}.
#' @param pc.biplot Character, indicating the type of
#'    biplot transformation of data: "default" and
#'    "pc.biplot", corresponding to the transformations
#'    performed in \code{\link[stats]{biplot.princomp}}
#'    with \code{pc.biplot = FALSE} ("default") and
#'    \code{pc.biplot = TRUE} ("pc.biplot"). If NULL,
#'    no processing is performed, assuming that data
#'    within \code{ordination_object} was previously transformed.
#'
#' @export get_lambda
get_lambda <- function(sdev,
                       n.obs,
                       dimensions = 2,
                       scale = 1,
                       pc.biplot = FALSE) {

  lambda <- sdev[1:dimensions]

  if (is.null(n <- n.obs)) n <- 1

  lambda <- lambda * sqrt(n)

  if (scale < 0 || scale > 1) warning("'scale' is outside [0, 1]")

  if (scale != 0) lambda <- lambda ^ scale
  else lambda <- 1

  if (pc.biplot) lambda <- lambda/sqrt(n)

  return(lambda)

}

