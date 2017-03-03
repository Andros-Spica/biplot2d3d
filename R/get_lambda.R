#' @export get_lambda
get_lambda <- function(sdev,
                       n.obs,
                       dimensions = 1:2,
                       scale = 1,
                       pc.biplot = FALSE) {

  lambda <- sdev[dimensions]

  if (is.null(n <- n.obs)) n <- 1

  lambda <- lambda * sqrt(n)

  if (scale < 0 || scale > 1) warning("'scale' is outside [0, 1]")

  if (scale != 0) lambda <- lambda ^ scale
  else lambda <- 1

  if (pc.biplot) lambda <- lambda/sqrt(n)

  return(lambda)

}

