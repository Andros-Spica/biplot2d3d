
#' Filter covariance arrows for biplots
#'
#' Filter the variables most represented in the firsts dimensions of an ordination object.
#'
#' @param loadings Data frame, loadings or the covariances
#'                 between variables and the coordinates calculated.
#' @param min_dist Numeric, ratio representing the minimun distante
#'                 of arrow's points to their origin. 1 return no variables.
#' @param dimensions Numeric, the number of dimensions where arrows will be projected.
#'
#' @export filter_arrows
filter_arrows <- function(loadings, min_dist = 0.5, dimensions = 2){

  filtered_arrows <- loadings[, 1:dimensions]

  if (min_dist > 0) {

    if (min_dist == 1)  warning("min_dist = 1 is too restricte.")

    if (dimensions == 2) {

      max_x <- max(abs(loadings[, 1])) * min_dist
      max_y <- max(abs(loadings[, 2])) * min_dist

      threshold <- sqrt((max_x ^ 2) + (max_y ^ 2))

      arrows_lengths <- sqrt((loadings[, 1] ^ 2) + (loadings[, 2] ^ 2))

      filtered_arrows <-
        loadings[arrows_lengths > threshold, 1:2]

    } else if (dimensions == 3) {

      max_x <- max(abs(loadings[, 1])) * min_dist
      max_y <- max(abs(loadings[, 2])) * min_dist
      max_z <- max(abs(loadings[, 3])) * min_dist

      threshold <- sqrt((max_x ^ 2) + (max_y ^ 2) + (max_z ^ 2))

      arrows_lengths <-
        sqrt((loadings[, 1] ^ 2) + (loadings[, 2] ^ 2) + (loadings[, 3] ^ 2))

      filtered_arrows <-
        loadings[arrows_lengths > threshold, 1:3]
    }
  }

  return(filtered_arrows)
}
