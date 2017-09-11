
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

      max_x <- max(abs(filtered_arrows[, 1])) * min_dist
      max_y <- max(abs(filtered_arrows[, 2])) * min_dist

      threshold <- sqrt((max_x ^ 2) + (max_y ^ 2))

      arrows_lengths <- sqrt((filtered_arrows[, 1] ^ 2) + (filtered_arrows[, 2] ^ 2))

      filtered_arrows <-
        subset(filtered_arrows, arrows_lengths > threshold)

    } else if (dimensions == 3) {

      max_x <- max(abs(filtered_arrows[, 1])) * min_dist
      max_y <- max(abs(filtered_arrows[, 2])) * min_dist
      max_z <- max(abs(filtered_arrows[, 3])) * min_dist

      threshold <- sqrt((max_x ^ 2) + (max_y ^ 2) + (max_z ^ 2))

      arrows_lengths <-
        sqrt((filtered_arrows[, 1] ^ 2) + (filtered_arrows[, 2] ^ 2) + (filtered_arrows[, 3] ^ 2))

      filtered_arrows <-
        subset(filtered_arrows, arrows_lengths > threshold)
    }
  }

  return(filtered_arrows)
}
