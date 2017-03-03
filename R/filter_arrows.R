#' @export filter_arrows
filter_arrows <- function(loadings, mim_arrow_dist = 0.5, dimensions = 2){

  filtered_arrows <- loadings[, 1:dimensions]

  if (mim_arrow_dist > 0) {

    if (dimensions == 2) {

      max_x <- max(abs(loadings[, 1])) * mim_arrow_dist
      max_y <- max(abs(loadings[, 2])) * mim_arrow_dist

      threshold <- sqrt((max_x ^ 2) + (max_y ^ 2))

      arrows_lengths <- sqrt((loadings[, 1] ^ 2) + (loadings[, 2] ^ 2))

      filtered_arrows <-
        loadings[arrows_lengths > threshold, 1:2]

    } else if (dimensions == 3) {

      max_x <- max(abs(loadings[, 1])) * mim_arrow_dist
      max_y <- max(abs(loadings[, 2])) * mim_arrow_dist
      max_z <- max(abs(loadings[, 3])) * mim_arrow_dist

      threshold <- sqrt((max_x ^ 2) + (max_y ^ 2) + (max_z ^ 2))

      arrows_lengths <-
        sqrt((loadings[, 1] ^ 2) + (loadings[, 2] ^ 2) + (loadings[, 3] ^ 2))

      filtered_arrows <-
        loadings[arrows_lengths > threshold, 1:3]
    }
  }
  return(filtered_arrows)
}
