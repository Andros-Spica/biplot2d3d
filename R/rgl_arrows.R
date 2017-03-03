
#' Places a set of arrows in a rgl device
#'
#' @param x,y,z Numeric vectors representing point coordinates where arrows
#'    are pointing to.
#' @param variable_names Character vector with the names of the variables
#'    to be plotted as arrow labels.
#' @param center_pos Numeric vector of length three containing the 3D
#'    position from which arrows are drawn.
#' @param color The color or colors to be used in arrows.
#' @param head_shape_theta Numeric, the angle of the barb of the arrows
#'    head ('theta' in \code{\link[heplots]{arrow3d}}).
#' @param head_shape_n Numeric, the number of barbs in the arrows head
#'    ('n' in \code{\link[heplots]{arrow3d}}).
#' @param head_size Numeric, the size of the arrows head ('s' in
#'    \code{\link[heplots]{arrow3d}}).
#' @param body_width Numeric, the width of the arrows body ('lwd' in
#'    \code{\link[heplots]{arrow3d}}).
#' @param body_length Numeric, the length of the arrows body, as a fraction
#'    of the true distance from the origin.
#' @param label_color The color or colors of arrows' labels.
#' @param label_cex,label_family,label_font,label_adj,label_alpha
#'    The text parameters and the alpha of the arrows' labels
#'    (\code{\link[rgl]{text3d}}, \code{\link[rgl]{rgl.material}}).
#'
#' @seealso \code{\link[heplots]{arrow3d}},\code{\link{biplot_3d}}
#'
#' @examples
#'
#' \dontrun{
#'
#' # Use iris data
#' data("iris")
#'
#' # Principal Components Analysis
#' pca <- princomp(iris[, 1:4])
#'
#' # Initializes the rgl device
#' rgl_init(theta = 330)
#'
#' # add axes and bounding box
#' rgl_format(x = pca$scores[, 1], y = pca$scores[, 2], z = pca$scores[, 3],
#'            axis_titles = c("PCA-1", "PCA-2", "PCA-3"))
#'
#' # Add data points
#' points3d(x = pca$scores[iris$Species=="setosa", 1],
#'          y = pca$scores[iris$Species=="setosa",2],
#'          z = pca$scores[iris$Species=="setosa", 3],
#'          color = "green")
#' points3d(x = pca$scores[iris$Species=="versicolor", 1],
#'          y = pca$scores[iris$Species=="versicolor",2],
#'          z = pca$scores[iris$Species=="versicolor", 3],
#'          color = "red")
#' points3d(x = pca$scores[iris$Species=="virginica", 1],
#'          y = pca$scores[iris$Species=="virginica",2],
#'          z = pca$scores[iris$Species=="virginica", 3],
#'          color = "blue")
#'
#' # Add covariance arrows
#' covArrows3D(x = pca$loadings[,1],
#'             y = pca$loadings[,2],
#'             z = pca$loadings[,3],
#'             body_width = 5)
#'
#'}
#'
#' @export covArrows3D
#'
covArrows3D <-
  function(x, y, z,
           variable_names = names(x),
           center_pos = c(0, 0, 0),
           color = "darkorange",
           head_shape_theta = pi / 6,
           head_shape_n = 3,
           head_size = 0.1,
           body_length = 1,
           body_width = 1,
           label_color = "black",
           label_cex = 1,
           label_family = "serif",
           label_font = 2,
           label_adj = 0,
           label_alpha = 1) {

    for (i in 1:length(x)) {

      # arrow
      trans_pos <- ((c(x[i], y[i], z[i]) * body_length) + center_pos)

      # Is there a single color or was it specified for each arrow?
      color_ <- color
      if (length(color) == length(x)) color_ <- color[i]

      heplots::arrow3d(p0 = center_pos,
                       p1 = trans_pos,
                       s = head_size,
                       theta = head_shape_theta,
                       n = head_shape_n,
                       color = color_,
                       lwd = body_width)

      # arrow labels
      label_color_ <- label_color
      if (length(label_color) == length(x)) label_color_ <- label_color[i]

      rgl::texts3d(x = trans_pos[1],
                   y = trans_pos[2],
                   z = trans_pos[3],
                   text = variable_names[i],
                   col = label_color,
                   cex = label_cex,
                   family = label_family,
                   font = label_font,
                   adj = label_adj,
                   alpha = label_alpha)
    }
  }
