
#' Draw ellipsoids per group
#'
#' Compute and draw a labeled ellipsoid for each group in a rgl device.
#' Singleton or groups with less than four observations are drawn as
#' individual spheres.
#'
#' @param x,y,z Numeric vectors representing point coordinates.
#' @param groups A factor vector of length \code{length(x)} containing
#'    the group assignation of each point.
#' @param group_color A vector of length \code{nlevels(groups)} containing the
#'    colors to be used in each group.
#' @param singleton_color,wire_color,shade_color,label_color The
#'    specific colors to be used in each ellipsoid's elements for each group.
#'    If not NULL, they override the \code{colors} argument.
#' @param type a character representing the type of ellipsoid filling: "wire",
#'    "shade" or "wire and shade" (\code{\link[rgl]{wire3d}},
#'    \code{\link[rgl]{shade3d}}).
#' @param level the confidence level of a simultaneous confidence region
#'    (\code{\link[rgl]{ellipse3d}}).
#' @param singleton_radius the radius of the spheres
#'    (\code{\link[rgl]{spheres3d}}) used to represent points of groups with
#'    three or less points.
#' @param wire_alpha,wire_lit the wire alpha and lit parameters
#'    (\code{\link[rgl]{rgl.material}}).
#' @param shade_alpha,shade_lit the shade alpha and lit parameters
#'    (\code{\link[rgl]{rgl.material}}).
#' @param label_cex,label_family,label_font,label_adj,label_alpha the group
#'    labels text parameters (\code{\link[rgl]{text3d}},
#'    \code{\link[rgl]{rgl.material}}). \code{label_adj} accepts a single
#'    numeric value (horizontal), a numeric vector of length two
#'    (horizontal, vertical) or a list of length \code{nlevels(groups)}
#'    containing the adj values for the specific groups.
#'
#' @seealso \code{\link{ellipsoid3d}}, \code{\link[rgl]{spheres3d}}
#'
#' @examples
#'
#' \dontrun{
#'
#' # Use iris data
#' data("iris")
#'
#' # introduce fictional singleton species
#' modIris <- iris
#' modIris$Species <- as.character(modIris$Species)
#' modIris <- rbind(modIris, list(6, 1, 2, 3, "outlier"))
#' modIris$Species <- factor(modIris$Species)
#'
#' # Initializes the rgl device
#' rgl_init(theta = 60, phi = 45)
#'
#' # add axes and bounding box
#' rgl_format(modIris$Sepal.Length, modIris$Sepal.Width, modIris$Petal.Length,
#'            axes_titles = c("Sepal length", "Sepal width", "Petal length"),
#'            show_planes = c("XZ", "XY", "YZ"))
#'
#' # Add data points
#' points3d(modIris[modIris$Species == "setosa", 1],
#'          modIris[modIris$Species == "setosa", 2],
#'          modIris[modIris$Species == "setosa", 3],
#'          color = "green")
#' points3d(modIris[modIris$Species == "versicolor", 1],
#'          modIris[modIris$Species == "versicolor", 2],
#'          modIris[modIris$Species == "versicolor", 3],
#'          color = "red")
#' points3d(modIris[modIris$Species == "virginica", 1],
#'          modIris[modIris$Species == "virginica", 2],
#'          modIris[modIris$Species == "virginica", 3],
#'          color = "blue")
#'
#' # Add ellipsoids
#' ellipsoids3d(modIris[, 1], modIris[, 2], modIris[, 3],
#'              groups = modIris$Species,
#'              group_color = c("purple", "green", "red", "blue"))
#'
#' remove(modIris)
#' }
#'
ellipsoids3d <-
  function(x, y, z, groups,
           group_color = rainbow(nlevels((groups))),
           type = "wire and shade",
           level = 0.95,
           singleton_color = NULL,
           singleton_radius = 0.1,
           wire_color = NULL,
           wire_alpha = 0.2,
           wire_lit = FALSE,
           shade_color = NULL,
           shade_alpha = 0.1,
           shade_lit = FALSE,
           label_color = NULL,
           label_cex = 1,
           label_family = "sans",
           label_font = 2,
           label_adj = c(-0.25, 0.5),
           label_alpha = 1) {

    # color override (was it introduced?)
    singleton_color_ <- singleton_color
    if (is.null(singleton_color))   singleton_color_ <- group_color
    wire_color_ <- wire_color
    if (is.null(wire_color))        wire_color_ <- group_color
    shade_color_ <- shade_color
    if (is.null(shade_color))       shade_color_ <- group_color
    label_color_ <- label_color
    if (is.null(label_color))       label_color_ <- group_color

    levs <- levels(groups)

    for (i in 1:length(levs)) {

      group <- levs[i]

      selected <- groups == group
      xx <- x[selected]
      yy <- y[selected]
      zz <- z[selected]

      # color override (Only if it has the required length?)
      singleton_color_ <- singleton_color_[1]
      if (length(singleton_color_) == nlevels(groups)) {
        singleton_color_ <-singleton_color_[i]
      }
      wire_color_ <- wire_color_[1]
      if (length(wire_color_) == nlevels(groups)) {
        wire_color_ <- wire_color_[i]
      }
      shade_color_ <- shade_color_[1]
      if (length(shade_color_) == nlevels(groups)) {
        shade_color_ <- shade_color_[i]
      }
      label_color_ <- label_color_[1]
      if (length(label_color_) == nlevels(groups)) {
        label_color_ <- label_color_[i]
      }

      # Is label_adj specified for each group?
      label_adj_ <- list(label_adj)[[1]]
      if (is.list(label_adj) & length(label_adj) == nlevels(groups)) {
        label_adj_ <- label_adj[[i]]
      }

      if (length(xx) > 3) {

        ellipsoid3d(xx, yy, zz,
                    type = type,
                    level = level,
                    label = group,
                    wire_color = wire_color_,
                    wire_alpha = wire_alpha,
                    wire_lit = wire_lit,
                    shade_color = shade_color_,
                    shade_alpha = shade_alpha,
                    shade_lit = shade_lit,
                    label_color = label_color_,
                    label_cex = label_cex,
                    label_family = label_family,
                    label_font = label_font,
                    label_adj = label_adj_,
                    label_alpha = label_alpha)

      } else {

        rgl::spheres3d(xx, yy, zz,
                       r = singleton_radius,
                       color = singleton_color_)

        rgl::texts3d(mean(xx), mean(yy), mean(zz),
                     text = group,
                     col = label_color_,
                     cex = label_cex,
                     family = label_family,
                     font = label_font,
                     adj = label_adj,
                     alpha = label_alpha)

      }
    }
  }

#' Draw an labeled custom ellipsoid
#'
#' Compute and draw a labeled ellipsoid in a rgl device.
#'
#' @param x,y,z Numeric vectors representing point coordinates.
#' @param color The default color to be used in all elements.
#' @param type Character, representing the type of ellipsoid filling: "wire",
#'    "shade" or "wire and shade" (\code{\link[rgl]{wire3d}},
#'    \code{\link[rgl]{shade3d}}).
#' @param level Numeric, the confidence level of a simultaneous confidence
#'    region
#'    (\code{\link[rgl]{ellipse3d}}).
#' @param label Character, The ellipsoid label.
#' @param wire_color,shade_color,label_color The specific colors to be used
#'    in the ellipsoid's elements. If not NULL, they override the
#'    \code{color} argument.
#' @param wire_alpha,wire_lit Numeric, the wire alpha and lit parameters
#'    (\code{\link[rgl]{rgl.material}}).
#' @param shade_alpha,shade_lit Numeric, the shade alpha and lit parameters
#'    (\code{\link[rgl]{rgl.material}}).
#' @param label_cex,label_family,label_font,label_adj,label_alpha The group
#'    labels text parameters (\code{\link[rgl]{text3d}},
#'    \code{\link[rgl]{rgl.material}}).
#'
#' @seealso \code{\link{ellipsoid3d}}, \code{\link{rgl_init}},
#'    \code{\link[rgl]{ellipse3d}}, \code{\link[rgl]{wire3d}},
#'    \code{\link[rgl]{shade3d}}, \code{\link[rgl]{texts3d}}
#'
#' @examples
#'
#' \dontrun{
#'
#' # Use iris data
#' data("iris")
#'
#' # get setosa
#' setosa <- iris[iris$Species == "setosa",]
#'
#' # Initializes the rgl device
#' rgl_init(zoom = 0.75)
#'
#' # add axes and bounding box
#' rgl_format(setosa$Sepal.Length, setosa$Sepal.Width, setosa$Petal.Length,
#'            axes_titles = c("Sepal length", "Sepal width", "Petal length"))
#'
#' # Add data points
#' points3d(setosa[, 1], setosa[, 2], setosa[, 3], color = "black")
#'
#' # Add ellipsoid
#' ellipsoid3d(setosa[, 1], setosa[, 2], setosa[, 3], label = "setosa",
#'             wire_color = "green", shade_color = "red",
#'             label_color = "blue", label_adj = c(-1, 0.5))
#'
#' remove(setosa)
#'
#' }
#'
ellipsoid3d <- function(x, y, z,
                        color = "black",
                        type = "wire and shade",
                        level = 0.95,
                        label = "",
                        wire_color = NULL,
                        wire_alpha = 0.2,
                        wire_lit = FALSE,
                        shade_color = NULL,
                        shade_alpha = 0.1,
                        shade_lit = FALSE,
                        label_color = NULL,
                        label_cex = 2,
                        label_family = "sans",
                        label_font = 2,
                        label_adj = c(-0.25, 0.5),
                        label_alpha = 1) {

  # color override (was it introduced?)
  wire_color_ <- wire_color
  if (is.null(wire_color))    wire_color_ <- color
  shade_color_ <- shade_color
  if (is.null(shade_color))   shade_color_ <- color
  label_color_ <- label_color
  if (is.null(label_color))   label_color_ <- color

  cv <- cov(cbind(x, y, z))

  ellips <- rgl::ellipse3d(cv,
                           centre = c(mean(x), mean(y), mean(z)),
                           level = level)

  if (type == "wire and shade" || type == "wire") {
    rgl::wire3d(ellips,
                col = wire_color_,
                alpha = wire_alpha,
                lit = wire_lit)
  }
  if (type == "wire and shade" || type == "shade") {
    rgl::shade3d(ellips,
                 col = shade_color_,
                 alpha = shade_alpha,
                 lit = shade_lit)
  }

  # show group labels
  rgl::texts3d(mean(x), mean(y), mean(z),
               text = label,
               col = label_color_,
               cex = label_cex,
               family = label_family,
               font = label_font,
               adj = label_adj,
               alpha = label_alpha)

}

#' Draw stars per group
#'
#' Compute and draw stars (centroid with links) for each group in a rgl device.
#'
#' @param x,y,z Numeric vectors representing point coordinates.
#' @param groups A factor vector of length \code{length(x)} containing
#'    the group assignation of each point.
#' @param group_color A vector of length \code{nlevels(groups)} containing the
#'    colors to be used in each group.
#' @param centroid_color A color or a vector of colors to be used in
#'    group centroid. If NULL, \code{group_color} is used.
#' @param centroid_radius Numeric, the radius of the spheres used to
#'    represent group centroids (\code{\link[rgl]{spheres3d}}).
#' @param centroid_alpha Numeric, the centroids alpha
#'    (\code{\link[rgl]{rgl.material}}).
#' @param link_color A color or a vector of colors to be used in
#'    links between observations and centroid in each group.
#'    If NULL, \code{group_color} is used.
#' @param link_width,link_alpha Numeric, the link width and alpha parameters
#'    (\code{\link[rgl]{segments3d}}, \code{\link[rgl]{rgl.material}}).
#' @param label_color A color or a vector of colors to be used in
#'    group labels. If NULL, \code{group_color} is used.
#' @param label_cex,label_family,label_font,label_adj,label_alpha The text
#'    parameters and the alpha of the group labels (\code{\link[rgl]{text3d}},
#'    \code{\link[rgl]{rgl.material}}). \code{label_adj} accepts a single
#'    numeric value (horizontal), a numeric vector of length two
#'    (horizontal, vertical) or a list of length \code{nlevels(groups)}
#'    containing the adj values for the specific groups.
#'
#' @seealso \code{\link{star3d}}
#'
#' @examples
#'
#' \dontrun{
#'
#' # Use iris data
#' data("iris")
#'
#' # introduce fictional singleton species
#' modIris <- iris
#' modIris$Species <- as.character(modIris$Species)
#' modIris <- rbind(modIris, list(6, 1, 2, 3, "outlier"))
#' modIris$Species <- factor(modIris$Species)
#'
#' # Initializes the rgl device
#' rgl_init(theta = 60, phi = 45, zoom = 0.75)
#'
#' # add axes and bounding box
#' rgl_format(modIris$Sepal.Length, modIris$Sepal.Width, modIris$Petal.Length,
#'            axes_titles = c("Sepal length", "Sepal width", "Petal length"),
#'            show_planes = c("XZ", "XY", "YZ"))
#'
#' # Add data points
#' points3d(modIris[modIris$Species == "setosa", 1],
#'          modIris[modIris$Species == "setosa", 2],
#'          modIris[modIris$Species == "setosa", 3],
#'          color = "green")
#' points3d(modIris[modIris$Species == "versicolor", 1],
#'          modIris[modIris$Species == "versicolor", 2],
#'          modIris[modIris$Species == "versicolor", 3],
#'          color = "red")
#' points3d(modIris[modIris$Species == "virginica", 1],
#'          modIris[modIris$Species == "virginica", 2],
#'          modIris[modIris$Species == "virginica", 3],
#'          color = "blue")
#'
#' # Add stars
#' stars3d(modIris[,1], modIris[,2], modIris[,3],
#'               groups = modIris$Species,
#'               group_color = c("purple","green","red","blue"),
#'               label_adj = list(c(-0.25, 0.5),
#'                                c(-0.5, 1.5),
#'                                c(-0.3, 1.2),
#'                                c(-0.3, 1.5)))
#'
#' remove(modIris)
#'
#' }
#'
stars3d <-
  function(x, y, z, groups,
           group_color = rainbow(nlevels((groups))),
           centroid_color = NULL,
           centroid_radius = 0.05,
           centroid_alpha = 0.5,
           link_color = NULL,
           link_width = 1,
           link_alpha = 1,
           label_color = NULL,
           label_cex = 1,
           label_family = "sans",
           label_font = 2,
           label_adj = c(-0.25, 0.5),
           label_alpha = 1) {

    # color override (was it introduced?)
    centroid_color_ <- centroid_color
    if (is.null(centroid_color)) centroid_color_ <- group_color
    link_color_ <- link_color
    if (is.null(link_color))     link_color_ <- link_color
    label_color_ <- label_color
    if (is.null(label_color))    label_color_ <- label_color

    levs <- levels(groups)

    for (i in 1:length(levs)) {

      group <- levs[i]

      selected <- groups == group
      xx <- x[selected]
      yy <- y[selected]
      zz <- z[selected]

      # color override (Only if it has the required length?)
      centroid_color_ <- centroid_color_[1]
      if (length(centroid_color_) == nlevels(groups)) {
        centroid_color_ <- centroid_color_[i]
      }
      link_color_ <- link_color_[1]
      if (length(link_color_) == nlevels(groups)) {
        link_color_ <- link_color_[i]
      }
      label_color_ <- label_color_[1]
      if (length(label_color_) == nlevels(groups)) {
        label_color_ <- label_color_[i]
      }

      # Is label_adj specified for each group?
      label_adj_ <- list(label_adj)[[1]]
      if (is.list(label_adj) & length(label_adj) == nlevels(groups)) {
        label_adj_ <- label_adj[[i]]
      }

      star3d(xx, yy, zz,
             color = centroid_color_,
             label = group,
             centroid_radius = centroid_radius,
             centroid_alpha = centroid_alpha,
             link_color = link_color_,
             link_width = link_width,
             link_alpha = link_alpha,
             label_color = label_color_,
             label_cex = label_cex,
             label_family = label_family,
             label_font = label_font,
             label_adj = label_adj_,
             label_alpha = label_alpha)
    }
  }

#' Draw stars
#'
#' Compute and draw a star (centroid with links) in a rgl device.
#'
#' @param x,y,z Numeric vectors representing point coordinates.
#' @param color The default color to be used in all elements.
#' @param centroid_radius Numeric, the radius of the sphere used to
#'    represent the centroid of the distribution (\code{\link[rgl]{spheres3d}}).
#' @param centroid_color The color of centroid point of the distribution.
#' @param centroid_alpha Numeric, the centroids alpha
#'    (\code{\link[rgl]{rgl.material}}).
#' @param link_color The color of the links connecting centroid and points.
#' @param link_width,link_alpha Numeric, the link width and alpha parameters
#'    (\code{\link[rgl]{segments3d}}, \code{\link[rgl]{rgl.material}}).
#' @param label The label placed at the centroid of the distribution.
#' @param label_color The color of the centroid label.
#' @param label_cex,label_family,label_font,label_adj,label_alpha The text
#'    parameters and the alpha of the centroid label (\code{\link[rgl]{text3d}},
#'    \code{\link[rgl]{rgl.material}}).
#'
#' @seealso \code{\link{stars3d}}, \code{\link{rgl_init}},
#'    \code{\link[rgl]{spheres3d}}, \code{\link[rgl]{segments3d}},
#'    \code{\link[rgl]{texts3d}}
#'
#' @examples
#'
#' \dontrun{
#'
#' # Use iris data
#' data("iris")
#'
#' # get setosa
#' setosa <- iris[iris$Species == "setosa",]
#'
#' # Initializes the rgl device
#' rgl_init(zoom = 0.8)
#'
#' # add axes and bounding box
#' rgl_format(setosa$Sepal.Length, setosa$Sepal.Width, setosa$Petal.Length,
#'            axes_titles = c("Sepal length", "Sepal width", "Petal length"),
#'            show_planes = c("XZ", "XY", "YZ"))
#'
#' # Add data points
#' points3d(setosa[, 1], setosa[, 2], setosa[, 3], color = "black")
#'
#' # Add star
#' star3d(setosa[, 1], setosa[, 2], setosa[, 3], label = "setosa",
#'        centroid_color = "green", link_color = "red", label_color = "blue")
#'
#' remove(setosa)
#'
#' }
#'
star3d <- function(x, y, z,
                   color = "black",
                   label = "",
                   centroid_color = NULL,
                   centroid_radius = 0.05,
                   centroid_alpha = 0.5,
                   link_color = NULL,
                   link_width = 1,
                   link_alpha = 1,
                   label_color = NULL,
                   label_cex = 2,
                   label_family = "sans",
                   label_font = 2,
                   label_adj = c(-0.25, 0.5),
                   label_alpha = 1) {

  centroid <- c(mean(x), mean(y), mean(z))

  # color override (was it introduced?)
  centroid_color_ <- centroid_color
  if (is.null(centroid_color)) centroid_color_ <- color
  link_color_ <- link_color
  if (is.null(link_color))     link_color_ <- color
  label_color_ <- label_color
  if (is.null(label_color))    label_color_ <- color

  rgl::spheres3d(centroid[1], centroid[2], centroid[3],
                 r = centroid_radius,
                 color = centroid_color_,
                 alpha = centroid_alpha)

  if (length(x) > 1) {
    for (j in 1:length(x)) {
      rgl::segments3d(c(centroid[1], x[j]),
                      c(centroid[2], y[j]),
                      c(centroid[3], z[j]),
                      color = link_color_,
                      lwd = link_width,
                      alpha = link_alpha)
    }
  }

  # show group labels
  rgl::texts3d(centroid[1], centroid[2], centroid[3],
               text = label,
               col = label_color_,
               cex = label_cex,
               family = label_family,
               font = label_font,
               adj = label_adj,
               alpha = label_alpha)

}
