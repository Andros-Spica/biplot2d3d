
#' Initializes a rgl device
#'
#' Initializes a new rgl device using specific settings.
#'
#' @param new_device If there is a rgl device open, should yet another be opened?
#' @param bg_color The background color (\code{\link[rgl]{bg3d}}).
#' @param view_theta,view_phi,view_fov,view_zoom The theta and phi angles
#'    (polar coordinates), the field-of-view angle,
#'    and the zoom level of the viewpoint (\code{\link[rgl]{view3d}}).
#' @param width,height The width and height of the rgl device window in pixels
#'    (\code{\link[rgl]{par3d}}).
#'
#' @note This function is based on the tutorial "A complete guide to 3D
#'    visualization device system in R - R software and data
#'    visualization" available in sthda.com Web site and last accessed in may
#'    24 2016 (\url{http://www.sthda.com/english/wiki/a-complete-guide-to-3d-visualization-device-system-in-r-r-software-and-data-visualization})
#'
#' @seealso \code{\link[rgl]{bg3d}}, \code{\link[rgl]{view3d}},
#'    \code{\link[rgl]{par3d}}, \code{\link[rgl]{rgl.open}},
#'    \code{\link[rgl]{rgl.close}}, \code{\link[rgl]{rgl.clear}}
#'
#' @examples
#'
#' \dontrun{
#'
#' rgl_init()
#' rgl_init(bg = "black")
#' rgl_init(width = 400, height = 300)
#'
#' }
#'
rgl_init <- function(new_device = FALSE,
                     bg_color = "white",
                     view_theta = 15,
                     view_phi = 20,
                     view_fov = 60,
                     view_zoom = 0.8,
                     width = 800,
                     height = 600) {

  if (!new_device & rgl::rgl.cur() != 0) {

    rgl::rgl.close()

  }

  rgl::rgl.open()# Open a new RGL device

  rgl::par3d(windowRect = 50 + c(0, 0, width, height))

  rgl::bg3d(color = bg_color)# Setup the background color

  rgl::rgl.clear(type = c("shapes", "bboxdeco"))

  rgl::view3d(theta = view_theta,
              phi = view_phi,
              fov = view_fov,
              zoom = view_zoom
  )

}

# Description of the used RGL functions:
## rgl.open(): open a new device
## rgl.cur(): returns active device ID
## par3d(windowRect): set the window size
## rgl.viewpoint(theta, phi, fov, zoom): set up viewpoint. The arguments theta and
## phi are polar coordinates.
## theta and phi are the polar coordinates.
## Default values are 0 and 15, respectively
## fov is the field-of-view angle in degrees. Default value is 60
## zoom is the zoom factor. Default value is 1
## rgl.bg(color): define the background color of the device
## rgl.clear(type): Clears the scene from the specified stack ("shapes", "lights",
## "bboxdeco", "background")

#' Set up a rgl device
#'
#' \code{rgl_format} set up the elements of a rgl device (axes, aspect, bounding
#'    box, projection planes) according to the space of the input data.
#'
#' @param x,y,z The numeric vectors corresponding to the 3D coordinates of
#'    points.
#' @param axes_colors The axes colors.
#' @param axes_head_size The size of the head (end point) of the axes.
#' @param axes_titles The axes titles.
#' @param axes_titles_cex,axes_titles_font,axes_titles_family,axes_titles_adj,axes_titles_alpha The text parameters and the alpha of the titles
#'    (\code{\link[rgl]{text3d}}). \code{axes_title_adj} accepts a single
#'    numeric value (horizontal), a numeric vector of length two
#'    (horizontal, vertical) or a list of length three (with elements named
#'    x, y and z) containing the adj values for the specific axes.
#' @param aspect A vector of length three indicating the relative scale of each
#'    dimension, to be past to \code{\link[rgl]{aspect3d}}.
#'    By default, aspect is balanced (i.e. c(1, 1, 1)).
#'    If equals NULL, the "true" aspect is calculated
#'    with \code{\link{calculate_aspect}}.
#' @param symmetric_axes Logical, whether the axes should
#'    be drawn symmetrically.
#' @param adapt_axes_origin Logical, whether to adapt the axes origin.
#' @param show_axes Character vector, indicating which axes to draw:
#'    "X","Y","Z". NULL draws no axes.
#' @param show_planes Character vector, indicating which planes to draw:
#'    "XZ","XY","YZ". NULL draws no plane.
#' @param show_bbox Logical, indicating whether to draw a bounding box.
#' @param planes_colors,planes_textures The colors and textures to be used
#'    in the planes. A vector can be given, following the order "XY","XZ"
#'    and "YZ". At least one color must be given.
#' @param planes_alpha,planes_lit,planes_shininess The graphical parameters of
#'    the planes (\code{\link[rgl]{rgl.material}}).
#' @param bbox_color,bbox_alpha,bbox_shininess,bbox_xat,bbox_yat,bbox_zat,bbox_xlab,bbox_ylab,bbox_zlab,bbox_xunit,bbox_yunit,bbox_zunit,bbox_xlen,bbox_ylen,bbox_zlen,bbox_marklen,bbox_marklen_rel,bbox_expand,bbox_draw_front arguments
#'    passed to create a bounding box (\code{\link[rgl]{bbox3d}}).
#'
#' @note This function is based on the tutorial "A complete guide to 3D
#'   visualization device system in R - R software and data visualization"
#'   available in sthda.com Web site and last accessed in may 24 2016
#'   (\url{http://www.sthda.com/english/wiki/a-complete-guide-to-3d-visualization-device-system-in-r-r-software-and-data-visualization})
#'
#' @seealso \code{\link{calculate_aspect}}, \code{\link{rgl_init}},
#'    \code{\link[rgl]{aspect3d}}, \code{\link[rgl]{axis3d}},
#'    \code{\link[rgl]{bbox3d}}
#'
#' @examples
#'
#' \dontrun{
#'
#' # Use iris data
#' data("iris")
#'
#' # Initializes the rgl device
#' rgl_init(theta = 60)
#'
#' # add axes and bounding box
#' rgl_format(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length,
#'            aspect = c(1, 1, 1),
#'            axes_titles = c("Sepal length", "Sepal width", "Petal length"),
#'            show_planes = c("XY", "XZ", "YZ"))
#'
#' # Add data points
#' points3d(iris[iris$Species == "setosa", 1],
#'          iris[iris$Species == "setosa", 2],
#'          iris[iris$Species == "setosa", 3], color = "green")
#' points3d(iris[iris$Species == "versicolor", 1],
#'          iris[iris$Species == "versicolor", 2],
#'          iris[iris$Species == "versicolor", 3], color = "red")
#' points3d(iris[iris$Species == "virginica", 1],
#'          iris[iris$Species == "virginica", 2],
#'          iris[iris$Species == "virginica", 3], color = "blue")
#' }
#'
rgl_format <- function(x, y, z,
                       aspect = c(1, 1, 1),
                       symmetric_axes = FALSE,
                       show_axes = c("X", "Y", "Z"),
                       show_planes = NULL,
                       show_bbox = FALSE,
                       adapt_axes_origin = TRUE,
                       axes_colors = "darkgrey",
                       axes_head_size = 3,
                       axes_titles = "",
                       axes_titles_cex = 2,
                       axes_titles_font = 2,
                       axes_titles_family = "sans",
                       axes_titles_adj = list(x = c(-0.1, 0),
                                              y = c(0.5, -0.5),
                                              z = c(1.1, 0)),
                       axes_titles_alpha = 1,
                       planes_colors = c("lightgrey", "lightgrey", "lightgrey"),
                       planes_textures = NULL,
                       planes_alpha = 0.5,
                       planes_lit = TRUE,
                       planes_shininess = 50,
                       bbox_color = c("#333377", "black"),
                       bbox_alpha = 0.5,
                       bbox_shininess = 5,
                       bbox_xat = NULL,
                       bbox_xlab = NULL,
                       bbox_xunit = 0,
                       bbox_xlen = 3,
                       bbox_yat = NULL,
                       bbox_ylab = NULL,
                       bbox_yunit = 0,
                       bbox_ylen = 3,
                       bbox_zat = NULL,
                       bbox_zlab = NULL,
                       bbox_zunit = 0,
                       bbox_zlen = 3,
                       bbox_marklen = 15,
                       bbox_marklen_rel = TRUE,
                       bbox_expand = 1,
                       bbox_draw_front = FALSE)
{

  lim <- function(var) {
    rangeVar <- max(var) - min(var)
    c(min(var) - rangeVar * 0.1, max(var) + rangeVar * 0.1)
  }
  if (symmetric_axes) {
    lim <- function(var) {
      rangeVar <- max(var) - min(var)
      c(-max(abs(var)), max(abs(var))) * 1.1
    }
  }

  xlim <- lim(x)
  ylim <- lim(y)
  zlim <- lim(z)

  origin <- c(0,0,0)
  if (adapt_axes_origin) {
    origin <- c(xlim[1], ylim[1], zlim[1])
  }

  if (is.null(aspect)) { aspect <- calculate_aspect(x, y, z)}
  rgl::aspect3d(aspect[1], aspect[2], aspect[3])

  if (!is.null(show_axes)) {
    # Add axes
    axis_color_x_ <- axes_colors[1]
    axis_color_y_ <- axes_colors[1]
    axis_color_z_ <- axes_colors[1]
    if (length(axes_colors) == 3) {
      axis_color_y_ <- axes_colors[2]
      axis_color_z_ <- axes_colors[3]
    }

    axis_title_x_ <- axes_titles[1]
    axis_title_y_ <- axes_titles[1]
    axis_title_z_ <- axes_titles[1]
    if (length(axes_titles) == 3) {
      axis_title_y_ <- axes_titles[2]
      axis_title_z_ <- axes_titles[3]
    }

    if ("X" %in% show_axes) {
      rgl::lines3d(xlim, c(origin[2], origin[2]), c(origin[3], origin[3]),
                   color = axis_color_x_)
      rgl::points3d(c(xlim[2], origin[2], origin[3]),
                    color = axis_color_x_, size = axes_head_size)
      rgl::texts3d(c(xlim[2],
                     origin[2],
                     origin[3]),
                   text = axis_title_x_,
                   color = axis_color_x_,
                   cex = axes_titles_cex,
                   font = axes_titles_font,
                   family = axes_titles_family,
                   adj = axes_titles_adj$x,
                   alpha = axes_titles_alpha)

      rgl::axis3d('x', pos = c(NA, origin[2], origin[3]), col = axis_color_x_)
    }

    if ("Y" %in% show_axes) {
      rgl::lines3d(c(origin[1], origin[1]), ylim, c(origin[3], origin[3]),
                   color = axis_color_y_)
      rgl::points3d(c(origin[1], ylim[2], origin[3]),
                    color = axis_color_y_, size = axes_head_size)
      rgl::texts3d(c(origin[1],
                     ylim[2],
                     origin[3]),
                   text = axis_title_y_,
                   color = axis_color_y_,
                   cex = axes_titles_cex,
                   font = axes_titles_font,
                   family = axes_titles_family,
                   adj = axes_titles_adj$y,
                   alpha = axes_titles_alpha)

      rgl::axis3d('y', pos = c(origin[1], NA, origin[3]), col = axis_color_y_)
    }

    if ("Z" %in% show_axes) {
      rgl::lines3d(c(origin[1], origin[1]), c(origin[2], origin[2]), zlim,
                   color = axis_color_z_)
      rgl::points3d(c(origin[1], origin[2], zlim[2]),
                    color = axis_color_z_, size = axes_head_size)
      rgl::texts3d(c(origin[1],
                     origin[2],
                     zlim[2]),
                   text = axis_title_z_,
                   color = axis_color_z_,
                   cex = axes_titles_cex,
                   font = axes_titles_font,
                   family = axes_titles_family,
                   adj = axes_titles_adj$z,
                   alpha = axes_titles_alpha)

      rgl::axis3d('z', pos = c(origin[1], origin[2], NA), col = axis_color_z_)
    }
    # For the function rgl.lines(), the arguments x, y, and z are numeric vectors of
    # length 2 (i.e, : x = c(x1,x2), y = c(y1, y2), z = c(z1, z2) ).
    ## The values x1, y1 and y3 are the 3D coordinates of the line starting point.
    ## The values x2, y2 and y3 corresponds to the 3D coordinates of the line ending
    ## point.
    # Add a point at the end of each axes to specify the direction
    # Add axis labels
    # Show tick marks
  }

  if (!is.null(show_planes)) {

    # Add planes

    plane_color_xy_ <- planes_colors[1]
    plane_color_xz_ <- planes_colors[1]
    plane_color_yz_ <- planes_colors[1]
    if (length(planes_colors) == 3) {
      plane_color_xz_ <- planes_colors[2]
      plane_color_yz_ <- planes_colors[3]
    }

    plane_texture_xy_ <- planes_textures[1]
    plane_texture_xz_ <- planes_textures[1]
    plane_texture_yz_ <- planes_textures[1]
    if (length(planes_textures) == 3) {
      plane_texture_xz_ <- planes_textures[2]
      plane_texture_yz_ <- planes_textures[3]
    }

    if ("XY" %in% show_planes) {
      rgl::quads3d(x = rep(xlim, each = 2),
                   z = c(origin[3], origin[3], origin[3], origin[3]),
                   y = c(ylim[1], ylim[2], ylim[2], ylim[1]),
                   color = plane_color_xy_,
                   texture = plane_texture_xy_,
                   alpha = planes_alpha,
                   lit = planes_lit,
                   shininess = planes_shininess,
                   back = "fill")
    }
    if ("XZ" %in% show_planes) {
      rgl::quads3d(x = rep(xlim, each = 2),
                   y = c(origin[2], origin[2], origin[2], origin[2]),
                   z = c(zlim[1], zlim[2], zlim[2], zlim[1]),
                   color = plane_color_xz_,
                   texture = plane_texture_xz_,
                   alpha = planes_alpha,
                   lit = planes_lit,
                   shininess = planes_shininess,
                   back = "fill")
    }
    if ("YZ" %in% show_planes) {
      rgl::quads3d(y = rep(ylim, each = 2),
                   x = c(origin[1], origin[1], origin[1], origin[1]),
                   z = c(zlim[1], zlim[2], zlim[2], zlim[1]),
                   color = plane_color_yz_,
                   texture = plane_texture_yz_,
                   alpha = planes_alpha,
                   lit = planes_lit,
                   shininess = planes_shininess,
                   back = "fill")
    }
  }

  # Add bounding box decoration
  if (show_bbox) {
    rgl::bbox3d(color = c(bbox_color[1], bbox_color[2]),
                alpha = bbox_alpha,
                emission = bbox_color[1],
                specular = bbox_color[1],
                shininess = bbox_shininess,
                xat = bbox_xat,
                xlab = bbox_xlab,
                xunit = bbox_xunit,
                xlen = bbox_xlen,
                yat = bbox_yat,
                ylab = bbox_ylab,
                yunit = bbox_yunit,
                ylen = bbox_ylen,
                zat = bbox_zat,
                zlab = bbox_zlab,
                zunit = bbox_zunit,
                zlen = bbox_zlen,
                marklen = bbox_marklen,
                marklen_rel = bbox_marklen_rel,
                expand = bbox_expand,
                draw_front = bbox_draw_front)
    # xlen, ylen, zlen: values specifying the number of tickmarks on x, y and
    # Z axes, respectively
    # marklen: value specifying the length of the tickmarks
    # .: other rgl material properties (see ?rgl.material) including:
    #   color: a vector of colors. The first color is used for the background color
    #   of the bounding box. The second color is used for the tick mark labels.
    # emission, specular, shininess: properties for lighting calculation
    # alpha: value specifying the color transparency. The value should be between
    # 0.0 (fully transparent) and 1.0 (opaque)
  }
}
# Description of the used RGL functions:
## The function rgl.texts(x, y, z, text ) is used to add texts to an RGL plot.
## rgl.quads(x, y, z) is used to add planes. x, y and z are numeric vectors of
## length four specifying the coordinates of the four nodes of the quad.

#' Calculate aspect
#'
#' Calculate the aspect or the relative scale of data in three dimensions.
#'
#' @param x,y,z numeric vectors representing point coordinates.
#'
#' @return Returns a vector with three numerical values equal or greater than 1.
#'
#' @examples
#'
#' \dontrun{
#'
#' calculate_aspect(0:100, 0:50, 0:10)
#'
#' }
#'
calculate_aspect <- function(x, y, z) {

  range_x <- max(x) - min(x)
  range_y <- max(y) - min(y)
  range_z <- max(z) - min(z)
  ranges <- c(range_x, range_y, range_z)

  aspect <- c(1, 1, 1)

  if (range_x == min(ranges)) {

    aspect[2] <- range_y / range_x
    aspect[3] <- range_z / range_x

  } else if (range_y == min(ranges)) {

    aspect[1] <- range_x / range_y
    aspect[3] <- range_z / range_y

  } else {

    aspect[1] <- range_x / range_z
    aspect[2] <- range_y / range_z

  }

  return(aspect)
}
