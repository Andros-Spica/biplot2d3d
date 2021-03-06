% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rgl_device_setup.R
\name{rgl_format}
\alias{rgl_format}
\title{Set up a rgl device}
\usage{
rgl_format(x, y, z, aspect = c(1, 1, 1), symmetric_axes = FALSE,
  show_axes = c("X", "Y", "Z"), show_planes = NULL, show_bbox = FALSE,
  adapt_axes_origin = TRUE, axes_colors = "darkgrey", axes_head_size = 3,
  axes_titles = "", axes_titles_cex = 2, axes_titles_font = 2,
  axes_titles_family = "sans", axes_titles_adj = list(x = c(-0.1, 0), y =
  c(0.5, -0.5), z = c(1.1, 0)), axes_titles_alpha = 1,
  planes_colors = c("lightgrey", "lightgrey", "lightgrey"),
  planes_textures = NULL, planes_alpha = 0.5, planes_lit = TRUE,
  planes_shininess = 50, bbox_color = c("#333377", "black"),
  bbox_alpha = 0.5, bbox_shininess = 5, bbox_xat = NULL,
  bbox_xlab = NULL, bbox_xunit = 0, bbox_xlen = 3, bbox_yat = NULL,
  bbox_ylab = NULL, bbox_yunit = 0, bbox_ylen = 3, bbox_zat = NULL,
  bbox_zlab = NULL, bbox_zunit = 0, bbox_zlen = 3, bbox_marklen = 15,
  bbox_marklen_rel = TRUE, bbox_expand = 1, bbox_draw_front = FALSE)
}
\arguments{
\item{x, y, z}{The numeric vectors corresponding to the 3D coordinates of
points.}

\item{aspect}{A vector of length three indicating the relative scale of each
dimension, to be past to \code{\link[rgl]{aspect3d}}.
By default, aspect is balanced (i.e. c(1, 1, 1)).
If equals NULL, the "true" aspect is calculated
with \code{\link{calculate_aspect}}.}

\item{symmetric_axes}{Logical, whether the axes should
be drawn symmetrically.}

\item{show_axes}{Character vector, indicating which axes to draw:
"X","Y","Z". NULL draws no axes.}

\item{show_planes}{Character vector, indicating which planes to draw:
"XZ","XY","YZ". NULL draws no plane.}

\item{show_bbox}{Logical, indicating whether to draw a bounding box.}

\item{adapt_axes_origin}{Logical, whether to adapt the axes origin.}

\item{axes_colors}{The axes colors.}

\item{axes_head_size}{The size of the head (end point) of the axes.}

\item{axes_titles}{The axes titles.}

\item{axes_titles_cex, axes_titles_font, axes_titles_family, axes_titles_adj, axes_titles_alpha}{The text parameters and the alpha of the titles
(\code{\link[rgl]{text3d}}). \code{axes_title_adj} accepts a single
numeric value (horizontal), a numeric vector of length two
(horizontal, vertical) or a list of length three (with elements named
x, y and z) containing the adj values for the specific axes.}

\item{planes_colors, planes_textures}{The colors and textures to be used
in the planes. A vector can be given, following the order "XY","XZ"
and "YZ". At least one color must be given.}

\item{planes_alpha, planes_lit, planes_shininess}{The graphical parameters of
the planes (\code{\link[rgl]{rgl.material}}).}

\item{bbox_color, bbox_alpha, bbox_shininess, bbox_xat, bbox_yat, bbox_zat, bbox_xlab, bbox_ylab, bbox_zlab, bbox_xunit, bbox_yunit, bbox_zunit, bbox_xlen, bbox_ylen, bbox_zlen, bbox_marklen, bbox_marklen_rel, bbox_expand, bbox_draw_front}{arguments
passed to create a bounding box (\code{\link[rgl]{bbox3d}}).}
}
\description{
\code{rgl_format} set up the elements of a rgl device (axes, aspect, bounding
   box, projection planes) according to the space of the input data.
}
\note{
This function is based on the tutorial "A complete guide to 3D
  visualization device system in R - R software and data visualization"
  available in sthda.com Web site and last accessed in may 24 2016
  (\url{http://www.sthda.com/english/wiki/a-complete-guide-to-3d-visualization-device-system-in-r-r-software-and-data-visualization})
}
\examples{

\dontrun{

# Use iris data
data("iris")

# Initializes the rgl device
rgl_init(theta = 60)

# add axes and bounding box
rgl_format(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length,
           aspect = c(1, 1, 1),
           axes_titles = c("Sepal length", "Sepal width", "Petal length"),
           show_planes = c("XY", "XZ", "YZ"))

# Add data points
points3d(iris[iris$Species == "setosa", 1],
         iris[iris$Species == "setosa", 2],
         iris[iris$Species == "setosa", 3], color = "green")
points3d(iris[iris$Species == "versicolor", 1],
         iris[iris$Species == "versicolor", 2],
         iris[iris$Species == "versicolor", 3], color = "red")
points3d(iris[iris$Species == "virginica", 1],
         iris[iris$Species == "virginica", 2],
         iris[iris$Species == "virginica", 3], color = "blue")
}

}
\seealso{
\code{\link{calculate_aspect}}, \code{\link{rgl_init}},
   \code{\link[rgl]{aspect3d}}, \code{\link[rgl]{axis3d}},
   \code{\link[rgl]{bbox3d}}
}
