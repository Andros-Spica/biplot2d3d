% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rgl_group_representation.R
\name{star_3d}
\alias{star_3d}
\title{Draw star}
\usage{
star_3d(x, y, z, color = "black", label = "", centroid_color = NULL,
  centroid_radius = 0.05, centroid_alpha = 0.5, link_color = NULL,
  link_width = 1, link_alpha = 1, label_color = NULL, label_cex = 2,
  label_family = "sans", label_font = 2, label_adj = c(-0.25, 0.5),
  label_alpha = 1)
}
\arguments{
\item{x, y, z}{Numeric vectors representing point coordinates.}

\item{color}{The default color to be used in all elements.}

\item{label}{The label placed at the centroid of the distribution.}

\item{centroid_color}{The color of centroid point of the distribution.}

\item{centroid_radius}{Numeric, the radius of the sphere used to
represent the centroid of the distribution (\code{\link[rgl]{spheres3d}}).}

\item{centroid_alpha}{Numeric, the centroids alpha
(\code{\link[rgl]{rgl.material}}).}

\item{link_color}{The color of the links connecting centroid and points.}

\item{link_width, link_alpha}{Numeric, the link width and alpha parameters
(\code{\link[rgl]{segments3d}}, \code{\link[rgl]{rgl.material}}).}

\item{label_color}{The color of the centroid label.}

\item{label_cex, label_family, label_font, label_adj, label_alpha}{The text
parameters and the alpha of the centroid label (\code{\link[rgl]{text3d}},
\code{\link[rgl]{rgl.material}}).}
}
\description{
Compute and draw a star (centroid with links) in a rgl device.
}
\examples{

\dontrun{

# Use iris data
data("iris")

# get setosa
setosa <- iris[iris$Species == "setosa",]

# Initializes the rgl device
rgl_init(zoom = 0.8)

# add axes and bounding box
rgl_format(setosa$Sepal.Length, setosa$Sepal.Width, setosa$Petal.Length,
           axes_titles = c("Sepal length", "Sepal width", "Petal length"),
           show_planes = c("XZ", "XY", "YZ"))

# Add data points
points3d(setosa[, 1], setosa[, 2], setosa[, 3], color = "black")

# Add star
star_3d(setosa[, 1], setosa[, 2], setosa[, 3], label = "setosa",
       centroid_color = "green", link_color = "red", label_color = "blue")

remove(setosa)

}

}
\seealso{
\code{\link{stars_3d}}, \code{\link{rgl_init}},
   \code{\link[rgl]{spheres3d}}, \code{\link[rgl]{segments3d}},
   \code{\link[rgl]{texts3d}}
}
