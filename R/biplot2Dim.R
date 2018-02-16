
#' 2D biplot
#'
#' Generates a 2D biplot representing the default
#' points/scores and loadings of an ordination object,
#' such as a PCA produced by \code{\link[stats]{princomp}}.
#'
#' @param ordination_object A R object containing a
#'    direct and named reference to default ordination
#'    outputs (i.e. \code{ordination_object$scores} or
#'    \code{ordination_object$points},
#'    \code{ordination_object$loadings} and
#'    \code{ordination_object$sdev}) available for at
#'    least 2 dimensions. Alternatively, a data frame
#'    or matrix with at least two columns is accepted
#'    provided that \code{ordination_method = NULL}),
#'    which creates a two-dimensional scatter plot.
#' @param ordination_method Character, the ordination
#'    method that was used to generate the ordination
#'    object: "PCA" for Principal Components Analysis
#'    (default), "PCoA" for Principal Coordinates Analysis,
#'    "NMDS" for Non-metric Multidimensional Scaling, and
#'    "LDA" for Linear Discriminant Analysis.
#'
#' @param biplot_type Character, indicating the type of
#'    biplot transformation of data: "default" and
#'    "pc.biplot", corresponding to the transformations
#'    performed in \code{\link[stats]{biplot.princomp}}
#'    with \code{pc.biplot = FALSE} ("default") and
#'    \code{pc.biplot = TRUE} ("pc.biplot"). If NULL,
#'    no processing is performed, assuming that data
#'    within \code{ordination_object} was previously transformed.
#' @param rows_over_columns Numeric, the value defining the
#'    degree in which distances between observations have
#'    priority over distances between variables
#'    (0 = observation-focused, 1 = variable-focused).
#'    It corresponds to the argument \code{scale} in
#'    \code{\link[stats]{biplot.princomp}}. It will be
#'    ignored if \code{biplot_type = NULL}. The default is
#'    set at 0.5, which corresponds to a 'SQRT-Biplot',
#'    a compromise between the two representations.
#'
#' @param groups A factor variable containing the group
#'    assignation of each point.
#' @param vips A list of logical (Boolean) vectors identifying
#'    "Very Important Points" under different methods or criteria.
#'
#' @param detach_arrows Logical, wheter to display covariance arrows
#'    a independent miniature plot, overlapping the main plot and
#'    placed according to \code{arrow_fig}.
#' @param show_axes Logical, wheter to display the axes.
#' @param show_grid Logical, wheter to display the background grid.
#' @param show_group_legend Logical, whether to display
#'    a legend for groups.
#' @param show_vip_legend Logical, whether to display a
#'    legend for vip criteria.
#' @param show_arrows Logical, whether to show variable
#'    covariance arrows.
#' @param show_fitAnalysis Logical, whether to display the
#'    fit analysis plot corresponding to the ordination method
#'    given (Scree plot or Shepard plot).
#'
#' @param main_lwd The line width to be used in the main plot.
#'    (\code{lwd} in (\code{\link[graphics]{par}}).
#' @param grid_cex,grid_font,grid_adj The scale, font type,
#'    and text justification of the grid notation.
#' @param invert_coordinates Logical, vector of length two
#'    expressing which dimensions, if any, must be inverted
#'    before plotting (e.g. for aesthetical reasons).
#' @param xlim,ylim the ranges to be encompassed by the x and
#'    y axes, if NULL they are computed
#'    (See  \code{\link[ade4]{s.class}}).
#' @param x_title,y_title Character, texts to be placed in
#'    the x and y axes.
#' @param x_title_cex,x_title_font The text parameters
#'    of the x axis or horizontal title (\code{\link[graphics]{par}}).
#' @param y_title_cex,y_title_font The text parameters
#'    of the y axis or vertical title (\code{\link[graphics]{par}}).
#' @param subtitle Character, a subtitle to be displayed
#'    in the bottom-left corner of the plot
#'    (\code{csub} in \code{\link[ade4]{s.class}}).
#'    If equals NULL and \code{ordination_method = "PCA"},
#'    a default subtitle is the R-Squared of the 2D fit
#'    respect to the original distances.
#' @param subtitle_position Character, value indicating the
#'    position of the subtitle in the main plot ("topleft",
#'    "topright", "bottomleft", and "bottomright"; see
#'    \code{\link[ade4]{s.class}}).
#' @param subtitle_cex the font size of the
#'    subtitle (\code{csub} in
#'    \code{\link[ade4]{s.class}}).
#'
#' @param point_type A character input accepting three
#'    values: "point", "label" (displaying the content
#'    of \code{point_label}) and "point and label", placing
#'    both points and labels.
#' @param point_pch A number or a numerical vector given
#'    to \code{pch} in \code{\link[graphics]{par}}.
#' @param point_size The size or scale given to \code{cpoint} in
#'    \code{\link[ade4]{s.class}}.
#' @param point_label A character vector labelling every
#'    observation. It's length must be equal to the number
#'    of rows in the points/scores of the ordination object
#'    (\code{nrow(ordination_object$points) ==
#'    length(point_label)}).
#' @param point_label_cex,point_label_font,point_label_adj
#'    The text parameters of the points' labels
#'    (\code{\link[graphics]{text}}).
#' @param point_label_adj_override A data frame with x,y
#'    values to be passed to \code{adj}, overrinding the
#'    default value given in \code{point_label_adj}.
#'    The rows must be named exactly as points
#'    are referred in the ordination object.
#'
#' @param arrow_color The color or colors to be used in
#'    covariance arrows.
#' @param arrow_mim_dist Numeric, the minimum distance of a arrow
#'    from the origin of arrows (i.e. zero covariance),
#'    in order for it to be displayed (range [0 = all arrows
#'    are displayed,1 = no arrow is displayed]).
#' @param arrow_length Numeric, the scalar factor applied to loadings
#'    to resize them respect to scores. If \code{detach_arrows = TRUE},
#'    resizing is controlled with \code{arrow_fig}, so  this value is ignored.
#' @param arrow_cex,arrow_lwd,arrow_label_cex,arrow_label_color,arrow_label_font,arrow_label_adj
#'    Graphical parameters of the covariance arrows and their
#'    labels (see \code{\link[graphics]{arrows}} and
#'    \code{\link[graphics]{par}}). \code{arrow_cex} is actually
#'    equivalent to \code{length} in \code{\link[graphics]{arrows}}.
#' @param arrow_label_adj_override A data frame with x,y values
#'    to be passed to \code{adj}, overrinding the default
#'    value given in \code{arrow_label_adj}. The rows must
#'    be named exactly as variables are referred in the
#'    ordination object.
#'
#' @param group_color A vector containing the colors to
#'    be used in each group (applied to points, labels,
#'    stars and ellipses). If NULL, automatically assign
#'    different colors from the \code{rainbow()} palette.
#' @param group_star_cex,group_ellipse_cex,group_label_cex The
#'    size or scale of the stars, ellipses, and labels
#'    representing groups passed to \code{\link[ade4]{s.class}}
#'    of the \code{ade4} package. Zero values render these
#'    elements invisible. NOTE: These are inertia ellipses, which,
#'    in words of ade4's co-author Thibaut Jombart,
#'    "are graphical summaries of a cloud of points." They will
#'    be confidence ellipses if points were sampled from a
#'    normal distribution for all variables concerned. In this case,
#'    group_ellipse_cex controls the degree of confidence (e.g.,
#'    1.5 corresponds to 67% and 2.5 to 95%).
#' @param group_ellipse_axes Logical, wheter to show
#'    the ellipses axes, passed to \code{axesell} in
#'    \code{\link[ade4]{s.class}} of the \code{ade4} package.
#' @param group_legend_title Character, the title of the
#'    group legend. If equal NULL or "", no title is displayed.
#' @param group_legend_title_pos A numeric vector of length
#'    two, the xy position of the title within the group legend
#'    box.
#' @param group_legend_title_cex,group_legend_title_font,group_legend_title_adj
#'    The text parameters to be applied in the group legend
#'    title (\code{\link[graphics]{par}}).
#' @param group_legend_box_color The background color of
#'    the group legend box.
#' @param group_legend_key_pch,group_legend_key_cex,group_legend_key_lwd
#'    The type, size and line width of the keys in the
#'    group legend.
#' @param group_legend_key_margin The x position of the keys
#'    within the group legend box. Values from 0 to 1.
#' @param group_legend_text_color The color or colors of
#'    the text entries in the group legend.
#' @param group_legend_text_cex,group_legend_text_font,group_legend_text_adj
#'    The text parameters of the text entries in the group legend.
#' @param group_legend_text_margin The x position of the
#'    text entries in the group legend. Values from 0 to 1.
#'
#' @param vip_color The color or colors to be used in vip markings.
#' @param vip_pch A character vector containing the
#'    characters used for the vips markings under
#'    each criterion.
#' @param vip_cex,vip_lwd,vip_font,vip_adj
#'    The graphical parameters of the vips markings.
#' @param vip_legend_title Character, the title of the vips
#'    legend.
#' @param vip_legend_title_pos A numeric vector of length
#'    two, the xy position of the title within the vips
#'    legend box.
#' @param vip_legend_title_cex,vip_legend_title_font,vip_legend_title_adj
#'    The text parameters to be applied in the vips legend
#'    title (\code{\link[graphics]{par}}).
#' @param vip_legend_box_color The background color of the
#'    vips legend box.
#' @param vip_legend_key_cex Numeric, the sizing factor of the keys
#'    in the vips legend respect to the vips marking
#'    in the plot.
#' @param vip_legend_key_margin Numeric, the x position of the keys
#'    within the vips legend box. Values from 0 to 1.
#' @param vip_legend_text_cex,vip_legend_text_font,vip_legend_text_adj The text parameters of the text entries in the vips legend.
#' @param vip_legend_text_margin Numeric, the x position of the
#'    text entries in the vips legend box. Values from 0 to 1.
#'
#' @param fitAnalysis_lwd,fitAnalysis_screePlot_color,fitAnalysis_stress_cex,fitAnalysis_stress_lab_cex,fitAnalysis_stress_axis_cex,fitAnalysis_stress_p_color,fitAnalysis_stress_l_color
#'    The graphical parameters of the plot for fit analysis
#'    correspondint to the ordination method (Scree plot for PCA, PCoA, LDA; Shepard or Stress plot for NMDS).
#'    (\code{\link[graphics]{par}},
#'    \code{\link[vegan]{stressplot}}
#'    of the \code{vegan} package).
#'
#' @param test_text A list of character vectors or expressions with the
#'    lines of text presenting the results of statistical tests. A example
#'    structure would be: \code{list(c("first line", "second line"), "second paragraph"), bquote("third paragraph" ~ alpha == 2)}.
#' @param test_spacing_paragraph,test_spacing_line Numeric, relative spacing
#'    between paragraphs (list elements) and lines (character elements
#'    within a list element, if more than one).
#' @param test_cex,test_font,test_adj The parameters of
#'    the text with the test results
#'    (\code{\link[graphics]{par}}).
#'
#' @param fit_into_main Logical, wheter to fit all elements
#'    into the main plot. If TRUE, the 'fig' parameter of
#'    every element is interpreted as relative to \code{main_fig}.
#' @param main_fig,group_legend_fig,vip_legend_fig,arrow_fig,fitAnalysis_fig,test_fig,x_title_fig,y_title_fig The
#'    \code{fig} parameter (\code{\link[graphics]{par}})
#'    to place in the display region of the graphics device,
#'    respectively, the main plot, the group and vip legends,
#'    the fit analysis plot, the tests results, and the x
#'    and y axes titles.
#'
#' @param output_type A character vector indicating the
#'    output image types to be generated. Values accepted
#'    are: "png", "tiff", "jpeg", "eps", and "preview"
#'    (i.e. R graphics device).
#' @param open_new_device Logical, wheter to build the plot in
#'    a new graphics device. If FALSE, the biplot is
#'    draw in the current device.
#' @param leave_device_open Logical, wheter to leave the
#'    graphics device open, e.g. to continue the plot
#'    adding external elements. TRUE is only accepted if
#'    \code{output_type} has a single value. If
#'    \code{output_type = c("preview")}, the device is
#'    left open by default.
#' @param directory Character, the directory within the
#'    working directory. For example, "MyFolder/".
#' @param file_name Character, the name of the output file.
#' @param width,height Numeric, the dimensions of the output image. For 'eps' files, dimensions are scaled down by 100 (e.g., X / 100).
#' @param family Character, the font family used in every text
#'    in the plot, (\code{\link[graphics]{par}}).
#'
#' @details This function allows customising virtually every
#' graphical parameter in a 2D biplot, including several extra
#' elements that may be useful for multivariate explorations.
#' It is focused mainly on improving basic visualization aspects
#' of ordination methods through 'classical' biplots. There
#' are several packages that address the creation of other
#' variations of biplot: BiplotGUI, GGEBiplotGUI, multibiplotGUI,
#' biplotbootGUI, NominalLogisticBiplot, OrdinalLogisticBiplot,
#' ade4, vegan, MultBiplotR.
#' When \code{biplot_type = "default"}, the biplot processing
#' is done as in \code{\link[stats]{biplot.princomp}}, which
#' follows the definition of Gabriel (1971). As in this method,
#' when \code{biplot_type = "pc.biplot"}, this function creates
#' biplots according with Gabriel and Odoroff (1990). Since there
#' are several types of biplot transformations,
#' it is possible to use 'scores' and 'loadings' that were
#' already transformed, passing \code{biplot_type = NULL}.
#' Groups can be represented as stars, ellipsoids, and/or colors,
#' using  (\code{\link[ade4]{s.class}}), which can be tracked by a
#' fully-customisable legend (\code{group_legend} arguments).
#' Individual observations deemed exceptional (vip = Very
#' Important Points) can be marked with custom symbols. Whenever
#' is are more than one type of marking (e.g. different
#' methods/criteria of outlier detection), the different symbols
#' can be presented in a legend (\code{vip_legend} arguments).
#' When desired, it is possible to display a Scree plot
#' representing the eigenvalues (Principal Components Analysis,
#' Principal Coordinates Analysis) or a Shepard or stress plot
#' (Nonmetric Multidimensional Scaling,
#' e.g. \code{\link[vegan]{metaMDS}} in the vegan package) by
#' enabling \code{show_fit_analysis}. It is also possible to
#' display statistical test results by enabling \code{show.tests}
#' and introducing the corresponding lines in \code{test_text}.
#' The position of all elements (legend boxes, title and
#' subtitle, fit analysis plot and tests) can be customized
#' using the corresponding \code{fig} parameter.
#'
#' @examples
#'
#' \dontrun{
#'
#' # Use iris data
#' data("iris")
#'
#' # get an ordination object
#' # ("PCA" is the default input of this function)
#' pca <- princomp(iris[, 1:4])
#'
#' # Default plot using Species as the group factor
#' biplot_2d(pca, groups = iris$Species)
#'
#' # Use the typical visualization,
#' # placing scores and loadings around the same origin
#' biplot_2d(pca, groups = iris$Species, detach_arrows = FALSE)
#'
#' # Compare different versions of the classical biplot
#' # "default" vs. "pc.biplot"
#' biplot_2d(pca,
#'           output_type = "preview",
#'           leave_device_open = TRUE,
#'           x_title = 'biplot_type = "default"',
#'           x_title_fig = c(0, 1, 0.9, 1),
#'           fit_into_main = TRUE,
#'           main_fig = c(0, 0.499, 0, 1))
#' biplot_2d(pca,
#'           output_type = "preview",
#'           open_new_device = FALSE,
#'           biplot_type = "pc.biplot",
#'           x_title = 'biplot_type = "pc.biplot"',
#'           x_title_fig = c(0, 1, 0.9, 1),
#'           fit_into_main = TRUE,
#'           main_fig = c(0.5099, 1, 0, 1))
#'
#' # varying focus on representing distances
#' # between observations or between variables
#' biplot_2d(pca,
#' output_type = "png",
#' leave_device_open = TRUE,
#' rows_over_columns = 1,
#' x_title = 'observation-focused\nrows_over_columns = 1',
#' x_title_fig = c(0, 1, 0.9, 1),
#' fit_into_main = TRUE,
#' main_fig = c(0, 0.329, 0, 1),
#' width = 1200)
#' biplot_2d(pca,
#'           open_new_device = FALSE,
#'           leave_device_open = TRUE,
#'           rows_over_columns = 0.5,
#'           x_title = 'compromise\nrows_over_columns = 0.5',
#'           x_title_fig = c(0, 1, 0.9, 1),
#'           fit_into_main = TRUE,
#'           main_fig = c(0.331, 0.659, 0, 1))
#' biplot_2d(pca,
#'           open_new_device = FALSE,
#'           rows_over_columns = 0,
#'           biplot_type = "pc.biplot",
#'           x_title = 'variable-focused\nrows_over_columns = 0',
#'           x_title_fig = c(0, 1, 0.9, 1),
#'           fit_into_main = TRUE,
#'           main_fig = c(0.661, 1, 0, 1))
#'
#' # ---------------------------------------------------------
#' # Plot groups as different colors and point types (pch),
#' # make group star, ellipsis, and label invisible and
#' # add a group legend with a a custom title.
#' biplot_2d(pca,
#'           groups = iris$Species,
#'           group_color = NULL,
#'           point_pch = c(1, 3, 2),
#'           group_star_cex = 0,
#'           group_ellipse_cex = 0,
#'           group_label_cex = 0,
#'           show_group_legend = T,
#'           group_legend_title = "Species")
#'
#' # ---------------------------------------------------------
#' # Polish covariance arrows
#' # Abbreviate variables names
#' dimnames(pca$loadings)[[1]] <- c("SL", "SW", "PL", "PW")
#' # Set a specific justification (adj) for each variable label
#' arrow_label_adj_override <- rbind(c(-0.1, 0),
#'                                   c(-0.1, 0.5),
#'                                   c(0.5, 1.3),
#'                                   c(0.5, 1.3))
#' row.names(arrow_label_adj_override) <-
#'    dimnames(pca$loadings)[[1]]
#' # Plot: arrows with different colors and
#' # without the background grid
#' biplot_2d(pca,
#'           groups = iris$Species,
#'           point_pch = c(1, 3, 2),
#'           group_star_cex = 0,
#'           group_ellipse_cex = 0,
#'           group_label_cex = 0,
#'           show_group_legend = T,
#'           group_legend_title = "Species",
#'           arrow_color = c("orange",
#'                           "blue",
#'                           "red",
#'                           "green"),
#'           arrow_label_adj_override = arrow_label_adj_override,
#'           show_grid = FALSE)
#'
#' # ---------------------------------------------------------
#' # Get arbitrary Very Important Points
#' irisVIP <- list(setosa = (1:nrow(iris) == 16 |
#'                           1:nrow(iris) == 42),
#'                 versicolor=(1:nrow(iris) == 61),
#'                 virginica=(1:nrow(iris) == 107 |
#'                            1:nrow(iris) == 118 |
#'                            1:nrow(iris) == 132))
#'
#' # Plot observations using their names and group by Species using only color.
#' # Mark the VIP and add the respective legend with custom characters.
#' biplot_2d(pca,
#'           groups = iris$Species,
#'           point_type = "label",
#'           point_label = row.names(iris),
#'           group_color = c("red", "blue", "green"),
#'           group_star_cex = 0,
#'           group_ellipse_cex = 0,
#'           group_label_cex = 0,
#'           show_group_legend = TRUE,
#'           group_legend_title = "",
#'           vips = irisVIP,
#'           vip_pch = c("X", "O", "+"),
#'           vip_cex = c(2, 2, 3),
#'           vip_legend_fig = c(0.01, 0.25, 0.7, 0.99),
#'           show_axes = FALSE)
#'
#' # ---------------------------------------------------------
#' # Test the setosa separation
#' irisDist <- dist(iris[, 1:4])
#' setosaSeparation <- iris$Species == "setosa"
#' ## multivariate test for the setosa separation
#' require(vegan)
#' irisTests <- NULL
#' irisTests$permanova <- adonis(irisDist~setosaSeparation)
#' irisTests$permdisp2 <- permutest(betadisper(irisDist,setosaSeparation),
#'                                  pairwise = TRUE)
#' # This function prepares a list of character vectors containing the test results
#' getTestText <- function(tests){
#'   permanova_F <- as.character(round(tests$permanova$aov.tab$F.Model[1], 3))
#'   permanova_pvalue <- as.character(round(tests$permanova$aov.tab$"Pr(>F)"[1], 3))
#'   permanova_rSquared <- as.character(round(tests$permanova$aov.tab$R2[1], 3))
#'   permdisp2_F <- as.character(round(tests$permdisp2$tab$F[1], 3))
#'   permdisp2_pvalue <- as.character(round(tests$permdisp2$tab$"Pr(>F)"[1], 3))
#'   text <- list(c(paste("PERMANOVA:\n   F = ", permanova_F,
#'                        " (p = ", permanova_pvalue, ")\n", sep = ""),
#'                  c(expression(paste("   ", R^2, " =", sep = "")),
#'                    paste("          ", permanova_rSquared, sep = ""))),
#'                paste("PERMDISP2:\n   F = ", permdisp2_F,
#'                      " (p = ", permdisp2_pvalue,")", sep = ""))
#'   return(text)
#' }
#'
#' # Plot observations using points and groups as colored stars with no labels.
#' # Place tests results in the top left corner and give a custom horizontal title.
#' biplot_2d(pca,
#'           groups = iris$Species,
#'           group_color = NULL,
#'           group_ellipse_cex = 0,
#'           group_label_cex = 0,
#'           show_group_legend = TRUE,
#'           group_legend_title = "",
#'           test_text = resultText(irisTests),
#'           test_fig = c(0.01,0.5,0.6,0.95),
#'           show_axes = FALSE,
#'           x_title = "testing setosa separation",
#'           x_title_cex = 1.5)
#'
#'}
#'
#' @export biplot_2d
#'
biplot_2d <-
  function(ordination_object,
           ordination_method = "PCA",
           biplot_type = "default",
           rows_over_columns = 0.5,
           groups = NULL,
           vips = NULL,

           detach_arrows = TRUE,
           show_grid = TRUE,
           show_axes = TRUE,
           show_group_legend = FALSE,
           show_vip_legend = TRUE,
           show_arrows = TRUE,
           show_fitAnalysis = TRUE,

           main_lwd = 2,
           grid_cex = 1,
           grid_font = 3,
           grid_adj = 0.5,
           invert_coordinates = c(FALSE, FALSE),
           xlim = NULL,
           ylim = NULL,
           x_title = "",
           y_title = "",
           x_title_cex = 1,
           x_title_font = 2,
           y_title_cex = 1,
           y_title_font = 2,
           subtitle = NULL,
           subtitle_position = "bottomleft",
           subtitle_cex = 1.2,

           point_type = "point",
           point_pch = 1,
           point_size = 1,
           point_label = NULL,
           point_label_cex = 1,
           point_label_font = 3,
           point_label_adj = c(0.5, 0.5),
           point_label_adj_override = NULL,

           arrow_color = "darkorange",
           arrow_mim_dist = 0,
           arrow_length = 0.2,
           arrow_cex = 0.1,
           arrow_lwd = 2,
           arrow_label_cex = 1,
           arrow_label_color = "black",
           arrow_label_font = 1,
           arrow_label_adj = c(0.5, 0.5),
           arrow_label_adj_override = NULL,

           group_color = "black",
           group_star_cex = 1,
           group_ellipse_cex = 2.5,
           group_ellipse_axes = FALSE,
           group_label_cex = 1,
           group_legend_title = "groups",
           group_legend_title_pos = c(0.5, 0.85),
           group_legend_title_cex = 1,
           group_legend_title_font = 3,
           group_legend_title_adj = 0.5,
           group_legend_box_color = "white",
           group_legend_key_pch = 15,
           group_legend_key_cex = 1,
           group_legend_key_lwd = 1,
           group_legend_key_margin = 0.15,
           group_legend_text_margin = 0.25,
           group_legend_text_color = "black",
           group_legend_text_cex = 1,
           group_legend_text_font = 1,
           group_legend_text_adj = 0,

           vip_color = "black",
           vip_pch = c(0, 1, 5, 2, 6, 4, 3),
           vip_cex = c(5, 5, 5, 5, 5, 3, 3),
           vip_lwd = 5,
           vip_font = 1,
           vip_adj = c(0.5, 0.5),
           vip_legend_title = "VIPs",
           vip_legend_title_pos = c(0.5, 0.85),
           vip_legend_title_cex = 1,
           vip_legend_title_font = 3,
           vip_legend_title_adj = 0.5,
           vip_legend_box_color = "white",
           vip_legend_key_cex = 0.8,
           vip_legend_key_margin = 0.15,
           vip_legend_text_cex = 1,
           vip_legend_text_font = 1,
           vip_legend_text_adj = 0,
           vip_legend_text_margin = 0.25,

           fitAnalysis_lwd = 3,
           fitAnalysis_screePlot_color = c("grey","white"),
           fitAnalysis_stress_cex = 1,
           fitAnalysis_stress_lab_cex = 1,
           fitAnalysis_stress_axis_cex = 1,
           fitAnalysis_stress_p_color = "darkgrey",
           fitAnalysis_stress_l_color = "black",

           test_text = NULL,
           test_spacing_paragraph = 0.8,
           test_spacing_line = 0.8,
           test_cex = 1,
           test_font = 1,
           test_adj = 0.5,

           fit_into_main = FALSE,
           main_fig =          c(  0,     1,     0,     1),
           group_legend_fig = c( 0.8,  0.99,   0.6,  0.90),
           vip_legend_fig =   c(0.78,  0.99,   0.3,  0.55),
           arrow_fig =        c(0.69,  0.99,  0.01,  0.31),
           fitAnalysis_fig =  c(0.02,  0.35,  0.06,  0.25),
           test_fig =         c(   0,   0.3,   0.8,  0.99),
           x_title_fig =      c(0.25,     1,  0.85,     1),
           y_title_fig =      c(0.91,     1,     0,     1),

           output_type = c("preview","png"),
           open_new_device = TRUE,
           leave_device_open = FALSE,
           directory = "",
           file_name = "2D Biplot",
           width = 400,
           height = 400,
           family = "sans"
           ) {

    # Load data ===================================================
    scores <- NULL
    loadings <- NULL
    sdev <- NULL
    eigenvalues <- NULL

    # Select data depending on the ordination method

    # Not an ordination object ------------------------------------
    # If there is no ordination method,
    # ordination_object is interpreted as a data frame.
    if (is.null(ordination_method)) {

      warning("ordination_method = NULL, so the ordination_object
              is interpreted as a data frame.\nNo covariance arrows
              or fit analysis will be displayed.", call = FALSE)
      scores <- ordination_object
      show_arrows = FALSE
      show_fitAnalysis = FALSE

    } else
    # Principal Components Analysis --------------------------------
    if (ordination_method == "PCA") {

      if (is.null(ordination_object$x)){

        scores <- ordination_object$scores
        loadings <- ordination_object$loadings
        sdev <- ordination_object$sdev

        if (is.null(sdev)){
          sdev <- ordination_object$princompOutputClr$sdev
        }

      } else {

        scores <- ordination_object$x
        loadings <- ordination_object$rotation
        sdev <- ordination_object$sdev

      }

      isPCbiplot <- FALSE

      if (biplot_type == "pc.biplot") isPCbiplot <- TRUE

      lambda <- get_lambda(sdev,
                           n.obs = nrow(scores),
                           dimensions = 2,
                           scale = rows_over_columns,
                           pc.biplot = isPCbiplot)

      if (biplot_type == "default" || biplot_type == "pc.biplot") {

        scores <- t(t(scores[, 1:2])/lambda)
        loadings <- t(t(loadings[, 1:2]) * lambda)

      } else {

        if (!is.null(biplot_type)) {

          stop("The biplot_type given is not supported.
               Choose between 'default' and 'pc.biplot'
               or NULL, if data was previously transformed.",
               call. = FALSE)

        }

      }

      eigenvalues <- (sdev) ^ 2
      names(eigenvalues) <- rep("", length(eigenvalues))

    }
    else
    # Nonmetric Multidimensional Scaling --------------------------
    if (ordination_method == "NMDS") {

      scores <- ordination_object$points
      loadings <- ordination_object$loadings

    }
    else
    # Principal Coordiantes Analysis ------------------------------
    if (ordination_method == "PCoA") {

      scores <- ordination_object$points
      loadings <- ordination_object$loadings
      eigenvalues <- ordination_object$eig

    }
    else
    # Linear Discriminant Analysis --------------------------------
    if (ordination_method == "LDA") {

      scores <- ordination_object$values$x

      if (is.null(ordination_object$loadings)){

        loadings <- ordination_object$scaling

      } else {

        loadings <- ordination_object$loadings

      }

      sdev <- ordination_object$svd
      eigenvalues <- (sdev) ^ 2
      eigenvalues <- (eigenvalues / sum(eigenvalues)) * 100

    } else
    # Method not supported ----------------------------------------
    # If the method given is not supported
    {

      stop(paste("The method '", ordination_method, "' is not
                 supported or not properly written.\nPlease pass
                 either 'PCA', 'PCoA', 'NMDS', or 'LDA' as the
                 ordination_method argument.", sep = ""))

    }

    # If the row names don't exist, fill them
    # with the index numbers
    if (is.null(row.names(scores))) {

      row.names(scores) <- as.character(1:nrow(scores))

    }

    # Set up parameters ===========================================

    # scale 'fig' parameters, if fit_into_main = TRUE
    group_legend_fig_ <- group_legend_fig
    vip_legend_fig_ <- vip_legend_fig
    arrow_fig_ <- arrow_fig
    fitAnalysis_fig_ <- fitAnalysis_fig
    test_fig_ <- test_fig
    x_title_fig_ <- x_title_fig
    y_title_fig_ <- y_title_fig

    if (fit_into_main) {

      group_legend_fig_ <- scale_to_main(group_legend_fig, main_fig)
      vip_legend_fig_ <- scale_to_main(vip_legend_fig, main_fig)
      arrow_fig_ <- scale_to_main(arrow_fig, main_fig)
      fitAnalysis_fig_ <- scale_to_main(fitAnalysis_fig, main_fig)
      test_fig_ <- scale_to_main(test_fig, main_fig)
      x_title_fig_ <- scale_to_main(x_title_fig, main_fig)
      y_title_fig_ <- scale_to_main(y_title_fig, main_fig)

    }

    # Load subtitle -----------------------------------------------

    sub <- subtitle

    if (is.null(subtitle)) {

      # If no subtitle is given
      if (!is.null(ordination_method)) {
        if (ordination_method != "PCA") {

          # If the method is not a PCA,
          # no subtitle is displayed
          # TODO: useful default subtitle for other methos
          sub <- ""

        } else {

          # If the method is a PCA,
          # the percentage of variance represented is displayed
          cumVar <- cumsum((sdev) ^ 2) / sum(sdev ^ 2)
          R2 <- as.character(100 * round(cumVar[2], digits = 4))
          sub <- paste(R2, "% of variance explained", sep = "")

        }
      }
    }

    possub_ <- subtitle_position
    if (possub_ != "topleft" &&
        possub_ != "topright" &&
        possub_ != "bottomleft" &&
        possub_ != "bottomright") {
          stop("Please introduce a valid value into
               subtitle_position ('topleft', 'topright',
               'bottomleft', 'bottomright')",
               call. = FALSE)
        }
    csub_ <- subtitle_cex
    addaxes_ <- show_axes
    grid_ <- show_grid

    # ivert coordinates (aesthetics) ------------------------------

    if (invert_coordinates[1]){

      scores[, 1] <- -scores[, 1]
      loadings[,1] <- -loadings[, 1]

    }
    if (invert_coordinates[2]){

      scores[, 2] <- -scores[, 2]
      loadings[, 2] <- -loadings[, 2]

    }

    # Prepare the parameters for distinguish points by groups -----

    groups_ <- groups
    group_color_ <- group_color
    cstar_ <- group_star_cex
    cellipse_ <- group_ellipse_cex
    axesell_ <- group_ellipse_axes
    clabel_ <- group_label_cex

    if (is.null(groups)) {

      # if there is no factor, do not draw groups.
      groups_ <- factor(rep(1, nrow(scores)))
      cstar_ <- 0
      cellipse_ <- 0
      axesell_ <- FALSE
      clabel_ <- 0

    } else {

      # if group_color is not specified, get diferent colors for each group
      if (is.null(group_color)) {

        group_color_ <- rainbow(nlevels((groups_)))

      } else {

        if (length(group_color) < nlevels(groups_)) {

          if (length(group_color) == 1) {

            group_color_ <- rep(group_color, nlevels(groups_))

          } else {

            stop("length(group_color) < nlevels(groups).
               Please specify colors for all groups or
               choose a single color.",
                 call. = FALSE)

          }
        }
      }
    }

    # Prepare the parameters for generating points ----------------

    # points
    point_pch_ <- point_pch[1]

    if (point_type == "label") {

      # Do not display points
      point_pch_ <- rep("", nrow(scores))

    } else {

      # In case pch is used to differentiate groups
      if (nlevels(groups_) > 1 && length(point_pch) > 1) {

        if (length(point_pch) >= nlevels(groups_)) {

          # initiate new pch vector with the proper length and type
          if (is.numeric(point_pch)) {
            point_pch_ <- rep(1, nrow(scores))
          } else {
            point_pch_ <- rep("", nrow(scores))
          }

          # get the numeric translation of the groups factor
          groups_numeric <- as.numeric(groups_)

          # replace values with point_pch, corresponding to groups
          for (i in 1:length(groups_numeric)) {
            point_pch_[i] <- point_pch[groups_numeric[i]]
          }

        } else {

          warning("length(point_pch) < nlevels(groups).
                    The first value in point_pch is used
                  for points in every group.",
                  call. = FALSE)

        }
      }
    }

    # point labels
    if (point_type == "label" ||
        point_type == "point and label" ||
        !is.null(point_label)) {

      labels_ <- point_label

      # position of labels of points
      point_label_adj_ <-
        cbind(rep(point_label_adj[1], nrow(scores)),
              rep(point_label_adj[2], nrow(scores)))

      if (!is.null(point_label_adj_override)){

        for (i in 1:nrow(point_label_adj_override)){

          point_label_adj_[row.names(scores) == row.names(point_label_adj_override)[i], ] <-
            point_label_adj_override[i, ]

        }
      }

      # if there are no labels given, take the row names
      if (is.null(labels_)) labels_ <- row.names(scores)

    }

    # Prepare the parameters for generating -----------------------
    # variables covariances (arrows) ------------------------------

    if (show_arrows) {

      # filter variables to display
      loadings <- filter_arrows(loadings,
                                min_dist = arrow_mim_dist,
                                dimensions = 2)

      if (nrow(loadings) == 0) {

        stop("arrow_mim_dist is too high (filter is too restrictive).\n
               Please decrease arrow_mim_dist value or hide arrow plot (show_arrow = FALSE).",
             call. = FALSE)

      }

      # position of labels of arrows
      arrow_label_adj_ <-
        cbind(rep(arrow_label_adj[1], nrow(loadings)),
              rep(arrow_label_adj[2], nrow(loadings)))
      if (!is.null(arrow_label_adj_override)){
        for (i in 1:nrow(arrow_label_adj_override)){
          arrow_label_adj_[row.names(loadings) == row.names(arrow_label_adj_override)[i], ] <-
            arrow_label_adj_override[i, ]

        }
      }

      # Get variation range of loadings at the first component
      # The one with most variation
      loadingsRange <- max(loadings[, 1]) - min(loadings[, 1])
      # apply scalling
      loadingsRange <- loadingsRange * arrow_length

      # color of arrows
      arrow_color_ <- arrow_color
      if (length(arrow_color) < nrow(loadings)) {

        if (length(arrow_color) == 1) {

          arrow_color_ <- rep(arrow_color, nrow(loadings))

        } else {

          stop("length(arrow_color) < number of variables.
               Please specify colors for all groups or
               choose a single color.",
               call. = FALSE)

        }
      }

      arrow_label_color_ <- arrow_color
      if (!is.null(arrow_label_color)) {
        arrow_label_color_ <- arrow_label_color
      }

    }

    # Plot ========================================================

    # Initiate graphics device ------------------------------------

    # Loop to generate the same plot in each output type

    for (out in output_type){

      # save current graphic configuration
      current_par <- par(no.readonly = TRUE)

      if (open_new_device) {

        # Set directory + file name
        if (directory == "") {
          fn <- paste(file_name,
                      out,
                      sep = ".")
        } else {
          fn <- paste(directory,
                      paste(file_name,
                            out,
                            sep = "."),
                      sep = "/")
        }

        # Open graphics device
        if (out == "png") {
          grDevices::png(filename = fn,
                         width = width,
                         height = height)
        } else
          if (out == "eps") {
          extrafont::loadfonts(device = "postscript")
          grDevices::postscript(file = fn,
                                pointsize = 10,
                                width = width / 100,
                                height = height / 100,
                                horizontal = FALSE,
                                paper = "special",
                                onefile = FALSE,
                                family = family,
                                colormodel = "cmyk")
        } else
          if (out == "tiff") {
          grDevices::tiff(filename = fn,
                          width = width,
                          height = height)
        } else
          if (out == "jpeg") {
          grDevices::jpeg(filename = fn,
                          width = width,
                          height = height)
        } else
          if (out == "preview") {

        } else {
          stop("The output type is not supported.
               \nThe accepted are: 'png', 'eps', 'tiff', 'jpeg', or 'preview'.",
               call. = FALSE)
        }
      } else {
        if (!is.null(dev.list())) {

          par(new = TRUE)

        } else {

          stop("There is no graphics device currently open,
               so 'open_new_device = FALSE'.",
               call. = FALSE)

        }
      }

      # Set up general background graphic parameters
      par(mar = c(0, 0, 0, 0),
          font = grid_font,
          family = family,
          adj = grid_adj,
          lwd = main_lwd,
          fig = main_fig)

      # Create main plot ------------------------------------------
      ade4::s.class(scores[, c(1, 2)],
                    xax = 1, yax = 2,
                    fac = groups_,
                    col = group_color_,
                    clabel = clabel_,
                    cpoint = point_size,
                    cstar = cstar_,
                    cellipse = cellipse_,
                    axesell = axesell_,
                    sub = sub,
                    pch = point_pch_,
                    possub = possub_,
                    csub = csub_,
                    cgrid = grid_cex,
                    addaxes = addaxes_,
                    grid = grid_,
                    xlim = xlim, ylim = ylim)

      # Mark vips -------------------------------------------------
      # (Very Important Points, e.g. outliers), when any is given,
      # distinguishing the various detection methods

      if (!is.null(vips)) {

        # Create markings following each criterion
        # (column or list element in vips)
        for (i in 1:length(vips)){

          # these overrides allow graphical arguments to be either
          # vectors or single values. Also, if the length of vips
          # is greater than any vector in such graphic arguments,
          # the first value is applied.

          vip_color_ <- vip_color[1]
          if (length(vip_color) >= length(vips)) {
            vip_color_ <- vip_color[i]
          }
          vip_pch_ <- vip_pch[1]
          if (length(vip_pch) >= length(vips)) {
            vip_pch_ <- vip_pch[i]
          }
          vip_cex_ <- vip_cex[1]
          if (length(vip_cex) >= length(vips)) {
            vip_cex_ <- vip_cex[i]
          }
          vip_lwd_ <- vip_lwd[1]
          if (length(vip_lwd) >= length(vips)) {
            vip_lwd_ <- vip_lwd[i]
          }
          vip_font_ <- vip_font[1]
          if (length(vip_font) >= length(vips)) {
            vip_font_ <- vip_font[i]
          }
          vip_adj_ <- vip_adj[1]
          if (!is.null(nrow(vip_adj)) &&
              nrow(vip_adj) >= length(vips)) {
            vip_adj_ <- vip_adj[i, ]
          }

          # Plot vip markings
          points(scores[vips[[i]], 1],
                 scores[vips[[i]], 2],
                 col = vip_color_,
                 pch = vip_pch_,
                 cex = vip_cex_,
                 lwd = vip_lwd_,
                 font = vip_font_,
                 adj = vip_adj_)

        }
      }

      # point labels ----------------------------------------------
      # labels are drawn after the vips are marked,
      # in sake of visibility

      if (point_type == "label" ||
          point_type == "point and label" ||
          !is.null(point_label)) {

        for (i in 1:nrow(scores)) {

          text(x = scores[i, 1],
               y = scores[i, 2],
               labels = labels_[i],
               col = group_color[groups_[i]],
               cex = point_label_cex,
               family = family,
               font = point_label_font,
               adj = point_label_adj_[i, ])

        }
      }

      # insert the miniature of variables covariances (arrows) ----
      if (show_arrows) {

        # copy of loadings to which scalling may be applied
        arrow_x <- loadings[, 1] * arrow_length
        arrow_y <- loadings[, 2] * arrow_length

        if (detach_arrows) {

          # back to original graphics configuration
          par(current_par)

          par(fig = arrow_fig_,
              new = T ,
              mar = c(0.1, 0.1, 0.1, 0.1),
              lwd = arrow_lwd)

          # Initialize plot
          plot(arrow_x,
               arrow_y,
               pch = "",
               axes = FALSE,
               xaxt = 'n',
               yaxt = 'n',
               ann = FALSE,
               xlim = c(min(c(0, min(arrow_x) - 0.05 * loadingsRange)),
                        max(c(0, max(arrow_x) + 0.05 * loadingsRange))),
               ylim = c(min(c(0, min(arrow_y) - 0.05 * loadingsRange)),
                        max(c(0, max(arrow_y) + 0.05 * loadingsRange)))
               )

        } else {

          par(lwd = arrow_lwd)

        }

        # Draw arrows
        arrows(0,
               0,
               arrow_x,
               arrow_y,
               length = arrow_cex,
               col = arrow_color_)

        # Draw arrows labels
        for (i in 1:nrow(loadings)){

          text(arrow_x[i],
               arrow_y[i],
               labels = row.names(loadings)[i],
               col = arrow_label_color_,
               cex = arrow_label_cex,
               font = arrow_label_font,
               adj = arrow_label_adj_[i, ],
               family = family)
        }

        # back to original graphics configuration
        par(current_par)
      }

      # Create the legend for groups ------------------------------
      # (color,pch -> groups)

      if (show_group_legend) {

        firstLine <- 1
        if (is.null(group_legend_title) ||
            group_legend_title == "") {
          firstLine <- 0
        }

        # Create the legend for the factor (color -> groups)
        par(fig = group_legend_fig_,
            new = T,
            mar = c(0, 0, 0, 0))

        plot.new()

        rect(xleft = 0,
             ybottom = 0,
             xright = 1,
             ytop = 1,
             col = group_legend_box_color)

        text(x = group_legend_title_pos[1],
             y = group_legend_title_pos[2],
             labels = group_legend_title,
             cex = group_legend_title_cex,
             font = group_legend_title_font,
             family = family,
             adj = group_legend_title_adj)

        for (i in 1:nlevels(groups_)) {

          pos_y <- 1 - ((firstLine + i) / ((firstLine + 1) + nlevels(groups_)))

          # select the shape of the key
          group_legend_key_pch_ <- group_legend_key_pch
          # In case that groups are differentiated by pch
          if (length(point_pch) > 1) {
            group_legend_key_pch_ <- point_pch[i]
          }

          # select the color of the key text
          group_legend_text_color_ <- group_legend_text_color
          if (!is.null(group_legend_text_color)) {

            if (length(group_legend_text_color) ==
                nlevels(groups_)) {
              group_legend_text_color_ <-
                group_legend_text_color[i]
            }

          } else {

            # If no color is given, the group_color is used
            group_legend_text_color_ <- group_color_[i]

          }

          points(x = group_legend_key_margin,
                 y = pos_y,
                 pch = group_legend_key_pch_,
                 lwd = group_legend_key_lwd,
                 cex = group_legend_key_cex,
                 col = group_color_[i])

          text(x = group_legend_text_margin,
               y = pos_y,
               labels = levels(groups_)[i],
               col = group_legend_text_color_,
               cex = group_legend_text_cex,
               font = group_legend_text_font,
               family = family,
               adj = group_legend_text_adj)

        }

        # back to original graphics configuration
        par(current_par)
      }

      # Create the legend for the vips ----------------------------

      if (show_vip_legend & !is.null(vips)) {

        firstLine <- 1
        if (is.null(vip_legend_title)) firstLine <- 0

        par(fig = vip_legend_fig_,
            new = T,
            mar = c(0, 0, 0, 0))

        plot.new()

        rect(0, 0, 1, 1, col = vip_legend_box_color)

        text(vip_legend_title_pos[1],
             vip_legend_title_pos[2],
             labels = vip_legend_title,
             cex = vip_legend_title_cex,
             font = vip_legend_title_font,
             adj = vip_legend_title_adj,
             family = family)

        for (i in 1:length(vips)){

          # these overrides allow graphical arguments
          # to be either vectors or single values.
          # Also, if the length of vips is greater
          # than any vector in such graphic arguments,
          # the first value is applied.

          vip_pch_ <- vip_pch[1]
          if (length(vip_pch) > 1 &&
              length(vip_pch) >= i) {
            vip_pch_ <- vip_pch[i]
          }
          vip_lwd_ <- vip_lwd[1]
          if (length(vip_lwd) > 1 &&
              length(vip_lwd) >= i) {
            vip_lwd_ <- vip_lwd[i]
          }
          vip_cex_ <- vip_cex[1]
          if (length(vip_cex) > 1 &&
              length(vip_cex) >= i) {
            vip_cex_ <- vip_cex[i]
          }
          vip_color_ <- vip_color[1]
          if (length(vip_color) > 1 &&
              length(vip_color) >= i) {
            vip_color_ <- vip_color[i]
          }

          pos_y = 1 - ((firstLine + i) / ((firstLine + 1) + length(vips)))

          points(vip_legend_key_margin,
                 pos_y,
                 pch = vip_pch_,
                 lwd = vip_lwd_,
                 cex = vip_cex_ * vip_legend_key_cex,
                 col = vip_color_)

          text(vip_legend_text_margin,
               pos_y,
               labels = names(vips)[i],
               cex = vip_legend_text_cex,
               font = vip_legend_text_font,
               family = family,
               adj = vip_legend_text_adj)

        }

        # back to original graphics configuration
        par(current_par)
      }

      # insert the plot corresponding to the fit analysis ---------
      # of the ordination object

      if (show_fitAnalysis) {

        if (ordination_method == "NMDS") {

          # for the NMDS, the stress plot is shown
          par(fig = fitAnalysis_fig_,
              new = T,
              mar = c(1, 0.1, 1, 0.1))

          vegan::stressplot(ordination_object,
                     p.col = fitAnalysis_stress_p_color,
                     cex = fitAnalysis_stress_cex,
                     cex.lab = fitAnalysis_stress_lab_cex,
                     cex.axis = fitAnalysis_stress_axis_cex,
                     lwd = fitAnalysis_lwd,
                     l.col = fitAnalysis_stress_l_color)

        } else {

          # for PCA and PCoA, the Screeplot is shown
          par(fig = fitAnalysis_fig_,
              new = T ,
              mar = c(0.1, 0.1, 1, 0.1),
              lwd = fitAnalysis_lwd)

          barplot(eigenvalues,
                  space = 0,
                  col = fitAnalysis_screePlot_color[2],
                  axes = FALSE,
                  cex.axis = 0)

          # this shades the two first eigenvalues
          barplot(eigenvalues[1:2],
                  space = 0,
                  col = fitAnalysis_screePlot_color[1],
                  axes = FALSE,
                  cex.axis = 0,
                  add = TRUE)

        }

        # back to original graphics configuration
        par(current_par)
      }

      # Display tests results -------------------------------------

      if (!is.null(test_text)) {

        par(fig = test_fig_,
            new = T,
            mar = c(0, 0, 0, 0))

        plot.new()

        for (i in 1:length(test_text)) {

          first_line_pos_y <-
            1 - test_spacing_paragraph * ( (i - .9) / length(test_text) )

          pos_y <- first_line_pos_y

          if (length(test_text[[i]]) > 1) {

            next_paragraph_pos_y <-
              1 - test_spacing_paragraph * ( i / length(test_text) )

            for (j in 2:length((test_text[[i]])))
            {
              pos_y <-
                c(pos_y,
                  first_line_pos_y - test_spacing_line *
                    ((j - 1) / (length((test_text[[i]])))) *
                    (first_line_pos_y - next_paragraph_pos_y)
                  )
            }
          }

          text(0,
               pos_y,
               labels = test_text[[i]],
               cex = test_cex,
               font = test_font,
               adj = test_adj,
               pos = 4,
               family = family)
        }

        # back to original graphics configuration
        par(current_par)
      }

      # Display horizontal (x) and vertical (y) titles ------------

      par(fig = x_title_fig_,
          new = T,
          mar = c(0, 0, 0, 0))

      plot.new()

      text(0,
           0.5,
           labels = x_title,
           cex = x_title_cex,
           font = x_title_font,
           pos = 4,
           family = family)

      #par(fig = c(0, 1, 0, 1))

      par(fig = y_title_fig_,
          new = T,
          mar = c(0, 0, 0, 0))

      plot.new()

      text(0,
           0.5,
           labels = y_title,
           cex = y_title_cex,
           font = y_title_font,
           pos = 3,
           srt = 270,
           family = family)

      #par(fig = c(0, 1, 0, 1))

      # back to original graphics configuration
      par(current_par)

      # Close graphics device if not in preview mode ==============
      if (out != "preview" && !leave_device_open) dev.off()

    }
  }
