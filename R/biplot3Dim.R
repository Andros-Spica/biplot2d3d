
#' 3D biplot
#'
#' Generates a 3D biplot using a rgl device,
#' representing the default points/scores and loadings of
#' an ordination object, such as a PCA produced by
#' \code{\link[stats]{princomp}}).
#'
#' @param ordination_object A R object containing a direct
#'    and named reference to default ordination outputs
#'    (i.e. \code{ordination_object$scores} or
#'    \code{ordination_object$points},
#'    \code{ordination_object$loadings} and
#'    \code{ordination_object$sdev}) available for at least
#'    3 dimensions. Alternatively, a data frame or matrix
#'    with three columns is accepted, provided that
#'    \code{ordination_method = NULL}), which will create a
#'    three-dimensional scatter plot.
#' @param ordination_method Character, the ordination method
#'    that was used to generate the ordination object:
#'    "PCA" for Principal Components Analysis (default),
#'    "PCoA" for Principal Coordinates Analysis,
#'    "NMDS" for Non-metric Multidimensional Scaling, and
#'    "LDA" for Linear Discriminant Analysis.
#'
#' @param biplot_type Character, indicating the type of
#'    biplot scalling of data: "default" and
#'    "pc.biplot", corresponding to the transformations
#'    performed in \code{\link[stats]{biplot.princomp}}
#'    with \code{pc.biplot = FALSE} ("default") and
#'    \code{pc.biplot = TRUE} ("pc.biplot"). If NULL,
#'    no processing is performed, assuming that data
#'    within \code{ordination_object} was previously prepared.
#' @param rows_over_columns Numeric, the value defining the
#'    degree in which distances between observations have
#'    priority over distances between variables
#'    (0 = variable-focused, 1 = observation-focused).
#'    It corresponds to the argument \code{scale} in
#'    \code{\link[stats]{biplot.princomp}}. It will be
#'    ignored if \code{biplot_type = NULL}.
#'
#' @param groups A factor variable containing the group
#'    assignation of each point.
#' @param vips A list of logical (Boolean) vectors identifying
#'    the "Very Important Points" under different methods
#'    or criteria.
#'
#' @param detach_arrows Logical, wheter to display covariance arrows
#'    a independent miniature plot, overlapping the main plot and
#'    placed according to \code{arrow_fig}.
#' @param show_group_legend Logical, whether to display a
#'    legend for groups?
#' @param show_vip_legend Logical, whether to display a
#'    legend for vip criteria?
#' @param show_arrows Logical, whether to show variable
#'    covariance arrows.
#' @param show_fitAnalysis Logical, whether to display
#'    the fit analysis plot corresponding to the ordination
#'    method given (Scree plot or Shepard plot).
#'
#' @param show_axes,show_planes Character, vectors indicating
#'    which axes and planes to draw (See \code{\link{rgl_format}}).
#' @param show_bbox Logical, wheter to display a
#'    bounding box (See \code{\link{rgl_format}}).
#' @param invert_coordinates Logical, vector of length
#'    three expressing which dimensions, if any, must be
#'    inverted before plotting (e.g. for aesthetical reasons).
#' @param aspect,symmetric_axes,adapt_axes_origin,axes_colors,axes_head_size,axes_titles,axes_titles_cex,axes_titles_font,axes_titles_adj,axes_titles_alpha The arguments passed
#'    to \code{\link{rgl_format}} to configure the space
#'    and create axes.
#' @param planes_colors,planes_textures,planes_alpha,planes_lit,planes_shininess
#'    The arguments passed to \code{\link{rgl_format}}
#'    to create dimensional planes.
#' @param bbox_color,bbox_alpha,bbox_shininess,bbox_xat,bbox_xlab,bbox_xunit,bbox_xlen,bbox_yat,bbox_ylab,bbox_yunit,bbox_ylen,bbox_zat,bbox_zlab,bbox_zunit,bbox_zlen,bbox_marklen,bbox_marklen_rel,bbox_expand,bbox_draw_front The arguments passed to
#'    \code{\link{rgl_format}} to create a bounding box.
#'
#' @param title Character, title to be placed in the
#'    fixed 2D canvas ('main' in \code{\link[graphics]{title}}).
#' @param title_line,title_color,title_size,title_font,title_adj the line,
#'    color, size, font, and justification of the
#'    title (\code{line}, \code{col.main}, \code{cex.main},
#'    \code{font.main}, and \code{adj} in
#'    \code{\link[graphics]{title}}, \code{\link[graphics]{par}}).
#' @param subtitle Character, subtitle to be placed in
#'    the fixed 2D canvas ('main' in \code{\link[graphics]{text}}).
#' @param subtitle_position Numeric verctor of length two indicating the
#'    position of the subtitle in the fixed 2D canvas ('main'
#'    in \code{\link[graphics]{text}}).
#' @param subtitle_color,subtitle_cex,subtitle_font,subtitle_adj the color, size, font,
#'    and justification of the subtitle ('col', 'cex',
#'    font and adj in \code{\link[graphics]{text}},
#'    \code{\link[graphics]{par}}).
#'
#' @param point_type Character, accepting three values: "point",
#'    the default \code{\link[rgl]{points3d}}; "label",
#'    displaying the content of \code{point_label}; and
#'    "point and label", placing both points and labels.
#' @param point_label Character, vector labelling every
#'    observation. It's length must be equal to the number
#'    of rows in the points/scores of the ordination object.
#'    (\code{nrow(ordination_object$points) ==
#'    length(point_label)}).
#' @param point_size The size or scale given to \code{size}
#'    in \code{\link[rgl]{points3d}}
#' @param point_alpha The alpha of points given to \code{alpha}
#'    in \code{\link[rgl]{points3d}}
#' @param point_label_cex,point_label_font,point_label_adj,point_label_alpha
#'    The text parameters and the alpha of the arrows' labels.
#'    (\code{\link[rgl]{text3d}}, \code{\link[rgl]{rgl.material}}).
#'
#' @param group_color A vector containing the color or colors
#'    to be used in each group (applied to points, labels, stars
#'    and ellipsoids).
#' @param group_representation Character, indicating which group
#'    representation to draw: "stars", "ellipsoids", or
#'    "stars and ellipsoids". Neither stars or ellipsoids
#'    are drawn, if NULL is given instead.
#' @param ellipsoid_type,ellipsoid_level,ellipsoid_singleton_color,ellipsoid_singleton_radius,ellipsoid_wire_alpha,ellipsoid_wire_lit,ellipsoid_shade_alpha,ellipsoid_shade_lit,ellipsoid_label_cex,ellipsoid_label_font,ellipsoid_label_adj,ellipsoid_label_alpha
#'    When ellipsoids are drawn, parameters given to
#'    \code{\link{ellipsoids_3d}}.
#' @param star_centroid_radius,star_centroid_alpha,star_link_width,star_link_alpha,star_label_cex,star_label_font,star_label_adj,star_label_alpha
#'    When stars are drawn, parameters given to
#'    \code{\link{stars_3d}}.
#' @param group_legend_title Character, the title of the groups
#'    legend. If equal NULL or "", no title is displayed.
#' @param group_legend_title_pos A numeric vector of length
#'    two, the xy position of the title within the groups
#'    legend box.
#' @param group_legend_title_cex,group_legend_title_font,group_legend_title_adj
#'    The text parameters to be applied in the groups
#'    legend title (\code{\link[graphics]{par}}).
#' @param group_legend_box_color The background color of the
#'    groups legend box.
#' @param group_legend_key_margin The x position of the keys
#'    within the groups legend box. Values from 0 to 1.
#' @param group_legend_key_pch,group_legend_key_lwd,group_legend_key_cex
#'    The type, line width and sizing factor of the keys in
#'    the groups legend.
#' @param group_legend_text_margin The x position of the
#'    text entries in the groups legend. Values from 0 to 1.
#' @param group_legend_text_color The color or colors of the
#'    text entries in the groups legend.
#' @param group_legend_text_cex,group_legend_text_font,group_legend_text_adj
#'    The text parameters of the text entries in the groups legend.
#'
#' @param vip_pch A character vector containing the characters
#'    used for the vips markings.
#' @param vip_cex,vip_colors,vip_font,vip_adj,vip_alpha
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
#' @param vip_legend_key_margin The x position of the keys
#'    within the vips legend box. Values from 0 to 1.
#' @param vip_legend_key_cexFactor The sizing factor of the
#'    keys in the vips legend respect to the vips marking
#'    in the plot.
#' @param vip_legend_text_margin The x position of the text
#'    entries in the vips legend box. Values from 0 to 1.
#' @param vip_legend_text_cex,vip_legend_text_font,vip_legend_text_adj The
#'    text parameters of the text entries in the vips legend.
#'
#' @param arrow_color The color or colors to be used in
#'    covariance arrows (pass to \code{\link{radial_arrows_3d}}).
#' @param arrow_min_dist The minimum distance of a variable
#'    arrow from the origin of arrows, in order for it to be
#'    displayed (range [0 = all arrows are displayed,
#'    1 = no arrows are displayed]).
#' @param arrow_center_pos A numeric vector of length three,
#'    containing the position of the origin of covariance
#'    arrows, expressed in relation to the 3D space represented
#'    (e.g. c(.5, .5, .5) will place the arrows in the center).
#' @param arrow_head_shape_theta,arrow_head_shape_n,arrow_head_size,arrow_body_width,arrow_body_length,arrow_label_color,arrow_label_cex,arrow_label_font,arrow_label_adj,arrow_label_alpha
#'    When the covariance arrows are displayed, parameters
#'    given to \code{\link{radial_arrows_3d}}.
#'
#' @param fitAnalysis_cex,fitAnalysis_lwd,fitAnalysis_screePlot_color,fitAnalysis_stress_p_color,fitAnalysis_stress_l_color
#'    The graphical parameters of the plot for fit analysis
#'    of the ordination method
#'    (\code{\link[graphics]{par}},
#'    \code{\link[vegan]{stressplot}}
#'    of the \code{vegan} package).
#'
#' @param test_text Character, vector with the lines of
#'    text presenting the results of statistical tests.
#' @param test_cex,test_font,test_adj The text parameters
#'    of the text in the test results displayed in the
#'    fixed 2D canvas (\code{\link[graphics]{par}}).
#'
#' @param group_legend_fig,vip_legend_fig,fitAnalysis_fig,test_fig The \code{fig} parameter (\code{\link[graphics]{par}}) to
#'    place in the display region of the graphics device,
#'    respectively, the group and vip legends, the fit
#'    analysis plot, and the tests results.
#'
#' @param new_device,bg_color,view_theta,view_phi,view_fov,view_zoom,width,height
#'    The arguments passed to \code{\link{rgl_init}}.
#' @param family The font family used in every text in the plot,
#'    (\code{\link[graphics]{par}}).
#'
#' @details This function allows customising virtually every
#' graphical parameter in a 3D biplot, including several extra
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
#' Groups can be represented as stars
#' (\code{\link{stars_3d}}), ellipsoids
#' (\code{\link{ellipsoids_3d}}), and/or colors,
#' which can be tracked by a fully-customisable legend
#' (\code{group_legend} arguments). Individual observations
#' deemed exceptional (vip = Very Important Points) can be
#' marked with custom characters. Whenever there are more than one
#' type of marking (e.g. different methods/criteria of
#' outlier detection), different characters can be presented
#' in a legend (vip legend). When desired, it is possible to
#' display a Scree plot representing the eigenvalues
#' (Principal Components Analysis, Principal Coordinates Analysis)
#' or a Shepard or stress plot (Nonmetric Multidimensional Scaling,
#' \code{\link[vegan]{metaMDS}} in the vegan package) by enabling
#' \code{show_fit_analysis}. It is also possible to display
#' statistical test results (enabling \code{show.tests} and
#' introducing lines of text in \code{tests.text}). Every 2D
#' element (legend boxes, title and subtitle, fit analysis plot
#' and tests) are placed in a fixed 2D canvas (i.e. viewport)
#' using \code{\link[rgl]{bgplot3d}}.
#'
#' @references
#'
#' Gabriel, K. R. (1971). The biplot graphical display of
#'   matrices with applications to principal component analysis.
#'   Biometrika, 58, 453-467.
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
#' # Default plot using Species as the groups
#' biplot_3d(pca, groups = iris$Species)
#'
#' # ---------------------------------------------------------
#' # Plot groups as ellipsoids, make group label invisible and
#' # add a groups legend with no title.
#' # Customize the covariance arrows default setting.
#' biplot_3d(pca,
#'           groups = iris$Species,
#'           group_representation = "ellipsoids",
#'           ellipsoid_label_alpha = 0,
#'           show_group_legend = TRUE,
#'           group_legend_title = "",
#'           arrow_center_pos = c(.5, 0, .5),
#'           arrow_body_length = 1,
#'           arrow_body_width = 2,
#'           view_theta = 0,
#'           view_zoom = 0.9)
#'
#' # ---------------------------------------------------------
#' # Plot observations using their names and groups as
#' # stars but adding a legend instead of labels.
#' # Modify the aspect to normalize the variability
#' # of axes and do not show them. Zoom out a little.
#' biplot_3d(pca, groups = iris$Species,
#'           point_type = "label", point_label = row.names(iris),
#'           star_label_alpha = 0,
#'           show_group_legend = TRUE, group_legend_title = "",
#'           arrow_center_pos = c(.5, 0, .5),
#'           arrow_body_length = 2, arrow_body_width = 2,
#'           show_axes = FALSE, view_zoom = 1)
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
#' # Plot observations using their names and group by
#' # Species using only color. Mark the VIP and add the
#' # respective legend with custom characters.
#' # Rotate the theta view angle to fit the arrows
#' # in the default setting.
#' biplot_3d(pca,
#'           groups = iris$Species,
#'           point_type = "label",
#'           point_label = row.names(iris),
#'           group_representation = NULL,
#'           show_group_legend = TRUE,
#'           group_legend_title = "",
#'           vips = irisVIP,
#'           vip_pch = c("X", "O", "+"),
#'           vip_cex = c(2, 2, 3),
#'           show_axes = FALSE, view_theta = 340)
#'
#' # ---------------------------------------------------------
#' # Test the setosa separation
#' irisDist <- dist(iris[, 1:4])
#'
#' setosaSeparation <- iris$Species == "setosa"
#'
#' ## multivariate test for the setosa separation
#' require(vegan)
#' irisTests <- NULL
#' irisTests$permanova <- adonis(irisDist ~ setosaSeparation)
#' irisTests$permdisp2 <- permutest(betadisper(irisDist,
#'                                             setosaSeparation),
#'                                  pairwise = TRUE)
#'
#' # The following function prepares a list of character vectors
#' # containing test results
#' getTestText <- function(tests){
#'   permanova_F <- as.character(round(tests$permanova$aov.tab$F.Model[1], 3))
#'   permanova_pvalue <- as.character(round(tests$permanova$aov.tab$"Pr(>F)"[1], 3))
#'   permanova_rSquared <- as.character(round(tests$permanova$aov.tab$R2[1], 3))
#'   permdisp2_F <- as.character(round(tests$permdisp2$tab$F[1], 3))
#'   permdisp2_pvalue <- as.character(round(tests$permdisp2$tab$"Pr(>F)"[1], 3))
#'   text <- list(c(paste("PERMANOVA:\n   F = ", permanova_F,
#'                        " (p = ", permanova_pvalue, ")\n",
#'                        sep = ""),
#'                  c(expression(paste("   ", R^2, " =",
#'                  sep = "")),
#'                    paste("          ", permanova_rSquared,
#'                    sep = ""))),
#'                paste("PERMDISP2:\n   F = ", permdisp2_F,
#'                      " (p = ", permdisp2_pvalue,")",
#'                      sep = ""))
#'   return(text)
#' }
#'
#' # Plot observations using points and
#' # groups as stars with no labels.
#' # Place tests results in the bottom left corner
#' # and give a custom title.
#' biplot_3d(pca,
#'           groups = iris$Species,
#'           point_type = "point",
#'           star_label_alpha = 0,
#'           show_group_legend = TRUE,
#'           group_legend_title = "",
#'           test_text = getTestText(irisTests),
#'           test_cex = 1.5,
#'           test_fig = c(0.01, 0.5, 0.7, 1),
#'           show_axes = FALSE,
#'           view_theta = 340,
#'           title = "testing setosa separation")
#'
#'}
#'
#' @export biplot_3d
#'
biplot_3d <-
  function(ordination_object,
           ordination_method = "PCA",
           biplot_type = "default",
           rows_over_columns = 0.5,
           groups = NULL,
           vips = NULL,

           detach_arrows = TRUE,
           show_group_legend = FALSE,
           show_vip_legend = FALSE,
           show_arrows = TRUE,
           show_fitAnalysis = TRUE,

           show_axes = c("X", "Y", "Z"),
           show_planes = NULL,
           show_bbox = FALSE,
           invert_coordinates = c(FALSE, FALSE, FALSE),
           aspect = c(1, 1, 1),
           symmetric_axes = FALSE,
           adapt_axes_origin = TRUE,
           axes_colors = "darkgrey",
           axes_head_size = 3,
           axes_titles = "",
           axes_titles_cex = 2,
           axes_titles_font = 2,
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
           bbox_draw_front = FALSE,
           title = "",
           title_color = "black",
           title_line = -2,
           title_size = 3,
           title_font = 20,
           title_adj = .5,
           subtitle = NULL,
           subtitle_color = "black",
           subtitle_position = c(0.03, .005),
           subtitle_cex = 2,
           subtitle_font = 2,
           subtitle_adj = 0,

           point_type = "point",
           point_label = NULL,
           point_size = 5,
           point_alpha = 1,
           point_label_cex = 0.8,
           point_label_font = 3,
           point_label_adj = c(0.5, 0.5),
           point_label_alpha = 1,

           group_color = NULL,
           group_representation = NULL,
           ellipsoid_type = "wire and shade",
           ellipsoid_level = 0.95,
           ellipsoid_singleton_color = NULL,
           ellipsoid_singleton_radius = 0.1,
           ellipsoid_wire_alpha = 0.2,
           ellipsoid_wire_lit = FALSE,
           ellipsoid_shade_alpha = 0.1,
           ellipsoid_shade_lit = FALSE,
           ellipsoid_label_cex = 1,
           ellipsoid_label_font = 2,
           ellipsoid_label_adj = c(-0.25, 0.5),
           ellipsoid_label_alpha = 1,
           star_centroid_radius = 0.005,
           star_centroid_alpha = 0.5,
           star_link_width = 1,
           star_link_alpha = 1,
           star_label_cex = 1,
           star_label_font = 2,
           star_label_adj = c(-0.25, 0.5),
           star_label_alpha = 1,
           group_legend_title = "Groups",
           group_legend_title_pos = c(0.5, 0.85),
           group_legend_title_cex = 2,
           group_legend_title_font = 3,
           group_legend_title_adj = 0.5,
           group_legend_box_color = "white",
           group_legend_key_pch = 15,
           group_legend_key_lwd = 1,
           group_legend_key_margin = 0.15,
           group_legend_key_cex = 3,
           group_legend_text_margin = 0.25,
           group_legend_text_color = "black",
           group_legend_text_cex = 1.5,
           group_legend_text_font = 1,
           group_legend_text_adj = 0,

           vip_pch = c("/", "\\", "o", "_", "|", "O"),
           vip_cex = c(2, 2, 2, 2, 2, 2),
           vip_colors = "black",
           vip_font = 3,
           vip_adj = c(0.5, 0.5),
           vip_alpha = 0.8,
           vip_legend_title = "Outliers",
           vip_legend_title_pos = c(0.5, 0.85),
           vip_legend_title_cex = 2,
           vip_legend_title_font = 3,
           vip_legend_title_adj = 0.5,
           vip_legend_box_color = "white",
           vip_legend_key_margin = 0.15,
           vip_legend_key_cexFactor = 0.8,
           vip_legend_text_margin = 0.25,
           vip_legend_text_cex = 1,
           vip_legend_text_font = 1,
           vip_legend_text_adj = 0,

           arrow_color = "darkorange",
           arrow_min_dist = 0,
           arrow_center_pos = c(1, 0, 1),
           arrow_head_shape_theta = pi / 6,
           arrow_head_shape_n = 3,
           arrow_head_size = 0.1,
           arrow_body_length = 0.2,
           arrow_body_width = 1,
           arrow_label_color = "black",
           arrow_label_cex = 0.8,
           arrow_label_font = 2,
           arrow_label_adj = 0.5,
           arrow_label_alpha = 1,

           fitAnalysis_cex = 3,
           fitAnalysis_lwd = 3,
           fitAnalysis_screePlot_color = "white",
           fitAnalysis_stress_p_color = "darkgrey",
           fitAnalysis_stress_l_color = "black",

           test_text = NULL,
           test_cex = 1,
           test_font = 1,
           test_adj = 0.5,

           group_legend_fig =    c(0.73, 0.99,  0.6, 0.90),
           vip_legend_fig =      c(0.03, 0.25,  0.1,  0.3),
           fitAnalysis_fig =     c(0.02, 0.35, 0.08,  0.3),
           test_fig =            c(   0,  0.3,  0.8,    1),

           new_device = FALSE,
           bg_color = "white",
           view_theta = 15,
           view_phi = 20,
           view_fov = 60,
           view_zoom = 0.8,
           width = 800,
           height = 600,
           family = "sans") {

    # Load data ===================================================
    scores <- NULL
    loadings <- NULL
    sdev <- NULL
    eigenvalues <- NULL

    # Select data depending on the ordination method

    # Not an ordination object -------------------------------------
    # If there is no ordination method,
    # ordination_object is interpreted as a data frame.
    if (is.null(ordination_method)) {

      warning("ordination_method = NULL, so the ordination_object is interpreted as a data frame.\nNo covariance arrows or fit analysis will be displayed.",
              call = FALSE)

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
                           dimensions = 1:3,
                           scale = rows_over_columns,
                           pc.biplot = isPCbiplot)

      if (biplot_type == "default" || biplot_type == "pc.biplot") {

        scores <- t(t(scores[, 1:3])/lambda)
        loadings <- t(t(loadings[, 1:3]) * lambda)

      } else {

        if (!is.null(biplot_type)) {

          stop("The biplot_type given is not supported. Choose between 'default' and 'pc.biplot' or NULL, if data was previously transformed.",
               call. = FALSE)

        }

      }

      eigenvalues <- (sdev) ^ 2
      names(eigenvalues) <- rep("", length(eigenvalues))

    }
    else
      # Nonmetric Multidimensional Scaling ---------------------------
    if (ordination_method == "NMDS") {

      scores <- ordination_object$points
      loadings <- ordination_object$loadings

    }
    else
      # Principal Coordiantes Analysis -------------------------------
    if (ordination_method == "PCoA") {

      scores <- ordination_object$points
      loadings <- ordination_object$loadings
      eigenvalues <- ordination_object$eig

    }
    else
      # Linear Discriminant Analysis ---------------------------------
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
      # Method not supported -----------------------------------------
    # If the method given is not supported
    {

      stop(paste("The method '", ordination_method, "' is not supported or not properly written.\nPlease pass either 'PCA', 'PCoA', 'NMDS', or 'LDA' as the ordination_method argument.", sep = ""),
           call. = FALSE)

    }

    # If the row names don't exist,
    # fill them with the index numbers
    if (is.null(row.names(scores))) {

      row.names(scores) <- as.character(1:nrow(scores))

    }

    # get points dimensions----------------------------------------

    x = scores[, 1]
    y = scores[, 2]
    z = scores[, 3]

    # get variables loadings---------------------------------------

    loadings <- ordination_object$loadings[, 1:3]


    # Set up parameters ===========================================

    # Load subtitle -----------------------------------------------

    sub <- subtitle

    if (is.null(subtitle)) {

      # If no subtitle is given
      if (is.null(ordination_method)) {

        # If there is no subtitle specified,
        # no subtitle is displayed
        sub <- ""

      } else {

        # If there is an ordination method,
        # the percentage of variance represented is displayed
        cumVar <- cumsum((sdev) ^ 2) / sum(sdev ^ 2)
        sub <-
          paste(as.character(100 * round(cumVar[3], digits = 4)),
                "% of variance explained",
                sep = "")

      }
    }

    # ivert coordinates (aesthetics) ------------------------------

    if (invert_coordinates[1]) {

      x <- -x
      loadings[, 1] <- -loadings[, 1]

    }

    if (invert_coordinates[2]) {

      y <- -y
      loadings[, 2] <- -loadings[, 2]

    }
    if (invert_coordinates[3]) {

      z <- -z
      loadings[, 3] <- -loadings[, 3]

    }

    # Prepare the parameters for distinguish points by groups -----

    groups_ <- groups
    group_color_ <- group_color
    group_pch <- NULL

    # if groups are not given,
    # display points as belonging all to the same group
    if (is.null(groups)) {

      groups_ <- factor(rep("", nrow(scores)))

    }

    if (is.null(groups)) {

      # if groups are not given,
      # display points as belonging all to the same group
      groups_ <- factor(rep("", nrow(scores)))
      if (is.null(group_color)) group_color_ <- "black"

    } else {

      # if group_color is not specified,
      # get diferent colors for each group
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

    # Recognize if point labels are used to differentiate groups.
    # If so, save group_pch to reflect this in the legend.
    if (nlevels(factor(point_label)) == nlevels(groups_)) {

      group_pch <- levels(factor(point_label))

    }

    # Plot ========================================================

    # initialize the rgl device -----------------------------------

    rgl_init(bg_color = bg_color,
             view_theta = view_theta,
             view_phi = view_phi,
             view_fov = view_fov,
             view_zoom = view_zoom,
             width = width,
             height = height)

    # Draw the 2D layer--------------------------------------------

    rgl::bgplot3d({

      par(mar = c(0, 0, 0, 0))

      plot.new()

      # title------------------------------------------------------

      title(main = title,
            line = title_line,
            col.main = title_color,
            cex.main = title_size,
            family = family,
            font.main = title_font,
            adj = title_adj)

      # subtitle---------------------------------------------------

      text(subtitle_position[1],
           subtitle_position[2],
           labels = sub,
           col = subtitle_color,
           cex = subtitle_cex,
           family = family,
           font = subtitle_font,
           adj = subtitle_adj)

      # insert the plot corresponding to the fit analysis ---------
      # of the ordination object

      if (show_fitAnalysis) {

        par(fig = fitAnalysis_fig,
            new = T,
            mar = c(0.1, 0.1, 1, 0.1))

        if (ordination_method == "NMDS") {

          # for the NMDS, the stress plot is shown
          if (!requireNamespace("vegan", quietly = TRUE)){

            stop("if ordination_method is NMDS (Non.metric Multidimensional Scaling),\nthe vegan package is required for drawing a stress plot.",
                 call. = FALSE)

          }

          vegan::stressplot(ordination_object,
                            p.col = fitAnalysis_stress_p_color,
                            l.col = fitAnalysis_stress_l_color,
                            xaxt = "n",
                            yaxt = "n",
                            cex = fitAnalysis_cex,
                            lwd = fitAnalysis_lwd)

        } else {

          # for PCA and PCoA, the Scree plot is shown
          barplot(eigenvalues,
                  space = 0,
                  col = fitAnalysis_screePlot_color,
                  axes = FALSE,
                  cex.axis = 0,
                  cex = fitAnalysis_cex,
                  lwd = fitAnalysis_lwd)

          # this shades the three first eigenvalues
          barplot(eigenvalues[1:3],
                  space = 0,
                  col = "grey",
                  cex = fitAnalysis_cex,
                  axes = FALSE,
                  cex.axis = 0,
                  add = TRUE)

        }
        ### TO DO: add LDA fit analysis
      }

      # Display tests results -------------------------------------

      if (!is.null(test_text)) {

        par(fig = test_fig,
            new = T,
            mar = c(0, 0, 0, 0))

        plot.new()

        for (i in 1:length(test_text)) {

          pos_y <- 1 - ((i + 1) /  (length(test_text) + 1.5))

          text(x = 0, y = pos_y,
               labels = test_text[[i]],
               cex = test_cex,
               font = test_font,
               family = family,
               adj = test_adj,
               pos = 4)

        }
      }

      # Create the legend for the vips ----------------------------

      if (!is.null(vips) & show_vip_legend) {

        firstLine <- 1

        if (is.null(vip_legend_title) ||
            vip_legend_title == "") {
          firstLine <- 0
        }

        # Create the legend for the vips methods/criteria
        par(fig = vip_legend_fig,
            new = T,
            mar = c(0, 0, 0, 0))

        plot.new()

        rect(xleft = 0,
             ybottom = 0,
             xright = 1,
             ytop = 1,
             col = vip_legend_box_color)

        text(x = vip_legend_title_pos[1],
             y = vip_legend_title_pos[2],
             labels = vip_legend_title,
             cex = vip_legend_title_cex,
             font = vip_legend_title_font,
             family = family,
             adj = vip_legend_title_adj)

        for (i in 1:length(vips)) {

          # the following gets the overrides
          # for the graphical arguments
          # being them either vectors or single values.
          vip_pch_ <- vip_pch[1]
          if (length(vip_pch) == length(vips)) {
            vip_pch_ <- vip_pch[i]
          }
          vip_cex_ <- vip_cex[1]
          if (length(vip_cex) == length(vips)) {
            vip_cex_ <- vip_cex[i]
          }
          vip_colors_ <- vip_colors[1]
          if (length(vip_colors) == length(vips)) {
            vip_colors_ <- vip_colors[i]
          }
          vip_font_ <- vip_font[1]
          if (length(vip_font) == length(vips)) {
            vip_font_ <- vip_font[i]
          }
          vip_adj_ <- vip_adj[1]
          if (!is.null(nrow(vip_adj)) &&
              nrow(vip_adj) > 1 &&
              nrow(vip_adj) >= i) {
            vip_adj_ <- vip_adj[i,]
          }
          vip_alpha_ <- vip_alpha[1]
          if (length(vip_alpha) == length(vips)) {
            vip_alpha_ <- vip_alpha[i]
          }

          pos_y <- 1 - ((firstLine + i) / ((firstLine + 1) + length(vips)))

          text(x = vip_legend_key_margin,
               y = pos_y,
               labels = vip_pch_,
               cex = vip_cex_ * vip_legend_key_cexFactor,
               col = vip_colors_,
               font = vip_font_,
               family = family,
               adj = vip_adj_)

          text(x = vip_legend_text_margin,
               y = pos_y,
               labels = names(vips)[i],
               cex = vip_legend_text_cex,
               font = vip_legend_text_font,
               family = family,
               adj = vip_legend_text_adj)

        }
      }

      # Create the legend for groups ------------------------------
      # (color -> groups)

      if (show_group_legend) {

        firstLine <- 1
        if (is.null(group_legend_title) ||
            group_legend_title == "") {
          firstLine <- 0
        }

        # Create the legend for the factor (color -> groups)
        par(fig = group_legend_fig,
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
          # In case that groups are differentiated
          # by point labels (as in pch of par)
          if (!is.null(group_pch)) {
            group_legend_key_pch_ <- group_pch[i]
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
      }
    })

    # Create main plot ------------------------------------------

    # Create points
    if (point_type == "point" ||
        point_type == "point and label") {

      rgl::points3d(x = x, y = y, z = z,
                    color = get_colors(groups_, group_color_),
                    size = point_size,
                    alpha = point_alpha)

    }

    # Create point labels
    if (point_type == "label" ||
        point_type == "point and label" ||
        !is.null(point_label)) {

      labels_ <- point_label
      # if there are no labels given, take the row names
      if (is.null(labels_)) labels_ <- row.names(scores)

      rgl::text3d(x = x, y = y, z = z,
                  texts = labels_,
                  col = get_colors(groups_, group_color_),
                  cex = point_label_cex,
                  family = family,
                  font = point_label_font,
                  adj = point_label_adj,
                  alpha = point_label_alpha)

    }

    # Mark vips ---------------------------------------------------
    # (Very Important Points, e.g. outliers), when any is given,
    # distinguishing the various detection methods

    if (!is.null(vips)) {

      # Create markings following each criterium
      # (column or list element in vips)
      for (i in 1:length(vips)) {

        # the following gets the overrides for the
        # graphical arguments
        # being them either vectors or single values.

        vip_pch_ <- vip_pch[1]
        if (length(vip_pch) >= length(vips))    vip_pch_ <- vip_pch[i]
        vip_cex_ <- vip_cex[1]
        if (length(vip_cex) >= length(vips))    vip_cex_ <- vip_cex[i]
        vip_colors_ <- vip_colors[1]
        if (length(vip_colors) >= length(vips)) vip_colors_ <- vip_colors[i]
        vip_font_ <- vip_font[1]
        if (length(vip_font) >= length(vips))    vip_font_ <- vip_font[i]
        vip_adj_ <- vip_adj[1]
        if (length(vip_adj) >= length(vips))    vip_adj_ <- vip_adj[i]
        vip_alpha_ <- vip_alpha[1]
        if (length(vip_alpha) >= length(vips))  vip_alpha_ <- vip_alpha[i]

        rgl::text3d(x = x[vips[[i]]],
                    y = y[vips[[i]]],
                    z = z[vips[[i]]],
                    texts = vip_pch_,
                    col = vip_colors_,
                    cex = vip_cex_,
                    family = family,
                    font = vip_font_,
                    adj = vip_adj_,
                    alpha = vip_alpha_)
      }
    }

    # get aspect
    asp <- aspect
    if (is.null(aspect)) {
      asp <- calculate_aspect(x, y, z)
    }

    # set up background elements of the plot
    rgl_format(x = x, y = y, z = z,
               aspect = asp,
               symmetric_axes = symmetric_axes,
               show_axes = show_axes,
               show_planes = show_planes,
               show_bbox = show_bbox,
               adapt_axes_origin = adapt_axes_origin,
               axes_colors = axes_colors,
               axes_titles = axes_titles,
               axes_titles_cex = axes_titles_cex,
               axes_titles_font = axes_titles_font,
               axes_titles_family = family,
               axes_titles_adj = axes_titles_adj,
               axes_titles_alpha = axes_titles_alpha,
               planes_colors = planes_colors,
               planes_textures = planes_textures,
               planes_alpha = planes_alpha,
               planes_lit = planes_lit,
               planes_shininess = planes_shininess,
               bbox_color = bbox_color,
               bbox_alpha = bbox_alpha,
               bbox_shininess = bbox_shininess,
               bbox_xat = bbox_xat,
               bbox_xlab = bbox_xlab,
               bbox_xunit = bbox_xunit,
               bbox_xlen = bbox_xlen,
               bbox_yat = bbox_yat,
               bbox_ylab = bbox_ylab,
               bbox_yunit = bbox_yunit,
               bbox_ylen = bbox_ylen,
               bbox_zat = bbox_zat,
               bbox_zlab = bbox_zlab,
               bbox_zunit = bbox_zunit,
               bbox_zlen = bbox_zlen,
               bbox_marklen = bbox_marklen,
               bbox_marklen_rel = bbox_marklen_rel,
               bbox_expand = bbox_expand,
               bbox_draw_front = bbox_draw_front)

    # Create group-related elements (ellipsoids and stars)---------

    if (!is.null(group_representation)) {

      if (group_representation == "ellipsoids" ||
          group_representation == "stars and ellipsoids") {

        # Compute and draw the ellipsoids of concentration
        ellipsoids_3d(x = x, y = y, z = z,
                           groups = groups_,
                           group_color = group_color_,
                           type = ellipsoid_type,
                           level = ellipsoid_level,
                           singleton_color = ellipsoid_singleton_color,
                           singleton_radius = ellipsoid_singleton_radius,
                           wire_alpha = ellipsoid_wire_alpha,
                           wire_lit = ellipsoid_wire_lit,
                           shade_alpha = ellipsoid_shade_alpha,
                           shade_lit = ellipsoid_shade_lit,
                           label_cex = ellipsoid_label_cex,
                           label_family = family,
                           label_font = ellipsoid_label_font,
                           label_adj = ellipsoid_label_adj,
                           label_alpha = ellipsoid_label_alpha)

      }

      if (group_representation == "stars" ||
          group_representation == "stars and ellipsoids") {

        # Star grouping
        stars_3d(x = x, y = y, z = z,
                      groups = groups_,
                      group_color = group_color_,
                      centroid_radius = star_centroid_radius,
                      centroid_alpha = star_centroid_alpha,
                      link_width = star_link_width,
                      link_alpha = star_link_alpha,
                      label_cex = star_label_cex,
                      label_family = family,
                      label_font = star_label_font,
                      label_adj = star_label_adj,
                      label_alpha = star_label_alpha)

      }
    }

    # insert the miniature of variables covariances (arrows) ------
    if (show_arrows) {

      arrow_center_pos_x <- 0
      arrow_center_pos_x <- 0
      arrow_center_pos_x <- 0

      if (detach_arrows) {
        # get a position for arrows center from the given relative position
        arrow_center_pos_x <- min(x) + (max(x) - min(x)) * arrow_center_pos[1]
        arrow_center_pos_y <- min(y) + (max(y) - min(y)) * arrow_center_pos[2]
        arrow_center_pos_z <- min(z) + (max(z) - min(z)) * arrow_center_pos[3]
      }

      # filter arrows
      loadings <- filter_arrows(loadings = loadings,
                                min_dist = arrow_min_dist,
                                dimensions = 3)

      if (nrow(loadings) > 0) {

        radial_arrows_3d(x = loadings[, 1],
                         y = loadings[, 2],
                         z = loadings[, 3],
                         center_pos = c(arrow_center_pos_x,
                                        arrow_center_pos_y,
                                        arrow_center_pos_z),
                         head_shape_theta = arrow_head_shape_theta,
                         head_shape_n = arrow_head_shape_n,
                         head_size = arrow_head_size,
                         body_length = arrow_body_length,
                         body_width = arrow_body_width,
                         color = arrow_color,
                         label_color = arrow_label_color,
                         label_cex = arrow_label_cex,
                         label_family = family,
                         label_font = arrow_label_font,
                         label_adj = arrow_label_adj,
                         label_alpha = arrow_label_alpha)

      }
    }

  }

#' Get colors for the different levels of a factor variable
#'
#' @param groups A factor variable containing the group of each observation.
#' @param color_palette The palette or vector of colors to be used.
#'
#' @return Return a vector of colors matching the levels of groups.
#'
get_colors <- function(groups, color_palette = palette()) {

  groups <- as.factor(groups)

  ngrps <- nlevels(groups)

  color_palette_ <- color_palette

  if (ngrps > length(color_palette)) color_palette_ <- rep(color_palette, ngrps)

  colors <- color_palette_[as.numeric(groups)]

  names(colors) <- as.vector(groups)

  return(colors)

}

#' Create an animated GIF with the rgl device
#'
#' @param directory The directory within the working directory. For example, "MyFolder/".
#' @param file_name The name of the output file.
#' @param axis_spin a numeric vector of lenght 3 indicating around which axis should the plot spin. See \code{\link[rgl]{movie3d}}.
#' @param axis_spin_rpm the velocity of the spin. See \code{\link[rgl]{movie3d}}.
#'
#' @export animation
#'
animation <- function(directory = "",
                      file_name = "animation",
                      axis_spin = c(0, 1, 0),
                      axis_spin_rpm = 5){

  if (directory == "") directory <- tempdir()

  # snapshot
  rgl::rgl.snapshot(filename = paste(directory,
                                     paste(file_name,"_snapshot.png", sep=""),
                                     sep="/"))

  # Create a movie
  rgl::movie3d(rgl::spin3d(axis = axis_spin,
                           rpm = axis_spin_rpm),
               duration = (60 / axis_spin_rpm),
               dir = directory,
               movie = file_name)

}
