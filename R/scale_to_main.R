
#' Scale a 'fig' argument to fit another
#'
#' Scale a numeric vector design to be used as \code{fig}
#' in \code{\link[graphics]{par}} to express a proportion
#' of another 'fig' vector. This function enables to easily
#' map several independent elements, with their own 'fig'
#' arguments  in the unit space (0 to 1 values),  within
#' a common plot area that is also controlled by a 'fig' argument.
#'
#' @param element_fig Numeric, the 'fig' vector delimitating
#'    the plot area of a given element, using the unity
#'    as a reference.
#' @param main_fig Numeric, the 'fig' vector delimitating
#'    the common plot area, within which limits the given
#'    element should be plot.
#'
#' @examples
#'
#' \dontrun{
#'
#' # graphics device is divided horizontally in two plots
#' plot1Fig <- c(0, 0.4999, 0, 1)
#' plot2Fig <- c(0.5099, 1, 0, 1)
#'
#' # each plot has two independent elements
#' # placed in a position relatively to a plot area
#' myElement1Fig <- c(0.85, 1, 0.4, 0.6)
#' myElement2Fig <- c(0.3, 0.85, 0.7, 0.9)
#'
#' # put plot fig arguments in a named list for convenience
#' plotsFigs <- list(plot1 = plot1Fig, plot2 = plot2Fig)
#'
#' # save the par() configuration
#' current_par <- par(no.readonly = TRUE)
#'
#' # iterate over the number of plots
#' for (i in 1:length(plotsFigs)) {
#'
#'   # set plot area
#'   par(fig = plotsFigs[[i]])
#'   # create main plot
#'   plot(1:10, 1:10)
#'
#'   # create first element
#'   par(fig = scale_to_main(myElement1Fig, plotsFigs[[i]]),
#'       new = T,
#'       mar = c(0, 0, 0, 0))
#'   plot.new()
#'   rect(0, 0, 1, 1)
#'   text(0.5, 0.5, labels = paste("myElement1\n", names(plotsFigs)[i], sep = ""))
#'
#'   # create second element
#'   par(fig = scale_to_main(myElement2Fig, plotsFigs[[i]]),
#'       new = T,
#'       mar = c(0, 0, 0, 0))
#'   plot.new()
#'   rect(0, 0, 1, 1)
#'   text(0.5, 0.5, labels = paste("myElement2\n", names(plotsFigs)[i], sep = ""))
#'
#'   par(current_par)
#'
#'   par(new = T)
#' }
#' par(current_par)
#'
#' }
#'
#' @export scale_to_main
#'
scale_to_main <- function(element_fig, main_fig) {

  x_range <- main_fig[2] - main_fig[1]
  y_range <- main_fig[4] - main_fig[3]

  new_element_fig <- c(main_fig[1] + element_fig[1] * x_range,
                       main_fig[1] + element_fig[2] * x_range,
                       main_fig[3] + element_fig[3] * y_range,
                       main_fig[3] + element_fig[4] * y_range)

  return(new_element_fig)
}
