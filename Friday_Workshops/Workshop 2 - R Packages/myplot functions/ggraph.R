#' Create a quick scatter plot in ggplot.
#'
#' This will graph two given vectors in a ggplot-style scatter plot with the
#' x-axis labeled "x" and the y-axis labeled "y".
#'
#' @param x This is the first vector to be plotted.
#' @param y This is the first vector to be plotted.
#' @param point_color This is the color of the points that will be plotted.
#' @param point_size This is the size of the points that will be plotted.
#' The default is size 1.5.
#' @param point_shape This is the shape of the points that will be plotted.
#' The default is 19: a filled circle.
#'
#' @return This function returns a ggplot scatter plot object.
#'
#' @examples
#' ## Create a scatter plot of y vs x.
#' x <- rnorm(100)
#' y <- x + rnorm(100, 0, 0.3)
#' ggraph(x, y)
#'
#' @import
#'   ggplot2
#'   magrittr
#'
#' @export

ggraph <- function(x, y, point_color = "black",
                   point_size = 1.5, point_shape = 19) {
  data.frame(x, y) %>%
    ggplot(aes(x = x, y = y)) +
    geom_point(color = point_color,
               size = point_size,
               shape = point_shape)
}
