#' Create a quick box plot in ggplot.
#'
#' This will graph a given vector in a ggplot-style box plot. If two vectors 
#' are entered, then it will group by the categorical one.
#'
#' @param x This is the vector to be plotted. If it is the only vector, 
#' it should be quantitative. If there are two vectors, it can be categorical.
#' @param y This is an optional second vector. If it is quantitative, then x 
#' should be categorical. If it is categorical, then x should be quantitative.
#' @param fill_color This is the color of the histogram bars.
#' @param horizontal This should be a TRUE or FALSE indicating if the box plot 
#' should be horizontal or not. 
#'
#' @return This function returns a ggplot box plot plot object.
#'
#' @examples
#' ## Create a box plot 
#' x <- rnorm(100)
#' gbox(x)
#' 
#' ## Create side-by-side box plots
#' x <- rnorm(100)
#' y <- c(rep("A", 30), rep("B", 70))
#' gbox(x, y)
#'
#' @import
#'   ggplot2
#'
#' @export

gbox <- function(x, y = NULL, fill_color = "lightblue", horizontal = FALSE) {
  if(class(x) != "numeric" & is.null(y)) {
    stop("The variable should be numeric.")
  }
  if(class(x) == "numeric" & is.null(y)) {
    if(horizontal) {
      data.frame(x) |>
        ggplot(aes(x = x)) +
        geom_boxplot(color = "black",
                       fill = fill_color)
    } else {
      data.frame(x) |>
        ggplot(aes(y = x)) +
        geom_boxplot(color = "black",
                       fill = fill_color)
    }
  } else if (class(x) == "numeric" & class(y) != "numeric") {
    if(horizontal) {
      data.frame(x, y) |>
        ggplot(aes(x = x, y = y)) +
        geom_boxplot(color = "black",
                       fill = fill_color) +
        labs(x = "x", y = "y")
    } else {
      data.frame(x, y) |>
        ggplot(aes(x = y, y = x)) +
        geom_boxplot(color = "black",
                       fill = fill_color) +
        labs(y = "x", x = "y")
    }
  } else if (class(x) != "numeric" & class(y) == "numeric") {
    if(horizontal) {
      data.frame(x, y) |>
        ggplot(aes(x = y, y = x)) +
        geom_boxplot(color = "black",
                       fill = fill_color) +
        labs(x = "x", y = "y")
    } else {
      data.frame(x, y) |>
        ggplot(aes(x = x, y = y)) +
        geom_boxplot(color = "black",
                       fill = fill_color) +
        labs(y = "x", x = "y")
    }
  }
}
