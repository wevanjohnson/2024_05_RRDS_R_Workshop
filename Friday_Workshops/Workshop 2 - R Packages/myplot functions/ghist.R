ghist <- function(x, fill_color = "lightblue", num_bins = 30) {
  data.frame(x) |>
    ggplot(aes(x = x)) +
    geom_histogram(color = "black",
                   fill = fill_color,
                   bins = num_bins)
}
