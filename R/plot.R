#' @description function to plot violin plot in shiny
#' @param log the log convert of data before plot
#' @param plot_by_group the col name of group to plot by
#' @param color_by_group the col name of group to color by
#' @param facet_by facet grid
#' @param trim 
#' @param alpha
#' @param orientation
#' 
#' 
plot_violin <- function(plot_data, log = c('log_2', 'log_10', 'none'),
                        plot_by_group = 'sample', color_by_group = NULL,
                        facet_by = NULL, trim = FALSE, alpha = 0.7,
                        orientation = c("vertical", "horizontal", "reverse")) {
  log = match.arg(log) 
  if (log == 'log_2') {
    plot_data$expression <- log2(plot_data$expression + 1)
  } else if (log == 'log_10') {
    plot_data$expression <- log10(plot_data$expression + 1)
  }
  
  ## plot by group
  p <- ggplot(plot_data) +
    aes(x = !!sym(plot_by_group), y = expression) +
    geom_violin(
      aes(fill = !!sym(plot_by_group), color = !!sym(plot_by_group)),
      alpha = alpha, trim = trim)
  ## color by group
  if (!is.null(color_by_group)) {
    p <- ggplot(plot_data) +
      aes(x = sample, y = expression) +
      geom_violin(
        aes(fill = !!sym(color_by_group), color = !!sym(color_by_group)),
        alpha = alpha, trim = trim)
  }
  ## orientation 
  ori <- match.arg(orientation)
  if (ori == "horizontal") {
    p <- p + coord_flip()
  } else if (ori == "reverse") {
    p <- p + scale_y_reverse()
  }
  
  ## facet
  print(is.null(facet_by))
  if (!is.null(facet_by)) {
    p <- p + facet_grid(cols = vars(!!sym(facet_by)), scales = 'free')
  }
  
  return(p)
}

# plot_violin <- function(data, x, y ,
#                         color = 'black', fill = 'white', palette = NULL, alpha = 1,
#                         title = NULL, xlab = NULL, ylab = NULL,
#                         facet_by = NULL, panel_labs = NULL,
# ) {
#   # Default options
#   
# }





# ggplot(plot_dat) +
#   aes(x = sample, y = expression, fill = sample) +
#   geom_violin(adjust = 1L, scale = "width") +
#   geom_jitter() +
#   scale_fill_manual(
#     values = c(Demo_A1 = "#F8766D",
#                Demo_A2 = "#DD8A1D",
#                Demo_A3 = "#B59C00",
#                Demo_B1 = "#78AC0A",
#                Demo_B2 = "#0DB832",
#                Demo_B3 = "#00BE79",
#                Demo_C1 = "#00BEB7",
#                Demo_C2 = "#08B6E5",
#                Demo_C3 = "#4FA1F9",
#                Demo_D1 = "#A385FC",
#                Demo_D2 = "#E46DEB",
#                Demo_D3 = "#FF61C3")
#   ) +
#   scale_y_continuous(trans = "log10") +
#   theme_dark() +
#   theme(legend.position = "none")