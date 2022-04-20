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
                        plot_by_group = 'sample',
                        color_by_group = 'sample',
                        facet_by = esquisse:::makeId('facet_by'),
                        trim = FALSE, alpha = 0.7,
                        orientation = c("vertical", "horizontal", "reverse"),
                        theme = 'theme_classic',
                        show_legend = TRUE, legend_title = 'Group', legend_position = 'right',
                        xlab = 'Sample', ylab = 'log10(Value+1)', main = 'Violin Plot',
                        title_size = 20, axis_font_size = 9, legend_title_size = 10,
                        axis_x_font_angle = 0, labs_title_size = 10,
                        y_lim = c(NA, NA), violin_width = 12,
                        add = c("none", "boxplot", "dotplot", "jitter"),
                        which_pal_scale_obj = NULL
                        ) {
  log = match.arg(log) 
  if (log == 'log_2') {
    plot_data$expression <- log2(plot_data$expression + 1)
  } else if (log == 'log_10') {
    plot_data$expression <- log10(plot_data$expression + 1)
  }
  
  ## plot by group && color_by_group
  p <- ggplot(plot_data) +
    aes(x = !!sym(plot_by_group), y = expression) +
    geom_violin(
      aes(fill = !!sym(color_by_group), color = !!sym(color_by_group)),
      width = violin_width, alpha = alpha, trim = trim)
  
  ## add other part
  add <- match.arg(add)
  if (add == 'boxplot') {
    p <- p +
      geom_boxplot(aes(color = !!sym(color_by_group)), width = 0.2)
  } else if (add == 'dotplot') {
    p <- p +
      geom_point(aes(color = !!sym(color_by_group)))
  } else if (add == 'jitter') {
    p <- p +
      geom_jitter(aes(color = !!sym(color_by_group)), width = 0.2)
  }
  
  ## orientation 
  ori <- match.arg(orientation)
  if (ori == "horizontal") {
    p <- p + coord_flip()
  } else if (ori == "reverse") {
    p <- p + scale_y_reverse()
  }
  
  ## facet
  if (facet_by != esquisse:::makeId('facet_by')) {
    p <- p + facet_grid(cols = vars(!!sym(facet_by)), scales = 'free')
  }
  
  ## set fill color
  if (!is.null(which_pal_scale_obj)) {
    p <- p +
      eval(rlang::call2(which_pal_scale_obj[[1]][1], !!!(which_pal_scale_obj[[2]][[1]]))) +
      eval(rlang::call2(which_pal_scale_obj[[1]][2], !!!(which_pal_scale_obj[[2]][[2]])))
  }
  
  ## y lim
  if (is.na(y_lim[1])) {
    if (is.na(y_lim[2])) {
      p <- p
    } else {
      p <- p + ylim(NA, y_lim[2])
    }
  } else {
    if (is.na(y_lim[2])) {
      p <- p + ylim(y_lim[1], NA)
    } else {
      p <- p + ylim(y_lim[1], y_lim[2])
    }
  }
  
   ## add theme
  if (grepl("::", x = theme)) {
    real_theme <- strsplit(x = theme, split = "::")[[1]][2]
    pkg <- strsplit(x = theme, split = "::")[[1]][1]
    p <- p + base::eval(call(real_theme), envir = rlang::search_env(rlang::pkg_env_name(pkg)))
  } else {
    p <- p + base::eval(call(theme))
  }
  
  ## add lab
  p <- p + labs(title = main, x = xlab, y = ylab, fill = legend_title, color = legend_title)
  
  ## legend
  if (!show_legend) {
    p <- p + theme(legend.position = 'none')
  } else {
    p <- p + theme(legend.position = legend_position)
  }
  
  ## font size
  p <- p +
    theme(axis.text = element_text(size = axis_font_size),
          axis.title = element_text(size = labs_title_size),
          plot.title = element_text(size = title_size, hjust = 0.5),
          axis.text.x = element_text(angle = axis_x_font_angle, hjust = 0.5, vjust = 0.5),
          legend.title = element_text(size = legend_title_size))
  
  return(p)
}





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