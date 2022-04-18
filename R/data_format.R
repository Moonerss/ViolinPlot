#' @description 修改数据用于绘图
#' 
#' @param dat 输入的绘图数据
#' @param group 输入的分组数据
#' @import magrittr
#' @import dplyr
#' @import tibble

# dat <- readxl::read_excel('./demo/Demo_Gene_Expression_ViolinPlot.xlsx', sheet = 1)
# group <- readxl::read_excel('./demo/Demo_Group.xlsx', sheet = 1)

dat_convert <- function(dat, group) {
  dat_mat <- dat %>% 
    rename(gene = !!sym(colnames(dat)[1])) %>% 
    pivot_longer(cols = !gene, names_to = 'sample', values_to = 'expression') %>% 
    left_join(group, by = c('sample' = colnames(group)[1]))
  return(dat_mat)
}

# plot_dat <- dat_convert(dat = dat, group = group)
