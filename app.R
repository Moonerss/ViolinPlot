library(shiny)
library(magrittr)
library(dplyr)
library(tibble)
library(ggplot2)
library(rlang)


source('./R/data_format.R', local = TRUE)
source('./R/get_themes.R', local = TRUE)
# source('./R/plot.R', local = TRUE)

# Run the application 
shinyApp(ui = ui, server = server)
