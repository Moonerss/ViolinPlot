## Create Date: 2022/4/18
## Author: Erjie Zhao
## Purpose: server of violin plot

library(stringr)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)

server <- function(input, output, session) {
  
  ## seeting 
  options(shiny.maxRequestSize=30*1024^2)
  jqui_bookmarking() ## 图片可以动态拖拽
  original_dir <- getwd()
  
  # 保存全局交互参数
  values <- reactiveValues(data = NULL, group = NULL, plot_data = NULL,
                           plot_setting = NULL, colors = NULL, plot = NULL)
  
  ## 展示分析方法内容 -------------------------
  methods <- readxl::read_excel(file.path(original_dir, 'inst', 'methods.xlsx'), sheet = 1)
  output$methods <- renderTable(
    expr = methods,
    bordered = TRUE,
    width = '90%'
  )
  
  ## Demo 数据下载 ----------------------------
  demo_data <- readxl::read_excel(file.path(original_dir, 'demo', 'Demo_Gene_Expression_ViolinPlot.xlsx'), sheet = 1)
  demo_group <- readxl::read_excel(file.path(original_dir, 'demo', 'Demo_Group.xlsx'), sheet = 1)
  output$downloaddemo <- downloadHandler(
    filename = 'Demo_Boxplot.xlsx',
    content = function(file) {
      openxlsx::write.xlsx(demo_data, file = file)
    }
  )
  output$downloadgroup <- downloadHandler(
    filename = 'Demo_Group.xlsx',
    content = function(file) {
      openxlsx::write.xlsx(demo_group, file = file)
    }
  )
  
  ## 绘图数据 ---------------------------------
  observe({
    if (!is.null(input$file)) {
      inFile <- input$file
      if ((str_detect(inFile$name,pattern = '.xlsx$')) | (str_detect(inFile$name,pattern = '.xls$'))) {
        sheets <- excel_sheets(path = inFile$datapath)
        if (length(sheets) > 1) {
          showModal(
            modalDialog(
              p("There are multiple sheets in the excel file, select first sheet for analysis!"),
              easyClose = TRUE,
              ooter = tagList(
                actionButton(inputId = "intro", label = "OK", icon = icon("info-circle")))
            )
          )
        } else {
          data_up <- read_excel(inFile$datapath, sheet = 1)
          values$data <- data_up
        }
      } else if (str_detect(inFile$name,pattern = '.csv$')) {
        values$data <- read_csv(file = inFile$datapath)
      } else {
        values$data <- read_delim(file = inFile$datapath, delim = '\t')
      }
    } else {
      values$data <- NULL
    }
  })
  
  ## 分组数据 ---------------------------------
  observe({
    if (!is.null(input$group)) {
      groupFile <- input$group
      if ((str_detect(groupFile$name,pattern = '.xlsx$')) | (str_detect(groupFile$name,pattern = '.xls$'))) {
        sheets <- excel_sheets(path = groupFile$datapath)
        if (length(sheets) > 1) {
          # 弹窗提醒
          showModal(
            modalDialog(
              p('There are multiple sheets in the excel file, select first sheet for analysis!'),
              easyClose = TRUE,
              footer = tagList(
                actionButton(inputId = "intro", label = "OK", icon = icon("info-circle")))
            )
          )
        } else {
          group <- read_excel(groupFile$datapath, sheet = 1)
          values$group <- group
        }
      } else if (str_detect(groupFile$name,pattern = '.csv$')) {
        values$group <- read_csv(file = groupFile$datapath)
      } else {
        values$group <- read_delim(file = groupFile$datapath, delim = '\t')
      }
    } else {
      values$group <- NULL
    }
  })
  
  ## 展示绘图数据-----------------
  output$data_output <- renderDT({
    req(!is.null(values$data))
    coln = 5
    if (coln > ncol(values$data)) {
      coln <- ncol(values$data)
    }
    rown = 5
    if (rown > nrow(values$data)) {
      rown <- nrow(values$data)
    }
    DT::datatable(values$data[1:rown,1:coln], extensions = 'Buttons', class = "row-border hover",### nowrap
                  options=list(pageLength = 10,lengthMenu = c(10, 20, 50, 100,-1),dom = 'rt', scrollX=TRUE))
  })
  
  ## 展示分组数据-----------------
  output$group_output <- renderDT({
    req(!is.null(values$group))
    coln = 5
    if (coln > ncol(values$group)) {
      coln <- ncol(values$group)
    }
    rown = 5
    if (rown > nrow(values$group)) {
      rown <- nrow(values$group)
    }
    DT::datatable(values$group[1:rown,1:coln], extensions = 'Buttons', class = "row-border hover",### nowrap
                  options=list(pageLength = 10,lengthMenu = c(10, 20, 50, 100,-1),dom = 'rt', scrollX=TRUE))
  })
  
  ## 整理画图数据 -------
  observe({
    req(!is.null(values$group))
    req(!is.null(values$data))
    values$plot_data <- dat_convert(values$data, values$group)
  })
  
  ## 观察绘图数据
  output$plot_data <- renderDT({
    req(!is.null(values$plot_data))
    DT::datatable(values$plot_data)
  })
  
  ## 动态UI，设置分组信息
  output$the_group <- renderUI({
    if (input$plot_type %in% c('按组绘制', '按组配色')) {
      selectInput(inputId = 'the_group', label = '选择分组',
                  choices = colnames(values$plot_data)[-c(1:3)])
    }
  })
  ## 动态UI，设置分面信息
  output$facet <- renderUI({
    if (input$plot_type %in% c('按组绘制', '按组配色')) {
      selectInput(inputId = 'facet', label = '选择分面分组',
                  choices = colnames(values$plot_data)[-c(1:3)])
    }
  })
  
  ## 设置绘图默认参数 -------------------------
  values$plot_setting <- list()
  
  ## 获取UI输入，改变默认参数 ---------------------
  observe({
    ## 
    if () {
      
    }
  })
  
}