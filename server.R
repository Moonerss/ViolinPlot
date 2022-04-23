## Create Date: 2022/4/18
## Author: Erjie Zhao
## Purpose: server of violin plot

library(stringr)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(ggthemes)

server <- function(input, output, session) {

  ## seeting
  options(shiny.maxRequestSize = 30 * 1024^2)
  jqui_bookmarking() ## 图片可以动态拖拽
  original_dir <- getwd()

  # 保存全局交互参数
  values <- reactiveValues(
    data = NULL, group = NULL, plot_data = NULL,
    plot_setting = NULL, colors = NULL, plot = NULL
  )

  ## 展示分析方法内容 -------------------------
  methods <- readxl::read_excel(file.path(original_dir, "inst", "methods.xlsx"), sheet = 1)
  output$methods <- renderTable(
    expr = methods,
    bordered = TRUE,
    width = "90%"
  )

  ## Demo 数据下载 ----------------------------
  demo_data <- readxl::read_excel(file.path(original_dir, "demo", "Demo_Gene_Expression_ViolinPlot.xlsx"), sheet = 1)
  demo_group <- readxl::read_excel(file.path(original_dir, "demo", "Demo_Group.xlsx"), sheet = 1)
  output$downloaddemo <- downloadHandler(
    filename = "Demo_Boxplot.xlsx",
    content = function(file) {
      openxlsx::write.xlsx(demo_data, file = file)
    }
  )
  output$downloadgroup <- downloadHandler(
    filename = "Demo_Group.xlsx",
    content = function(file) {
      openxlsx::write.xlsx(demo_group, file = file)
    }
  )

  ## 绘图数据 ---------------------------------
  observe({
    if (!is.null(input$file)) {
      inFile <- input$file
      if ((str_detect(inFile$name, pattern = ".xlsx$")) | (str_detect(inFile$name, pattern = ".xls$"))) {
        sheets <- excel_sheets(path = inFile$datapath)
        if (length(sheets) > 1) {
          showModal(
            modalDialog(
              p("There are multiple sheets in the excel file, select first sheet for analysis!"),
              easyClose = TRUE,
              ooter = tagList(
                actionButton(inputId = "intro", label = "OK", icon = icon("info-circle"))
              )
            )
          )
        } else {
          data_up <- read_excel(inFile$datapath, sheet = 1)
          values$data <- data_up
        }
      } else if (str_detect(inFile$name, pattern = ".csv$")) {
        values$data <- read_csv(file = inFile$datapath)
      } else {
        values$data <- read_delim(file = inFile$datapath, delim = "\t")
      }
    } else {
      values$data <- NULL
    }
  })

  ## 分组数据 ---------------------------------
  observe({
    if (!is.null(input$group)) {
      groupFile <- input$group
      if ((str_detect(groupFile$name, pattern = ".xlsx$")) | (str_detect(groupFile$name, pattern = ".xls$"))) {
        sheets <- excel_sheets(path = groupFile$datapath)
        if (length(sheets) > 1) {
          # 弹窗提醒
          showModal(
            modalDialog(
              p("There are multiple sheets in the excel file, select first sheet for analysis!"),
              easyClose = TRUE,
              footer = tagList(
                actionButton(inputId = "intro", label = "OK", icon = icon("info-circle"))
              )
            )
          )
        } else {
          group <- read_excel(groupFile$datapath, sheet = 1)
          values$group <- group
        }
      } else if (str_detect(groupFile$name, pattern = ".csv$")) {
        values$group <- read_csv(file = groupFile$datapath)
      } else {
        values$group <- read_delim(file = groupFile$datapath, delim = "\t")
      }
    } else {
      values$group <- NULL
    }
  })

  ## 展示绘图数据-----------------
  output$data_output <- renderDT({
    req(!is.null(values$data))
    coln <- 5
    if (coln > ncol(values$data)) {
      coln <- ncol(values$data)
    }
    rown <- 5
    if (rown > nrow(values$data)) {
      rown <- nrow(values$data)
    }
    DT::datatable(values$data[1:rown, 1:coln],
      extensions = "Buttons", class = "row-border hover", ### nowrap
      options = list(pageLength = 10, lengthMenu = c(10, 20, 50, 100, -1), dom = "rt", scrollX = TRUE)
    )
  })

  ## 展示分组数据-----------------
  output$group_output <- renderDT({
    req(!is.null(values$group))
    coln <- 5
    if (coln > ncol(values$group)) {
      coln <- ncol(values$group)
    }
    rown <- 5
    if (rown > nrow(values$group)) {
      rown <- nrow(values$group)
    }
    DT::datatable(values$group[1:rown, 1:coln],
      extensions = "Buttons", class = "row-border hover", ### nowrap
      options = list(pageLength = 10, lengthMenu = c(10, 20, 50, 100, -1), dom = "rt", scrollX = TRUE)
    )
  })

  ## 整理画图数据 -------
  observe({
    req(!is.null(values$group))
    req(!is.null(values$data))
    values$plot_data <- dat_convert(values$data, values$group)
  })

  ## 观察绘图数据
  # output$plot_data <- renderDT({
  #   req(!is.null(values$plot_data))
  #   DT::datatable(values$plot_data)
  # })

  ## 动态UI，设置分组信息
  output$the_group <- renderUI({
    plot_type <- ifelse(input$plot_type == "按样本绘制", "ploted_by_id",
                        ifelse(input$plot_type == "按组绘制", "ploted_by_group", "colored_by_group")
    )
    if (plot_type %in% c("ploted_by_group", "colored_by_group")) {
      selectInput(
        inputId = "the_group", label = "选择分组",
        choices = colnames(values$plot_data)[-c(1:3)],
        selected = colnames(values$plot_data)[-c(1:3)][1]
      )
    }
  })
  ## 动态UI，设置分面信息
  output$facet <- renderUI({
    plot_type <- ifelse(input$plot_type == "按样本绘制", "ploted_by_id",
                        ifelse(input$plot_type == "按组绘制", "ploted_by_group", "colored_by_group")
    )
    show_type <- ifelse(input$show_type == '分面', 'facet', 'no_facet')
    if (plot_type %in% c("ploted_by_group", "colored_by_group")) {
      if (show_type == "facet") {
        selectInput(
          inputId = "facet", label = "选择分面分组",
          choices = colnames(values$plot_data)[-c(1:3)],
          selected = colnames(values$plot_data)[-c(1:3)][1]
        )
      }
    }
  })
  ## 动态UI，设置图例信息
  output$legend_setting <- renderUI({
    if (isTRUE(input$show_legend)) {
      fluidRow(
        column(width = 4, textInput("legend_title", "图例标题", value = NULL)),
        column(width = 4, selectInput(
          inputId = "legend_position", label = "图例位置",
          choices = c("上", "下", "左", "右"),
          selected = "右"
        )),
        column(width = 4, numericInput("legend_title_size", "图例标题大小", min = 0, max = 20, value = 10))
      )
    }
  })
  ## 动态UI，设置颜色信息
  observe({
    req(!is.null(values$plot_data))
    if (input$plot_type == "按样本绘制") {
      all_samples <- values$plot_data$sample
    } else {
      if (is.null(input$the_group)) {
        all_samples <- pull(values$plot_data, 4)
      } else {
        all_samples <- pull(values$plot_data, !!sym(input$the_group))
      }
    }
    palette_server(id = "colors", all_samples = all_samples)
    colors_r_d <- debounce(palette_server(id = "colors", all_samples = all_samples), millis = 1000)

    observe({
      values$colors <- colors_r_d()
    })
  })

  ## 设置绘图默认参数 -------------------------
  values$plot_setting <- list()

  ## 获取UI输入，改变默认参数 ---------------------
  observe({
    req(!is.null(values$plot_data))
    ## 由于有中文，避免乱码造成错误，对应成英文
    plot_type <- ifelse(input$plot_type == "按样本绘制", "ploted_by_id",
      ifelse(input$plot_type == "按组绘制", "ploted_by_group", "colored_by_group")
    )
    show_type <- ifelse(input$show_type == '分面', 'facet', 'no_facet')

    plot_by_group <- "sample"
    facet_by <- esquisse:::makeId('facet_by')
    
    if (plot_type == 'ploted_by_id') {
      plot_by_group <- "sample"
      color_by_group <- "sample"
      if (show_type == "facet") {
        facet_by <- "sample"
      }
    } else if (plot_type == 'colored_by_group') {
      plot_by_group <- "sample"
      if (is.null(input$the_group)) {
        color_by_group <- colnames(values$plot_data)[4]
      } else {
        color_by_group <- input$the_group
      }
      if (show_type == "facet") {
        if (is.null(input$facet)) {
          facet_by <- colnames(values$plot_data)[4]
        } else {
          facet_by <- input$facet 
        }
      }
    } else if (plot_type == 'ploted_by_group') {
      if (is.null(input$the_group)) {
        color_by_group <- colnames(values$plot_data)[4]
      } else {
        color_by_group <- input$the_group 
      }
      if (is.null(input$the_group)) {
        plot_by_group <- colnames(values$plot_data)[4]
      } else {
        plot_by_group <- input$the_group 
      }
      if (show_type == "facet") {
        if (is.null(input$facet)) {
          facet_by <- colnames(values$plot_data)[4]
        } else {
          facet_by <- input$facet 
        }
      }
    }
    values$plot_setting$plot_by_group <- plot_by_group
    values$plot_setting$color_by_group <- color_by_group
    values$plot_setting$facet_by <- facet_by

    values$plot_setting$log <- switch(input$log,
      log2 = "log_2",
      log10 = "log_10",
      不处理 = "none"
    )
    values$plot_setting$trim <- switch(input$trim,
      是 = TRUE,
      否 = FALSE
    )
    values$plot_setting$y_lim <- c(input$y_min, input$y_max)
    values$plot_setting$orientation <- input$orientation
    values$plot_setting$theme <- input$theme
    values$plot_setting$alpha <- input$alpha
    #   values$plot_setting$violin_width <- input$violin_width
    #   values$plot_setting$add <- input$add
    values$plot_setting$title <- input$title
    values$plot_setting$xlab <- input$xlab
    values$plot_setting$ylab <- input$ylab
    
    values$plot_setting$show_legend <- input$show_legend
    values$plot_setting$legend_title <- input$legend_title
    
    if (is.null(input$legend_position)) {
      legend_position <- 'right'
    } else {
      legend_position <- ifelse(input$legend_position == "上", "top",
                                ifelse(input$legend_position == "下", "bottom",
                                       ifelse(input$legend_position == "左", "left", "right")
                                )
      )
    }
    values$plot_setting$legend_position <- legend_position
    values$plot_setting$title_size <- input$title_size
    values$plot_setting$labs_title_size <- input$labs_title_size
    values$plot_setting$axis_font_size <- input$axis_font_size
    values$plot_setting$legend_title_size <- input$legend_title_size
    values$plot_setting$axis_x_font_angle <- input$rect
    values$plot_setting$violin_width <- input$violin_width
    values$plot_setting$add <- input$add
  })

  # output$options <- renderPrint({
  #   req(!is.null(values$plot_setting))
  #   str(values$plot_setting)
  # })

  ## 绘图
  observe({
    req(!is.null(values$plot_data))
    
    if (is.null(values$colors)) {
      which_pal_scale_obj <- NULL
    } else {
      if (values$colors$scale == 'palette') {
        which_pal_scale_obj <- esquisse::which_pal_scale(mapping = aes(fill = !!sym(values$plot_setting$color_by_group),
                                                                       color = !!sym(values$plot_setting$color_by_group)),
                                                         data = values$plot_data,
                                                         palette = values$colors$colors,
                                                         fill_type = 'discrete',
                                                         color_type = 'discrete',
                                                         reverse = values$colors$reverse)      
      } else if (values$colors$scale == 'manual') {
        which_pal_scale_obj <- esquisse::which_pal_scale(mapping = aes(fill = !!sym(values$plot_setting$color_by_group),
                                                                       color = !!sym(values$plot_setting$color_by_group)),
                                                         data = values$plot_data,
                                                         palette = values$colors$colors,
                                                         fill_type = values$colors$type,
                                                         color_type = values$colors$type,
                                                         reverse = FALSE) 
      }
    }
    
    values$plot <- plot_violin(values$plot_data,
      log = values$plot_setting$log,
      plot_by_group = values$plot_setting$plot_by_group,
      facet_by = values$plot_setting$facet_by,
      
      color_by_group = values$plot_setting$color_by_group,
      alpha = values$plot_setting$alpha,

      trim = values$plot_setting$trim,
      orientation = values$plot_setting$orientation,
      
      theme = values$plot_setting$theme,
      
      show_legend  = values$plot_setting$show_legend,
      legend_title = values$plot_setting$legend_title,
      legend_position = values$plot_setting$legend_position,
      
      xlab = values$plot_setting$xlab,
      ylab = values$plot_setting$ylab,
      main = values$plot_setting$title,
      
      title_size = values$plot_setting$title_size,
      axis_font_size = values$plot_setting$axis_font_size,
      labs_title_size = values$plot_setting$labs_title_size,
      legend_title_size = values$plot_setting$legend_title_size,
      axis_x_font_angle = values$plot_setting$axis_x_font_angle,
      
      y_lim = values$plot_setting$y_lim,
      violin_width = values$plot_setting$violin_width,
      
      add = values$plot_setting$add,
      
      which_pal_scale_obj = which_pal_scale_obj
    )
  })

  output$boxplot <- renderPlot({
    req(!is.null(values$plot))
    print(values$plot)
  })


  ## 下载结果图片 -----------------------
  output$download_label <- renderUI({
    req(!is.null(values$plot))
    tagList(
      tags$div(
        align = "center",
        tags$br(),
        downloadBttn(
          outputId = "download_png",
          label = "PNG",
          style = "bordered",
          color = "primary",
          no_outline = TRUE,
          icon = ph("image"),
          size = "xs"
        ),
        downloadBttn(
          outputId = "download_pdf",
          label = "PDF",
          style = "bordered",
          color = "primary",
          no_outline = TRUE,
          icon = ph("file-pdf"),
          size = "xs"
        ),
        downloadBttn(
          outputId = "download_svg",
          label = "SVG",
          style = "bordered",
          color = "primary",
          no_outline = TRUE,
          icon = ph("browsers"),
          size = "xs"
        ),
        downloadBttn(
          outputId = "download_jpeg",
          label = "JEPG",
          style = "bordered",
          color = "primary",
          no_outline = TRUE,
          icon = ph("image"),
          size = "xs"
        ),
        downloadBttn(
          outputId = "download_pptx",
          label = "PPTX",
          style = "bordered",
          color = "primary",
          no_outline = TRUE,
          icon = ph("projector-screen"),
          size = "xs"
        )
      )
    )
  })

  output$download_png <- downloadHandler(
    filename = paste0("Boxplot-", Sys.Date(), ".png"),
    content = function(file) {
      ggsave(
        filename = file, plot = values$plot,
        width = input$boxplot_size$width / 80,
        height = input$boxplot_size$height / 80
      )
    }
  )
  output$download_pdf <- downloadHandler(
    filename = paste0("Boxplot-", Sys.Date(), ".pdf"),
    content = function(file) {
      ggsave(
        filename = file, plot = values$plot,
        width = input$boxplot_size$width / 80,
        height = input$boxplot_size$height / 80
      )
    },
    contentType = "pdf"
  )
  output$download_svg <- downloadHandler(
    filename = paste0("Boxplot-", Sys.Date(), ".svg"),
    content = function(file) {
      ggsave(
        filename = file, plot = values$plot,
        width = input$boxplot_size$width / 80,
        height = input$boxplot_size$height / 80
      )
    }
  )
  output$download_jpeg <- downloadHandler(
    filename = paste0("Boxplot-", Sys.Date(), ".jpeg"),
    content = function(file) {
      ggsave(
        filename = file, plot = values$plot,
        width = input$boxplot_size$width / 80,
        height = input$boxplot_size$height / 80
      )
    },
    contentType = "jpeg"
  )
  output$download_pptx <- downloadHandler(
    filename = paste0("Boxplot-", Sys.Date(), ".pptx"),
    content = function(file) {
      editable_graph <- rvg::dml(ggobj = values$plot)
      ppt <- officer::read_pptx() %>%
        officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
        officer::ph_with(
          value = editable_graph,
          location = officer::ph_location_type(type = "body")
        ) %>%
        print(target = file)
    }
  )
}
