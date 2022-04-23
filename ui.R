## Create Date: 2022/4/13
## Author: Erjie Zhao
## Purpose: UI of violin plot

library(shiny)
library(esquisse)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(shinyjqui) ## 图片拖拽大小
library(datamods)
library(htmltools)
library(ggplot2)

# Header ----------------------------------
header <- dashboardHeader(title = "ViolinPlot", titleWidth = 200, disable = F)


# sidebar ---------------------------------
sidebar <- dashboardSidebar(
  width = 200, disable = T, collapsed = F,
  sidebarMenu(
    menuItem("Violin plot", tabName = "Violinplot", icon = icon("meteor"))
  )
)


# left panel tab 1 ---------------------------------
left_panel_tab1 <- tabPanel(
  title = "输入文件",
  br(),
  fluidRow(
    column(width = 8, fileInput("file", "输入绘图数据",
      buttonLabel = "上传文件", multiple = FALSE,
      placeholder = "Demo_Violinplot.xlsx",
      accept = c(".txt", ".csv", ".xls", ".xlsx")
    )),
    column(width = 4, style = "padding: 24px;float:left;", downloadButton("downloaddemo", "下载示例文件"))
  ),
  # hr(),
  fluidRow(
    column(width = 8, fileInput("group", "输入分组信息", buttonLabel = "上传分组信息", placeholder = "Group.xlsx")),
    column(width = 4, style = "padding: 24px;float:left;", downloadButton("downloadgroup", "下载分组文件"))
  ),
  hr(),
  strong("数据处理"),
  tags$div(
    style = "color:#737373",
    p("选择log数据处理时，默认将原始数据+1,避免计算log(0)")
  ),
  radioButtons(
    inputId = "log", label = NULL,
    choices = c("log2", "log10", "不处理"),
    selected = "不处理", inline = T
  ),
  # br(), br(),
  fluidRow(
    column(
      width = 6,
      selectInput(
        inputId = "plot_type", label = "绘图选项",
        choices = c("按样本绘制", "按组绘制", "按组配色"),
        selected = '按样本绘制'
      )
    ),
    column(
      width = 4,
      uiOutput(outputId = "the_group")
    )
  ),
  # br(), br(),
  fluidRow(
    column(
      width = 4,
      radioButtons(
        inputId = "show_type", label = "展示方式",
        choices = c("分面", "不分面"), selected = "不分面", inline = T
      )
    ),
    column(
      width = 4,
      uiOutput(outputId = "facet")
    )
  ),
  # br(), br(),
  fluidRow(
    column(
      width = 6,
      selectInput(
        inputId = "orientation", label = "图片转置",
        choices = c("vertical", "horizontal", "reverse")
      )
    ),
    column(
      width = 4,
      tagList(
        radioButtons(
          inputId = "trim", label = "是否进行数据拟合",
          choices = c("是", "否"), inline = T,
          selected = '否'
        ),
        tags$div(
          style = "color:#737373; font-size:10px; ",
          p("进行数据拟合会使图片相对美观")
        )
      )
    )
  )
)

# left panel tab 2 -------------
left_panel_tab2 <- tabPanel(
  title = "主要参数",
  br(),
  fluidRow(
    column(width = 3, selectInput(
      inputId = "theme", label = "背景主题", choices = get_themes(),
      selected = "theme_classic"
    )),
    column(width = 3, numericInput(
      inputId = "alpha", label = "填充色透明度", min = 0, max = 1,
      value = 0.8
    )),
    column(width = 3, numericInput(
      inputId = "violin_width", label = "小提琴宽度", min = 0, max = 10,
      value = 1
    )),
    column(width = 3, selectizeInput(
      inputId = "add", label = "图形效果",
      choices = c("none", "boxplot", "dotplot", "jitter")
    ))
  ),
  # br(),
  fluidRow(
    column(width = 4, textInput("title", "图片主标题", value = "Violin Plot")),
    column(width = 4, textInput("xlab", "x轴标题", value = "Sample")),
    column(width = 4, textInput("ylab", "y轴标题", value = "Expression"))
  ),
  # br(),
  fluidRow(
    # column(width = 3, radioButtons("show_legend", "图例", c("显示", "隐藏"), inline = T, selected = "显示")),
    column(width = 4, 
           br(),
           materialSwitch("show_legend", strong("显示图例"), value = TRUE, status = 'primary', inline = FALSE)),
    column(width = 8, uiOutput("legend_setting"))
  ),
  fluidRow(
    column(width = 4, sliderInput("rect", "x标签的角度", value = 45, min = 0, max = 90, step = 15)),
    # column(width = 8, numericRangeInput(inputId = 'ylim', label = 'Y轴区间:', value = c(NA, NA)))),
    column(width = 4, numericInput("y_min", "y轴最小值", value = NA, min = 0, max = 100000)),
    column(width = 4, numericInput("y_max", "y轴最大值", value = NA, min = 0, max = 100000))
  ),
  fluidRow(
    column(width = 3, numericInput("title_size", "主标题大小", min = 0, max = 100, value = 20)),
    column(width = 3, numericInput("labs_title_size", "坐标轴标题大小", min = 0, max = 100, value = 15)),
    # column(width = 3, numericInput("legend_title_size", "图例度标题大小", min = 0, max = 100, value = 15)),
    column(width = 3, numericInput("axis_font_size", "刻度文本大小", min = 0, max = 100, value = 10))
  ),
  br(),
  h5(strong("颜色设置")),
  palette_ui("colors")
)

# middle panel tab 1 ----------
middle_panel_tab1 <- tabPanel(
  title = "图片&下载",
  ## 使下载标签在有图片的时候显现
  uiOutput(outputId = "download_label"),
  tags$div(
    align = "center",
    tags$br(),
    jqui_resizable(plotOutput(outputId = "boxplot", width = 500, height = 500))
  )
)

# middle panel tab 2 --------------------------------
middle_panel_tab2 <- tabPanel(
  "分析方法",
  tags$div(
    align = "center",
    tags$br(),
    tableOutput(outputId = "methods")
  )
)

# right panel tab 1 --------------------------------
right_panel_tab1 <- tabPanel(
  "输入数据",
  h4("绘图数据", align = "center"),
  DTOutput(outputId = "data_output"),
  h4("分组数据", align = "center"),
  DTOutput(outputId = "group_output")
)

# right panel tab 2 --------------------------------
right_panel_tab2 <- tabPanel('使用教程',
                             div(
                               style="margin:0px 12px;max-height: 800px; overflow-y: auto;",
                               h4(align="center",tags$b("使用教程")),
                               tags$div(
                                 h4(strong('简介：')),
                                 p('本工具适用于根据不同的样本或不同的组别绘制各自特征的小提琴图。'),
                                 h4(strong('适用范围：')),
                                 p('适用于蛋白组、修饰组、转录组、代谢组、16S、实验结果采集等所有给出二维数字矩阵的生物学数据。'),
                                 h4(strong('输入：')),
                                 p('输入文件主要有两个：(1) ', strong('样本的表达量数据'), '；(2) ', strong('样本的分组数据')),
                                 p('输入的文件格式包括txt(制表符分隔)文本文件、csv(逗号分隔)文本文件、以及Excel专用的xlsx格式。'),
                                 h5(strong('表达量数据')),
                                 p('表达量数据需要包含以下信息：'),
                                 div(align="center", tags$img(src = "data.png", width = "100%", height = "100%")),
                                 tags$br(),
                                 p('可以下载Demo数据文件进行修改，重新上传'),
                                 h5(strong('分组数据')),
                                 p('分组数据需要包含以下信息：'),
                                 div(align="center", tags$img(src = "group.png", width = "50%", height = "50%")),
                                 tags$br(),
                                 p(strong('注：'), '可以是多个分组信息，工具能展示不同分组下的情况'),
                                 p('可以下载Demo数据文件进行修改，重新上传'),
                                 h4(strong('下载：')),
                                 p('可拖拽以改变图片大小和比例'),
                                 div(align="center", tags$img(src = "pic.png", width = "80%", height = "80%")),
                                 tags$br(),
                                 p('点击上部的不同下载按钮，可以下载不同格式的图片文件'),
                                 div(align="center", tags$img(src = "download.png", width = "80%", height = "80%")),
                                 h4(strong('已传数据预览：')),
                                 p('可以在右侧的输入数据栏中查看已上传的数据，默认都只展示前五行的信息。'),
                                 tags$br(),
                                 tags$br()
                               )
                             )
)

# right panel tab 3 --------------------------------
right_panel_tab3 <- tabPanel('版本说明',
                             h4('v1.0.0'),
                             p('1. 该版本实现主要的绘制小提琴图的功能'),
                             br(),
                             p("开发者："),
                             p("景杰生信部"),
                             p("维护者："),
                             p("景杰生信部"),
                             p("发布日期："),
                             p("2022-04-20"),
                             p("最后更新日期："),
                             p("2022-04-21")
)

# body ------------------------------------
body <- dashboardBody(
  theme = shinytheme(theme = "flatly"),
  style = "background-color:white; font-size: 90%; ", ## 将全局默认背景改为白色
  tabItems(
    tabItem(
      "Violinplot",
      fluidRow(column(width = 4, h4("参数"), tabsetPanel(left_panel_tab1, left_panel_tab2)),
        column(width = 5, h4("结果与方法"), tabsetPanel(middle_panel_tab1, middle_panel_tab2)),
        column(width = 3, h4("说明"), tabsetPanel(right_panel_tab1, right_panel_tab2, right_panel_tab3)),
        style = "height:849px"
      )
    )
  )
)


# dashboard ---------------------------------------------------------------
ui <- dashboardPage(title = "Violin Plot", skin = "blue", header = header, sidebar = sidebar, body = body)
