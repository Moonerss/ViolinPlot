## Create Date: 2022/4/13
## Author: Erjie Zhao
## Purpose: UI of violin plot

library(shiny)
library(esquisse)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(shinyjqui)##图片拖拽大小
library(datamods)
library(htmltools)

# Header ----------------------------------
header <- dashboardHeader(title = 'ViolinPlot', titleWidth = 200, disable = F)


# sidebar ---------------------------------
sidebar <- dashboardSidebar(width = 200, disable = F, collapsed = F,
                            sidebarMenu(
                              menuItem('Violin plot', tabName = 'Violinplot', icon = icon("meteor"))
                            )
)


# left panel tab 1 ---------------------------------
left_panel_tab1 <- tabPanel(title = '输入文件',
                            br(),
                            fluidRow(column(width = 8, fileInput("file", '输入绘图数据', buttonLabel = "上传文件", multiple = FALSE,
                                                                 placeholder = 'Demo_Violinplot.xlsx',
                                                                 accept = c(".txt",".csv",".xls",".xlsx"))),
                                     column(width = 4, style='padding: 24px;float:left;', downloadButton("downloaddemo","下载示例文件"))),
                            hr(),
                            fluidRow(column(width = 8, fileInput("group", '输入分组信息', buttonLabel = "上传分组信息", placeholder = 'Group.xlsx')),
                                     column(width = 4, style='padding: 24px;float:left;', downloadButton("downloadgroup","下载分组文件"))),
                            hr(),
                            strong('数据处理'),
                            tags$div(
                              style = "color:#737373",
                              p('选择log数据处理时，默认将原始数据+1,避免计算log(0)')
                            ),
                            radioButtons(inputId = 'log', label = NULL, 
                                         choices = c('log2' = 'log2', 'log10' = 'log10', '不处理' = 'no_change'),
                                         selected = 'no_change', inline = T),
                            br(), br(),
                            fluidRow(
                              column(width = 6,
                                     selectInput(inputId = 'plot_type', label = '绘图选项',
                                                 choices = c('按样本绘制', '按组绘制', '按组配色'))),
                              column(width = 4,
                                     uiOutput(outputId = 'the_group')
                                     )
                              ),
                            br(), br(),
                            fluidRow(
                              column(width = 4,
                                     radioButtons(inputId = 'show_type', label = '展示方式',
                                                  choices = c('分面', '不分面'), inline = T)),
                              column(width = 4,
                                     uiOutput(outputId = 'facet')
                                     )
                            ),
                            br(), br(),
                            fluidRow(
                              column(width = 6,
                                     selectInput(inputId = 'orientation', label = '图片转置',
                                                 choices = c('vertical', 'horizontal', 'reverse'))
                                     ),
                              column(width = 4,
                                     tagList(
                                       radioButtons(inputId = 'trim', label = '是否进行数据拟合',
                                                    choices = c('是', '否'), inline = T),
                                       tags$div(
                                         style = 'color:#737373; font-size:10px; ',
                                         p('进行数据拟合会使图片相对美观') 
                                         )
                                       )
                                     )
                              )
                            )

# left panel tab 2 -------------
left_panel_tab2 <- tabPanel(title = '主要参数',
                            br(),
                            fluidRow(column(width = 4, selectInput(inputId = 'theme', label = '背景主题', choices = get_themes(),
                                                                   selected = "theme_classic")),
                                     column(width = 4, numericInput(inputId = 'alpha', label = '填充色透明度', min = 0, max = 1,
                                                                    value = 0.8)),
                                     column(width = 4, numericInput(inputId = 'violin_width', label = '小提琴宽度', min = 0, max = 10,
                                                                    value = 1))
                            ),
                            br(),
                            fluidRow(column(width = 4, selectizeInput(inputId = 'add', label = '图形效果', choices = c('none', 'boxplot', 'dotplot', 'jitter')))
                                     ),
                            br(),
                            fluidRow(column(width = 3, textInput("title", "图片主标题", value = 'Violin Plot')),
                                     column(width = 3, textInput("xlab", "x轴标题", value = 'Sample')),
                                     column(width = 3, textInput("ylab", "y轴标题", value = 'Expression')),
                                     column(width = 3, textInput("legend_title", "图例标题", value = NULL))
                                     ),
                            br(),
                            fluidRow(column(width = 4, sliderInput('rect', 'x标签的角度', value = 45, min = 0, max = 90, step = 15)),
                                     #column(width = 8, numericRangeInput(inputId = 'ylim', label = 'Y轴区间:', value = c(NA, NA)))),
                                     column(width = 4, numericInput('y_min', 'y轴最小值', value = NA, min = 0, max = 100000)),
                                     column(width = 4, numericInput('y_max', 'y轴最大值', value = NA, min = 0, max = 100000))),
                            fluidRow(column(width = 4, radioButtons('show_legend', '图例', c('显示', '隐藏'), inline = T, selected = '显示')),
                                     column(width = 3, selectInput(inputId = 'legend_in', label = i18n('图例位置'), c('坐标系外', '坐标系内'), selected = '坐标系外')),
                                     column(width = 5, uiOutput('legend_position'))
                            ),
                            fluidRow(column(width = 3, numericInput('title_size', '主标题大小', min = 0, max = 100, value = 20)),
                                     column(width = 3, numericInput('labs_title_size', '坐标轴标题大小', min = 0, max = 100, value = 15)),
                                     column(width = 3, numericInput('legend_title_size', '图例度标题大小', min = 0, max = 100, value = 15)),
                                     column(width = 3, numericInput('axis_font_size', '刻度文本大小', min = 0, max = 100, value = 10))),
                            br(),
                            h5(strong('颜色设置')),
                            
                            )

# middle panel tab 1 ----------
middle_panel_tab1 <- tabPanel(title = '图片&下载',
                              ## 使下载标签在有图片的时候显现
                              uiOutput(outputId = 'download_label'),
                              tags$div(
                                align = 'center',
                                tags$br(),
                                jqui_resizable(plotOutput(outputId = "boxplot", width = 400, height = 400))
                                ),
                              DTOutput(outputId = 'plot_data')
                              )

# middle panel tab 2 --------------------------------
middle_panel_tab2 <- tabPanel('分析方法',
                              tags$div(
                                align = 'center',
                                tags$br(),
                                tableOutput(outputId = 'methods')
                              )
)

# right panel tab 1 --------------------------------
right_panel_tab1 <- tabPanel('输入数据',
                             h4('绘图数据', align = 'center'),
                             DTOutput(outputId = 'data_output'),
                             h4('分组数据', align = 'center'),
                             DTOutput(outputId = 'group_output')
)

# right panel tab 2 --------------------------------
right_panel_tab2 <- tabPanel('使用教程',
                             div(
                               style="margin:0px 12px;max-height: 800px; overflow-y: auto;",
                               h4(align="center",tags$b("使用教程"))
                               )
                             )

# right panel tab 3 --------------------------------
right_panel_tab3 <- tabPanel('版本说明',
                             h4('v1.0.0'),
                             p('1. 该版本实现主要的Violin plot')
)

# body ------------------------------------
body <- dashboardBody(
  theme = shinytheme(theme="flatly"),
  style="background-color:white; font-size: 90%; ",##将全局默认背景改为白色
  tabItems(
    tabItem('Violinplot',
            fluidRow(column(width = 4, h4('参数'), tabsetPanel(left_panel_tab1, left_panel_tab2)),
                     column(width = 5, h4('结果与方法'), tabsetPanel(middle_panel_tab1, middle_panel_tab2)),
                     column(width = 3, h4('说明'), tabsetPanel(right_panel_tab1, right_panel_tab2, right_panel_tab3)),
                     style = "height:849px"
            )
    )
  )
)


# dashboard ---------------------------------------------------------------
ui <- dashboardPage(title = 'Violin Plot', skin = "blue", header = header, sidebar = sidebar, body = body)