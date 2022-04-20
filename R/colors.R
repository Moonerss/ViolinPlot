# function used in shiny
#' @import esquisse
#'
palette_ui <- function(id) {
  ns <- NS(id)
  pals <- esquisse:::get_palettes()
  tagList(
    tags$style(
      ".bootstrap-select .dropdown-menu li a span.text { display: block !important; }"
    ),
    radioButtons(
      inputId = ns("type"),
      label = NULL,
      choiceValues = c("palette", "manual"),
      choiceNames = tagList(
        tags$div(
          tags$b("使用色板"),
          palettePicker(
            inputId = ns("palette"),
            label = NULL,
            choices = pals$choices,
            textColor = pals$textColor,
            pickerOpts = list(container = "body")
          ),
          checkboxInput(
            inputId = ns("reverse"),
            value = FALSE,
            label = "Reverse colors?"
          )
        ),
        tags$div(
          tags$b("使用特定颜色"),
          br(),
          uiOutput(outputId = ns("manual"))
        )
      )
    )
  )
}


palette_server <- function(id, all_samples) {
  ## palettes names
  palettes <- esquisse:::get_palettes()
  palettes <- palettes$choices
  palettes <- unlist(palettes, recursive = FALSE, use.names = TRUE)
  names(palettes) <- gsub("^.+\\.", "", names(palettes))

  callModule(
    id = id,
    function(input, output, session) {
      ns <- session$ns
      ##
      colors_manual <- reactiveValues(x = NULL, type = "discrete")
      output$manual <- renderUI({
        req(all_samples)
        ## get colors
        unique_sample <- sort(unique(all_samples))
        colors <- colorRampPalette(palettes[[input$palette]])(length(unique_sample))
        colors_id <- paste0("colors_", esquisse:::makeId(unique_sample))
        ## set muanual color
        colors_manual$x <- setNames(as.list(colors_id), unique_sample)
        # colors_manual$type <- "discrete"
        ## set colors in different row
        all_cut <- split(unique_sample, ceiling(seq_along(unique_sample) / 4))
        all_colors_id <- split(colors_id, ceiling(seq_along(colors_id) / 4))
        all_colors <- split(colors, ceiling(seq_along(colors) / 4))
        purrr::pmap(
          .l = list(all_cut, all_colors_id, all_colors),
          .f = function(x, y, z) {
            tags$div(
              style = "width: 150%;",
              do.call(
                fluidRow,
                lapply(
                  X = seq_along(x),
                  FUN = function(i) {
                    column(
                      width = 3,
                      tagList(
                        tags$span(
                          strong(x[i]),
                          tagAppendAttributes(
                            colorPickr(
                              inputId = ns(y[i]),
                              selected = z[i],
                              label = NULL,
                              theme = "classic",
                              # useAsButton = TRUE,
                              update = "save",
                              interaction = list(
                                hex = FALSE,
                                rgba = FALSE,
                                input = TRUE,
                                save = TRUE,
                                clear = FALSE
                              )
                            ),
                            style = "display: inline; vertical-align: middle;"
                          )
                        ),
                        tags$br()
                      )
                    )
                  }
                )
              )
            )
          }
        )
      })
      ## update colors
      observeEvent(colors_manual$type,
        {
          pals <- esquisse:::get_palettes()
          esquisse:::updatePalettePicker(
            inputId = "palette",
            choices = pals$choices,
            textColor = pals$textColor,
            selected = isolate(input$palette)
          )
        },
        ignoreInit = TRUE
      )

      ## return result
      return(
        reactive({
          if (identical(input$type, "palette")) {
            list(
              scale = "palette",
              reverse = input$reverse,
              colors = input$palette
            )
          } else {
            ids <- colors_manual$x
            list(
              scale = "manual",
              type = colors_manual$type,
              colors = lapply(
                X = ids,
                FUN = function(x) {
                  input[[x]]
                }
              )
            )
          }
        })
      )
    }
  )
}
