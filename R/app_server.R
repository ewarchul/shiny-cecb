#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  ecdf_data <- dataServer("ecdf_data")
  ecdf_plot <- reactiveValues(
    plot = NULL
  )
  observeEvent(input$plotButton, {
    ecdf_plot$plot <- cecb::ecdf_plot(ecdf_data())
  })
  output$ecdf_plot <- renderPlot({
    if (is.null(ecdf_plot$plot)) {
      return()
    }
    ecdf_plot$plot
  })
}

fileServer <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      filepath <- reactive({
        input$file
      })
      return(filepath)
    }
  )
}

configServer <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      config <- reactive({
        list(
          dim = input$dim,
          prob = input$probs,
          rep = input$rep
        )
      })
      return(config)
    }
  )
}

dataServer <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      file <- fileServer("files")
      config <- configServer("config")
      data <- reactive({
        cecb::get_dfr(
          idpaths = file(),
          config = config()
        )
      })
      return(data)
    }
  )
}
