fileUI <- function(id, label = "Choose JSON files with benchmark results") {
  ns <- NS(id)
  tagList(
    h2("Files"),
    textInput(
      inputId = ns("file"),
      label = label,
      value = "",
      #      multiple = TRUE,
      placeholder = "Choose JSONs with benchmark results"
    )
  )
}

benchmarkConfigUI <- function(id, label = "Set benchmark setup") {
  ns <- NS(id)
  tagList(
    h2("Benchmark setup"),
    selectInput(
      inputId = ns("dim"),
      label = "Dimensionality",
      choices = c(10, 30, 50, 100)
    ),
    numericInput(
      inputId = ns("rep"),
      label = "Repetitions",
      value = 51,
      min = 1,
      step = 1
    ),
    numericInput(
      inputId = ns("probs"),
      label = "Functions",
      value = 1,
      min = 1,
      step = 1
    ),
    selectInput(
      inputId = ns("cec"),
      label = "CEC version",
      choices = c("CEC-2017", "CEC-2013")
    )
  )
}

plotUI <- function(id, label = "Plot type") {
  ns <- NS(id)
  tagList(
    h2("Plot type"),
    radioButtons(
      inputId = "plot_type",
      label = "Type of plot",
      choices = c("ECDF per class" = "class", "ECDF per problem" = "problems")
    )
  )
}

dataUI <- function(id, label = "Data") {
  ns <- NS(id)
  wellPanel(
    fileUI(ns("files")),
    benchmarkConfigUI(ns("config"))
  )
}

#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  fluidPage(
    theme = "superhero.css",
    titlePanel("ECDF plotter"),
    sidebarLayout(
      sidebarPanel(
        dataUI("ecdf_data"),
        plotUI("plot"),
        conditionalPanel(
          condition = "input.plot_type === 'ECDF per problem'",
          shinyWidgets::numericRangeInput(
            inputId = "problems",
            label = "Set problems",
            value = c(1, 30)
          )
        ),
        actionButton(
          inputId = "plotButton",
          label = "Plot",
          icon = icon("paint-brush"),
          width = "100%",
        )
      ),
      mainPanel(
        plotOutput("ecdf_plot")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "xx"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
