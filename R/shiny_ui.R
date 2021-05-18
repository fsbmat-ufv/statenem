#' The Shiny App UI.
#' @export
shiny_ui <- function() {
  fluidPage(
    titlePanel('Shiny Parameter Test'),
    verbatimTextOutput('environment'),
    tableOutput('thedata')
  )
}
