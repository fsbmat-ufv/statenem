#' @title Compiles the shiny
#' @name runShinyApp
#'
#' @description Function to generate the shiny
#'
#'
#' @return An interactive dashboard.
#'
#' @author Fernando de Souza Bastos
#'
#'
#'
#'@export
runShinyApp <- function() {
  appDir <- system.file("shiny-examples", "myapp", package = "statenem")
  options(year = year)
  shiny::shinyAppDir(appDir)
}

