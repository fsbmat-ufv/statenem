#' @title Compiles the shiny
#' @name runExample
#'
#' @description Function to generate the shiny
#'
#'
#' @return An interactive dashboard.
#'
#' @author Fernando Bastos
#'
#'
#'
#' @export
runExample <- function() {
  appDir <- system.file("shiny-examples", "myapp", package = "statenem")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
