#' @title Faz a leitura do banco de dados do Enem
#' @name runExample
#'
#' @description Funcao para gerar o shiny
#'
#'
#' @details Utilize este campo para escrever detalhes mais tecnicos da
#'     sua funcao (se necessario), ou para detalhar melhor como
#'     utilizar determinados argumentos.
#'
#' @return o banco de dados do ano \code{ano}.
#'
#' @author Fernando Bastos
#'
#'
#' @examples
#' soma(2, 2)
#'
#' x <- 3
#' y <- 4
#' soma(x = x, y = y)
#'
#' @export
runExample <- function() {
  appDir <- system.file("shiny-examples", "myapp", package = "statenem")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
