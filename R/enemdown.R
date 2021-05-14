#' @title Faz o download do banco de dados do Enem
#' @name enemdown
#'
#' @description Funcao para fazer o download dos
#' microdados do Enem
#'
#' @param ano Ano que voce deseja os microdados
#'
#' @details Utilize este campo para escrever detalhes mais tecnicos da
#'     sua funcao (se necessario), ou para detalhar melhor como
#'     utilizar determinados argumentos.
#'
#' @return o banco de dados do ano \code{ano}.
#'
#' @author Fernando Bastos
#'
#' @importFrom utils download.file unzip
#'
#' @examples
#' soma(2, 2)
#'
#' x <- 3
#' y <- 4
#' soma(x = x, y = y)
#'
#' @export
enemdown <- function(ano){
  if (ano=="2018") {
    url <- "https://download.inep.gov.br/microdados/microdados_enem2018.zip"
    download.file(url, destfile = "microdadosenem2018.zip")
    unzip("microdadosenem2018.zip", overwrite = TRUE,  junkpaths = TRUE, files = "DADOS/MICRODADOS_ENEM_2018.csv")

  } else{
    if (ano=="2019") {
      url <- "https://download.inep.gov.br/microdados/microdados_enem2019.zip"
      download.file(url, destfile = "microdadosenem2019.zip")
      unzip("microdadosenem2019.zip", overwrite = TRUE,  junkpaths = TRUE, files = "DADOS/MICRODADOS_ENEM_2019.csv")
    } else {
    cat("Não há como baixar de outros anos")
  }
  }}
