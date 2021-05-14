#' @title Faz o download do banco de dados do Enem
#' @name readenem
#'
#' @description Funcao para fazer a leitura dos
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
#'
#' @examples
#' soma(2, 2)
#'
#' x <- 3
#' y <- 4
#' soma(x = x, y = y)
#'
#' @export
readenem <- function(ano){
  df <- if(ano==2018){
    df <- "MICRODADOS_ENEM_2018.csv"
  } else {
    df <- "MICRODADOS_ENEM_2019.csv"
  }
  dados <- data.table::fread(input=df,
                             integer64='character',
                             skip=0, #Ler do inicio
                             nrow=1000, #Quantidade de registros a serem lidos
                             na.strings = "",
                             showProgress = TRUE, encoding = "Latin-1",
                             header = TRUE)
}
