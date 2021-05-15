#' @title Faz a limpeza do banco de dados do Enem
#' @name enemclear
#'
#' @description Funcao para fazer a limpeza e organizacao dos
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
enemclear <- function(ano){
  df <- if(ano==2018){
    df <- "MICRODADOS_ENEM_2018.csv"
  } else {
    df <- "MICRODADOS_ENEM_2019.csv"
  }
  dados <- data.table::fread(input=df,
                             integer64='character',
                             skip=0, #Ler do inicio
                             nrow=10000, #Quantidade de registros a serem lidos
                             na.strings = "",
                             showProgress = TRUE, encoding = "Latin-1",
                             header = TRUE, select = c(1,2,4,6:11,16:18,20,83:86,91:94,99,104:137)) %>%
    filter(TP_PRESENCA_CN=="1",TP_PRESENCA_CH=="1",TP_PRESENCA_LC=="1",TP_PRESENCA_MT=="1")
}
