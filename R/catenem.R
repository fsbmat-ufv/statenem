#' @title Faz a limpeza do banco de dados do Enem
#' @name catenem
#'
#' @description Funcao para fazer a limpeza e organizacao dos
#' microdados do Enem
#'
#' @param dados banco de dados gerado apos a limpeza
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
catenem <- function(dados){
  dados$nt_final <- (rowSums(dados[,c("NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT", "NU_NOTA_REDACAO")]))/5
  dados <- dados[!is.na(dados$nt_final),]
  summary(dados$nt_final)
  dados$nt_cat <- cut(dados$nt_final,
                      breaks=c(-Inf, 317.58, 452.735, 587.89, 723.045, 858.2),
                      labels=c("0" , "1", "2", "3", "4"))
  }
