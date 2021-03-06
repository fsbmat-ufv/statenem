#' @title Makes the cleaning of the Enem database
#' @name enemclear
#'
#' @description Function to do the cleaning and organization of the
#' Enem microdata
#'
#' @param year Year you want the microdata
#'
#'
#' @return the database of the year \code{year}.
#'
#' @author Fernando Bastos
#'
#'
#'
#' @export
enemclear <- function(year){
  if(year==2018){
    df <- "MICRODADOS_ENEM_2018.csv"
  } else {
    df <- "MICRODADOS_ENEM_2019.csv"
  }
  dados <- data.table::fread(input=df,
                             integer64='character',
                             skip=0, #Ler do inicio
                             nrow=-1, #Ler todos os registros
                             na.strings = "",
                             showProgress = TRUE, encoding = "Latin-1",
                             header = TRUE, select = c(1,2,4,6:11,16:18,20,83:86,91:94,99,104:137)) %>%
    dplyr::filter(TP_PRESENCA_CN=="1",TP_PRESENCA_CH=="1",TP_PRESENCA_LC=="1",TP_PRESENCA_MT=="1")
    dados$nt_final <- (rowSums(dados[,c("NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT", "NU_NOTA_REDACAO")]))/5
    dados <- dados[!is.na(dados$nt_final),]
    dados$nt_cat <- cut(dados$nt_final,
                      breaks=c(-Inf, 317.58, 452.735, 587.89, 723.045, 858.2),
                      labels=c("0" , "1", "2", "3", "4"))
    saveRDS(dados, file="dados2018.Rds")
    remove(dados)
    dados <- readRDS("dados2018.Rds")
    }
