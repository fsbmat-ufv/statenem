#' @title Download the Enem database
#' @name enemdown
#'
#' @description Function to download the
#' Enem microdata
#'
#' @param year Year you want the microdata
#'
#' @return the database of the year \code{year}.
#'
#' @author Fernando Bastos
#'
#' @importFrom utils download.file unzip
#'
#'
#' @export
enemdown <- function(year){
  if (year=="2018") {
    url <- "https://download.inep.gov.br/microdados/microdados_enem2018.zip"
    download.file(url, destfile = "microdadosenem2018.zip")
    unzip("microdadosenem2018.zip", overwrite = TRUE,  junkpaths = TRUE, files = "DADOS/MICRODADOS_ENEM_2018.csv")

  } else{
    if (year=="2019") {
      url <- "https://download.inep.gov.br/microdados/microdados_enem2019.zip"
      download.file(url, destfile = "microdadosenem2019.zip")
      unzip("microdadosenem2019.zip", overwrite = TRUE,  junkpaths = TRUE, files = "DADOS/MICRODADOS_ENEM_2019.csv")
    } else {
    cat("Não há como baixar de outros anos")
  }
  }}
