#' @title DF_str
#' @description Función que estructura información leida
#' de una direcciín IP proporcionada por el usuario.
#' @param IP IP del dispositivo telemétrico
#'
#' @return un data frame estructurado
#' @export DF_str
#' @examples
#' @import tidyr
#' @import dplyr
#' @import magrittr
#' @import lubridate


library(tidyr)
library(dplyr)
library(magrittr)
library(lubridate)
DF_str <-function(IP)
{
  D<-read.table(IP, sep = ",", header = F)
  Dx<-D%>%select(V1,V3,V5, V7,V9,V11,V13)
  colnames(Dx)<-c("Fecha", "Temperatura", "Humedad_Relativa", "R", "G", "B","C")
  Dx$Fecha<-dmy_hms(Dx$Fecha)
  return(Dx)
}
