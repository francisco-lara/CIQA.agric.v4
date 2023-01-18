#Esta funci?n calcula el periodo de tiempo
#transcurrido entre el inicio de la toma
#de datos y el final

#' @title Time_dif
#' @description Imprime el tiempo transcurrido desde que se encendio el sistema telemetrico
#' @param DF Un DF cuya primera columna se llame FECHA y contenga datos posixct
#' @return Una impresión con el tiempo transcurrido
#' @export Time_dif
#' @examples
#'

Time_dif<-function(DF)
{
  i<-DF$Fecha[1]
  f<-DF$Fecha[nrow(DF)]
  int<-f-i
  print (int)

}
