#' @title data_lost
#' @description Crea una grafica de pastel que contiene el porcentaje de datos perdidos
#' @param DF Es un Data Frame
#' @return Grafico
#' @export data_lost
#' @examples
#' @import ggplot2

library(ggplot2)
#Crea una grafica de pastel que contiene el porcentaje de datos perdidos
data_lost<-function(DF)
{
  i<-DF$Fecha[1]
  f<-DF$Fecha[nrow(DF)]
  A<-data.frame (seq(from = i, to= f, by= "10 min"))
  colnames(A)<-"Fecha"
  DFx<- merge(x=A, y=DF, by= "Fecha", all=T)
  Rep<-is.na(DFx$Temperatura)
  Lec_t<-nrow(DFx)
  Error<-sum(Rep)
  #print (paste("Se perdieron", Error, "de", Lec_t, "Lecturas"))
  Lec_e<-(Error * 100) /Lec_t
  Lec_c<-100 - Lec_e

  DF_error<-data.frame(Categorias=c("Lecturas_correctas", "Lecturas_perdidas"),
                       Porcentaje=c(Lec_c, Lec_e))

  G1<-ggplot(data = DF_error, aes(x= "",y=Porcentaje, fill=Categorias))+
    geom_bar(stat = "identity",color="white")+
    coord_polar(theta="y")
  return (G1)
}
