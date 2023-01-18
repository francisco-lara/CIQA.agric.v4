
#' @title data_lost_vis
#' @description #Esta funci?n genera una grafica de visualizaci?n de datos
#perdidos por variable con la intenci?n de verificar la existencia
#de patrones en la perdida de los datos
#'
#' @param DF Data frame
#'
#' @return un data frame estructurado
#' @export data_lost_vis
#' @examples

#Esta funci?n genera una grafica de visualizaci?n de datos
#perdidos por variable con la intenci?n de verificar la existencia
#de patrones en la perdida de los datos
#El argumento de entrada es un DF estructurado
library(VIM)
data_lost_vis<-function(DF)
{
  i<-DF$Fecha[1]
  f<-DF$Fecha[nrow(DF)]
  A<-data.frame (seq(from = i, to= f, by= "10 min"))
  colnames(A)<-"Fecha"
  DFx<- merge(x=A, y=DF, by= "Fecha", all=T)
  aggr_plot <- aggr(DFx, col=c('navyblue','red'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(data),
                    cex.axis=.7, gap=3,
                    ylab=c("Histogram de datos perdidos","Pattern"))

}
