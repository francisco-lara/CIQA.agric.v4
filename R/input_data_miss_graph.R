#' @title input_data_miss_graph
#' @description Esta funcioón grafica la imputación de datos asumiendo que  La perdida de datos es aleatoria
#' El m?todo para imputar es  predictive mean matching
#' de la libreria mice
#'
#' @param DF Data frame
#'
#' @return un data frame estructurado
#' @export input_data_miss_graph
#' @examples


##AHORA VAMOS A IMPUTAR DATOS
#####SUPUESTOS
##### La perdida de datos es aleatoria
##### El m?todo para imputar es  predictive mean matching
##### de la libreria mice
#####Esta funcion devuelve unicamente
####la grafica strip que contiene la
#####distribuci?n de los datos imputados

library(mice)
input_data_miss_graph<-function(DF)
{
  imput_data<-mice::mice(DF[,2:3], m=5, maxit=50,meth='pmm', seed = 1304)
  summary(imput_data)
  DFi<-complete(imput_data,1)
  G2<-mice::stripplot(imput_data, pch = 20)
  return(G2)
}
