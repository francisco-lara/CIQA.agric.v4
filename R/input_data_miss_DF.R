#' @title input_data_miss_DF
#' @description sta funcion devuelve un DF con los datos imputados
#' @param DF Data frame
#'
#' @return un data frame con datos imputados
#' @export input_data_miss_DF


####Esta funcion devuelve un DF con los datos imputados
####por el metodo predictive mean matching de
####la libreria mice
input_data_miss_DF<-function(DF)
{
  Ax<-mice::mice(DF[,2:3], m=5, maxit=50,meth='pmm', seed = 1304)
  summary(Ax)
  DFi<-complete(Ax,1)
  DFI_completo<-cbind(DF$Fecha,DFi)
  colnames(DFI_completo)<-c("Fecha", "Temperatura", "Humedad_Relativa")
  DFI_completo$Fecha<- DFI_completo$Fecha - 59

  i<-DFI_completo$Fecha[1]
  f<-DFI_completo$Fecha[nrow(DFI_completo)]
  A<-data.frame (seq(from = i, to= f, by= "1 hour"))
  colnames(A)<-"Fecha"


  DFhour<-DFI_completo%>%group_by(year(Fecha),month(Fecha), day(Fecha), hour(Fecha))%>% mutate(Temperatura=mean(Temperatura))%>%mutate(Humedad_Relativa = mean(Humedad_Relativa))
  DF_hour<-unique(DFhour[2:7])
  DF_hourx<-unite (DF_hour, "Fecha", c("year(Fecha)","month(Fecha)",
                                       "day(Fecha)","hour(Fecha)"))
  DF_hourx$Fecha <-as.POSIXct(DF_hourx$Fecha, format = "%Y_%m_%d_%H")
  DFx<- merge(x=DF_hourx, y=A, by= "Fecha", all=T)

  DFxy<-mice::mice(DFx[,2:3], m=5, maxit=50,meth='pmm', seed = 1304)
  DFxyz<-complete(DFxy,1)
  Fecha<-DFx$Fecha
  DF_inx<-cbind(Fecha, DFxyz)

  return(DF_inx)
}
