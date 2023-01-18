#Función para hacer un arima y pronosticar h periodos
#El argumento de entrada es un DF estructurado
#Los datos ser?n agrupados a nivel horario
#La salida es un DF con h periodos de mas, lo cual
#incrementa el tama?o del DF en h filas
#h se requiere pasar como par?metro
#' @title arima_DF
#' @description hacer un arima y pronosticar h periodos
#'Los datos ser?n agrupados a nivel horario
#'La salida es un DF con h periodos de mas, lo cual
#'@param DF Data frame
#'@param h  numero de periodos a estimar
#'
library(forecast)

arima_DF<-function(DFI_completo, h)
{
  DFhour<-DFI_completo%>%group_by(year(Fecha),month(Fecha), day(Fecha), hour(Fecha))%>% mutate(Temperatura=mean(Temperatura))%>%mutate(Humedad_Relativa = mean(Humedad_Relativa))
  DF_hour<-unique(DFhour[2:7])
  DF_hourx<-unite (DF_hour, "Fecha", c("year(Fecha)","month(Fecha)",
                                       "day(Fecha)","hour(Fecha)"))
  DF_hourx$Fecha <-as.POSIXct(DF_hourx$Fecha, format = "%Y_%m_%d_%H")
  DF_testh<-ts(DF_hourx)
  ar_h<-forecast::auto.arima(DF_testh[,1])
  Pred<-forecast::forecast(ar_h, h=h)

  ar_hr<-forecast::auto.arima(DF_testh[,2])
  Pred_hr<-forecast::forecast(ar_hr, h=h)
  Gxy<-forecast::autoplot(Pred_hr)
  Gxy

  str(DF_hourx)
  fi<-DF_hourx$Fecha[1]
  ff<-DF_hourx$Fecha[nrow(DF_hourx)]
  fg<-ff + hours(h)
  DF_a<-data.frame(seq(from = fi, to= fg, by= "1 hour"))
  colnames(DF_a)<-"Fecha"
  fit3<-data.frame(seq(from = ff+hours(1), to= ff+hours(h), by= "1 hour"),Pred[["mean"]][1:h])
  colnames(fit3)<-c("Fecha","Temperatura")
  data_fit<-c(Pred$fitted[1:length(Pred$fitted)], fit3$Temperatura)
  DF_temp_ari<-cbind(DF_a, data_fit)
  colnames(DF_temp_ari)<-c("Fecha", "Temperatura")

  fit4<-data.frame(seq(from = ff+hours(1), to= ff+hours(h), by= "1 hour"),Pred_hr[["mean"]][1:h])
  colnames(fit4)<-c("Fecha", "Humedad_Relativa")
  data_fit4<-c(Pred_hr$fitted[1:length(Pred_hr$fitted)], fit4$Humedad_Relativa)
  DF_temp_ari$Humedad_relativa <-data_fit4
  return(DF_temp_ari)
}
