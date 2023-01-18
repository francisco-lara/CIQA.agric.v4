#' @title arima_DF
#' @description hacer un arima y pronosticar h periodos
#'Los datos ser?n agrupados a nivel horario
#'La salida es un DF con h periodos de mas, lo cual
#'@param DF Data frame
#'@param h  numero de periodos a estimar
#'
arima_Graph<-function(DFI_completo, h)
{
  DFhor<-DFI_completo%>%group_by(year(Fecha),month(Fecha), day(Fecha), hour(Fecha))%>%mutate(Temperatura=mean(Temperatura))%>%mutate(Humedad_Relativa = mean(Humedad_Relativa))
  DF_hour<-unique(DFhor[2:7])
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
  DF_temp_ari$Media_HR<-data_fit4

  #Esta es la ultima fecha con datos observados
  ff
  which(DF_hourx$Fecha==ff, arr.ind = T)
  locy<-DF_hourx$Temperatura[which(DF_hourx$Fecha==ff, arr.ind = T)]
  yend =DF_temp_ari$Media_temp[which(DF_temp_ari$Fecha==fg, arr.ind = T)]

  nr<-nrow(DF_temp_ari)
  DF_star<-which(DF_temp_ari$Fecha == DF_temp_ari$Fecha[nr] - hours(h))
  DF_end<- which(DF_temp_ari$Fecha == DF_temp_ari$Fecha[nr])
  DF_imput<-DF_temp_ari[DF_star:DF_end,]


  Gx<-ggplot(data = DF_temp_ari, aes(x=Fecha, y=Temperatura))+
    geom_line(col = "darkgreen")+
    #geom_point(x=ff, y=locy, size=2, col="red", alpha=1/20, stroke=1)+
    geom_point(data=DF_imput, aes(), col = "blue")+
    #geom_line(data = DF_imput, aes(), col="red")+
    geom_smooth(data = DF_imput, aes())+
    #geom_segment(x=ff, y=locy, xend = fg, yend = yend)
    geom_text(x =DF_imput$Fecha[1] , y=DF_imput$Temperatura[1],
    label="Datos_Pronosticados")+
    theme(panel.background = element_rect(fill = "lightblue1"),
          plot.background = element_rect(fill = "aliceblue"))+
    annotate('text', label="Desarrollado por: CIQA-DBA", x= fi, y= min(DF_temp_ari$Temperatura),
             hjust=0.1, vjust=0)
    Gx

    return(Gx)
}




