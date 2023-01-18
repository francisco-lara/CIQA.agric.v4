#' @title temp_min
#' @description Grafica las temperaturas minimas de un DF estructurado
#' @param DF Data frame
#'


library(ggplot2)
library(dplyr)
library(lubridate)
temp_min <- function(DF)
{
D.min<-AR%>%group_by(year(Fecha),month(Fecha), day(Fecha))%>%mutate(Temp_min=min(Temperatura))
D.minx<-unique(D.min[4:7])

D.minxy<-unite (D.minx, "Fecha", c("year(Fecha)","month(Fecha)",
                                     "day(Fecha)"))

D.minxy$Fecha <-as.POSIXct(D.minxy$Fecha, format = "%Y_%m_%d")

Gx<-ggplot(data = D.minxy, aes(x=Fecha, y=Temp_min))+
  geom_point(col = "darkgreen")+
  geom_smooth()+
  theme(panel.background = element_rect(fill = "lightblue1"),
        plot.background = element_rect(fill = "aliceblue"))+
  annotate('text', label="Desarrollado por: CIQA-DBA", x= D.minxy$Fecha[1], y= min(D.minxy$Temp_min),
           hjust=0.1, vjust=0)
return (Gx)

}
