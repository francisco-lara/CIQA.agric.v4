library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

UT<-function(AR)
{

Temp_Base<-7
Flor<-2500 #Si florea a finales de marzo o principios de abril entonces
          #a partir de enero se deben de contar como 2200 a 3000 UT hasta las
          #Flores

M<-AR%>%select(Fecha, Temperatura)%>%
  group_by(year(Fecha),month(Fecha), day(Fecha))%>%
  mutate(Temp_Media=(mean(Temperatura-Temp_Base)))%>%summarise(UT=mean(Temp_Media))
Mx<-data.frame(M)
Mx$Fecha<-paste(Mx$year.Fecha., Mx$month.Fecha., Mx$day.Fecha.,  sep="-")
Mz<-data.frame(Mx$Fecha, Mx$UT)
colnames(Mz)<-c("Fecha", "UT")
Mz$Fecha<-ymd(Mz$Fecha)
Mz$UT.acum<-cumsum(Mz$UT)

xi<-ymd("2023-1-5")
xf<-ymd("2023-6-30")
yi<-0
yf<-3000

Xsi<-max(Mz$Fecha)
Xsf<-max(Mz$Fecha)

Ysi<-max(Mz$UT.acum)
Ysf<-min(Mz$UT.acum)

xnn<-min(Mz$Fecha)
Gx<-ggplot(data = Mz)+geom_line(aes(x=Fecha, y=UT.acum))+
  xlim(c(xi,xf))+ylim(yi,yf)+
  geom_point(aes(x=max(Fecha), y=max(UT.acum)),col="red")+
  geom_label(aes(label=round(max(UT.acum),2), x=max(Fecha)+3, y=max(UT.acum)+3))+
  geom_segment(aes(x=Xsi, xend=Xsf, y=Ysi, yend=Ysf), linetype = 2, col="blue")+
  geom_hline(yintercept = Flor, linetype = 2, col="blue")+
  annotate("text", x=xnn,y=Flor+100, label = "Floración")+
  labs(title="Desarrollado por CIQA")+
  theme(plot.title=element_text(hjust = 0))
return (Gx)
}
