####Aqui iniciamos con la función de temperatura minima, media y maxima

temp_aire<-function(DF)
{
M<-DF

M<-AR%>%group_by(year(Fecha),month(Fecha), day(Fecha))%>%mutate(Temp_Max=max(Temperatura))%>%
  mutate(Temp_min=min(Temperatura))
M$Temp_Max<-ifelse(M$Temp_Max == M$Temperatura, M$Temperatura, NaN)
M$Temp_min<-ifelse(M$Temp_min == M$Temperatura, M$Temperatura, NaN)

G<-ggplot(data = M)+geom_line(aes(x= Fecha, y= Temperatura))+
  geom_point(aes(x=Fecha, y=Temp_Max), col="red")+
  geom_label(aes(label=round(Temp_Max,2), x=Fecha+0.2, y=Temperatura+0.2),fill="red", alpha=0.1)+
  geom_point(aes(x=Fecha, y=Temp_min), col="blue")+
  geom_label(aes(label=round(Temp_min,2), x=Fecha-0.2, y=Temperatura-0.2), fill="blue", alpha=0.1)+
  labs(x="Fecha", y="Temperatura", title="Desarrollado por CIQA")+
  theme(plot.title=element_text(hjust = 0))
return (G)
}

