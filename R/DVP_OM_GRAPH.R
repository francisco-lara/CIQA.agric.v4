
VPD_OM_GRAPH<-function(AR)
{
Temperatura = -10
Humedad_relativa <- 10
C1<-0.61078
C2<-17.2694
C3<-238.3
e<-2.71828183
es<-C1*(e^(C2*Temperatura/(C3+Temperatura)))
ea<-(es*Humedad_relativa)/100


DP<-AR%>%group_by(year(Fecha),month(Fecha), day(Fecha), hour(Fecha))%>%
  mutate(es = C1*(e^(C2*Temperatura/(C3+Temperatura))))%>%
  mutate(ea=(es*Humedad_relativa)/100)%>%mutate(VPD=es-ea)
DFx<-data.frame(DP$Fecha, DP$Temperatura, DP$Humedad_relativa,DP$VPD)
colnames(DFx)<-c("Fecha", "Temperatura", "Humedad_relativa", "VPD")

DFxy<-DFx%>%group_by(year(Fecha),month(Fecha), day(Fecha))%>%
  mutate(VPD_Min=min(VPD))%>%
  mutate(VPD_Max=max(VPD))
DFxy$VPD_Min<-ifelse(DFxy$VPD_Min == DFxy$VPD, DFxy$VPD, NaN)
DFxy$VPD_Max<-ifelse(DFxy$VPD_Max == DFxy$VPD, DFxy$VPD, NaN)

Gx<-ggplot(data = DFxy) + geom_line(aes(x= Fecha, y= VPD))+
  geom_point(aes(x=Fecha, y=VPD_Max), col="red")+
  geom_label(aes(label=round(VPD_Max,2), x=Fecha+0.05, y=VPD+0.05),fill="red", alpha=0.1)+
  geom_point(aes(x=Fecha, y=VPD_Min), col="blue")+
  geom_label(aes(label=round(VPD_Min,2), x=Fecha-0.05, y=VPD-0.05),fill="blue", alpha=0.1)+
  labs(title="Desarrollado por CIQA")+geom_hline(aes(yintercept = 1.8,  col="red"),linetype='dashed')+
  theme(plot.title=element_text(hjust = 0))+scale_color_discrete(name="", labels="Valor Crítico")

return(Gx)
}
