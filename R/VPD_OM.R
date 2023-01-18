
VPD_OM<-function(DF)
{
Temperatura = -10
Humedad_relativa <- 10
C1<-0.61078
C2<-17.2694
C3<-238.3
e<-2.71828183
es<-C1*(e^(C2*Temperatura/(C3+Temperatura)))
ea<-(es*Humedad_relativa)/100
VPD<-es-ea

DP<-AR%>%group_by(year(Fecha),month(Fecha), day(Fecha), hour(Fecha))%>%
  mutate(es = C1*(e^(C2*Temperatura/(C3+Temperatura))))%>%
  mutate(ea=(es*Humedad_relativa)/100)%>%mutate(VPD=es-ea)
DFx<-data.frame(DP$Fecha, DP$Temperatura, DP$Humedad_relativa,DP$VPD)
colnames(DFx)<-c("Fecha", "Temperatura", "Humedad_relativa", "VPD")
return(DFx)
}

