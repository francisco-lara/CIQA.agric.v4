library(dplyr)
library(lubridate)
library(ggplot2)

UF<-function(AR)
{
Tem_base<-18
UF<-AR%>%group_by(year(Fecha),month(Fecha), day(Fecha))
UF$U.Frio<-Tem_base - ifelse(UF$Temperatura<=Tem_base, UF$Temperatura, NaN)
UF$U.Frio[is.nan(UF$U.Frio)]<-0
UF$U.Frio<-cumsum(UF$U.Frio)

Gx<-ggplot(data=UF)+geom_line(aes(x=Fecha, y=U.Frio))
return (Gx)
}
