library(forecast)
library(fpp2)
library(ggplot2)
library(nortest)
###analisis de residuales y correlogramas
##base goog200
#residuales por codigo, tambien se pueden obtener con $ al aplicar naive
res1<-residuals(naive(goog200))
autoplot(res1)+xlab("Día")+ylab("Valor")
gghistogram(res1)+ggtitle("Histograma de residuos")
ggAcf(res1)#correlograma, nos permite ver si los residuos estan correlacionados, si pasa la frontera
#quiere decir que no pasa la prueba, si pasa tiene autocorrelacion(mide la relacion entre 1 dato con
#datos anteriores).
#Ruido blanco: tiene que ver con la autocorrelacion, aleatorieidad, la serie de tiempo tiene ruido
#blanco si tiene media 0 y varianza constante. En muchos casos la serie con ruido blanco nos indica
#que sera un buen pronostico.
####databases
pob<-read.csv("C://Users//lenovo//Downloads//pob.csv")#1960
pib<-read.csv("C://Users//lenovo//Downloads//pib.csv")#1961
televisa<-read.csv("C://Users//lenovo//Downloads//televisa.csv")#2018
####reordenando
pob[(1:58),1]<-pob[(58:1),1]
pob[(1:58),2]<-pob[(58:1),2]
pib[(1:57),1]<-pib[(57:1),1]
pib[(1:57),2]<-pib[(57:1),2]
televisa[(1:272),1]<-televisa[(272:1),1]
televisa[(1:272),2]<-televisa[(272:1),2]
#serie de tiempo
pobts<-ts(pob$Poblacion,start = 1960,frequency = 1)
pibts<-ts(pib$Crecimiento,start = 1961,frequency = 1)
televisats<-ts(televisa$Precio,start = 2018,frequency = 272)
autoplot(pobts)+
  ggtitle("Serie de tiempo Poblacion 1960-2017")+xlab("Año")+ylab("Serie")+
  theme(panel.background = element_rect(fill = "white",colour = "blue"),panel.grid = element_blank())
#crecimiento lineal muy marcado
autoplot(pibts)+
  ggtitle("Serie de tiempo PIB 1960-2017")+xlab("Año")+ylab("Serie")+
  theme(panel.background = element_rect(fill = "white",colour = "red"),panel.grid = element_blank())
#aleatoreidad en el crecimiento, tal vez sea una serie estacionaria
autoplot(televisats)+ggtitle("Serie de tiempo Televisa 2018-2019")+xlab("Año")+ylab("Serie")+
  theme(panel.background = element_rect(fill = "white",colour = "yellow"),panel.grid = element_blank())
#tendencia a la baja muy marcada
#pronostico (naive)
npob<-naive(pobts)
npib<-naive(pibts)
ntel<-naive(televisats)
#residuales
autoplot(npob$residuals)+ggtitle("Residuos ST poblacion")+xlab("Año")+ylab("Escala")+
  theme(panel.background = element_rect(fill = "white",colour = "darkblue"),panel.grid = element_blank())
#residuales muy lineales y abruptos en el año 2000
autoplot(npib$residuals)+ggtitle("Residuos ST PIB")+xlab("Año")+ylab("Escala")+
  theme(panel.background = element_rect(fill = "white",colour = "red"),panel.grid = element_blank())
#muy cercano a ser ruido blanco, sin embargo dudo que pase las pruebas
autoplot(ntel$residuals)+ggtitle("Residuos ST Televisa")+xlab("Año")+ylab("Escala")+
  theme(panel.background = element_rect(fill = "white",colour = "yellow"),panel.grid = element_blank())
#casi se puede apreciar estacionariedad
#normalidad de los residuos
lillie.test(npob$residuals)#p-value < 2.2e-16, por lo tanto no tiene normalidad
lillie.test(npib$residuals)#p-value = 0.08943, tiene normalidad al 95%
lillie.test(ntel$residuals)#p-value = 0.03713, tiene normalidad al 99%
#histogramas
gghistogram(npob$residuals)+ggtitle("Histograma residuos ST poblacion")+xlab("Año")+ylab("Escala")+
  theme(panel.background = element_rect(fill = "white",colour = "darkblue"),panel.grid = element_blank())
#sin comentarios, no va a pasar la prueba
gghistogram(npib$residuals)+ggtitle("Histograma residuos ST poblacion")+xlab("Año")+ylab("Escala")+
  theme(panel.background = element_rect(fill = "white",colour = "red"),panel.grid = element_blank())
#Tal vez pase la prueba
gghistogram(ntel$residuals)+ggtitle("Histograma residuos ST poblacion")+xlab("Año")+ylab("Escala")+
  theme(panel.background = element_rect(fill = "white",colour = "yellow"),panel.grid = element_blank())
#la serie con mejor histograma de las 3
#correlogramas
ggAcf(npob$residuals)+ggtitle("Correlograma residuos ST poblacion")+xlab("Año")+ylab("Escala")+
  theme(panel.background = element_rect(fill = "white",colour = "darkblue"),panel.grid = element_blank())
#tiene ruido blanco
ggAcf(npib$residuals)+ggtitle("Residuos ST poblacion")+xlab("Año")+ylab("Escala")+
  theme(panel.background = element_rect(fill = "white",colour = "red"),panel.grid = element_blank())
#tienen autocorrelacion los residuos, tal vez con una prueba de outliers pase
ggAcf(ntel$residuals)+ggtitle("Residuos ST poblacion")+xlab("Año")+ylab("Escala")+
  theme(panel.background = element_rect(fill = "white",colour = "yellow"),panel.grid = element_blank())
#tienen autocorrelacion los residuos, tal vez con una prueba de outliers pase






