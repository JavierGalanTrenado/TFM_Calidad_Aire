library(shiny)
library(fpp2)
require(gridExtra)
library(readxl)
library(tidyverse)
library(readxl)
library(forecast)
library(lubridate)
library(tseries)
# Cargamos la libreria plotly
library(plotly) ##  
library(zoo)

##### FUNCIONES #############  
## funcion que limpia valores No validos N
limpia_valoresN<- function(anyox2) {
  
  for (i in 1:length(anyox2[,1])) 
    # codigo
    for (j in 7:67)
    {
      if (j%% 2==0)
      {
        next
      }
      if (anyox2[i,j]=="N")
        
      { anyox2[i,(j-1)]=NA
      }
   
    }
  #####
  
  return(anyox2)
} 

## lectura de datos ###

## año22
anyo22 <-read.csv("datos202212.csv",sep=';')
anyoxx <-read.csv("datos202212.csv",sep=';')
str(anyo22)
#  vamos a limpiar quito provimcia y municipio
anyoxx <- select(anyoxx,-PROVINCIA,-MUNICIPIO)
anyo22 <- select(anyo22,-PROVINCIA,-MUNICIPIO)

anyo22 <-anyo22[anyo22$MAGNITUD=='6'|anyo22$MAGNITUD=='8'|anyo22$MAGNITUD=='9'|anyo22$MAGNITUD=='10'|anyo22$MAGNITUD=='12'|anyo22$MAGNITUD=='14',]
anyoxx <-anyoxx[anyoxx$MAGNITUD=='6'|anyoxx$MAGNITUD=='8'|anyoxx$MAGNITUD=='9'|anyoxx$MAGNITUD=='10'|anyoxx$MAGNITUD=='12'|anyoxx$MAGNITUD=='14',]

####
 



anyo22 <-limpia_valoresN(anyo22)



anyo22<-anyo22 %>%
  mutate("media" = rowMeans (select(anyo22, c(6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,
                                              38,40,42,44,46,48,50,52,54,56,58,60,62,64,66)),na.rm=TRUE))



### año 21 ###

anyo21 <-read.csv("datos202112.csv",sep=';')

str(anyo21)
#  vamos a limpiar quito provincia y municipio

anyo21 <- select(anyo21,-PROVINCIA,-MUNICIPIO)

anyo21 <-anyo21[anyo21$MAGNITUD=='6'|anyo21$MAGNITUD=='8'|anyo21$MAGNITUD=='9'|anyo21$MAGNITUD=='10'|anyo21$MAGNITUD=='12'|anyo21$MAGNITUD=='14',]

str(anyo21)
anyo21 <-limpia_valoresN(anyo21)
anyo21<-anyo21 %>%
  mutate("media" = rowMeans (select(anyo21, c(6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66)),na.rm=TRUE))


#### añyo 19
anyo19 <-read.csv("datos201912.csv",sep=';')

anyo19 <- select(anyo19,-PROVINCIA,-MUNICIPIO)

anyo19 <-anyo19[anyo19$MAGNITUD=='6'|anyo19$MAGNITUD=='8'|anyo19$MAGNITUD=='9'|anyo19$MAGNITUD=='10'|anyo19$MAGNITUD=='12'|anyo19$MAGNITUD=='14',]

 

anyo19 <-limpia_valoresN(anyo19)


anyo19<-anyo19 %>%
  mutate("media" = rowMeans (select(anyo19, c(6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66)),na.rm=TRUE))


#### año 20
anyo20 <-read.csv("datos202012.csv",sep=';')

str(anyo20)
#  vamos a limpiar quito provimcia y municipio

anyo20 <- select(anyo20,-PROVINCIA,-MUNICIPIO)

anyo20 <-anyo20[anyo20$MAGNITUD=='6'|anyo20$MAGNITUD=='8'|anyo20$MAGNITUD=='9'|anyo20$MAGNITUD=='10'|anyo20$MAGNITUD=='12'|anyo20$MAGNITUD=='14',]

str(anyo20) 
anyo20 <-limpia_valoresN(anyo20)
 

anyo20<-anyo20 %>%
  mutate("media" = rowMeans (select(anyo20, c(6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66)),na.rm=TRUE))


## año 23
anyo23 <-read.csv("datos202309.csv",sep=';')
anyo23 <- select(anyo23,-PROVINCIA,-MUNICIPIO)

anyo23 <-anyo23[anyo23$MAGNITUD=='6'|anyo23$MAGNITUD=='8'|anyo23$MAGNITUD=='9'|anyo23$MAGNITUD=='10'|anyo23$MAGNITUD=='12'|anyo23$MAGNITUD=='14',]

anyo23<-anyo23 %>%
  mutate("media" = rowMeans (select(anyo23, c(6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66)),na.rm=TRUE))

 

## año 18
anyo18 <-read.csv("datos201812.csv",sep=';')
anyo18 <- select(anyo18,-PROVINCIA,-MUNICIPIO)

anyo18 <-anyo18[anyo18$MAGNITUD=='6'|anyo18$MAGNITUD=='8'|anyo18$MAGNITUD=='9'|anyo18$MAGNITUD=='10'|anyo18$MAGNITUD=='12'|anyo18$MAGNITUD=='14',]

anyo18<-anyo18 %>%
  mutate("media" = rowMeans (select(anyo18, c(6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66)),na.rm=TRUE))

 
anyo17 <-read.csv("datos17.csv",sep=';')
anyo17 <- select(anyo17,-PROVINCIA,-MUNICIPIO)

anyo17 <-anyo17[anyo17$MAGNITUD=='6'|anyo17$MAGNITUD=='8'|anyo17$MAGNITUD=='9'|anyo17$MAGNITUD=='10'|anyo17$MAGNITUD=='12'|anyo17$MAGNITUD=='14',]

anyo17<-anyo17 %>%
  mutate("media" = rowMeans (select(anyo17, c(6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66)),na.rm=TRUE))

 


##############fin
 

 
 

#### datos globales

datos_global  <- rbind(anyo20,anyo21, anyo22,anyo19,anyo23,anyo17,anyo18)

                   
                    
 


# quitar valores nulos 

sapply(datos_global, function(x) sum(is.na(x)))


 


# cargamos estaciones calidad aire

estaciones <-read.csv("estaciones_calidad.csv",sep=';',encoding = "UTF-8")

str(estaciones)




est <-   read_xls('calidad_aire.xls')

est ['LONGITUD']<-as.numeric(unlist(est ['LONGITUD']))
est ['LATITUD']<-as.numeric(unlist(est ['LATITUD']))

puntos<-cbind(est$LONGITUD , est$LATITUD, est$CODIGO_CORTO,est$ESTACION)
puntos2<-as.data.frame(puntos)

puntos2$V1<-as.numeric(puntos2$V1)
puntos2$V2<-as.numeric(puntos2$V2)
puntos2
datos_punto<-puntos2[(puntos2$V3==4),]



estaciones_nombre<-est$ESTACION
 

est[est$ESTACION=="Villaverde",]$CODIGO_CORTO

### para magnitudes
codigos <- c("12","08","14","06","10","09")
Magnitud <- c("Oxidos de Nitrogeno","Dioxido Nitrogeno","Ozono","Monoxido Carbono", "PM10","PM 2.5")

labels <- c("ene", "feb", "mar","abr","may","jun","jul","agos","sept","oct","nov","dic")
 
Magnitud_DF <- data.frame(codigos,Magnitud)
Magnitud_DF$Magnitud

dd<- as.numeric(Magnitud_DF[Magnitud_DF$Magnitud=="Ozono",]$codigos)
 
 