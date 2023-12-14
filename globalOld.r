# script global.R

library(shiny)

# lectura de datos
  
datos22 <-read.csv("datos202212.csv",sep=';')
mean222<-aggregate(datos22, mean) 

anyo22 <-read.csv("datos202212.csv",sep=';')

str(anyo22)
#  vamos a limpiar quito provimcia y municipio

anyo22 <- select(anyo22,-PROVINCIA,-MUNICIPIO)

anyo22 <-anyo22[anyo22$MAGNITUD=='6'|anyo22$MAGNITUD=='8'|anyo22$MAGNITUD=='9'|anyo22$MAGNITUD=='10'|anyo22$MAGNITUD=='12'|anyo22$MAGNITUD=='14',]

str(anyo22)
# tenemos el prolblema que al hacer la media cuenta los valores 0
# vamos a reemplazar los valores 0.00 por NA ,ya que son valores que no estan medido y gracias a esto podemos hacer media

anyo22[anyo22==0.00] <-NA

anyo22<-anyo22 %>%
  mutate("media" = rowMeans (select(anyo22, c(6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66)),na.rm=TRUE))



### año 21 ###

anyo21 <-read.csv("datos202112.csv",sep=';')

str(anyo21)
#  vamos a limpiar quito provimcia y municipio

anyo21 <- select(anyo21,-PROVINCIA,-MUNICIPIO)

anyo21 <-anyo21[anyo21$MAGNITUD=='6'|anyo21$MAGNITUD=='8'|anyo21$MAGNITUD=='9'|anyo21$MAGNITUD=='10'|anyo21$MAGNITUD=='12'|anyo21$MAGNITUD=='14',]

str(anyo21)
# tenemos el prolblema que al hacer la media cuenta los valores 0
# vamos a reemplazar los valores 0.00 por NA ,ya que son valores que no estan medido y gracias a esto podemos hacer media

anyo21[anyo21==0.00] <-NA

anyo21<-anyo21 %>%
  mutate("media" = rowMeans (select(anyo21, c(6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66)),na.rm=TRUE))


#### añyo 19
anyo19 <-read.csv("datos201912.csv",sep=';')

str(anyo19)
#  vamos a limpiar quito provimcia y municipio

anyo19 <- select(anyo19,-PROVINCIA,-MUNICIPIO)

anyo19 <-anyo19[anyo19$MAGNITUD=='6'|anyo19$MAGNITUD=='8'|anyo19$MAGNITUD=='9'|anyo19$MAGNITUD=='10'|anyo19$MAGNITUD=='12'|anyo19$MAGNITUD=='14',]

str(anyo19)
# tenemos el prolblema que al hacer la media cuenta los valores 0
# vamos a reemplazar los valores 0.00 por NA ,ya que son valores que no estan medido y gracias a esto podemos hacer media

anyo19[anyo19==0.00] <-NA

anyo19<-anyo19 %>%
  mutate("media" = rowMeans (select(anyo19, c(6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66)),na.rm=TRUE))


#### año 20
anyo20 <-read.csv("datos202012.csv",sep=';')

str(anyo20)
#  vamos a limpiar quito provimcia y municipio

anyo20 <- select(anyo20,-PROVINCIA,-MUNICIPIO)

anyo20 <-anyo20[anyo20$MAGNITUD=='6'|anyo20$MAGNITUD=='8'|anyo20$MAGNITUD=='9'|anyo20$MAGNITUD=='10'|anyo20$MAGNITUD=='12'|anyo20$MAGNITUD=='14',]

str(anyo20)
# tenemos el prolblema que al hacer la media cuenta los valores 0
# vamos a reemplazar los valores 0.00 por NA ,ya que son valores que no estan medido y gracias a esto podemos hacer media

anyo20[anyo20==0.00] <-NA

anyo20<-anyo20 %>%
  mutate("media" = rowMeans (select(anyo20, c(6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66)),na.rm=TRUE))


## año 23
anyo23 <-read.csv("datos202309.csv",sep=';')
anyo23 <- select(anyo23,-PROVINCIA,-MUNICIPIO)

anyo23 <-anyo20[anyo23$MAGNITUD=='6'|anyo23$MAGNITUD=='8'|anyo23$MAGNITUD=='9'|anyo23$MAGNITUD=='10'|anyo23$MAGNITUD=='12'|anyo23$MAGNITUD=='14',]

str(anyo23)
# tenemos el prolblema que al hacer la media cuenta los valores 0
# vamos a reemplazar los valores 0.00 por NA ,ya que son valores que no estan medido y gracias a esto podemos hacer media

anyo23[anyo23==0.00] <-NA

anyo23<-anyo23 %>%
  mutate("media" = rowMeans (select(anyo23, c(6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66)),na.rm=TRUE))

## problema xq meto valores na en filas.. no se xq?
##############fin

#### datos globales


datos_global<-merge(anyo19,merge(anyo20,anyo21,all = TRUE),all = TRUE)

datos_global<-merge(datos_global,anyo22,all = TRUE)                    
                    
lineas2 <- datos_global[datos_global$ANO=='2019'& datos_global$ESTACION=='8' & datos_global$MAGNITUD=='10',]


# quitar valors nulos 

sapply(datos_global, function(x) sum(is.na(x)))


# idea esto ees unafucnoin
delete.na <- function(df, n=0) {
  df[rowSums(is.na(df)) <= n,]
}
 
#problema hya filas enteras na

 

###** por aqui##
###*
###*
###*
###*
######## preuba

dd <-datos_global[datos_global$ESTACION=='60',]
d2 <-dd[dd$ANO=='2019',]
d3 <-d2[d2$MAGNITUD=='10',]

plot(d3$MES, d3$media, type = "l")


suma22_agrupados <-
  suma22 %>% 
  group_by(ESTACION,MES) %>% 
  summarise("media2" = sum(media))

suma22_agrupados[suma22_agrupados$ESTACION=='60',]$media2
#esto es el año 22 con la columna 22)

 
  

#colMeans (df [, 1: 3]colMeans (df [, c (2, 3)])
datos21 <-read.csv("datos202112.csv",sep=';')
str(datos22)
nn <-suma22$MES='12'& suma22$ESTACION='4'
vars <- c(4,5) 

datos22_8<- suma22[suma22$MAGNITUD=='4',]
#cada estacion tiene varios puntos de muestra
ppp<- datos22_8[datos22_8$PUNTO_MUESTREO=='28079008_1_38',]$media

head(datos22)
datos22_Pm25<- datos22[datos22$MAGNITUD=='9',]
datos22_PM25_8<- datos22_Pm25[datos22_Pm25$ESTACION=='8',]
meses<-c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
plot(datos22_PM25_8$MES, datos22_PM25_8$D01, type = "l")

datos21_Pm25<- datos21[datos21$MAGNITUD=='9',]
datos21_PM25_8<- datos21_Pm25[datos21_Pm25$ESTACION=='8',] 

datos22_PM25_8_df = as.data.frame(datos22_PM25_8, header=FALSE)
datos21_PM25_8_df = as.data.frame(datos21_PM25_8, header=FALSE)

# Transpone todas las columnas menos la primer
df_transpose <- data.frame (t(datos22_PM25_8_df [-7]))
df_transpose_21 <- data.frame (t(datos21_PM25_8_df [-7]))
# Añadimos los nombres de las columnas
colnames(df_transpose) <- datos22_PM25_8_df [, 1]
colnames(df_transpose_21) <- datos21_PM25_8_df [, 1]

df_transpose
df_limpio <- df_transpose[c(7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,66,68),]
df_limpio_21 <- df_transpose_21[c(7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,66,68),]


dias <- (1:31)
dias[3]
df_completo_21 <- cbind(df_limpio_21,dias)

plot(df_completo$dias, df_completo[,3], type = "l")

#agregar dias a df transpose
valores_dias<- as.data.frame(x = (datos22_PM25_8[c("D01", "D02", "D03", "D04")]))
#lenght(valores_dias[1,])
#plot(lenght(4, valores_dias, valores_dias[1,])

paleta <- c("#999999", "#E69F00", "#56B4E9", "#009E73")
valores_dias[1,1]
colnames(valores_dias)
#library(ggplot2)
#ggplot(valores_dias, 
 #     aes(x=colnames(valores_dias), y=valores_dias[1,])) +
  #xlab("Valores") +
  #ylab("Promedio") +        
 #geom_point(size=5, colour=paleta)  
