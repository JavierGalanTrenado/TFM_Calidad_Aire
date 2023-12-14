#install.packages("magrittr")
#install.packages("vctrs")

library(shiny)
library(leaflet)
library(dplyr)
library(shinycssloaders)
library(rgdal)
#library(plotly)
library(htmltools)
library(DT)
library(shinyjs)
library(data.table)
library(leaflet)
library(geojsonio)
library(dygraphs)
library(xts)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(forecast)
library(lubridate)
library(tseries)
#library(shiny)
#ui <-  fluidPage(
#https://rstudio.github.io/shinydashboard/get_started.html
library(ggplot2)
library(shinythemes)
source("./global.R")

button_color_css <- "{
#DivCompClear, #FinderClear, #EnterTimes{
# Change the background color of the update button
#to blue. *
background: DodgerBlue;

/* Change the text size to 15 pixels. */
font-size: 15px;
}"


ui <- tagList( 
  
  shinythemes::themeSelector(),
  #navegacion para ui
  
  navbarPage("Calidad del aire", theme = shinytheme("cerulean"),
            
             
             
             
             tabPanel("Historico",
                      
                      # Sidebar layout with a input and output definitions
                      
                        
                  sidebarPanel(
                  helpText("Creacion de dashboard historico de principales magnitudes "),
      
                   selectInput("estacion", 
                  label = "Elige estacion",
                  choices = estaciones_nombre,
                 # choices = c("4", 
                  #            "8","60"
                   #            ),
                  selected = "Villaverde"),
                  
                 selectInput("magnitud", 
                  label = "Elige magnitud",
                  choices = Magnitud_DF$Magnitud,
                  #choices = c("12", 
                  #            "8","10"
                  #),
                  selected = "Ozono"),
    
      
                  width = "2"),
  
  
  #https://shiny.posit.co/r/gallery/application-layout/shiny-theme-selector/
  #https://epirhandbook.com/es/dashboards-with-shiny.html
  
            mainPanel(
                  tabsetPanel(
                    tabPanel("tab1",
                            h4("h4"),
             #  textOutput("num"),
                            plotOutput("grafico"),
                            (leafletOutput(outputId = "map", height = "500px", width="100%"))
              #leafletOutput(outputId = "map", height = "500px", width="100%")
  # hay que ver que quersmo ver para pintarlos abajo en el server        
  # )
  #)
                            ),
                            )
 
                      )
                  ), #tab panel
#### aqui segunda pestaña
            tabPanel("Predicciones_ARIMA",  


              sidebarPanel(
              helpText("Creacion de dashboard historico de principales magnitudes "),
  
              selectInput("estacionARI", 
                          label = "Elige estacion",
                         choices = estaciones_nombre,
                         selected = "Villaverde"),
  
               selectInput("magnitudARI", 
                          label = "Elige magnitud",
                          choices = Magnitud_DF$Magnitud,
                          selected = "Ozono"),
  
              width = "2"),


              mainPanel(
                tabsetPanel(
                  tabPanel("Predicciones 2",
                         h6("el grafico puede tardar en cargar"),
                      plotOutput("grafico_ARIMA"),
                         textOutput("num")),
                tabPanel("Predicciones 3",
                           )),
             # ),
                ),
#https://shiny.posit.co/r/gallery/application-layout/shiny-theme-selector/

####
 
 

#####
            ),
            
#segunda pestaña
tabPanel("Predicciones_Diversos_Modelos",  
         
         
         sidebarPanel(
           helpText("Creacion de dashboard historico de principales magnitudes "),
           
           selectInput("estacionPREC", 
                       label = "Elige estacion",
                       choices = estaciones_nombre,
                       selected = "Villaverde"),
           
           selectInput("magnitudPREC", 
                       label = "Elige magnitud",
                       choices = Magnitud_DF$Magnitud,
                       selected = "Ozono"),
           radioButtons("model", "Model to select",
                        choices = c( "ARIMA", "naive","NeuralNet"),
                        choiceValues = "ARIMA"), width = "2"
           
           ),
         mainPanel(
           tabsetPanel(
             tabPanel(h6("Evolucion temporal"),
           
           plotOutput("trendPlot")),
           tabPanel(h6("Serie"),
                    
                    )
           
          
           )
         )
         
         
         
         
)


        ), #de nav

#############tercera pstaña











##############






) #del ui

server <- function(input, output){
  source("./global.R")  ## da error x algo
  
  
  dataInput <- reactive({
    input$estacion
  })
  
  
  #Renderizado del control de mapa con leaftlet..
  
  output$map <- renderLeaflet(
   
    {
     
      estacionpunto=est[est$ESTACION==dataInput(),]$CODIGO_CORTO
      datos_punto2=puntos2[(puntos2$V3==estacionpunto),]
 
     
     
     leaflet(mapa_estaciones) %>%  addTiles() %>% setView( lng = -3.68444444,lat = 40.45861111, zoom =11)  %>% 
      addCircleMarkers(lng =datos_punto2$V1,lat =datos_punto2$V2 , color = "red",radius = 10,  popup =datos_punto2$V4) 

   # leaflet() %>%  addTiles() %>% setView( lng = -3.68444444,lat = 40.45861111, zoom =11)  %>%
    #  addMarkers(lng =puntos2$V1,lat =puntos2$V2 , popup =puntos2$V4) 
    
    
        })
  #idea meter las estaciones medicion
  
  output$num <- renderText({
    paste(input$anyo,input$estacion,input$magnitud)
    
  })
  
  
 
  
  ### grafico pestaña 1, serie interesante meter selector de años a comparar
 
  
  output$grafico <- renderPlot ({
   
    
    anoselected = input$anyo
    #estacionselected=input$estacion
    estacionselected=est[est$ESTACION==dataInput(),]$CODIGO_CORTO
     #magnitudselected=input$magnitud
    magnitudselected=as.numeric(Magnitud_DF[Magnitud_DF$Magnitud==input$magnitud,]$codigos)
    
     
    
     lineas <- datos_global[datos_global$ESTACION==estacionselected & datos_global$MAGNITUD==magnitudselected,]
     #ggplot(lineas3) + geom_line(aes(MES, media, group = ANO, color = ANO), alpha = 0.8) 
     
     if (nrow(lineas)==0)
     {
       
       plot(0:5, 0:5, type = "n", xlab = "", ylab = "")
       text(2, 5, "   NO EXISTEN DATOS ",  col = "red", font = 6, cex = 1.5)
       
     }  
     
     if (nrow(lineas)>0)
     {
      ggplot(lineas) + geom_line(aes(MES, media, group = ANO, color = ANO),alpha = 0.8)  
     }
  
  })
  ## grafico ARIMA
  
 
  
  
  
  
 
  
  #####
  
#  event_trigger <- reactive({
 #   list(input$estacionARI,input$magnitudARI)
#  })
  
#pensar en quitar esto   
  
 output$grafico_ARIMA <- renderPlot ({
   
    ### prueba ARIMA en selector ####
    
    estacionselectedARI=est[est$ESTACION==input$estacionARI,]$CODIGO_CORTO
    #magnitudselected=input$magnitud
    magnitudselectedARI=as.numeric(Magnitud_DF[Magnitud_DF$Magnitud==input$magnitudARI,]$codigos)
    
    
    
    
    datos_ARI <- datos_global[datos_global$ESTACION==estacionselectedARI & datos_global$MAGNITUD==magnitudselectedARI,]
    
    
    
    
    if (nrow(datos_ARI)==0)
    {
      
      plot(0:5, 0:5, type = "n", xlab = "", ylab = "")
      text(2, 5, "   NO EXISTEN DATOS ",  col = "red", font = 6, cex = 1.5)
      
    }  
    
    if (nrow(datos_ARI)>0)
    {
      
    
    
    #vemos que tenemos la evolucion de ozono en estacion..
    # tenbemos en nuestro dataset columnas de año y mes y nuestra media de la magnitud
    
    #Ahora vamos a transformar nuestro conjunto de datos en una serie temporal que asignaremos a una variable Y, indicando el periodo y la frecuencia = 12 (meses), a la que vamos a someter a estudio. La variable objetivo a pronosticar sera el “Índice general”
    #El resultado nos dara la transformacion de nuestros datos originales a formato “ts”, que nos indica que es una serie temporal.
    
    Y_ARI <- ts(datos_ARI[,"media"], start = c(2017,1), end = c(2023,9), frequency = 12)
    
 #   withProgress(message = 'Creating plot', value = 0.1, {
 #     Sys.sleep(0.25)}) revisar bien que hace
    
    #vamos a ver visualmente los datos
    
    #  autoplot(Y) +
    #   ggtitle("indice de ozono") +
    #  ylab("magnitud")
    
    
    ## descomponemos grfico en 3 : seasonal, trend y resisudos
    
    # descomp = decompose(Y)
    #  autoplot(descomp)
    
    
    # adf.test(Y)
    
    # es estacionaria
    
    #descomp = decompose(ts(datos_ARI[,"media"], start = c(2017,1), end = c(2023,9), frequency = 12))
    # p2<-plot(descomp)
    
     model_ARI <- auto.arima(Y_ARI, trace = T, stepwise = F, approximation =F, max.d = 0)
    
    # print(model)
    
    #checkresiduals(model)
    #-Vamos a hacer ahora una estimacion de este modelo, haciendo un forecast en el que vamos a predecir el valor de las ventas para los proximos 8 meses con un nivel de significancia de un 95%.
    
      mod1_ARI <- forecast(model_ARI, 12, level = 95)
    
    #mod1
     
    #poner ultimo y para sumar luego
      last_Y_ARI = length(Y_ARI) 
    #last_Y
     # pred_mod1_ARI = mod1_ARI$mean
    # pred_mod1
      #pron_real_ARI = (pred_mod1_ARI) + last_Y_ARI
    
    # vamos a ver prediccion
    #pron_real
    
     #pron_real_ts_ARI = ts(pron_real_ARI, start = c(2023,10), frequency=12)
      plot(mod1_ARI)
    #dygraph(mod1_ARI)
    # idea poner dos plots.. es q lo que cacluclo en un sitio no puedo reutilzar
    #  pron_real_ts
    # vemos original mas predicha
    
    #ts.plot(Y_ARI, pron_real_ts_ARI, lty = c(1,3), col = c(5,2))
   
    
    #grid.arrange(p2,p1, ncol=1,top="titulo" )
    # dygraph(graf_inter)
   # ts.plot(Y_ARI, pron_real_ts_ARI, lty = c(1,3), col = c(5,2))
    
    }

}
)
##############pestaña prediccion

output$trendPlot <- renderPlot({
 
  ######
  
  
  
  ####
  estacionselectedPREC=est[est$ESTACION==input$estacionPREC,]$CODIGO_CORTO
  #magnitudselected=input$magnitud
  magnitudselectedPREC=as.numeric(Magnitud_DF[Magnitud_DF$Magnitud==input$magnitudPREC,]$codigos)
  
  
  
  
  datos_PREC <- datos_global[datos_global$ESTACION==estacionselectedPREC & datos_global$MAGNITUD==magnitudselectedPREC,]
  
  Y_PREC <- ts(datos_PREC[,"media"], start = c(2019,1), end = c(2022,12), frequency = 12)
  
  
  
 #~# library(fpp2)
  #require(gridExtra)
   
  
  if (input$model == "naive"){
    mod <- naive(Y_PREC,h=10,level=95)
     
    
   } else if (input$model == "ARIMA"){
      mod <- auto.arima(Y_PREC, trace = T, stepwise = F, approximation =F, max.d = 0)
                      
    } else {
     mod <- nnetar(Y_PREC)
    
      }
  data <- forecast(mod,12,level=95)
  #p2 <- autoplot(forecast(mod, 12)) + 
   # ggtitle("Forecast for next 12 meses")
  
 # grid.arrange(p2, ncol=1)
  
 # last_Y_PREC = Y_PREC[48]
  #last_Y
  #pred_mod1_PREC = data$mean
  # pred_mod1
  #pron_real_PREC = (pred_mod1_PREC) + last_Y_PREC
  
  # vamos a ver prediccion
  #pron_real
  
  #pron_real_ts_PREC = ts(pron_real_PREC, start = c(2023,1), frequency=12)
  plot(data)
  #  pron_real_ts
  # vemos original mas predicha
  
  #ts.plot(Y_ARI, pron_real_ts_ARI, lty = c(1,3), col = c(5,2))
  
  
 # pron_total= ts(Y_PREC, pron_real_ts_PREC)
  # dygraph(graf_inter)
  
 # par(mfrow=c(1,2))
 # ts.plot(Y_PREC, pron_real_ts_PREC, lty = c(1,3), col = c(5,2))
 # p2 <- autoplot(pron_total, lty = c(1,3), col = c(5,2)) 
 
  
  
  # dd= decompose(Y_PREC)
   #autoplot(dd)
    
    
  
  # p2 <- autoplot(ts.plot(Y_PREC, pron_real_ts_PREC, lty = c(1,3), col = c(5,2)))
     #grid.arrange(p2,p1, ncol=1,top="titulo" )
  
  
  
  
})





   
  
}#fin del server

shinyApp(ui, server)

