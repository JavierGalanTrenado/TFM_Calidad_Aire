library(shiny)
library(leaflet)
library(dplyr)
library(shinycssloaders)
library(rgdal)
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
library(ggplot2)
library(shinythemes)
source("./global.R")

 


ui <- tagList( 
  
  shinythemes::themeSelector(),
  #navegacion para ui
  
  navbarPage("Calidad del aire", theme = shinytheme("cerulean"),
            
             
             
             
             tabPanel("Dashboard Interactivo",
                      
                     
                        
                  sidebarPanel(
                  h4("Creacion de dashboard historico de principales magnitudes "),
      
                   selectInput("estacion", 
                  label = h3("Elige estacion"),
                  choices = estaciones_nombre,
                  selected = "Villaverde"),
                  
                 selectInput("magnitud", 
                  label = h3("Elige magnitud"),
                  choices = Magnitud_DF$Magnitud,
                   selected = "Ozono"),
                 selectInput("annos", 
                             label = h3("Elige años comparar"),
                             choices = c( "2017", "2018","2019","2020","2021","2022","2023"),
                             
                           selected = "2017", multiple = TRUE),
                            width = "2"),
  
  
  #https://shiny.posit.co/r/gallery/application-layout/shiny-theme-selector/
  #https://epirhandbook.com/es/dashboards-with-shiny.html
  
            mainPanel(
                  tabsetPanel(
                    tabPanel("Historicos"),
                            h4("Graficos Historicos"),
                    
                    fluidRow( column( width = 6,h4("Comparativa Años", align = 'center'), plotlyOutput("grafico") ),
                              column( width = 6,h4("Evolucion contaminante en Estacion", align = 'center'), plotlyOutput("grafico_barras") )
                    ),
                    
                    fluidRow( column( 2,width=12,h4("Estaciones de Control", align = 'center'), (leafletOutput(outputId = "map", height = "300px", width="100%")) ),
                             
                    ),
                    
                    )
            
                            
             
             
 
                      )
                  ), #tab panel

#####
#     ),
            
#segunda pestaña
tabPanel("Predicciones_Diversos_Modelos",  
         
         
         sidebarPanel(
           helpText("Modelos  Predictivos "),
           
           selectInput("estacionPREC", 
                       label = "Elige estacion",
                       choices = estaciones_nombre,
                       selected = "Villaverde"),
           
           selectInput("magnitudPREC", 
                       label = "Elige magnitud",
                       choices = Magnitud_DF$Magnitud,
                       selected = "Ozono"),
           radioButtons("model", "Model to select",
                        choices = c( "ARIMA", "NeuralNet"),
                        choiceValues = "ARIMA"), width = "2"
           
           ),
         mainPanel(
           tabsetPanel(
             tabPanel(h6("Prediccion en el tiempo"),
           
           plotOutput("predictiva")),
           tabPanel(h6("Datos Exploratorios"),
                  fluidRow( column( 2,width=12,h4("Grafico exploratorio", align = 'center'), (plotOutput("graficoserie") ))) ,
                  fluidRow( column( 2,width=12,h4("Grafico tendencias", align = 'center'), (plotOutput("trendEstad") ))) ,
                  
                    )
           
          
           )
         )
         
         
         
         
)


        ), #de nav

 





) #del ui


#***********************server ################
#*
server <- function(input, output){
  #source("./global.R")   
  
  ## funciones Reactive
  dataInput <- reactive({input$estacion})
  
  estPREC <- reactive({estacionselectedPREC=est[est$ESTACION==input$estacionPREC,]$CODIGO_CORTO})
  magniPREC <-reactive({as.numeric(Magnitud_DF[Magnitud_DF$Magnitud==input$magnitudPREC,]$codigos)})
  datosPREC <- reactive({datos_global[datos_global$ESTACION==estPREC() & datos_global$MAGNITUD==magniPREC(),]}) 
  
  
  
  anoselected = reactive({input$anyo})
  estacionselected=reactive({est[est$ESTACION==dataInput(),]$CODIGO_CORTO})
  magnitudselected=reactive({as.numeric(Magnitud_DF[Magnitud_DF$Magnitud==input$magnitud,]$codigos)})
  datoshistselected=reactive({datos_global[datos_global$ESTACION==estacionselected() & datos_global$MAGNITUD==magnitudselected(),]})
  
  
  #Renderizado del control de mapa con leaftlet..
  
  output$map <- renderLeaflet(
   
    {
     
      estacionpunto=est[est$ESTACION==dataInput(),]$CODIGO_CORTO
      datos_punto2=puntos2[(puntos2$V3==estacionpunto),]
 
     
     
     leaflet() %>%  addTiles() %>% setView( lng = -3.68444444,lat = 40.45861111, zoom =10)  %>% 
     addMarkers(lng =datos_punto2$V1,lat =datos_punto2$V2,label =datos_punto2$V4  ) %>% 
     addCircles(lng = puntos2$V1, lat = puntos2$V2,color="blue",radius = 3, 
                fillOpacity = 0.5, label = puntos2$V4)
                 

    
    
        })
  #ieste output no hace nada.
  
 # output$num <- renderText({
  #  paste(input$anyo,input$estacion,input$magnitud)
    
#  })
  
  
 
  
  ### graficos pestaña 1:comparativa años 
 
 
  
  ## grafico comparativa años
  output$grafico <- renderPlotly ({
   
     lineas <- datoshistselected()
     
     if (nrow(lineas)==0)
     {
       
      ggplotly(plot(0:5, 0:5, type = "n", xlab = "", ylab = ""))
       text(2, 5, "   NO EXISTEN DATOS ",  col = "red", font = 6, cex = 1.5)
       
     }  
     
     if (nrow(lineas)>0)
     {
       anos_selc=input$annos
       lineas2 = subset(lineas,ANO %in% anos_selc )
       
      ggplotly(lineas2 %>% ggplot(aes(x = MES, y = media, group=ANO, color = ANO))+
         geom_line(size = 0.8)+
         geom_point(size = 1.8) +  scale_x_continuous(breaks = seq(1, 12, by = 1),labels = labels))
      
     }
  
  })
  ## grafico de evolucion contaminante en estacion
  
 
  output$grafico_barras <- renderPlotly ({
   
    
    datoscompara=({datos_global[datos_global$ESTACION==estacionselected() & datos_global$MAGNITUD==magnitudselected(),]})
    
    
    if (nrow(datoscompara)==0)
    {
      
     ggplotly( plot(0:5, 0:5, type = "n", xlab = "", ylab = ""))
      text(2, 5, "   NO EXISTEN DATOS ",  col = "red", font = 6, cex = 1.5)
      
    }  
    
    if (nrow(datoscompara)>0)
    {
    
    
    datoscompara= datoscompara %>%
      mutate("fecha" =  paste (datoscompara$ANO, datoscompara$MES,sep = "-"))
    
   datoscompara$fecha = as.yearmon(datoscompara$fecha,format = "%Y-%m")
    
    
    
    datoscompara = datoscompara %>%
      mutate(
    periodo = case_when( 
      datoscompara$fecha >= as.yearmon("2021-09",format="%Y-%m") ~ "Madrid 360",
      datoscompara$fecha < as.yearmon("2021-09",format="%Y-%m") & datoscompara$fecha >= as.yearmon("2018-11",format="%Y-%m") ~ "Madrid Central",
      TRUE ~ "SIN PLAN CALIDAD"
                      )
          )
    
    
    # ploteo graficos segun periodo
    
    ggplotly(ggplot(datoscompara, aes(x = fecha, y = media,color=periodo )) +
      geom_line() +
      geom_point())
    
    }
  })
  
  
  
 
  
  ##### grafico 2nda pestaña, predicciones
 
##############pestaña prediccion
   

  
  
  output$graficoserie<- renderPlot({   
    
    
    
    dat_serie <- datosPREC()
    
    if (nrow(dat_serie)==0)
    {
      
      plot(0:5, 0:5, type = "n", xlab = "", ylab = "")
      text(2, 5, "   NO EXISTEN DATOS ",  col = "red", font = 6, cex = 1.5)
      
    } 
    
    if (nrow(dat_serie)>0)
    {
      
    Y_serie <- ts(dat_serie[,"media"], start = c(2019,1), end = c(2022,12), frequency = 12)
    
    autoplot(Y_serie,frequency=12,xlab="Años",ylab="media",main="Analisis Exploratorio") 
    }
    
  })
  
  
   

   output$trendEstad <- renderPlot({   
  
  dat_pred_est <- datosPREC()
  
  if (nrow(dat_pred_est)==0)
  {
    
    plot(0:5, 0:5, type = "n", xlab = "", ylab = "")
    text(2, 5, "   NO EXISTEN DATOS ",  col = "red", font = 6, cex = 1.5)
    
  } 
  
  if (nrow(dat_pred_est)>0)
  {
    
  
  Y_PREC_est <- ts(dat_pred_est[,"media"], start = c(2019,1), end = c(2022,12), frequency = 12)
  
  descomp_est = decompose(Y_PREC_est)
  par(mfrow = c(2, 2)) #Se utiliza para dividir la ventana gráfica en una matriz de 2 filas y 2 columnas
  plot(descomp_est$x, main = "Datos-Original", col = "black", ylab = "Serie de tiempo")
  plot(descomp_est$trend, main = "Tendencia", col = "blue", ylab = "Valores")
  plot(descomp_est$seasonal, main = "Estacionalidad", col = "red", ylab = "Valores")
  plot(descomp_est$random, main = "Irregularidad", col = "green", ylab = "Valores")
  
  
}
  
})

   

output$predictiva <- renderPlot({
 
  
  
  datos_PREC <- datosPREC()
  
  if (nrow(datos_PREC)==0)
  {
    
    plot(0:5, 0:5, type = "n", xlab = "", ylab = "")
    text(2, 5, "   NO EXISTEN DATOS ",  col = "red", font = 6, cex = 1.5)
    
  } 
  
  if (nrow(datos_PREC)>0)
  {
    
  
   
  Y_PREC <- ts(datos_PREC[,"media"], start = c(2019,1), end = c(2022,12), frequency = 12)
  
  
   
  
  if (input$model == "ARIMA"){
     
     
        mod <- auto.arima(Y_PREC, trace = T, stepwise = F, approximation =F, max.d = 0)
                      
    } else {
     mod <- nnetar(Y_PREC)
    
      }
  data <- forecast(mod,12,level=95)
  
   autoplot(data)+autolayer(fitted(data), series="Ajuste")
  
  
  }
})





   
  
}#fin del server

shinyApp(ui, server)

