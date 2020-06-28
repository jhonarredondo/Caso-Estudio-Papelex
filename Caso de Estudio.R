###Shiny------------------------------------------------------------------------------
library(readxl)
library(qcc)
library(shiny)
library(shinythemes)
library(qcc)
library(SixSigma)
library(ggplot2)

Longitud <-read_excel("www/Caso de Estudio Papelex.xlsx", 
                                                  sheet = "Longitud")
Longitud<-as.matrix(Longitud[,2:8])

Peso<-read_excel("www/Caso de Estudio Papelex.xlsx", 
                            sheet = "Peso")
Peso<-as.matrix(Peso[,2])

Defectuosos<-read_excel("www/Caso de Estudio Papelex.xlsx", 
                        sheet = "Defectuosos")
P<-as.data.frame(Defectuosos[,2])
MuestraP <- as.matrix(Defectuosos[,3])
MuestraP <- as.integer(MuestraP)

Textura<-read_excel("www/Caso de Estudio Papelex.xlsx", 
                        sheet = "Rugosidades")
Rugosidades<-as.data.frame(Textura[,2])
MuestraP <- as.matrix(Textura[,3])
MuestraP <- as.integer(MuestraP)

RR<-read_excel("www/Caso de Estudio Papelex.xlsx", 
               sheet = "R&R")

RR2<-read_excel("www/Caso de Estudio Papelex.xlsx", 
                sheet = "R&R2")


##Cartas Arregladas---------------------------------
Longitud2 <-read_excel("www/Caso de Estudio Papelex.xlsx", 
                       sheet = "Longitud2")
Longitud2<-as.matrix(Longitud2[,2:8])

Defectuosos2<-read_excel("www/Caso de Estudio Papelex.xlsx", 
                         sheet = "Defectuosos 2")
P2<-as.data.frame(Defectuosos2[,2])
MuestraP2 <- as.matrix(Defectuosos2[,3])
MuestraP2 <- as.integer(MuestraP2)

Textura2<-read_excel("www/Caso de Estudio Papelex.xlsx", 
                     sheet = "Rugosidades 2")
Rugosidades2<-as.data.frame(Textura2[,2])
MuestraP2 <- as.matrix(Textura2[,3])
MuestraP2 <- as.integer(MuestraP2)

ui <- shinyUI(
  
  navbarPage(title="Jhon Esteban Arredondo Parra",theme = shinytheme("cerulean"), 
             tabPanel(
               h1("Caso de Estudio: Papelex",style="color:ivory",align="center"),
             hr(),
             sidebarPanel(
               fluidRow( #segunda fila
                 column(img(src="UdeA.png",width="350px",height="350px"), width = 11, align="center")
               ),
               hr(),
               br(),
               fluidRow( #segunda fila
                 column(img(src="Papel.jpg",width="350px",height="300px"), width = 11, align="center"))
               ),
             
             mainPanel(

               h4(span("Pasos Implementados", style="color:chocolate"),align="center"),
               tabsetPanel(type = "tabs",
                           tabPanel("AMEF", icon = icon("bezier-curve"), 
                                    plotOutput("Ishikawa"),
                                    br(),
                                    imageOutput("AMEF1"),align="center"),
                           
                           tabPanel("Cartas de Control Por Variables", icon = icon("chart-area"), 
                                    plotOutput("CartaR"),
                                    br(),
                                    plotOutput("CartaX"),
                                    br(),
                                    plotOutput("CartaIndividual")),
                           
                           tabPanel("Cartas de Control Por Atributos", icon = icon("chart-line"), 
                                    plotOutput("CartaP"),
                                    br(),
                                    plotOutput("CartaU")),
                           
                           tabPanel("Distribuciones", icon = icon("chart-bar"),
                                    plotOutput("HistogramaL"),
                                    plotOutput("HistogramaP"),
                                    plotOutput("HistogramaD"),
                                    plotOutput("HistogramaR")),
                           
                           tabPanel("R&R", icon = icon("check-square"), 
                                    plotOutput("RRLargo",height = "700px"),
                                    hr(),
                                    h3("Tras conseguir un nuevo Flexometro:"),
                                    imageOutput("Salida"),
                                    br(),
                                    plotOutput("RRLargo2",height = "700px"),
                                    br()),
                           
                           tabPanel("Muestreo", icon = icon("search"),
                                    imageOutput("Flujograma")),
                           
                           tabPanel("Cartas Corregidas V.", icon = icon("sync-alt"),
                                    plotOutput("CartaR2"),
                                    br(),
                                    plotOutput("CartaX2")),
                           
                           tabPanel("Cartas Corregidas A.", icon=icon("sync-alt"),
                                    plotOutput("CartaP2"),
                                    br(),
                                    plotOutput("CartaU2")),
                           
                           tabPanel("Capacidad del Proceso", icon = icon("signal"),
                                    plotOutput("CapacidadX"),
                                    br(),
                                    h2("A continuacion, los indices de capacidad por atributos:", align="center"),
                                    textOutput("CP"),
                                    textOutput("CP2"),
                                    textOutput("CP3"),
                                    br(),width = 6, style="background-color:azure;border-right:8px solid bluesky")
               )#Fin TabsetPanel
             )#Fin Mainpanel
             )#Tabpanel
  )#navbar
)#ui

server2 <- shinyServer(function(input, output) {
  
  output$Ishikawa <- renderPlot({
    #Ishikawa-----------------------
    H<-c("Falta de Capacitacion", "Desatencion", "Fatiga")                     
    Mq<-c("Equipos Descalibrados", "Maquinas Saturadas")
    MP<-c("Materia Prima desgastada","Almacenamiento Inadecuado")
    Medio<-c("Demanda", "Ruido Externo")
    Mt<-c("Falta Estandarizacion", "Espacio Inadecuado")
    Md<-c("Falta Inspeccion")                     
    Efecto<-"Quejas Cliente"                    
    CE<-cause.and.effect(cause=list(Hombre=H,Maquinaria=Mq, Material=MP, Entorno=Medio,
                                Metodo=Mt,Medida=Md), effect=Efecto)
    CE
  })
  
  output$AMEF1 <- renderImage({ #AMEF------------------------------
    
    filename <- normalizePath(file.path('www/AMEF1.png'))
    list(src = filename)
  }, deleteFile = FALSE)
  
  output$CartaX <- renderPlot({
    CartaX <- qcc(Longitud, type = "xbar", data.name = "Longitud")
    CartaX
  })
  
  output$CartaR <- renderPlot({
    CartaR <- qcc(Longitud, type = "R", data.name = "Longitud")    
  })
  
  output$CartaIndividual <- renderPlot({
    CartaX1 <- qcc(Peso, type = "xbar.one", data.name = "Peso")   
  })
  
  output$CartaP <- renderPlot({
    CartaP <- qcc(P, type = "p", size=MuestraP, data.name = "Defectuosos")  
  })
  
  output$CartaU <- renderPlot({
    CartaU <- qcc(Rugosidades, type = "u", size=MuestraP, data.name = "Rugosidades")  
  })
  
  ##Histogramas-----------------------------------------
  output$HistogramaL <- renderPlot({
    ggplot(NULL,aes(x=as.vector(Longitud))) +
      geom_histogram(binwidth=0.5, aes(y=..density..,fill=..count..), col="firebrick2",lwd=0.3) +
      scale_fill_gradient(low="goldenrod1", high="deepskyblue2")+
      geom_density(col="brown", fill="brown1", alpha=0.2, lty=10) + 
      ylab("Frecuencia") + 
      xlab("Longitud en mm") +
      ggtitle(paste("Histograma de Longitud")) + theme(plot.title = element_text(hjust = 0.5))
  })
      
  output$HistogramaP <- renderPlot({  
    ggplot(NULL,aes(x=as.vector(Peso))) +
      geom_histogram(binwidth=1, aes(y=..density..,fill=..count..), col="firebrick2",lwd=0.3) +
      scale_fill_gradient(low="goldenrod1", high="deepskyblue2")+
      geom_density(col="brown", fill="brown1", alpha=0.2, lty=10) + 
      ylab("Frecuencia") + 
      xlab("Peso en Kg") +
      ggtitle(paste("Histograma de Peso")) + theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$HistogramaD <- renderPlot({  
    ggplot(NULL,aes(x=as.matrix(P))) +
      geom_histogram(binwidth=0.5, aes(y=..density..,fill=..count..), col="firebrick2",lwd=0.3) +
      scale_fill_gradient(low="goldenrod1", high="deepskyblue2")+
      geom_density(col="brown", fill="brown1", alpha=0.2, lty=10) + 
      ylab("Frecuencia") + 
      xlab("Cantidad de Defectuosos") +
      ggtitle(paste("Histograma de Defectuosos")) + theme(plot.title = element_text(hjust = 0.5))
  })

  output$HistogramaR <- renderPlot({  
    ggplot(NULL,aes(x=as.matrix(Rugosidades))) +
      geom_histogram(binwidth=0.5, aes(y=..density..,fill=..count..), col="firebrick2",lwd=0.3) +
      scale_fill_gradient(low="goldenrod1", high="deepskyblue2")+
      geom_density(col="brown", fill="brown1", alpha=0.2, lty=10) + 
      ylab("Frecuencia") + 
      xlab("Cantidad de Rugosidades") +
      ggtitle(paste("Histograma de Defectos")) + theme(plot.title = element_text(hjust = 0.5))
  })  
  
  output$RRLargo <- renderPlot({
    ss.rr(var = Medida, part = Hoja, appr = Operador,
          data=RR, main = "Analisis R&R")
    })
  
  output$Salida <- renderImage({ #RR2------------------------------
    
    filename <- normalizePath(file.path('www/Salida.png'))
    list(src = filename)
  }, deleteFile = FALSE)
  
  output$RRLargo2 <- renderPlot({
    ss.rr(var = Medida, part = Hoja, appr = Operador,
          data=RR2, main = "Analisis R&R2",lsl = 592.5,usl = 595.5)
  })
  
  output$Flujograma <- renderImage({ #RR2------------------------------
    filename <- normalizePath(file.path('www/Flujograma.png'))
    list(src = filename)
  }, deleteFile = FALSE)
  
  output$CartaX2 <- renderPlot({
    CartaX2 <- qcc(Longitud2, type = "xbar", data.name = "Longitud")
  })
  
  output$CartaR2 <- renderPlot({
    CartaR2 <- qcc(Longitud2, type = "R", data.name = "Longitud")     
  })
  
  output$CartaP2 <- renderPlot({
    CartaP2 <- qcc(P2, type = "p", size=MuestraP2, data.name = "Defectuosos") 
  })
  
  output$CartaU2 <- renderPlot({
    CartaU2 <- qcc(Rugosidades2, type = "u", size=MuestraP2, data.name = "Rugosidades")  
  })
  
  ##Capacidad del Proceso-----------------------------------------
  #carta X
  output$CapacidadX <- renderPlot({
    CartaX2 <- qcc(Longitud2, type = "xbar", data.name = "Longitud")
    process.capability(CartaX2, spec.limits = c(592.5,595.5))
    })
  
  PPM<-reactive({
    CartaP2 <- qcc(P2, type = "p", size=MuestraP2, data.name = "Defectuosos") 
    CartaP2$center*1000000
  })
  
  PorCap<-reactive({
    CartaP2 <- qcc(P2, type = "p", size=MuestraP2, data.name = "Defectuosos") 
    PPM=CartaP2$center*1000000
    Cap=1-PPM/1000000
  })
    
  CapPotencial<-reactive({
    CartaP2 <- qcc(P2, type = "p", size=MuestraP2, data.name = "Defectuosos") 
    PPM=CartaP2$center
    L=qnorm(1-0.0027/2)
    Pmedia=PPM
    Capacidad=1-Pmedia
    Z=qnorm((1+Capacidad)/2)
    CPAtributos=Z/L
  })
  
  output$CP <- renderText({
    paste("Las Hojas Por Millon (ppm) defectusosas son:
    PPM=", as.numeric(PPM()))
  })  
  
  output$CP2 <- renderText({
    paste("El porcentaje de Capacidad de (1-P) es ", (as.double(PorCap())))
  })       
    
  output$CP3 <- renderText({
    paste("El indice de Capacidad Potencial Cp=", (as.numeric(CapPotencial())))
  })
})

shinyApp(ui, server2)