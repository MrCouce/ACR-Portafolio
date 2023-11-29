library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(leaflet)
library(corrplot)
library(reshape2)
#### datos ####

datos<-read.csv("airbnb.csv")
head(datos)
dat<-datos 
attach(dat)
# Datos mapa
datmap <- readr::read_csv("airbnb.csv")
datmap<-datmap %>% select(latitude, longitude, host_name, name,price, neighbourhood_group)  %>%  sample_n(30)
# Datos cor

datcor<-dat %>% select(latitude,longitude,price,availability_365,calculated_host_listings_count,number_of_reviews,minimum_nights)


head(dat)

# View(dat)



#### UI ####

Interfaz<-navbarPage("Airb&b App",
                     #### Histograma ####
                     tabPanel("Histograma de Precio", # PRIMER TAB
                              sidebarPanel(
                                
                                selectInput("barrio","Barrio:",
                                            choices=dat$neighbourhood_group,selected="Ciutat Vella"),
                                helpText("Elige un barrio"),
                                hr(),
                                
                                checkboxInput("checkbox","Box-plot",value=FALSE),
                                helpText("Selecciona para ver el Box-Plot"),
                                hr(),
                                
                                sliderInput("rangoprecio",
                                            "Rango de precios:",
                                            value = c(50,500),
                                            min = 20,
                                            max = 500,step = 10)),
                              
                              
                              mainPanel(
                                plotlyOutput("GP1"),
                                
                              )),
                     
                     #### Disponibilidad #### 
                     tabPanel("Disponibilidad", # SEGUNDO TAB
                              sidebarPanel(  
                                
                                selectizeInput("barrio2","Barrio:",
                                               choices=dat$neighbourhood_group,selected="Ciutat Vella"),
                                helpText("Elige un barrio"),
                                hr(),     
                                
                                sliderInput("disp",
                                            "Dias disponibles al año:",
                                            value = c(50,100),
                                            min = 0,
                                            max = 365,step = 10),
                                hr(),
                                
                                
                              ),          
                              
                              mainPanel(plotlyOutput("GP2"))
                     ),
                     #### Correlaciones ####     
                     tabPanel("Correlaciones", # TERCER TAB
                              sidebarLayout(
                                sidebarPanel(  
                                  
                                  checkboxInput("checkbox1","Gradiente",value=FALSE),
                                  helpText("Selecciona para ver con gradiente de color avanzado"),
                                  hr(),
                                  
                                  checkboxInput("checkbox2","Valores",value=FALSE),
                                  helpText("Selecciona para ver con valores superpuestos"),
                                  hr(),
                                ),
                                
                                
                                mainPanel(plotOutput("tcor"))
                                
                              )),
                     #### SUMMARY ####
                     tabPanel("Resumen", # CUARTO TAB
                              column(3,
                                
                                  checkboxGroupInput("variable", "Variable:", choices = names(dat))
                              ),
                              column(4,
                                     
                                  checkboxInput("barriocheck", "Grupo por Barrio:", value=F),
                              
                                  checkboxInput("roomcheck", "Grupo por Habitación:", value=F),
                                  
                                  hr(),
                                  helpText("Seleccionar ambos criterios no producirá ningún cambio")
                              ),  
                              
                                     
                                
                                     
                                
                                mainPanel(
                                  tableOutput("sumario")
                                  
                                )
                              ), 
                     
                     
                     #### MAPA ####      
                     tabPanel("Mapa", # QUINTO TAB
                              sidebarLayout(
                                sidebarPanel(  
                                  
                                  helpText("Selecciona aquí el rango de latitud"),
                                  sliderInput("rlat",
                                              "Rango de latitud:",
                                              value = c(41.35,41.39),
                                              min = 41.34954,
                                              max = 41.46882),
                                  
                                  hr(),
                                  helpText("Selecciona aquí el rango de longitud"),
                                  sliderInput("rlong",
                                              "Rango de longitud:",
                                              value = c(2.089,2.128),
                                              min = 2.088379,
                                              max = 2.227329),
                                  hr(),
                                  sliderInput("rpreciom",
                                              "Rango de precios:",
                                              value = c(50,500),
                                              min = 20,
                                              max = 500,step = 5),
                                ),          
                                
                                mainPanel(leafletOutput("map"),height="100%")
                              ) 
                     ),
                     
                     
)


#### SERVER ####


# Define a server for the Shiny app

server <- function(input,output) {
  
  #### OUTPUT Histograma PRECIO ####
  
  output$GP1 <- renderPlotly({
    if (input$checkbox == "FALSE"){#histograma
      dat %>% filter(neighbourhood_group==input$barrio,price>input$rangoprecio[1],price<input$rangoprecio[2]) %>% 
        plot_ly(x=~price,color=~room_type, type = 'histogram') %>% layout(title=list(text="Histograma"),
                                                                          xaxis = list(title ="Precio"))
    }else{#Boxplot
      dat %>% filter(neighbourhood_group==input$barrio,price>input$rangoprecio[1],price<input$rangoprecio[2]) %>%
        plot_ly(x=~price,color=~room_type,type='box') %>% layout(title=list(text="Box Plot"),
                                                                 xaxis = list(title ="Precio"),
                                                                 yaxis = list(title ="Alojamiento"))
      
    }  

    
  })
  # OUTPUT Gráfico de DISPONIBILIDAD
 
  output$GP2 <- renderPlotly({
        dat %>% filter(availability_365>=input$disp[1],availability_365<=input$disp[2],neighbourhood_group==input$barrio2) %>%  
      plot_ly(x=~availability_365,y=~price,color=~room_type,size=~reviews_per_month) %>%  layout(title=list(text="Disponibilidad"),
                                                                                 xaxis = list(title ="Disponibilidad")
                                                                                 ,yaxis=list(title="Precio")) 
                                                                                      
    
  })  
  #### OUTPUT CORRELACIONES ####
  
  output$tcor<-renderPlot ({
    
    if(input$checkbox1==F & input$checkbox2==F){
      # Matriz sin gradiente y sin valores
      cormat<-round(cor(datcor,method="spearman"),3) 
      melted_cormat <- melt(cormat)  
      ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
        geom_tile()
      
    }else if(input$checkbox1==T & input$checkbox2==F){
      # Matriz con gradiente        
      cormat<-round(cor(datcor,method="spearman"),3) 
      melted_cormat <- melt(cormat)
      ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
        geom_tile(color = "white")+
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                             midpoint = 0, limit = c(-1,1), space = "Lab", 
                             name="Correlación de Spearman") +
        theme_minimal()+ 
        theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                         size = 12, hjust = 1),
              axis.text.y=element_text(vjust = 1,size = 12, hjust = 1))+
        coord_fixed()
    }else if(input$checkbox1==F & input$checkbox2==T){
      # Matriz sin gradiente avanzado y con valores        
      cormat<-round(cor(datcor,method="spearman"),3) 
      melted_cormat <- melt(cormat)  
      ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
        geom_tile()+geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          legend.justification = c(1, 0),
          legend.position = "bottom",
          legend.direction = "horizontal")+
        guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                     title.position = "top", title.hjust = 0.5))  
    }else{
      cormat<-round(cor(datcor,method="spearman"),3) 
      melted_cormat <- melt(cormat)
      ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
        geom_tile(color = "white")+
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                             midpoint = 0, limit = c(-1,1), space = "Lab", 
                             name="Correlación de Spearman") +geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          legend.justification = c(1, 0),
          legend.position = "bottom",
          legend.direction = "horizontal")+
        guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                     title.position = "top", title.hjust = 0.5))  
    }
    
    
  })
  
  #### OUTPUT SUMMARY ####
  

  output$sumario <- renderTable({
 
    if(is.null(input$variable)){
      return()
    }
    else if(input$barriocheck==T){
      
      df1 <- dat %>% dplyr::arrange(desc(as.list(neighbourhood_group)))
      return(df1[input$variable])
    }
    else if(input$roomcheck==T){
     
      df1 <- dat  %>% dplyr::arrange(desc(as.list(room_type)))
      return(df1[input$variable])
    }
    else if(input$roomcheck==T & input$barriocheck==T){
      
      renderPrint({print("Sólo puede elegir un criterio de ordenación a la vez")})
    }
    else{
     df1<-dat[input$variable]
     return(df1)
    }
      
    

  
  })
  #### OUTPUT MAPA ####
  output$map<-renderLeaflet({
    dat %>% filter(longitude>=input$rlong[1],longitude<=input$rlong[2],
                   latitude>=input$rlat[1],latitude<=input$rlat[2],price>=input$rpreciom[1],price<=input$rpreciom[2]) %>% leaflet(datmap) %>% 
      addTiles() %>% addMarkers(lng = ~longitude, lat = ~latitude, popup = ~host_name, label= ~price)
    
    
  })  
  
} # fin función server


# LLAMADAS



shinyApp(ui = Interfaz, server = server)



