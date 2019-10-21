#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(chron)
library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(fields)
library(shinyjs)
library(plotly)

#Leememos las bases de datos
data_accidentalidad <- read.csv("accidentalidad_main_limpia.txt", header=TRUE, sep = " ")
fec_acc<-paste(data_accidentalidad$PERIODO,data_accidentalidad$MES,data_accidentalidad$DIA,sep="-")
fechas<-as.Date(fec_acc)
data_accidentalidad$fechas<-fechas

fec_dic<-read.csv("fec_dic.csv",header= TRUE,sep=",",encoding="UTF-8")
fec_dic$fechas<-as.Date(fec_dic$fechas)
fec_santa<-read.csv("fec_santa.csv",header= TRUE,sep=",",encoding="UTF-8")
fec_santa$fechas<-as.Date(fec_santa$fechas)
fec_flores<-read.csv("fec_flores.csv",header= TRUE,sep=",",encoding="UTF-8")
fec_flores$fechas<-as.Date(fec_flores$fechas)
clasicos <-read.csv("clasicos.csv",sep=";")

#Preprocesamiento de la base de datos
# fecha_cla<-as.character(clasicos$Fechas)
# split_date<-unlist(strsplit(fecha_cla ,split="de",fixed=TRUE))
# dia_clasico_futb <- split_date[seq(1,length(split_date),by=3)]
# mes_clasico_futb <- split_date[seq(2,length(split_date),by=3)]
# anno_clasico_futb <- split_date[seq(3,length(split_date),by=3)]
# fecha_complete_clasicos <- as.character(paste(anno_clasico_futb, mes_clasico_futb,
#                              dia_clasico_futb,sep="-"))
# clasicos$Fechas <- as.Date(fecha_complete_clasicos, format =  "%y-%m-%d")


#Damos formato a las Horas
data_accidentalidad$HORA<-format(strptime(data_accidentalidad$HORA, "%I:%M %p"), format="%H:%M:%S")
data_accidentalidad$HORA<-times(data_accidentalidad$HORA)
barrios<-unique(data_accidentalidad$BARRIO)

#Obtenemos cada una de las clases de columnas 
list_years <- unique(data_accidentalidad$PERIODO)
gravedad_accide <- unique(data_accidentalidad$GRAVEDAD)
min_year <- toString(min(list_years))
max_year <- toString(max(list_years))
class_accide <- unique(data_accidentalidad$CLASE)
list_comunas <- unique(data_accidentalidad$COMUNA)
hora<-unique(data_accidentalidad$HORA)
dia<-unique(data_accidentalidad$DIA_NOMBRE)
festividades <- c("SEMANA SANTA", "FERIA FLORES", "FIESTA NAVIDAD")
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    useShinyjs(),

    # Application title
    titlePanel("Filtrado por accidentalidad "),
    sidebarPanel(
        selectInput("class_accidente", "Seleccione Clase de Accidente",
                    choices = class_accide),
    #selectInput("year", "Seleccione ano:",
    #            choices = list_years),
    #numericInput("obs", "Number of observations to view:", 10),
        selectInput("Comuna", "Seleccione la Comuna",
                    choices = c(1:16)),
        selectInput("Gravedad_accidente", "Seleccione el tipo de Gravedad de Accidente",
                    choices = gravedad_accide),
        selectInput("festividades_medellin", "Seleccione una festividad", 
                    choices = festividades),
    
        dateRangeInput("rango_tiempo", "Selecione un rango de tiempo:",
                    start  = paste(min_year,'01-01'), #"2001-01-01",
                    end    = paste(max_year,'12-31'),#"2010-12-31",
                    format = "yyyy-mm-dd",
                    separator = " - "),
        actionButton(inputId = "mostrar", label = "Actualizar Filtro")
    
    ),
    mainPanel(
        
        tabsetPanel(id = "tabs", type = "tabs",
                    tabPanel("Accidentalidad Medellin", plotlyOutput("time_series"),align="center", value = "time_series"),
                    tabPanel("Ocurrencia de Accidentes por Comunas", leafletOutput("mapas"), plotlyOutput("distPlot2"),value = "mapa_comunas"),
                    tabPanel("Diagramas de Calor", plotOutput("diag_calor"), value = "diag_cal"),
                    tabPanel("Accidentalidad Dias Festivos", leafletOutput("mapas_2"), plotlyOutput("festividades"), value = "dias_de_festividad")
        )

    
    #textOutput("mostrar_class")
    
    )

    


)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    


    datasetInput <- eventReactive(input$mostrar, {
        date_to_numeric <- as.Date(data_accidentalidad[,c('FECHA')])
        data_accidentalidad$FECHA <- date_to_numeric
        data_aux = filter(data_accidentalidad, CLASE == input$class_accidente & FECHA > input$rango_tiempo[1] & FECHA < input$rango_tiempo[2]) #subset(data_accidentalidad, subset = (CLASE_ACCIDENTE = input$class_accidente & FECHA_ACCIDENTE > input$rango_tiempo[1]))
        #data_aux[,c("CLASE", "GRAVEDAD", "FECHA")]
    }, ignoreNULL = FALSE)
    
    # if(input$tabs == "dias_de_festividad"){
    #     date_to_numeric <- as.Date(data_accidentalidad[,c('FECHA')])
    #     data_accidentalidad$FECHA <- date_to_numeric
    #     data_aux = filter(data_accidentalidad, GRAVEDAD == input$Gravedad_accidente & FECHA > input$rango_tiempo[1] & FECHA < input$rango_tiempo[2])
    #     #datasetInput <- data_aux
    #     }

    output$time_series <- renderPlotly({
        
        data_accidentalidad$PERIODO <- as.numeric(data_accidentalidad$PERIODO)
        muertos <- filter(data_accidentalidad, GRAVEDAD == "MUERTO")
        heridos <- filter(data_accidentalidad, GRAVEDAD == "HERIDO")
        danos <- filter(data_accidentalidad, GRAVEDAD == unique(data_accidentalidad$GRAVEDAD)[1])
        counts_muertos <- table(muertos$PERIODO)
        counts_heridos <- table(heridos$PERIODO)
        counts_material <- table(danos$PERIODO)
        serie_muerto = plotly::plot_ly(x = unique(muertos$PERIODO), y = counts_muertos, height = 500, width = 700, name = "Serie de Tiempo Muertos Accidentalidad" , marker = list(color = c("rgb(253,94,83)")))%>% 
            plotly::layout(title = ' Cantidad Muertos Accidentalidad Medellin 2014-2018' ,yaxis = list(title="Cantidad de Muertos por Accidentalidad"), 
                           xaxis = list(title="Periodo"))%>%plotly::add_bars(width=c(0.03,0.03,0.03,0.03,0.03))
        serie_herido = plotly::plot_ly(x = unique(heridos$PERIODO), y = counts_heridos,height = 500, width = 700,  name = "Serie de Tiempo Heridos Accidentalidad" , marker = list(color = c("rgb(255,174,66)")))%>% 
            plotly::layout(title = ' Cantidad Heridos Accidentalidad Medellin 2014-2018' ,yaxis = list(title="Cantidad de Heridos por Accidentalidad"), 
                           xaxis = list(title="Periodo"))%>%plotly::add_bars(width=c(0.03,0.03,0.03,0.03,0.03))
        serie_danos = plotly::plot_ly(x = unique(danos$PERIODO), y = counts_material,height = 500, width = 700,  name = "Serie de Tiempo Danos Materiales Accidentalidad" , marker = list(color = c("rgb(154,206,235)")))%>% 
            plotly::layout(title = ' Cantidad Danos Materiales Accidentalidad Medellin 2014-2018' ,yaxis = list(title="Cantidad de Danos Materiales por Accidentalidad"), 
                           xaxis = list(title="Periodo"))%>%plotly::add_bars(width=c(0.03,0.03,0.03,0.03,0.03))
        
        if(input$Gravedad_accidente == "MUERTO"){
            
            serie_muerto
        }
        else if(input$Gravedad_accidente == "HERIDO"){
            
            serie_herido
        }
        else{
            
            serie_danos
        }
    })
    output$distPlot2 <- renderPlotly({
        data_aux <- datasetInput()
        data_aux$COMUNA <-  as.numeric(data_aux$COMUNA)
        ocurrencia_x_comuna <- filter(data_aux, COMUNA < 17 & GRAVEDAD == input$Gravedad_accidente)
        counts <- table(ocurrencia_x_comuna$COMUNA)
        bar_colors = ifelse(input$Gravedad_accidente == "HERIDO","orange",ifelse(input$Gravedad_accidente == "MUERTO","red","blue"))
        if(length(counts) == 16){
            
            x = factor(c(1:16))
        }
        else{
            
            x = factor(unique(ocurrencia_x_comuna$COMUNA))
            x = c(as.numeric(levels(x)))
        }
        print(x)
        #print(table(ocurrencia_x_comuna$COMUNA))
        if(length(counts) != 0){
            ## Si se coloca x = factor(c(1:16)) hay problemas en el plot interactivo dado que esta lista no depende del input.
            showing = plotly::plot_ly(x = x, y = counts, type = "bar", marker = list(color = c(bar_colors)))%>%plotly::layout(title = paste("Ocurrencias de ",toString(input$class_accidente), "por Comunas", 
                                                                                                                                                               sep=" "), xaxis = list(title="Comuna"),                                                                                                                             yaxis = list(title = "Numero de Accidentes"))
            
            #print(x)
            print(counts)
            showing
        }
        else{
            
            validate(1 == 0, "")
        }
        })
    
    output$mostrar_class <- renderText({
        # data_aux <- datasetInput()
        # toString(unique(data_aux$COMUNA))
        input$rango_tiempo[1]

    })
    output$mapas <- renderLeaflet({
        map<-leaflet()
        data_aux <- datasetInput()
        data_aux <- filter(data_aux, COMUNA == input$Comuna, GRAVEDAD == input$Gravedad_accidente)
        data_aux[,c("Latitud","Longitud")]
        longitud = data_aux$Longitud
        latitud = data_aux$Latitud
        
        lng1 = min(longitud)
        lat1=min(latitud)
        lng2=max(longitud)
        lat2=max(latitud)
        
        if (length(longitud) > 1 && length(latitud) > 1){
            
            lng1 = min(longitud)
            lat1=min(latitud)
            lng2=max(longitud)
            lat2=max(latitud)
            
        }
        
        else if(length(longitud) == 1 && length(latitud) == 1){
            
            lng1 = min(longitud)
            lat1= min(latitud) 
            lng2= lng1 + 0.005
            lat2= lat1 - 0.005
        }
        if(lng1 != Inf | lng2 != -Inf){
            
            icono <- awesomeIcons("ion", markerColor = ifelse(data_aux$GRAVEDAD == "HERIDO","orange",ifelse(data_aux$GRAVEDAD == "MUERTO","red","blue")))
            map <- addProviderTiles(map,provider="OpenStreetMap.Mapnik")
            map <- fitBounds(map, lng1, lat1, lng2,lat2)
            #map<-addAwesomeMarkers(map,lng=longitud, lat=latitud, icon = icono)
            map<-addCircleMarkers(map, lng=longitud, lat=latitud, opacity=1,fill=TRUE,color="rgba(255,0,0,1)",fillColor = "rgba(255,0,0,1)",label = "1", labelOptions = labelOptions(noHide = T, direction = 'center', textOnly = T),fillOpacity=0.7,radius=7,clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {    
            var childCount = cluster.getChildCount(); 
            var c = ' marker-cluster-';  
            if (childCount < 100) {  
              c = 'rgba(0,0,255,1);' 
            } else if (childCount < 1000) {  
              c = 'rgba(255,255,0,1);'  
            } else { 
              c = 'rgba(255,0,0,1);'  
            }    
            return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });

            }")))
            map
        }
        
        else{
            validate(lng1 != Inf | lat1 != Inf | lng2 != -Inf | lat2 != -Inf, "No hay ocurrencias,\n  por favor intente de nuevo con otro filtrado.")
        }
    
    })


    output$diag_calor <- renderPlot({
        data_aux <- datasetInput()

        #Conversion de formato de hora
        data_aux$HORA_SOLA<-hours(data_aux$HORA)
        dayLabs<-c("LUNES","MARTES ","MIÉRCOLES","JUEVES ","VIERNES","SÁBADO ","DOMINGO")
        data_aux$DIA_NOMBRE <- factor(data_aux$DIA_NOMBRE, levels= dayLabs)

        #Extracción de subconjuntos según el daño presentado
        choques_material<-subset(data_aux,subset=(GRAVEDAD== input$Gravedad_accidente))
        #choques_heridos<-subset(data_aux,subset=(GRAVEDAD=="HERIDO"))
        #choques_fatal<-subset(data_aux,subset=(GRAVEDAD=="MUERTO"))

        #####Daños materiales
        consolidado_material<-table(choques_material$DIA_NOMBRE,choques_material$HORA_SOLA)
        
        if(length(consolidado_material) == 0){
            
            validate(1==0, "No hay ocurrencias, pruebe otra combinacion")
        }
        else{
            image.plot(consolidado_material ,ylab='Hora',main= paste("Accidentalidad en Medellin-",toString(input$Gravedad_accidente), "por Comunas", sep=" "), xaxt='n', yaxt='n')
            axis(1, at=seq(0,1,by=1/6), labels=c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado",
                                                 "Domingo"),las=2,cex.axis=1.2)
            axis(2, at=seq(0,1,by=1/23), labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,
                                                  13,14,15,16,17,18,19,20,21,22,
                                                  23),las=1,cex.axis=1.2)
        }

    })

    output$mapas_2 <- renderLeaflet({
        data_aux <- datasetInput()
        if (input$festividades_medellin == "SEMANA SANTA"){
            data_feria <- merge(data_aux,fec_santa,by.x="fechas",by.y="fechas") 
            data_feria <- filter(data_feria, GRAVEDAD == input$Gravedad_accidente)
            
        }
        else if (input$festividades_medellin == "FERIA FLORES"){
            data_feria <- merge(data_aux,fec_flores,by.x="fechas",by.y="fechas")
            data_feria <- filter(data_feria, GRAVEDAD == input$Gravedad_accidente)
            
        }
        else if (input$festividades_medellin == "FIESTA NAVIDAD"){
            data_feria <- merge(data_aux,fec_dic,by.x="fechas",by.y="fechas")
            data_feria <- filter(data_feria, GRAVEDAD == input$Gravedad_accidente)
            
        }
        else if (input$festividades_medellin == "PARTIDOS NACIONAL MEDELLIN"){
            print(clasicos)
            data_feria <- merge(data_aux,fec_dic,by.x="fechas",by.y="fechas")
            data_feria <- filter(data_feria, GRAVEDAD == input$Gravedad_accidente)
            
        }
        
        data_feria[,c("Latitud","Longitud")]
        longitud = data_feria$Longitud
        latitud = data_feria$Latitud
        
        lng1 = min(longitud)
        lat1=min(latitud)
        lng2=max(longitud)
        lat2=max(latitud)
        
        if (length(longitud) > 1 && length(latitud) > 1){
            
            lng1 = min(longitud)
            lat1=min(latitud)
            lng2=max(longitud)
            lat2=max(latitud)
            
        }
        
        else if(length(longitud) == 1 && length(latitud) == 1){
            
            lng1 = min(longitud)
            lat1= min(latitud) 
            lng2= lng1 + 0.005
            lat2= lat1 - 0.005
        }
        if(lng1 != Inf | lng2 != -Inf){
        map<-leaflet()
        data_feria[,c("Latitud","Longitud")]
        longitud = data_feria$Longitud
        latitud = data_feria$Latitud
        
        lng1 = min(longitud)
        lat1=min(latitud)
        lng2=max(longitud)
        lat2=max(latitud)
        #icono <- awesomeIcons("ion", markerColor = ifelse(data_aux$GRAVEDAD == "HERIDO","orange",ifelse(data_aux$GRAVEDAD == "MUERTO","red","blue")))
        map <- addProviderTiles(map,provider="OpenStreetMap.Mapnik")
        map <- fitBounds(map, lng1, lat1, lng2,lat2)
        #map<-addAwesomeMarkers(map,lng=longitud, lat=latitud, icon = icono)
        map<-addCircleMarkers(map, lng=longitud, lat=latitud, opacity=1,fill=TRUE,color="rgba(255,0,0,1)",fillColor = "rgba(255,0,0,1)",label = "1", labelOptions = labelOptions(noHide = T, direction = 'center', textOnly = T),fillOpacity=0.7,radius=7,clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {    
            var childCount = cluster.getChildCount(); 
            var c = ' marker-cluster-';  
            if (childCount < 15) {  
              c = 'rgba(0,0,255,1);' 
            } else if (childCount < 45) {  
              c = 'rgba(255,255,0,1);'  
            } else { 
              c = 'rgba(255,0,0,1);'  
            }    
            return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });

        }")))
        map
        }
    })
    
    output$festividades <- renderPlotly({
        data_aux <- datasetInput()
        if (input$festividades_medellin == "SEMANA SANTA"){
            data_feria <- merge(data_aux,fec_santa,by.x="fechas",by.y="fechas") 
            data_feria <- filter(data_feria, GRAVEDAD == input$Gravedad_accidente)
            
        }
        else if (input$festividades_medellin == "FERIA FLORES"){
            data_feria <- merge(data_aux,fec_flores,by.x="fechas",by.y="fechas")
            data_feria <- filter(data_feria, GRAVEDAD == input$Gravedad_accidente)
            
        }
        else if (input$festividades_medellin == "FIESTA NAVIDAD"){
            data_feria <- merge(data_aux,fec_dic,by.x="fechas",by.y="fechas")
            data_feria <- filter(data_feria, GRAVEDAD == input$Gravedad_accidente)
            
        }
        else if (input$festividades_medellin == "PARTIDOS NACIONAL MEDELLIN"){
            print(clasicos)
            data_feria <- merge(data_aux,fec_dic,by.x="fechas",by.y="fechas")
            data_feria <- filter(data_feria, GRAVEDAD == input$Gravedad_accidente)
            
        }
        counts <- table(data_feria$PERIODO)
        if (length(counts) == 0){
            
            plotly_empty()
            validate(1 == 0, "No hay ocurrencias, intente otra combinacion")
        }
        
        else{
            
            print(counts)
            #barplot(counts, main=  paste("Ocurrencias de ",toString(input$Gravedad_accidente), " en ", toString(input$festividades_medellin), sep=" "), xlab="Comunas")
            
            if(input$Gravedad_accidente == "HERIDO"){
                colorear = "rgb(255,174,66)"
            }
            else if(input$Gravedad_accidente == "MUERTO"){
                colorear = "rgb(253,94,83)"
            }
            else{
                colorear = "rgb(154,206,235)"
            }
            
            plotly::plot_ly(x = unique(data_feria$PERIODO), y = counts, type ="bar",  name = paste("Ocurrencias de ",toString(input$Gravedad_accidente), " en ", toString(input$festividades_medellin)) , marker = list(color = c(colorear)))%>% 
                plotly::layout(title = paste("Ocurrencias de ",toString(input$Gravedad_accidente), " en ", toString(input$festividades_medellin)) ,yaxis = list(title="Cantidad de Accidentes"), 
                               xaxis = list(title="Periodo"))
            
        }

        
        
    })
    

    observe({
        
        
        if(input$tabs == "mapa_comunas")   {
            #habilitar/deshab botones
            
            shinyjs::enable("Comuna")
            shinyjs::enable("Gravedad_accidente")
            shinyjs::enable("rango_tiempo")
            shinyjs::enable("class_accidente")
            shinyjs::disable("festividades_medellin")
            shinyjs::enable("mostrar")
            
            #actualizar choices para que afecten o no afecten el filtrado
            
            updateSelectInput(session,"festividades_medellin", choices = "")
            updateSelectInput(session,"Comuna", choices = c(1:16))
            updateSelectInput(session,"Gravedad_accidente", choices = gravedad_accide)
            updateSelectInput(session,"class_accidente", choices = class_accide)
            updateDateRangeInput(session,"rango_tiempo",
                                 start  = paste(min_year,'01-01', sep = "-"), #"2001-01-01",
                                 end    = paste(max_year,'12-31', sep = "-")#"2010-12-31",
            )
        }
        
        else if(input$tabs == "diag_cal"){
            
            #habilitar/deshab botones
            
            shinyjs::disable("Comuna")
            shinyjs::enable("Gravedad_accidente")
            shinyjs::enable("rango_tiempo")
            shinyjs::enable("class_accidente")
            shinyjs::disable("festividades_medellin")
            shinyjs::enable("mostrar")
            
            #actualizar choices para que afecten o no afecten el filtrado o deshabilitar las que ni siquiera filtran algo
            
            updateSelectInput(session,"festividades_medellin", choices = "")
            updateSelectInput(session,"Comuna", choices = "")
            updateSelectInput(session,"Gravedad_accidente", choices = gravedad_accide)
            updateSelectInput(session,"class_accidente", choices = class_accide)
            updateDateRangeInput(session,"rango_tiempo",
                                 start  = paste(min_year,'01-01', sep = "-"), #"2001-01-01",
                                 end    = paste(max_year,'12-31', sep = "-")#"2010-12-31",
            )    
        }
        else if(input$tabs == "dias_de_festividad"){
            
            #habilitar/deshab botones
            
            shinyjs::disable("Comuna")
            shinyjs::enable("Gravedad_accidente")
            shinyjs::enable("rango_tiempo")
            shinyjs::enable("class_accidente")
            shinyjs::enable("festividades_medellin")
            shinyjs::enable("mostrar")
            
            #actualizar choices para que afecten o no afecten el filtrado o deshabilitar las que ni siquiera filtran algo
            
            updateSelectInput(session,"festividades_medellin", choices = festividades)
            updateSelectInput(session,"Comuna", choices = "")
            updateSelectInput(session,"Gravedad_accidente", choices = gravedad_accide)
            updateSelectInput(session,"class_accidente", choices = class_accide)
            updateDateRangeInput(session,"rango_tiempo",
                                 start  = paste(min_year,'01-01', sep = "-"), #"2001-01-01",
                                 end    = paste(max_year,'12-31', sep = "-")#"2010-12-31",
            )    
        }
        else if(input$tabs == "time_series"){
            
            #habilitar/deshab botones
            
            shinyjs::disable("Comuna")
            shinyjs::enable("Gravedad_accidente")
            shinyjs::disable("rango_tiempo")
            shinyjs::disable("class_accidente")
            shinyjs::disable("festividades_medellin")
            shinyjs::disable("mostrar")
            
            #actualizar choices para que afecten o no afecten el filtrado o deshabilitar las que ni siquiera filtran algo
            
            updateSelectInput(session,"festividades_medellin", choices = "")
            updateSelectInput(session,"Comuna", choices = "")
            updateSelectInput(session,"Gravedad_accidente", choices = gravedad_accide)
            updateSelectInput(session,"class_accidente", choices = "")
            updateDateRangeInput(session,"rango_tiempo",
                                 start  = paste(min_year,'01-01', sep = "-"), #"2001-01-01",
                                 end    = paste(max_year,'12-31', sep = "-")#"2010-12-31",
            )    
        }
        
    })


}

# Run the application 
shinyApp(ui = ui, server = server)
