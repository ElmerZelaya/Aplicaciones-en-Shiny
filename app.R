library(openxlsx)
library(dplyr)
library(sp)
library(sf)
library(rgdal)
library(leaflet)
library(rgee)
library(shiny)
library()

# cargando el archivo excel 
barrios_df <- read.xlsx("~/ElmerZelaya/mapas/barrio_df_peri.xlsx", 
                        sheet = "Hoja1") 

barrios_df_ptos <- barrios_df %>% mutate(Ptos = sample(1:592))

# cargando el shipefile
barrios_shp <- readOGR('Barrios.shp')
#barrios_shp <- asS4(barrios_shp)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel('Mapa Barrios'),
  leafletOutput(('map'), height="100vh")
)


server <- function(input, output) {
  
  output$map <- renderLeaflet({
    barrios_shp@data <- barrios_shp@data %>% left_join(barrios_df_ptos, by = c('FID' = 'ID'))
    
    # creacion del mapa
    
    
    # color numeric
    colorP1 <- colorNumeric(palette = 'Reds', domain =  barrios_shp@data$Ptos, reverse = FALSE)
    colorP2 <- colorNumeric(palette = 'Greens', domain =  barrios_shp@data$Ptos, reverse = FALSE)
    colorP3 <- colorNumeric(palette = 'Oranges', domain = barrios_shp@data$Ptos, reverse = FALSE)
    colorND <- colorNumeric(palette = 'Purples', domain =  barrios_shp@data$Ptos, reverse = FALSE)
    
    
    # creando los subsets del shapefile (por perifericas)
    Perif1_shp <- subset(barrios_shp, Periferica == 'Periferica 1')
    Perif2_shp <- subset(barrios_shp, Periferica == 'Periferica 2')
    Perif3_shp <- subset(barrios_shp, Periferica == 'Periferica 3')
    PerifND_shp <- subset(barrios_shp, Periferica == 'ND')
    
    
    # labels para cada periferica (filtracion)
    labelsPeri1 <- sprintf('<strong> %s </strong></br>casos: %g', Perif1_shp$NAME, Perif1_shp$Ptos)%>%
      lapply(htmltools::HTML)
    
    labelsPeri2 <- sprintf('<strong> %s </strong></br>casos: %g', Perif2_shp$NAME, Perif2_shp$Ptos)%>%
      lapply(htmltools::HTML)
    
    labelsPeri3 <- sprintf('<strong> %s </strong></br>casos: %g', Perif3_shp$NAME, Perif3_shp$Ptos)%>%
      lapply(htmltools::HTML)
    
    labelsND <- sprintf('<strong> %s </strong></br>casos: %g', PerifND_shp$NAME, PerifND_shp$Ptos)%>%
      lapply(htmltools::HTML)
    
    # Agregeando capas
    mapa_barrios <- leaflet()%>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView( -87.20521, 14.08702, 12)%>%
      addPolygons(data = Perif1_shp, fillColor = ~colorP1(Ptos),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labelsPeri1,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"), group = 'Periferica 1')%>%
      
      addPolygons(data = Perif2_shp, fillColor = ~colorP2(Ptos),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labelsPeri2,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"), group = 'Periferica 2')%>%
      
      addPolygons(data = Perif3_shp, fillColor = ~colorP3(Ptos),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(
                    weight = 5,
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labelsPeri3,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"), group = 'Periferica 3')%>%
      
      addPolygons(data = PerifND_shp, fillColor = ~colorND(Ptos),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(
                    weight = 5,
                    dashArray = "",
                    fillOpacity = 0.7,   
                    bringToFront = TRUE),
                  label = labelsND, 
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"), group = 'ND')
    
    
    
    # Control de capas
    mapa_barrios <- mapa_barrios %>% addLayersControl(overlayGroups = c('Periferica 1{color:red}','Periferica 2','Periferica 3', 'ND' ), options = layersControlOptions(collapsed = F))
    
    # ver mapa 
    mapa_barrios
    
    
  })
  
}

shinyApp(ui = ui, server = server)
