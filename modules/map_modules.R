library(sf)
library(sp)
library(ggplot2)
library(scales)
library(dplyr)


createMainMapsModuleUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
        splitLayout(cellWidths = c("50%", "50%"), plotOutput(ns("district_plot")), plotOutput(ns("stations_plot")))
      
#      column(width = 12, )
    )
  )
}





prepareDataStationsToFirstMapsServer <- function(input, output, session) {

  stations <- read.csv(file.path(getwd(), "data", "station_coordinates.csv"), sep = ";")
  coordinates(stations) <- c("longitude", "latitude")
  stations_sf <- st_as_sf(stations)
  st_crs(stations_sf) <- 4326 
  #stations_sf$amount_bike_rack <- as.numeric(as.character(stations_sf$amount_bike_rack))
  stations_sf$amount_bike_rack <- as.factor(stations$amount_bike_rack)
  
  # stations_sf_points <- stations_sf$geometry
  # stations_sf_coords <- as.data.frame(sf::st_coordinates(stations_sf_points))
  # stations_sf_coords$station_name <- as.factor(stations_sf$station_name)
  return(stations_sf)  
  print("stat")
}


prepareDataDistrictsToFirstMapsServer<- function(input, output, session){
 
  file_shp <- file.path(getwd(), "data", "GraniceOsiedli", "GraniceOsiedli.shp")
  districts <- read.csv(file.path(getwd(), "data", "district_Wroclaw.csv"), sep = ';')
  wroclaw_communities <- st_read(dsn = file_shp)
  st_transform(wroclaw_communities, 4326)  
  wroclaw_communities <- wroclaw_communities %>% 
    left_join(districts %>% select(OBJECTID, DZIELNICA) , by = c("OBJECTID" = "OBJECTID")) 
  return(wroclaw_communities)
  print("wro")
}



createMainMapsServer <- function(input, output, session, data_stations, data_districts) {
  
  # stations <- data_stations()
  # districts <- data_districts()
  districts_points <- sf::st_point_on_surface(data_districts$geometry) 
  districts_coords <- as.data.frame(sf::st_coordinates(districts_points))
  districts_coords$NAZWAOSIED <- data_districts$NAZWAOSIED
  stations_sf_points <- data_stations$geometry
  stations_sf_coords <- as.data.frame(sf::st_coordinates(stations_sf_points))
  stations_sf_coords$station_name <- as.factor(data_stations$station_name)
  
  output$district_plot <- renderPlot({
    ggplot() +
      geom_sf(data = data_districts, aes(fill = data_districts$DZIELNICA)) +
      geom_text(data = districts_coords, aes(X, Y, label = NAZWAOSIED), colour = "black", size  = 3)+
      scale_fill_discrete(name = "Districts:")+
      #theme(legend.position = "none")+
      coord_sf()+
      ggtitle("Districts of Wrocław")
  })
  
  output$stations_plot <-renderPlot({
    ggplot() + 
      geom_sf(data = data_districts, aes(fill = DZIELNICA)) + 
      geom_sf(data = data_stations, aes(size = amount_bike_rack), size = 3) +
      scale_fill_discrete(name = "Districts:")+
      ggtitle("Bike stations in Wrocław")
    
  })
  
  
}





