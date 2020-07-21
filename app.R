library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(ggplot2)
library(scales)

source(file.path(getwd(), "modules", "data_modules.R"))
source(file.path(getwd(), "modules", "plot_modules.R"))
source(file.path(getwd(), "modules", "table_modules.R"))
source(file.path(getwd(), "modules", "map_modules.R"))


ui <- dashboardPage(
  
  dashboardHeader(title = "Nextbike", titleWidth = 150),
  dashboardSidebar(width = 150,
                   sidebarMenu(
                     menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                     menuItem("Users", tabName = "users", icon = icon("users")),
                     menuItem("Bike stations", tabName ="bike_stations", icon = icon("map-marker-alt")),
                     menuItem("Info", tabName = "info", icon = icon("question-circle"))
                     )
                   ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              infoBoxModuleUI("infoboxes"),
              br(),
              fluidRow(
                filteringDatasetModuleUI("select_data"),
                br(),
                column(width = 7, plotOutput(outputId = "amount_rents_plot") %>% withSpinner(color = "#0dc5c1", size = 0.75))
                ),
              h1("Peak Rental Season", align = "center"),
              tabsetPanel(type = "pills",
                          tabPanel("Hours rents",
                                   peakRentalModuleUI("generateplos_hours")
                                   ),
                          tabPanel("Performance indicators",
                                   peakMonthsIndicatorsModuleUI("agreagted_month_ind_plot")),
                          tabPanel(
                                   peakPercentagesIndicatorsModuleUI("agregated_percentage_ind_plot")
                                   )
                                   
                                   )
                          
              ),
      tabItem(tabName = "bike_stations" , 
              createMainMapsModuleUI("main_maps")
      
        
      )
  )))

server <- function(input, output, session) {
  filtered_data  <- callModule(module = filteringDatasetModule, id = "select_data")
  agregated_data_byhours <- callModule(module = peakRentalDataModuleServer, id = "peak_hours", dataset = filtered_data)
  agregated_data_ind_month <- callModule(module = peakDataMonthsIndicatorsModuleServer, id = "agregated_month_ind", dataset = filtered_data)
  agregated_data_percentage <- callModule(module = peakDataPercentagesIndicatorsServer, id = "agregated_percentage_id", dataset = filtered_data)
  
  callModule(module = infoBoxModuleServer , id = "infoboxes",  dataset = filtered_data)
  callModule(module = peakHoursPlotsModuleServer, id = "generateplos_hours", dataset = agregated_data_byhours)
  callModule(module = peakMonthsIndicatorsModuleServer, id = "agreagted_month_ind_plot", dataset = agregated_data_ind_month)
  callModule(module = peakPercentagesIndicatorsServer, id = "agregated_percentage_ind_plot", dataset = agregated_data_percentage)
  
  stations <- callModule(module = prepareDataStationsToFirstMapsServer, id = "stations_id")
  districts <- callModule(module = prepareDataDistrictsToFirstMapsServer, id = "districts_id")
  callModule(module = createMainMapsServer, id = "main_maps", data_stations = stations, data_districts = districts)
  
  
  
  
  
  
 # callModule(module = peakMonthsIndicatorsModuleServer, id = "indicators", dataset = filtered_data)
  
  output$amount_rents_plot <- renderPlot({
    number_of_rentals_plot(dataset = filtered_data())
  })
}



shinyApp(ui = ui, server = server)

