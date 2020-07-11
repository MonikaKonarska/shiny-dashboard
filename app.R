library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(ggplot2)
library(scales)

source(file.path(getwd(), "modules", "data_modules.R"))


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
                column(width = 7, plotOutput(outputId = "amount_rents_plot") %>% withSpinner(color="#0dc5c1"))
              )
              )
      )
    )
  )

server <- function(input, output, session) {
  filtered_data  <- callModule(module = filteringDatasetModule, id = "select_data")
  callModule(module = infoBoxModuleServer , id = "infoboxes",  dataset = filtered_data)
  
  output$amount_rents_plot <- renderPlot({
    number_of_rentals_plot(dataset = filtered_data())
  })
}



shinyApp(ui = ui, server = server)

