library(ggplot2)
library(dplyr)
library(data.table)

custom.col <- c( "#E69F00", "#56B4E9", "#009E73",
                 "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

peakRentalModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             box(plotOutput(ns("hours_rent_plot")) %>% withSpinner(color = "#0dc5c1", size = 0.75), width = 6),
             box(plotOutput(ns("hours_rent_days_plot")) %>% withSpinner(color = "#0dc5c1", size = 0.75), width = 6)
             ))
    )
  }


peakRentalDataModuleServer <- function(input, output, session, dataset) {
  agregated_data_byhours <- reactive({
    dataset()[, .(n = .N), keyby = .(day_rent, hour_rent_start)][, freq := prop.table(n), by = "day_rent"]
  })
  return(agregated_data_byhours)
  }

peakHoursPlotsModuleServer <- function(input, output, session, dataset){
  output$hours_rent_plot <- renderPlot({
    ggplot(dataset(), aes(x = hour_rent_start, y = freq,  fill = day_rent)) +
      geom_bar(stat = "identity")+
      scale_y_continuous(labels = percent)+
      scale_x_continuous(breaks = scales::pretty_breaks(n = 15), expand = c(0, 0))+
      labs( y = "Frequency of rents", x = "Hour rent start", fill = "Day")+
      scale_fill_manual(values = custom.col)
  })
  output$hours_rent_days_plot <- renderPlot({
    ggplot(dataset(), aes(x = hour_rent_start, y = freq )) +
      geom_bar(stat = "identity")+
      scale_y_continuous(labels = percent)+
      facet_wrap(~day_rent, nrow = 4)
  })
}




