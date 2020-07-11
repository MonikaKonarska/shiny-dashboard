library(ggplot2)
library(dplyr)
library(data.table)

custom.col <- c( "#E69F00", "#56B4E9", "#009E73",
                 "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

peakHoursModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    h1("Peak hours", align = "center"),
    fluidRow(
      column(width = 12,
             box(plotOutput(ns("hours_rent_plot")), width = 6),
             box(plotOutput(ns("hours_rent_days_plot")), width = 6)
             )
      )
    )
  }


peakHoursModuleServer <- function(input, output, session, dataset) {
  output$hours_rent_plot <- renderPlot({
  grouped_data <- dataset()[, .(n = .N), keyby = .(day_rent, hour_rent_start)][, freq := prop.table(n), by = "day_rent" ]
  ggplot(grouped_data, aes(x = hour_rent_start, y = freq,  fill = day_rent)) +
    geom_bar(stat = "identity")+
    scale_y_continuous(labels = percent)+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 15), expand = c(0, 0))+
    labs( y = "Frequency of rents", x = "Hour rent start", fill = "Day")+
    scale_fill_manual(values = custom.col)
  })
  
  output$hours_rent_days_plot <- renderPlot({
    grouped_data <- dataset()[, .(n = .N), keyby = .(day_rent, hour_rent_start)][, freq := prop.table(n), by = "day_rent"]
    ggplot(grouped_data, aes(x = hour_rent_start, y = freq )) +
      geom_bar(stat = "identity")+
      scale_y_continuous(labels = percent)+
      facet_wrap(~day_rent, nrow = 4)
    })
  }







