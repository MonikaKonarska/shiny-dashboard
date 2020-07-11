library(data.table)
library(roxygen2)
library(scales)

source(file.path(getwd(), "helpers", "data_functions.R"))

if(!exists("dataDT")){
  source(file.path(getwd(), "data", "import_data.R"))
  Sys.setlocale(locale = "English") 
}

min_date_start_rent <- dataDT[, min(start_date)]
max_date_start_rent <- dataDT[, max(start_date)]
selected_choices_is_payment_rent <- c("All", dataDT[ ,unique(is_payment_for_rent)])
selected_choices_is_payment_return <- c("All", dataDT[ ,unique(is_payment_for_return_place)])





filteringDatasetModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    dateRangeInput(inputId = ns("date_range"), label = "Range dates of rent a bike",
                   start = min_date_start_rent, end = max_date_start_rent,
                   min = min_date_start_rent, max = max_date_start_rent,
                   format = "dd/mm/yyyy", separator = "-"),
    
    selectInput(ns("is_payment_rent"), "Payments for rent a bikes (More than 20 minutes ride a bike is payment)", choices = selected_choices_is_payment_rent, selected = "All"),
    selectInput(ns("is_payment_return_place"), "Payments for return bike in diffrent place (Poza stacja)", choices = selected_choices_is_payment_return, selected = "All"),
    checkboxInput(ns("check_outliers"), "WITH OUTLIERS", FALSE),
    actionButton(ns("filter_button"), "Filter data")
    )
  }


filteringDatasetModule <- function(input, output, session) {
  filtered_table <- eventReactive(input$filter_button, ignoreNULL = FALSE, {

    if("All" %in% input$is_payment_rent) {
      selected_choices_is_payment_rent_active  <- selected_choices_is_payment_rent[-1]
    } else {
      selected_choices_is_payment_rent_active <- input$is_payment_rent
    }

    if("All" %in% input$is_payment_return_place) {
      selected_choices_is_payment_return_active <- selected_choices_is_payment_return[-1]
    } else {
      selected_choices_is_payment_return_active <- input$is_payment_return_place
    }

    if(TRUE == input$check_outliers) {
      selected_choices_outliers_active <- c(TRUE, FALSE)
    } else {
      selected_choices_outliers_active <- input$check_outliers
    }

    dataDT[(is_outlier %in% selected_choices_outliers_active) &
             (start_date >= input$date_range[1] & end_date <=  input$date_range[2]) &
             is_payment_for_rent %in% selected_choices_is_payment_rent_active &
             is_payment_for_return_place %in% selected_choices_is_payment_return_active]
    })

  return(filtered_table)
}





infoBoxModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
  fluidRow(
    infoBoxOutput(ns("mean_minutes"), width = 3), infoBoxOutput(ns("median_minutes"), width = 3),
    infoBoxOutput(ns("amount_rents"), width = 3), infoBoxOutput(ns("amount_bikes"), width = 3)
    )
  )
}


infoBoxModuleServer <- function(input, output, session, dataset) {
  
  mean_minutes_value <- reactive({
    return(compute_statistic_to_infoBox(dataset() , variable_name = "rent_minutes", type_statistic = "mean"))
    })
  median_minutes_value <- reactive({
    return(compute_statistic_to_infoBox(dataset(), variable_name = "rent_minutes", type_statistic = "median"))
  })
  count_rents_value <- reactive({
    return(compute_statistic_to_infoBox(dataset(), variable_name = "amount_rents", type_statistic = "count"))
  })
  count_bikes_value <- reactive({
    return(compute_statistic_to_infoBox(dataset(), variable_name = "bike_number", type_statistic = "count_distinct"))
  })
  
  output$mean_minutes <- renderInfoBox({
    infoBox(title = "Mean minutes",
            value = round(mean_minutes_value()),
            icon = icon("hourglass-start"),
            color = "yellow")
  })
  output$median_minutes <- renderInfoBox({
    infoBox(title = "Median minutes",
            value = round(median_minutes_value()),
            icon = icon("hourglass-start"),
            color = "yellow")
  })
  output$amount_rents <- renderInfoBox({
    infoBox(title = "Amount of rents",
            value = comma(count_rents_value()),
            icon = icon("bicycle"),
            color = "yellow")
  })
  output$amount_bikes <- renderInfoBox({
    infoBox(title = "Amount of bikes",
            value = count_bikes_value(),
            icon = icon("bicycle"),
            color = "yellow")
  })
}




