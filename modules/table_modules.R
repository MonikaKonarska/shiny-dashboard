library(formattable)
library(data.table)
library(reshape2)
library(tidyr)
library(scales)
library(DT)
customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"

peakMonthsIndicatorsModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12, align = "center",
             formattableOutput(ns("table_of_minutes_months")))
      )
  )
}

peakPercentagesIndicatorsModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12, align = "center",
             formattableOutput(ns("performance_indocators_table")))
      )
  )
}

peakDataMonthsIndicatorsModuleServer <- function(input, output, session, dataset) {
  agregated_data <-  reactive({ 
    dataset()[, c("month_rent_start", "rent_minutes")] %>% 
      .[, .(Min = min(rent_minutes),
            Max = max(rent_minutes),
            Median = median(rent_minutes),
            Average = mean(rent_minutes),
            IQRange = IQR(rent_minutes)), by = month_rent_start]
    })
  return(agregated_data)
}
 
peakDataPercentagesIndicatorsServer <- function(input, output, session, dataset) {
  number_rents <- reactive({return(nrow(dataset()))})
  percentage_data <- reactive({
    dataset()[, c("month_rent_start", "is_payment_for_rent", "is_payment_for_return_place", "day_rent", "return_place")] %>%
      .[, .(
        `Percentage of rents` = round(sum(.N)/number_rents(), 3),
        `Percentage of rents with payment`= round(sum(ifelse(is_payment_for_rent == "Yes", 1, 0))/number_rents(), 3),
        `Percentage of payment for return place` = round(sum(ifelse(is_payment_for_return_place == "Yes", 1, 0))/number_rents(), 3),
        `Percentage of rents on weekend` = round(sum(ifelse(day_rent %in% c("Sunday", "Saturday"), 1, 0))/number_rents(), 3),
        `Percentage of rents on Monday` = round(sum(ifelse(day_rent == "Monday", 1, 0))/number_rents(), 3),
        `Percentage of rents on Tuesday` = round(sum(ifelse(day_rent == "Tuesday", 1, 0))/number_rents(), 3),
        `Percentage of rents on Wednesday` = round(sum(ifelse(day_rent == "Wednesday", 1, 0))/number_rents(), 3),
        `Percentage of rents on Thursday` = round(sum(ifelse(day_rent == "Thursday", 1, 0))/number_rents(), 3),
        `Percentage of rents on Friday` = round(sum(ifelse(day_rent == "Friday", 1, 0))/number_rents(), 3),
        `Percentage of returns place Rondo_Reagana` = round(sum(ifelse(return_place == "Rondo Reagana", 1, 0))/number_rents(), 3)
      ), by = month_rent_start] %>%
      as.data.frame() %>%
      melt(id.vars = "month_rent_start", variable.name =  "Indicator_name") %>%
      spread(month_rent_start, value)
    })
  return(percentage_data)
}
 
peakMonthsIndicatorsModuleServer <- function(input, output, session, dataset) {
  output$table_of_minutes_months <- renderFormattable({
  formattable(dataset(), list(
    `month_rent_start` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
    `Min` = color_tile(customGreen0, customGreen),
    `Max` = color_tile(customGreen0, customGreen),
    `Median` = color_tile(customGreen0, customGreen),
    `Average` = color_tile(customGreen0, customGreen),
    `IQRange` = color_tile(customGreen0, customGreen)
    ))
    })    
}

peakPercentagesIndicatorsServer <- function(input, output, session, dataset) {
  output$performance_indocators_table <- renderFormattable({
    number_cols <- reactive({ncol(dataset())})
   formattable(dataset(),
                 align = c("r", rep("c", number_cols() - 1), "r"),
                 list(
                   "Indicator_name" = formatter("span", style = ~ style(color = "grey", font.weight = "bold")),
                      area(col = 2:number_cols(), row = 1) ~ color_bar("#71CA97"),
                      area(col = 2:number_cols(), row = 2:3) ~ color_bar("#3E7DCC"),
                      area(col = 2:number_cols(), row = 4:9) ~ color_bar("#FA614B66"),
                      area(col = 2:number_cols(), row = 10) ~ color_bar("yellow")
                 ))
   })
}

