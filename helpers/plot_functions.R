library(ggplot2)
library(data.table)


number_of_rentals_plot <- function(dataset, x_variable_name = "start_date") {
  
  x <- sym(x_variable_name)
  
  data <- dataset %>%
    group_by(!!x) %>%
    dplyr::summarise(number_rentals = n())

  p <- ggplot(data, aes(x = !!x, y = number_rentals)) +
    geom_line(color = "steelblue", size = 1) +
    scale_x_date(date_labels = "%B")+
    scale_y_continuous(name = "Amount of rents", labels = scales::comma)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}


