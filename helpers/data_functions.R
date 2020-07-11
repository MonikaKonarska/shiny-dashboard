

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}


download_data_from_website <- function(url){
  
  if(!require(RCurl)) {
    install.packages("RCurl")
  } else {library(RCurl)}
  
  get_url_data <- RCurl::getURL(url)
  data <- read.csv(textConnection(get_url_data), encoding = "UTF-8", stringsAsFactors=FALSE)
  return(data)
}



compute_statistic_to_infoBox <- function(dataset, variable_name, type_statistic = c("mean", "median", "count", "count_distinct")){
  statistic <- match.arg(type_statistic)
  
  if(statistic == "count"){
    value <- nrow(dataset)
  } else if (statistic == "count_distinct") {
    value <- length(dataset[, unique(get(variable_name))])
  } else {
  value <- dataset[, get(statistic)(get(variable_name))]
  }
  return(value)
}




