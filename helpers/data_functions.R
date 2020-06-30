

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
