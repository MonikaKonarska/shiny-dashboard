library(dplyr)
library(data.table)
library(stringr)
library(lubridate)
source(file.path(getwd(), "helpers", "data_functions.R"))

data_path <- file.path(getwd(), "data", "dataDT.rds")

if(file.exists(data_path)) {
  dataDT <- readRDS(file = data_path) 
  districts <- read.csv(file.path(getwd(), "data", "district_Wroclaw.csv"), sep = ';')
  stations <- read.csv(file.path(getwd(), "data", "station_coordinates.csv"), sep = ";")
} else {
  id_stations <- read.csv(file.path(getwd(), "data", "id_stations.csv"), sep = ';', stringsAsFactors = FALSE)
  data201903 <- download_data_from_website(url = "https://www.wroclaw.pl/open-data/dataset/a646ce34-b24a-4f5c-8174-71dfc1bd2d0b/resource/30d9e087-bafd-441e-aa09-f20a11fc60f5/download/historia_przejazdow_2019-03.csv")
  data201904 <- download_data_from_website(url = "https://www.wroclaw.pl/open-data/dataset/a646ce34-b24a-4f5c-8174-71dfc1bd2d0b/resource/1175d9da-3ae2-4ebc-8b1c-1f13fb5bf7b0/download/historia_przejazdow_2019-04.csv")
  data201905 <- download_data_from_website(url = "https://www.wroclaw.pl/open-data/dataset/a646ce34-b24a-4f5c-8174-71dfc1bd2d0b/resource/a24161cc-3974-4741-9ddf-fe8663fa641b/download/historia_przejazdow_2019-05.csv")
  data201906 <- download_data_from_website(url = "https://www.wroclaw.pl/open-data/dataset/a646ce34-b24a-4f5c-8174-71dfc1bd2d0b/resource/4a63e438-2c73-4fe3-9599-21c319887a85/download/historia_przejazdow_2019-06.csv")
  data201907 <- download_data_from_website(url = "https://www.wroclaw.pl/open-data/dataset/a646ce34-b24a-4f5c-8174-71dfc1bd2d0b/resource/aa5da036-daad-480b-93c2-f1fbfda9b663/download/historia_przejazdow_2019-07.csv")  
  data201908 <- download_data_from_website(url = "https://www.wroclaw.pl/open-data/dataset/a646ce34-b24a-4f5c-8174-71dfc1bd2d0b/resource/5812256f-d0a5-43dd-850b-b8e8d13fe557/download/historia_przejazdow_2019-08.csv")
  data201909 <- download_data_from_website(url = "https://www.wroclaw.pl/open-data/dataset/a646ce34-b24a-4f5c-8174-71dfc1bd2d0b/resource/673bec03-a145-46ee-a302-9eeabec53728/download/historia_przejazdow_2019-09.csv")
  data201910 <- download_data_from_website(url = "https://www.wroclaw.pl/open-data/dataset/a646ce34-b24a-4f5c-8174-71dfc1bd2d0b/resource/31b7a6c6-d0e1-4818-9677-f66cb4809803/download/historia_przejazdow_2019-10.csv")
  
  data <- dplyr::bind_rows(data201903, data201904, data201905,  data201906, data201907, data201908, data201909, data201910) %>%
    select(-one_of("X"))
                           
  dataDT <- data %>%
    mutate_if(str_detect(colnames(.), "time"), as_datetime) %>%
    mutate(rent_seconds = time_length(end_time - start_time, 'seconds'),
           rent_minutes = round(time_length(end_time - start_time, 'minutes'),2),
           rent_hours = round(time_length(end_time - start_time, 'hour'),2),
           rent_days = round(time_length(end_time - start_time, 'day')),
           start_date = as.Date(start_time),
           end_date = as.Date(end_time),
           month_rent_start = month(start_time, label = TRUE, abbr = FALSE),
           month_rent_end = month(end_time, label = TRUE, abbr = FALSE),
           hour_rent_start = hour(start_time),
           hour_rent_end = hour(end_time),
           day_rent = wday(start_time, label = TRUE, abbr = FALSE, locale = Sys.setlocale("LC_TIME", "English")),
           is_payment_for_rent = case_when(rent_minutes >= 20 ~ "Yes", TRUE ~ "No"),
           is_payment_for_return_place = case_when(str_detect(return_place, "Poza stacj") == TRUE ~ "Yes", TRUE ~ "No" ),
           is_rent_less60seconds = case_when(rent_seconds<=60 ~ "Yes", TRUE ~ "No"),
           is_fake_rent = case_when(((rental_place == return_place) & (str_detect(rental_place, "Poza stacj") & str_detect(return_place, "Poza stacj")) & is_rent_less60seconds == "Yes") ~ "Yes",
                                    TRUE ~ "No"),
           rent_with_more_1day <- case_when(rent_hours > 24 ~ "Yes", TRUE ~ "No"),
           is_outlier = is_outlier(rent_minutes)
    ) %>% setDT()
  
  dataDT <- dataDT[which(dataDT$month_rent_start != "February"), ]
  dataDT <- dataDT %>%
    filter(!str_trim(rental_place) %in% c("# Rowery skradzione Wrocław 2014", "#Rowery zapasowe Warszawa", ".GOTOWE DO REZERWACJI", ".RELOKACYJNA", ".RELOKACYJNA A1-4", ".SERWIS - ŁADOWANIE", "55555", "Młynarska/Wąska", "NIOL test", "Fabryczna  (WSB)")
           ) 
  dataDT$rental_place_id <- str_trim(dataDT$rental_place)
  dataDT$return_place_id <- str_trim(dataDT$return_place)
  id_stations$rental_place <- str_trim(id_stations$rental_place)

  dataDT <- dataDT %>%
    left_join(id_stations, by = c("rental_place_id" = "rental_place")) %>%
    rename("id_station_rental" = "id_station") %>%
    left_join(id_stations, by = c("return_place_id" = "rental_place")) %>%
    rename("id_station_return" = "id_station")
  
  dataDT <-   dataDT %>%
    filter(!is.na(id_station_rental))%>%
    filter(!is.na(id_station_return))

  dataDT$rn <- seq.int(nrow(dataDT))
  dataDT <- as.data.table(dataDT)
  saveRDS(dataDT, file = data_path)
}
