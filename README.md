# **Interactive dashboard in Shiny**

Project Status: 

- [x] Active
- [ ] On-Hold
- [ ] Completed


![In Progress](helpers/app_in_progress/Screenshot_app.png)

![](helpers/app_in_progress/Screenshot_maps.png)


## Project Intro/Objective

The purpose of this project is to build the analytical dashboard in Shiny.
The app presents information about the rental bikes in Wrocław in 2019. I decided to use the [modules](https://github.com/MonikaKonarska/shiny-dashboard/tree/master/modules) to manage the growing complexity of Shiny application code.


## Used 

- Shiny modules  
- shinycssloaders   
- shinydashboard   
- sf :globe_with_meridians:
- sp :globe_with_meridians:
- ggplot2 :bar_chart:
- plotly :bar_chart:
- tidyverse
- formattable


## Data

- spatial data of city :point_right: [here](https://geoportal.wroclaw.pl/osiedla/)   
- rental bikes data :point_right:  [here](https://www.wroclaw.pl/open-data/dataset/przejazdy-wroclawskiego-roweru-miejskiego-archiwalne)   



### How to run ?
You can start with the [`import_data.R`](https://github.com/MonikaKonarska/shiny-dashboard/blob/master/data/import_data.R) script and after that run the [`app.R`](https://github.com/MonikaKonarska/shiny-dashboard/blob/master/app.R).
