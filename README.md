# key_drivers_fun_app_example

Demo Key Drivers Project
This is a Shiny app that displays key drivers of NPS scores for selected accounts and year quarters. It uses several R packages to generate and visualize data, including dplyr, shiny, plotly, tidyverse, and glue, among others.

Installation
To run this app, you need to have R installed on your machine. You can then install the required packages by running the following command in the R console:
pacman::p_load(renv, dplyr, shiny, shinydashboard, shinydashboardPlus, tidyverse, tidymodels, glue, plotly, devtools, shinyWidgets, fontawesome, formattable, janitor, tsibble, DT, readr, lubridate, ggrepel)

source("app.R")

Usage
The app displays a dashboard page with a sidebar menu containing a single tab for Key Drivers. Within this tab, users can select one or more account names and year quarters from the drop-down menus provided. The app then generates a plot showing the key drivers of NPS scores for the selected accounts and year quarters.

Data Generation
The app uses two R Markdown files (data_generation_a360.Rmd and key_driver_function.Rmd) to generate the data and calculate the key drivers, respectively. These files are sourced in the main app.R file using the knitr::purl() function.

Dependencies
This app requires several R packages to run, including:

renv
dplyr
shiny
shinydashboard
shinydashboardPlus
tidyverse
tidymodels
glue
plotly
devtools
shinyWidgets
fontawesome
formattable
janitor
tsibble
DT
readr
lubridate
ggrepel
