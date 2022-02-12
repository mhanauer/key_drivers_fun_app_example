library(pacman)
pacman::p_load(renv, dplyr, shiny, shinydashboard, shinydashboardPlus, tidyverse, glue, plotly, devtools, shinyWidgets, fontawesome, formattable, janitor, tsibble, DT, readr, lubridate, tidymodels, ggrepel)

setwd("~/shiny_360_app_example/src")
source(knitr::purl("data_generation_a360.rmd", quiet = TRUE))
nps_data = nps_data %>%
  mutate(year_quarter = as.character(year_quarter))

ui <- dashboardPage(
  dashboardHeader(title = "Demo Key Drivers and KPI Dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem("Key Drivers", tabName = "key_drivers"),
    menuItem("KPIs", tabName = "kpis")
  )),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "key_drivers",
        fluidRow(
          column(
            3,
            pickerInput(
              inputId = "account_names", label = "Account name",
              choices = c(unique(nps_data$account_names)),
              multiple = TRUE,
              options = list(`actions-box` = TRUE),
              selected = unique(nps_data$account_names)
            )
          ),
          column(
            3,
            pickerInput(
              inputId = "year_quarter", label = "Year quarter",
              choices = unique(nps_data$year_quarter), multiple = TRUE,
              options = list(`actions-box` = TRUE),
              select = unique(nps_data$year_quarter)
            )
          )
        ), 
        fluidRow(box(
          plotOutput("key_drivers")
          )
        )
      )
    )
  )
)
server <- function(input, output) {

}

shinyApp(ui, server)
