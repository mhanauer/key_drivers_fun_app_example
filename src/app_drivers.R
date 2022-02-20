library(pacman)
pacman::p_load(renv, dplyr, shiny, shinydashboard, shinydashboardPlus, tidyverse, glue, plotly, devtools, shinyWidgets, fontawesome, formattable, janitor, tsibble, DT, readr, lubridate, tidymodels, ggrepel)

setwd("~/key_driver_fun_app_example/src")
source(knitr::purl("data_generation_a360.rmd", quiet = TRUE))
source(knitr::purl("key_driver_function.rmd", quiet = TRUE))


nps_data <- nps_data %>%
  mutate(year_quarter = as.character(year_quarter))

ui <- dashboardPage(
  dashboardHeader(title = "Demo Key Drivers and KPIs"),
  dashboardSidebar(sidebarMenu(
    menuItem("Key Drivers", tabName = "key_drivers"),
    menuItem("KPIs", tabName = "kpis"),
    menuItem("App details", tabName = "app_details")
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
        fluidRow(
          plotOutput("key_drivers")
        )
      ), 
      tabItem(
        tabName = "kpis", 
        fluidRow(
          column(
          3,
          pickerInput(
            inputId = "account_names_chatter",
            label = "Account name",
            # placeholder is enabled when 1st choice is an empty string
            choices = c(unique(chatter_data$account_names_chatter)),
            multiple = TRUE,
            options = list(`actions-box` = TRUE),
            selected = c(unique(chatter_data$account_names_chatter))
          )
        ),
        column(
          3,
          pickerInput(
            inputId = "year_quarter_chatter",
            label = "Year quarter",
            # placeholder is enabled when 1st choice is an empty string
            choices = c(unique(chatter_data$year_quarter_chatter)),
            multiple = TRUE,
            options = list(`actions-box` = TRUE),
            selected = c(unique(chatter_data$year_quarter_chatter))
            )
          )
        ), 
        fluidRow(valueBoxOutput("absolute_chatter"), 
                 valueBoxOutput("change_chatter"))
      ),
      tabItem(
        tabName = "app_details",
        fluidRow(
          box(strong("Please see key driver function on github for more details: https://github.com/mhanauer/key_drivers_fun_app_example")
          )
        )
      )
    )
  )
)
server <- function(input, output) {
  output$key_drivers <- renderPlot({
    nps_data <- nps_data %>%
      filter(account_names %in% input$account_names) %>%
      filter(year_quarter %in% input$year_quarter) %>%
      select(-c(account_names, year_quarter))

    key_drivers_data <- key_driver_function(data = nps_data, outcome = "nps")

    viz_key_driver_function(key_drivers_data)
    }
  )
}

shinyApp(ui, server)
