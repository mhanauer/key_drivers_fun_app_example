library(pacman)
pacman::p_load(renv, dplyr, shiny, shinydashboard, shinydashboardPlus, tidyverse, glue, plotly, devtools, shinyWidgets, fontawesome, formattable, janitor, tsibble, DT, readr, lubridate, tidymodels, ggrepel)

source(knitr::purl(glue("data_generation_a360.Rmd")))
source(knitr::purl(glue("key_driver_function.Rmd")))


nps_data <- nps_data %>%
  mutate(year_quarter = as.character(year_quarter))

ui <- dashboardPage(
  dashboardHeader(title = "Demo Key Drivers and KPIs"),
  dashboardSidebar(sidebarMenu(
    menuItem("Key Drivers", tabName = "key_drivers")
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
  })
}

shinyApp(ui, server)
