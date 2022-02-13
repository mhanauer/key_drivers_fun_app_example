library(pacman)
pacman::p_load(renv, dplyr, shiny, shinydashboard, shinydashboardPlus, tidyverse, glue, plotly, devtools, shinyWidgets, fontawesome, formattable, janitor, tsibble, DT, readr, lubridate, tidymodels, ggrepel)

setwd("~/shiny_360_app_example/src")
source(knitr::purl("data_generation_a360.rmd", quiet = TRUE))
source(knitr::purl("key_driver_function.rmd", quiet = TRUE))


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
        fluidRow(
          plotOutput("key_drivers")
        )
      )
    )
  )
)
server <- function(input, output) {
output$key_drivers  = renderPlot({
  
nps_data = nps_data %>%
  filter(account_names %in% input$account_names) %>%
  filter(year_quarter %in% input$year_quarter) %>%
  select(-c(account_names, year_quarter))
  
key_drivers_data = key_driver_function(data = nps_data, outcome = "nps")

x_max = max(nps_data$importance)
x_min = min(key_drivers_data$importance)
y_max = max(key_drivers_data$positive)
y_min = min(key_drivers_data$positive)


title = paste0("Key drivers", " ", "N = ", " ", dim(key_drivers_data)[1])

kd <- ggplot(key_drivers_data,aes(importance, positive, label = variable))+
  geom_point()+
  ylim(y_min- y_min*.1, y_max + y_max*.1) +
  xlim(x_min - x_min*.1, x_max + x_max*.1) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x = "Importance", y =  "Favorable") +
  geom_hline(yintercept=median(key_drivers_data$positive))+
  geom_vline(xintercept=median(key_drivers_data$importance)) +
  labs(x = "Importance", y =  "Favorable") +
  ggtitle(title) +
  annotate(geom="text", x = x_max , y= y_max, label="Key strength",
           color="#00B0F0") +
  annotate(geom="text", x = x_min , y = y_max, label="Unimportant strength",
           color="#808080") +
  annotate(geom="text", x=  x_max, y=y_min, label="Important opportunity",
           color="#FF0000") +
  annotate(geom="text", x=  x_min, y= y_min, label= "Unimportant opportunity",
           color="#808080") + 
  geom_text_repel(data = key_drivers_data %>% filter(color == "red"), aes(label = variable),
                  size = 3.5,  color = "#FF0000") +
  geom_text_repel(data = key_drivers_data %>% filter(color == "blue"), aes(label = variable),
                  size = 3.5,  color = "#00B0F0") +
  geom_text_repel(data = key_drivers_data %>% filter(color == "grey"), aes(label = variable),
                  size = 3.5,  color = "#808080") +
  coord_flip()
kd
      }
    )
  }

shinyApp(ui, server)
