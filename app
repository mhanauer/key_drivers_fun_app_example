library(shiny)
library(shinydashboard)
library(tidyverse)
library(glue)
library(plotly)
library(devtools)
library(shinydashboardPlus)
library(shiny.semantic)
library(shinyWidgets)
library(fontawesome)
library(formattable)
library(janitor)
library(tsibble)
library(DT)
library(readr)
library(styler)
library(lubridate)
library(tidymodels)
library(vip)
library(ggrepel)

### Buy - Chatter by account data
setwd("/data/data_for_analysis/matthew.hanauer/a360_complaints_sales/versioned_data")
account_task_feed_tbl_data <- read_csv("account_task_feed_tbl_data.csv")
### Own -Call logs call time data
setwd("/data/data_for_analysis/matthew.hanauer/a360_complaints_sales/versioned_data/call_log")
call_logs_data_linked <- read_csv("call_logs_data_linked.csv")
### Need to rename the filters so the app doesn't get confused
call_logs_data_linked <- call_logs_data_linked %>%
  rename(acct_name_call_logs_time = acct_name, network_name_call_logs_time = network_name, network_parent_name_call_logs_time = network_parent_name)
### Buy - Account news
setwd("/data/data_for_analysis/matthew.hanauer/a360_complaints_sales/versioned_data")
defhc_news_clean <- read_csv("defhc_news_clean.csv")
defhc_news_clean <- defhc_news_clean %>%
  rename(acct_name_news = acct_name, network_name_news = network_name, network_parent_name_news = network_parent_name)

### Load in the fake customer research panel data
setwd("/data/data_for_analysis/matthew.hanauer/a360_complaints_sales/versioned_data")
data_customer_panel <- read_csv("data_customer_panel.csv")

### Create the function grouping the sentiment.
sentiment_format_fun <- function(account_task_feed_tbl_data) {
  data_out <- account_task_feed_tbl_data %>%
    dplyr::group_by(year_quarter) %>%
    summarise(chatter_sentiment_mean = mean(chatter_sentiment), chatter_n = n()) %>%
    mutate(chatter_sentiment_mean = round(chatter_sentiment_mean, 2)) %>%
    mutate(chatter_sentiment_pct_change = tsibble::difference(chatter_sentiment_mean) / (ifelse(lag(chatter_sentiment_mean) == 0, 1, lag(chatter_sentiment_mean)))) %>%
    mutate(chatter_sentiment_pct_change = formattable::percent(chatter_sentiment_pct_change, digits = 0))
}

### Create a function for the call logs
call_logs_time_format_fun <- function(data) {
  data_out <- data %>%
    dplyr::group_by(year_quarter) %>%
    dplyr::summarise(hold_time_mean = mean(hold_time), total_time_mean = mean(total_time, na.rm = TRUE)) %>%
    mutate(hold_time_pct_change = tsibble::difference(hold_time_mean) / (ifelse(lag(hold_time_mean) == 0, 1, lag(hold_time_mean)))) %>%
    mutate(hold_time_pct_change = formattable::percent(hold_time_pct_change, digits = 0)) %>%
    mutate(total_time_pct_change = tsibble::difference(total_time_mean) / (ifelse(lag(total_time_mean) == 0, 1, lag(total_time_mean)))) %>%
    mutate(total_time_pct_change = formattable::percent(total_time_pct_change, digits = 0)) %>%
    mutate_at(c("hold_time_mean", "total_time_mean"), funs(round(., 0))) %>%
    mutate_at(c("hold_time_mean", "total_time_mean"), seconds_to_period)
  return(data_out)
}

#### Account news function
graph_type_format_fun <- function(data, var) {
  data_clean <- data %>%
    group_by({{ var }}) %>%
    dplyr::summarise(count_news_event_type = n()) %>%
    mutate(count = formattable::accounting(count_news_event_type, digits = 0)) %>%
    mutate(percentage = formattable::percent(count_news_event_type / sum(count_news_event_type), digits = 0)) %>%
    arrange(desc(percentage)) %>%
    mutate(cummulative_sum = cumsum(percentage))

  data_clean_agg <- data_clean %>%
    mutate(threshold = if_else(cummulative_sum < .55, 1, 0)) %>%
    mutate(threshold = as.factor(threshold)) %>%
    mutate(remove = if_else(percentage > .05, 1, 0)) %>%
    filter(remove == 1)

  return(data_clean_agg)
}

ui <- dashboardPage(
  dashboardHeader(title = "A360 App"),
  dashboardSidebar(sidebarMenu(
    menuItem("Buy - Chatter by account", tabName = "buy_chatter"),
    menuItem("Own - Call logs call time", tabName = "call_logs_call_time"),
    menuItem("Buy - Account news", tabName = "account_news"),
    menuItem("Advocate - Customer panel", tabName = "customer_panel"),
    menuItem("App details", tabName = "app_details")
  )),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "buy_chatter",
        fluidRow(
          column(
            3,
            pickerInput(
              inputId = "acct_name",
              label = "Account name",
              # placeholder is enabled when 1st choice is an empty string
              choices = c(unique(account_task_feed_tbl_data$acct_name)),
              multiple = TRUE,
              options = list(`actions-box` = TRUE),
              selected = c(unique(account_task_feed_tbl_data$acct_name))
            )
          ),
          column(
            3,
            pickerInput(
              inputId = "network_name",
              label = "Network name",
              # placeholder is enabled when 1st choice is an empty string
              choices = c(unique(account_task_feed_tbl_data$network_name)),
              multiple = TRUE,
              options = list(`actions-box` = TRUE),
              selected = c(unique(account_task_feed_tbl_data$network_name))
            )
          ),
          column(
            3,
            pickerInput(
              inputId = "network_parent_name",
              label = "Parent Network",
              # placeholder is enabled when 1st choice is an empty string
              choices = c(unique(account_task_feed_tbl_data$network_parent_name)),
              multiple = TRUE,
              options = list(`actions-box` = TRUE),
              selected = c(unique(account_task_feed_tbl_data$network_parent_name))
            )
          )
        ),
        fluidRow(
          valueBoxOutput("absolute_sales_chatter"),
          valueBoxOutput("percent_sales_chatter")
        ),
        fluidRow(
          box(
            formattableOutput("table_agg_sales_chatter")
          ),
          box(
            dataTableOutput("table_chatter_sales_chatter")
          )
        )
      ),
      tabItem(
        tabName = "call_logs_call_time",
        fluidRow(
          column(
            3,
            pickerInput(
              inputId = "acct_name_call_logs_time",
              label = "Account name",
              # placeholder is enabled when 1st choice is an empty string
              choices = c(unique(call_logs_data_linked$acct_name_call_logs_time)),
              multiple = TRUE,
              options = list(`actions-box` = TRUE),
              selected = c(unique(call_logs_data_linked$acct_name_call_logs_time))
            )
          ),
          column(
            3,
            pickerInput(
              inputId = "network_name_call_logs_time",
              label = "Network name",
              # placeholder is enabled when 1st choice is an empty string
              choices = c(unique(call_logs_data_linked$network_name_call_logs_time)),
              multiple = TRUE,
              options = list(`actions-box` = TRUE),
              selected = c(unique(call_logs_data_linked$network_name_call_logs_time))
            )
          ),
          column(
            3,
            pickerInput(
              inputId = "network_parent_name_call_logs_time",
              label = "Parent Network",
              # placeholder is enabled when 1st choice is an empty string
              choices = c(unique(call_logs_data_linked$network_parent_name_call_logs_time)),
              multiple = TRUE,
              options = list(`actions-box` = TRUE),
              selected = c(unique(call_logs_data_linked$network_parent_name_call_logs_time))
            )
          ),
          column(
            3,
            pickerInput(
              inputId = "wrap_up_data",
              label = "Wrap up code",
              # placeholder is enabled when 1st choice is an empty string
              choices = c(unique(call_logs_data_linked$wrap_up_data)),
              multiple = TRUE,
              options = list(`actions-box` = TRUE),
              selected = c(unique(call_logs_data_linked$wrap_up_data))
            )
          )
        ),
        fluidRow(
          valueBoxOutput("absolute_hold_time_call_logs"),
          valueBoxOutput("percent_hold_time_call_logs"),
          valueBoxOutput("absolute_total_time_call_logs"),
          valueBoxOutput("percent_total_time_call_logs")
        ),
        fluidRow(
          box(
            formattableOutput("table_hold_call_logs")
          ),
          box(plotOutput("graph_wrap_up"))
        )
      ),
      tabItem(
        tabName = "account_news",
        fluidRow(
          column(
            3,
            pickerInput(
              inputId = "acct_name_news",
              label = "Account name",
              # placeholder is enabled when 1st choice is an empty string
              choices = c(unique(defhc_news_clean$acct_name_news)),
              multiple = TRUE,
              options = list(`actions-box` = TRUE),
              selected = c(unique(defhc_news_clean$acct_name_news))
            )
          ),
          column(
            3,
            pickerInput(
              inputId = "network_name_news",
              label = "Network name",
              # placeholder is enabled when 1st choice is an empty string
              choices = c(unique(defhc_news_clean$network_name_news)),
              multiple = TRUE,
              options = list(`actions-box` = TRUE),
              selected = c(unique(defhc_news_clean$network_name_news))
            )
          ),
          column(
            3,
            pickerInput(
              inputId = "network_parent_name_news",
              label = "Parent Network",
              # placeholder is enabled when 1st choice is an empty string
              choices = c(unique(defhc_news_clean$network_parent_name_news)),
              multiple = TRUE,
              options = list(`actions-box` = TRUE),
              selected = c(unique(defhc_news_clean$network_parent_name_news))
            )
          ),
          column(
            3,
            pickerInput(
              inputId = "year_quarter",
              label = "Year quarter",
              # placeholder is enabled when 1st choice is an empty string
              choices = c(unique(defhc_news_clean$year_quarter)),
              multiple = TRUE,
              options = list(`actions-box` = TRUE),
              selected = c(unique(defhc_news_clean$year_quarter))
            )
          )
        ),
        fluidRow(
          box(plotOutput("news_plot")),
          box(dataTableOutput("news_table"))
        )
      ),
      tabItem(
        tabName = "customer_panel",
        fluidRow(
          column(
            3,
            pickerInput(
              inputId = "role",
              label = "Role",
              # placeholder is enabled when 1st choice is an empty string
              choices = c(unique(data_customer_panel$role)),
              multiple = TRUE,
              options = list(`actions-box` = TRUE),
              selected = c(unique(data_customer_panel$role))
            )
          )
        ),
        fluidRow(
          plotOutput("customer_panel_drivers")
        )
      ),
      tabItem(
        tabName = "app_details",
        fluidRow(
          box(strong("Scope"), br(), "To provide KPIs and benchmarks where possible for account chatter sentiment, call logs, and account-based news.", br(), br(), strong("Definitions"), br(), em("All sheets"), br(), br(), "Year quarter: The year and quarter the data was captured.", br(), "Account name: The name of the account from Salesforce.", br(), "Network: Name of the Health System that has ownership or operational control over the facility.", br(), "Network: Name of the Health System that has ownership or operational control over the facility.", br(), br(), em("Buy – Chatter by account"), br(), br(), "Sentiment: Sentiment is the positive or negative valance found in the chatter by account.  Values above 0 indicate positive sentiment and values below 0 indicate negative sentiment.  Values generally range between .5 and -.5 but can go above / below those thresholds.", br(), br(), em("Own – Call logs call time"), br(), br(), "Hold time: Time Cook Medical rep puts the customer on hold.", br(), "Total time: Total time Cook Medical rep spends with customer on phone.", br(), "Wrap up code: The Cook Medical rep’s call description.", br(), br(), strong("Who can use this app?"), br(), "TBD"),
          box(strong("Filter Clarity"), br(), "For the customer id filters (e.g., account name, network name, and parent network name) we linked account numbers (i.e., A numbers) found in Salesforce and used Definitive Healthcare to map the networks and parent networks to the facilities (i.e., accounts).  For example, when you select Kaiser Permanente in the parent network name filter, you are selecting all the facilities, networks and the one parent network associated with Kaiser Permanente.", br(), br(), strong("Sheet advocates"), br(), "TBD", br(), br(), strong("Exclusions"), br(), em("Buy – Chatter by account"), br(), "The graph in the lower right corner has the chatter organized by negative to positive sentiment and most to least recent.  Chatter is stripped of symbols and all Open Order and Pre-Meeting notes tagged (i.e., the term Open Order or Pre-Meeting were used in sequence) were removed.", br(), br(), em("Own – Call logs call time"), br(), "For the customer id filters (e.g., account name, network name, and parent network name), we linked the customer numbers in the data.  We then attempt to link the customer numbers with Cook’s data (e.g., Global Customer File) and link the matching customer numbers.  We then linked the matching customer numbers with the Salesforce database.  Therefore, if a customer number is inputted in correctly, none was provided, or it has not been mapped to an account, the account, network, and parent network name will show up as none.", br(), br(), "Data missing a session id or date were dropped.")
        )
      )
    )
  )
)

server <- function(input, output) {
  output$absolute_sales_chatter <- renderValueBox({
    value_data <- account_task_feed_tbl_data %>%
      filter(acct_name %in% input$acct_name) %>%
      filter(network_name %in% input$network_name) %>%
      filter(network_parent_name %in% input$network_parent_name)


    value_data <- sentiment_format_fun(value_data)

    title_out <- paste0(if_else(tail(value_data$chatter_sentiment_mean, 1) > 0, "Is above", if_else(tail(value_data$chatter_sentiment_mean, 1) == 0, "The same", "Is below")), " ", "the benchmark of 0 for average account chatter sentiment for", " ", tail(value_data$year_quarter, 1))
    value_out <- tail(value_data$chatter_sentiment_mean, 1)
    color_select <- if_else(value_out >= 0, "blue", "red")
    icon_select <- if_else(value_out >= 0, "- fas fa-arrow-up", "- fas fa-arrow-down")
    shinydashboard::valueBox(value_out, title_out, icon = tags$i(class = icon_select, style = "font-size: 75px"), color = color_select)
  })
  output$percent_sales_chatter <- renderValueBox({
    value_data <- account_task_feed_tbl_data %>%
      filter(acct_name %in% input$acct_name) %>%
      filter(network_name %in% input$network_name) %>%
      filter(network_parent_name %in% input$network_parent_name)

    value_data <- sentiment_format_fun(value_data)

    title_out <- paste0(if_else(tail(value_data$chatter_sentiment_pct_change, 1) > 0, "Increase", if_else(tail(value_data$chatter_sentiment_pct_change, 1) == 0, "No", "Decrease")), " ", "in average account chatter sentiment from", " ", tail(value_data$year_quarter, 2)[1], " ", "to", " ", tail(value_data$year_quarter, 1))
    value_out <- tail(value_data$chatter_sentiment_pct_change, 1)
    color_select <- if_else(value_out >= 0, "blue", "red")
    icon_select <- if_else(value_out >= 0, "- fas fa-arrow-up", "- fas fa-arrow-down")
    shinydashboard::valueBox(value_out, title_out, icon = tags$i(class = icon_select, style = "font-size: 75px"), color = color_select)
  })
  output$table_agg_sales_chatter <- renderFormattable({
    value_data <- account_task_feed_tbl_data %>%
      filter(acct_name %in% input$acct_name) %>%
      filter(network_name %in% input$network_name) %>%
      filter(network_parent_name %in% input$network_parent_name)
    value_data <- sentiment_format_fun(value_data)
    value_data <- value_data %>%
      dplyr::rename("Year quarter" = year_quarter, "Account chatter sentiment mean" = chatter_sentiment_mean, "Account chatter sentiment % change" = chatter_sentiment_pct_change, "Number of chatter posts" = chatter_n)

    customBlue <- "#136BBD"
    customRed <- "#C22017"

    improvement_formatter <- formatter("span",
      style = x ~ formattable::style(
        font.weight = "bold",
        color = ifelse(x > 0, customBlue, ifelse(x < 0, customRed, "grey"))
      ),
      x ~ icontext(if_else(x > 0, "arrow-up", if_else(x == 0, "equal-solid", "arrow-down")), x)
    )
    improvement_indicator <- formatter("span",
      style = x ~ formattable::style(
        font.weight = "bold",
        color = ifelse(x > 0, customBlue, ifelse(x < 0, customRed, "grey"))
      )
    )
    formattable(value_data,
      align = c("l", "c", "c", "r"),
      list(
        `Year quarter` = formatter(
          "span",
          style = ~ formattable::style(color = "grey", font.weight = "bold")
        ),
        `Account chatter sentiment % change` = improvement_formatter,
        `Account chatter sentiment mean` = improvement_indicator
      )
    )
  })

  output$table_chatter_sales_chatter <- renderDataTable(
    datatable(account_task_feed_tbl_data %>%
      filter(acct_name %in% input$acct_name) %>%
      filter(network_name %in% input$network_name) %>%
      filter(network_parent_name %in% input$network_parent_name) %>%
      arrange(desc(year_quarter), chatter_sentiment) %>%
      select(text, year_quarter, chatter_sentiment) %>%
      rename("Chatter" = text, "Year quarter" = year_quarter, "Avg chatter sentiment" = chatter_sentiment), rownames = FALSE, options = list(pageLength = 4), caption = "Chatter by account organized from most negative to positive and most to least recent.") %>%
      formatStyle("Avg chatter sentiment", color = styleInterval(c(0), c("#C22017", "#136BBD")), fontWeight = "bold")
  )

  output$absolute_hold_time_call_logs <- renderValueBox({
    value_data <- call_logs_data_linked %>%
      filter(acct_name_call_logs_time %in% input$acct_name_call_logs_time) %>%
      filter(network_name_call_logs_time %in% input$network_name_call_logs_time) %>%
      filter(network_parent_name_call_logs_time %in% input$network_parent_name_call_logs_time) %>%
      filter(wrap_up_data %in% input$wrap_up_data)


    value_data <- call_logs_time_format_fun(value_data)

    title_out <- paste0(if_else(tail(value_data$hold_time_mean, 1) < seconds(26), "Is below", if_else(tail(value_data$hold_time_mean, 1) == 0, "The same", "Is above")), " ", "the benchmark of 26 seconds for hold time", " ", "in", " ", tail(value_data$year_quarter, 1))
    value_out <- tail(value_data$hold_time_mean, 1)
    color_select <- if_else(value_out <= seconds(26), "blue", "red")
    icon_select <- if_else(value_out > seconds(26), "- fas fa-arrow-up", "- fas fa-arrow-down")
    shinydashboard::valueBox(value_out, title_out, icon = tags$i(class = icon_select, style = "font-size: 75px"), color = color_select)
  })

  output$percent_hold_time_call_logs <- renderValueBox({
    value_data <- call_logs_data_linked %>%
      filter(acct_name_call_logs_time %in% input$acct_name_call_logs_time) %>%
      filter(network_name_call_logs_time %in% input$network_name_call_logs_time) %>%
      filter(network_parent_name_call_logs_time %in% input$network_parent_name_call_logs_time) %>%
      filter(wrap_up_data %in% input$wrap_up_data)

    value_data <- call_logs_time_format_fun(value_data)

    title_out <- paste0(if_else(tail(value_data$hold_time_pct_change, 1) < 0, "Decrease", if_else(tail(value_data$hold_time_pct_change, 1) == 0, "No", "Increase")), " ", "in average hold time from", " ", tail(value_data$year_quarter, 2)[1], " ", "to", " ", tail(value_data$year_quarter, 1))
    value_out <- tail(value_data$hold_time_pct_change, 1)
    color_select <- if_else(value_out <= 0, "blue", "red")
    icon_select <- if_else(value_out < 0, "- fas fa-arrow-down", "- fas fa-arrow-up")
    shinydashboard::valueBox(value_out, title_out, icon = tags$i(class = icon_select, style = "font-size: 75px"), color = color_select)
  })

  output$absolute_total_time_call_logs <- renderValueBox({
    value_data <- call_logs_data_linked %>%
      filter(acct_name_call_logs_time %in% input$acct_name_call_logs_time) %>%
      filter(network_name_call_logs_time %in% input$network_name_call_logs_time) %>%
      filter(network_parent_name_call_logs_time %in% input$network_parent_name_call_logs_time) %>%
      filter(wrap_up_data %in% input$wrap_up_data)

    value_data <- call_logs_time_format_fun(value_data)

    title_out <- paste0(if_else(tail(value_data$total_time_mean, 1) < minutes(6), "Is below", if_else(tail(value_data$total_time_mean, 1) == 0, "The same", "Is above")), " ", "the benchmark of 6 minutes for total time", " ", "in", " ", tail(value_data$year_quarter, 1))
    value_out <- tail(value_data$total_time_mean, 1)
    color_select <- if_else(value_out <= minutes(6), "blue", "red")
    icon_select <- if_else(value_out > minutes(6), "- fas fa-arrow-up", "- fas fa-arrow-down")
    shinydashboard::valueBox(value_out, title_out, icon = tags$i(class = icon_select, style = "font-size: 75px"), color = color_select)
  })

  output$percent_total_time_call_logs <- renderValueBox({
    value_data <- call_logs_data_linked %>%
      filter(acct_name_call_logs_time %in% input$acct_name_call_logs_time) %>%
      filter(network_name_call_logs_time %in% input$network_name_call_logs_time) %>%
      filter(network_parent_name_call_logs_time %in% input$network_parent_name_call_logs_time) %>%
      filter(wrap_up_data %in% input$wrap_up_data)

    value_data <- call_logs_time_format_fun(value_data)

    title_out <- paste0(if_else(tail(value_data$total_time_pct_change, 1) < 0, "Decrease", if_else(tail(value_data$total_time_pct_change, 1) == 0, "No", "Increase")), " ", "in average total time from", " ", tail(value_data$year_quarter, 2)[1], " ", "to", " ", tail(value_data$year_quarter, 1))
    value_out <- tail(value_data$total_time_pct_change, 1)
    color_select <- if_else(value_out <= 0, "blue", "red")
    icon_select <- if_else(value_out < 0, "- fas fa-arrow-down", "- fas fa-arrow-up")
    shinydashboard::valueBox(value_out, title_out, icon = tags$i(class = icon_select, style = "font-size: 75px"), color = color_select)
  })

  output$table_hold_call_logs <- renderFormattable({
    value_data <- call_logs_data_linked %>%
      filter(acct_name_call_logs_time %in% input$acct_name_call_logs_time) %>%
      filter(network_name_call_logs_time %in% input$network_name_call_logs_time) %>%
      filter(network_parent_name_call_logs_time %in% input$network_parent_name_call_logs_time) %>%
      filter(wrap_up_data %in% input$wrap_up_data)

    value_data <- call_logs_time_format_fun(value_data)
    value_data <- value_data %>%
      dplyr::rename("Year quarter" = year_quarter, "Hold time mean" = hold_time_mean, "Total time mean" = total_time_mean, "Hold time percentage change" = hold_time_pct_change, "Total time percentage change" = total_time_pct_change)

    customBlue <- "#136BBD"
    customRed <- "#C22017"

    improvement_formatter <- formatter("span",
      style = x ~ style(
        font.weight = "bold",
        color = ifelse(x < 0, customBlue, ifelse(x > 0, customRed, "grey"))
      ),
      x ~ icontext(if_else(x < 0, "arrow-down", if_else(x == 0, "equal-solid", "arrow-up")), x)
    )
    improvement_indicator_hold <- formatter("span",
      style = x ~ style(
        font.weight = "bold",
        color = ifelse(x < seconds(26), customBlue, ifelse(x > seconds(26), customRed, "grey"))
      )
    )
    improvement_indicator_total <- formatter("span",
      style = x ~ style(
        font.weight = "bold",
        color = ifelse(x < minutes(6), customBlue, ifelse(x < minutes(6), customRed, "grey"))
      )
    )
    formattable(value_data,
      align = c("l", "c", "c", "r", "r"),
      list(
        `Year quarter` = formatter(
          "span",
          style = ~ style(color = "grey", font.weight = "bold")
        ),
        `Hold time mean` = improvement_indicator_hold,
        `Total time mean` = improvement_indicator_total,
        `Hold time percentage change` = improvement_formatter,
        `Total time percentage change` = improvement_formatter
      )
    )
  })

  output$graph_wrap_up <- renderPlot({
    data_out <- call_logs_data_linked %>%
      filter(acct_name_call_logs_time %in% input$acct_name_call_logs_time) %>%
      filter(network_name_call_logs_time %in% input$network_name_call_logs_time) %>%
      filter(network_parent_name_call_logs_time %in% input$network_parent_name_call_logs_time)

    custom_blue <- "#136BBD"
    custom_grey <- "#808080"

    data_out <- graph_type_format_fun(data_out, wrap_up_data)
    wrap_title <- paste0("Top wrap up categories", " ", "(", "n", " ", "=", " ", sum(data_out$count), ")")

    p <- ggplot(data = data_out, aes(x = reorder(wrap_up_data, percentage), y = percentage, fill = threshold)) +
      geom_bar(stat = "identity") +
      # Fill the plot with your selected colors
      scale_fill_manual(values = c(custom_grey, custom_blue)) +
      # Make the background blank
      theme_void() +
      coord_flip() +
      ggtitle(wrap_title) +
      # Add text to bars and make them inside the bars
      geom_text(aes(label = percentage), hjust = 1.1, nudge_x = 0) +
      # Move the y axis labels and bars closer
      scale_y_continuous(expand = c(.002, .002)) +
      # More the title, remove the legend, change the y-axis size
      theme(axis.text.y = element_text(size = 14, hjust = 1)) +
      theme(text = element_text(family = "Helvetica Neue")) +
      theme(legend.position = "none", plot.title = element_text(hjust = 3, vjust = 5, size = 16), plot.margin = margin(15, 30, 15, 15))
    p
  })
  output$news_plot <- renderPlot(
    {
      data_out <- defhc_news_clean %>%
        filter(year_quarter %in% input$year_quarter) %>%
        filter(acct_name_news %in% input$acct_name_news) %>%
        filter(network_name_news %in% input$network_name_news) %>%
        filter(network_parent_name_news %in% input$network_parent_name_news)
      data_out <- graph_type_format_fun(data_out, news_event_type)
      custom_blue <- "#136BBD"
      custom_grey <- "#808080"

      news_title <- paste0("Top news articles", " ", "n", " ", "=", " ", sum(data_out$count))
      ### Need to reorder to get highest to lowest and fill to get the coloring set later
      p <- ggplot(data = data_out, aes(x = reorder(news_event_type, percentage), y = percentage, fill = threshold)) +
        geom_bar(stat = "identity") +
        # Fill the plot with your selected colors
        scale_fill_manual(values = c(custom_grey, custom_blue)) +
        # Make the background blank
        theme_void() +
        coord_flip() +
        ggtitle(news_title) +
        # Add text to bars and make them inside the bars
        geom_text(aes(label = percentage), hjust = 1.1, nudge_x = 0) +
        # Move the y axis labels and bars closer
        scale_y_continuous(expand = c(.002, .002)) +
        # More the title, remove the legend, change the y-axis size
        theme(axis.text.y = element_text(size = 14, hjust = 1)) +
        theme(text = element_text(family = "Helvetica Neue")) +
        theme(legend.position = "none", plot.title = element_text(hjust = -.75, vjust = 5, size = 16), plot.margin = margin(15, 30, 15, 15))
      p
      # Remove white background
    },
    bg = "transparent"
  )

  output$news_table <- renderDataTable(
    datatable(defhc_news_clean %>%
      filter(year_quarter %in% input$year_quarter) %>%
      filter(acct_name_news %in% input$acct_name_news) %>%
      filter(network_name_news %in% input$network_name_news) %>%
      filter(network_parent_name_news %in% input$network_parent_name_news) %>%
      select(news_event_type, news_event_title) %>%
      rename("News event type" = news_event_type, "Headline" = news_event_title),
    rownames = FALSE, options = list(pageLength = 4)
    )
  )

  output$customer_panel_drivers <- renderPlot({
    data_out <- data_customer_panel %>%
      filter(role %in% input$role)
    data_out = data_out %>% select(-c(role, csat, ces))
    
    driver_recipe <- recipe(nps ~ ., data = data_out) %>% 
      ## Can help make the distribution more symmetric                     
      step_YeoJohnson(all_numeric(), -all_outcomes()) %>% 
      ### Create z-scores for data to reduce collinearity
      step_normalize(all_numeric(), -all_outcomes()) %>%
      ## Impute missing values
      step_impute_knn(all_predictors()) %>%
      ## Remove variables with .9 correlation or higher
      step_corr(all_predictors(), method = "spearman") %>%
      ## Remove all variables with no or near zero variance
      step_nzv(all_predictors())
    
    driver_model <- linear_reg() %>% 
      set_engine('lm') %>% 
      set_mode('regression')
    
    driver_data_baked <- driver_recipe %>% 
      prep() %>% 
      bake(new_data = data_out)
    
    driver_final_fit <- driver_model %>% 
      fit(nps ~ ., data = driver_data_baked)
    
    driver_fit_import = vi_model(driver_final_fit)
    driver_fit_import = driver_fit_import %>%
      clean_names() %>% 
      mutate(importance = round(importance, 2))
    
    likert_customer_panel =  data_out %>%
      select(c(respected:easy_products)) %>%
      t() %>%
      data.frame() %>%
      mutate(one = rowSums(. == "1")) %>% 
      mutate(two = rowSums(. == "2")) %>% 
      mutate(three = rowSums(. == "3")) %>%
      mutate(four = rowSums(. == "4")) %>%
      mutate(five = rowSums(. == "5")) %>%
      select(one:five) %>%
      mutate_all(funs(./dim(data_customer_panel)[1])) %>%
      mutate_all(funs(formattable::percent(., digits = 0))) %>%
      mutate(positive = four+five) %>%
      select(positive) %>%
      add_rownames("variable")
    
    key_drivers_data = driver_fit_import %>%
      select(-c(sign)) %>%
      left_join(likert_customer_panel %>% select(variable, positive), by  = "variable")
    
    key_drivers_data <- key_drivers_data %>%
      mutate(color = if_else(positive < median(positive) & importance > median(importance), "red", if_else(positive > median(positive) & importance > median(importance), "blue", "grey"))) 
    ### Rename the variables 
    
    key_drivers_data$variable = recode_factor(key_drivers_data$variable, respected = "Respected", timely_communication = "Timely communication", quality_training = "Quality training", reliable = "Reliable", fair_pricing = "Fair pricing", comprehensive_product_line = "Comprehensive product line", product_quality = "Product quality", product_innovation = "Product innovation", value_added_services = "Value added services", clinical_data = "Clinical data", responsive = "Responsive", easy_products = "Easy products")
    
    x_max = max(key_drivers_data$importance)
    x_min = min(key_drivers_data$importance)
    y_max = max(key_drivers_data$positive)
    y_min = min(key_drivers_data$positive)
    
    title = paste0("Key drivers", " ", "N = ", " ", dim(data_customer_panel)[1])
    
    kd <- ggplot(key_drivers_data,aes(importance, positive, label = variable))+
      geom_point()+
      ylim(y_min-.1, y_max + .1) +
      xlim(x_min - 1, x_max + 1) +
      theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
      labs(x = "Importance", y =  "Favorable") +
      geom_hline(yintercept=median(key_drivers_data$positive))+
      geom_vline(xintercept=median(key_drivers_data$importance)) +
      labs(x = "Importance", y =  "Favorable") +
      ggtitle(title) +
      annotate(geom="text", x = x_max + 1, y= y_max + .1, label="Key strength",
               color="#00B0F0") +
      annotate(geom="text", x = x_min - 1, y = y_max + .1, label="Unimportant strength",
               color="#808080") +
      annotate(geom="text", x=  x_max + 1, y=y_min-.09, label="Important opportunity",
               color="#FF0000") +
      annotate(geom="text", x=  x_min - 1, y= y_min-.09, label= "Unimportant opportunity",
               color="#808080") + 
      geom_text_repel(data = key_drivers_data %>% filter(color == "red"), aes(label = variable),
                      size = 3.5,  color = "#FF0000") +
      geom_text_repel(data = key_drivers_data %>% filter(color == "blue"), aes(label = variable),
                      size = 3.5,  color = "#00B0F0") +
      geom_text_repel(data = key_drivers_data %>% filter(color == "grey"), aes(label = variable),
                      size = 3.5,  color = "#808080") +
      coord_flip()
    kd
  },
  bg = "transparent"
  )
}

shinyApp(ui, server)
