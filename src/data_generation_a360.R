## ----setup, include=FALSE--------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## --------------------------------------------------------------------------------------------------------------------------
library(pacman)
pacman::p_load(renv, dplyr, shiny, shinydashboard, shinydashboardPlus, tidyverse, glue, plotly, devtools, shinyWidgets, fontawesome, formattable, janitor, tsibble, DT, readr, lubridate, tidymodels, ggrepel)


## --------------------------------------------------------------------------------------------------------------------------
chatter_low = seq(from = -.5, to = -.2, by = .01)
chatter_medium = seq(from = -.2, to = .2, by = .01)
chatter_high = seq(from = .2, to = .5, by = .01)
sample_rate = 126/3

chatter_data = c(sample(chatter_low, sample_rate, replace = TRUE), sample(chatter_medium, sample_rate, replace = TRUE), sample(chatter_high, sample_rate, replace = TRUE))

account_names = c("Indiana", "Purdue", "Northwestern", "Illinois", "Penn State", "Ohio State", "Nebraska", "Michigan State", "Michigan", "Rutgers", "Maryland", "Minnesota", "Iowa", "Wisconsin") 

account_names = rep(account_names, each = 9)

year_quarter = data.frame(year_quarter = seq(as.Date("2020/1/1"), as.Date("2022/1/1"), "months")) %>%
  mutate(year_quarter = ymd(year_quarter)) %>%
  mutate(year_quarter = tsibble::yearquarter(year_quarter)) %>%
  dplyr::distinct(year_quarter) 

year_quarter = rep(year_quarter$year_quarter, times = 14)

account_year = data.frame(account_names_chatter = account_names, year_quarter_chatter = as.character(year_quarter))

chatter_data = data.frame(account_year, chatter_data = chatter_data)
chatter_data


## --------------------------------------------------------------------------------------------------------------------------

set.seed(123)
nps_low = c(4:10)
nps_medium = c(7:10)
nps_high = c(8:10)
sampling_rate = 126/3
nps = c(sample(nps_low, size = sampling_rate, replace = TRUE), sample(nps_medium, size = sampling_rate, replace = TRUE), sample(nps_high, size = sampling_rate, replace = TRUE))


driver_low = c(1:5)
driver_high = c(3:5)

#useful, usable, desireable, findable, credible, valuable
sampling_rate_driver = 126
set.seed(123)
driver_function = function(data){
  x = sample(data, size = sampling_rate_driver, replace = TRUE)
}

useful = driver_function(data = driver_high)
useable = driver_function(data = driver_low)
desireable = driver_function(data = driver_high)
findable = driver_function(data = driver_low)
credible = driver_function(data = driver_high)
valuable = driver_function(data = driver_low)

nps_data = data.frame(year_quarter, account_names, nps, useful, useable, desireable, findable, credible, valuable)

