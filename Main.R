# SETUP                  #### #### #### #### #### #### #### #### #### #### ####
library(readr)
library(dplyr)
library(stringr)
library(zoo)     # masks as.Date
library(forcats)
library(ggplot2)

source("./R/Utils.R")
source("./R/Functions.R")

# TODO: Fix population for NYT's noted regions

# GET DATA               #### #### #### #### #### #### #### #### #### #### ####
url_census   <- "https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv"
url_states   <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
url_counties <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"

raw_census   <- readr::read_csv(url(url_census))
raw_states   <- readr::read_csv(url(url_states))
raw_counties <- readr::read_csv(url(url_counties))

# CLEAN AND MERGE DATA   #### #### #### #### #### #### #### #### #### #### ####
covid_19 <-
  consolidate_levels(raw_states, raw_counties) %>% 
  add_population(raw_census) %>% 
  add_variables() %>% 
  beautify()

# Ensure combined, transformed data hasn't added/dropped rows
stopifnot(nrow(covid_19) == nrow(raw_counties) + nrow(raw_states))

readr::write_csv(covid_19, paste0("./Data/COVID_19_", Sys.Date(), ".csv"))

rm(
  list = dplyr::setdiff(
    ls(),
    c(
      "covid_19",
      "totitle", "custom_theme",
      "get_plot_data", "graph_timeseries", "graph_bars"
    )
  )
)

# VISUALIZE DATA         #### #### #### #### #### #### #### #### #### #### ####
level    <- "County" # Choose from: c("State", "County")
states   <- "Pennsylvania" # Example: c("Pennsylvania", "New Jersey", "New York")
counties <- c("Philadelphia", "Montgomery", "Delaware", "Chester", "Bucks") # Example: "All"
log_y    <- FALSE

graph_timeseries(covid_19, TOTAL_CASES, level, states, counties, log_y)
graph_timeseries(covid_19, TOTAL_DEATHS, level, states, counties, log_y)

graph_timeseries(covid_19, TOTAL_INFECTED_PER_100K, level, states, counties, log_y)
graph_timeseries(covid_19, TOTAL_DEATHS_PER_100K, level, states, counties, log_y)

graph_timeseries(covid_19, NEW_CASES, level, states, counties, log_y)
graph_timeseries(covid_19, NEW_DEATHS, level, states, counties, log_y)

graph_timeseries(covid_19, AVG_NEW_CASES, level, states, counties, log_y)
graph_timeseries(covid_19, AVG_NEW_DEATHS, level, states, counties, log_y)

graph_timeseries(covid_19, AVG_INFECTION_RATE, level, states, counties, log_y)
graph_timeseries(covid_19, AVG_DEATH_RATE, level, states, counties, log_y)

graph_bars(covid_19, TOTAL_CASES, level, states, counties, 0)
graph_bars(covid_19, TOTAL_DEATHS, level, states, counties, 0)

graph_bars(covid_19, TOTAL_INFECTED_PER_100K, level, states, counties, 1)
graph_bars(covid_19, TOTAL_DEATHS_PER_100K, level, states, counties, 3)

graph_bars(covid_19, NEW_CASES, level, states, counties, 0)
graph_bars(covid_19, NEW_DEATHS, level, states, counties, 0)

graph_bars(covid_19, AVG_NEW_CASES, level, states, counties, 0)
graph_bars(covid_19, AVG_NEW_DEATHS, level, states, counties, 0)

graph_bars(covid_19, AVG_INFECTION_RATE, level, states, counties, 3)
graph_bars(covid_19, AVG_DEATH_RATE, level, states, counties, 3)
