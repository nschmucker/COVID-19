# SETUP                  #### #### #### #### #### #### #### #### #### #### ####
library(readr)
library(dplyr)
library(stringr)
library(zoo)     # masks as.Date
library(ggplot2)

source("./Functions.R")

# GET DATA               #### #### #### #### #### #### #### #### #### #### ####
url_census   <- "https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv"
url_states   <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
url_counties <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"

raw_census   <- readr::read_csv(url(url_census))
raw_states   <- readr::read_csv(url(url_states))
raw_counties <- readr::read_csv(url(url_counties))

# CLEAN AND MERGE DATA   #### #### #### #### #### #### #### #### #### #### ####
clean_census <-
  raw_census %>% 
  dplyr::transmute(
    LEVEL = dplyr::case_when(
      SUMLEV == "040" ~ "STATE",
      SUMLEV == "050" ~ "COUNTY",
      TRUE ~ SUMLEV
    ),
    STATE_FIPS = STATE,
    COUNTY_FIPS = COUNTY,
    STATE_NAME = STNAME,
    COUNTY_NAME = CTYNAME,
    POPULATION = POPESTIMATE2019
  )

clean_states <- 
  raw_states %>% 
  dplyr::rename_all(toupper) %>% 
  dplyr::transmute(
    DATE,
    LEVEL = "STATE",
    STATE_FIPS = FIPS,
    COUNTY_FIPS = "000",
    STATE_NAME = STATE,
    COUNTY_NAME = NA_character_,
    TOTAL_CASES = CASES,
    TOTAL_DEATHS = DEATHS
  ) 

clean_counties <- 
  raw_counties %>% 
  dplyr::rename_all(toupper) %>% 
  dplyr::transmute(
    DATE,
    LEVEL = "COUNTY",
    STATE_FIPS = dplyr::if_else(
      stringr::str_length(FIPS) == 5,
      stringr::str_trunc(FIPS, 2, "right", ""),
      NA_character_
    ),
    COUNTY_FIPS = dplyr::if_else(
      stringr::str_length(FIPS) == 5,
      stringr::str_trunc(FIPS, 3, "left", ""),
      NA_character_
    ),
    STATE_NAME = STATE,
    COUNTY_NAME = COUNTY,
    TOTAL_CASES = CASES,
    TOTAL_DEATHS = DEATHS
  )

covid_19 <-
  # Join state and county data in to single tibble
  dplyr::bind_rows(clean_states, clean_counties) %>% 
  dplyr::left_join(
    dplyr::select(clean_census, STATE_FIPS, COUNTY_FIPS, POPULATION),
    by = c("STATE_FIPS", "COUNTY_FIPS")
  ) %>% 
  dplyr::mutate(
    COUNTY_FIPS = dplyr::if_else(
      COUNTY_FIPS == "000",
      NA_character_,
      COUNTY_FIPS
    )
  ) %>%
  # Organize and group by reporting unit
  dplyr::arrange(STATE_FIPS, COUNTY_FIPS, DATE) %>% 
  dplyr::group_by(STATE_FIPS, COUNTY_FIPS) %>% 
  # Add new variables for analysis
  dplyr::mutate(
    # Add percent of population infected/dead
    TOTAL_INFECTED_PER_K = 1000*TOTAL_CASES/POPULATION,
    TOTAL_DEATHS_PER_K = 1000*TOTAL_DEATHS/POPULATION,

    # Add incremental cases/deaths per day
    NEW_CASES = TOTAL_CASES - dplyr::lag(TOTAL_CASES, 1),
    NEW_DEATHS = TOTAL_DEATHS - dplyr::lag(TOTAL_DEATHS, 1),
    NEW_CASES = dplyr::if_else(is.na(NEW_CASES), TOTAL_CASES, NEW_CASES),
    NEW_DEATHS = dplyr::if_else(is.na(NEW_DEATHS), TOTAL_DEATHS, NEW_DEATHS),

    # Add rolling 7-day incremental deaths/cases
    AVG_NEW_CASES = zoo::rollmeanr(NEW_CASES, 7, fill = NA),
    AVG_NEW_DEATHS = zoo::rollmeanr(NEW_DEATHS, 7, fill = NA),

    # Add current infection/mortality rate
    AVG_INFECTION_RATE = AVG_NEW_CASES/TOTAL_CASES,
    AVG_DEATH_RATE = AVG_NEW_DEATHS/TOTAL_CASES
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-POPULATION, POPULATION)

# Ensure combined, transformed data hasn't added/dropped rows
stopifnot(nrow(covid_19) == nrow(raw_counties) + nrow(raw_states))

rm(
  list = dplyr::setdiff(
    ls(),
    c("totitle", "graph_timeseries", "covid_19")
  )
)

# VISUALIZE DATA         #### #### #### #### #### #### #### #### #### #### ####
level <- "STATE" # c("STATE", "COUNTY")
states <- c("Pennsylvania", "New Jersey", "New York")
counties <- "All"
log_y <- TRUE

graph_timeseries(covid_19, TOTAL_CASES, level, states, counties, log_y)
graph_timeseries(covid_19, TOTAL_DEATHS, level, states, counties, log_y)

graph_timeseries(covid_19, TOTAL_INFECTED_PER_K, level, states, counties, log_y)
graph_timeseries(covid_19, TOTAL_DEATHS_PER_K, level, states, counties, log_y)

graph_timeseries(covid_19, AVG_NEW_CASES, level, states, counties, log_y)
graph_timeseries(covid_19, AVG_NEW_DEATHS, level, states, counties, log_y)

graph_timeseries(covid_19, AVG_INFECTION_RATE, level, states, counties, log_y)
graph_timeseries(covid_19, AVG_DEATH_RATE, level, states, counties, log_y)

# Bar chart: mortality rate by state/county

# Bar chart: infection rate by state/county