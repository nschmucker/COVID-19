consolidate_levels <- function(df_states, df_counties){
  clean_states <- 
    df_states %>% 
    dplyr::rename_all(toupper) %>% 
    dplyr::filter(!STATE %in% c(
      "Alaska", "American Samoa", "Guam", "Hawaii", 
      "Northern Mariana Islands", "Puerto Rico", "Virgin Islands"
    )) %>% 
    dplyr::transmute(
      DATE,
      LEVEL = "State",
      GEOID = FIPS,
      STATE_FIPS = FIPS,
      COUNTY_FIPS = "000",
      STATE_NAME = STATE,
      COUNTY_NAME = NA_character_,
      TOTAL_CASES = CASES,
      TOTAL_DEATHS = DEATHS
    )
  
  clean_counties <- 
    df_counties %>% 
    dplyr::rename_all(toupper) %>% 
    dplyr::filter(!STATE %in% c(
      "Alaska", "American Samoa", "Guam", "Hawaii", 
      "Northern Mariana Islands", "Puerto Rico", "Virgin Islands"
    )) %>%
    dplyr::transmute(
      DATE,
      LEVEL = "County",
      GEOID = dplyr::if_else(COUNTY == "New York City", "36NYC", FIPS),
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
      STATE_ABBR = dplyr::if_else(
        STATE_NAME == "District of Columbia",
        "DC", state.abb[match(STATE_NAME, state.name)]
      ),
      COUNTY_NAME = paste0(COUNTY, " (", STATE_ABBR, ")"),
      TOTAL_CASES = CASES,
      TOTAL_DEATHS = DEATHS
    ) %>% 
    dplyr::select(-STATE_ABBR)
  
  dplyr::bind_rows(clean_states, clean_counties) %>% 
    dplyr::filter(!is.na(GEOID))
}

add_population <- function(df, census_df){
  population <- census_df %>% 
    dplyr::transmute(
      LEVEL = dplyr::case_when(
        SUMLEV == "040" ~ "State",
        SUMLEV == "050" ~ "County",
        TRUE ~ SUMLEV
      ),
      STATE_FIPS = STATE,
      COUNTY_FIPS = COUNTY,
      STATE_NAME = STNAME,
      COUNTY_NAME = CTYNAME,
      POPULATION = POPESTIMATE2019
    ) %>% 
    dplyr::select(STATE_FIPS, COUNTY_FIPS, POPULATION)
    
  dplyr::left_join(df, population, by = c("STATE_FIPS", "COUNTY_FIPS")) 
}

add_variables <- function(df){
  df %>% 
    # Organize and group by reporting unit
    dplyr::arrange(STATE_FIPS, COUNTY_FIPS, DATE) %>% 
    dplyr::group_by(STATE_FIPS, COUNTY_FIPS) %>% 
    dplyr::mutate(
      # Proportion of population infected/dead
      TOTAL_CASES_PER_100K = 100000*TOTAL_CASES/POPULATION,
      TOTAL_DEATHS_PER_100K = 100000*TOTAL_DEATHS/POPULATION,
      
      # Incremental cases/deaths per day
      NEW_CASES = TOTAL_CASES - dplyr::lag(TOTAL_CASES, 1),
      NEW_DEATHS = TOTAL_DEATHS - dplyr::lag(TOTAL_DEATHS, 1),
      NEW_CASES = dplyr::if_else(is.na(NEW_CASES), TOTAL_CASES, NEW_CASES),
      NEW_DEATHS = dplyr::if_else(is.na(NEW_DEATHS), TOTAL_DEATHS, NEW_DEATHS),
      
      # Rolling 7-day incremental deaths/cases
      AVG_NEW_CASES = zoo::rollmeanr(NEW_CASES, 7, fill = NA),
      AVG_NEW_DEATHS = zoo::rollmeanr(NEW_DEATHS, 7, fill = NA),
      
      # Current infection/mortality rate
      AVG_INFECTION_RATE = AVG_NEW_CASES/TOTAL_CASES,
      AVG_DEATH_RATE = AVG_NEW_DEATHS/TOTAL_CASES,
      
      CASE_FATALITY_RATE = TOTAL_DEATHS / TOTAL_CASES,
      
      LATEST_DATA_IND = dplyr::if_else(DATE == max(DATE), 1, 0)
    ) %>% 
    dplyr::ungroup()
}

beautify <- function(df){
  df %>% 
    dplyr::mutate(
      COUNTY_FIPS = dplyr::if_else(
        COUNTY_FIPS == "000", NA_character_, COUNTY_FIPS
      ) # "000" needed for join, but analysis is cleaner if NA instead
    ) %>% 
    dplyr::select(-POPULATION, POPULATION) %>% 
    dplyr::rename_all(~purrr::map_chr(., totitle)) %>% 
    dplyr::rename(GEOID = Geoid, `State FIPS` = `State Fips`, `County FIPS` = `County Fips`)
}

prep_output_data <- function(df, metric, log_axis, latest){
  temp_df <-
    df %>%
    dplyr::filter(log_axis == "No" | {{metric}} != 0) %>% # Can't log transform a 0
    dplyr::filter(!is.na({{metric}})) %>% # Avoid missing value warning
    dplyr::filter(!latest | `Latest Data Ind` == 1)
  
  max_val <- temp_df %>% 
    dplyr::summarize(MAX = max({{metric}})) %>%
    dplyr::pull()
  digits <- if(max_val > 100) {0} else if(max_val > 10) {1} else {2}
  
  # If both states and counties are present, keep only states
  state_level_present <- max(as.integer(temp_df$Level == "State")) == 1
  
  temp_df %>% 
    dplyr::filter(!state_level_present | Level == "State") %>%
    dplyr::mutate(
      Value = round({{metric}}, digits),
      Name = dplyr::if_else(Level == "State", `State Name`, `County Name`),
      Label = paste0("<strong>", Name, "</strong><br/>", Value)
    ) %>% 
    dplyr::select(
      Level, GEOID,
      Date, `Latest Data Ind`, `Full Value` = {{metric}}, Value,
      `State Name`, `County Name`, Name, Label
    ) %>% 
    dplyr::arrange(dplyr::desc(Date), dplyr::desc(`Full Value`))
}

add_nyc_counties <- function(df){
  new_rows <- dplyr::filter(df, GEOID == "36NYC")

  bronx <- new_rows %>%
    dplyr::mutate(
      GEOID = "36005", 
      `County Name` = "Bronx", 
      Name = `County Name`, 
      Label = paste0("<strong>", Name, "</strong><br/>", Value)
    )
  brooklyn <- new_rows %>%
    dplyr::mutate(
      GEOID = "36047", 
      `County Name` = "Kings", 
      Name = `County Name`, 
      Label = paste0("<strong>", Name, "</strong><br/>", Value)
    )
  manhattan <- new_rows %>%
    dplyr::mutate(
      GEOID = "36061", 
      `County Name` = "New York", 
      Name = `County Name`, 
      Label = paste0("<strong>", Name, "</strong><br/>", Value)
    )
  queens <- new_rows %>%
    dplyr::mutate(
      GEOID = "36081", 
      `County Name` = "Queens", 
      Name = `County Name`, 
      Label = paste0("<strong>", Name, "</strong><br/>", Value)
    )
  staten_island <- new_rows %>%
    dplyr::mutate(
      GEOID = "36085", 
      `County Name` = "Richmond", 
      Name = `County Name`, 
      Label = paste0("<strong>", Name, "</strong><br/>", Value)
    )
  
  df %>% 
    dplyr::filter(GEOID != "36NYC") %>%
    dplyr::bind_rows(bronx, brooklyn, manhattan, queens, staten_island)
}

get_top_n <- function(df, n){
  top_n_geoid <- df %>%  
    dplyr::filter(`Latest Data Ind` == 1) %>% 
    dplyr::arrange(dplyr::desc(`Full Value`)) %>% 
    head(n) %>% 
    dplyr::pull(GEOID)
  
  dplyr::filter(df, GEOID %in% top_n_geoid)
}

graph_timeseries <- function(df, metric, log_axis){
  graph_data <- df %>% 
    prep_output_data({{metric}}, log_axis = log_axis, latest = FALSE) %>% 
    dplyr::mutate(Date = highcharter::datetime_to_timestamp(Date))
  
  graph_data <- get_top_n(graph_data, 8)

  variable <- dplyr::select(df, {{metric}}) %>% names()
  level    <- graph_data$Level[1]
  max_val  <- max(graph_data$`Full Value`)
  digits   <- if(max_val > 100) {0} else if(max_val > 10) {1} else if(max_val > 1) {2} else {3}
  
  title    <- paste0("Time trend of ", tolower(variable))
  subtitle <- paste0("By ", tolower(level))
  x_lab    <- NULL
  y_lab    <- variable
  caption  <- if(variable %in% c("Total Cases Per 100k", "Total Deaths Per 100k")) {
    "Source: NYT, U.S. Census Bureau"
  } else {
    "Source: NYT"
  }
  
  highcharter::highchart(type = "chart") %>%
    highcharter::hc_add_series(
      graph_data,
      highcharter::hcaes(x = Date, y = `Full Value`, group = Name, name = Name),
      type = "line",
      marker = list(enabled = TRUE, symbol = "circle")
    ) %>%
    highcharter::hc_add_theme(hc_custom_theme()) %>%
    highcharter::hc_title(text = title) %>% 
    highcharter::hc_subtitle(text = subtitle) %>% 
    highcharter::hc_xAxis(
      type = "datetime",
      title = list(text = x_lab)
    ) %>%
    highcharter::hc_yAxis(
      type = if(log_axis == "Yes") {"logarithmic"} else {"linear"},
      title = list(text = y_lab)
    ) %>% 
    highcharter::hc_credits(enabled = TRUE, text = caption) %>% 
    highcharter::hc_legend(enabled = FALSE) %>% 
    highcharter::hc_tooltip(
      valueDecimals = digits,
      pointFormat = '{point.x:%B %d, %Y}: {point.y}'
    ) %>%
    hc_chart(zoomType = "xy") %>% 
    hc_rangeSelector(
      enabled = TRUE,
      buttons = list(
        list(type = "week", count = 1, text = "1wk"),
        list(type = "month", count = 1, text = "1m"),
        list(type = "month", count = 3, text = "3m"),
        list(type = "all", text = "All")),
      selected = 4) %>% 
    hc_exporting(
      enabled = TRUE,
      filename = "Covid-19_Chart_Export",
      formAttributes = list(target = "_blank"),
      buttons =
        list(
          contextButton =
            list(
              text = "Export",
              theme = list(fill = "transparent"),
              menuItems = hc_export_opts()
            )
        )
    )
}

graph_bars <- function(df, metric){
  graph_data <- df %>% 
    prep_output_data({{metric}}, log_axis = "No", latest = TRUE) 
  
  graph_data <- get_top_n(graph_data, 20)
  
  variable <- dplyr::select(df, {{metric}}) %>% names()
  level    <- graph_data$Level[1]
  max_val  <- max(graph_data$`Full Value`)
  digits   <- if(max_val > 100) {0} else if(max_val > 10) {1} else if(max_val > 1) {2} else {3}
  
  title    <- paste0("Latest value of ", tolower(variable))
  subtitle <- paste0("By ", tolower(level))
  x_lab    <- level
  y_lab    <- variable
  caption  <- if(variable %in% c("Total Cases Per 100k", "Total Deaths Per 100k")) {
    "Source: NYT, U.S. Census Bureau"
  } else {
    "Source: NYT"
  }
  
  highcharter::highchart(type = "chart") %>%
    highcharter::hc_add_series(
      graph_data,
      highcharter::hcaes(categories = Name, y = `Full Value`, name = Name),
      type = "column"
    ) %>%
    highcharter::hc_add_theme(hc_custom_theme()) %>%
    highcharter::hc_title(text = title) %>% 
    highcharter::hc_subtitle(text = subtitle) %>% 
    highcharter::hc_xAxis(
      type = "category",
      title = list(text = x_lab)
    ) %>%
    highcharter::hc_yAxis(
      type = "linear",
      title = list(text = y_lab)
    ) %>% 
    highcharter::hc_credits(enabled = TRUE, text = caption) %>% 
    highcharter::hc_legend(enabled = FALSE) %>% 
    highcharter::hc_tooltip(
      valueDecimals = digits,
      pointFormat = '{point.y}'
    ) %>%
    hc_exporting(
      enabled = TRUE,
      filename = "Covid-19_Chart_Export",
      formAttributes = list(target = "_blank"),
      buttons =
        list(
          contextButton =
            list(
              text = "Export",
              theme = list(fill = "transparent"),
              menuItems = hc_export_opts()
            )
        )
    )
}
