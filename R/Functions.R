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
    dplyr::select(-POPULATION, POPULATION)
}

prep_output_data <- function(df, metric, log_axis, latest){
  temp_df <-
    df %>%
    dplyr::filter(log_axis == "No" | {{metric}} != 0) %>% # Can't log transform a 0
    dplyr::filter(!is.na({{metric}})) %>% # Avoid missing value warning
    dplyr::filter(!latest | LATEST_DATA_IND == 1)
  
  max_val <- temp_df %>% 
    dplyr::summarize(MAX = max({{metric}})) %>%
    dplyr::pull()
  digits <- if(max_val > 100) {0} else if(max_val > 10) {1} else {2}
  
  # If both states and counties are present, keep only states
  state_level_present <- max(as.integer(temp_df$LEVEL == "State")) == 1
  
  temp_df %>% 
    dplyr::filter(!state_level_present | LEVEL == "State") %>%
    dplyr::mutate(
      VALUE = round({{metric}}, digits),
      NAME = dplyr::if_else(LEVEL == "State", STATE_NAME, COUNTY_NAME),
      LABEL = paste0("<strong>", NAME, "</strong><br/>", VALUE)
    ) %>% 
    dplyr::select(
      LEVEL, GEOID,
      DATE, LATEST_DATA_IND, VALUE_FULL = {{metric}}, VALUE,
      STATE_NAME, COUNTY_NAME, NAME, LABEL
    ) %>% 
    dplyr::arrange(dplyr::desc(DATE), dplyr::desc(VALUE_FULL))
}

add_nyc_counties <- function(df){
  new_rows <- dplyr::filter(df, GEOID == "36NYC")

  bronx <- new_rows %>%
    dplyr::mutate(
      GEOID = "36005", 
      COUNTY_NAME = "Bronx", 
      NAME = COUNTY_NAME, 
      LABEL = paste0("<strong>", NAME, "</strong><br/>", VALUE)
    )
  brooklyn <- new_rows %>%
    dplyr::mutate(
      GEOID = "36047", 
      COUNTY_NAME = "Kings", 
      NAME = COUNTY_NAME, 
      LABEL = paste0("<strong>", NAME, "</strong><br/>", VALUE)
    )
  manhattan <- new_rows %>%
    dplyr::mutate(
      GEOID = "36061", 
      COUNTY_NAME = "New York", 
      NAME = COUNTY_NAME, 
      LABEL = paste0("<strong>", NAME, "</strong><br/>", VALUE)
    )
  queens <- new_rows %>%
    dplyr::mutate(
      GEOID = "36081", 
      COUNTY_NAME = "Queens", 
      NAME = COUNTY_NAME, 
      LABEL = paste0("<strong>", NAME, "</strong><br/>", VALUE)
    )
  staten_island <- new_rows %>%
    dplyr::mutate(
      GEOID = "36085", 
      COUNTY_NAME = "Richmond", 
      NAME = COUNTY_NAME, 
      LABEL = paste0("<strong>", NAME, "</strong><br/>", VALUE)
    )
  
  df %>% 
    dplyr::filter(GEOID != "36NYC") %>%
    dplyr::bind_rows(bronx, brooklyn, manhattan, queens, staten_island)
}

get_top_n <- function(df, n){
  top_n_geoid <- df %>%  
    dplyr::filter(LATEST_DATA_IND == 1) %>% 
    dplyr::arrange(dplyr::desc(VALUE_FULL)) %>% 
    head(n) %>% 
    dplyr::pull(GEOID)
  
  dplyr::filter(df, GEOID %in% top_n_geoid)
}

graph_timeseries <- function(df, metric, log_axis){
  graph_data <- df %>% 
    prep_output_data({{metric}}, log_axis = log_axis, latest = FALSE) %>% 
    dplyr::mutate(DATE = highcharter::datetime_to_timestamp(DATE))
  
  graph_data <- get_top_n(graph_data, 8)

  variable <- dplyr::select(df, {{metric}}) %>% names()
  level    <- graph_data$LEVEL[1]
  max_val  <- max(graph_data$VALUE_FULL)
  digits   <- if(max_val > 100) {0} else if(max_val > 10) {1} else if(max_val > 1) {2} else {3}
  
  title    <- paste0("# ", tolower(totitle(variable)), " over time")
  subtitle <- paste0("By ", tolower(level))
  x_lab    <- NULL
  y_lab    <- paste0("# ", tolower(totitle(variable)))
  caption  <- if(variable %in% c("TOTAL_CASES_PER_100K", "TOTAL_DEATHS_PER_100K")) {
    "Source: NYT, U.S. Census Bureau"
  } else {
    "Source: NYT"
  }
  
  highcharter::highchart(type = "chart") %>%
    highcharter::hc_add_series(
      graph_data,
      highcharter::hcaes(x = DATE, y = VALUE_FULL, group = NAME, name = NAME),
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
  level    <- graph_data$LEVEL[1]
  max_val  <- max(graph_data$VALUE_FULL)
  digits   <- if(max_val > 100) {0} else if(max_val > 10) {1} else if(max_val > 1) {2} else {3}
  
  title    <- paste0("Latest value of ", tolower(totitle(variable)))
  subtitle <- paste0("By ", tolower(level))
  x_lab    <- level
  y_lab    <- paste0("# ", tolower(totitle(variable)))
  caption  <- if(variable %in% c("TOTAL_CASES_PER_100K", "TOTAL_DEATHS_PER_100K")) {
    "Source: NYT, U.S. Census Bureau"
  } else {
    "Source: NYT"
  }
  
  highcharter::highchart(type = "chart") %>%
    highcharter::hc_add_series(
      graph_data,
      highcharter::hcaes(categories = NAME, y = VALUE_FULL, name = NAME),
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
