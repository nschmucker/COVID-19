totitle <- function(x) {
  s <- strsplit(x, " |_")[[1]]
  paste(
    toupper(substring(s, 1, 1)), tolower(substring(s, 2)),
    sep = "", collapse = " "
  )
}

graph_timeseries <- function(df, metric, level, states, counties, log_y){
  title        <- paste0("# ", tolower(totitle(deparse(substitute(metric)))), " over time")
  subtitle     <- paste0("By ", tolower(level))
  x_lab        <- NULL
  y_lab        <- paste0("# ", tolower(totitle(deparse(substitute(metric)))))
  caption      <- "Source: NYT"
  legend_title <- totitle(level)

  plot_data <- df %>% 
    dplyr::filter(LEVEL == level) %>%           # State or county data?
    dplyr::filter("All" %in% states | STATE_NAME %in% states) %>% 
    dplyr::filter("All" %in% counties | COUNTY_NAME %in% counties) %>% 
    dplyr::filter(!log_y | {{metric}} != 0) %>% # Can't log transform a 0
    dplyr::filter(!is.na({{metric}}))           # Avoid ggplot2 missing value warning

  plot_data %>%
    # Main plot
    ggplot2::ggplot(ggplot2::aes(
      x = DATE, y = {{metric}},
      group = if (level == "STATE") {STATE_NAME} else {COUNTY_NAME},
      color = if (level == "STATE") {STATE_NAME} else {COUNTY_NAME}
    )) +
    ggplot2::geom_line() +
    # Aesthetics
    {if (log_y) ggplot2::scale_y_log10()} +     # Log y-axis?
    ggplot2::labs(
      title = title, subtitle = subtitle,
      x = x_lab, y = y_lab, caption = caption
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        title = legend_title,
        title.position = "left",
        direction = "horizontal"
      )
    )
}