totitle <- function(x){
  s <- strsplit(x, " |_")[[1]]
  paste(
    toupper(substring(s, 1, 1)), tolower(substring(s, 2)),
    sep = "", collapse = " "
  )
}

hc_custom_theme <- function(...){
  font_color <- "grey"
  line_color <- "grey"
  palette <- RColorBrewer::brewer.pal(n = 8, name = "Dark2")
  
  theme <- highcharter::hc_theme(
    chart = list(
      plotBackgroundColor = "#FFFFFF"
    ),
    colors = palette,
    title = list(
      style = list(
        color = font_color,
        fontSize = "20px"
      ),
      align = "left"
    ),
    subtitle = list(
      style = list(
        color = font_color,
        fontSize = "16px"
      ),
      align = "left"
    ),
    credits = list(
      style = list(
        color = font_color,
        fontSize = "12px"
      )
    ),
    xAxis = list(
      labels = list(
        style = list(
          color = font_color,
          fontSize = "13px"
        )
      ),
      title = list(
        style = list(
          color = font_color,
          fontSize = "14px"
        )
      ),
      lineColor = line_color,
      lineWidth = 0.75,
      tickColor = line_color,
      tickLength = 5,
      minorTickInterval = 0,
      minorGridLineColor = "#FFFFFF"
    ),
    yAxis = list(
      labels = list(
        style = list(
          color = font_color,
          fontSize = "13px"
        )
      ),
      title = list(
        style = list(
          color = font_color,
          fontSize = "14px"
        )
      ),
      startOnTick = FALSE,
      lineWidth = 0.75,
      tickWidth = 0.75,
      tickLength = 5,
      lineColor = line_color,
      gridLineColor = "#FFFFFF",
      tickColor = line_color,
      minorTickInterval = 0,
      minorGridLineColor = "#FFFFFF"
    ),
    legendBackgroundColor = "rgba(0, 0, 0, 0.5)",
    background2 = "#505053",
    dataLabelsColor = "#B0B0B3",
    textColor = font_color,
    contrastTextColor = "#F0F0F3",
    maskColor = "rgba(255,255,255,0.3)"
  )
  
  if (length(list(...)) > 0) {
    theme <- highcharter::hc_theme_merge(
      theme,
      hc_theme(...)
    )
  }
  
  theme
}

write_export_list <- function(text, type, is_data = FALSE){
  
  x <- list(text = text)
  
  if (is_data) {
    x$onclick <- JS(glue::glue("function () {{ this.{type}(); }}"))
  } else {
    x$onclick <- JS(
      glue::glue("function () {{this.exportChart({{ type: '{type}' }}); }}")
    )
  }
  
  list(x)
}

hc_export_opts <- function(png = TRUE,
                           jpeg = TRUE,
                           svg = FALSE,
                           pdf = FALSE,
                           csv = TRUE,
                           xls = FALSE
){
  
  has_two_types <- (png + jpeg + svg + pdf > 0) & (csv + xls > 0)
  
  x <- list()
  
  if (png) {
    x <-
      append(
        x,
        write_export_list(text = "PNG image", type = "image/png")
      )
  }
  
  if (jpeg) {
    x <-
      append(
        x,
        write_export_list(text = "JPEG image", type = "image/jpeg")
      )
  }
  
  if (svg) {
    x <-
      append(
        x,
        write_export_list(text = "SVG image", type = "image/svg+xml")
      )
  }
  
  if (pdf) {
    x <-
      append(
        x,
        write_export_list(text = "PDF document", type = "application/pdf")
      )
  }
  
  if (has_two_types) {
    x <- append(x, list(list(separator = TRUE)))
  }
  
  if (csv) {
    x <-
      append(
        x,
        write_export_list(
          text = "CSV document",
          type = "downloadCSV",
          is_data = TRUE
        )
      )
  }
  
  if (xls) {
    x <-
      append(
        x,
        write_export_list(
          text = "XLS document",
          type = "downloadXLS",
          is_data = TRUE
        )
      )
  }
  
  x
}
