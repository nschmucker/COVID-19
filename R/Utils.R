totitle <- function(x){
  s <- strsplit(x, " |_")[[1]]
  paste(
    toupper(substring(s, 1, 1)), tolower(substring(s, 2)),
    sep = "", collapse = " "
  )
}

hc_custom_theme <- function(...){
  font_color <- "grey"
  line_color <- "brown"
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

write_export_list <- function(text, type, is_data = FALSE) {
  
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

# hc_custom_opts <- function(hc, export_options = hc_export_opts()){
#   hc %>%
#   hc_yAxis(min = 0, opposite = FALSE) %>%
#   hc_navigator(enabled = TRUE) %>%
#   hc_rangeSelector(enabled = FALSE) %>% 
#   hc_tooltip(
#     split = FALSE, shared = TRUE,
#     headerFormat = '<span style="font-size: 10px">{point.key}</span><br/>'
#   ) %>%
#   hc_chart(zoomType = "xy") %>%
#   hc_exporting(
#     enabled = TRUE,
#     filename = "Covid-19_Chart_Export",
#     formAttributes = list(target = "_blank"),
#     buttons =
#       list(
#         contextButton =
#           list(
#             text = "Export",
#             theme = list(fill = "transparent"),
#             menuItems = export_options
#           )
#       )
#   )
# }

# custom_theme <- function(...){
#   ggplot2::update_geom_defaults("point", list(size = 2.5))
#   ggplot2::theme_grey(...) %+replace%
#     ggplot2::theme(
#       plot.background = ggplot2::element_blank(),
#       plot.title = ggplot2::element_text(
#         color = font_color,
#         hjust = 0, vjust = 1, size = ggplot2::rel(1.35),
#         margin = ggplot2::margin(t = 10, b = 15, unit = "pt")
#       ),
#       plot.subtitle = element_text(
#         color = font_color,
#         hjust = 0, vjust = 1, size = ggplot2::rel(1.1),
#         margin = ggplot2::margin(t = -5, b = 15, unit = "pt")
#       ),
#       plot.caption = ggplot2::element_text(
#         color = font_color,
#         hjust = 1, vjust = 1, size = ggplot2::rel(0.9)
#       ),
#       panel.grid.major.x = ggplot2::element_blank(),
#       panel.grid.minor = ggplot2::element_blank(),
#       panel.border = ggplot2::element_blank(),
#       panel.background = ggplot2::element_blank(),
#       axis.ticks = ggplot2::element_line(color = "brown", size = 0.7),
#       axis.line = ggplot2::element_line(color = "brown", size = 0.6),
#       axis.text = ggplot2::element_text(color = "brown", size = ggplot2::rel(0.9)),
#       axis.title.y = ggplot2::element_text(
#         color = "brown", vjust = 1.5,
#         margin = ggplot2::margin(r = 10, l = 5), angle = 90,
#         size = ggplot2::rel(1)
#       ),
#       axis.title.x = element_text(
#         colour = "brown", vjust = -.5,
#         margin = ggplot2::margin(t = 5, b = 5),
#         size = rel(1)
#       ),
#       legend.title = ggplot2::element_text(color = "brown", hjust = 0),
#       legend.key = ggplot2::element_blank(),
#       legend.position = "bottom",
#       legend.text = ggplot2::element_text(color = "brown", size = ggplot2::rel(0.9)),
#       legend.justification = "center"
#     )
# }