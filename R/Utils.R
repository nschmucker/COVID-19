totitle <- function(x){
  s <- strsplit(x, " |_")[[1]]
  paste(
    toupper(substring(s, 1, 1)), tolower(substring(s, 2)),
    sep = "", collapse = " "
  )
}

custom_theme <- function(...){
  ggplot2::update_geom_defaults("point", list(size = 2.5))
  ggplot2::theme_grey(...) %+replace%
    ggplot2::theme(
      plot.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(
        color = "grey",
        hjust = 0, vjust = 1, size = ggplot2::rel(1.35),
        margin = ggplot2::margin(t = 10, b = 15, unit = "pt")
      ),
      plot.subtitle = element_text(
        color = "grey",
        hjust = 0, vjust = 1, size = ggplot2::rel(1.1),
        margin = ggplot2::margin(t = -5, b = 15, unit = "pt")
      ),
      plot.caption = ggplot2::element_text(
        color = "grey",
        hjust = 1, vjust = 1, size = ggplot2::rel(0.9)
      ),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_line(color = "brown", size = 0.7),
      axis.line = ggplot2::element_line(color = "brown", size = 0.6),
      axis.text = ggplot2::element_text(color = "brown", size = ggplot2::rel(0.9)),
      axis.title.y = ggplot2::element_text(
        color = "brown", vjust = 1.5,
        margin = ggplot2::margin(r = 10, l = 5), angle = 90,
        size = ggplot2::rel(1)
      ),
      axis.title.x = element_text(
        colour = "brown", vjust = -.5,
        margin = ggplot2::margin(t = 5, b = 5),
        size = rel(1)
      ),
      legend.title = ggplot2::element_text(color = "brown", hjust = 0),
      legend.key = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(color = "brown", size = ggplot2::rel(0.9)),
      legend.justification = "center"
    )
}