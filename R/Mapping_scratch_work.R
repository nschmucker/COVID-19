library(leaflet)
library(RColorBrewer)
library(tigris)
library(sf)

# Get data on counties (simplefeatures object)
counties <- tigris::counties(
  state = NULL,
  cb = TRUE,
  resolution = "500k",
  year = 2018,
  class = "sf"
) %>%
  sf::st_transform(crs = "+proj=longlat +datum=WGS84")

# Add info for plotting
counties <- counties %>% 
  sf::st_centroid() %>% 
  sf::st_coordinates() %>% 
  cbind(counties)

counties <- tigris::geo_join(
  spatial_data = counties,
  data_frame = {covid_19 %>% 
      dplyr::filter(LEVEL == "COUNTY") %>% 
      dplyr::mutate(GEOID = as.character(GEOID))
  },
  by_sp = "GEOID",
  by_df = "GEOID",
  how = "left"
)

# Get data on states (simplefeatures object)
states <- tigris::states(
  cb = TRUE, 
  resolution = "500k", 
  year = 2018,
  class = "sf"
) %>%
  sf::st_transform(crs = "+proj=longlat +datum=WGS84")

# Add info for plotting
states <- states %>% 
  sf::st_centroid() %>% 
  sf::st_coordinates() %>% 
  cbind(states)

states <- tigris::geo_join(
  spatial_data = states,
  data_frame = {covid_19 %>% 
      dplyr::filter(LEVEL == "State") %>% 
      dplyr::mutate(GEOID = as.character(GEOID))
  },
  by_sp = "GEOID",
  by_df = "GEOID",
  how = "left"
) 

pal <- leaflet::colorNumeric(
  palette = "YlGnBu",
  domain = countries$gdp_md_est
)

qpal <- leaflet::colorQuantile(
  palette = "YlOrRd", 
  domain = countries$gdp_md_est,
  n = 5
)

# The name of a preset palette from the RColorBrewer package, e.g. "RdYlBu", "Accent", or "Greens".
# The full name of a viridis palette: "viridis", "magma", "inferno", or "plasma".

map %>%
  leaflet::addPolygons(
    stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
    color = ~pal(gdp_md_est)
  ) %>%
  leaflet::addLabelOnlyMarkers(
    ~Long, ~Lat, label = ~leaflet::htmlEscape(COUNTY_NAME)
  )
leaflet::addLegend(
  "bottomright",
  pal = pal,
  values = ~gdp_md_est,
  title = "Est. GDP (2010)",
  labFormat = leaflet::labelFormat(prefix = "$"),
  opacity = 1
)



## OPTION 2

# fillColor = ~leaflet::colorQuantile(
#   palette = "YlOrRd",
#   domain = TOTAL_INFECTED_PER_100K,
#   n = 4)(TOTAL_INFECTED_PER_100K)

states$LABEL <- 
  paste0(
    "<strong>", states$STATE_NAME, "</strong><br/>",
    round(states$TOTAL_CASES, 0)
  ) %>% 
  lapply(htmltools::HTML)

# renderLeaflet({
# leaflet(reactive_df()) %>%
leaflet::leaflet(states) %>%
  leaflet::addPolygons(
    color = "#444444", weight = 1, smoothFactor = 0.5,
    opacity = 1.0, fillOpacity = 0.5,
    fillColor = ~leaflet::colorNumeric(
      palette = "YlOrRd",
      domain = TOTAL_CASES
    )(TOTAL_CASES),
    label = ~LABEL,
    highlightOptions = leaflet::highlightOptions(
      color = "white", weight = 2, bringToFront = TRUE)
  ) 
# })