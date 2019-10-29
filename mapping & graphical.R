library(maps)
library(readr)
library(readxl)
library(geojsonio)
library(broom)
library(rgeos)
library(maps)
library(viridis)
library(RColorBrewer)
library(rgdal)
library(janitor)
library(viridis)
library(tidyverse)



# Prepare binning
state_data <- read_xlsx("By_state_data.xlsx") %>% clean_names()

# Prepare a color scale coming from the viridis color palette
my_palette <- rev(magma(8))[c(-1,-8)]

# Load this file. (Note: I stored in a folder called DATA)
spdf <- geojson_read("us_states_hexgrid.geojson",  what = "sp")
# Bit of reformating
spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

spdf_fortified <- spdf_fortified %>%
  left_join(. , state_data, by=c("id"="state")) 

spdf_fortified <- spdf_fortified %>%
  left_join(state_data, by=c("id"="state")) 



# Make a first chloropleth map
spdf_fortified %>% ggplot() +
  geom_polygon(aes(fill = per_capita.x, x = long, y = lat, group = group), colour= "#9A1B00", size=.1, alpha=0.9) +
  geom_text(data = centers, aes(x=x, y=y, label=id), color="white", size=8, alpha=0.6) +
  scale_fill_gradient(low = "#FFD8D0", high = "#9A1B00") +
  #scale_fill_gradient(trans = "log") +
  theme_void() +
  ggtitle( "Unclaimed Property per Capita by State" ) +
  theme(
    text = element_text(color = "#22211d"),
   # plot.background = element_rect(fill = , color = NA), 
  #  panel.background = element_rect(fill = "#f5f5f2", color = NA), 
  #  legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 18, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    legend.position = c(0.9, 0.2)
    )

spdf_fortified %>% ggplot() +
  geom_polygon(aes(fill = total_unclaimed.x, x = long, y = lat, group = group), colour= "#9A1B00", size=.1, alpha=0.9) +
  geom_text(data = centers, aes(x=x, y=y, label=id), color="white", size=8, alpha=0.6) +
  scale_fill_gradient(low = "#FFD8D0", high = "#9A1B00") +
  #scale_fill_gradient(trans = "log") +
  theme_void() +
  ggtitle( "Unclaimed Property Totals by State" ) +
  theme(
    text = element_text(color = "#22211d"),
#    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
#    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
#    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size=18, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    legend.position = c(0.9, 0.2)
  )