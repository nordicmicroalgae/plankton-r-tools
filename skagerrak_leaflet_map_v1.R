# Script description
# Map based pn leaflets
# Bengt Karlson 24 May 2019
# paths are for Mac/UNIX
# Change / to \\ if Windows is used


# load libraries ----------------------------------------------------------

library(tidyverse) # includes ggplot2 tidyr etc
library(stringi) #useful when sorting data
library(lubridate) # useful for workiing with dates
library(cowplot) # useul for combining multiple plots
library(scales)
library(ggthemes)
library(leaflet, quietly = T, warn.conflicts = F)
library(mapview, quietly = T, warn.conflicts = F)



# set paths ----------------------------------------------------------

bpath<-'/Users/Bengt/Documents/SMHI/R-BK-working-dir/projects/plankton_r_tools' # set base path
setwd(bpath) # set working directory

# define start and end dates ------------------------------------------
start2016 <- ISOdatetime(2016,8,1,0,0,0, tz = "GMT")
end2016 <- ISOdatetime(2016,10,31,0,0,0, tz = "GMT")

start_end_2016 <- c(start2016,end2016)

# load data

mydata <- read.table("data/PTBX_1.3.0_Demofiler/REG_program_SE_Wcoast_2017.txt",
                     header = TRUE,
                     sep = "\t",
                     fileEncoding = "latin1")

# select unique station names and positions

position_data <- mydata %>%
  select(station_name,sample_latitude_dd,sample_longitude_dd)

# select unique stations and postions
position_data <- unique (position_data)

# change name
pts <- position_data %>%
  rename(lat = sample_latitude_dd) %>%
  rename(lon = sample_longitude_dd)
  
  # make a simple track line
lin = data.frame(lon = c(9, 10, 10.5),
                 lat = c(58.5, 58.5, 58.5))

# # make a few points
# pts = data.frame(lon = c(9, 10, 10.5),
#                  lat = c(58.5, 58.5, 58.5))

# build a polygon
ply = data.frame(lon = c(10.5, 10, 9.5, 8.5),
                 lat = c(58.5, 58.3, 58.2, 58))


# ply = data.frame(lon = c(-64.916667, -64.983333, -65.516667, -66.083333),
#                  lat = c(43.266667, 42.783333, 42.65, 42.866667))

# create a map

# start basemap (note the argument to hide the zoom buttons)
map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  
  # add ocean basemap
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  
  # focus map in a certain area / zoom level
  setView(lng = 10, lat = 58.5, zoom = 7) %>%
  
  # add inset map
  addMiniMap(
    tiles = providers$Esri.OceanBasemap,
    position = 'topleft', 
    width = 200, height = 200,
    toggleDisplay = FALSE) %>%
  
  # add graticules with nice labels (recommended for static plot)
  addSimpleGraticule(interval = 1) %>%
  
  # add graticules from a NOAA webserver (recommended for interactive plot)
  # addWMSTiles(
  #   "https://gis.ngdc.noaa.gov/arcgis/services/graticule/MapServer/WMSServer/",
  #   layers = c("1-degree grid", "5-degree grid"),
  #   options = WMSTileOptions(format = "image/png8", transparent = TRUE),
  #   attribution = NULL,group = 'Graticules') %>%
  
  # add points (as circle markers)
  addCircleMarkers(data = pts, ~lon, ~lat,
                   weight = 0.5,
                   col = 'black', 
                   fillColor = 'red',
                   radius = 4, 
                   fillOpacity = 0.9, 
                   stroke = T, 
                   label = ~paste0('Point at: ', 
                                   as.character(round(lat,3)), ', ', 
                                   as.character(round(lon,3))), 
                   group = 'Points') %>%
  
  # add lines
  addPolylines(data = lin, ~lon, ~lat,
               weight = 3,
               color = 'red',
               popup = 'This is a line!', 
               smoothFactor = 3,
               group = 'Lines') %>%
  
  # add polygons
  addPolygons(data=ply, lng=~lon, lat=~lat,
              weight = 1, 
              color = 'grey', 
              fillColor = 'grey',
              fill = T, 
              fillOpacity = 0.25, 
              stroke = T, 
              dashArray = c(5,5), 
              smoothFactor = 3,
              options = pathOptions(clickable = F),
              group = 'Polygons')

# show map
map

# save map as static image

webshot::install_phantomjs()
mapshot(map, file = 'skagerrak_leaflet_map.png')
