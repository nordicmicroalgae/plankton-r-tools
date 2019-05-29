# Script description
# Plot a map of the Skagerrak with bubbles for Alexandrium abundance using PBSmapping
# Bengt Karlson 19 May 2019
# paths are for Mac/UNIX
# Change / to \\ if Windows is used


# GSHHG - A Global Self-consistent, Hierarchical,
# High-resolution Geography Database http://www.soest.hawaii.edu/pwessel/gshhg/

# put the folder "gshhg-bin-2.3.6" in the maptools or in the PBSmapping folder
# you will need the path to the folder you use


# make sure you have copied the directory gshhg-bin-2.3.6
# to the Mac directory Library/Frameworks/R.framework/Versions/3.5/Rresources/library/maptools/share
# På svenska: /Bibliotek/Frameworks/R.framework/Resources/library

# load libraries ----------------------------------------------------------

library(tidyverse) # includes ggplot2 tidyr etc
library(stringi) #useful when sorting data
library(stringr) #useful when sorting data
library(lubridate) # useful for workiing with dates
library(cowplot) # useul for combining multiple plots
library(scales)
library(ggthemes)
library(scales)
library(maps)
library(mapdata)
library(PBSmapping)

# set paths ----------------------------------------------------------

bpath<-'/Users/Bengt/Documents/SMHI/R-BK-working-dir/projects/plankton_r_tools' # set base path
setwd(bpath) # set working directory

# load data ----------------------------------------------------------
map_data <- read.table("data/släggö_n14_anholt_e_2017_aggregated_to_genus_zeroes_added.txt",
                       sep="\t",
                       skip=0,
                       header = TRUE,
                       na.strings = "",
                       comment.char = "", # needed to avoid problems with "# counted"
                       encoding = "latin1") # may need to be specified on Mac

# select the data you would like to use

# check variables
variables <- variable.names(map_data)

# select variables of interest
map_data1 = map_data %>%
  filter(parameter == "Abundance") %>%
  select(sample_date,station_name,sample_latitude_dd,sample_longitude_dd,sample_min_depth_m,sample_max_depth_m,scientific_name,parameter,value,unit)

variables1 <- variable.names(map_data1)

#create dates that r can use
map_data1 = map_data1 %>%
  mutate(rdate = as.POSIXct(sample_date, "%Y-%m-%d", tz = 'GMT')) %>%
  mutate(ryear = year(rdate)) %>%
  mutate(rmonth = month(rdate)) %>%
  mutate(rday = day(rdate))
  
  
# select depth 0-10 m only
map_data1 = map_data1 %>%
  filter(sample_min_depth_m == 0) %>%
  filter(sample_max_depth_m == 10)

# check depths
sample_min_depths <- unique(map_data1$sample_min_depth_m)
sample_max_depths <- unique(map_data1$sample_max_depth_m)


# select Alexandrium only
map_data1 = map_data1 %>%
  filter(scientific_name %in% c("Alexandrium"))

# select April only
map_data1 = map_data1 %>%
  filter(rmonth == 4)

# change name of latittude and longitude
map_data2 <- map_data1 %>%
  mutate(lon = sample_longitude_dd)  %>%
  mutate(lat = sample_latitude_dd)  %>%
  select(station_name,lat,lon,scientific_name,value,rdate)


# modify data for PBSmapping
# spaces are not allowed in station names, it seems
# take away the space in "N14 FALKENBERG"

map_data2$station_name<-gsub("N14 FALKENBERG", "N14", map_data2$station_name, ignore.case = FALSE, perl = FALSE,
                            fixed = TRUE, useBytes = FALSE)

# Create event id EID
map_data3 <- map_data2 %>%
  mutate(X = lon)  %>%
  mutate(Y = lat)  %>%
  mutate(Z = value) %>%
  mutate(EID = 1:length(lat)) %>% #create a list of event numbers
  select(EID,X,Y,Z,station_name)
  

# save the data as a text file
write.table(map_data3, "data_output/map_data3.txt", sep = "\t", quote = FALSE, na = "", row.names=F)


# create a map with stations ----------------------------------------------

# Specify desired map boundaries:
xlim <- c(2, 13)
ylim <- c(54, 60)


#import the data
myevents <- importEvents("data_output/map_data3.txt",projection = "LL")

# set the paths for the high resolution map data
# full resolution
# callGSHHG <- system.file("share/gshhg-bin-2.3.6/gshhs_f.b",
#                         package = "maptools", mustWork = TRUE)

#set the paths for the high resolution map data
#high resolution
# callGSHHG <- system.file("share/gshhg-bin-2.3.6/gshhs_h.b",
#                          package = "maptools", mustWork = TRUE)



#set the paths for the intermediate resolution map data
#intermediate resolution
callGSHHG <- system.file("share/gshhg-bin-2.3.6/gshhs_i.b",
                         package = "maptools", mustWork = TRUE)

#set the paths for the low resolution map data
#low resolution
# callGSHHG <- system.file("share/gshhg-bin-2.3.6/gshhs_l.b",
#                          package = "maptools", mustWork = TRUE)

map_area <- importGSHHS(callGSHHG, xlim = xlim,
                               ylim = ylim, maxLevel = 1)


# plot the mapp -----
#set a name for the map
pdf('plots/alexandrium_map_kattegat_skagerrak.pdf',family = "Helvetica", width = 5) #,width = 3.5, height = 4
#plot the map
plotMap (map_area, col = "grey", bg = "white",projection = "LL",cex = 1)
# Add the locations along with the labels:
addBubbles(myevents,
           max.size = 0.5,
           symbol.bg = "red",
           symbol.zero = FALSE,
           legend.title = "Alexandrium abundance",
           legend.type = "vert",
           legend.cex = 0.5)

text(myevents$X+0.4, myevents$Y-0.05, myevents$station_name,
     pos = 1, cex = 0.7)

# #Add text
text (10, 58, "Skagerrak",cex=1.1,font = 3)


dev.off()


