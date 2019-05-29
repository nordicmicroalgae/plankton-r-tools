# Script description
# Plot a station map using PBSmapping
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
map_data <- read.table("data/station_list.txt",
                       sep = "\t",
                       header = TRUE)


map_data2 <- map_data %>%
  mutate(lon = longitude)  %>%
  mutate(lat = latitude)  %>%
  select(short__station_name,lat,lon)


# modify data for PBSmapping
# spaces are not allowed in station names, it seems
# take away the space in "Anholt E"

map_data2$short__station_name<-gsub("Anholt E", "Anholt", map_data2$short__station_name, ignore.case = FALSE, perl = FALSE,
                            fixed = TRUE, useBytes = FALSE)


map_data3 <- map_data2 %>%
  mutate(X = lon)  %>%
  mutate(Y = lat)  %>%
  mutate(EID = 1:length(lat)) %>% #create a list of event numbers
  select(EID,X,Y,short__station_name)
  

# save the data as a text file
write.table(map_data3, "data_output/map_data3.txt", sep = "\t", quote = FALSE, na = "", row.names=F)


# create a map with stations ----------------------------------------------

# Specify desired map boundaries:
xlim <- c(2, 31)
ylim <- c(53.4, 66.5)


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

baltic_sea_area <- importGSHHS(callGSHHG, xlim = xlim,
                               ylim = ylim, maxLevel = 1)

#set a name for the map
pdf('plots/station_map_baltic.pdf',family = "Helvetica", width = 5) #,width = 3.5, height = 4
#plot the map
plotMap (baltic_sea_area, col = "grey", bg = "white",projection = "LL",cex = 1)
# Add the locations along with the labels:
addPoints(myevents, pch = 21, col = "red", bg = "red", cex = 1)
text(myevents$X+0.4, myevents$Y-0.05, myevents$short__station_name,
     pos = 1, cex = 0.7)
# #Add text

text (6, 57, "North",cex=0.7,font = 3)
text (6, 56.7, "Sea",cex=0.7,font = 3)
text (18.7, 56, "Baltic",cex=0.7,font = 3)
text (18.7, 55.7, "Proper",cex=0.7,font = 3)
text (19.2, 61.6, "Bothnian",cex=0.7,font = 3)
text (19.2, 61.3, "Sea",cex=0.7,font = 3)
text (23.2, 65.1, "Bothnian",cex=0.7,font = 3)
text (23.2, 64.8, "Bay",cex=0.7,font = 3)

dev.off()


