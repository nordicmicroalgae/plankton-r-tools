# Script description
#
# Bengt Karlson 19 May 2019
# paths are for Mac/UNIX
# Change / to \\ if Windows is used


# load libraries ----------------------------------------------------------

library(tidyverse) # includes ggplot2 tidyr etc
library(stringi) #useful when sorting data
library(stringr) #useful when sorting data
library(lubridate) # useful for workiing with dates
library(cowplot) # useul for combining multiple plots
library(scales)
library(ggthemes)
library(scales)


# set paths ----------------------------------------------------------

bpath<-'/Users/Bengt/Documents/SMHI/R-BK-working-dir/projects/plankton_r_tools' # set base path
setwd(bpath) # set working directory


# define start and end dates ------------------------------------------

start2017 <- ISOdatetime(2017,1,1,0,0,0, tz = "GMT")
end2017 <- ISOdatetime(2017,12,31,0,0,0, tz = "GMT")

start_end_2017 <- c(start2017,end2017)


plankton_data <- read.table("data/släggö_n14_anholt_e_2017_aggregated_to_species_zeroes_added.txt",
                            sep="\t",
                            skip=0,
                            header = TRUE,
                            na.strings = "",
                            comment.char = "", # needed to avoid problems with "# counted"
                            encoding = "latin1") # may need to be specified on Mac

# check variables
variables <- variable.names(plankton_data)

plankton_data2 = plankton_data %>%
  filter(parameter == "Abundance") %>%
  select(sample_date,station_name,sample_latitude_dd,sample_longitude_dd,sample_min_depth_m,sample_max_depth_m,scientific_name,parameter,value,unit)

variables2 <- variable.names(plankton_data2)

#create dates that r can use
plankton_data2 = plankton_data2 %>%
  mutate(rdate = as.POSIXct(sample_date, "%Y-%m-%d", tz = 'GMT'))


plankton_data2$ryear <- year(plankton_data2$rdate)
plankton_data2$rmonth <- month(plankton_data2$rdate)
plankton_data2$rday <- mday(plankton_data2$rdate)


# # check what stations are found
list_of_stations <- as.character(unique(plankton_data2$station_name))

#sort the list
list_of_stations <- stri_sort(list_of_stations, decreasing = FALSE, na_last = NA, opts_collator = NULL)

#save the list as a txt file
write.csv(list_of_stations, "data_output/list_of_stations.txt", quote = FALSE, na = "", row.names=F)


# # check what taxa are found
list_of_taxa <- as.character(unique(plankton_data2$scientific_name))

#sort the list
list_of_taxa <- stri_sort(list_of_taxa, decreasing = FALSE, na_last = NA, opts_collator = NULL)

#save the list as a txt file
write.csv(list_of_taxa, "data_output/list_of_taxa.txt", quote = FALSE, na = "", row.names=F)


# start plotting ----------------------------------------------------------

# plot all taxa at all stations
plot0 <- plankton_data2 %>%
  ggplot +
  geom_point() +
  aes(x = rdate,
      y = value)

print(plot0)

# plot Skeletonema marinoi
plot1 <- plankton_data2 %>%
  filter(scientific_name == "Skeletonema marinoi") %>%
  filter(station_name == "SLÄGGÖ") %>%
  ggplot +
  geom_point() +
  aes(x = rdate,
    y = value)

print(plot1)

# Add nice legends and a title
plot2 <- plankton_data2 %>%
  filter(scientific_name == "Skeletonema marinoi") %>%
  filter(station_name == "SLÄGGÖ") %>%
  ggplot +
  aes(
    x = rdate,
    y = value) +
  # geom_hline(aes(yintercept = 1500) , color="red", linetype="dashed") +
  geom_point() +
  ggtitle('Skeletonema marinoi') +
  # ylim(0, 11000) +
  # scale_y_log10() +
  xlim(start2017, end2017) +
  xlab("Date") +
  ylab(bquote('Abundance, '*cells*' '*L^-1*''))+
  theme_bw(base_size = 11, base_family = "sans") +
  theme(plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5),
        # panel.grid.major = element_line(colour = 'black', linetype = 'dotted'),
        # panel.grid.minor = element_line(colour = 'black', linetype = 'dotted'),
        legend.position="right",
        legend.title = element_blank())

print(plot2)

# Use color to differentiate stations
plot3 <- plankton_data2 %>%
  filter(scientific_name == "Skeletonema marinoi") %>%
  # filter(station_name == "SLÄGGÖ") %>%
  ggplot +
  aes(
    x = rdate,
    y = value+1, # added 1 for log plot
    colour = station_name) +
  # geom_hline(aes(yintercept = 1500) , color="red", linetype="dashed") +
  geom_point() +
  ggtitle('Skeletonema marinoi') +
  # ylim(0, 11000) +
  scale_y_log10() +
  xlim(start2017, end2017) +
  xlab("Date") +
  ylab(bquote('Abundance, '*cells + 1*' '*L^-1*''))+
  theme_bw(base_size = 11, base_family = "sans") +
  theme(plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5),
        # panel.grid.major = element_line(colour = 'black', linetype = 'dotted'),
        # panel.grid.minor = element_line(colour = 'black', linetype = 'dotted'),
        legend.position="right",
        legend.title = element_blank())

print(plot3)

# Try a logarithmic x-axis
plot4 <- plankton_data2 %>%
  filter(scientific_name == "Skeletonema marinoi") %>%
  # filter(station_name == "SLÄGGÖ") %>%
  ggplot +
  aes(
    x = rdate,
    y = value+1) + # added 1 for log plot
  # geom_hline(aes(yintercept = 1500) , color="red", linetype="dashed") +
  geom_point() +
  ggtitle('Skeletonema marinoi') +
  # ylim(0, 11000) +
  scale_y_log10() +
  xlim(start2017, end2017) +
  xlab("Date") +
  ylab(bquote('Abundance, '*cells + 1*' '*L^-1*''))+
  theme_bw(base_size = 11, base_family = "sans") +
  theme(plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5),
        # panel.grid.major = element_line(colour = 'black', linetype = 'dotted'),
        # panel.grid.minor = element_line(colour = 'black', linetype = 'dotted'),
        legend.position="right",
        legend.title = element_blank())

print(plot4)


# plot Dinophysis
# Use different symbols to discrimate taxa
# Define the date labels position and type on the axis in detail
p106 <- plankton_data2 %>%
  filter(station_name == "SLÄGGÖ") %>%
  filter(scientific_name %in% c("Dinophysis acuminata",
                                "Dinophysis acuta",
                                "Dinophysis norvegica",
                                "Dinophysis tripos")) %>%
  ggplot +
  aes(
    x = rdate,
    y = value,
    shape = scientific_name) +
  scale_shape_manual(values = c(0,1,2,6),labels =c("Dinophysis acuminata",
                                                            "Dinophysis acuta",
                                                             "Dinophysis norvegica",
                                                            "Dinophysis tripos")) +
  # geom_hline(aes(yintercept = 1500) , color="red", linetype="dashed") +
  geom_point() +
  ggtitle('Dinophysis spp.') +
  # ylim(0, 25000) +
  # scale_y_log10(labels = trans_format(log10, math_format(10^.x))) +
  # xlim(start2017, end2017) +
  scale_x_datetime(lim = start_end_2017, date_breaks = "2 month", date_labels = "%b") +
  xlab("Month") +
  ylab(bquote('Abundance, '*cells*' '*L^-1*''))+
  theme_bw(base_size = 11, base_family = "sans") +
  theme(plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5),
        # panel.grid.major = element_line(colour = 'black', linetype = 'dotted'),
        # panel.grid.minor = element_line(colour = 'black', linetype = 'dotted'),
        legend.position="right",
        legend.title = element_blank())

print(p106)

# Save plot in two diffent formats
save_plot("plots/four Dinophysis 2017.pdf", p106, base_height = 3, base_width = 6) #inches
save_plot("plots/four Dinophysis 2017.png", p106, base_height = 3, base_width = 6) #inches

# tips and tricks

# modify data
# mutate(salinity = ifelse(salinity >= 10, 10 ,chl_fluor))

# change name of a station

plankton_data2 = plankton_data2 %>%
  mutate(station_name = ifelse(station_name == "ANHOLT E", "Anholt",
                               as.character(station_name)))
  

