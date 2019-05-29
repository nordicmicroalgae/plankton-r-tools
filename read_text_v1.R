# Script description
# Demonstrates reading text files
# Bengt Karlson 1 May  2018
# paths are for Mac/UNIX
# Change / to \\ if Windows is used


# load libraries ----------------------------------------------------------

# no libraries needed

# set paths ----------------------------------------------------------


# set paths
bpath<-'/Users/Bengt/Documents/SMHI/R-BK-working-dir/projects/planktologists_r_companion' # set base path
setwd(bpath) # set working directory


mydata <- read.table("data/station_list.txt",
                     sep = "\t",
                     header = TRUE)


# load phytoplankton data downloaded from http://sharkweb.smhi.se
mydata2 <- read.table("data/släggö_n14_anholt_e_2017.txt",
                     sep = "\t",
                     skip=0,
                     header = TRUE,
                     na.strings = "",
                     comment.char = "", # needed to avoid problems with "# counted"
                     encoding = "utf8") # may need to be specified)


# load phytoplankton data downloaded from http://sharkweb.smhi.se
# teh data has been aggregated to the species level using Plankton toolbox
# zeroes have been added when a species was not observed
mydata3 <- read.table("data/släggö_n14_anholt_e_2017_aggregated_to_species_zeroes_added.txt",
                      sep = "\t",
                      skip=0,
                      header = TRUE,
                      na.strings = "",
                      comment.char = "", # needed to avoid problems with "# counted"
                      encoding = "utf8") # may need to be specified)



