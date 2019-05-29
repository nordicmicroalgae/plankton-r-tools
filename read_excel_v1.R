# Script description
# Demonstrates reading data from Excel files
# Bengt Karlson 1 May 2018
# paths are for Mac/UNIX
# Change / to \\ if Windows is used


# load libraries ----------------------------------------------------------

library(readxl)


# set paths
bpath<-'/Users/Bengt/Documents/SMHI/R-BK-working-dir/projects/planktologists_r_companion' # set base path
setwd(bpath) # set working directory

# load the data for a list of stations
mydata <- read_excel("data/station_list.xlsx")

# load some data from Plankton Toolbox counting module

# First sheet (default)
mydata2 <- read_excel("data/Anholt E_2017-03-11_0-10m.xlsx")

# First sheet (selected)
mydata3 <- read_excel("data/Anholt E_2017-03-11_0-10m.xlsx",
                      sheet = "Table summary")

# Fifth sheet (selected)
mydata4 <- read_excel("data/Anholt E_2017-03-11_0-10m.xlsx", sheet = "README")







