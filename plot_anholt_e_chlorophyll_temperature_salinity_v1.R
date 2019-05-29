# Read and plot chlorophyll data from staion Anholt E in the Kattegat
#
# Bengt Karlson 19 June 2019
# paths are for Mac/UNIX
# Change / to \\ if Windows is used

# Data have been downloaded from http://sharkweb.smhi.se
# Tabell Fysik/kemi kolumner
# Rubrikrad: internt namn

# Mac/Unix:
# Decimal/fält-avgränsare: Punkt/tab
# Radbrytning: UNIX
# Teckenkodning: UTF-8

# Windows:
# Decimal/fält-avgränsare: Punkt/tab
# Radbrytning: Windows
# Teckenkodning: Windows-1252

# load libraries ----------------------------------------------------------

library(tidyverse) # includes ggplot2 tidyr etc
library(stringi) #useful when sorting data
library(stringr) #useful when sorting data
library(lubridate) # useful for workiing with dates
library(cowplot) # useul for combining multiple plots
library(scales)
library(ggthemes)
library(ggpmisc)
library(ggpubr)

# set paths ----------------------------------------------------------
bpath<-'/Users/Bengt/Documents/SMHI/R-BK-working-dir/projects/plankton_r_tools' # set base path
setwd(bpath) # set working directory

# set start and enddates ---------------------------------------------

starttime <- ISOdatetime(1972,1,1,0,0,1, tz = "GMT")
start1998 <- ISOdatetime(1998,1,1,0,0,1, tz = "GMT")
endtime <- ISOdatetime(2017,12,31,23,59,59, tz = "GMT")
start_end = c(starttime,endtime)


# load data ----------------------------------------------------------

phys_chem_data <- read.table("data/shark_phys_chem/phys_chem_anholt_e_1972_2017_utf8.txt",
                            sep="\t",
                            skip=0,
                            header = TRUE,
                            na.strings = "")


phys_chem_variables <- variable.names(phys_chem_data)

#create dates that r can use
phys_chem_data = phys_chem_data %>%
  mutate(rdate = as.POSIXct(visit_date, "%Y-%m-%d", tz = 'GMT'))


# create separate columns for year, month, day
phys_chem_data$ryear <- year(phys_chem_data$rdate)
phys_chem_data$rmonth <- month(phys_chem_data$rdate)
phys_chem_data$rday <- mday(phys_chem_data$rdate)


# create sensible names of variables
phys_chem_data2 = phys_chem_data %>%
  mutate(latitude = sample_latitude_dd) %>%
  mutate(longitude = sample_longitude_dd) %>%
  mutate(depth = sample_depth_m) %>%
  mutate(secchi = Secchi.depth..m.) %>%
  mutate(temp = Temperature.bottle..C.) %>%
  mutate(temp_ctd = Temperature.CTD..C.) %>%
  mutate(salinity = Salinity.bottle..o.oo.psu.) %>%
  mutate(salinity_ctd = Salinity.CTD..o.oo.psu.) %>%
  mutate(po4 = Phosphate.PO4.P..umol.l.) %>%
  mutate(sumno3no2 = Nitrite.Nitrate.NO2.NO3.N..umol.l.) %>%
  mutate(no3 = Nitrate.NO3.N..umol.l.) %>%
  mutate(no2 = Nitrite.NO2.N..umol.l.) %>%
  mutate(nh4 = Ammonium.NH4.N..umol.l.) %>%
  mutate(sio3 = Silicate.SiO3.Si..umol.l.) %>%
  mutate(totn = Total.Nitrogen.Tot.N..umol.l.) %>%
  mutate(totp = Total.phosphorus.Tot.P..umol.l.) %>%
  mutate(chla = Chlorophyll.a.bottle..ug.l.) %>%
  mutate(ph = pH) %>%
  mutate(totalk = Alkalinity..mmol.kg.) %>%
  mutate(humus = Humus..ug.l.) %>%
  mutate(oxygen = Dissolved.oxygen.O2.bottle..ml.l.) %>%
  mutate(oxygen_ctd = Dissolved.oxygen.O2.CTD..ml.l.) %>%
  mutate(pressure_ctd = Pressure.CTD..dbar.) %>%
  mutate(doc = Dissovled.organic.carbon.DOC..umol.l.) %>%
  mutate(poc = Particulate.organic.carbon.POC..umol.l.) %>%
  mutate(pon = Particulate.organic.nitrogen.PON..umol.l.) %>%
  
  select(visit_date,
         station_name,
         latitude,
         longitude,
         rdate,
         ryear,
         rmonth,
         rday,
         secchi,
         water_depth_m,
         depth,
         temp,
         temp_ctd,
         salinity,
         salinity_ctd,
         po4,
         sumno3no2,
         no2,
         no3,
         nh4,
         sio3,
         humus,
         ph,
         totalk,
         totn,
         totp,
         chla,
         oxygen,
         oxygen_ctd,
         pressure_ctd,
         doc,
         poc,
         pon)

# combine ctd and bottle data, bottle data is preferred ----------------------------------------------

x <- phys_chem_data2$salinity
y <- phys_chem_data2$salinity_ctd

z <-coalesce(x,y)

phys_chem_data2$salinity_combined <- z   

x <- phys_chem_data2$temp
y <- phys_chem_data2$temp_ctd

z <-coalesce(x,y)

phys_chem_data2$temp_combined <- z     

# calculate N:P ratio

phys_chem_data2 = phys_chem_data2 %>%
  mutate(np_ratio = (no3 + no2 + nh4)/po4)


# select chla data from 0-10 m

phys_chem_data3 <- phys_chem_data2 %>%
  filter(rdate >= start1998) %>%
  filter(depth >= 0) %>%
  filter(depth <= 10) %>%
  select(station_name,rdate,latitude,longitude,depth,chla)


data_to_plot <- phys_chem_data3 %>%
  group_by(rdate,station_name) %>%
  summarise(
    chla_mean = mean(chla),
    chla_sd = sd(chla),
    chla_min = min(chla),
    chla_max = max(chla),
    number_of_samples = n())

# make some plots ---------------------------------------------------------
#  chl a
# Anholt E
anholt_e_chla_plot <- data_to_plot %>%
  filter(station_name %in% c("ANHOLT E"))  %>%
  ggplot +
  aes(
    x = rdate,
    y = chla_mean
  ) +
  geom_point(shape = 21, 
             colour = "darkgreen",
             fill = "darkgreen",
             size = 1) +
  ylim(0, 30) +
  scale_x_datetime(limits = start_end, date_breaks = "5 years", date_labels = "%Y") +
  labs(x = "År", y = 'klorofyll a, µg/l') +
  ggtitle("Anholt E", subtitle = 'Chlorophyll a, means 0-10 m') +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(plot.title = element_text(size = 14, lineheight=.8, face="bold",hjust = 0.5),
        plot.subtitle=element_text(size=14, hjust=0.5, face="italic", color="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

print(anholt_e_chla_plot)

save_plot("plots/anholt_e_chla_plot.pdf", anholt_e_chla_plot,
          ncol = 1, # we're saving a grid plot of 1 column
          nrow = 1, # and 1 row
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.5
)


# surface temperature and salinity
# surface salinity
# Anholt E
anholt_e_surface_temperature_plot <- phys_chem_data2 %>%
  filter(station_name %in% c("ANHOLT E"))  %>%
  filter(depth <= 1)  %>%
  ggplot +
  aes(
    x = rdate,
    y = temp_combined
  ) +
  geom_point(shape = 21, 
             colour = "red",
             fill = "red",
             size = 1) +
  ylim(-5, 25) +
  scale_x_datetime(limits = start_end, date_breaks = "10 years", date_labels = "%Y") +
  labs(x = "Year", y = 'Temperature, °C') +
  ggtitle("Anholt E", subtitle = 'Temperature, surface') +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(plot.title = element_text(size = 14, lineheight=.8, face="bold",hjust = 0.5),
        plot.subtitle=element_text(size=14, hjust=0.5, face="italic", color="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

print(anholt_e_surface_temperature_plot)

save_plot("plots/anholt_e_surface_temperature_plot.pdf", anholt_e_surface_temperature_plot,
          ncol = 1, # we're saving a grid plot of 1 column
          nrow = 1, # and 1 row
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.5
)


# surface salinity
# Anholt E
anholt_e_surface_salinity_plot <- phys_chem_data2 %>%
  filter(station_name %in% c("ANHOLT E"))  %>%
  filter(depth <= 1)  %>%
  ggplot +
  aes(
    x = rdate,
    y = salinity_combined
  ) +
  geom_point(shape = 21, 
             colour = "blue",
             fill = "blue",
             size = 1) +
  # ylim(-5, 25) +
  scale_x_datetime(limits = start_end, date_breaks = "10 years", date_labels = "%Y") +
  labs(x = "Year", y = 'Salinity, ppt') +
  ggtitle("Anholt E", subtitle = 'Salinity, surface') +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(plot.title = element_text(size = 14, lineheight=.8, face="bold",hjust = 0.5),
        plot.subtitle=element_text(size=14, hjust=0.5, face="italic", color="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

print(anholt_e_surface_salinity_plot)

save_plot("plots/anholt_e_surface_salinity_plot.pdf", anholt_e_surface_salinity_plot,
          ncol = 1, # we're saving a grid plot of 1 column
          nrow = 1, # and 1 row
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.5
)

# calculate means and standard deviatons and plot

more_data_to_plot = phys_chem_data2 %>%
  filter(depth <= 1) %>%
  filter(ryear >= 1988) %>%
  select(rdate,ryear,rmonth,temp_combined,salinity_combined)

temperature_barplot = more_data_to_plot %>%
  ggplot() +
  geom_boxplot(
    aes(
      x = rmonth,
      y = temp_combined,
      group = rmonth
    )) +
  # scale_fill_brewer(palette = "Set1") +
  ggtitle('Anholt E, Temperature medians 1988 to 2017') +
  # ylim(-5, 25) +
  # xlim(startdate, enddate) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  # scale_y_continuous(trans = "log10", limits = c(0.001,1),breaks=c(0.001,0.01, 0.1,1)) +
  # xlim(0, 13) +
  xlab("Month") +
  # ylab(bquote('Biovolume, '*mm^3*' '*L^-1*''))+
  ylab("Temeprature, °C")+
  
  theme_bw(base_size = 14, base_family = "sans") +
  theme(plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # panel.grid.major = element_line(colour = 'black', linetype = 'dotted'),
        # panel.grid.minor = element_line(colour = 'black', linetype = 'dotted'),
        legend.position="right",
        legend.title = element_blank())

print(temperature_barplot)



salinity_barplot = more_data_to_plot %>%
  ggplot() +
  geom_boxplot(
    aes(
      x = rmonth,
      y = salinity_combined,
      group = rmonth
    )) +
  # scale_fill_brewer(palette = "Set1") +
  ggtitle('Anholt E, salinity, medians 1988 to 2017') +
  ylim(10, 35) +
  # xlim(startdate, enddate) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  # scale_y_continuous(trans = "log10", limits = c(0.001,1),breaks=c(0.001,0.01, 0.1,1)) +
  # xlim(0, 13) +
  xlab("Month") +
  # ylab(bquote('Biovolume, '*mm^3*' '*L^-1*''))+
  ylab("Temeprature, °C")+
  
  theme_bw(base_size = 14, base_family = "sans") +
  theme(plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # panel.grid.major = element_line(colour = 'black', linetype = 'dotted'),
        # panel.grid.minor = element_line(colour = 'black', linetype = 'dotted'),
        legend.position="right",
        legend.title = element_blank())

print(salinity_barplot)

two_bar_plots <- plot_grid(temperature_barplot,salinity_barplot, labels = c("A", "B"), ncol = 1)

print(two_bar_plots)

save_plot("plots/anholt_e_bar_plots.pdf", two_bar_plots,
          ncol = 1, # we're saving a grid plot of 1 column
          nrow = 2, # and 1 row
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.5
)

# correlations --------------------------------------------------

# check correlation between depth and salinity

correlation_data = phys_chem_data2 %>%
  select(depth,salinity_combined,temp_combined)

salinity_depth_correlation <- ggscatter(correlation_data,
                                              x = "salinity_combined",
                                              y = "depth",
                                              add = "reg.line",
                                              add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                                              conf.int = TRUE # Add confidence interval
) +
  stat_cor(method = "pearson", label.x = 15, label.y = 35)

# Add correlation coefficient
salinity_depth_correlation_with_coeff <- salinity_depth_correlation + stat_cor(method = "pearson", label.x = 15, label.y = 50)

print(salinity_depth_correlation_with_coeff)


correlation_data = phys_chem_data2 %>%
  select(depth,salinity_combined,temp_combined)




salinity_temperature_correlation <- ggscatter(correlation_data,
                                      x = "salinity_combined",
                                      y = "temp_combined",
                                      add = "reg.line",
                                      add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                                      conf.int = TRUE # Add confidence interval
 ) +
   stat_cor(method = "pearson", label.x = 15, label.y = 15)

 # Add correlation coefficient
salinity_temperature_correlation + stat_cor(method = "pearson", label.x = 15, label.y = 15)

print(salinity_temperature_correlation)
