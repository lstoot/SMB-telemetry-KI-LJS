#Kenauk SMB Analysis

library(ggplot2)
library(sf)
library(qs)
library(readr)
library(dplyr)
library(ggspatial)
library(here)
library(mapview)

#---------------------------#
#    Building Figure 1      #
# --------------------------#

# we are going to load in your spatial data to plot the receivers on a Figure 1 map

setwd("~/GitHub/SMB-telemetry-KI-LJS")                 
spatial <- read.csv("Input/lake_papineau_qc.csv")        
head(spatial)                                   

#Now let's load the shape files of the lakes and rivers
lakes <- st_read("Input/shapefiles/lake_papineau_qc.shp") # Use sf package (keep projection!)


# --- ggplot for Figure 1 ----
#plot
sf_use_s2(FALSE)
plotA <- ggplot() + theme_bw() +
  geom_sf(data = lakes, fill = 'gray60', alpha = 0.4,  size = 0.07, colour = "gray60") +
  geom_point(data = spatial, aes(x = Longitude, y = Latitude, fill = Array), pch = 21, stroke = 0.2) +
  guides(fill = guide_legend(ncol = 1))
plotA

#Let's load in the data
data <- qread("Data/Kenauk_SMB.qs")
head(data)
str(data)


Fish_depth <- ggplot(daily_smb_depth_data, aes(x = month, y = mean_value, color = factor(floy_tag))) +
  geom_point() +
  labs(x = "Time", y = "Mean Depth", color = "Fish ID") +
  theme_minimal()
