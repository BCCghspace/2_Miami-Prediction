#load libraries, etc
library(tidyverse)
library(sf)
library(spdep)
library(caret)
library(ckanr)
library(FNN)
library(grid)
library(gridExtra)
library(ggcorrplot)
library(stargazer)
library(mapview)
library(osmdata)

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  )
}

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )
}

palette5 <- c("#25CB10", "#5AB60C", "#8FA108",   "#C48C04", "#FA7800")

qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

q5 <- function(variable) {as.factor(ntile(variable, 5))}

#read data (project to NAD 1983 StatePlane Florida East FIPS 0901 Feet)
miamiBound <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/Municipal_Boundary.geojson") %>%
  filter(NAME == "MIAMI BEACH" | NAME == "MIAMI") %>%
  st_union()

  #OSM bounding box
xmin = st_bbox(miamiBound)[[1]]
ymin = st_bbox(miamiBound)[[2]]
xmax = st_bbox(miamiBound)[[3]]  
ymax = st_bbox(miamiBound)[[4]]

greenSpace <- opq(bbox = c(xmin, ymin, xmax, ymax)) %>% 
  add_osm_feature(key = 'landuse', value = c("recreation_ground","village_green")) %>%
  osmdata_sf()

greenSpace <- 
  greenSpace$osm_points %>%
  .[miamiBound,]

parks <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/parks.geojson")
ggplot() +
  geom_sf(data=miamiBound, fill="black") +
  geom_sf(data=parks, colour="green", size=.75) 

studentsData <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/studentsData.geojson") %>%
  st_transform('ESRI:102658')

  #schools, etc
landmarks <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/Landmarks.geojson") %>%
  st_transform('ESRI:102658')

  #crime - (vandalism could reduce home prices - broken windows theory)
  
  #water distance



