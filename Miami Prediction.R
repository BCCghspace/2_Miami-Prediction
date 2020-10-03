####load libraries, etc
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

####READ DATA####
#projected to NAD 1983 StatePlane Florida East FIPS 0901 Feet

#Study area base
miamiBound <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/Municipal_Boundary.geojson") %>%
  st_transform('ESRI:102658') %>%
  filter(NAME == "MIAMI BEACH" | NAME == "MIAMI") %>%
  st_union()

#Training + test data
houses <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/studentsData.geojson") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102658')

#OSM bounding box
xmin = st_bbox(miamiBound)[[1]]
ymin = st_bbox(miamiBound)[[2]]
xmax = st_bbox(miamiBound)[[3]]  
ymax = st_bbox(miamiBound)[[4]]

#greenSpace <- opq(bbox = c(xmin, ymin, xmax, ymax)) %>% 
  add_osm_feature(key = 'landuse', value = c("recreation_ground","village_green")) %>%
  osmdata_sf()

#bars, restaurants, shopping

#greenSpace <- 
#  greenSpace$osm_points %>%
#  .[miamiBound,]

green <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/green.geojson") %>%
  st_transform('ESRI:102658') %>% 
  filter(CITY == "Miami" | CITY == "Miami Beach")

#school dist
schoolDist <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/School_Board_District.geojson") %>%
  st_transform('ESRI:102658') %>% 
  filter(CITY == "Miami" | CITY == "Miami Beach")

#Public school catchments
elementarySchool <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/Elementary_School_Attendance_Boundary.geojson") %>%
  st_transform('ESRI:102658') %>% 
  filter(CITY == "Miami" | CITY == "Miami Beach")

middleSchool <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/Middle_School_Attendance_Boundary.geojson") %>%
  st_transform('ESRI:102658') %>% 
  filter(CITY == "Miami" | CITY == "Miami Beach")

highSchool <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/High_School_Attendance_Boundary.geojson") %>%
  st_transform('ESRI:102658') %>% 
  filter(CITY == "Miami" | CITY == "Miami Beach")

#crime - (vandalism could reduce home prices - broken windows theory)
  
#beaches
beach <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/Beach_Polygon.geojson") %>%
  st_transform('ESRI:102658') %>% 
  filter(CITY == "Miami" | CITY == "Miami Beach")

#malls
malls <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/Major_Mall.geojson") %>%
  st_transform('ESRI:102658') %>% 
  filter(CITY == "Miami" | CITY == "Miami Beach")

#contaminated sites
contaminated <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/Multi-Property_Contaminated_Site.geojson") %>%
  st_transform('ESRI:102658')


################PART 1: DATA################
####TABLE: SUMM. STATS + VAR. DESCRIPTIONS (sorted by category)####

####CORRELATION MATRIX####

####SCATTERPLOTS: 4 HOME PRICE CORRELATIONS (we choose, open data)####

####MAP: DEPENDENT VAR. (sale price)####
ggplot() +
  geom_sf(data = miamiBound, fill = "black") +
  geom_sf(data = houses, aes(colour = q5(salePrice)), 
          show.legend = "point", size = 0.7) +
  scale_colour_manual(values = palette5,
                      labels=qBr(houses,"salePrice"),
                      name="Quintile\nBreaks") +
  labs(title="Home Sale Price, Miami") +
  mapTheme()

####MAPS: 3 MOST INTERESTING INDEPENDENT VARS.####

####OTHER MAPS/GRAPHS/CHARTS OF INTEREST####





################PART 2: RESULTS################
####SPLIT toPredict == 0 into TRAINING and TEST sets####

####TABLE: TRAINING LM SUMMARY RESULTS####

####TABLE: MAE, MAPE FOR SINGLE TEST SET####

####CROSS-VALIDATION TEST RESULTS - 100 FOLDS (PLOT X-VALID MAE AS HISTOGRAM)####

####PLOT: PREDICTED PRICES x OBSERVED PRICES####

####MAP: TEST SET RESIDUALS####

####MORANS I TEST####

####PLOT OF SPATIAL LAG IN ERRORS####

####MAP: PREDICTED VALUES FOR WHERE toPREDICT IS BOTH 0 and 1####

####MAP: MAPE BY NEIGHBORHOOD (USE TEST SET PREDICTIONS)####

####SCATTERPLOT: MAPE BY NEIGHBORHOOD AS FUNCTION OF MEAN PRICE BY NEIGHBORHOOD####

####TEST GENERALIZABILITY: SPLIT CITY INTO 2 GROUPS WITH TIDYCENSUS (e.g. income, race)####
