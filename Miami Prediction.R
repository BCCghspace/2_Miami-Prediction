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
library(tidycensus)
library(tidygeocoder)

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

#nearest neighbor function 
nn_function <- function(measureFrom,measureTo,k) {
  measureFrom_Matrix <- as.matrix(measureFrom)
  measureTo_Matrix <- as.matrix(measureTo)
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.dist
  output <-
    as.data.frame(nn) %>%
    rownames_to_column(var = "thisPoint") %>%
    gather(points, point_distance, V1:ncol(.)) %>%
    arrange(as.numeric(thisPoint)) %>%
    group_by(thisPoint) %>%
    summarize(pointDistance = mean(point_distance)) %>%
    arrange(as.numeric(thisPoint)) %>% 
    dplyr::select(-thisPoint) %>%
    pull()
  
  return(output)  
}

####READ DATA####
#projected to NAD 1983 StatePlane Florida East FIPS 0901 Feet

#Study area base
miamiBound <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/Municipal_Boundary.geojson") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102658') %>%
  filter(NAME == "MIAMI BEACH" | NAME == "MIAMI") %>%
  st_union()

#Training + test data
houses <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/studentsData.geojson") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102658')

train <- filter(houses, SalePrice != 0)
predict <- filter(houses, SalePrice == 0) 

#census
#census_api_key("d9ebfd04caa0138647fbacd94c657cdecbf705e9", install = TRUE, overwrite = TRUE)
#vacant property, mhhinc, white, population
#acs <- 
#  get_acs(geography = "tract", variables = c("B25005_002E", "B19013_001E", "B01001A_001E", "B01003_001E"), 
 #         year=2010, state=12, county=086, geometry=T) %>% 
#  st_transform('ESRI:102658')

#demographics + tracts
demogr <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/Tract_Pop_2010.geojson") %>%
  st_transform('ESRI:102658')

tracts <- 
  rbind(
    st_centroid(demogr)[miamiBound,] %>%
      st_drop_geometry() %>%
      left_join(demogr) %>%
      st_sf() %>%
      mutate(inMiami = "YES"),
    st_centroid(demogr)[miamiBound, op = st_disjoint] %>%
      st_drop_geometry() %>%
      left_join(demogr) %>%
      st_sf() %>%
      mutate(inMiami = "NO")) %>%
  filter(inMiami == "YES") %>%
  mutate(pctWhite = WHITENH / POP2010) %>%
  dplyr::select(NAME10, pctWhite)
  
#OSM bounding box
xmin = st_bbox(miamiBound)[[1]]
ymin = st_bbox(miamiBound)[[2]]
xmax = st_bbox(miamiBound)[[3]]  
ymax = st_bbox(miamiBound)[[4]]

#bars, restaurants, shopping
#foodBev <- opq(bbox = c(xmin, ymin, xmax, ymax)) %>% 
#  add_osm_feature(key = 'amenity', value = c("bar","pub","restaurant","cafe")) %>%
#  osmdata_sf()
#foodBev <-
#  foodBev$osm_points %>%
#  .[miamiBound,]

green <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/green.geojson") %>%
  st_as_sf(coords = c("LON", "LAT"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102658')
green <- rbind(
    st_centroid(green)[miamiBound,] %>%
      st_drop_geometry() %>%
      left_join(green) %>%
      st_sf() %>%
      mutate(inMiami = "YES"),
    st_centroid(green)[miamiBound, op = st_disjoint] %>%
      st_drop_geometry() %>%
      left_join(green) %>%
      st_sf() %>%
      mutate(inMiami = "NO")) %>%
  filter(inMiami == "YES")

#school dist
schoolDist <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/School_Board_District.geojson") %>%
  st_as_sf(coords = c("LON", "LAT"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102658')

#Public school catchments
elementary <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/Elementary_School_Attendance_Boundary.geojson") %>%
  st_as_sf(coords = c("LON", "LAT"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102658') 
elementary <- rbind(
  st_centroid(elementary)[miamiBound,] %>%
    st_drop_geometry() %>%
    left_join(elementary) %>%
    st_sf() %>%
    mutate(inMiami = "YES"),
  st_centroid(elementary)[miamiBound, op = st_disjoint] %>%
    st_drop_geometry() %>%
    left_join(elementary) %>%
    st_sf() %>%
    mutate(inMiami = "NO")) %>%
  filter(inMiami == "YES") %>%
  dplyr::select(NAME)

middle <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/Middle_School_Attendance_Boundary.geojson") %>%
  st_as_sf(coords = c("LON", "LAT"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102658')
middle <- rbind(
  st_centroid(middle)[miamiBound,] %>%
    st_drop_geometry() %>%
    left_join(middle) %>%
    st_sf() %>%
    mutate(inMiami = "YES"),
  st_centroid(middle)[miamiBound, op = st_disjoint] %>%
    st_drop_geometry() %>%
    left_join(middle) %>%
    st_sf() %>%
    mutate(inMiami = "NO")) %>%
  filter(inMiami == "YES") %>%
  dplyr::select(NAME)


high <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/High_School_Attendance_Boundary.geojson") %>%
  st_as_sf(coords = c("LON", "LAT"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102658')
high <- rbind(
  st_centroid(high)[miamiBound,] %>%
    st_drop_geometry() %>%
    left_join(high) %>%
    st_sf() %>%
    mutate(inMiami = "YES"),
  st_centroid(high)[miamiBound, op = st_disjoint] %>%
    st_drop_geometry() %>%
    left_join(high) %>%
    st_sf() %>%
    mutate(inMiami = "NO")) %>%
  filter(inMiami == "YES") %>%
  dplyr::select(NAME)

#crime - (vandalism could reduce home prices - broken windows theory)
  
#beaches
beach <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/Beach_Polygon.geojson") %>%
  st_as_sf(coords = c("LON", "LAT"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102658')
beach <-   rbind(
  st_centroid(beach)[miamiBound,] %>%
    st_drop_geometry() %>%
    left_join(beach) %>%
    st_sf() %>%
    mutate(inMiami = "YES"),
  st_centroid(beach)[miamiBound, op = st_disjoint] %>%
    st_drop_geometry() %>%
    left_join(beach) %>%
    st_sf() %>%
    mutate(inMiami = "NO")) %>%
  filter(inMiami == "YES")

#malls
malls <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/Major_Mall.geojson") %>%
  st_as_sf(coords = c("LON", "LAT"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102658') %>% 
  filter(CITY == "Miami" | CITY == "Miami Beach")

#transport
bus <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/Bus_Stop.geojson") %>%
  st_as_sf(coords = c("LON", "LAT"), crs = 4326, agr = "constant")  %>%
  st_transform('ESRI:102658')
bus <- rbind(
  bus[miamiBound,] %>%
    st_drop_geometry() %>%
    left_join(bus) %>%
    st_sf() %>%
    mutate(inMiami = "YES"),
  bus[miamiBound, op = st_disjoint] %>%
    st_drop_geometry() %>%
    left_join(bus) %>%
    st_sf() %>%
    mutate(inMiami = "NO")) %>%
  filter(inMiami == "YES")

metromover <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/Metromover_Station.geojson") %>%
  st_as_sf(coords = c("LON", "LAT"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102658') 
metromover <- rbind(
  metromover[miamiBound,] %>%
    st_drop_geometry() %>%
    left_join(metromover) %>%
    st_sf() %>%
    mutate(inMiami = "YES"),
  metromover[miamiBound, op = st_disjoint] %>%
    st_drop_geometry() %>%
    left_join(metromover) %>%
    st_sf() %>%
    mutate(inMiami = "NO")) %>%
  filter(inMiami == "YES")

metrorail <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/Metrorail_Station.geojson") %>%
  st_as_sf(coords = c("LON", "LAT"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102658') 
metrorail <- rbind(
  metrorail[miamiBound,] %>%
    st_drop_geometry() %>%
    left_join(metrorail) %>%
    st_sf() %>%
    mutate(inMiami = "YES"),
  metrorail[miamiBound, op = st_disjoint] %>%
    st_drop_geometry() %>%
    left_join(metrorail) %>%
    st_sf() %>%
    mutate(inMiami = "NO")) %>%
  filter(inMiami == "YES")

#culture
culture <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/Culture_Venue.geojson") %>%
  st_as_sf(coords = c("LON", "LAT"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102658') %>%
  filter(CITY == "Miami" | CITY == "Miami Beach")

#commercial property
commercial <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/Commercial_Property.geojson") %>%
  st_as_sf(coords = c("LON", "LAT"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102658') 

commercial <- rbind(
  commercial[miamiBound,] %>%
    st_drop_geometry() %>%
    left_join(commercial) %>%
    st_sf() %>%
    mutate(inMiami = "YES"),
  commercial[miamiBound, op = st_disjoint] %>%
    st_drop_geometry() %>%
    left_join(commercial) %>%
    st_sf() %>%
    mutate(inMiami = "NO")) %>%
  filter(inMiami == "YES")

commercial.sf <-
  commercial %>%
  filter(LAT > -1) %>%
  dplyr::select(LAT, LON) %>%
  na.omit() %>%
  st_as_sf(coords = c("LAT", "LON"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102658') %>%
  distinct()

#flood zone
flood <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/FEMA\ FLOOD\ ZONE") %>%
  st_as_sf(coords = c("LON", "LAT"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102658') 

#contaminated sites
contaminated <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/Multi-Property_Contaminated_Site.geojson") %>%
  st_as_sf(coords = c("LON", "LAT"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102658')
 contaminated <-
   rbind(
  st_centroid(contaminated)[miamiBound,] %>%
    st_drop_geometry() %>%
    left_join(contaminated) %>%
    st_sf() %>%
    mutate(inMiami = "YES"),
  st_centroid(contaminated)[miamiBound, op = st_disjoint] %>%
    st_drop_geometry() %>%
    left_join(contaminated) %>%
    st_sf() %>%
    mutate(inMiami = "NO")) %>%
  filter(inMiami == "YES")

 ################DATA WRANGLING################
 
 #split house set
 train <- filter(houses, SalePrice != 0)
 predict <- filter(houses, SalePrice == 0) 
 

 ggplot() + 
   geom_sf(data = train, aes(colour=q5(beachDist))) + 
   scale_fill_manual(values = palette5) +
   mapTheme()
 #nearest neighbor commercial
 st_c <- st_coordinates
 
 train <-
   train %>% 
   mutate(
     beachDist = st_distance(train, beach),
     #commercial properties
     commNN1 = nn_function(st_c(st_centroid(train)), st_c(st_centroid(commercial.sf)), 1),
     commNN2 = nn_function(st_c(st_centroid(train)), st_c(st_centroid(commercial.sf)), 2), 
     commNN3 = nn_function(st_c(st_centroid(train)), st_c(st_centroid(commercial.sf)), 3), 
     commNN4 = nn_function(st_c(st_centroid(train)), st_c(st_centroid(commercial.sf)), 4), 
     commNN5 = nn_function(st_c(st_centroid(train)), st_c(st_centroid(commercial.sf)), 5),
     #green space
     greenNN1 = nn_function(st_c(st_centroid(train)), st_c(st_centroid(green)), 1),
     greenNN2 = nn_function(st_c(st_centroid(train)), st_c(st_centroid(green)), 2),
     greenNN3 = nn_function(st_c(st_centroid(train)), st_c(st_centroid(green)), 3),
     greenNN4 = nn_function(st_c(st_centroid(train)), st_c(st_centroid(green)), 4),
     greenNN5 = nn_function(st_c(st_centroid(train)), st_c(st_centroid(green)), 5),
     #metro mover stations
     metroMNN1 = nn_function(st_c(st_centroid(train)), st_c(metromover), 1),
     metroMNN2 = nn_function(st_c(st_centroid(train)), st_c(metromover), 2), 
     metroMNN3 = nn_function(st_c(st_centroid(train)), st_c(metromover), 3), 
     metroMNN4 = nn_function(st_c(st_centroid(train)), st_c(metromover), 4), 
     metroMNN5 = nn_function(st_c(st_centroid(train)), st_c(metromover), 5),
     #metro rail stations
     metroRNN1 = nn_function(st_c(st_centroid(train)), st_c(metrorail), 1),
     metroRNN2 = nn_function(st_c(st_centroid(train)), st_c(metrorail), 2), 
     metroRNN3 = nn_function(st_c(st_centroid(train)), st_c(metrorail), 3), 
     metroRNN4 = nn_function(st_c(st_centroid(train)), st_c(metrorail), 4), 
     metroRNN5 = nn_function(st_c(st_centroid(train)), st_c(metrorail), 5),
     #bus stations
     busNN1 = nn_function(st_c(st_centroid(train)), st_c(bus), 1),
     busNN2 = nn_function(st_c(st_centroid(train)), st_c(bus), 2), 
     busNN3 = nn_function(st_c(st_centroid(train)), st_c(bus), 3), 
     busNN4 = nn_function(st_c(st_centroid(train)), st_c(bus), 4), 
     busNN5 = nn_function(st_c(st_centroid(train)), st_c(bus), 5),
     #culture
     cultureNN1 = nn_function(st_c(st_centroid(train)), st_c(bus), 1),
     cultureNN3 = nn_function(st_c(st_centroid(train)), st_c(bus), 3), 
     cultureNN5 = nn_function(st_c(st_centroid(train)), st_c(bus), 5), 
     #mall
     mallNN1 = nn_function(st_c(st_centroid(train)), st_c(st_centroid(malls)), 1),
     mallNN2 = nn_function(st_c(st_centroid(train)), st_c(st_centroid(malls)), 2), 
     mallNN3 = nn_function(st_c(st_centroid(train)), st_c(st_centroid(malls)), 3), 
     mallNN4 = nn_function(st_c(st_centroid(train)), st_c(st_centroid(malls)), 4), 
     mallNN5 = nn_function(st_c(st_centroid(train)), st_c(st_centroid(malls)), 5))

 

#add catchment info
 train <-
   st_intersection(elementary, train) %>%
   rename(elemCatch = NAME) %>%
   st_intersection(middle, train) %>%
   rename(middleCatch = NAME) %>%
   st_intersection(high, train) %>%
   rename(highCatch = NAME)
 
 #add census tract, race info
 train <-
   st_intersection(tracts, train) %>%
   rename(censusTract = NAME10, tractPctWhite = pctWhite)

 ggplot() + 
   geom_sf(data = train, aes(fill = commNN5), colour = "transparent") +
   scale_colour_manual(values = palette5) +
   mapTheme()
 
 
 
 
 #cor test
 cor.test(train$LivingSqFt, train$SalePrice, method = "pearson")
 
 #reg1
 reg <- lm(SalePrice ~ ., data = st_drop_geometry(train) %>% 
             dplyr::select(SalePrice, LivingSqFt, highCatch, 
                           Bed, Bath, YearBuilt,
                           censusTract, tractPctWhite))
 summary(reg)
 
################PART 1: DATA################
 
####TABLE: SUMM. STATS + VAR. DESCRIPTIONS (sorted by category)####

####CORRELATION MATRIX####
 numericVars <- 
   select_if(st_drop_geometry(train), is.numeric) %>% na.omit()
 
 ggcorrplot(
   round(cor(numericVars), 1), 
   p.mat = cor_pmat(numericVars),
   colors = c("#25CB10", "white", "#FA7800"),
   type="lower",
   insig = "blank") +  
   labs(title = "Correlation across numeric variables") 

####SCATTERPLOTS: 4 HOME PRICE CORRELATIONS (we choose, open data)####
 st_drop_geometry(train) %>% 
   mutate(Age = 2020 - YearBuilt) %>%
   dplyr::select(SalePrice, elemCatch, Age, busNN5) %>%
   filter(SalePrice <= 1000000, Age < 500) %>%
   gather(Variable, Value, -SalePrice) %>% 
   ggplot(aes(Value, SalePrice)) +
   geom_point(size = .5) + geom_smooth(method = "lm", se=F, colour = "#FA7800") +
   facet_wrap(~Variable, ncol = 3, scales = "free") +
   labs(title = "Price as a function of continuous variables") +
   plotTheme()
 
####MAP: DEPENDENT VAR. (sale price)####
ggplot() +
  geom_sf(data = tracts, colour = "white", fill = "gray") +
#  geom_sf(data = miamiBound, fill = "black") +
  geom_sf(data = train, aes(colour = q5(SalePrice)), 
          show.legend = "point", size = 0.7) +
  scale_colour_manual(values = palette5,
                      labels=qBr(train,"SalePrice"),
                      name="Quintile\nBreaks") +
  labs(title="Home Sale Price", subtitle="Miami, FL") +
  mapTheme()

####MAPS: 3 MOST INTERESTING INDEPENDENT VARS.####
 ggplot() + geom_sf(data = tracts, fill = "grey40") +
   stat_density2d(data = data.frame(st_coordinates(green)), 
                  aes(X, Y, fill = ..level.., alpha = ..level..),
                  size = 0.01, bins = 40, geom = 'polygon') +
   scale_fill_gradient(low = "red", high = "green", name = "Density") +
   scale_alpha(range = c(0.00, 0.35), guide = FALSE) +
   labs(title = "Density of Green Space, Miami") +
   mapTheme()
 
 ggplot() + geom_sf(data = tracts, fill = "grey40") +
   stat_density2d(data = data.frame(st_coordinates(commercial)), 
                  aes(X, Y, fill = ..level.., alpha = ..level..),
                  size = 0.01, bins = 100, geom = 'polygon') +
   scale_fill_gradient(low = "green", high = "orange", name = "Density") +
   scale_alpha(range = c(0.00, 0.5), guide = FALSE) +
   labs(title = "Density of Commercial Properties, Miami") +
   mapTheme()
 
 ggplot() + geom_sf(data = tracts, colour = "white", fill = "black") +
   geom_sf(data = commercial, size = 0.1, colour = "yellow") +
   mapTheme()

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
