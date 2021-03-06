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
library

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

#neighborhood 
#https://opendata.arcgis.com/datasets/2f54a0cbd67046f2bd100fb735176e6c_0.geojson
nhoods <- st_read("https://opendata.arcgis.com/datasets/2f54a0cbd67046f2bd100fb735176e6c_0.geojson") %>%
  st_as_sf(coords = c("LON", "LAT"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102658')
#not working
trial_houses <- st_read("E:/Upenn/CPLN508/miami/2_Miami-Prediction/Raw Data/studentsData.geojson")
trial_houses %>% mutate(SalePricePrFt = SalePrice / AdjustedSqFt) 

ggplot() +
  geom_sf(data = nhoods, fill = "grey40") +
  geom_sf(data = houses, aes(colour = q5(PricePerSq)), #PricePerSq not yet
          show.legend = "point", size = .75) +
  scale_colour_manual(values = palette5,
                      labels=qBr(boston,"PricePerSq"),
                      name="Quintile\nBreaks") +
  labs(title="Price Per Square Foot, Boston") +
  mapTheme()

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

#####housing correlation
#1*bedrooms

#2bathrooms  

#3year built

#4sq ft

#5lot size

#6senior vs long term senior
#land? bldg?total?
st_drop_geometry(houses) %>% 
  mutate(Age = 2020 - YearBuilt) %>%
  dplyr::select(SalePrice, LotSize, AdjustedSqFt, Age) %>%
  filter(SalePrice <= 1000000, Age < 500) %>%
  gather(Variable, Value, -SalePrice) %>% 
  ggplot(aes(Value, SalePrice)) +
  geom_point(size = .5) + geom_smooth(method = "lm", se=F, colour = "#FA7800") +
  facet_wrap(~Variable, ncol = 4, scales = "free") +
  labs(title = "Price as a function of continuous variables") +
  plotTheme()

##?Style, OWN_OCC
st_drop_geometry(houses) %>% 
  dplyr::select(SalePrice, Property.City, Stories) %>%
  mutate(Stories = as.factor(Stories)) %>%
  filter(SalePrice <= 1000000) %>%
  gather(Variable, Value, -SalePrice) %>% 
  ggplot(aes(Value, SalePrice)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  facet_wrap(~Variable, ncol = 1, scales = "free") +
  labs(title = "Price as a function of categorical variables", y = "Mean_Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  plotTheme()

##correlation of the house 
numericVars <- 
  select_if(st_drop_geometry(houses), is.numeric) %>% na.omit()

ggcorrplot(
  round(cor(numericVars), 1), 
  p.mat = cor_pmat(numericVars),
  colors = c("#25CB10", "white", "#FA7800"),
  type="lower",
  insig = "blank") +  
  labs(title = "Correlation across numeric variables")

####READ DATA####
#projected to NAD 1983 StatePlane Florida East FIPS 0901 Feet

#Study area base
miamiBound <- st_read("E:/Upenn/CPLN508/miami/2_Miami-Prediction/Raw Data/Municipal_Boundary.geojson") %>%
#miamiBound <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/Municipal_Boundary.geojson") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102658') %>%
  filter(NAME == "MIAMI BEACH" | NAME == "MIAMI") %>%
  st_union()

#Training + test data
houses <- st_read("E:/Upenn/CPLN508/miami/2_Miami-Prediction/Raw Data/studentsData.geojson") %>%
#houses <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/studentsData.geojson") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102658')
#mutate uncessussful
houses %>%
  mutate(SalePricePrFt = SalePrice / AdjustedSqFt) 

train <- filter(houses, SalePrice != 0)
predict <- filter(houses, SalePrice == 0) 

#census
census_api_key("d9ebfd04caa0138647fbacd94c657cdecbf705e9", install = TRUE, overwrite = TRUE)
#vacant property, mhhinc, white, population
#acs <-
dd18_5 <- load_variables(year = 2018, dataset = "acs5", cache = TRUE)
#tracts18 <-  get_acs(geography = "tract", variables = c(), 
#                           year=2018, state=12, county=086, geometry=T) %>% 
#  get_acs(geography = "tract", variables = c("B25005_002E", "B19013_001E", "B01001A_001E", "B01003_001E"), 
 #         year=2010, state=12, county=086, geometry=T) %>% 
#  st_transform('ESRI:102658')
#data that maybe useful: B25001	Housing Units; B25008	Total Population In Occupied Housing Units By Tenure
#we canuse to calculate renter units in an area
#the bigger the hh size, lower the prize? B25009	Tenure By Household Size
#B25097	Mortgage Status By Median Value (Dollars)
#B25104	Monthly Housing Costs
#https://www.socialexplorer.com/data/ACS2018_5yr/metadata/?ds=ACS18_5yr table to look for useful acs information

#demographics + tracts
#demogr <- st_read("E:/Upenn/CPLN508/miami/2_Miami-Prediction/Raw Data/Tract_Pop_2010.geojson")
#demogr <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/Tract_Pop_2010.geojson") %>%
#  st_transform('ESRI:102658')
#BC can't find such data
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
green <- st_read("E:/Upenn/CPLN508/miami/2_Miami-Prediction/Raw Data/green.geojson") %>%
#green <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/green.geojson") %>%
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
schoolDist <- st_read("E:/Upenn/CPLN508/miami/2_Miami-Prediction/Raw Data/School_Board_District.geojson") %>%
#schoolDist <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/School_Board_District.geojson") %>%
  st_as_sf(coords = c("LON", "LAT"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102658')

#Public school catchments
elementary <- st_read("E:/Upenn/CPLN508/miami/2_Miami-Prediction/Raw Data/Elementary_School_Attendance_Boundary.geojson") %>%
#elementary <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/Elementary_School_Attendance_Boundary.geojson") %>%
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
# beach <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/Beach_Polygon.geojson") %>%
#  st_as_sf(coords = c("LON", "LAT"), crs = 4326, agr = "constant") %>%
#  st_transform('ESRI:102658')
#beach <-   rbind(
#  st_centroid(beach)[miamiBound,] %>%
#    st_drop_geometry() %>%
#    left_join(beach) %>%
#    st_sf() %>%
#    mutate(inMiami = "YES"),
#  st_centroid(beach)[miamiBound, op = st_disjoint] %>%
#    st_drop_geometry() %>%
#    left_join(beach) %>%
#    st_sf() %>%
#    mutate(inMiami = "NO")) %>%
#  filter(inMiami == "YES")

water <- st_read("/Users/annaduan/Documents/GitHub/2_Miami\ Prediction/Raw\ Data/Water.geojson") %>%
  st_as_sf(coords = c("LON", "LAT"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102658')
water <- rbind(
  water[miamiBound,] %>%
    st_drop_geometry() %>%
    left_join(water) %>%
    st_sf() %>%
    mutate(inMiami = "YES"),
  water[miamiBound, op = st_disjoint] %>%
    st_drop_geometry() %>%
    left_join(water) %>%
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
#BC add bus station/rail station

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
 
 #####added variables
landuse <- st_read("https://opendata.arcgis.com/datasets/8c6e2575c8ad46eb887e6bb35825e1a6_0.geojson") 

utility_water <- st_read("https://opendata.arcgis.com/datasets/d22634dc2f4745de8a9d8fb72e2bce3a_0.geojson") 
 
utility_sewer <- st_read("https://opendata.arcgis.com/datasets/e4ac368322ee43c798ff194265e2a488_0.geojson")

employee_pay <- st_read("https://opendata.arcgis.com/datasets/4b372d5fa4884947a20d03464b292219_0.geojson")

low_income_depressed <- st_read("https://opendata.arcgis.com/datasets/40119bfc50274c1da548ec8022e9a7a9_0.geojson")

neighborhood_revitalization <- st_read("https://opendata.arcgis.com/datasets/fe6f419e21264158b18eb77be9870d97_0.geojson")

shop <- st_read("E:/Upenn/CPLN508/miami/2_Miami-Prediction/Raw Data/shop_point.geojson")
#supermarket as well as all the others



 ################DATA WRANGLING#################
 
 #split house set
 train <- filter(houses, SalePrice != 0)
 predict <- filter(houses, SalePrice == 0) 
 
 #TOD
 metroRBuffer <- 
   rbind(
     st_buffer(metrorail, 0.5*5280) %>% #in feet
       mutate(Legend = "Buffer") %>%
       dplyr::select(Legend),
     st_union(st_buffer(metrorail, 0.5*5280)) %>% #union buffer
       st_sf() %>%
       mutate(Legend = "Unioned Buffer"))
 
 ggplot() +
   geom_sf(data=st_union(tracts)) +
   geom_sf(data=metroRBuffer) +
   geom_sf(data=metrorail, show.legend = "point") +
   facet_wrap(~Legend) +  #wrap by years and make small multiple plots
   labs(caption = "Figure 1.2") +
   mapTheme()
 
# metroMBuffer <- 
#   rbind(
#     st_buffer(metromover, 0.5*5280) %>% #in feet
#       mutate(Legend = "Buffer") %>%
#       dplyr::select(Legend),
#     st_union(st_buffer(metromover, 0.5*5280)) %>% #union buffer
#       st_sf() %>%
#       mutate(Legend = "Unioned Buffer"))
 
# busBuffer <- 
#  rbind(
#     st_buffer(bus, 0.5*5280) %>% #in feet
#       mutate(Legend = "Buffer") %>%
#       dplyr::select(Legend),
#     st_union(st_buffer(bus, 0.5*5280)) %>% #union buffer
#      st_sf() %>%
#       mutate(Legend = "Unioned Buffer"))
 
 train.group <- 
   rbind(
     st_centroid(train)[metroRBuffer,] %>%
       st_drop_geometry() %>%
       left_join(train) %>%
       st_sf() %>%
       mutate(TOD = 1),
     st_centroid(train)[metroRBuffer, op = st_disjoint] %>%
       st_drop_geometry() %>%
       left_join(train) %>%
       st_sf() %>%
       mutate(TOD = 0))
 
 #beaches/water
 houseCentroid <- st_centroid(houses)
 houses <- houseCentroid %>%
   mutate(distWater = st_distance(., water)) %>%
     mutate(distWater = units::drop_units((distWater))) 
 #not sure what is this train
   

 st_c <- st_coordinates
 houses <-
   houses %>% 
   mutate(
     #commercial properties NN
     commNN2 = nn_function(st_c(st_centroid(train)), st_c(st_centroid(commercial.sf)), 2), 
     #green space
     greenNN1 = nn_function(st_c(st_centroid(train)), st_c(st_centroid(green)), 1),
     #metro mover stations
     metroMNN5 = nn_function(st_c(st_centroid(train)), st_c(metromover), 5),
     #metro rail stations
     metroRNN1 = nn_function(st_c(st_centroid(train)), st_c(metrorail), 1),
     #bus stations
     busNN5 = nn_function(st_c(st_centroid(train)), st_c(bus), 5),
     #culture
     cultureNN5 = nn_function(st_c(st_centroid(train)), st_c(bus), 5), 
     #mall
     mallNN2 = nn_function(st_c(st_centroid(train)), st_c(st_centroid(malls)), 2))

summary(houses)


#add catchment info
 houses <-
   st_intersection(elementary, houses) %>%
   rename(elemCatch = NAME) %>%
   st_intersection(middle, houses) %>%
   rename(middleCatch = NAME) %>%
   st_intersection(high, houses) %>%
   rename(highCatch = NAME)
 
 #add census tract, race info
 houses <-
   st_intersection(tracts, houses) %>%
   rename(censusTract = NAME10, tractPctWhite = pctWhite)

 ggplot() + 
   geom_sf(data = houses, aes(fill = commNN5), colour = "transparent") +
   scale_colour_manual(values = palette5) +
   mapTheme()
 
 #cor test
 cor.test(train.group$distWater, train.group$SalePrice, method = "pearson")
 
 #reg1
 reg <- lm(SalePrice ~ ., data = st_drop_geometry(train.group) %>% 
             dplyr::select(SalePrice, TOD, LivingSqFt, elemCatch, highCatch, middleCatch,
                           Bed, Bath, LotSize, Stories, Zoning, YearBuilt, Property.Zip, greenNN1, cultureNN5, commNN2))
 summary(reg)

#############HOUSE DATA WRANGLING###############

 
 

 
################PART 1: DATA################
 
####TABLE: SUMM. STATS + VAR. DESCRIPTIONS (sorted by category)####

####CORRELATION MATRIX####
 numericVars <- 
   select_if(st_drop_geometry(houses), is.numeric) %>% na.omit()
 
 ggcorrplot(
   round(cor(numericVars), 1), 
   p.mat = cor_pmat(numericVars),
   colors = c("#25CB10", "white", "#FA7800"),
   type="lower",
   insig = "blank") +  
   labs(title = "Correlation across numeric variables") 

####SCATTERPLOTS: 4 HOME PRICE CORRELATIONS (we choose, open data)####
 st_drop_geometry(houses) %>% 
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
  geom_sf(data = houses, aes(colour = q5(SalePrice)), 
          show.legend = "point", size = 0.7) +
  scale_colour_manual(values = palette5,
                      labels=qBr(houses,"SalePrice"),
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
