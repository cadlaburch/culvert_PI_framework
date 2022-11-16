#Load Libraries
library(sf)
library(spData)
library(tidyverse)
library(here)
library(patchwork)
library(cowplot)
library(readxl)
library(scatterpie)
library(leaflet) #interactive maps
library(htmlwidgets) #save maps as .html files
library(biscale) #chloropleth maps
library(ggmap) #basemaps
library(ggsn) #arrow and scales
library(prettymapr)

#################
#FIRST STEP: Load in the geographies

#-------------WDFW
st_layers(here("data/culv_inventories/WdfwFishPassage/WdfwFishPassage.gdb")) #find layer
sf.allculv.wdfw <- st_read(here("data/culv_inventories/WdfwFishPassage/WdfwFishPassage.gdb"),
                           layer = "WDFW_FishPassageSite")

#-------------County Boundaries
counties <- read_sf(here("data", "cb_2018_us_county_5m", "cb_2018_us_county_5m.shp"))

#-------------WRIA Boundaries
st_layers(dsn = here("data/WR_WAT_WRIA.gdb")) #layer name: Water_Resource_Inventory_Areas
wria <- read_sf(dsn = here("data/WR_WAT_WRIA.gdb"), layer = "Water_Resource_Inventory_Areas")

#-------------Case Area Boundary
sf.case <- st_read(here("data/WSDOT_-_Fish_Passage_US_v._WA_Case_Area_Boundary-shp/WSDOT_-_Fish_Passage_US_v._WA_Case_Area_Boundary.shp"),
                   quiet = TRUE)

#-------------Bellingham City Limits
bham <- read_sf(here("data/bham_geom/COB_plan_CityUGAs.shp"))
bham.crs <- st_transform(bham, crs = st_crs(counties))
bham.crs <- bham.crs %>% 
  filter(CITYUGA == "CITY")

#-------------THURSTON (I DONT USE THIS DATA)
thurston.dat <- read_xlsx(here("data/Thurston/Fixed Thurston County FP Potential Project List.xlsx"))
#'ISSUE: Thurston data doesn't have lat long but I think I can match the culverts 
#'to a geometry by site ID in the WDFW database.

#filtering to only show scored culverts
thurston.dat <- thurston.dat %>%
  filter(TotalScore > 0)
#'there are several culverts that were grouped together in the ID for scoring. I need to separate them
#'so that it doesn't look like they are individual culverts 

thurston.dat <- thurston.dat %>% 
  separate_rows(`Asset ID`)
#next I need to get rid of the CU at the beginning of the ID because it doesnt match WDFW
thurston.dat$`Asset ID` <- gsub("CU", "", as.character(thurston.dat$`Asset ID`))

match2 <- sf.allculv.wdfw %>% 
  select(SiteRecordID) #just selecting ID and geometry from WDFW to simplify 

sf.thurston <- merge(x = match2, y = thurston.dat, by.x = "SiteRecordID", by.y = "Asset ID")
#the Site ID doesnt match the WDFW ID :(
#I tried both Site ID and SiteRecordID no luck
#Assumption: I'm ommitting Thurston for now 

#------------BELLINGHAM
#same issue as thurston, theres a site ID but no lat/long
bell.dat <- read_csv(here("data/2019_bellingham_ranking/bellingham_scores.csv"))

match <- sf.allculv.wdfw %>% 
  select(SiteId) #just selecting ID and geometry from WDFW to simplify

#matching WDFW ID's to Bellingham culverts
sf.bell <- merge(x = match, y = bell.dat, by.x = "SiteId", by.y = "Site ID")
#it worked!
#note that 4 culverts were omitted because their ID's didn't match

#checking geometry
ggplot() +
  geom_sf(data = sf.bell)

#------------CHEHALIS
chehalis.data <- read_csv(here("data/Chehalis/wdfw_chehalis_ranking.csv"))

#The following code is my very round-about way of trying to get the data into the right format to convert to sf
#I'm still working on getting better at data manipulation, so if you know of a more efficient method let me know!
chehalis.data$geoms <- gsub("=", "", as.character(chehalis.dat$geoms))
chehalis.data$geoms <- gsub("c", "", as.character(chehalis.data$geoms))
chehalis.data$geoms <- gsub("x", "", as.character(chehalis.data$geoms))
chehalis.data$geoms <- gsub("y", "", as.character(chehalis.data$geoms))
chehalis.data$geoms <- gsub(" ", "", as.character(chehalis.data$geoms))

chehalis.data1 <- chehalis.data %>% 
  separate(geoms, c("x", "y", "c", "d", "e", "f"))

chehalis.data1$X <- paste(chehalis.data1$y, chehalis.data1$c, sep = ".")
chehalis.data1$Y <- paste(chehalis.data1$d, chehalis.data1$e, sep = ".")

#transforming the X and Y columns to numeric and making x negative
chehalis.data1 <- chehalis.data1 %>% 
  mutate(X = as.numeric(X)) %>% 
  mutate(X = X*-1) %>% 
  mutate(Y = as.numeric(Y))

#checking that it worked
class(chehalis.data1$X)
class(chehalis.data1$Y)

#transforming to sf object
sf.chehalis <- st_as_sf(chehalis.data1, coords = c("X", "Y"), crs = 4269)

st_crs(sf.chehalis)
#checking plot
ggplot() +
  geom_sf(data = sf.chehalis) +
  geom_sf(data = sf.case, fill = "transparent")

#--------------CWCC
cwcc.dat <- read_csv(here("data/CWCCResults.csv"))

#There are several options to choose from for coordinates. x and y are strange numbers, not a recognizable crs.
#Latitude and Longitude (from database) I'm assuming these are the WDFW coordinates
#Latitude and Longitude (map adjusted) I'm assuming these are corrected coordinates based on CWCC inventory
#I'm choosing to use the WDFW coordinates because I want to match with WDFW inventory and it doesn't matter to me if they are a little off
#note that this is an assumption that may be a limitation of my map.

#filtering out NA values in lat/long columns
cwcc.dat <- cwcc.dat %>% 
  mutate_all(na_if, "") %>% #replace spaces with NA values
  filter(`Longitude (from database)` != "NA")

#transform to sf object
sf.cwcc <- st_as_sf(cwcc.dat, coords = c("Longitude (from database)", "Latitude (from database)"),
                    crs = 4269)

st_crs(sf.cwcc) 

#plot to check how it looks
ggplot() +
  geom_sf(data = sf.cwcc)+
  geom_sf(data = sf.case, fill = "transparent")

#-----------KING
king.dat <- read_csv(here("data", "Feb22King.csv"))
king.dat <- king.dat %>% 
  mutate_all(na_if, "") %>% #replace spaces with NA values
  filter(X != "NA") #remove NA observations froom coords

  #convert to sf
sf.king <- st_as_sf(king.dat, coords = c("X", "Y"), crs = 4269)

#plot to check how it looks
ggplot() +
  geom_sf(data = sf.king)+
  geom_sf(data = sf.case, fill = "transparent")

#------------Change CRS to match
sf.case <- st_transform(sf.case, crs = st_crs(counties))
sf.allculv.wdfw <- st_transform(sf.allculv.wdfw, crs = st_crs(counties))
wria <- st_transform(wria, crs = st_crs(counties))
sf.bell <- st_transform(sf.bell, crs = st_crs(counties))

##############################
##############################
#SECOND STEP: Filter for barrier culverts with PI scores

#---------------WDFW

# Subset culvert dataset by case area
sf.case.culverts <- sf.allculv.wdfw[sf.case, ] 

#filtering for culverts
sf.case.culverts = sf.case.culverts %>% 
  filter(grepl('Culvert', FeatureType)) %>% 
  filter(!grepl('Non', FeatureType)) 

#Decode based on cross reference (written by Braeden and Connor)
sf.case.culverts = sf.case.culverts %>% 
  mutate(FishPassageBarrierStatusCode = as.factor(case_when(
    FishPassageBarrierStatusCode == 0 ~ "NA",
    FishPassageBarrierStatusCode == 10 ~ "barrier",
    FishPassageBarrierStatusCode == 20 ~ "non-barrier",
    FishPassageBarrierStatusCode == 99 ~ "unknown"
  ))) %>% 
  mutate(PercentFishPassableCode = as.factor(case_when(
    PercentFishPassableCode == 0 ~ "NA",
    PercentFishPassableCode == 10 ~ "0",
    PercentFishPassableCode == 20 ~ "33",
    PercentFishPassableCode == 30 ~ "66",
    PercentFishPassableCode == 40 ~ "100",
    PercentFishPassableCode == 99 ~ "unkown"
  ))) %>% 
  mutate(OwnerTypeCode = as.factor(case_when(
    OwnerTypeCode == 1 ~ "city",
    OwnerTypeCode == 2 ~ "county",
    OwnerTypeCode == 3 ~ "federal",
    OwnerTypeCode == 4 ~ "private",
    OwnerTypeCode == 5 ~ "state",
    OwnerTypeCode == 6 ~ "tribal",
    OwnerTypeCode == 7 ~ "other",
    OwnerTypeCode == 8 ~ "port",
    OwnerTypeCode == 9 ~ "drainage district",
    OwnerTypeCode == 11 ~ "irrigation district",
    OwnerTypeCode == 12 ~ "unknown"
  ))) %>% 
  mutate(FishUseCode = as.factor(case_when(
    FishUseCode == 0 ~ "NA", #Think we should keep NAs?
    FishUseCode == 10 ~ "yes",
    FishUseCode == 20 ~ "no",
    FishUseCode == 99 ~ "unknown"
  ))) %>% 
  mutate(SignificantReachCode = as.factor(case_when(
    SignificantReachCode == 0 ~ "NA",
    SignificantReachCode == 10 ~ "yes",
    SignificantReachCode == 20 ~ "no",
    SignificantReachCode == 99 ~ "unknown"
  )))

# Filter out non barrier culverts, and culverts that weren't scored by PI  
sf.case.culverts = sf.case.culverts %>% 
  filter(PercentFishPassableCode != "100") %>% 
  filter(FishUseCode != "no") %>% 
  filter(FishPassageBarrierStatusCode != "non-barrier") %>% 
  filter(PriorityIndexTotalQuantity > 0) #filtering for only scored barriers

#check that WDFW is scoring culverts for different owner types and not just for state owned culverts
sf.case.culverts %>% 
  ggplot()+
  geom_point(aes(x = PriorityIndexTotalQuantity, y = OwnerTypeCode))

#Preliminary Plot
ggplot() +
  geom_sf(data = sf.case.culverts)+
  geom_sf(data = sf.case, fill = "transparent")

#---------------Bellingham
#All of the Bellingham barriers have a PI Score
#I'm making an assumption that they are all culverts, it doesn't say what type of barrier they are

#---------------Chehalis
#I checked and they only score barriers, so I don't have to filter out non-barriers
#All the barriers are scored
sf.chehalis <- sf.chehalis %>% 
  filter(FeatureType == "Culvert")

#---------------CWCC
#filter for culverts
sf.cwcc <- sf.cwcc %>% 
  filter(`Barrier Type` == "Culvert")

#inventory includes barriers and unknown passability
unique(sf.cwcc$`Barrier Passability`)

#---------------KING
#'King county is only looking at county owned barriers in their PI
#filtering for barrier culverts that received a PI score
sf.king <- sf.king %>%
  filter(FishPassageBarrierStatusCode==10, FishPassageFeatureTypeCode==1, OverallScore>0)

#preliminary plot
ggplot() +
  geom_sf(data = sf.king)+
  geom_sf(data = sf.case, fill = "transparent")+
  geom_sf(data = sf.chehalis) +
  geom_sf(data = sf.cwcc) +
  geom_sf(data = sf.bell) +
  geom_sf(data = sf.case.culverts, color = "red", alpha = 0.5)

#########################################
#########################################
#Step 3 Compare each PI to WDFW

#-------------------Bellingham

#crop wdfw to bellingham city limits
wdfw.bham <- sf.case.culverts[bham.crs, ] 

#filter wdfw for only city owned culverts (because that is what bellingham scored)
wdfw.bham <- wdfw.bham %>% 
  filter(OwnerTypeCode == "city")

#create dataframe of culverts that were scored by both bellingham and wdfw
shared <- sf.bell[wdfw.bham, ]

#this next section is prep for the scatterpie (honestly I don't know if the scatterpie is worth it, I found it buggy)
#transform back to dataframe 
bham_scatter <- sf.bell %>% 
  select(SiteId, `Total PI (WDFW Form)`, SCORE) %>% 
  st_drop_geometry() 

wdfw.scatter <- wdfw.bham %>% 
  select(SiteId, PriorityIndexTotalQuantity) %>% 
  st_drop_geometry()

#join bellingham scores vs 
scatter <- full_join(bham_scatter, wdfw.scatter, by = "SiteId")

#scatterpie doesnt like only making one pie so I had to make a second pie with dummy variables and then set the radius to 0
scatter.df <- data.frame(X = c(-122.4, -122.51),
                         Y = c(48.82, 48.8), 
                         WDFW = c(9, 20),
                         Both = c(10, 20),
                         Bellingham = c(18, 60),
                         radius = c(0.02, 0))

#create preliminary plot
ggplot()+
  geom_sf(data = bham.crs, fill = "transparent") +
  geom_sf(data = wdfw.bham, color = "coral2") + #
  geom_sf(data = sf.bell, color = "cornflowerblue") +
  geom_sf(data = shared, color = "green3") +
  geom_scatterpie(data = scatter.df, aes (x = X, y = Y, r = radius),
                  cols = c("WDFW", "Both", "Bellingham"),
                  pie_scale = 60,
                  color = NA) +
  scale_fill_discrete(name = "Culverts Scored By:") +
  theme_void() +
  labs(title = "Bellingham Geography")

#'This plot was the point where I decided to change my plan for the final map.
#'I think that this is useful information, but I think it will be more visually interesting to 
#'make a map that looks at the culverts that have been scored twice. I want to see if the 
#'scores are similar for most barriers or if they vary greatly. 

#---------------CHEHALIS
#Crop WDFW to Chehalis
  #I need a polygon to crop to so I have to pull in some of the WRIA data
chehalis.geom <- wria %>% 
  filter(WRIA_ID %in% c(40, 49)) %>% 
  summarise(geometry = sf::st_union(Shape)) %>%
  ungroup()

wdfw.chehalis <- sf.case.culverts[chehalis.geom, ] #cropping

#I don't have to filter for an ownership type because WDFW and Chehalis both score all ownership types

#Match WDFW ID to Chehalis ID
chehalis.df <- sf.chehalis %>% 
  select(BarrierID, OwnerType, Score) %>% 
  st_drop_geometry() 
merge.chehalis <- merge(x = wdfw.chehalis, y = chehalis.df, by.x = "SiteId", by.y = "BarrierID")

#Plot
ggplot()+
  geom_sf(data = chehalis.geom, fill = "transparent") +
  geom_sf(data = wdfw.chehalis, color = "coral2", alpha = 0.5) + #342 wdfw barriers
  geom_sf(data = sf.chehalis, color = "cornflowerblue", alpha = 0.5) + #1946 chehalis
  geom_sf(data = merge.chehalis, color = "green3") + #237 scored by both
  theme_void()

#---------------CWCC
#crop WDFW to CWCC
  #creating cwcc geometry
cwcc.geom <- wria %>% 
  filter(WRIA_ID %in% c(15, 35)) %>% 
  summarise(geometry = sf::st_union(Shape)) %>%
  ungroup()

wdfw.cwcc <- sf.case.culverts[cwcc.geom, ] 

unique(sf.cwcc$`Road Ownership`)
#looks like CWCC also scores all of the different ownership types

#Match WDFW ID to CWCC
cwcc.df <- sf.cwcc %>% 
  select(`WDFW ID`, `Road Ownership`, `Total Prioritization Score`) %>% 
  st_drop_geometry() 

merge.cwcc <- merge(x = wdfw.cwcc, y = cwcc.df, by.x = "SiteId", by.y = "WDFW ID")

#Plot
ggplot()+
  geom_sf(data = cwcc.geom) +
  geom_sf(data = wdfw.cwcc, color = "coral2", alpha = 0.5) + #180 wdfw barriers
  geom_sf(data = sf.cwcc, color = "cornflowerblue", alpha = 0.5) + #790 cwcc
  geom_sf(data = merge.cwcc, color = "green3") #126 scored by both

#---------------KING

#crop WDFW to king
#I found county FIPS codes here: https://www.cccarto.com/fipscodes/washington/
#create king county geometry
king.geom <- counties %>% 
  filter(GEOID == 53033)

#subset wdfw data to king county
wdfw.king <- sf.case.culverts[king.geom, ] 

unique(sf.king$OwnerEntity) #I think these are all county owned roads? Also private?
unique(wdfw.king$OwnerTypeCode)

#select county owned culverts
wdfw.king <- wdfw.king %>% 
  filter(OwnerTypeCode == c("county", "private"))

#merge the king county data with the wdfw data
#Match WDFW ID to CWCC
king.df <- sf.king %>% 
  select(`WDFWID`, `OwnerEntity`, `OverallScore`) %>% 
  st_drop_geometry() 

merge.king <- merge(x = wdfw.king, y = king.df, by.x = "SiteId", by.y = "WDFWID")

#Plot
ggplot()+
  geom_sf(data = king.geom) +
  geom_sf(data = wdfw.king, color = "coral2", alpha = 0.5) + #239 wdfw barriers
  geom_sf(data = sf.king, color = "cornflowerblue", alpha = 0.5) + #749 king
  geom_sf(data = merge.king, color = "green3") #25 scored by both

##############################
##############################
#Step 4: Bivariate Cloropleths

#I need to normalize the scores to a scale of 0-100
#'WDFW doesn't have a maximum possible score because habitat quantity is continuous
#'the range for WDFW is 0-82. So I'm going to multiply by 1.22 to change the range to 0-100.

#---------Bellingham
#the max score for bellingham is 31, multiply by 3.23
#convert to numeric
shared$`Total PI (WDFW Form)` <- as.numeric(shared$`Total PI (WDFW Form)`)
#normalizing scores
bham.chloro <- shared %>% 
  select(SiteId, `Total PI (WDFW Form)`, SCORE) %>% 
  mutate(wdfw_score = `Total PI (WDFW Form)`*1.22) %>% 
  mutate(bell_score = SCORE*3.23)

#creating chloro data
bham.bi.data <- bi_class(bham.chloro, x = bell_score, y = wdfw_score,
                         style = "quantile", dim = 3)

#locating a good basemap
#bounding box for basemap
st_bbox(bham.crs)
bham.bounds <- makebbox(w = -122.55134, e = -122.39871,
                        n = 48.81741, s = 48.68801)

#downloading stamen map
basemap <- get_stamenmap(bbox = as.vector(bham.bounds), zoom = 12, 
                   maptype = 'toner-lite') 

#checking how it looks
ggmap(basemap) + theme_void()

#creating main plot
bell.bi.map <- ggmap(basemap) + 
  geom_sf(data = bham.crs, fill = "transparent", color = "red", inherit.aes = F) +
  geom_sf(data = bham.bi.data,
          mapping = aes(fill = bi_class), alpha = 0.8, pch = 21,
          size = 4, show.legend = F, inherit.aes = F) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  bi_theme() +
  coord_sf(datum = NA) +
  ggsn::scalebar(x.min = -122.55134, x.max = -122.41,
                 y.min = 48.6991, y.max = 48.817405,
                 st.size = 3, st.dist = 0.05, dist = 1,
                 dist_unit = "km", transform = T,
                 model = 'WGS84') +
  ggsn::north(x.min = -122.55134, x.max = -122.41,
              y.min = 48.6991, y.max = 48.817405,
              scale = 0.1, symbol = 10, location = "bottomright",
              anchor = c(x= -122.417, y = 48.7)) +
  labs(x = "", y = "") +
  annotate("text", x = -122.535, y = 48.7, label = "n = 10", color = "red")
  

#plotting legend
bell.bi.leg <- bi_legend(pal = "DkBlue", dim = 3,
                         xlab = "Bham Score", ylab = "WDFW Score")

#Adding legend
bellbi <- ggdraw() +
  draw_plot(bell.bi.map, 0, 0, 1, 1) +
  draw_plot(bell.bi.leg, 0.7, 0.7, 0.3, 0.3)

#saving map
ggsave("FinalProjPlot1Opaque.pdf", plot = bellbi, device = "pdf", path = here("output"))

#-------------------CHEHALIS
#Chehalis has a max score of 125, multiply by 0.8 to normalize
chehalis.chloro <- merge.chehalis %>% 
  select(SiteId, PriorityIndexTotalQuantity, Score) %>% 
  mutate(wdfw_score = PriorityIndexTotalQuantity*1.22) %>% 
  mutate(chehalis_score = Score*0.8)

#creating chloro data
chehalis.bi.data <- bi_class(chehalis.chloro, x = chehalis_score, y = wdfw_score,
                         style = "quantile", dim = 3)

#locating a good basemap
#bounding box for basemap
st_bbox(chehalis.geom)
chehalis.bounds <- makebbox(w = -124.25404, e = -122.43226,
                        n = 47.54161 , s = 46.33193)

#downloading stamen map
basemap.chehalis <- get_stamenmap(bbox = as.vector(chehalis.bounds), zoom = 11, 
                         maptype = 'toner-lite') 

#checking how it looks
ggmap(basemap.chehalis) + theme_void()

#creating main plot
chehalis.bi.map <- ggmap(basemap.chehalis) + 
  geom_sf(data = chehalis.geom, fill = "transparent", color = "red", inherit.aes = F) +
  geom_sf(data = chehalis.bi.data,
          mapping = aes(color = bi_class), alpha = 0.8,
          size = .1, show.legend = F, inherit.aes = F) +
  bi_scale_color(pal = "DkBlue", dim = 3) +
  bi_theme() +
  coord_sf(datum = NA) +
  labs(x = "", y = "") +
  ggsn::scalebar(x.min = -124.25404, x.max = -122.43226,
                 y.min = 46.33193, y.max = 47.54161,
                 anchor = c(x = -122.55, y = 46.43),
                 st.size = 3, st.dist = 0.05, dist = 10,
                 dist_unit = "km", transform = T,
                 model = 'WGS84') +
  ggsn::north(x.min = -124.25404, x.max = -122.43226,
              y.min = 46.33193, y.max = 47.54161,
              scale = 0.1, symbol = 10, 
              anchor = c(x = -122.6, y = 46.58)) +
  annotate("text", x = -124, y = 46.4, label = "n = 237", color = "red")

#plotting legend
chehalis.bi.leg <- bi_legend(pal = "DkBlue", dim = 3,
                         xlab = "Chehalis Score", ylab = "WDFW Score")

#Adding legend
chehalisbi <- ggdraw() +
  draw_plot(chehalis.bi.map, 0, 0, 1, 1) +
  draw_plot(chehalis.bi.leg, 0.65, 0.65, 0.3, 0.3)

#saving map
ggsave("FinalProjPlot2Opaque.pdf", plot = chehalisbi, device = "pdf", path = here("output"))


#-------------------CWCC
#CWCC has a max score of 100, don't need to normalize
#convert to numeric
merge.cwcc$`Total Prioritization Score` <- as.numeric(merge.cwcc$`Total Prioritization Score`)

cwcc.chloro <- merge.cwcc %>% 
  select(SiteId, PriorityIndexTotalQuantity, `Total Prioritization Score`) %>% 
  mutate(wdfw_score = PriorityIndexTotalQuantity*1.22) %>% 
  mutate(cwcc_score = `Total Prioritization Score`*1)

#creating chloro data
cwcc.bi.data <- bi_class(cwcc.chloro, x = cwcc_score, y = wdfw_score,
                             style = "quantile", dim = 3)

#locating a good basemap
#bounding box for basemap
st_bbox(cwcc.geom)
cwcc.bounds <- makebbox(w = -124.86084, e = -123.31674,
                            n = 48.38590, s = 47.01668)

#downloading stamen map
basemap.cwcc <- get_stamenmap(bbox = as.vector(cwcc.bounds), zoom = 10, 
                                  maptype = 'toner-lite') 

#checking how it looks
ggmap(basemap.cwcc) + theme_void()

#creating main plot
cwcc.bi.map <- ggmap(basemap.cwcc) + 
  geom_sf(data = cwcc.geom, fill = "transparent", color = "red", inherit.aes = F) +
  geom_sf(data = cwcc.bi.data, 
          mapping = aes(color = bi_class), alpha = 0.8,
          size = .1, show.legend = F, inherit.aes = F) +
  bi_scale_color(pal = "DkBlue", dim = 3) +
  bi_theme() +
  coord_sf(datum = NA) +
  labs(x = "", y = "") +
  ggsn::scalebar(x.min = -124.86084, x.max = -123.31674,
                 y.min = 47.01668, y.max = 48.38590,
                 anchor = c(x = -123.5, y = 47.14),
                 st.size = 3, st.dist = 0.05, dist = 10,
                 dist_unit = "km", transform = T,
                 model = 'WGS84') +
  ggsn::north(x.min = -124.86084, x.max = -123.31674,
              y.min = 47.01668, y.max = 48.38590,
              scale = 0.1, symbol = 10, 
              anchor = c(x = -123.55, y = 47.3)) +
  annotate("text", x = -124.5, y = 47.1, label = "n = 126", color = "red")

#plotting legend
cwcc.bi.leg <- bi_legend(pal = "DkBlue", dim = 3,
                             xlab = "CWCC Score", ylab = "WDFW Score")

#Adding legend
cwccbi <- ggdraw() +
  draw_plot(cwcc.bi.map, 0, 0, 1, 1) +
  draw_plot(cwcc.bi.leg, 0.55, 0.65, 0.3, 0.3)

#saving map
ggsave("FinalProjPlot3Opaque.pdf", plot = cwccbi, device = "pdf", path = here("output"))

#-------------------KING
#King has a max score of 100, don't need to normalize
#convert to numeric
merge.cwcc$`Total Prioritization Score` <- as.numeric(merge.cwcc$`Total Prioritization Score`)

king.chloro <- merge.king %>% 
  select(SiteId, PriorityIndexTotalQuantity, OverallScore) %>% 
  mutate(wdfw_score = PriorityIndexTotalQuantity*1.22) %>% 
  mutate(king_score = OverallScore)

#creating chloro data
king.bi.data <- bi_class(king.chloro, x = king_score, y = wdfw_score,
                         style = "quantile", dim = 3)

#locating a good basemap
#bounding box for basemap
st_bbox(king.geom)
king.bounds <- makebbox(w = -122.52813, e = -121.06601,
                        n = 47.78058 , s = 47.08538)

#downloading stamen map
basemap.king <- get_stamenmap(bbox = as.vector(king.bounds), zoom = 10, 
                              maptype = 'toner-lite') 

#checking how it looks
ggmap(basemap.king) + theme_void()

#creating main plot
king.bi.map <- ggmap(basemap.king) + 
  geom_sf(data = king.geom, fill = "transparent", color = "red", inherit.aes = F) +
  geom_sf(data = king.bi.data, pch = 21,
          mapping = aes(fill = bi_class), alpha = 0.8,
          size = 4, show.legend = F, inherit.aes = F) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  bi_theme() +
  coord_sf(datum = NA) +
  labs(x = "", y = "") +
  ggsn::scalebar(x.min = -122.52813, x.max = -121.06601,
                 y.min = 47.08538, y.max = 47.78058,
                 anchor = c(x = -121.2, y = 47.2),
                 st.size = 3, st.dist = 0.05, dist = 10,
                 dist_unit = "km", transform = T,
                 model = 'WGS84') +
  ggsn::north(x.min = -122.52813, x.max = -121.06601,
              y.min = 47.08538, y.max = 47.78058,
              scale = 0.1, symbol = 10, 
              anchor = c(x = -121.25, y = 47.29)) +
  annotate("text", x = -122.4, y = 47.14, label = "n = 25", color = "red")

#plotting legend
king.bi.leg <- bi_legend(pal = "DkBlue", dim = 3, 
                         xlab = "King Score", ylab = "WDFW Score")

#Adding legend
kingbi <- ggdraw() +
  draw_plot(king.bi.map, 0, 0, 1, 1) +
  draw_plot(king.bi.leg, 0.67, 0.57, 0.3, 0.3)

#saving map
ggsave("FinalProjPlot4Opaque.pdf", plot = kingbi, device = "pdf", path = here("output"))


##############################
###############################
#BONUS SECTION
#Making these maps was quite fun, and it's hard to find the right place to stop. 
#The biggest limitation that I see with these maps are that the dots are very close together and overlap
#I tried to figure out how to jitter the points, one other option is to use leaflet so you can zoom in
#I think future direction would be to calculate statistics to better understand why some barriers
#are scored higher by WDFW but lower by the local jurisdiction and vice versa.

#here are a couple of point plots to see if it looks like there is a relationship
ggplot(data = king.bi.data, aes(x=king_score, y = wdfw_score)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(data = cwcc.bi.data, aes(x=cwcc_score, y = wdfw_score)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(data = chehalis.bi.data, aes(x=chehalis_score, y = wdfw_score)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(data = bham.bi.data, aes(x=bell_score, y = wdfw_score)) +
  geom_point() +
  geom_smooth(method = "lm")

