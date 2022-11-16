#Author: Catalina Burch
#Date: June 6, 2022

#Load Libraries
library(tidyverse)
library(sf)
library(here)
library(readxl)
library(patchwork)
library(plotly)
library(moments)
library(janitor)
library(ragg)


#----------------------------------
#KINGCOUNTY
#Import Data for King
King <- read_csv(here("data", "Feb22King.csv"))
  #I downloaded the data for king from this website https://gis-kingcounty.opendata.arcgis.com/datasets/fish-passage-sites-fp-fishpassagesites-point/explore?location=47.481751%2C-121.810500%2C9.76&showTable=true

#How many culverts are in the King Survey Area
  #FishPassageFeatureTypeCode 1 (culvert)
King %>% 
  filter(FishPassageFeatureTypeCode==1)->
  KingCulverts
distinct(KingCulverts)

#Filter down for Barrier Culverts that received a PI Score
KingSub <- King %>% 
  dplyr::select(AssetID, FishPassageBarrierStatusCode, FishPassageFeatureTypeCode, OverallScore) %>%
  filter(FishPassageBarrierStatusCode==10, FishPassageFeatureTypeCode==1, OverallScore>=2)

distinct(KingSub) 

#Summary Stats, n=107 #NOTE IS THIS CORRECT????
mean(KingSub$OverallScore) # 23.1
median(KingSub$OverallScore) #18
range(KingSub$OverallScore) #2, 95
sd(KingSub$OverallScore) # 19.9
IQR(KingSub$OverallScore) #28
quantile(KingSub$OverallScore)
#0%  25%  50%  75% 100% 
#2    7   18   35   95 

#Test for normal distribution
shapiro.test(KingSub$OverallScore) #p<2.2e-16

#Test for skew
skewness(KingSub$OverallScore) #1.20454

P1 <- KingSub %>% #Plot of PI distribution
  ggplot(aes(x=OverallScore))+
  geom_rect(aes(xmin = 7, xmax = 35, ymin = 0, ymax = 60), fill = "pink1",alpha = 0.1)+
  geom_histogram(bins = 100)+
  theme_classic()+
  theme(plot.title = element_text(size = 11),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())+
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,100))+
  scale_y_continuous(expand= c(0,0),
                     breaks = c(20, 40, 60))+
  xlab(element_blank())+
  ylab(element_blank())+
  geom_vline(xintercept=23.1, color = "red")+
  geom_vline(xintercept = 18, linetype = "dashed", color = "red")+
  ggtitle("King")+
  annotate(geom = "text", y = 30, x = 80, label = "skew = 1.20", size=3.5) +
  annotate(geom = "text", y = 40, x = 80, label = "n = 107", size=3.5)


P1

#---------------------------------------
#WDFW PI
#The following chunck of code was created by Braedon and Connor in the wdfw_explore file. I need it in order to calculate summary statistics and plot distributions

sf_allculv_wdfw <- st_read(here("data/culv_inventories/WdfwFishPassage/WdfwFishPassage.gdb"), layer = "WDFW_FishPassageSite")
# Repair names
sf_allculv_wdfw <- sf_allculv_wdfw %>% clean_names()
# Case area borders
sf_case <- st_read(here("data/WSDOT_-_Fish_Passage_US_v._WA_Case_Area_Boundary-shp/WSDOT_-_Fish_Passage_US_v._WA_Case_Area_Boundary.shp"), quiet = TRUE)
# Must set CRS as well as save for spatial clipping
sf_case <- st_transform(sf_case, crs = 4326)
sf_allculv_wdfw <- st_transform(sf_allculv_wdfw, crs = 4326)

# Clip culvert dataset by case area
sf_case_culverts <- sf_allculv_wdfw[sf_case, ] 

sf_case_culverts = sf_case_culverts %>% 
  filter(grepl('Culvert', feature_type)) %>% 
  filter(!grepl('Non', feature_type)) ### Change to Contains culverts

# Decode coded section based on cross reference
sf_case_culverts = sf_case_culverts %>% 
  mutate(fish_passage_barrier_status_code = as.factor(case_when(
    fish_passage_barrier_status_code == 0 ~ "NA",
    fish_passage_barrier_status_code == 10 ~ "barrier",
    fish_passage_barrier_status_code == 20 ~ "non-barrier",
    fish_passage_barrier_status_code == 99 ~ "unknown"
  ))) %>% 
  mutate(percent_fish_passable_code = as.factor(case_when(
    percent_fish_passable_code == 0 ~ "NA",
    percent_fish_passable_code == 10 ~ "0",
    percent_fish_passable_code == 20 ~ "33",
    percent_fish_passable_code == 30 ~ "66",
    percent_fish_passable_code == 40 ~ "100",
    percent_fish_passable_code == 99 ~ "unkown"
  ))) %>% 
  mutate(owner_type_code = as.factor(case_when(
    owner_type_code == 1 ~ "city",
    owner_type_code == 2 ~ "county",
    owner_type_code == 3 ~ "federal",
    owner_type_code == 4 ~ "private",
    owner_type_code == 5 ~ "state",
    owner_type_code == 6 ~ "tribal",
    owner_type_code == 7 ~ "other",
    owner_type_code == 8 ~ "port",
    owner_type_code == 9 ~ "drainage district",
    owner_type_code == 11 ~ "irrigation district",
    owner_type_code == 12 ~ "unknown"
  ))) %>% 
  mutate(fish_use_code = as.factor(case_when(
    fish_use_code == 0 ~ "NA", #Think we should keep NAs?
    fish_use_code == 10 ~ "yes",
    fish_use_code == 20 ~ "no",
    fish_use_code == 99 ~ "unknown"
  ))) %>% 
  mutate(significant_reach_code = as.factor(case_when(
    significant_reach_code == 0 ~ "NA",
    significant_reach_code == 10 ~ "yes",
    significant_reach_code == 20 ~ "no",
    significant_reach_code == 99 ~ "unknown"
  )))

# Filter out non barrier culverts  
sf_case_culverts = sf_case_culverts %>% 
  filter(percent_fish_passable_code != "100") %>% 
  filter(fish_use_code != "no") %>% 
  filter(fish_passage_barrier_status_code != "non-barrier") 


#I loaded in the data and did a bunch of summary stats within the wdfw_explore file. This next bit of code will not work unless you run the code from that other file.
mean(sf_case_culverts$priority_index_total_quantity, na.rm=TRUE) #answer 13.1
median(sf_case_culverts$priority_index_total_quantity, na.rm=TRUE) #answer 11
range(sf_case_culverts$priority_index_total_quantity, na.rm=TRUE) #answer 0, 82
sd(sf_case_culverts$priority_index_total_quantity, na.rm=TRUE) #answer 8.6
shapiro.test(sf_case_culverts$priority_index_total_quantity) #p<2.2e-16 not normal
IQR(sf_case_culverts$priority_index_total_quantity, na.rm=TRUE) #IQR = 10.11
quantile(sf_case_culverts$priority_index_total_quantity, na.rm=TRUE)
#0%    25%    50%    75%   100% 
#0.000  7.165 11.140 17.275 82.030 
skewness(sf_case_culverts$priority_index_total_quantity, na.rm=TRUE) #1.63

WDFW <- sf_case_culverts %>% 
  ggplot(aes(x=priority_index_total_quantity, na.rm=TRUE))+
  geom_rect(aes(xmin = 7.17, xmax = 17.28, ymin = 0, ymax = 300), fill = "pink1",alpha = 0.1)+
  geom_histogram(bins = 100) +
  geom_vline(xintercept=13.1, color = "red")+
  geom_vline(xintercept = 11, linetype = "dashed", color = "red")+
  theme_classic()+
  theme(plot.title = element_text(size = 11),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())+
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,100))+
  scale_y_continuous(expand= c(0,0))+
  xlab(element_blank())+
  ylab(element_blank())+
  ggtitle("WDFW")+
  annotate(geom = "text", y = 150, x = 80, label = "n = 4259", size=3.5) +
  annotate(geom = "text", y = 115, x = 80, label = "skew = 1.63", size=3.5) 

WDFW


#-----------------------------------------
#CWCC PI
#Import Data
CWCC <- read_csv(here("Data/CWCCResults.csv"))

#Filter for Barrier Culverts with Score
CWCC %>%
  dplyr::select(`Unique ID`, `Barrier Type`, `Total Prioritization Score`) %>% 
  filter(`Barrier Type` == "Culvert")->
  CWCCFiltered

#Summary Stats, n = 697
mean(CWCCFiltered$`Total Prioritization Score`) #41.5
median(CWCCFiltered$`Total Prioritization Score`) #41.6
range(CWCCFiltered$`Total Prioritization Score`) #14.0, 83.3
sd(CWCCFiltered$`Total Prioritization Score`) #10.33
IQR(CWCCFiltered$`Total Prioritization Score`) #IQR:14.775
quantile(CWCCFiltered$`Total Prioritization Score`)
#0%    25%    50%    75%   100% 
#14.025 33.900 41.625 48.675 83.275 
skewness(CWCCFiltered$`Total Prioritization Score`) #0.195

#Test for normal distribution
shapiro.test(CWCCFiltered$`Total Prioritization Score`) #p=0.003 Not Normal

#Plot Histogram
P2 <- CWCCFiltered %>% 
  ggplot(aes(x=`Total Prioritization Score`))+
  geom_rect(aes(xmin = 33.9, xmax = 48.68, ymin = 0, ymax = 35), fill = "pink1",alpha = 0.1)+
  geom_histogram(bins = 100)+
  geom_vline(xintercept=41.5, color = "red")+
  geom_vline(xintercept = 41.6, linetype = "dashed", color = "red")+
  theme_classic()+
  theme(plot.title = element_text(size = 11),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())+
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,100))+
  scale_y_continuous(expand= c(0,0),
                     breaks = c(10, 20, 30))+
  xlab(element_blank())+
  ylab(element_blank())+
  ggtitle("CWCC")+
  annotate(geom = "text", y = 20, x = 80, label = "n = 697", size=3.5) +
  annotate(geom = "text", y = 14, x = 80, label = "skew = 0.195", size=3.5) 

P2

#------------------------------------
#BELLINGHAM PI

#Import Data
bham <- read_csv(here("data/2019_bellingham_ranking/bellingham_scores.csv"))

#Summary Stats, n = 32
mean(bham$SCORE) #10.5
median(bham$SCORE) #11
range(bham$SCORE) #5, 15.7
sd(bham$SCORE) #2.96
IQR(bham$SCORE) #5.225
quantile(bham$SCORE)
#0%    25%    50%    75%   100% 
#5.000  8.000 11.000 13.225 15.700 
skewness(bham$SCORE) #-0.079

#Test for normal distribution
shapiro.test(bham$SCORE) #p<0.19 Normal

#Plot Histogram
#multiplied by 3.125 to normalize score to 100
#ERROR: DOUBLE CHECK I THINK THAT THE MULTIPLIER SHOULD BE 3.22 (Bellingham report says 31 is maximum possible score)
P3 <-bham %>% 
  ggplot(aes(x=SCORE*3.125))+
  geom_rect(aes(xmin = 8*3.125, xmax = 13.23*3.125, ymin = 0, ymax = 4), fill = "pink1",alpha = 0.1)+
  geom_histogram(bins = 100)+
  theme_classic()+
  theme(plot.title = element_text(size = 11),
        axis.title = element_text(size=10),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())+
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,100),
                     breaks = c(0, 25, 50, 75, 100))+
  scale_y_continuous(expand= c(0,0),
                     breaks = c(1, 2, 3, 4))+
  xlab(element_blank())+
  ylab(element_blank())+
  geom_vline(xintercept=10.5*3.125, color = "red")+
  geom_vline(xintercept = 11*3.125, linetype = "dashed", color = "red")+
  ggtitle("Bellingham")+
  annotate(geom = "text", y = 2.3, x = 80, label = "n = 32", size=3.5)+
  xlab("Barrier Culvert PI Scores")+
  annotate(geom = "text", y = 1.7, x = 80, label = "skew = -0.079", size=3.5) 

P3

#-------------------------------------
#CHEHALIS PI

#Import Data
chehalis <- read_csv(here("data/Chehalis/wdfw_chehalis_ranking.csv"))

#Subset for barrier culverts with scores
chehalis %>% 
  filter(FeatureType == "Culvert")->
  chehalisFiltered

#Summary Stats, n = 1946
mean(chehalisFiltered$Score) #46.9
median(chehalisFiltered$Score) #46.3
range(chehalisFiltered$Score) #25.3, 87.7
sd(chehalisFiltered$Score) #8.77
IQR(chehalisFiltered$Score) #IQR = 11.33
quantile(chehalisFiltered$Score)
#0%      25%      50%      75%     100% 
#25.33333 40.66667 46.33333 52.00000 87.66667
skewness(chehalisFiltered$Score) #0.708

#Test for normal distribution
shapiro.test(chehalisFiltered$Score) #p<2.2e-16

#Plot Histogram
#Plot Histogram
#I multiplied by 0.8 to normalize to 100
P4 <- chehalisFiltered%>% 
  ggplot(aes(x=Score*0.8))+
  geom_rect(aes(xmin = 40.67*0.8, xmax = 52*0.8, ymin = 0, ymax = 150), fill = "pink1",alpha = 0.1)+
  geom_histogram(bins=100)+
  theme_classic()+
  theme(plot.title = element_text(size = 11),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())+
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,100))+
  scale_y_continuous(expand= c(0,0),
                     breaks = c(50, 100, 150))+
  xlab(element_blank())+
  ylab(element_blank())+
  geom_vline(xintercept=46.9*0.8, color = "red")+
  geom_vline(xintercept = 46.3*0.8, linetype = "dashed", color = "red")+
  ggtitle("Chehalis")+
  annotate(geom = "text", y = 100, x = 80, label = "n = 1946", size=3.5)+
  annotate(geom = "text", y = 75, x = 80, label = "skew = 0.708", size=3.5) 

P4

#-----------------------------------------------
#THURSTON PI

#Import Data
Thurston <- read_excel(here("data/Thurston/Fixed Thurston County FP Potential Project List.xlsx"))

#Filtering out non-barriers
Thurston %>%
  filter(TotalScore > 0)->
  ThurstonF

#Summary Stats, n = 297
mean(ThurstonF$TotalScore) #20.6
median(ThurstonF$TotalScore) #12
range(ThurstonF$TotalScore) #1, 86
sd(ThurstonF$TotalScore) #18.9
IQR(ThurstonF$TotalScore)
quantile(ThurstonF$TotalScore)
#0%  25%  50%  75% 100% 
#1    5   12   34   86 
skewness(ThurstonF$TotalScore) #1.02

#Test for normal distribution
shapiro.test(ThurstonF$TotalScore) #p<3.4e-16

#Plot Hist
#Multiplied by 1.05 to normalize to 100
P5 <- ThurstonF%>% 
  ggplot(aes(x=TotalScore*1.05))+
  geom_rect(aes(xmin = 5*1.05, xmax = 34*1.05, ymin = 0, ymax = 50), fill = "pink1",alpha = 0.1)+
  geom_histogram(bins=100)+
  theme_classic()+
  theme(plot.title = element_text(size = 11),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())+
  scale_x_continuous(expand = c(0,0),
                     limits = c(0, 100))+
  scale_y_continuous(expand= c(0,0),
                     breaks = c(15, 30, 45))+
  xlab(element_blank())+
  ylab(element_blank())+
  geom_vline(xintercept=20.6*1.05, color = "red")+
  geom_vline(xintercept = 12*1.05, linetype = "dashed", color = "red")+
  ggtitle("Thurston")+
  annotate(geom = "text", y = 30, x = 80, label = "n = 297", size=3.5) +
  annotate(geom = "text", y = 20, x = 80, label = "skew = 1.02", size=3.5) 

P5

#Cool now I want to combine all of the plots

histogram <- WDFW/P4/P2/P5/P1/P3

ggsave("histogram.tiff", path = here("output/DistributionFigures"), plot=histogram, device=agg_tiff,
       height = 8.5, width = 4, dpi = 300)


#---------------------------------------------
#Sunny and I talked about looking at different portfolios of species compositions at barriers in the WDFW dataset

#filtering to only include culverts that recieved PI score
WDFW_Filtered <- sf_case_culverts %>% 
  filter(priority_index_total_quantity != "NA")

#Calculating the number of each unique fish portfolio and the median PI scores for each portfolio
Portfolios_WDFW <- WDFW_Filtered %>% 
  group_by(potential_species) %>% 
  summarise(median = median(priority_index_total_quantity),
            n= (n()/4259)*100)


#Interactive plot showing the most common portfolios of fish
WDFW_Filtered %>% 
  ggplot(aes(x=potential_species))+
  geom_histogram(stat = "count")+
  theme_classic() +
  scale_y_discrete(expand = c(0,0))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) ->
  Experiment
ggplotly(Experiment)

#filtering for portfolios that contain certain species

PSock <- WDFW_Filtered %>% 
  filter(grepl("Sockeye", potential_species)) %>% #n = 294/4259 = 6.9%
  summarise(median = median(priority_index_total_quantity)) #median = 19.205

PChum <- WDFW_Filtered %>% 
  filter(grepl("Chum", potential_species)) %>% #n = 903/4259 = 21.2%
  summarise(median = median(priority_index_total_quantity)) #median = 18.45

PPink <- WDFW_Filtered %>% 
  filter(grepl("Pink", potential_species)) %>% #n = 130/4259 = 3.05%
  summarise(median = median(priority_index_total_quantity)) #median = 21.96

PCoho <- WDFW_Filtered %>% 
  filter(grepl("Coho", potential_species)) %>% #n = 3725/4259 = 87.46%
  summarise(median = median(priority_index_total_quantity)) #median = 12.6

PResident <- WDFW_Filtered %>% 
  filter(grepl("Resident Trout", potential_species)) %>% #n = 4229/4259 = 99.3%
  summarise(median = median(priority_index_total_quantity)) #median = 11.14

PCut <- WDFW_Filtered %>% 
  filter(grepl("SR Cutthroat", potential_species)) %>% #n = 3834/4259 = 90.02
  summarise(median = median(priority_index_total_quantity)) #median = 11.68

PChinook <- WDFW_Filtered %>% 
  filter(grepl("Chinook", potential_species)) %>% #n = 364/4259 = 8.55
  summarise(median = median(priority_index_total_quantity)) #median = 19.68

PSteelhead <- WDFW_Filtered %>% 
  filter(grepl("Steelhead", potential_species)) %>% #n = 3641/4259 = 85.49%
  summarise(median = median(priority_index_total_quantity)) #median = 11.96

PBull <- WDFW_Filtered %>% 
  filter(grepl("Bull Trout", potential_species)) %>% #n = 712/4259 = 16.72%
  summarise(median = median(priority_index_total_quantity)) #median = 10.73
            