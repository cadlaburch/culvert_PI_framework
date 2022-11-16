# TITLE: Website mining code
# AUTHOR: C. Lewis-Smith
# DATE: Dec. 2021

# Prepare environment ----
library(tidyverse)
library(rvest)
library(dplyr)
library(sf)
library(sp)
library(rgdal)
library(here)
library(pdftools)
library(remotes)

# This package is used to pull shp from arcgis online mapping tools 
#install_github("yonghah/esri2sf")
# Work with ESRI
library("esri2sf")

# Pull data from WDFW Chehalis Prioritization Tool ----

url <- "https://services.arcgis.com/rcya3vExsaVBGUDp/ArcGIS/rest/services/Prioritized_Chehalis_Barriers_May_2020/FeatureServer/0"
wdfw_chehalis_df <- esri2sf(url)

# Check file
plot(wdfw_chehalis_df)

# Write out as CSV file 
write_csv(wdfw_chehalis_df, here("output", "wdfw_chehalis_ranking.csv"))

check <- read_sf(here("output", "wdfw_chehalis_ranking.shp"))

url2 <- "https://services.arcgis.com/Ej0PsM5Aw677QF1W/arcgis/rest/services/FPPrioritizationWebMapPLibraryDataLayers/FeatureServer/85"
king_county_df <- esri2sf(url2)

print(unique(df2$FishPassageFeatureTypeCode))

df2.view = king_county_df %>% 
  filter(FishPassageFeatureTypeCode == "Culvert") 
  

plot(king_county_df)

write_csv(king_county_df, here("output", "king_county_prelim.csv"))


# Scarping from the JS
# Note, coastal salmon csv can be downloaded from their map tool directly. 
# Below is code to help start scrapping using java 

coastal_salmon_page <- "https://coastsalmon.maps.arcgis.com/apps/webappviewer/index.html?id=07f560d3820f43fca1970e5446e76be3"
coastal_salmon <- read_html(coastal_salmon_page)

body_nodes <- coastal_salmon %>% 
  html_node("body") %>% 
  html_children()
body_nodes

body_nodes %>% 
  html_children() %>% 
  html_children()

priority <- coastal_salmon %>% 
  rvest::html_nodes('body') %>% 
  xml2::xml_find_all("//td[contains(@class, 'attrValue')]") %>% 
  rvest::html_text()


