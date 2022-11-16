# Libraries
library(here)
library(tidyverse)
library(sf)
library(nhdplusTools)

# Goal: ID boundaries used in Skagit River Coop
# http://skagitcoop.org/wp-content/uploads/Skagit-Basin-Barrier-Culvert-Analysis-Report-and-Appendices.pdf

## Load NHDPlus HR ----
hucs <- c(1710, 1711)

if(!file.exists(here("data", "NHDPlusHR", "17"))){
  
  # Try getting the HR version
  message("NHDPlus HR missing. Now attempting download.")
  
  download_nhdplushr(
    nhd_dir = here("data", "NHDPlusHR"),
    hu_list = hucs # Can download additional HU at 2 or 4 level but these are really big detailed files!
  )
  
} else {
  message("NHDPlus HR installed. Loading existing data.")
}



# Load NHDPlus HR data
hr_data <- 
  get_nhdplushr(
    here("data", "NHDPlusHR/"),
    # Include core flowline data, catchment data, and WDB HUC boundaries
    layers = c("WBDHU6", "WBDHU8", "WBDHU10", "WDBHUC12")
  )

ggplot(
  hr_data$WBDHU8 %>% 
    filter(Name %in% c("Upper Skagit", "Lower Skagit", "Sauk", "Strait of Georgia"))
) + 
  geom_sf() + 
  geom_sf(
    color = "red",
    data = hr_data$WBDHU10 %>% filter(str_starts(as.character(HUC10), "17110002"))
  ) + 
  geom_sf_text(
    aes(label = Name),
    data = hr_data$WBDHU10 %>% filter(str_starts(as.character(HUC10), "17110002"))
  )

ggplot(
  hr_data$WBDHU8 %>% 
    filter(Name %in% c("Upper Skagit", "Lower Skagit", "Sauk"))
) + 
  geom_sf() + 
  geom_sf(
    color = "red",
    data = hr_data$WBDHU10 %>% filter(Name %in% c("Samish River", "Telegraph Slough-Frontal Padilla Bay"))
  )


