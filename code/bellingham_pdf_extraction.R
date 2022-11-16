# TITLE: bellingham_pdf_extraction
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

# Extract data from Bellingham 
var_lines <- c("RANK", "Site ID", "Stream", "Road Crossing", 
               "Total PI (WDFW Form)", "Lineal Gain (m)", "Passability (%)", "ESA*", "SCORE")

pdf <- pdf_text(here("data", "2019_bellingham_ranking", "2019-bellingham-p1.pdf")) %>% 
  readr::read_lines()

pdf <- data.frame(pdf)

PDF.1 <-pdf[c(7, 10, 12, 15, 18, 21,
              24, 27,30, 33, 36, 39, 42,
              44, 47, 50, 53, 56), ] %>% as.data.frame()

# Replace first 3 chracters with empty string ""
PDF.1$. <- gsub("^.{0,3}", "", PDF.1$.)

PDF.1$. <- gsub("  +", "_", PDF.1$.)

colnames(PDF.1) <- "combine.data" 

bellingham.page1 <- PDF.1 %>% 
  separate(combine.data, var_lines, "_") 

bellingham.page1$RANK <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)

# page 2 
pdf <- pdf_text(here("data", "2019_bellingham_ranking", "2019-bellingham-p2.pdf")) %>% 
  readr::read_lines()

pdf <- data.frame(pdf)

PDF.1 <-pdf[c(1, 3, 6, 9, 12, 15, 17, 20, 
              22, 25, 28, 31, 33, 36), ] %>% as.data.frame()

PDF.1$. <- gsub("  +", "_", PDF.1$.)

colnames(PDF.1) <- "combine.data" 

bellingham.page2 <- PDF.1 %>% 
  separate(combine.data, var_lines, "_") 

bellingham.page2$SCORE <- ifelse(is.na(bellingham.page2$SCORE), bellingham.page2$`ESA*`, bellingham.page2$SCORE)

# replace all numbers with empty string
bellingham.page2$`ESA*` <- gsub("[0-9]+", "", bellingham.page2$`ESA*`)

bellingham.page2$SCORE[is.na(bellingham.page2$SCORE)] <- 7.0

bellingham_scores <- rbind(bellingham.page1, bellingham.page2)

write_csv(bellingham_scores, here("output", "bellingham_scores.csv"))



