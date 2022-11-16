# TITLE: WSDOT PDF Table Extraction
# AUTHOR: C. Lewis-Smith
# DATE: Nov. 2021

# Prepare environment ----
rm(list = ls())
# Load libraries
library(tidyverse)
library(here)
library("tabulizer")
library(tm)
library(pdftools)
library("stringr")

# Extract data from WSDOT PDF

var_lines <- c("WSDOT Region", "Site Id", "Road", "MP", 
               "Stream", "WRIA", "Year Fixed", "Potential Lineal Gain (mi)") # create your variable name

# Page 1
pdf <- pdf_text(here("data", "WSDOT_projects", "page1.pdf")) %>% 
  readr::read_lines()

pdf <- data.frame(pdf) 

PDF.1 <-pdf[6:63, ] %>% as.data.frame()
PDF.1 <- PDF.1[-c(1,35,51), ] %>% as.data.frame()

PDF.1$. <- gsub("  +", "_", PDF.1$.)

colnames(PDF.1) <- "combine.data" 

wsdot.page1 <- PDF.1 %>% 
  separate(combine.data, var_lines, "_")

# Page 2
pdf2 <- pdf_text(here("data","WSDOT_projects", "page2.pdf")) %>% 
  readr::read_lines()

pdf <- data.frame(pdf2) 

PDF.1 <-pdf[6:63, ] %>% as.data.frame()
PDF.1 <- PDF.1[-c(1,14), ] %>% as.data.frame()

PDF.1$. <- gsub("  +", "_", PDF.1$.)

colnames(PDF.1) <- "combine.data" 

# Replace first 3 chracters with empty string ""
PDF.1$combine.data <- gsub("^.{0,1}", "", PDF.1$combine.data)

wsdot.page2 <- PDF.1 %>% 
  separate(combine.data, var_lines, "_")


# Page 3
pdf3 <- pdf_text(here("data","WSDOT_projects", "page3.pdf")) %>% 
  readr::read_lines()

pdf <- data.frame(pdf3) 

PDF.1 <-pdf[6:63, ] %>% as.data.frame()
PDF.1 <- PDF.1[-c(1), ] %>% as.data.frame()

PDF.1$. <- gsub("  +", "_", PDF.1$.)

colnames(PDF.1) <- "combine.data" 

wsdot.page3 <- PDF.1 %>% 
  separate(combine.data, var_lines, "_")

# Page 4
pdf4 <- pdf_text(here("data","WSDOT_projects", "page4.pdf")) %>% 
  readr::read_lines()

pdf <- data.frame(pdf4) 

PDF.1 <-pdf[6:63, ] %>% as.data.frame()

PDF.1 <- PDF.1[-c(1, 12), ] %>% as.data.frame()

PDF.1[18,] <- "  OL        15.0285 H 0.50   SR 305   12.34    SF Dogfish Creek                             15         2006    0.99"

PDF.1[20,] <- "  SW        30.0068 0.40    SR 142   20.2     Bowman Creek                                 30.0068    2006    22.79"

PDF.1[26,] <- "  NW        08.0059 7.00    I-405    29.75    Swamp Creek                                  08.0059    2007    0.39"  

PDF.1[27,] <- "  NW        07.0148 1.30    SR 92    1.93     Catherine Creek                              07.0148    2007    4.55"

PDF.1$. <- gsub("  +", "_", PDF.1$.)

colnames(PDF.1) <- "combine.data" 

# Replace first 3 chracters with empty string ""
PDF.1$combine.data <- gsub("^.{0,1}", "", PDF.1$combine.data)

wsdot.page4 <- PDF.1 %>% 
  separate(combine.data, var_lines, "_")

# Page 5
pdf5 <- pdf_text(here("data","WSDOT_projects", "page5.pdf")) %>% 
  readr::read_lines()

pdf <- data.frame(pdf5) 

PDF.1 <-pdf[6:63, ] %>% as.data.frame()

PDF.1 <- PDF.1[-c(1, 58), ] %>% as.data.frame()

PDF.1$. <- gsub("  +", "_", PDF.1$.)

colnames(PDF.1) <- "combine.data" 

wsdot.page5 <- PDF.1 %>% 
  separate(combine.data, var_lines, "_")

# Page 6
pdf6 <- pdf_text(here("data","WSDOT_projects", "page6.pdf")) %>% 
  readr::read_lines()

pdf <- data.frame(pdf6) 

PDF.1 <-pdf[6:63, ] %>% as.data.frame()

PDF.1 <- PDF.1[-c(1, 5, 58), ] %>% as.data.frame()

PDF.1$. <- gsub("  +", "_", PDF.1$.)

colnames(PDF.1) <- "combine.data" 

# Replace first 3 chracters with empty string ""
PDF.1$combine.data <- gsub("^.{0,1}", "", PDF.1$combine.data)

wsdot.page6 <- PDF.1 %>% 
  separate(combine.data, var_lines, "_")

# Page 7
pdf7 <- pdf_text(here("data","WSDOT_projects", "page7.pdf")) %>% 
  readr::read_lines()

pdf <- data.frame(pdf7) 

PDF.1 <-pdf[7:38, ] %>% as.data.frame()

PDF.1 <- PDF.1[-c(32), ] %>% as.data.frame()

PDF.1$. <- gsub("  +", "_", PDF.1$.)

colnames(PDF.1) <- "combine.data" 

wsdot.page7 <- PDF.1 %>% 
  separate(combine.data, var_lines, "_")

# Combine pages for WSDOT projects dataset
wsdot_projects <- rbind(wsdot.page1, wsdot.page2, wsdot.page3,
                        wsdot.page4, wsdot.page5, wsdot.page6,
                        wsdot.page7)

wsdot_projects$`Potential Lineal Gain (mi)` <- as.numeric(wsdot_projects$`Potential Lineal Gain (mi)`)

wsdot_projects$`Year Fixed` <- as.numeric(wsdot_projects$`Year Fixed`)

write_csv(wsdot_projects, here("output", "wsdot_projects.csv"))
