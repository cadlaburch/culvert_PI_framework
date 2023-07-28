#load libraries
library(readxl)
library(here)
library(tidyverse)
library(shades)
library(patchwork)
library(ragg)

#-------------------------
#Panel A & B
#load data
Variable <- read_excel(here("output/Compare_Contrast Data/Variable Comparisons.xlsx"))

#Panel A
AllPIVar <- Variable %>% 
  ggplot(aes(y=reorder(J2, desc(percentshared)), x=percentshared))+
  geom_bar(stat = "identity", na.rm = T)+
  theme_classic()+
  theme(axis.ticks = element_blank(),
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 1))+
  scale_y_discrete(expand = c(0,0),
                   na.translate = F)+
  scale_x_continuous(expand = c(0,0),
                     limits = c(0, 101))+
  labs(x="% Shared Metrics", y=element_blank(), title = "A.")
AllPIVar

#Panel B
AllCat <- Variable %>% 
  ggplot(aes(y=reorder(category_2, desc(percent_shared)), x=percent_shared))+
  geom_bar(stat = "identity", na.rm = T)+
  theme_classic()+
  theme(axis.ticks = element_blank(),
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 1))+
  scale_y_discrete(expand = c(0,0),
                   na.translate = F)+
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,101))+
  labs(x="% Shared Metrics", y=element_blank(), title = "B.")

AllCat

#-------------------------
#Panel C
#load data
Weight <- read_excel(here("output/Compare_Contrast Data/Linear Scoring Weight Analysis.xlsx"))

#create custom color palette for plot
pal <- c("#AA4499",
         "#CC6677",
         "#DDCC77",
         "#88CCEE",
         "#44AA99",
         "#117733",
         "#332288")

#Plot
WPFlipped <- Weight %>% 
  filter(Jurisdiction!= "CWCCReport") %>% 
  ggplot(aes(fill=BroadCat, y=Jurisdiction, x=Weight*100))+
  geom_bar(position="stack", stat="identity")+
  theme_classic()+
  theme(axis.ticks = element_blank(),
        plot.title = element_text(face = "bold", hjust = 1))+
  scale_fill_manual(values=pal)+
  scale_y_discrete(expand = c(0,0),
                   limits=c("Chehalis", "CWCC", "King", "Bellingham", "Thurston"))+
  scale_x_continuous(expand = c(0,0))+
  labs(x="% Scoring Weight", y=element_blank(), fill=element_blank(), title = "C.")

#---------------
#Combine and Export Plot
Figure3 <- (AllPIVar + AllCat + WPFlipped)
Figure3

ggsave("Figure3.tiff", plot = Figure3, path = here("output/Figures"),
       device = agg_tiff, width = 10, height = 3)

