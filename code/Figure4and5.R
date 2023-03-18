#load libraries
library(tidyverse)
library(here)
library(RColorBrewer)
library(ragg)

#----------------------------------
#Figure 4
#creating dummy models where M and D are held constant
HighCost <- data.frame(B=1, P=3, H=1:5000, Scenario = "Low Cost", M=2, D=1, C=3) %>% 
  mutate(PI = (B*P*H*M*D*C)^(1/4))
MedCost <- data.frame(B=1, P=3, H=1:5000, Scenario = "Medium Cost", M=2, D=1, C=2) %>% 
  mutate(PI = (B*P*H*M*D*C)^(1/4))
LowCost <- data.frame(B=1, P=3, H=1:5000, Scenario = "High Cost", M=2, D=1, C=1) %>% 
  mutate(PI = (B*P*H*M*D*C)^(1/4))

#merging the datasets
Cost = rbind(HighCost, MedCost, LowCost)

#plotting results
#Horizontal PI value (y=13) based on the average PI for the WDFW dataset 

Figure4 <- ggplot(data = Cost, aes(x = H, y = PI, color = Scenario))+
  geom_line()+
  theme_classic()+
  geom_hline(yintercept = 13)+
  geom_vline(xintercept = c(1586, 2380, 4760), linetype = "dashed")+
  geom_segment(aes(x = 0, y = 0, xend = 4760, yend = 0), color = "#e41a1c")+
  annotate(geom = "text", x = 4500, y = 1, label = "1", color = "#e41a1c")+
  geom_segment(aes(x = 0, y = 1, xend = 2380, yend = 1), color = "#377eb8")+
  annotate(geom = "text", x = 2100, y = 2, label = "1/2", color = "#377eb8")+
  geom_segment(aes(x = 0, y = 2, xend = 1586, yend = 2), color = "#4daf4a")+
  annotate(geom = "text", x = 1300, y = 3, label = "1/3", color = "#4daf4a")+
  scale_y_continuous(breaks = c(3, 9, 13, 17))+
  scale_x_continuous(breaks = c(1586, 2380, 4760),
                     expand = c(0,0))+
  labs(x = expression(Upstream~lineal~habitat~gain~m^2), y = "PI Score") +
  scale_color_manual(breaks = c("Low Cost", "Medium Cost", "High Cost"), 
                     values = c("#4daf4a", "#377eb8", "#e41a1c")) 

Figure4

#Export
ggsave(dpi = 300, units = "in", here("output/Figures","Figure4.tiff"), plot = Figure4, 
       device = agg_tiff, width = 5, height = 4)

#----------------------------------
#Figure 5
#Create dummy dataframes for each species and each run type
#ESA listing status in case area was referenced from salmonscape
#Chinook
CoastChinook<- data.frame(B=1, P=0.016, H=1:5000, Species = "Chinook Salmon", ESA = "Not listed",Run = "WA Coast", Scenario = "Real", M=2, D=1, C=3) %>% 
  mutate(PI = (B*P*H*M*D*C)^(1/4))
PugetChinook <- data.frame(B=1, P=0.016, H=1:5000, Species = "Chinook Salmon", ESA = "Listed", Run = "Puget Sound", Scenario = "Real", M=2, D=3, C=3) %>% 
  mutate(PI = (B*P*H*M*D*C)^(1/4))
ChinookS<- data.frame(B=1, P=0.016, H=1:5000, Species = "Chinook Salmon", ESA = "Species of concern",Run = "WA Coast", Scenario = "Scenario", M=2, D=2, C=3) %>% 
  mutate(PI = (B*P*H*M*D*C)^(1/4))

#Coho
OPCoho<- data.frame(B=1, P=0.05, H=1:5000, Species = "Coho Salmon", ESA = "Not listed", Run = "Olympic Peninsula/SW WA", Scenario = "Real", M=2, D=1, C=3) %>% 
  mutate(PI = (B*P*H*M*D*C)^(1/4))
PugetCoho <- data.frame(B=1, P=0.05, H=1:5000, Species = "Coho Salmon", ESA = "Species of concern", Run = "Puget Sound", Scenario = "Real", M=2, D=2, C=3) %>% 
  mutate(PI = (B*P*H*M*D*C)^(1/4))
CohoS<- data.frame(B=1, P=0.05, H=1:5000, Species = "Coho Salmon", ESA = "Listed", Run = "Olympic Peninsula/SW WA", Scenario = "Scenario", M=2, D=3, C=3) %>% 
  mutate(PI = (B*P*H*M*D*C)^(1/4))

#Sockeye
PlesantSockeye <- data.frame(B=1, P=3, H=1:5000, Species = "Sockeye Salmon", ESA = "Listed", Run = "Lake Plesant",  Scenario = "Real", M=2, D=3, C=3) %>% 
  mutate(PI = (B*P*H*M*D*C)^(1/4))
QuinaltSockeye <- data.frame(B=1, P=3, H=1:5000, Species = "Sockeye Salmon", ESA = "Not listed", Run = "Quinalt/Baker River", Scenario = "Real",  M=2, D=1, C=3) %>% 
  mutate(PI = (B*P*H*M*D*C)^(1/4))
SockeyeS <- data.frame(B=1, P=3, H=1:5000, Species = "Sockeye Salmon", ESA = "Species of concern", Run = "Quinalt/Baker River", Scenario = "Scenario",  M=2, D=2, C=3) %>% 
  mutate(PI = (B*P*H*M*D*C)^(1/4))

#Bull
Bull_Dolly <- data.frame(B=1, P=0.0007, H=1:5000, Species = "Bull Trout/Dolly Varden", ESA = "Not listed", Run = "NA", Scenario = "Real", M=1, D=1, C=3) %>% 
  mutate(PI = (B*P*H*M*D*C)^(1/4))
BullDollyS2 <- data.frame(B=1, P=0.0007, H=1:5000, Species = "Bull Trout/Dolly Varden", ESA = "Species of concern", Run = "NA", Scenario = "Scenario", M=1, D=2, C=3) %>% 
  mutate(PI = (B*P*H*M*D*C)^(1/4))
BullDollyS3 <- data.frame(B=1, P=0.0007, H=1:5000, Species = "Bull Trout/Dolly Varden", ESA = "Listed", Run = "NA", Scenario = "Scenario", M=1, D=3, C=3) %>% 
  mutate(PI = (B*P*H*M*D*C)^(1/4))

#Rainbow
Rainbow <- data.frame(B=1, P=0.04, H=1:5000, Species = "Rainbow Trout", ESA = "Not listed", Run = "NA", Scenario = "Real", M=1, D=1, C=3) %>% 
  mutate(PI = (B*P*H*M*D*C)^(1/4))
RainbowS2 <- data.frame(B=1, P=0.04, H=1:5000, Species = "Rainbow Trout", ESA = "Species of concern", Run = "NA", Scenario = "Scenario", M=1, D=2, C=3) %>% 
  mutate(PI = (B*P*H*M*D*C)^(1/4))
RainbowS3 <- data.frame(B=1, P=0.04, H=1:5000, Species = "Rainbow Trout", ESA = "Listed", Run = "NA", Scenario = "Scenario", M=1, D=3, C=3) %>% 
  mutate(PI = (B*P*H*M*D*C)^(1/4))

#Chum
HoodChum <- data.frame(B=1, P=1.25, H=1:5000, Species = "Chum Salmon", ESA = "Listed", Run = "Hood River", Scenario = "Real", M=2, D=3, C=3) %>% 
  mutate(PI = (B*P*H*M*D*C)^(1/4))
CoastChum <- data.frame(B=1, P=1.25, H=1:5000, Species = "Chum Salmon", ESA = "Not listed", Run = "Pacific Coast/Puget Sound", Scenario = "Real", M=2, D=1, C=3) %>% 
  mutate(PI = (B*P*H*M*D*C)^(1/4))
CoastChumS <- data.frame(B=1, P=1.25, H=1:5000, Species = "Chum Salmon", ESA = "Species of concern", Run = "Pacific Coast/Puget Sound", Scenario = "Scenario", M=2, D=2, C=3) %>% 
  mutate(PI = (B*P*H*M*D*C)^(1/4))

#Pink
OddPink <- data.frame(B=1, P=1.25, H=1:5000, Species = "Pink Salmon", ESA = "Not listed", Run = "Odd/Even", Scenario = "Real", M=2, D=1, C=3) %>% 
  mutate(PI = (B*P*H*M*D*C)^(1/4))
OddPinkS2 <- data.frame(B=1, P=1.25, H=1:5000, Species = "Pink Salmon", ESA = "Species of concern", Run = "Odd/Even", Scenario = "Scenario", M=2, D=2, C=3) %>% 
  mutate(PI = (B*P*H*M*D*C)^(1/4))
OddPinkS3 <- data.frame(B=1, P=1.25, H=1:5000, Species = "Pink Salmon", ESA = "Listed", Run = "Odd/Even", Scenario = "Scenario", M=2, D=3, C=3) %>% 
  mutate(PI = (B*P*H*M*D*C)^(1/4))

#Steelhead
OPSteelhead <- data.frame(B=1, P=0.0021, H=1:5000, Species = "Steelhead", ESA = "Not listed", Run = "Olympic Penninsula", Scenario = "Real", M=2, D=1, C=3) %>% 
  mutate(PI = (B*P*H*M*D*C)^(1/4))
PugetSteelhead <- data.frame(B=1, P=0.0021, H=1:5000, Species = "Steelhead", ESA = "Listed", Run = "Puget/SW WA", Scenario = "Real", M=2, D=3, C=3) %>% 
  mutate(PI = (B*P*H*M*D*C)^(1/4))
PugetSteelheadS <- data.frame(B=1, P=0.0021, H=1:5000, Species = "Steelhead", ESA = "Species of concern", Run = "Puget/SW WA", Scenario = "Scenario", M=2, D=2, C=3) %>% 
  mutate(PI = (B*P*H*M*D*C)^(1/4))

#Cutthroat
Cutthroat <- data.frame(B=1, P=0.037, H=1:5000, Species = "Cutthroat Trout", ESA = "Not listed", Run = "NA", Scenario = "Real", M=2, D=1, C=3) %>% 
  mutate(PI = (B*P*H*M*D*C)^(1/4))
CutthroatS2 <- data.frame(B=1, P=0.037, H=1:5000, Species = "Cutthroat Trout", ESA = "Species of concern", Run = "NA", Scenario = "Scenario", M=2, D=2, C=3) %>% 
  mutate(PI = (B*P*H*M*D*C)^(1/4))
CutthroatS3 <- data.frame(B=1, P=0.037, H=1:5000, Species = "Cutthroat Trout", ESA = "Listed", Run = "NA", Scenario = "Scenario", M=2, D=3, C=3) %>% 
  mutate(PI = (B*P*H*M*D*C)^(1/4))

#Join all the dataframes so that they can be plotted together
SpeciesJoin = rbind(Cutthroat, PugetSteelhead, OPSteelhead, OddPink,
                    CoastChum, HoodChum, Rainbow, Bull_Dolly, QuinaltSockeye, PlesantSockeye,
                    PugetCoho, OPCoho, CoastChinook, PugetChinook, ChinookS, CohoS, SockeyeS, BullDollyS3, BullDollyS2, RainbowS2, RainbowS3, CoastChumS, OddPinkS3, OddPinkS2, PugetSteelheadS, CutthroatS3, CutthroatS2)

#Plot Figure
Figure5 <- SpeciesJoin %>% 
  mutate(Species = fct_reorder(Species, desc(P))) %>% 
  ggplot(aes(x=H, y=PI, color=ESA))+
  geom_line(size=.8, aes(linetype = Scenario))+
  scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a"),
                     limits = c("Listed", "Species of concern", "Not listed"))+
  facet_wrap(~Species)+
  theme_classic()+
  theme(strip.background=element_blank())+
  scale_linetype_manual(values = c("solid", "dotted"),
                        guide = "none") +
  scale_x_continuous(expand = c(0,0),
                     breaks = c(1000, 3000, 5000))+
  scale_y_continuous(expand= c(0,0))+
  labs(x = "Habitat Quanity (m)", y = "PI Score")

Figure5

#Save Plot
ggsave(dpi = 300, units = "in", here("output/Figures","Figure5draft.tiff"), plot = Figure5, 
       device = agg_tiff, width = 5, height = 5)

#Final figure5 was made on illustrator



