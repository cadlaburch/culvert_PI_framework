#This file was a left over from Connor's work. I (Catalina) can't get it to run because I don't
#have the excel file below.

library(here)
library(tidyverse)
library(readxl)
library(ggridges)

dat <- read_excel(here("data/PrioritizationPilotBearCherrySoosKCBarriers_20210618.xlsx"))

#Histograms of scores

dat.l <- dat %>% 
  select(OBJECTID, `Option A (formula)`, `Option B (formula)`, 
         `Option C (formula)`, `Original Score (formula)`) %>% 
  pivot_longer(!OBJECTID, names_to = "option", values_to = "score")

ggplot(dat.l, aes(x = score, y = option, fill = option)) + 
  geom_density_ridges(scale = 3, alpha = 0.25) +
  labs(y = "", fill = "") +
  theme_classic() +
  theme(legend.position = "none")


#Attributes by quantile for options A-C

dat <- dat %>% mutate(quantA = ntile(dat$`Option A (formula)`, 4),
                      quantB = ntile(dat$`Option B (formula)`, 4),
                      quantC = ntile(dat$`Option C (formula)`, 4),
                      quant = factor(paste(quantA, quantB, quantC, sep = ", ")))

knitr::kable(dat %>% 
  group_by(factor(quant)) %>% 
  summarize(count = n()), "rst")


ggplot(data = dat, aes(x = factor(quant), 
                       y = `Habitat Quantity Distance`)) +
  labs(x = "quantile A, B, C") +
  geom_boxplot(fill = "grey") +
  theme_classic()

ggplot(data = dat, aes(x = factor(quant), 
                       y = `Count DS Barriers (All)`)) +
  labs(x = "quantile A, B, C") +
  geom_boxplot(fill = "grey") +
  theme_classic()

ggplot(data = dat, aes(x = factor(quant), 
                       y = `Percent Forest Value`)) +
  labs(x = "quantile A, B, C") +
  geom_boxplot(fill = "grey") +
  theme_classic()


#observations: (2) there's barriers with high habitat distance 
# that are in the 3rd quartile for option B and in the 4th
#quartiles for options A and C ("4, 3, 4" has 3 barriers). 
# (3) options A and C place barriers with a lot of other 
# barriers downstream in the 3rd quartile ("3, 2, 2" and "3, 2, 3" 
# have 7 barriers total). 
# (4) The 4th quartile of option A contains barriers with 
# more forest cover than barriers that are also in the 4th quartile
# for options B and C ("4, 3, 3" has 3 barriers).