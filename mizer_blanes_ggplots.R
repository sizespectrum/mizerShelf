#PLOTS MIZER GGPLOT#####

library(ggplot2)
library(tidyverse)
library(hrbrthemes)

biomass_sel_hake$Species <- as.factor(biomass_sel_hake$Species)

commercial = c("Hake", "Red mullet", "Poor cod","Blue whiting", "Shortfin squid",
                          "Blue whiting", "Horse mackerel", "Shortfin squid", 
                          "Angler fish", "Horned octopus", "Gurnards")

data.filt <- biomass_sel_hake[biomass_sel_hake$Species %in% commercial,]

ggplot(data=data.filt, aes(x=Year, y=Biomass, colour=Species)) +
         geom_line() +
  theme_classic()


biomass_sel_mullet$Species <- as.factor(biomass_sel_mullet$Species)

commercial = c("Hake", "Red mullet", "Poor cod","Blue whiting", "Shortfin squid",
               "Blue whiting", "Horse mackerel", "Shortfin squid", 
               "Angler fish", "Horned octopus", "Gurnards")

data.filt <- biomass_sel_mullet[biomass_sel_mullet$Species %in% commercial,]

ggplot(data=data.filt, aes(x=Year, y=Biomass, colour=Species)) +
  geom_line() +
  theme_classic()

        
biomass_sel_mullet_and_hk$Species <- as.factor(biomass_sel_mullet_and_hk$Species)

commercial = c("Hake", "Red mullet", "Poor cod","Blue whiting", "Shortfin squid",
               "Blue whiting", "Horse mackerel", "Shortfin squid", 
               "Angler fish", "Horned octopus", "Gurnards")

data.filt <- biomass_sel_mullet_and_hk[biomass_sel_mullet_and_hk$Species %in% commercial,]

ggplot(data=data.filt, aes(x=Year, y=Biomass, colour=Species)) +
  geom_line() +
  theme_classic()


biomass_sel_target$Species <- as.factor(biomass_sel_target$Species)

commercial = c("Hake", "Red mullet", "Poor cod","Blue whiting", "Shortfin squid",
               "Blue whiting", "Horse mackerel", "Shortfin squid", 
               "Angler fish", "Horned octopus", "Gurnards")

data.filt <- biomass_sel_target[biomass_sel_target$Species %in% commercial,]

ggplot(data=data.filt, aes(x=Year, y=Biomass, colour=Species)) +
  geom_line() +
  theme_classic()



       
biomass_dis_Hake$Species <- as.factor(biomass_dis_Hake$Species)

commercial = c("Hake", "Red mullet", "Poor cod","Blue whiting", "Shortfin squid",
               "Blue whiting", "Horse mackerel", "Shortfin squid", 
               "Angler fish", "Horned octopus", "Gurnards")

data.filt <- biomass_dis_Hake[biomass_dis_Hake$Species %in% commercial,]

ggplot(data=data.filt, aes(x=Year, y=Biomass, colour=Species)) +
  geom_line() +
  theme_classic()


biomass_dis$Species <- as.factor(biomass_dis$Species)

commercial = c("Hake", "Red mullet", "Poor cod","Blue whiting", "Shortfin squid",
               "Blue whiting", "Horse mackerel", "Shortfin squid", 
               "Angler fish", "Horned octopus", "Gurnards", "Starfish", 
               "Suprabenthic crustacea", "Angular crab", "Harbour crab",
               "Small DF crustacea", "DF worms", "Endobethic pred. worms", 
               "Red snapping shrimp", "Spotted flounder")

data.filt <- biomass_dis[biomass_dis$Species %in% commercial,]

ggplot(data=data.filt, aes(x=Year, y=Biomass, colour=Species)) +
  geom_line() +
  theme_classic()




biomass_dis_mullet$Species <- as.factor(biomass_dis_mullet$Species)

commercial = c("Hake", "Red mullet", "Poor cod","Blue whiting", "Shortfin squid",
               "Blue whiting", "Horse mackerel", "Shortfin squid", 
               "Angler fish", "Horned octopus", "Gurnards")

data.filt <- biomass_dis_mullet[biomass_dis_mullet$Species %in% commercial,]

ggplot(data=data.filt, aes(x=Year, y=Biomass, colour=Species)) +
  geom_line() +
  theme_classic()


biomass_effort$Species <- as.factor(biomass_effort$Species)

commercial = c("Hake", "Red mullet", "Poor cod","Blue whiting", "Shortfin squid",
               "Blue whiting", "Horse mackerel", "Shortfin squid", 
               "Angler fish", "Horned octopus", "Gurnards", "Starfish", 
               "Suprabenthic crustacea", "Angular crab", "Harbour crab",
               "Small DF crustacea", "DF worms", "Endobethic pred. worms", 
               "Red snapping shrimp", "Spotted flounder")

data.filt <- biomass_effort[biomass_effort$Species %in% commercial,]

ggplot(data=data.filt, aes(x=Year, y=Biomass, colour=Species)) +
  geom_line() +
  theme_classic()


yield_sel_hake$Species <- as.factor(yield_sel_hake$Species)

commercial = c("Hake", "Red mullet", "Poor cod","Blue whiting", "Shortfin squid",
               "Blue whiting", "Horse mackerel", "Shortfin squid", 
               "Angler fish", "Horned octopus", "Gurnards")

data.filt <- yield_sel_hake[yield_sel_hake$Species %in% commercial,]

ggplot(data=data.filt, aes(x=Year, y=Yield, colour=Species)) +
  geom_line() +
  theme_classic()



yield_sel_mullet$Species <- as.factor(yield_sel_mullet$Species)

commercial = c("Hake", "Red mullet", "Poor cod","Blue whiting", "Shortfin squid",
               "Blue whiting", "Horse mackerel", "Shortfin squid", 
               "Angler fish", "Horned octopus", "Gurnards")

data.filt <- yield_sel_mullet[yield_sel_mullet$Species %in% commercial,]

ggplot(data=data.filt, aes(x=Year, y=Yield, colour=Species)) +
  geom_line() +
  theme_classic()


yield_sel_mullet_and_hk$Species <- as.factor(yield_sel_mullet_and_hk$Species)

commercial = c("Hake", "Red mullet", "Poor cod","Blue whiting", "Shortfin squid",
               "Blue whiting", "Horse mackerel", "Shortfin squid", 
               "Angler fish", "Horned octopus", "Gurnards")

data.filt <- yield_sel_mullet_and_hk[yield_sel_mullet_and_hk$Species %in% commercial,]

ggplot(data=data.filt, aes(x=Year, y=Yield, colour=Species)) +
  geom_line() +
  theme_classic()


yield_sel_target$Species <- as.factor(yield_sel_target$Species)

commercial = c("Hake", "Red mullet", "Poor cod","Blue whiting", "Shortfin squid",
               "Blue whiting", "Horse mackerel", "Shortfin squid", 
               "Angler fish", "Horned octopus", "Gurnards")

data.filt <- yield_sel_target[yield_sel_target$Species %in% commercial,]

ggplot(data=data.filt, aes(x=Year, y=Yield, colour=Species)) +
  geom_line() +
  theme_classic()





yield_dis_Hake$Species <- as.factor(yield_dis_Hake$Species)

commercial = c("Hake", "Red mullet", "Poor cod","Blue whiting", "Shortfin squid",
               "Blue whiting", "Horse mackerel", "Shortfin squid", 
               "Angler fish", "Horned octopus", "Gurnards")

data.filt <- yield_dis_Hake[yield_dis_Hake$Species %in% commercial,]

ggplot(data=data.filt, aes(x=Year, y=Yield, colour=Species)) +
  geom_line() +
  theme_classic()


yield_dis_mullet$Species <- as.factor(yield_dis_mullet$Species)

commercial = c("Hake", "Red mullet", "Poor cod","Blue whiting", "Shortfin squid",
               "Blue whiting", "Horse mackerel", "Shortfin squid", 
               "Angler fish", "Horned octopus", "Gurnards")

data.filt <- yield_dis_mullet[yield_dis_mullet$Species %in% commercial,]

ggplot(data=data.filt, aes(x=Year, y=Yield, colour=Species)) +
  geom_line() +
  theme_classic()


yield_effort$Species <- as.factor(yield_effort$Species)

commercial = c("Hake", "Red mullet", "Poor cod","Blue whiting", "Shortfin squid",
               "Blue whiting", "Horse mackerel", "Shortfin squid", 
               "Angler fish", "Horned octopus", "Gurnards")

data.filt <- yield_effort[yield_effort$Species %in% commercial,]

ggplot(data=data.filt, aes(x=Year, y=Yield, colour=Species)) +
  geom_line() +
  theme_classic()
