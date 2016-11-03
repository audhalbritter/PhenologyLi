# Figures Presentation
message("in important figures.R - 1")
#### LIBRARIES ####
library("lme4")
library("tidyr")
library("dplyr")
library("lubridate")
library("ggplot2")
library("readxl")


############################################################################################################################
#### COMMUNITY FIGURES ####
############################################################################################################################

# Community Figures
PeakCommunity <- pheno.long %>% # first/end time of the 4 stages show the same tendency with peak
  select(turfID, species, newtreat, origSite, block, value, pheno.var, pheno.stage) %>% 
  #filter(newtreat %in% c("Control", "OTC")) %>% 
  group_by(origSite, species, newtreat, pheno.var, pheno.stage) %>% 
  summarize(mean = mean(value), sd = sd(value)) %>% 
  filter(origSite != "M") %>%
  filter(pheno.var == "peak", pheno.stage %in% c("b","f","s","r")) %>%
  ggplot(aes(x = newtreat, y = mean)) +
  geom_boxplot() +
  ylab("Doy")+
  facet_grid(pheno.stage ~origSite)

DurationCommunity <- pheno.long %>% 
  select(turfID, species, newtreat, origSite, block, value, pheno.var, pheno.stage) %>% 
  #filter(newtreat %in% c("Control", "OTC")) %>% 
  group_by(origSite, species, newtreat, pheno.var, pheno.stage) %>% 
  summarize(mean = mean(value), sd = sd(value)) %>% 
  filter(origSite != "M") %>%
  filter(pheno.stage %in% c("b","f","s","r"), pheno.var == "duration") %>%
  ggplot(aes(x = newtreat, y = mean)) +
  geom_boxplot() +
  ylab("Days") +
  facet_grid(pheno.stage ~origSite)

StagesCommunity <-  pheno.long %>% 
  select(turfID, species, newtreat, origSite, block, value, pheno.var, pheno.stage) %>% 
  #filter(newtreat %in% c("Control", "OTC")) %>% 
  group_by(origSite, species, newtreat, pheno.var, pheno.stage) %>% 
  summarize(mean = mean(value), sd = sd(value)) %>% 
  filter(origSite != "M") %>%
  filter(pheno.stage %in% c("bf","fs","sr")) %>%
  ggplot(aes(x = newtreat, y = mean)) +
  geom_boxplot() +
  ylab("Days") +
  facet_grid(pheno.stage ~origSite)

############################################################################################################################
#### FUNCTIONAL GROUPS FIGURES ####
############################################################################################################################

PeakFounctionalGroup <- pheno.long %>% #first/end time of each functionalGroup show the same tendency with peak
  select(turfID, species, newtreat, origSite, block, value, pheno.var, pheno.stage,functionalGroup) %>% 
  #filter(newtreat %in% c("Control", "OTC")) %>% 
  group_by(origSite, species, newtreat, pheno.var, pheno.stage,functionalGroup) %>% 
  summarize(mean = mean(value), sd = sd(value)) %>% 
  filter(origSite != "M", functionalGroup != "shrub") %>%
  filter(pheno.var == "peak", pheno.stage %in% c("b","f","s","r")) %>%
  ggplot(aes(x = newtreat, y = mean, color= functionalGroup )) +
  geom_boxplot() +
  ylab("Doy")+
  facet_grid(pheno.stage ~origSite)

DurationFounctionalGroup <- pheno.long %>% 
  select(turfID, species, newtreat, origSite, block, value, pheno.var, pheno.stage,functionalGroup) %>% 
  #filter(newtreat %in% c("Control", "OTC")) %>% 
  group_by(origSite, species, newtreat, pheno.var, pheno.stage,functionalGroup) %>% 
  summarize(mean = mean(value), sd = sd(value)) %>% 
  filter(origSite != "M", functionalGroup != "shrub") %>%
  filter(pheno.var == "duration") %>%
  ggplot(aes(x = newtreat, y = mean, color= functionalGroup )) +
  geom_boxplot() +
  ylab("Days") +
  facet_grid(pheno.stage ~origSite)

StagesFounctionalGroup <- pheno.long %>% 
  select(turfID, species, newtreat, origSite, block, value, pheno.var, pheno.stage,functionalGroup) %>% 
  #filter(newtreat %in% c("Control", "OTC")) %>% 
  group_by(origSite, species, newtreat, pheno.var, pheno.stage,functionalGroup) %>% 
  summarize(mean = mean(value), sd = sd(value)) %>% 
  filter(origSite != "M", functionalGroup != "shrub") %>%
  filter(pheno.stage %in% c("bf","fs","sr")) %>%
  ggplot(aes(x = newtreat, y = mean, color= functionalGroup )) +
  geom_boxplot() +
  ylab("Days") +
  facet_grid(pheno.stage ~origSite)


############################################################################################################################
#### SPECIES FIGURES ####
############################################################################################################################

# Compare Treatments

#import the data

Treat<- pheno.long %>% 
  #filter(pheno.var == "first", pheno.stage == "f", origSite == "H", species == "Gen.cra") %>% 
  select(turfID, species, newtreat, origSite, block, value, pheno.var, pheno.stage) %>% 
  group_by(origSite, species, pheno.var, pheno.stage, newtreat) %>% 
  summarize(mean = mean(value)) %>%
  spread(key = newtreat, value = mean) %>% 
  gather(key = newTT, value = newTTvalue,  -species, -origSite, -pheno.stage, -pheno.var, -Control) %>% # devide control and treamtents
  mutate(Diff = newTTvalue - Control) %>% 
  mutate(newTT = factor(newTT, levels=c("OTC", "Warm", "Cold"))) %>% 
  filter(!is.na(Diff))

  Treat2 <- expand.grid(newTT=unique(Treat$newTT), species=unique(Treat$species), origSite = unique(Treat$origSite), pheno.var = unique(Treat$pheno.var)) %>% data.frame %>% left_join(Treat)

  # make figures of the first time of different stages
firstbudSpecies <- Treat2 %>%
  filter(pheno.var == "first", pheno.stage == "b") %>%
  ggplot( aes(y = Diff, x = species, fill = newTT)) +
  geom_bar(position="dodge", stat="identity") +
  coord_flip() +
  ylab("Treatment - Control") +
  xlab("") +
  ggtitle("first buds") +
  facet_wrap(~ origSite)


firstfloweringSpecies <- Treat2 %>%
  filter(pheno.var == "first", pheno.stage == "f") %>%
  ggplot( aes(y = Diff, x = species, fill = newTT)) +
  geom_bar(position="dodge", stat="identity") +
  coord_flip() +
  ylab("Treatment - Control") +
  xlab("") +
  ggtitle("first flowering") +
  facet_wrap(~ origSite)

firstseedingSpecies <- Treat2 %>%
  filter(pheno.var == "first", pheno.stage == "s") %>%
  ggplot( aes(y = Diff, x = species, fill = newTT)) +
  geom_bar(position="dodge", stat="identity") +
  coord_flip() +
  ylab("Treatment - Control") +
  xlab("") +
  ggtitle("first seeding") +
  facet_wrap(~ origSite)

firstripeseedSpecies <- Treat2 %>%
  filter(pheno.var == "first", pheno.stage == "r") %>%
  ggplot( aes(y = Diff, x = species, fill = newTT)) +
  geom_bar(position="dodge", stat="identity") +
  coord_flip() +
  ylab("Treatment - Control") +
  xlab("") +
  ggtitle("first ripe seeds") +
  facet_wrap(~ origSite)

  # maike figures of the duration of different stagesD
DurationseedingSpecies <- Treat2 %>%
  filter(pheno.var == "duration", pheno.stage == "s") %>%
  ggplot( aes(y = Diff, x = species, fill = newTT)) +
  geom_bar(position="dodge", stat="identity") +
  coord_flip() +
  ylab("Treatment - Control") +
  xlab("") +
  ggtitle("Duration of seeding") +
  facet_wrap(~ origSite)

  # duration of the first time of neighboring stages
StagesBfSpecies <- Treat2 %>% #bf is similar with sr
  filter( pheno.stage == "bf") %>%
  ggplot( aes(y = Diff, x = species, fill = newTT)) +
  geom_bar(position="dodge", stat="identity") +
  coord_flip() +
  ylab("Treatment - Control") +
  xlab("") +
  ggtitle("duration of first bud and first flower") +
  facet_wrap(~ origSite)

StagesFsSpecies <- Treat2 %>% 
  filter( pheno.stage == "fs") %>%
  ggplot( aes(y = Diff, x = species, fill = newTT)) +
  geom_bar(position="dodge", stat="identity") +
  coord_flip() +
  ylab("Treatment - Control") +
  xlab("") +
  ggtitle("duration of first flower and seeding") +
  facet_wrap(~ origSite)