
#### Those figures made for the first report for Yan,VV
# Figures Presentation

## ----loadPhenology
#load libraries
library("lme4")
library("tidyr")
library("dplyr")
library("lubridate")
library("ggplot2")
library("readxl")

# load data
load("PhenoLong.RData")

# set the theme
th <- theme()

##############################################################################
####  FIRST REPORT #### 2016.11.05 ####
##############################################################################

## ----loadTrait
trait <- read_excel("SpeciesTraits2016_China.xlsx", col_names = TRUE)

trait <- trait %>% mutate(flTime = 
                            ifelse(floweringTime %in% c("Apr-Jun", "Apr-May", "Jun", "May-Jun"), "early",
                                   ifelse(floweringTime %in% c("Jul-Aug", "Apr-Jul", "Jul", "Jun-Jul", "May-Jul", "May-Jul-(Aug)", "summer", "Jun-Aug", "Jun-Sep"), "mid", 
                                          ifelse(floweringTime %in% c("Aug-Nov", "Aug-Oct", "Aug-Sep", "Jul-Sep", "Jul-Oct"), "late", "always")))) %>%
mutate(flTime = ifelse(sp %in% c("Car.sp.black","Car.sp.black.big","Car.sp.middle","Car.sp.yellow","Fes.sp.big","Kob.sp.sigan","Kob.sp.small","Kob.sp.yellow"), "early", flTime))
pheno.long <- pheno.long %>% left_join(trait, by = c("species" = "sp"))


## ----loadTreatment
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

Treat2 <- expand.grid(newTT=unique(Treat$newTT), species=unique(Treat$species), origSite = unique(Treat$origSite), pheno.var = unique(Treat$pheno.var),pheno.stage = unique(Treat$pheno.stage)) %>% data.frame %>% left_join(Treat)


############################################################################################################################
#### COMMUNITY FIGURES #### 2016.11.05 ####
############################################################################################################################

## ----PeakCommunity
PeakCommunity <- pheno.long %>% # first/end time of the 4 stages show the same tendency with peak
  select(turfID, species, newtreat, origSite, block, value, pheno.var, pheno.stage) %>% 
  #filter(newtreat %in% c("Control", "OTC")) %>% 
  group_by(origSite, species, newtreat, pheno.var, pheno.stage) %>% 
  summarize(mean = mean(value), sd = sd(value)) %>% 
  filter(origSite != "M") %>%
  filter(pheno.var == "peak", pheno.stage %in% c("b","f","s","r")) %>%
  ggplot(aes(x = newtreat, y = mean)) +
  geom_boxplot() +
  ylab("Doy") + xlab("Treatment") +
  ggtitle("Peak") +
  facet_grid( origSite ~ pheno.stage) +
  th
  print(PeakCommunity)

## ----DurationCommunity
DurationCommunity <- pheno.long %>% 
  select(turfID, species, newtreat, origSite, block, value, pheno.var, pheno.stage,RepDuration) %>% 
  #filter(newtreat %in% c("Control", "OTC")) %>% 
  group_by(origSite, species, newtreat, pheno.var, pheno.stage) %>% 
  summarize(mean = mean(value), sd = sd(value)) %>% 
  filter(origSite != "M") %>%
  filter(pheno.stage %in% c("b","f","s","r"), pheno.var == "duration") %>%
  ggplot(aes(x = newtreat, y = mean)) +
  geom_boxplot() +
  ylab("Days") +  xlab("Treatment") +
  ggtitle("duration") +
  facet_grid(pheno.stage ~origSite) +
  th
  print(DurationCommunity)


## ----TimeCommunity
TimeCommunity <-  pheno.long %>% 
  select(turfID, species, newtreat, origSite, block, value, pheno.var, pheno.stage) %>% 
  #filter(newtreat %in% c("Control", "OTC")) %>% 
  group_by(origSite, species, newtreat, pheno.var, pheno.stage) %>% 
  summarize(mean = mean(value), sd = sd(value)) %>% 
  filter(origSite != "M") %>%
  filter(pheno.stage %in% c("BF","FS","SR")) %>%
  ggplot(aes(x = newtreat, y = mean)) +
  geom_boxplot() +
  ylab("Days") +
  xlab("Treatment") +
  ggtitle("Time") +
  facet_grid(pheno.stage ~origSite)

############################################################################################################################
#### FUNCTIONAL GROUPS FIGURES #### 2016.11.05 ####
############################################################################################################################

## ----PeakFunctionalGroup
PeakFunctionalGroup <- pheno.long %>% #first/end time of each functionalGroup show the same tendency with peak
  select(turfID, species, newtreat, origSite, block, value, pheno.var, pheno.stage,functionalGroup) %>% 
  #filter(newtreat %in% c("Control", "OTC")) %>% 
  group_by(origSite, species, newtreat, pheno.var, pheno.stage,functionalGroup) %>% 
  summarize(mean = mean(value), sd = sd(value)) %>% 
  filter(origSite != "M", functionalGroup != "shrub") %>%
  filter(pheno.var == "peak", pheno.stage %in% c("b","f","s","r")) %>%
  ggplot(aes(x = newtreat, y = mean, fill= functionalGroup )) +
  geom_boxplot() +
  ylab("Doy") +
  xlabs("Treatment") +
  ggtitle("Peak") +
  facet_grid(pheno.stage ~origSite)+
  th
  print(PeakFunctionalGroup)
  


## ----DurationFunctionalGroup
DurationFunctionalGroup <- pheno.long %>% 
  select(turfID, species, newtreat, origSite, block, value, pheno.var, pheno.stage,functionalGroup) %>% 
  #filter(newtreat %in% c("Control", "OTC")) %>% 
  group_by(origSite, species, newtreat, pheno.var, pheno.stage,functionalGroup) %>% 
  summarize(mean = mean(value), sd = sd(value)) %>% 
  filter(origSite != "M", functionalGroup != "shrub") %>%
  filter(pheno.var == "duration") %>%
  ggplot(aes(x = newtreat, y = mean, fill= functionalGroup )) +
  geom_boxplot() +
  ylab("Days") +
  xlab("")+
  ggtitle("Duration")+
  facet_grid(pheno.stage ~origSite)+
  th
  print(DurationFunctionalGroup)

## ----TimeFunctionalGroup
TimeFunctionalGroup <- pheno.long %>% 
  select(turfID, species, newtreat, origSite, block, value, pheno.var, pheno.stage,functionalGroup) %>% 
  #filter(newtreat %in% c("Control", "OTC")) %>% 
  group_by(origSite, species, newtreat, pheno.var, pheno.stage,functionalGroup) %>% 
  summarize(mean = mean(value), sd = sd(value)) %>% 
  filter(origSite != "M", functionalGroup != "shrub") %>%
  filter(pheno.stage %in% c("bf","fs","sr")) %>%
  ggplot(aes(x = newtreat, y = mean, fill= functionalGroup )) +
  geom_boxplot() +
  ylab("Days") +
  xlab("Treatment") +
  ggtitle("Time") +
  facet_grid(pheno.stage ~origSite)+
  th
  print(TimeFunctionalGroup)
## ----nothing 

############################################################################################################################
#### SPECIES FIGURES #### 2016.11.05 ####
############################################################################################################################

# Compare Treatments

# make figures of the first time of different stages

## ----FirstBudSpecies 
FirstBudSpecies <- Treat2 %>%
  filter(pheno.var == "first", pheno.stage == "b") %>%
  ggplot( aes(y = Diff, x = species, fill = newTT)) +
  geom_bar(position="dodge", stat="identity") +
  coord_flip() +
  ylab("Treatment - Control") +
  xlab("") +
  ggtitle("first buds") +
  facet_wrap(~ origSite)+
  th
  print(FirstBudSpecies)

## ----FirstFloweringSpecies
FirstFloweringSpecies <- Treat2 %>%
  filter(pheno.var == "first", pheno.stage == "f") %>%
  ggplot( aes(y = Diff, x = species, fill = newTT)) +
  geom_bar(position="dodge", stat="identity") +
  coord_flip() +
  ylab("Treatment - Control") +
  xlab("") +
  ggtitle("First flowering") +
  facet_wrap(~ origSite)+
  th
 print(FirstFloweringSpecies)

## ----FirstSSeedingSpecies
FirstSeedingSpecies <- Treat2 %>%
  filter(pheno.var == "first", pheno.stage == "s") %>%
  ggplot( aes(y = Diff, x = species, fill = newTT)) +
  geom_bar(position="dodge", stat="identity") +
  coord_flip() +
  ylab("Treatment - Control") +
  xlab("") +
  ggtitle("First seeding") +
  facet_wrap(~ origSite)+
  th
print(FirstSeedingSpecies)

## ----FirstRipeseedSpecies
FirstRipeseedSpecies <- Treat2 %>%
  filter(pheno.var == "first", pheno.stage == "r") %>%
  ggplot( aes(y = Diff, x = species, fill = newTT)) +
  geom_bar(position="dodge", stat="identity") +
  coord_flip() +
  ylab("Treatment - Control") +
  xlab("") +
  ggtitle("First Ripe seeds") +
  facet_wrap(~ origSite)+
  th
print(FirstRipeseedSpecies)


# maike figures of the duration of different stages

## ----DurationseedingSpecies
DurationseedingSpecies <- Treat2 %>%
  filter(pheno.var == "duration", pheno.stage == "s") %>%
  ggplot( aes(y = Diff, x = species, fill = newTT)) +
  geom_bar(position="dodge", stat="identity") +
  coord_flip() +
  ylab("Treatment - Control") +
  xlab("") +
  ggtitle("Duration of seeding") +
  facet_wrap(~ origSite)+
  th
  print(DurationseedingSpecies)

# duration of the first time of neighboring stages

## ----TimeBfSpecies
TimeBfSpecies <- Treat2 %>% #bf is similar with sr
  filter( pheno.stage == "bf") %>%
  ggplot( aes(y = Diff, x = species, fill = newTT)) +
  geom_bar(position="dodge", stat="identity") +
  coord_flip() +
  ylab("Treatment - Control") +
  xlab("") +
  ggtitle("Time between first bud and first flower") +
  facet_wrap(~ origSite)+
  th
  print(TimeBfSpecies)

## ----TimeFsSpecies
TimeFsSpecies <- Treat2 %>% 
  filter( pheno.stage == "fs") %>%
  ggplot( aes(y = Diff, x = species, fill = newTT)) +
  geom_bar(position="dodge", stat="identity") +
  coord_flip() +
  ylab("Treatment - Control") +
  xlab("") +
  ggtitle("Time between first flower and seeding") +
  facet_wrap(~ origSite)+
  th
  print(TimeFsSpecies)
  
## ----end
  
############################################################################################################################
###### IMPORT NEWTRAIT #### 2016.11.15 ####
############################################################################################################################

  
  