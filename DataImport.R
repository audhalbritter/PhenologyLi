# IMPORT DATA

#### LIBRARIES ####
library("lme4")
library("tidyverse")
library("lubridate")
library("ggplot2")
library("readxl")

pn <- . %>% print(n = Inf)
source(file = "PhenoFunctions.R")
#load(file = "taxa.RData")


#### IMPORT DATA ####
#### 2016
dat1 <- ReadInBodyPhenology("Phenologydata2016_China_H.csv", "H", "2016")
dat2 <- ReadInBodyPhenology("Phenologydata2016_China_A.csv", "A", "2016")
dat3 <- ReadInBodyPhenology("Phenologydata2016_China_M.csv", "M", "2016")

#### 2017
dat4 <- ReadInBodyPhenology("Phenologydata2017_China_H.csv", "H", "2017")
dat5 <- ReadInBodyPhenology("Phenologydata2017_China_A.csv", "A", "2017")
dat6 <- ReadInBodyPhenology("Phenologydata2017_China_M.csv", "M", "2017")

# RBIND TABLES
pheno.dat <- rbind(dat1[-nrow(dat1),], dat2[-nrow(dat2),], dat3[-nrow(dat3),], dat4[-nrow(dat4),], dat5[-nrow(dat5),], dat6[-nrow(dat6),])
pheno.dat <- pheno.dat %>% filter(turfID != "")
#head(pheno.dat)
#str(pheno.dat)


#### CREATE META DATA ####
meta.pheno <- pheno.dat %>% 
  distinct(turfID, origSite, destSite, block, treatment, year) %>% 
  mutate(newTT = plyr::mapvalues(treatment, c("1", "2", "C", "O", "OTC"), c("1", "2", "C", "C", "OTC")))


## CORRECT SPECIES NAMES ##
# Replace wrong names
pheno.dat <- pheno.dat %>%
  mutate(species=replace(species,species=="Pol.leu","Pot.leu"))%>%
  mutate(species=replace(species,species=="Cal.pal","Oxy.gla"))%>%
  mutate(species=replace(species,species=="Cha.tha","Jun.leu"))%>%
  mutate(species=replace(species,species=="Sal.bra","Sal.sou")) %>% 
  mutate(species=replace(species,species=="Agr.ner","Agr.sp")) %>% 
  mutate(species=replace(species,species=="Jun.all","Jun.leu")) %>% 
  mutate(species=replace(species,species=="Gal.spa","Gal.hof")) %>% 
  mutate(species=replace(species,species=="Voi.sze","Vio.sze"))

# Change names to match community and trait data
pheno.dat <- pheno.dat %>% 
  #mutate(species = replace(species, species %in% c("Car.sp.black", "Car.sp.yellow"), "Car.spp")) %>% 
  #mutate(species = replace(species, species %in% c("Kob.sp.small", "Kob.sp.sigan"), "Kob.spp")) %>% 
  mutate(species = replace(species, species %in% c("Fes.sp.big", "Fes.sp.small"), "Fes.spp")) %>% 
  mutate(species = replace(species, species %in% c("Luz.mul"), "Luzula")) %>% 
  mutate(species = replace(species, species %in% c("Gen.sp", "Gen.sp.white"), "Gen.spp")) %>%
  mutate(species = replace(species, species %in% c("Eup.reg"), "Eup.L")) %>% 
  mutate(species = replace(species, species %in% c("Agr.sp"), "Agr.spp")) %>% 
  mutate(species = replace(species, species %in% c("Sax.lin"), "Saxifrage")) %>% 
  mutate(species = replace(species, species %in% c("Oxy.gla"), "Oxy.yun")) %>%
  mutate(species = replace(species, species %in% c("Fra.sp.2"), "Fra.spp")) %>% 
  mutate(species = replace(species, species %in% c("Dey.pul"), "Cal.lah")) %>%
  mutate(species = replace(species, species %in% c("Dey.sca"), "Cal.sca")) %>%
  mutate(species = replace(species, species %in% c("All.cya"), "All.pra")) %>% #???
  mutate(species = replace(species, species %in% c("Sau.cet", "Sau.gra", "Sau.hie", "Sau.pac", "Sau.sub"), "Sau.spp"))
  
# Compare community taxa table with phenology data
setdiff(pheno.dat$species, taxa$species)
# Ok in trait: Cya.inc, Sal.sou, Ped.ver, Sau.hie, Sau.sub, Lom.car, Bro.sin, Tar.lug


## Calculate Sums for bud, flower, seed and ripe seeds per turf
pheno <- CalcSums(pheno.dat)
#head(pheno)

pheno %>%
  filter(year == "2017", species == "Car.A") %>% 
  ggplot(aes(x = doy, y = ripe)) +
  geom_point() +
  facet_wrap(~ turfID)


#### CALCULATE FIRST, PEAK, END AND DURATION ####
## MAKE LONG DATA SET ##
pheno.long <- pheno %>%
  select(turfID, species, date, year, doy, origSite, destSite, block, treatment, bud, flower, seed, ripe) %>%
  gather(key = pheno.stage, value = value, bud, flower, seed, ripe) %>% # make variable pheno.stage
  group_by(year, turfID, species, pheno.stage) %>%  # group by turfID, species and phenological stage to calculate first, end etc for each stage
  mutate(minDoy = min(doy, na.rm = TRUE)) %>% # calculate min doy
  group_by(minDoy, add = TRUE) %>% # add variable but remember the previous groups
  filter(value > 0) %>%
  summarize(first = first(doy), end = last(doy), peak = doy[which.max(value)]) %>%
  filter(first > minDoy) %>% # remove if plant is flowering in the first week
  ungroup() %>% 
  select(-minDoy) %>% # remove this variable
  # make the data nice, rename variables and order them
  #mutate(pheno.stage = substring(pheno.stage, nchar(pheno.stage), nchar(pheno.stage))) %>%  # take last letter from pheno.stage
  mutate(duration = end - (first-1)) %>% # calculate duration
  gather(key = pheno.var, value = value, -turfID, -species, -pheno.stage, -year) %>%  # create pheno.var and gather 4 variable into 1 column
  mutate(pheno.var = factor(pheno.var, levels = c("first", "peak", "end", "duration"))) %>%
  left_join(meta.pheno, by = c("turfID", "year"))


#### CLEAN DATA ####

# Replace impossible values
# Replace "first" if: b > f, f > s, s > r, b == s, f == r, b == r
# to test use:
# filter(pheno.var == "end", b > f) %>% arrange(species, turfID) %>% pn
pheno.long <- pheno.long %>% 
  mutate(pheno.stage = factor(pheno.stage, levels = c("bud", "flower", "seed", "ripe"))) %>% 
  spread(key = pheno.stage, value = value) %>% 
  mutate(bud = ifelse(pheno.var == "first" & bud > flower, NA, bud)) %>% # bud = NA
  mutate(flower = ifelse(pheno.var == "first" & flower > seed & !species %in% c("Pol.viv", "Pol.mac"), NA, flower)) %>% # flower = NA
  mutate(ripe = ifelse(pheno.var == "first" & seed > ripe, NA, ripe)) %>% # ripe = NA
  mutate(bud = ifelse(pheno.var == "first" & bud == seed, NA, bud)) %>% # bud = NA
  filter(!(species == "Car.sp.yellow" & turfID == "A6-O")) %>%  # remove this turf
  mutate(ripe = ifelse(species %in% c("Pol.viv", "Pol.mac"), NA, ripe)) %>% # remove ripe seeds for Polygonum, does not make sense, difficult to see when bulbils are ripe
  gather(key = pheno.stage, value = value, -year, -turfID, -species, -pheno.var, -origSite, -destSite, -block, -treatment, -newTT) %>% 
  spread(key = pheno.var, value = value) %>% 
  mutate(duration = ifelse(is.na(first), NA, duration)) %>%  # replace duration with NA if first is NA
  gather(key = pheno.var, value = value, -year, -turfID, -species, -pheno.stage, -origSite, -destSite, -block, -treatment, -newTT) %>% 
  filter(!is.na(value))


## List of species with more than 3 occurrences per species, site, treatment and pheno.var
ThreeOccurences <- pheno.long %>% 
  filter(pheno.var == "first") %>% 
  group_by(species, year, turfID, pheno.stage) %>% 
  summarise(n = n()) %>%
  left_join(meta.pheno, by = c("turfID", "year")) %>% 
  group_by(species, year, pheno.stage, newTT, origSite) %>% # keep O and C together
  summarize(n = n()) %>% 
  filter(n > 2)

# Reduce nr. species 
pheno.long <- pheno.long %>% 
  inner_join(ThreeOccurences, by = c("species", "year", "pheno.stage", "newTT", "origSite")) %>% 
  select(-n)

# Select species that occur in at least 2 treatments
NrTreat <- as.data.frame(table(pheno.long$species, pheno.long$newTT, pheno.long$year))
sp.list <- NrTreat %>% 
  filter(Freq > 0) %>% 
  group_by(Var3, Var1) %>% 
  summarise(n = n()) %>%
  filter(n > 1) %>% ### !!!! SHOULD BE > 2, TO CALC MEAN FIRST DATE !!!!
  # remove sp with c and treat not at same site
  filter(Var3 != "2016" | !Var1 %in% c("Jun.leu", "Ver.sze")) %>% 
  select(-n)

# Reduce nr. species 
pheno.long <- pheno.long %>% 
  # remove species with only one treatment
  inner_join(sp.list, by = c("species" = "Var1", "year" = "Var3"))
  



#### CLEAN VARIABLES ####
phenology <- pheno.long %>%
  # order sites
  mutate(destSite = factor(destSite, levels =c("H", "A", "M"))) %>% 
  mutate(origSite = factor(origSite, levels =c("H", "A", "M"))) %>% 
  # order and rename treatments
  mutate(treatment = plyr::mapvalues(treatment, c("OTC", "C", "O", "1", "2"), c("OTC", "Control", "Local", "Warm", "Cold"))) %>% 
  mutate(treatment = factor(treatment, levels=c("Control", "OTC", "Warm", "Cold", "Local"))) %>% 
  mutate(newTT = plyr::mapvalues(newTT, c("OTC", "C", "1", "2"), c("OTC", "Control", "Warm", "Cold"))) %>% 
  mutate(newTT = factor(newTT, levels=c("Control", "OTC", "Warm", "Cold"))) %>% 
  mutate(pheno.stage = plyr::mapvalues(pheno.stage, c("bud", "flower", "seed", "ripe"), c("Bud", "Flower", "Seed", "Ripe"))) %>% 
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Seed", "Ripe"))) %>% 
  mutate(pheno.unit = ifelse(pheno.var == "duration", "days", "doy"))



### NEEDS TO BE FIXED!!!
# Left_join trait data
#phenology <- phenology %>% left_join(NewTrait, by = c("species" = "sp"))
# check species
#setdiff(pheno.long$species, NewTrait$sp)
#setdiff(NewTrait$sp, pheno.long$species)


# Save pheno.long
#save(phenology, file = "Phenology.RData")

