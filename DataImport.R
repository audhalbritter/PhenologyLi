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
dat1 <- ReadInBodyPhenology2016("Phenologydata2016_China_H.csv", "H", "2016")
dat2 <- ReadInBodyPhenology2016("Phenologydata2016_China_A.csv", "A", "2016")
dat3 <- ReadInBodyPhenology2016("Phenologydata2016_China_M.csv", "M", "2016")

#### 2017
dat4 <- ReadInBodyPhenology2017("17-09-25_Phenologydata2017_China_H.csv", "H", "2017")
dat5 <- ReadInBodyPhenology2017("17-09-25_Phenologydata2017_China_A.csv", "A", "2017")
dat6 <- ReadInBodyPhenology2017("17-09-25_Phenologydata2017_China_M.csv", "M", "2017")
dat7 <- ReadInBodyPhenology2017("17-09-25_Phenologydata2017_China_L.csv", "L", "2017")

#### 2017
dat8 <- ReadInBodyPhenologyExtra("17-09-25_ExtraControls_2017_H.csv", "H", "2017")
dat9 <- ReadInBodyPhenologyExtra("17-09-25_ExtraControls_2017_A.csv", "A", "2017")
dat10 <- ReadInBodyPhenologyExtra("17-09-25_ExtraControls_2017_M.csv", "M", "2017")

# RBIND TABLES
pheno.dat <- dat1 %>% 
  bind_rows(dat2, dat3, dat4, dat5, dat6, dat7, dat8, dat9, dat10) %>% 
  filter(turfID != "") # remove empty rows

save(pheno.dat, file = "pheno.dat.RData")
load(file = "pheno.dat.RData")
#head(pheno.dat)
#str(pheno.dat)


#### CREATE META DATA ####
meta.pheno <- pheno.dat %>% 
  distinct(turfID, origSite, destSite, block, treatment, year) %>% 
  mutate(newTT = plyr::mapvalues(treatment, c("1", "2", "C", "O", "OTC", "EC"), c("1", "2", "C", "C", "OTC", "C")))


## IMPORT SPECIES TABLE ##
taxa <- read_excel("SpeciesTraits2016_China_ZL_170925.xlsx", sheet = 1, col_names = TRUE)
taxa <- taxa %>% 
  gather(key = year, value = sp, sp_2016, sp_2017) %>% 
  rename(sp_new = new_name) %>% 
  select(year, sp, species, species_new, sp_new, family, functionalGroup, lifeSpan, floweringTime, fruitsTime, lowerLimit, higherLimit) %>% 
  mutate(year = gsub("sp_", "", year))

## JOIN TAXA DICTIONARY, REPLACE NAMES TO MATCH EACH YEAR
pheno.dat %>% 
  anti_join(taxa, by = c("species" = "sp")) %>% distinct(species)


## CORRECT SPECIES NAMES ##
# Replace wrong names
pheno.dat <- pheno.dat %>%
  mutate(species=replace(species,species=="Pol.leu","Pot.leu"))%>%
  mutate(species=replace(species,species=="Cal.pal","Oxy.gla"))%>% #?
  mutate(species=replace(species,species=="Cha.tha","Jun.leu"))%>% #?
  mutate(species=replace(species,species=="Sal.bra","Sal.sou")) %>% #?
  mutate(species=replace(species,species=="Agr.ner","Agr.sp")) %>% #?
  mutate(species=replace(species,species=="Jun.all","Jun.leu")) %>% #?
  mutate(species=replace(species,species=="Gal.spa","Gal.hof")) %>% #?
  mutate(species=replace(species,species=="luz.mul","Luz.mul")) %>%
  mutate(species=replace(species,species=="cya.inc","Cya.inc")) %>%
  mutate(species=replace(species,species=="Pol.mac.","Pol.mac")) %>%
  mutate(species=replace(species,species=="tan.tat","Tan.tat")) %>%
  mutate(species=replace(species,species=="ver.sze","Ver.sze")) %>%
  mutate(species=replace(species,species=="Voi.sze","Vio.sze"))

# Change names to match community and trait data
pheno.dat <- pheno.dat %>% 
  #mutate(species = replace(species, species %in% c("Car.sp.black", "Car.sp.yellow"), "Car.spp")) %>% 
  mutate(species = replace(species, species %in% c("Poa.sp.", "Poacaea.sp"), "Poa.sp")) %>% 
  mutate(species = replace(species, species %in% c("Kob.sigan", "Kob.sp.sigan"), "Kob.sig")) %>% 
  mutate(species = replace(species, species %in% c("Kob.sp.small"), "Kob.small")) %>% 
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
  mutate(species = replace(species, species %in% c("Tan.tat"), "Pyr.tat")) %>%
  mutate(species = replace(species, species %in% c("All.cya"), "All.pra")) %>% #???
  mutate(species = replace(species, species %in% c("Sau.cet", "Sau.gra", "Sau.hie", "Sau.pac", "Sau.sub"), "Sau.spp"))
  

# Compare community and Trait taxa table with phenology data
load(file = "taxa.RData")
load(file = "TraitTaxa.RData")
setdiff(pheno.dat$species, taxa$species)
# Ok in trait: Cya.inc, Sal.sou, Ped.ver, Sau.hie, Sau.sub, Lom.car, Bro.sin, Tar.lug


## Calculate Sums for bud, flower, seed and ripe seeds per turf
pheno <- CalcSums(pheno.dat)
#head(pheno)


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
  filter(n > 2) %>% 
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
  mutate(treatment = plyr::mapvalues(treatment, c("OTC", "C", "O", "1", "2", "EC"), c("OTC", "Control", "Local", "Warm", "Cold", "ExtraControl"))) %>% 
  mutate(treatment = factor(treatment, levels=c("Control", "OTC", "Warm", "Cold", "Local", "ExtraControl"))) %>% 
  mutate(newTT = plyr::mapvalues(newTT, c("OTC", "C", "1", "2"), c("OTC", "Control", "Warm", "Cold"))) %>% 
  mutate(newTT = factor(newTT, levels=c("Control", "OTC", "Warm", "Cold"))) %>% 
  mutate(pheno.stage = plyr::mapvalues(pheno.stage, c("bud", "flower", "seed", "ripe"), c("Bud", "Flower", "Seed", "Ripe"))) %>% 
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Seed", "Ripe"))) %>% 
  mutate(pheno.unit = ifelse(pheno.var == "duration", "days", "doy"))


save(phenology, file = "Phenology.RData")

### NEEDS TO BE FIXED!!!
# Left_join trait data
#phenology <- phenology %>% left_join(NewTrait, by = c("species" = "sp"))
# check species
#setdiff(pheno.long$species, NewTrait$sp)
#setdiff(NewTrait$sp, pheno.long$species)


# Save pheno.long
#save(phenology, file = "Phenology.RData")

