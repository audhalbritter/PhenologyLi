# IMPORT DATA

#### LIBRARIES ####
library("lme4")
library("tidyverse")
library("lubridate")
library("ggplot2")
library("readxl")
library("cowplot")

pn <- . %>% print(n = Inf)
source(file = "PhenoFunctions.R")
#load(file = "taxa.RData")


#### IMPORT DATA ####
#### 2016
dat1 <- ReadInBodyPhenology2016("Data/Phenologydata2016_China_H.csv", "H", "2016")
dat2 <- ReadInBodyPhenology2016("Data/Phenologydata2016_China_A.csv", "A", "2016")
dat3 <- ReadInBodyPhenology2016("Data/Phenologydata2016_China_M.csv", "M", "2016")

#### 2017
dat4 <- ReadInBodyPhenology2017("Data/17-09-25_Phenologydata2017_China_H.csv", "H", "2017")
dat5 <- ReadInBodyPhenology2017("Data/17-09-25_Phenologydata2017_China_A.csv", "A", "2017")
dat6 <- ReadInBodyPhenology2017("Data/17-09-25_Phenologydata2017_China_M.csv", "M", "2017")
dat7 <- ReadInBodyPhenology2017("Data/17-09-25_Phenologydata2017_China_L.csv", "L", "2017")

#### 2017
dat8 <- ReadInBodyPhenologyExtra("Data/17-09-26_ExtraControls_2017_H.csv", "H", "2017")
dat8 <- dat8 %>% 
  mutate(block = plyr::mapvalues(block, c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), c("22", "23", "24", "25", "26", "27", "28", "29", "30", "31")))
dat9 <- ReadInBodyPhenologyExtra("Data/17-09-26_ExtraControls_2017_A.csv", "A", "2017")
dat9 <- dat9 %>% 
  filter(block != "") %>%
  mutate(block = plyr::mapvalues(block, c("1", "2", "3", "4", "5", "7", "8", "9", "10"), c("32", "33", "34", "35", "36", "37", "38", "39", "40")))
dat10 <- ReadInBodyPhenologyExtra("Data/17-09-26_ExtraControls_2017_M.csv", "M", "2017")
dat10 <- dat10 %>% 
  filter(block != "") %>% 
  mutate(block = plyr::mapvalues(block, c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), c("41", "42", "43", "44", "45", "46", "47", "48", "49", "50")))

# RBIND TABLES
pheno.dat <- dat1 %>% 
  bind_rows(dat2, dat3, dat4, dat5, dat6, dat7, dat8, dat9, dat10) %>% 
  filter(turfID != "") %>%  # remove empty rows
  filter(!is.na(doy))

#save(pheno.dat, file = "pheno.dat.RData")
#load(file = "pheno.dat.RData")
#head(pheno.dat)
#str(pheno.dat)


#### CREATE META DATA ####
meta.pheno <- pheno.dat %>% 
  distinct(turfID, origSite, destSite, block, treatment, year) %>% 
  mutate(newTT = plyr::mapvalues(treatment, c("1", "2", "C", "O", "OTC", "EC"), c("1", "2", "C", "C", "OTC", "C"))) # variable newTT merges all Controls


## IMPORT SPECIES TABLE ##
taxa <- read_excel("Data/SpeciesTraits2016_China_ZL_170927.xlsx", sheet = 1, col_names = TRUE)
taxaDictionary <- taxa %>% 
  gather(key = year, value = sp, sp_2016, sp_2017) %>% 
  rename(sp_new = new_name) %>% 
  select(year, sp, sp_new) %>% 
  mutate(year = gsub("sp_", "", year)) 


## CORRECT SPECIES NAMES ##
# Replace wrong names
pheno.dat <- pheno.dat %>%
  # 2016
  mutate(species=replace(species,species=="Sal.bra","Sal.sou")) %>%
  mutate(species=replace(species,species %in% c("Agr.ner", "Agr.sp"),"Agr.spp")) %>%
  mutate(species=replace(species,species %in% c("Jun.all", "Jun.sp"),"Jun.spp")) %>%
  mutate(species=replace(species,species=="Voi.sze","Vio.sze")) %>% 
  # 2017
  mutate(species=replace(species,species=="luz.mul","Luz.mul")) %>%
  mutate(species=replace(species,species=="cya.inc","Cya.inc")) %>%
  mutate(species=replace(species,species=="Pol.mac.","Pol.mac")) %>%
  mutate(species=replace(species,species=="tan.tat","Tan.tat")) %>%
  mutate(species=replace(species,species=="ver.sze","Ver.sze")) %>%
  mutate(species=replace(species,species %in% c("Poa.", "Poa.sppspp"),"Poa.spp"))



## JOIN TAXA DICTIONARY, REPLACE NAMES TO MATCH EACH YEAR
pheno.dat <- pheno.dat %>% 
  full_join(taxaDictionary, by = c("species" = "sp", "year")) %>% 
  rename(sp_old = species, species = sp_new) %>% 
  filter(!is.na(species)) %>%  # remove some empty lines
  filter(!is.na(doy))


# Compare community and Trait taxa table with phenology data
#load(file = "taxa.RData")
#load(file = "TraitTaxa.RData")
#setdiff(pheno.dat$species, taxa$species)
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


# Remove Extra controls to make it simpler
#pheno.long <- pheno.long %>% 
  #filter(treatment != "EC")


## List of species with more than 3 occurrences per species, site, treatment and pheno.var
ThreeOccurences <- pheno.long %>% 
  #filter(pheno.var == "first") %>% 
  group_by(species, year, turfID, pheno.stage, pheno.var) %>% 
  summarise(n = n()) %>%
  left_join(meta.pheno, by = c("turfID", "year")) %>% 
  group_by(species, year, pheno.stage, pheno.var, newTT, origSite) %>% # keep O and C together
  summarize(n = n()) %>% 
  filter(n > 2)

# Reduce nr. species 
pheno.long <- pheno.long %>% 
  inner_join(ThreeOccurences, by = c("species", "year", "pheno.stage", "pheno.var", "newTT", "origSite")) %>% 
  select(-n)

# Select species that occur in Control, Warm and C
NrTreat <- as.data.frame(table(pheno.long$species, pheno.long$newTT, pheno.long$year, pheno.long$pheno.stage, pheno.long$pheno.var))
sp.list <- NrTreat %>% 
  filter(Freq > 0) %>% 
  mutate(Var2 = plyr::mapvalues(Var2, c("1", "2", "C", "OTC"), c("Warm", "Cold", "C", "OTC"))) %>% 
  spread(key = Var2, value = Freq) %>% 
  filter(!is.na(C)) %>% # need to occur at least in C
  mutate(Warm = ifelse(is.na(Warm), 0, 1), Cold = ifelse(is.na(Cold), 0, 1), C = ifelse(is.na(C), 0, 1), OTC = ifelse(is.na(OTC), 0, 1)) %>% 
  mutate(sum = Warm + OTC + Cold) %>% 
  filter(sum > 0) %>% 
  select(-Warm, -Cold, -C, -OTC, -sum)

# Reduce nr. species 
pheno.long <- pheno.long %>% 
  # remove species with only one treatment
  inner_join(sp.list, by = c("species" = "Var1", "year" = "Var3", "pheno.stage" = "Var4", "pheno.var" = "Var5"))
  


#### CLEAN VARIABLES ####
phenology <- pheno.long %>% 
  # order sites
  mutate(destSite = factor(destSite, levels =c("H", "A", "M", "L"))) %>% 
  mutate(origSite = factor(origSite, levels =c("H", "A", "M"))) %>% 
  # order and rename treatments
  mutate(treatment = plyr::mapvalues(treatment, c("OTC", "C", "O", "1", "2", "EC"), c("OTC", "Control", "Local", "Warm", "Cold", "ExtraControl"))) %>% 
  mutate(treatment = factor(treatment, levels=c("Control", "OTC", "Warm", "Cold", "Local", "ExtraControl"))) %>% 
  mutate(newTT = plyr::mapvalues(newTT, c("OTC", "C", "1", "2"), c("OTC", "Control", "Warm", "Cold"))) %>% 
  mutate(newTT = factor(newTT, levels=c("Control", "OTC", "Warm", "Cold"))) %>% 
  mutate(pheno.stage = plyr::mapvalues(pheno.stage, c("bud", "flower", "seed", "ripe"), c("Bud", "Flower", "Seed", "Ripe"))) %>% 
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Seed", "Ripe"))) %>% 
  mutate(pheno.unit = ifelse(pheno.var == "duration", "days", "doy")) %>% 
  mutate(block = as.numeric(block)) %>%
  # replace block in A and M site to 11-30
  spread(key = origSite, value = block) %>%
  mutate(A = ifelse(treatment != "ExtraControl", A + 7, A), M = ifelse(treatment != "ExtraControl", M + 14, M)) %>%
  gather(key = origSite, value = block, H, A, M) %>% 
  filter(!is.na(block))


#save(phenology, file = "Phenology.RData")
#save(phenology, file = "PhenologyWEC.RData")

### NEEDS TO BE FIXED!!!
# Left_join trait data
#phenology <- phenology %>% left_join(NewTrait, by = c("species" = "sp"))
# check species
#setdiff(pheno.long$species, NewTrait$sp)
#setdiff(NewTrait$sp, pheno.long$species)


### NOT NEEDED ### ****************************************************************
### CALCULATE DIFFERENCES BETWEEN TREATMENT AND CONTROL ###
differences <- phenology %>% 
  filter(year == 2017) %>% 
  select(turfID, species, origSite, block, newTT, pheno.stage, pheno.var, value) %>% 
  spread(key = newTT, value = value) %>% 
  
  # Calculate site mean value for controls
  group_by(species, origSite, pheno.stage, pheno.var) %>%
  mutate(ControlMeanSite = mean(Control, na.rm = TRUE), ControlSDSite = sd(Control, na.rm = TRUE)) %>%
  select(-turfID) %>% 
  gather(key = newTT, value = value, OTC, Warm, Cold) %>% 
  mutate(Diff = value - ifelse(!is.na(Control), Control, ControlMeanSite)) %>% 
  filter(!is.na(Diff))

