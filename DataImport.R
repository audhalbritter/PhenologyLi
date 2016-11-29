# IMPORT DATA

#### LIBRARIES ####
library("lme4")
library("tidyr")
library("dplyr")
library("lubridate")
library("ggplot2")
library("readxl")


#### DATA ####
dat1 <- ReadInBodyPhenology("Phenologydata2016_China_H.csv", "H")
dat2 <- ReadInBodyPhenology("Phenologydata2016_China_A.csv", "A")
dat3 <- ReadInBodyPhenology("Phenologydata2016_China_M.csv", "M")
pheno.dat <- rbind(dat1[-nrow(dat1),], dat2[-nrow(dat2),], dat3[-nrow(dat3),])
pheno.dat <- pheno.dat %>% filter(turfID != "")
head(pheno.dat)
str(pheno.dat) 

# Replace wrong names
pheno.dat <- pheno.dat %>%
  mutate(species=replace(species,species=="Pol.leu","Pot.leu"))%>%
  mutate(species=replace(species,species=="Cal.pal","Oxy.gla"))%>%
  mutate(species=replace(species,species=="Cha.tha","Jun.leu"))%>%
  mutate(species=replace(species,species=="Sal.bra","Sal.sou")) %>% 
  mutate(species=replace(species,species=="Agr.ner","Agr.sp")) %>% 
  mutate(species=replace(species,species=="Jun.all","Jun.leu")) %>% 
  mutate(species=replace(species,species=="Gal.spa","Gal.hof"))

# Calculate Sums of bud, flower etc.
pheno <- CalcSums(pheno.dat)
head(pheno)


# Check data, make figures for pheno.stages
pheno %>% 
  select(turfID, species, date, doy, origSite, destSite, block, treatment, nr.b, nr.f, nr.s, nr.r) %>%
  gather(key = pheno.stage, value = value, nr.b, nr.f, nr.s, nr.r) %>% 
  filter(value > 0) %>%
  filter(turfID == "A4-1", species == "Car.sp.black") %>% 
  group_by(species, pheno.stage) %>% 
  ggplot(aes(x = doy, y = value, color = pheno.stage)) +
  geom_line() +
  facet_wrap(~ species, scales = "free")



#### CALCULATE FIRST, PEAK, END AND DURATION ####
### MAKE LONG DATA SET ###
pheno.long <- pheno %>%
  select(turfID, species, date, doy, origSite, destSite, block, treatment, nr.b, nr.f, nr.s, nr.r) %>%
  gather(key = pheno.stage, value = value, nr.b, nr.f, nr.s, nr.r) %>% # make variable pheno.stage
  group_by(turfID, species, pheno.stage) %>%  # group by turfID, species and phenological stage to calculate first, end etc for each stage
  mutate(minDoy = min(doy, na.rm = TRUE)) %>% # calculate min doy
  group_by(minDoy, add = TRUE) %>% # add variable but remember the previous groups
  filter(value > 0) %>%
  summarize(first = first(doy), end = last(doy), peak = doy[which.max(value)]) %>%
  filter(first > minDoy) %>% # remove if plant is flowering in the first week
  ungroup() %>% 
  select(-minDoy) %>% # remove this variable
  #mutate_each(funs(as.numeric), first, peak, end) %>% # make variables numeric (probably not necessary)
  # make the data nice, rename variables and order them
  mutate(pheno.stage = substring(pheno.stage, nchar(pheno.stage), nchar(pheno.stage))) %>%  # take last letter from pheno.stage
  mutate(duration = end - (first-1)) %>% # calculate duration
  gather(key = pheno.var, value = value, -turfID, -species, -pheno.stage) %>%  # create pheno.var and gather 4 variable into 1 column
  mutate(pheno.var = factor(pheno.var, levels = c("first", "peak", "end", "duration")))
head(pheno.long)

#### CALCULATE DAYS BETWEEN FIRST BUD AND FLOWER, FLOWER AND SEED ETC (PHENO.STAGES IN DAYS) ####
pheno.long <- pheno.long %>% 
  spread(key = pheno.stage, value = value) %>% 
  # calculate difference in days between peak bud-flower and flower-seed
  mutate(bf = ifelse(pheno.var == "peak", f-(b-1), NA), fs = ifelse(pheno.var == "peak", s-(f-1), NA), sr = ifelse(pheno.var == "peak", r-(s-1), NA)) %>%
  gather(key = pheno.stage, value = value, b, f, s, r, bf, fs, sr) %>%
  mutate(pheno.unit = ifelse(pheno.var == "duration", "days",
                             ifelse(pheno.var == "peak" & pheno.stage %in% c("bf", "fs", "sr"), "days", "doy"))) %>%
           filter(!is.na(value)) %>%  # remove empty rows
  mutate(value = ifelse(value < 0, NA, value)) # make negative values NA


# merge site, block and treatment
pheno.long[,(ncol(pheno.long)+1):(ncol(pheno.long)+4)] <- pheno.dat[match(pheno.long$turfID,pheno.dat$turfID),c("origSite", "destSite", "block", "treatment")]

# Rename variables and order
pheno.long <- pheno.long %>%
  mutate(destSite = factor(destSite, levels =c("H", "A", "M"))) %>% 
  mutate(origSite = factor(origSite, levels =c("H", "A", "M"))) %>% 
  mutate(treatment = plyr::mapvalues(treatment, c("OTC", "C", "O", "1", "2"), c("OTC", "Control", "Local", "Warm", "Cold"))) %>% 
  mutate(treatment = factor(treatment, levels=c("Control", "OTC", "Warm", "Cold", "Local"))) %>% 
  # make new variable combining Local and Control
  mutate(newtreat = plyr::mapvalues(treatment, c("OTC", "Control", "Local", "Warm", "Cold"), c("OTC", "Control", "Control", "Warm", "Cold"))) %>% 
  mutate(newtreat = factor(newtreat, levels=c("Control", "OTC", "Warm", "Cold"))) %>%
  mutate(pheno.stage = plyr::mapvalues(pheno.stage, c("b", "f", "s", "r", "bf", "fs", "sr"), c("Bud", "Flower", "Seed", "Ripe", "BudFlower", "FlowerSeed", "SeedRipe"))) %>% 
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Seed", "Ripe", "BudFlower", "FlowerSeed", "SeedRipe")))



# Left_join trait data
pheno.long <- pheno.long %>% left_join(NewTrait, by = c("species" = "sp"))
# check species
setdiff(pheno.long$species, NewTrait$sp)
setdiff(NewTrait$sp, pheno.long$species)
# These species never flowered, so not in pheno.long
#[1] "Ana.fla" "Oxy.gla" "Cer.sze" "Cya.inf" "Gal.spa" "Hal.ell" "Hed.alg" "Pol.run" "Pri.ame" "Pru.vul" "Rho.yun" "Tri.rep" "Vio.sze"


# Save pheno.long
save(pheno.long, file = "PhenoLong.RData")


