# IMPORT DATA

#### LIBRARIES ####
library("lme4")
library("tidyr")
library("dplyr")
library("lubridate")
library("ggplot2")


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
  mutate(species=replace(species,species=="Sal.bra","Sal.sou"))

# Calculate Sums of bud, flower etc.
pheno <- CalcSums(pheno.dat)
head(pheno)



# Check data
pheno %>% 
  select(turfID, species, date, doy, origSite, destSite, block, treatment, nr.b, nr.f, nr.s, nr.r) %>%
  gather(key = pheno.stage, value = value, nr.b, nr.f, nr.s, nr.r) %>% 
  filter(value > 0) %>%
  filter(turfID == "H3-OTC") %>% 
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
  mutate(pheno.stage = factor(pheno.stage, levels = c("b", "f", "s", "r"))) %>% 
  mutate(pheno.var = factor(pheno.var, levles =c("first", "peak", "end"))) %>%
  gather(key = pheno.var, value = value, -turfID, -species, -pheno.stage) %>%  # create pheno.var and gather 4 variable into 1 column
  mutate(treatment = substring(turfID, 4, nchar(turfID))) %>% 
  mutate(block = substring(turfID, 2,2)) %>% 
  mutate(originSite = substring(turfID,1,1))
head(pheno.long)

# add destSite
pheno.long$destSite <- pheno.dat[match(pheno.long$turfID,pheno.dat$turfID),c("destSite")]

mutate(newTT = plyr::mapvalues(newTT, c("control", "TT2", "TT3", "TT4"), c("Control", "Warm", "Wet", "WarmWet"))) %>%
  mutate(newTT = factor(newTT, levels = c("Control", "Warm", "Wet", "WarmWet"))) %>%



#### CALCULATE DAYS BETWEEN PHENO.STAGES IN DAYS ####
### DURATION BETWEEN FIRST AND END OF PHENO.STAGES
pheno.long <- pheno.long %>% 
  select(turfID, species, newTT, value, pheno.var, pheno.stage, d.dosm, o.dosm) %>%
  spread(key = pheno.stage, value = value) %>% 
  # in days
  mutate(d.smb = b - d.dosm) %>% 
  mutate(o.smb = b - ifelse(newTT == "control", o.dosm, d.dosm)) %>% 
  mutate(bf = f-b, fs = s-f) %>% # calculate difference in days between bud-flower and flower-seed
  gather(key = pheno.stage, value = value, b, f, r, s, d.smb, o.smb, bf, fs) %>% 
  mutate(pheno.unit = ifelse(pheno.stage %in% c("d.smb", "o.smb", "bf", "fs"), "days", "doy")) %>% # create variable pheno.unit
  # calculate duration of stages
  spread(key = pheno.var, value = value) %>% 
  mutate(duration = ifelse(pheno.unit == "doy", end-(first-1), NA)) %>% # calculate duration
  gather(key = pheno.var, value = value, end, first, peak, duration) %>% 
  mutate(pheno.unit = replace(pheno.unit, pheno.var == "duration", "days")) %>% 
  filter(!is.na(value))

mutate(treatment = plyr::mapvalues(treatment, c("OTC", "C", "O", "1", "2"), c("OTC", "Control", "Local", "Warm", "Cold"))) %>% 
  mutate(treatment = factor(treatment, levels=c("Control", "OTC", "Warm", "Cold", "Local"))) %>%
  mutate(destSite = factor(pheno.var, levles =c("H", "A", "M"))) %>% 
  mutate(originSite = factor(pheno.var, levles =c("H", "A", "M"))) %>% 

# Making Figures
pheno.long %>% 
  #filter(pheno.stage == "f", species == "All.cya") %>% 
  group_by(pheno.stage, pheno.var, treatment, destSite) %>% 
  summarise(mean = mean(value)) %>% 
  ggplot(aes(x = treatment, y = mean, color = pheno.var)) +
  geom_point() +
  facet_grid(pheno.stage~destSite)

pheno %>% 
  select(turfID, species, date, doy, origSite, destSite, block, treatment, nr.b, nr.f, nr.s, nr.r) %>%
  filter(turfID == "A6-1", species == "Pot.leu") %>% 
  gather(key = pheno.stage, value = value, nr.b, nr.f, nr.s, nr.r) %>%
  filter(value > 0) %>%
  ggplot(aes(x = doy, y = value, color = pheno.stage)) +
  geom_line() +
  facet_wrap(~ species, scales = "free")



