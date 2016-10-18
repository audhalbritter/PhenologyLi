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
  mutate_each(funs(as.numeric), first, peak, end) %>% # make variables numeric (probably not necessary)
  mutate(pheno.stage = substring(pheno.stage, nchar(pheno.stage), nchar(pheno.stage))) %>%  # take last letter from pheno.stage
  gather(key = pheno.var, value = value, -turfID, -species, -pheno.stage) %>%  # create pheno.var and gather 4 variable into 1 column
  mutate(treatment = substring(turfID, 4, nchar(turfID))) %>% 
  mutate(block = substring(turfID, 2,2)) %>% 
  mutate(originSite = substring(turfID,1,1))
head(pheno.long)

pheno.long$destSite <- pheno.dat[match(pheno.long$turfID,pheno.dat$turfID),c("destSite")]


pheno.long %>% 
  filter(originSite == "A", species == "All.pra") %>% 
  group_by(species, pheno.var, treatment) %>% 
  ggplot(aes(x = treatment, y = value)) +
  geom_boxplot() +
  facet_grid(pheno.stage~pheno.var)


