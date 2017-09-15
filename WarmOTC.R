#### COMAPARE PHENOLOGY BETWEEN OTC AND TRANSPLANT ####

# remove "Cold" treatment
phenology <- phenology %>% 
  filter(newTT != "Cold")

OnlyOTCWarm <- as.data.frame(table(phenology$species, phenology$newTT))
OnlyOTCWarm %>% 
  filter(Freq > 0) %>% 
  spread(key = Var2, value = Freq)

library("cowplot")
# Get Mean and SE
MeanSE <- phenology %>% 
  filter(pheno.var == "first", pheno.unit == "doy") %>% 
  group_by(newTT, origSite, pheno.stage, species) %>% 
  summarise(N = sum(!is.na(value)), mean = mean(value, na.rm = TRUE), se = sd(value, na.rm = TRUE)/sqrt(N))

# Calculate difference between Control and Treatment for SE
SEData <- MeanSE %>% 
  ungroup() %>% 
  select(-mean, -N) %>% # remove site, because it causes problems
  spread(key = newTT, value = se) %>% # spread Treatments
  mutate(Warm = Warm - Control, OTC = OTC - Control) %>% # Difference Treatment - Control
  gather(key = Treatment, value = SE, -origSite, -pheno.stage, -Control, -species) %>% # gather Treatments
  select(SE, origSite, pheno.stage, Treatment, species)


# Calculate difference between Control and Treatment for Mean
#SPOnlyInOneTreatment
MeanData <- MeanSE %>% 
  ungroup() %>% 
  select(-se, -N) %>% # remove site, because it causes problems
  spread(key = newTT, value = mean) %>% # spread Treatments
  mutate(Warm = Warm - Control, OTC = OTC - Control) %>% # Difference Treatment - Control
  gather(key = Treatment, value = Difference, -origSite, -pheno.stage, -Control, -species) %>% # gather Treatments
  mutate(newname = paste(origSite, Treatment, sep = "_")) %>% # paste Treatment and site, they are unique and can be renamed
  mutate(newname = plyr::mapvalues(newname, c("H_OTC", "A_OTC", "H_Warm", "A_Warm"), c("High alpine OTC", "Alpine OTC", "High alpine Warm", "Alpine Warm"))) %>% 
  mutate(newname = factor(newname, levels = c("High alpine OTC", "Alpine OTC", "High alpine Warm", "Alpine Warm"))) %>% 
  left_join(SEData, by = c("origSite" = "origSite", "pheno.stage" = "pheno.stage", "Treatment" = "Treatment", "species" = "species")) %>%   # join SW
  #filter(is.na(Difference))
  select(-Control) %>% 
  filter(!is.na(Difference)) %>% 
  group_by(origSite, pheno.stage, Treatment, newname) %>% 
  summarise(Difference = mean(Difference, na.rm = TRUE), SE = mean(SE, na.rm = TRUE))



Treat<- phenology %>% 
  #filter(pheno.var == "first", pheno.stage == "f", origSite == "H", species == "Gen.cra") %>% 
  select(turfID, species, newTT, origSite, block, value, pheno.var, pheno.stage) %>% 
  group_by(origSite, species, pheno.var, pheno.stage, newTT) %>% 
  summarize(mean = mean(value)) %>%
  spread(key = newTT, value = mean) %>% 
  gather(key = newTT, value = newTTvalue,  -species, -origSite, -pheno.stage, -pheno.var, -Control) %>% # devide control and treamtents
  mutate(Diff = newTTvalue - Control) %>% 
  mutate(newTT = factor(newTT, levels=c("OTC", "Warm"))) %>% 
  filter(!is.na(Diff))

Treat2 <- expand.grid(newTT=unique(Treat$newTT), species=unique(Treat$species), origSite = unique(Treat$origSite), pheno.var = unique(Treat$pheno.var),pheno.stage = unique(Treat$pheno.stage)) %>% data.frame %>% left_join(Treat)

Treat2 %>%
  filter(pheno.var == "first", pheno.stage == "Ripe") %>%
  ggplot( aes(y = Diff, x = species, fill = newTT)) +
  geom_bar(position="dodge", stat="identity") +
  coord_flip() +
  ylab("Treatment - Control") +
  xlab("") +
  ggtitle("first buds") +
  facet_wrap(~ origSite)

