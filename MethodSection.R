#### METHOD ####

### STUDY SITES
# calculate difference in temperature between controls
monthlyiButton %>% 
  group_by(treatment, depth, site) %>% 
  summarise(mean = mean(Tmean)) %>% 
  mutate(site = factor(site, levels = c("H", "A", "M", "L"))) %>%
  spread(key = site, value = mean) %>% 
  mutate(HA = H - A, AM = A - M, ML = M - L)


## PHENOLOGICAL MEASUREMENTS
# DURATION
pheno.dat %>% group_by(year) %>% summarise(min(date), max(date))


# COUNT NUMBER OF SPECIES IN 2016 AND 2017
taxa2 <- taxa %>% 
  select(new_name, family, functionalGroup, lifeSpan, floweringTime) %>% 
  group_by(new_name, family, functionalGroup, lifeSpan, floweringTime) %>% 
  filter(!duplicated(new_name))
  
phenology %>% 
  full_join(taxa2, by = c("species" = "new_name")) %>% 
  filter(pheno.var == "peak") %>% 
  distinct(year, species, functionalGroup) %>% 
  arrange(year, functionalGroup) %>%
  group_by(year, functionalGroup) %>% 
  summarize(n())
  
# TABLE S2
phenology %>% 
  full_join(taxa2, by = c("species" = "new_name")) %>% 
  filter(pheno.var == "peak") %>% 
  distinct(year, species, functionalGroup) %>% 
  arrange(year, functionalGroup, species) %>% pn
  spread(key = year, value = species)
