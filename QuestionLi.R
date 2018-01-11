# import data
source(file = "DataImport.R")

# meta data
d1 <- pheno.dat %>% 
  filter(treatment == "EC") %>% 
  select(turfID, sp_old, date, doy, origSite, destSite, block, treatment, year, species)
# select only values
d2 <- pheno.dat %>% 
  filter(treatment == "EC") %>% 
  select(-turfID, -sp_old, -date, -doy, -origSite, -destSite, -block, -treatment, -year, -species)
# replace NA with 0 and all numbers with 1
d2 <- ifelse(is.na(d2), 0, 1)
# count the number of subplots each species occurs per turfID
d2 <- d2 %>% 
  as_tibble() %>% 
  mutate(bud = rowSums(.[grep("b\\.", names(.))], na.rm = TRUE)) %>% 
  mutate(flower = rowSums(.[grep("f\\.", names(.))], na.rm = TRUE)) %>% 
  mutate(seed = rowSums(.[grep("s\\.", names(.))], na.rm = TRUE)) %>% 
  mutate(ripe = rowSums(.[grep("r\\.", names(.))], na.rm = TRUE)) %>% 
  select(bud, flower, seed, ripe)

# HOW MANY SPECIES OCCUR IN AT LEAST 3 SUBPLOTS
d1 %>% bind_cols(d2) %>% 
  filter(bud > 2) %>% distinct(species) # replace bud with flower, seed, ripe

# BUD: 43 species
# FLOWER: 35 species
# SEED: 43 species
# RIPE SEED: 25 species

