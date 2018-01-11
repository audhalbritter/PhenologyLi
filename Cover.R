#### Flowering non-flowering

load(file = "~/Dropbox/transplant/USE THIS DATA/Community.Rdata", verbose = TRUE)

head(cover_thin)

cover <- cover_thin %>% 
  filter(year == "2016", TTtreat %in% c("OTC", "local", "warm1", "cool1", "control")) %>% 
  mutate(TTtreat = plyr::mapvalues(TTtreat, c("OTC", "local", "warm1", "cool1", "control"), c("OTC", "Control", "Warm", "Cool", "Control")))

# only flowering
flowering <- phenology %>% 
  filter(pheno.stage == "Flower", pheno.var == "peak", treatment != "ExtraControl", year == 2017) %>% 
  full_join(cover, by = c("turfID", "species")) %>% 
  mutate(flowering = ifelse(is.na(value), 0, 1)) %>% filter(is.na(value))

flowering %>% 
  ggplot(aes(x = treatment, y = flowering, color = newTT)) +
  geom_boxplot() +
  facet_grid( ~ origSite)
