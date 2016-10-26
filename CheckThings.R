### Check things


# How many species are the same/different per turfID
SameAndDiffSP <- pheno.long %>% 
  select(turfID, species, treatment, origSite, block, value, pheno.var, pheno.stage) %>% 
  filter(treatment %in% c("Control", "OTC")) %>% 
  group_by(origSite, block, species, treatment) %>% 
  summarize(mean = mean(value)) %>% 
  spread(key = treatment, value = mean) %>% 
  mutate(Diff = ifelse(is.na(Control), "different", ifelse(is.na(OTC), "different", "same"))) %>% 
  group_by(origSite, block, Diff) %>% 
  summarise(n = n()) %>% 
  mutate(SiteBlock = paste(origSite, block)) %>% 
  ggplot(aes(x = Diff, y = n)) +
  geom_point() +
  facet_wrap(~SiteBlock)
  
  


