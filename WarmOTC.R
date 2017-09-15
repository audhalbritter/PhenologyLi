#### COMAPARE PHENOLOGY BETWEEN OTC AND TRANSPLANT ####

source(file = "DataImport.R")

# remove "Cold" treatment
phenology <- phenology %>% 
  filter(newTT != "Cold")

OnlyOTCWarm <- as.data.frame(table(phenology$species, phenology$newTT))
OnlyOTCWarm %>% 
  filter(Freq > 0) %>% 
  spread(key = Var2, value = Freq)


#### Community Figures ####
MeanSE <- SpeciesMeanSE(phenology, "peak")
PlotCommunityData(MeanSE, "peak")

#### Species Figures ####
MeanSE <- SpeciesMeanSE(phenology, "peak")
PlotSpeciesData(MeanSE, "peak")

  

########################################################################
# Maybe do not do this, data is not detailed enough to calc this !!!!
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

########################################################################