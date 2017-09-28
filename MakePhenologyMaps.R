# **************************************************
# MAKE PHENOLOGY MAPS
PhenologyMap <- function(df){
  ggplot(df, aes(x = doy, y = value, color = pheno.stage)) + 
    geom_line() + 
    geom_point() +
    facet_wrap(~ turfID) +
    theme_minimal() +
    ggtitle(unique(df$species))
}

## plot maps
phenoMaps2016 <- pheno %>% 
  filter(year == "2016") %>% 
  select(turfID, species, date, doy, origSite, destSite, block, treatment, bud, flower, seed, ripe) %>%
  gather(key = pheno.stage, value = value, bud, flower, seed, ripe) %>% # make variable pheno.stage
  filter(value > 0) %>% 
  group_by(species) %>% 
  do(pheno.maps = PhenologyMap(.))

pdf(file = "Phenologymaps2016.pdf")
phenoMaps2016$pheno.maps
dev.off()

phenoMaps2017 <- pheno %>% 
  filter(year == "2017") %>% 
  select(turfID, species, date, doy, origSite, destSite, block, treatment, bud, flower, seed, ripe) %>%
  gather(key = pheno.stage, value = value, bud, flower, seed, ripe) %>% # make variable pheno.stage
  filter(value > 0) %>% 
  group_by(species) %>% 
  do(pheno.maps = PhenologyMap(.))

## Now open up a pdf file and write all the plots out as separate pages
## the output pdf will be located in the getwd() directory named 'Rplots.pdf'
##
pdf(file = "Phenologymaps2017.pdf")
phenoMaps2017$pheno.maps
dev.off()
# **************************************************