# **************************************************
# MAKE PHENOLOGY MAPS
PhenologyMap <- function(df){
  ggplot(df, aes(x = doy, y = value, color = pheno.stage)) + 
    geom_line() + 
    geom_point() +
    facet_wrap(~ turfID) +
    ggtitle(unique(df$species))
}

## plot maps
phenoMaps <- pheno %>% 
  select(turfID, species, date, doy, origSite, destSite, block, treatment, nr.b, nr.f, nr.s, nr.r) %>%
  gather(key = pheno.stage, value = value, nr.b, nr.f, nr.s, nr.r) %>% # make variable pheno.stage
  filter(value > 0) %>% 
  group_by(species) %>% 
  do(pheno.maps = PhenologyMap(.))

## Now open up a pdf file and write all the plots out as separate pages
## the output pdf will be located in the getwd() directory named 'Rplots.pdf'
##
pdf(file = "Phenologymaps.pdf")
phenoMaps$pheno.maps
dev.off()
# **************************************************