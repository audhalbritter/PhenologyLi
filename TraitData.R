
#######################################################################################
####using the avarange first flowering time to define the trait of each species
#######################################################################################


#load libraries

library("tidyr")
library("dplyr")
library("readxl")


#### TRAIT DATA ####
# read in trait data
trait <- read_excel("SpeciesTraits2016_China.xlsx", col_names = TRUE)
head(trait) 

# import the original trait data
trait <- trait %>% 
  mutate(floweringTime = replace(floweringTime, floweringTime == "summer",NA)) # replace summer with NA

#caculate the averange time of each species, acoording the oringal pheno data
# control spp
Flowering <- pheno.long %>%
  select(turfID, species, newtreat, value, pheno.var, pheno.stage, origSite) %>%
  filter(pheno.stage == "f") %>% 
  filter(newtreat == "Control") %>% 
  filter(pheno.var %in% c( "first", "end", "duration")) %>%
  group_by(species, pheno.var) %>% 
  unique() %>%
  summarise(mean = mean(value)) %>% #mean value of each
  spread(pheno.var, mean)

#obs! beacuse we do use the "origSite == M"  spoecies, so we don take care the trait of these spp,
#so it doesn't matter that what's the trait of M site' species
#### early == averange first flowering time is before 182(160631)
## else late group
#### alpine == lowerlimit is above the tree line
## else general species
NewTrait <- trait %>% left_join(Flowering, by = c("sp" = "species")) %>% #add date info to trait data
  mutate(FlTime = ifelse(first > 182, "late", "early")) %>% #before DOY:182(160631)
  mutate(FlTime = ifelse(is.na(duration), "late", FlTime)) %>% # alreadly compared to the Warm¡¢OTC and Cold spp averange first date
  mutate(FlTime = ifelse(sp %in% c("Sag.jap","Kob.sp.small","Ane.dem","Car.sp.black.big", "Gen.sp.white","Pri.ame"), "early", FlTime)) %>% #Pri.ame,cut 26th,May;Ane.dem cut for floral traits at 0602
  mutate(Span = ifelse(lowerLimit > 3700, "alpine", "general")) %>%
  mutate(Span = ifelse(sp %in% c("Art.fla","Car.sp.black","Car.sp.yellow","Hal.ell","Tri.rep"),"general",
                       ifelse( sp %in% c("Car.sp.black.big","Car.sp.middle","Fes.sp.big",
                                         "Fes.sp.small","Fra.sp.2","Gen.sp","Gen.sp.white",
                                         "Kob.sp.sigan","Kob.sp.small","Kob.sp.yellow","Par.pus"),"alpine", Span)))


