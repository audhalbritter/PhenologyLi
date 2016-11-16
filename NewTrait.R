
#################### REDEFINE STAGES AND ALPINE VEGETATION 


library("tidyr")
library("dplyr")
library("readxl")


#### TRAIT DATA ####
# read in trait data
trait <- read_excel("SpeciesTraits2016_China.xlsx", col_names = TRUE)
head(trait) 


#### redifine the flowering Time according to first month of flowering ####
# early = Apr
# mid  = May,Jun
# late = Jul,Aug

#### elevation, compare to the tree line of Gongga Mountain
# alpine = lowerlimit > 3700m 
# lower = lowerlimit < 3700m

#### flowerDuration,according to eflora
# long  >1 months
# short =< 1 months

# import the original trait data
trait <- trait %>% 
  mutate(floweringTime = replace(floweringTime, floweringTime == "summer",NA)) # replace summer with NA
# difine floweringTime according the info from e-Flora Of China 
NewTrait <- trait %>% 
  mutate(first = substr(trait$floweringTime, 1, 3)) %>% # time of first flowering month 
  mutate(end = substr(trait$floweringTime, 5, 7)) %>% # time of end flowering month 
  # species with only one month of flowering
  mutate(end = ifelse(sp == "Bro.sin","Jul", end)) %>% 
  mutate(end = ifelse(sp == "Sal.sou","Jun", end)) %>%
  mutate(FlTime = ifelse(first == "Apr","early",
                        ifelse( first %in% c("May","Jun"),"mid", "late"))) %>%
  #mutate(FlTime = ifelse(sp %in% c("Kob.sp.sigan","Kob.sp.small","Kob.sp.yellow","Gen.sp.white","Pol.mac"), "early",
                          #ifelse(sp %in% c("Car.sp.black","Car.sp.black.big","Car.sp.middle","Car.sp.yellow","Fes.sp.big","Fes.sp.small","Fra.sp.2","Pol.run","Cer.sze","Tar.lug"),"mid",
                                 #ifelse(sp =="Gen.sp","late", FlTime)))) %>% # define early, mid and late flowering species
  mutate(FirstMonth = plyr::mapvalues(first, c("Apr","May","Jun","Jul","Aug"), c("4","5","6","7","8"))) %>%
  mutate(EndMonth = plyr::mapvalues(end, c("May","Jun","Jul","Aug","Sep","Oct","Nov"), c("5","6","7","8","9","10","11"))) %>%
  mutate(FlowerDuration = ifelse(as.numeric(EndMonth)-as.numeric(FirstMonth) < 2,"short","long")) %>% # Warning because there are NA's. We have no information for these species
  #mutate(FlowerDuration = ifelse(sp %in% c("Cer.sze","Pol.mac","Pol.viv","Eup.reg","Fra.sp.2","Gen.sp","Gen.sp.white","Pri.ame","Ver.sze","Tar.lug"), "short",
                                     #ifelse(sp %in% c("Par.pus","Cya.inf","Ran.tan"),"long", FlowerDuration))) %>% ### !!! NEED TO FIX THIS LIST OF SP; MAYBE CHECK IN DATA
  
  ### ELEVATION
  mutate(Span = ifelse(lowerLimit > 3700, "alpine", "lower")) %>%
  mutate(Span = ifelse(sp %in% c("Art.fla","Car.sp.black","Car.sp.yellow","Hal.ell","Tri.rep"),"lower",
                                  ifelse( sp %in% c("Car.sp.black.big","Car.sp.middle","Fes.sp.big",
                                                    "Fes.sp.small","Fra.sp.2","Gen.sp","Gen.sp.white",
                                                    "Kob.sp.sigan","Kob.sp.small","Kob.sp.yellow","Par.pus"),"alpine", Span))) %>% # speceis with NA for lower limit; alpine = sp only found at A or H site
  select(sp, family, functionalGroup, lifeSpan, FlTime, Span, FlowerDuration)

  
#### take first flowering Time and floweringduration info from pheno data
 Flowering <- pheno.long %>%
   select(turfID,species, newtreat, value, pheno.var, pheno.stage) %>%
   filter(pheno.stage == "f") %>% 
   filter(newtreat == "Control") %>% 
   filter(pheno.var %in% c( "first", "end", "duration")) %>%
   group_by(species, pheno.var) %>% 
   unique() %>%
   summarise(mean = mean(value)) %>% #mean value of each
   spread(pheno.var, mean) %>% 
   # acording to T. DORJI et al, 2012
   # early = mean of first flowring before 10th,Jun(Doy:160)
   # late = mean of first flowering later than 19th,Jul(Doy:200)
   # otherwise = mid
   mutate(FlTime2 = ifelse(first < 160, "early",
                           ifelse(first > 200, "late", "mid"))) %>%
   # short = durtion < mean duration
   # long = durationg >= mean duration
   mutate(flowerduration2 = ifelse(duration < 10, "short", "long")) %>%
   select(-first,-duration,-end)  # delete the "end" cloumn
 
 
 
 #### match NewTrait data with Flowering
 #### i.e. compare/add the info from eflora and pheno data

NewTrait <- NewTrait %>% left_join(Flowering, by = c("sp" = "species")) 
 



#################################################################################################################
####Old Trait stuff ######
#################################################################################################################

# define flowering time
# early: <= 4 month until June
# mid: <= 4 month and between April and August
# late: <= 4 month from July
# late: >= 4 month
trait <- trait %>% mutate(flTime = 
                            ifelse(floweringTime %in% c("Apr-Jun", "Apr-May", "Jun", "May-Jun"), "early",
                                   ifelse(floweringTime %in% c("Jul-Aug", "Apr-Jul", "Jul", "Jun-Jul", "May-Jul", "May-Jul-(Aug)", "summer", "Jun-Aug", "Jun-Sep"), "mid", 
                                          ifelse(floweringTime %in% c("Aug-Nov", "Aug-Oct", "Aug-Sep", "Jul-Sep", "Jul-Oct"), "late", "always")))
) %>%
  mutate(flTime = ifelse(sp %in% c("Car.sp.black","Car.sp.black.big","Car.sp.middle","Car.sp.yellow","Fes.sp.big","Kob.sp.sigan","Kob.sp.small","Kob.sp.yellow"), "early", flTime))

