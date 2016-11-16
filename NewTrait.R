
#################### REDEFINE STAGES AND ALPINE VEGETATION 

library("lme4")
library("tidyr")
library("dplyr")
library("lubridate")


#### TRAIT DATA ####
 
#### redifine the flowering Time ####
# early = Apr
# mid  = May,Jun
# late = Jul,Aug

#### elevation, compare to tree line
# alpine = lowerlimit > 3700m 
# lower = lowerlimit < 3700m

#### RepDuration,according to eflora
# long  >3 months
# short =< 3 months

NewTrait <- trait %>% 
  mutate( first= substr(trait$floweringTime,1,3)) %>% # time of first flowring 
  mutate( end = substr(trait$fruitsTime,5,7)) %>% #  time of end friut
  mutate( end = ifelse(sp %in% c("Cya.inf","Dey.pul","Eup.reg","Par.pus","Pri.ame","Ran.tan","Ver.sze"),substr(trait$floweringTime,5,7),
                       ifelse(sp == "Cli.pol","Sep",
                              ifelse(sp =="Ped.mus","Aug",
                                     ifelse(sp =="Bro.sin","Jul",end ))))) %>%
  mutate( FlTime = ifelse(first == "Apr","early",
                        ifelse( first %in% c("MAy","Jun"),"mid", "late"))) %>%
  mutate( FlTime = ifelse(sp %in% c("Kob.sp.sigan","Kob.sp.small","Kob.sp.yellow","Gen.sp.white","Pol.mac"), "early",
                          ifelse(sp %in% c("Car.sp.black","Car.sp.black.big","Car.sp.middle","Car.sp.yellow","Fes.sp.big","Fes.sp.small","Fra.sp.2","Pol.run","Cer.sze","Tar.lug"),"mid",
                                 ifelse(sp =="Gen.sp","late",FlTime )))) %>%
  mutate( FirstMouth = plyr::mapvalues(first, c("Apr","May","Jun","Jul","Aug","sum"),c("4","5","6","7","8","6"))) %>%
  mutate( EndMouth = plyr::mapvalues(end, c("Jun","Jul","Aug","Sep","Oct","Nov"),c("6","7","8","9","10","11"))) %>%
  mutate( RepDuration = ifelse(as.numeric(EndMouth)-as.numeric(FirstMouth) < 4,"short","long")) %>%
  mutate( RepDuration = ifelse(functionalGroup %in% c("graminoid"),"long",RepDuration)) %>%
  mutate( RepDuration = ifelse(sp %in% c("Cer.sze","Pol.mac","Pol.viv","Eup.reg","Fra.sp.2","Gen.sp","Gen.sp.white","Pri.ame","Ver.sze","Tar.lug"),"short",
                                     ifelse(sp %in% c("Par.pus","Cya.inf","Ran.tan"),"long",RepDuration))) %>%
  mutate( Span = ifelse( lowerLimit > 3700,"alpine","lower")) %>%
  mutate( Span = ifelse( sp %in% c("Art.fla","Car.sp.black","Car.sp.yellow","Hal.ell","Tri.rep"),"lower",
                                  ifelse( sp %in% c("Car.sp.black.big","Car.sp.middle","Fes.sp.big",
                                                    "Fes.sp.small","Fra.sp.2","Gen.sp","Gen.sp.white",
                                                    "Kob.sp.sigan","Kob.sp.small","Kob.sp.yellow","Par.pus"),"alpine", Span))) %>%
  select(sp,family,functionalGroup,lifeSpan,FlTime,Span,RepDuration)

# macth data
pheno.long <- pheno.long %>% left_join(NewTrait, by = c("species" = "sp")) 
  
