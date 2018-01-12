#### COMAPARE PHENOLOGY BETWEEN OTC AND TRANSPLANT ####
load(file = "Phenology.RData", verbose = TRUE)
source(file = "PhenoFunctions.R")

library("tidyverse")
library("lubridate")


#### FIGURES ####
### Community Figures ###
MeanSE <- SpeciesMeanSE(phenology, "peak")
PlotCommunityData(MeanSE, "peak")

### Species Figures ###
MeanSE <- SpeciesMeanSE(phenology)
SpeciesPlot <- PlotSpeciesData2(dat = MeanSE, phenovar = "peak", Year = 2017)
ggsave(SpeciesPlot, file = "Figures/SpeciesPlot.jpeg", width = 10, height = 5, dpi = 300)

# Test Sensitivity
head(phenology)
MeanSE %>% 
  left_join(AirTemp, by = c("origSite", "Treatment" = "newTT")) %>% 
  filter(pheno.var == "peak") %>% 
  mutate(origSite = factor(origSite, levels = c("H", "A", "M"))) %>% 
  ggplot(aes(x = meanTair, y = mean, color = Treatment)) +
  geom_jitter() +
  stat_smooth(method = "lm", formula = "y ~ x") +
  facet_grid(~ pheno.stage)



# Cumulative Temperature
MeanSE <- cumT %>% 
  filter(pheno.var == "peak", pheno.stage == "Bud", is.na(cumTemp)) %>% 
  group_by(year, newTT, origSite, pheno.stage, pheno.var, species, depth) %>% 
  summarise(N = sum(!is.na(cumTemp)), mean = mean(cumTemp, na.rm = TRUE), se = sd(cumTemp, na.rm = TRUE)/sqrt(N))

# Calculate mean for difference between Control and Treatment
#SPOnlyInOneTreatment
SpeciesDifference <- MeanSE %>% 
  ungroup() %>% 
  select(-N) %>%  # remove site, because it causes problems
  unite(united, mean, se, sep = "_") %>% # unite mean and se
  spread(key = newTT, value = united) %>% # spread Treatments
  separate(col = Control, into = c("Control_mean", "Control_se"), sep = "_", convert = TRUE) %>% 
  separate(col = OTC, into = c("OTC_mean", "OTC_se"), sep = "_", convert = TRUE) %>% 
  separate(col = Warm, into = c("Warm_mean", "Warm_se"), sep = "_", convert = TRUE) %>% 
  separate(col = Cold, into = c("Cold_mean", "Cold_se"), sep = "_", convert = TRUE) %>% 
  mutate(OTC_mean = OTC_mean - Control_mean, Warm_mean = Warm_mean - Control_mean, Cold_mean = Cold_mean - Control_mean) %>% 
  mutate(OTC_se = sqrt(Control_se^2 + OTC_se^2), Warm_se = sqrt(Control_se^2 + Warm_se^2), Cold_se = sqrt(Control_se^2 + Cold_se^2)) %>% 
  select(-Control_mean, -Control_se) %>% 
  unite(OTC, OTC_mean, OTC_se, sep = "_") %>% 
  unite(Warm, Warm_mean, Warm_se, sep = "_") %>% 
  unite(Cold, Cold_mean, Cold_se, sep = "_") %>% 
  gather(key = Treatment, value = united, -year, -origSite, -pheno.stage, -pheno.var, -species, -depth) %>%
  separate(col = united, into = c("mean", "se"), sep = "_", convert = TRUE) %>% 
  filter(!is.na(mean))


SpeciesDifference %>% 
  filter(year == 2017, pheno.var == "peak", pheno.stage != "Ripe", depth == "ground") %>% 
  mutate(Treatment = plyr::mapvalues(Treatment, c("Cold", "OTC", "Warm"), c("Transplant Cold", "OTC", "Transplant Warm"))) %>% 
  mutate(Treatment = factor(Treatment, levels = c("OTC", "Transplant Warm", "Transplant Cold"))) %>% 
  mutate(origSite = plyr::mapvalues(origSite, c("A", "H", "M"), c("Alpine", "High alpine", "Mid"))) %>% 
  mutate(origSite = factor(origSite, levels = c("High alpine", "Alpine", "Mid"))) %>% 
  mutate(Order = paste(Treatment, origSite, species, sep = "_")) %>% 
  ggplot(aes(y = mean, x = species, fill = Treatment, ymin = mean - se, ymax = mean + se)) +
  geom_col(position="dodge", width = 0.7) +
  geom_errorbar(position = position_dodge(0.7), width = 0.2) +
  geom_hline(yintercept = 0, colour = "grey", linetype = 2) +
  scale_fill_manual(name = "", values = c(rep("purple", 1), rep("orange", 1), rep("lightblue", 1))) +
  #scale_x_discrete(labels = SP) +
  labs(y = "Difference between treatment and control in days", x = "", title = "peak") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_grid(pheno.stage ~ Treatment * origSite, scales = "free_x", space = "free_x")



#### ANALYSIS ####
library("MuMIn")
#  change the default "na.omit" to prevent models from being fitted to different datasets in case of missing values.
options(na.action = "na.fail") # can also be put in the model
options(na.action = "na.omit") # change back
# Alternatively put it in the modle


# Function for model selection
ModelSelection <- function(dat){
  # fit model
  fit1 <- lmer(value ~ newTT + origSite + (1|species) + (1|block), dat)
  
  # Model selection using dredge
  
  model.set <- dredge(fit1, rank = "AICc", extra = "R^2")
  
  res <- data.frame(model.set)
  res$cumsum <- cumsum(res$weight)
  
  res <- res %>% 
    select(-logLik, -AICc) %>% 
    rename(R.square = R.2, delta.AIC = delta, akaikeWeight = weight) %>% 
    mutate(R.square = round(R.square, 3), delta.AIC = round(delta.AIC, 3), akaikeWeight = round(akaikeWeight, 3), cumsum = round(cumsum, 3))
    
    res
}



res <- phenology %>% 
  filter(year == "2017", pheno.stage != "Ripe") %>% 
  group_by(year, pheno.stage, pheno.var) %>% 
  select(value, newTT, species, origSite, block) %>% 
  droplevels() %>% 
  do(ModelSelection(.))

res %>%
  group_by(year, pheno.stage, pheno.var) %>%
  filter(cumsum < 0.95)


# test examples
ddd <- phenology %>% 
  filter(year == "2017", pheno.stage == "Seed", pheno.var == "duration")

fit1 <- lmer(value ~ newTT + origSite + (1|species) + (1|block), ddd)
summary(fit1)
ModelCheck(fit1)

model.set <- dredge(fit1, rank = "AICc", extra = "R^2")

mm <- data.frame(model.set)
mm$cumsum <- cumsum(mm$weight)
mm95 <- mm %>% filter(cumsum < 0.95)
averaged.model <- model.avg(model.set, cumsum(weight) <= percent.thresh)
res <- data.frame(summary(averaged.model)$coefmat.full)




# backtransform the data to get doy
newdat <- with(myData, expand.grid(
  newTT = c("Control", "OTC", "Warm", "Cold"),
  origSite = c("H", "A", "M"),
  value = 0
))


newdat$value <- predict(fit3, newdat, re.form = NA) # , type="response"
mm <- model.matrix(terms(fit3), newdat)

## or newdat$distance <- mm %*% fixef(fit1)

pvar1 <- diag(mm %*% tcrossprod(vcov(fit1), mm))
tvar1 <- pvar1 + VarCorr(fit1)$block[1]  ## must be adapted for more complex models
cmult <- 1.96 ## could use 1.96

newdat <- newdat %>% 
  mutate(value = exp(value)) %>% 
  mutate(plow = exp(value - cmult * sqrt(pvar1))) %>% 
  mutate(phigh = exp(value + cmult * sqrt(pvar1))) %>% 
  mutate(tlow = exp(value - cmult * sqrt(tvar1))) %>% 
  mutate(thigh = exp(value + cmult * sqrt(tvar1)))

#plot confidence
g0 <- ggplot(newdat, aes(x = newTT, y = value, ymin = plow, ymax = phigh, colour = species)) + 
  geom_point() +
  geom_errorbar(colour = "red") +
  facet_wrap(~ species)



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
