
#------------------------------------------------------------------------------
# PREPARE AND LOAD LIBRARIES AND DATA

graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

# load libraries
library("tidyverse")
library("rjags")
library("R2jags")
library("lme4")

load(file = "Phenology.RData")
source("BayesianModelFunction.R")

#------------------------------------------------------------------------------
# RUN ANALYSIS

RunBayesianAnalysis(# Data input
                    dat = phenology, 
                    Year = 2017, 
                    phenostage = "Bud", 
                    phenovar = "peak", 
  
                    # Running analysis
                    niter = 100000, 
                    nburn = 50000, 
                    nthin = 5,
                    nchain = 3, 
                    mod = "NormalRegression.R")


#------------------------------------------------------------------------------
# OUTPUT

# PEAK
load(file = "ModelOutput/mod2016Budpeak.RData", verbose = TRUE)
Budpeak16 <- res2
load(file = "ModelOutput/mod2016Flowerpeak.RData", verbose = TRUE)
Flpeak16 <- res2
load(file = "ModelOutput/mod2016Seedpeak.RData", verbose = TRUE)
Seedpeak16 <- res2
load(file = "ModelOutput/mod2017Budpeak.RData", verbose = TRUE)
Budpeak17 <- res2
load(file = "ModelOutput/mod2017Flowerpeak.RData", verbose = TRUE)
Flpeak17 <- res2
load(file = "ModelOutput/mod2017Seedpeak.RData", verbose = TRUE)
Seedpeak17 <- res2

PlotTreatmentEffects <- Budpeak16 %>% 
  rbind(Flpeak16, Seedpeak16, Budpeak17, Flpeak17, Seedpeak17) %>%
  filter(grepl("newTT", var)) %>% 
  mutate(signif = ifelse(sign(X2.5.) == sign(X97.5.), 0, 1)) %>% 
  mutate(Shape = paste(year, signif, sep = "_")) %>% 
  ggplot(aes(x = variable, y = X50., ymin = X2.5., ymax = X97.5., color = variable, shape = Shape)) +
  geom_point(size = 5) +
  geom_errorbar(width = 0) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  scale_color_manual(values = c("purple", "orange", "lightblue")) +
  scale_shape_manual(values = c(16, 1, 17)) +
  labs(x = "", y = "Difference between treatment and control (days)") +
  facet_grid(year ~ pheno.stage) +
  theme(legend.position="none", text = element_text(size=18))
ggsave(PlotTreatmentEffects, filename = "Figures/PlotTreatmentEffects.jpeg", height = 6, width = 15, dpi = 300)


Budpeak16 %>% 
  rbind(Flpeak16, Seedpeak16, Budpeak17, Flpeak17, Seedpeak17) %>%
  filter(grepl("diff", var)) %>% 
  ggplot(aes(x = variable, y = X50., ymin = X2.5., ymax = X97.5.)) +
  geom_point() +
  geom_errorbar(width = 0) +
  facet_grid(year ~ pheno.stage) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed")



# DURATION
load(file = "ModelOutput/mod2016Budduration.RData", verbose = TRUE)
Buddur16 <- res2
load(file = "ModelOutput/mod2016Flowerduration.RData", verbose = TRUE)
Fldur16 <- res2
load(file = "ModelOutput/mod2016Seedduration.RData", verbose = TRUE)
Seeddur16 <- res2
load(file = "ModelOutput/mod2017Budduration.RData", verbose = TRUE)
Buddur17 <- res2
load(file = "ModelOutput/mod2017Flowerduration.RData", verbose = TRUE)
Fldur17 <- res2
load(file = "ModelOutput/mod2017Seedduration.RData", verbose = TRUE)
Seeddur17 <- res2

PlotTreatmentEffectsDuration <- Buddur16 %>% 
  rbind(Fldur16, Seeddur16, Buddur17, Fldur17, Seeddur17) %>%
  filter(grepl("newTT", var)) %>% 
  mutate(signif = ifelse(sign(X2.5.) == sign(X97.5.), 0, 1)) %>% 
  mutate(Shape = paste(year, signif, sep = "_")) %>% 
  ggplot(aes(x = variable, y = X50., ymin = X2.5., ymax = X97.5., color = variable, shape = Shape)) +
  geom_point(size = 5) +
  geom_errorbar(width = 0) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  scale_color_manual(values = c("purple", "orange", "lightblue")) +
  scale_shape_manual(values = c(16, 1, 17, 2)) +
  labs(x = "", y = "Difference between treatment and control (days)") +
  facet_grid(year ~ pheno.stage) +
  theme(legend.position="none", text = element_text(size=18))

ggsave(PlotTreatmentEffectsDuration, filename = "Figures/PlotTreatmentEffectsDuration.jpeg", height = 6, width = 15, dpi = 300)


####################################################################################
### RANDOM SLOPE MODEL

load(file = "Phenology.RData")
source("BaysianSlopeModelFunction.R")

#------------------------------------------------------------------------------
# RUN ANALYSIS

RunBayesianAnalysis(# Data input
                    dat = phenology, 
                    Year = 2017, 
                    phenostage = "Bud", 
                    phenovar = "peak", 
  
                    # Running analysis
                    niter = 10000, 
                    nburn = 5000, 
                    nthin = 5,
                    nchain = 3, 
                    mod = "NormalRegression_RandomSlopeSpecies.R")


load(file = "ModelOutput/modelmod2017Budpeak.RData", verbose = TRUE)
res <- data.frame(mod$BUGSoutput$summary)
alpha <- res %>% 
  rownames_to_column(var = "variable") %>% 
  filter(grepl("alpha", variable)) %>% 
  select(mean)

res %>% 
  rownames_to_column(var = "variable") %>% 
  filter(grepl("newTT", variable)) %>% 
  bind_cols(meta) %>% 
  inner_join(dd, by = c("newTT", "species")) %>% 
  mutate(mean = 185.3687 + mean) %>% 
  group_by(newTT) %>% 
  summarise(mean = mean(mean))

dd <- phenology %>% 
  # subset
  filter(year == 2017, pheno.stage == "Bud", pheno.var == "peak") %>% 
  distinct(newTT, species)
  

# meta data
meta <- expand.grid(species = unique(dd$species),
                    newTT = unique(dd$newTT))
meta <- meta %>% 
  arrange(newTT, species)




load(file = "ModelOutput/mod2017Budpeak.RData", verbose = TRUE)
res2 %>% 
  filter(grepl("newTTCoeff[d,1]", variable))


