
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
                    Year = 2016, 
                    phenostage = "Bud", 
                    phenovar = "duration", 
  
                    # Running analysis
                    niter = 100000, 
                    nburn = 50000, nthin = 5,
                    nchain = 3, 
                    mod = "NormalRegression.R")


#------------------------------------------------------------------------------
# OUTPUT


load(file = "ModelOutput/mod2016Budduration.RData", verbose = TRUE)
res2

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
