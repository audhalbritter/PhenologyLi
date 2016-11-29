MeanSE %>% filter(treatment %in% c("Control", "Warm")) %>% 
  ggplot(aes(x = treatment, y = mean, color = origSite, group = origSite)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ pheno.stage)


library("cowplot")
# Get Mean and SE
head(pheno.long)
MeanSE <- pheno.long %>% 
  filter(!newtreat %in% c("Cold"), pheno.var == "peak", pheno.unit == "doy", !pheno.stage %in% c("Ripe", "SeedRipe")) %>% 
  group_by(newtreat, origSite, pheno.stage, species) %>% 
  summarise(N = sum(!is.na(value)), mean = mean(value, na.rm = TRUE), se = sd(value, na.rm = TRUE)/sqrt(N))

# Calculate difference between Control and Treatment for SE
SEData <- MeanSE %>% 
  ungroup() %>% 
  select(-mean, -N) %>% # remove site, because it causes problems
  spread(key = newtreat, value = se) %>% # spread Treatments
  mutate(Warm = Warm - Control, OTC = OTC - Control) %>% # Difference Treatment - Control
  gather(key = Treatment, value = SE, -origSite, -pheno.stage, -Control, -species) %>% # gather Treatments
  select(SE, origSite, pheno.stage, Treatment, species)

# Calculate difference between Control and Treatment for Mean
#SPOnlyInOneTreatment
MeanData <- MeanSE %>% 
  ungroup() %>% 
  select(-se, -N) %>% # remove site, because it causes problems
  spread(key = newtreat, value = mean) %>% # spread Treatments
  mutate(Warm = Warm - Control, OTC = OTC - Control) %>% # Difference Treatment - Control
  gather(key = Treatment, value = Difference, -origSite, -pheno.stage, -Control, -species) %>% # gather Treatments
  mutate(newname = paste(origSite, Treatment, sep = "_")) %>% # paste Treatment and site, they are unique and can be renamed
  mutate(newname = plyr::mapvalues(newname, c("H_OTC", "A_OTC", "H_Warm", "A_Warm"), c("High alpine OTC", "Alpine OTC", "High alpine Warm", "Alpine Warm"))) %>% 
  mutate(newname = factor(newname, levels = c("High alpine OTC", "Alpine OTC", "High alpine Warm", "Alpine Warm"))) %>% 
  left_join(SEData, by = c("origSite" = "origSite", "pheno.stage" = "pheno.stage", "Treatment" = "Treatment", "species" = "species")) %>%   # join SW
  #filter(is.na(Difference))
  select(-Control) %>% 
  filter(!is.na(Difference)) %>% 
  group_by(origSite, pheno.stage, Treatment, newname) %>% 
  summarise(Difference = mean(Difference, na.rm = TRUE), SE = mean(SE, na.rm = TRUE))

DifferencePlot <- ggplot(MeanData, aes(x = newname, y = Difference, color = Treatment, shape = Treatment, ymax = Difference + SE, ymin = Difference - SE)) +
  geom_hline(yintercept=0, color = "gray") +
  geom_point(size = 1.8) +
  labs(x = "", y = "Treatment - control in days") +
  scale_colour_manual(name = "", values = c("red", "purple")) +
  scale_shape_manual(name = "", values = c(16, 17)) +
  facet_grid(~ pheno.stage) +
  geom_errorbar(width=0.2) +
  ggtitle("Peak of phenological stage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(DifferencePlot)
save_plot("DifferencePlot_Doy.jpeg", DifferencePlot,base_aspect_ratio = 1.8)

pheno.long %>% 
  filter(pheno.stage == "Flower", pheno.var == "peak", newtreat %in% c("Control", "OTC"), origSite == "A") %>% 
  ggplot(aes(x = newtreat, y = value, color = species, group = species)) + geom_point() + geom_line()


table(SPOnlyInOneTreatment$species, SPOnlyInOneTreatment$pheno.stage)
hist(SPOnlyInOneTreatment$Control)



## ----EventPlot
### PLOT FOR ONSET AND DURATION OF EVENTS
EventMeanAndSE <- pheno.long %>% 
  filter(!newtreat %in% c("Cold"), pheno.var == "peak", pheno.stage != "Ripe", pheno.stage != "SeedRipe") %>% 
  #filter(pheno.unit != "dogs") %>% 
  mutate(pheno.stage = plyr::mapvalues(pheno.stage, c("Bud", "Flower", "Seed", "BudFlower", "FlowerSeed"), c("Bud", "Flower", "Seed", "Flower", "Seed"))) %>%
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Seed"))) %>%
  mutate(orig = plyr::mapvalues(origSite, c("H", "A"), c("High-alpine", "Alpine"))) %>%
  mutate(orig = factor(orig, levels = c("High-alpine", "Alpine"))) %>%
  group_by(newtreat, origSite, pheno.stage, pheno.unit) %>% 
  summarise(N = sum(!is.na(value)), mean = mean(value, na.rm = TRUE), se = sd(value, na.rm = TRUE)/sqrt(N))

EventSE <- EventMeanAndSE %>% 
  select(-mean, -N) %>% # remove mean
  spread(key = pheno.unit, value = se)

EventData <- EventMeanAndSE %>% 
  select(-se, -N) %>% # remove mean
  spread(key = pheno.unit, value = mean) %>% 
  left_join(EventSE, by = c("origSite" = "origSite", "pheno.stage" = "pheno.stage", "newtreat" = "newtreat"),  suffix = c(".mean", ".se"))  # join SE


OnsetDurationEventPlot <- ggplot(EventData, aes(x = doy.mean, y = days.mean, shape = pheno.stage, color = newtreat, group = newtreat, ymax = days.mean + days.se, ymin = days.mean - days.se, xmax = doy.mean + doy.se, xmin = doy.mean - doy.se)) +
  scale_colour_manual(name = "Treatment", values = c("grey", "red", "purple")) +
  scale_shape_manual(name = "Stage", values = c(16,17,15)) +
  labs(x = "Peak of stage in day of the year", y = "Duration between peak of stages in days") +
  geom_errorbar(width=0.18) +
  geom_errorbarh() +
  geom_line(linetype="dashed") +
  geom_point(size = 2) +
  facet_grid(~ origSite)

save_plot("OnsetDurationEventPlotDOGS.jpeg", OnsetDurationEventPlot, base_aspect_ratio = 2)

