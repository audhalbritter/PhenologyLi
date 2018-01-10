load(file = "~/Dropbox/Bergen/Transplant - China/Temperature_monthlyiButton.RData", verbose = TRUE)
load(file = "~/Dropbox/Bergen/Transplant - China/TemperatureiButton.RData", verbose = TRUE)

WarmAirT <- monthlyiButton %>% 
  filter(depth == "air", treatment == "C", site != "H") %>%
  mutate(treatment = "Transplant warm") %>%
  rename(origSite = site) %>% 
  mutate(destSite = origSite) %>% 
  mutate(destSite = plyr::mapvalues(destSite, c("A", "M"), c("H", "A")))

ColdAirT <- monthlyiButton %>% 
  filter(depth == "air", treatment == "C", site != "L") %>%
  mutate(treatment = "Transplant cold") %>% 
  rename(origSite = site) %>% 
  mutate(destSite = origSite) %>% 
  mutate(destSite = plyr::mapvalues(destSite, c("H", "A", "M"), c("A", "M", "L")))


iButtonTreatmentPlot <- monthlyiButton %>% 
  filter(depth == "air") %>% 
  mutate(destSite = site) %>% 
  rename(origSite = site) %>% 
  bind_rows(WarmAirT, ColdAirT) %>%
  gather(key = Temperature, value = value, Tmean) %>%
  filter(destSite != "L") %>% 
  mutate(destSite = plyr::mapvalues(destSite, c("H", "A", "M"), c("High - 4130m a.s.l.", "Middle - 3800m a.s.l.", "Low - 3500m a.s.l."))) %>% 
  mutate(destSite = factor(destSite, levels = c("High - 4130m a.s.l.", "Middle - 3800m a.s.l.", "Low - 3500m a.s.l."))) %>% 
  mutate(treatment = factor(treatment, levels = c("C", "OTC", "Transplant warm", "Transplant cold"))) %>% 
  ggplot(aes(x = month, y = value, color = treatment)) +
  geom_line() +
  scale_color_manual(name = "Treatment", values = c("grey", "purple", "orange", "lightblue")) +
  labs(x = "", y = "Mean monthly temperature °C") +
  facet_grid(Temperature ~ destSite, scales = "free") +
  theme_minimal()

ggsave(iButtonTreatmentPlot, file = "Figures/iButtonTreatmentPlot.pdf", width = 8, height = 3)
ggsave(iButtonTreatmentPlot, file = "Figures/iButtonTreatmentPlot.jpeg", width = 8, height = 3, dpi = 300)

#### monthly iButton data for each treatment ####
Warm <- monthlyiButton %>% 
  filter(depth == "air", treatment == "C", !site %in% c("H", "L")) %>%
  mutate(treatment = "Warm")

Cold <- monthlyiButton %>% 
  filter(depth == "air", treatment == "C", !site %in% c("M", "L")) %>%
  mutate(treatment = "Cold")

origSite <- data_frame(origSite = c("A", "H", "H", "M", "A", "A", "H", "M", "M", "A"))

AirTemp <- monthlyiButton %>% 
  filter(depth == "air", site != "L") %>% 
  bind_rows(Warm, Cold) %>% 
  mutate(site = factor(site, levels = c("H", "A", "M", "L"))) %>% 
  mutate(treatment = plyr::mapvalues(treatment, c("C", "OTC", "Warm", "Cold"), c("Control", "OTC", "Warm", "Cold"))) %>% 
  rename(destSite = site, newTT = treatment) %>% 
  group_by(destSite, newTT) %>% 
  summarise(meanTair = mean(Tmean)) %>% 
  mutate(meanTair = round(meanTair, 2)) %>% 
  ungroup() %>% 
  select(-destSite) %>% 
  bind_cols(origSite) %>% 
  spread(key = newTT, value = meanTair) %>% 
  mutate(Cold = Cold - Control, Warm = Warm - Control, OTC = OTC - Control) %>% 
  select(-Control) %>% 
  gather(key = newTT, value = meanTair, -origSite) %>% 
  filter(!is.na(meanTair))



#### iButton data ####
dailyiButton <- iButton %>%
  filter(site != "L") %>% 
  mutate(date = dmy(format(date, "%d.%b.%Y"))) %>%
  mutate(treatment = plyr::mapvalues(treatment, c("C", "OTC"), c("Control", "OTC"))) %>% 
  group_by(date, depth, site, treatment) %>%
  summarise(n = n(), mean = mean(value)) %>%
  filter(n > 100) %>%
  select(-n) %>% 
  mutate(mean0 = ifelse(mean < 0, 0, mean)) %>%  # replace temperature below 5°C with 0
  mutate(doy = yday(ymd(date))) %>% # calculate day of the year
  group_by(site, depth, treatment) %>% 
  mutate(cumTemp = cumsum(mean0)) %>% 
  select(-date, -mean, -mean0)

cumT <- phenology %>% 
  filter(year == "2017") %>% 
  mutate(newTT2 = plyr::mapvalues(newTT, c("Warm", "Cold", "Control", "OTC"), c("Control", "Control", "Control", "OTC"))) %>% 
  left_join(dailyiButton, by = c("destSite" = "site", "newTT2" = "treatment", "value" = "doy"))
