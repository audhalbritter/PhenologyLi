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


monthlyiButton %>% 
  filter(depth == "air") %>% 
  mutate(destSite = site) %>% 
  rename(origSite = site) %>% 
  bind_rows(WarmAirT, ColdAirT) %>%
  gather(key = Temperature, value = value, Tmean) %>%
  mutate(destSite = factor(destSite, levels = c("H", "A", "M", "L"))) %>% 
  mutate(treatment = factor(treatment, levels = c("C", "OTC", "Transplant warm", "Transplant cold"))) %>% 
  ggplot(aes(x = month, y = value, color = treatment)) +
  geom_line() +
  scale_color_manual(name = "Treatment", values = c("grey", "purple", "orange", "lightblue")) +
  labs(x = "", y = "Mean monthly temperature °C") +
  facet_grid(Temperature ~ destSite, scales = "free") +
  theme_minimal()

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

#