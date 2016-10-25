# Import climate data

load("clean_weather.Rdata", verbose = TRUE)

head(distinct_weather)

ggplot(distinct_weather, aes(x = dateTime, y = Tsoil20, color = site)) +
  geom_line() +
  facet_wrap(~site)

### Calculate Daily Temperature
climate <- distinct_weather %>% 
  filter(!is.na(dateTime) & !is.na(Tair) & !is.na(Tsoil20)) %>% # remove NA
  select(site, dateTime, Tair, Tsoil20) %>% 
  gather(key = "variable", value = "value", -site, -dateTime) %>% 
  #mutate(dateHour = paste(format(dateTime, "%Y-%m-%d"), hour(dateTime), sep = "_")) %>% # for hourly data
  mutate(dateDaily = format(dateTime, "%Y-%m-%d")) %>%
  group_by(site, dateDaily, variable) %>% 
  summarise(n = n(), mean = mean(value))


# Subset data for 2016, replace values below 5°C with 0, get cumsum
CumSum2016 <- climate %>% 
  filter(year(ymd(dateDaily)) >= 2016) %>% # only 2016 data
  mutate(mean = ifelse(mean < 5, 0, mean)) %>%  # replace temperature below 5°C with 0
  mutate(doy = yday(ymd(dateDaily))) %>% # calculate day of the year
  group_by(variable, site) %>% 
  mutate(cumTemp = cumsum(mean)) %>% 
  mutate(climateID = paste(site, doy, sep = "_"))

CumSum2016 %>% 
  filter(site != "L") %>% 
  ggplot(aes(x = ymd(dateDaily), y = cumTemp, color = site)) +
  geom_line() +
  facet_wrap(~variable)


climate %>% 
  filter(site == "H", variable == "Tair") %>% 
  ggplot(aes(x = ymd(dateDaily), y = mean, color = site)) +
  geom_line() +
  facet_wrap(~site)
