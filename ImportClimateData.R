# Import climate data

load("clean_weather.Rdata", verbose = TRUE)

head(distinct_weather)

# plot the data
ggplot(distinct_weather, aes(x = dateTime, y = Tair, color = site)) +
  geom_line() +
  facet_wrap(~site)


#### CHECK REGRESSION BETWEEN TAIR IN A AND H SITE
### Regression for 10 min data
HAsite <- distinct_weather %>% 
  select(site, dateTime, Tair) %>% 
  filter(site %in% c("H", "A")) %>% 
  spread(key = site, value = Tair)

fit10 <- lm(H ~ A, data = HAsite)
summary(fit10)
plot(HAsite$A, HAsite$H)
abline(fit10)


### Regression for daily data
### Calculate Daily Temperature
dailyTemperature <- distinct_weather %>% 
  select(site, dateTime, Tair) %>% 
  filter(!is.na(dateTime)) %>% # remove NA in dateTime (2 rows)
  #mutate(dateHour = paste(format(dateTime, "%Y-%m-%d"), hour(dateTime), sep = "_")) %>% # for hourly data
  mutate(dateDaily = ymd(format(dateTime, "%Y-%m-%d"))) %>%
  group_by(site, dateDaily) %>% 
  summarise(n = n(), Tair = mean(Tair)) %>% 
  filter(!n < 144)

HAdaily <- dailyTemperature %>% 
  filter(site %in% c("H", "A")) %>% 
  spread(key = site, value = Tair) %>% 
  na.omit()

plot(HAdaily$A, HAdaily$H)
fitdaily <- lm(H ~ A, data = HAdaily)
summary(fitdaily)
abline(fitdaily)
# using daily values is better

### TO DO
identifier
check data for certain days


#### FILL MISSING VALUES IN H WITH REGRESSION
MissingHValues <- dailyTemperature %>% 
  filter(site %in% c("H", "A")) %>% 
  spread(key = site, value = Tair) %>%
  filter(is.na(H), year(dateDaily) > 2015)
new.df <- data.frame(A = MissingHValues$A)
newH <- predict(fit, new.df)

dailyTemperature %>% 
  mutate(Tair = ifelse(year(dateDaily) > 2015 & is.na(Tair), newH, Tair))
  filter(site %in% c("H", "A")) %>%
  filter(year(dateTime) > 2015, site != "L")

abline(0,1, col = "red")


### CALCULATE CUMSUM
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
