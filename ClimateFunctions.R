##### FUNCTIONS FOR CLIMATE DATA ####

# Function to calculate daily Temperature
CaclulateDailyValues <- function(dd){
  dd2 <- dd %>% 
    filter(!is.na(dateTime)) %>% # remove NA in dateTime
    mutate(dateDaily = ymd(format(dateTime, "%Y-%m-%d"))) %>%
    group_by(site, dateDaily, logger, location) %>% 
    summarise(n = n(), value = mean(value, na.rm = TRUE)) %>% 
    filter(!n < 144)
}