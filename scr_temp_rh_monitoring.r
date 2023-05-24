
library(tidyverse)
library(janitor)
library(here)
library(solarPos)

fixDate <- function(dataframe)
{
  year = as.numeric(substr(dataframe$date_time_pdt, 7,10))
  month = as.numeric(substr(dataframe$date_time_pdt, 1,2))
  day = as.numeric(substr(dataframe$date_time_pdt, 4,5))
  hour = as.numeric(substr(dataframe$date_time_pdt, 12,13))
  minute = as.numeric(substr(dataframe$date_time_pdt, 15,16))
  second = as.numeric(substr(dataframe$date_time_pdt, 18,19))

  dataframe <- dataframe %>%
    mutate(time = lubridate::ymd_hms(paste(year,"-",month,"-",day," ",
                                                    hour,":",minute,":",second, sep=""))) 
  return(dataframe)
}

# Load a Hobo log
#  Choose only appropriate columns (sample index, timestamp, temp, relative humidity, dew point)
loadHoboLog <- function(filename)
{
  temp_rh <- read_csv(here::here("scr_hobos",
                              filename)) %>% 
    dplyr::select(1:5) %>%
    janitor::clean_names() %>% 
    drop_na() %>%
    fixDate() %>%
    mutate(site = as.numeric(substr(filename, 5,7)))
  names(temp_rh) = c("index",
                     "date_time",
                     "temperature",
                     "rel_hum",
                     "dew_point",
                     "time",
                     "site")
  
  return(temp_rh)
}

# File List
file_list <- list.files(here::here("scr_hobos"))

# Load all data
temp_rh <- lapply(file_list, loadHoboLog)

# Collapse all data into one dataframe
temp_rh <- bind_rows(temp_rh)

# Add new fields for time and solar angles
temp_rh$year <- lubridate::year(as.Date(temp_rh$time))
temp_rh$month <- lubridate::month(as.Date(temp_rh$time))
temp_rh$day <- lubridate::day(as.Date(temp_rh$time))
temp_rh$hour <- lubridate::hour(temp_rh$time) + 8
temp_rh$minute <- lubridate::minute(temp_rh$time)
temp_rh$second <- lubridate::second(temp_rh$time)
temp_rh$solar_zenith <- solarPosition(julianDay(temp_rh$year, temp_rh$month, temp_rh$day, temp_rh$hour, temp_rh$minute, temp_rh$second), -119.85, 34.42)[,1]
temp_rh$solar_azimuth <- solarPosition(julianDay(temp_rh$year, temp_rh$month, temp_rh$day, temp_rh$hour, temp_rh$minute, temp_rh$second), -119.85, 34.42)[,2]

# Some basic plots
ggplot(temp_rh %>% filter(month == 4,
                          day %in% 25:27)) + 
  geom_line(aes(x=date_time, y=temperature,
                group=site, col=as.factor(site), linetype = as.factor(site))) + 
  geom_point(aes(x=date_time, y=solar_zenith/4 + 40), col="black")

