
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
    mutate(date_time_pdt = lubridate::ymd_hms(paste(year,"-",month,"-",day," ",
                                                    hour,":",minute,":",second, sep=""))) 
  return(dataframe)
}

post_210 <- read_csv(here::here("data",
                                "home_post_215 2023-04-12 17_05_25 PDT (Data PDT).csv")) %>% 
  dplyr::select(1:5) %>%
  janitor::clean_names() %>% 
  drop_na() %>%
  fixDate() 
tree_050 <- read_csv(here::here("data",
                                "home_tree_050 2023-04-12 17_04_43 PDT (Data PDT).csv")) %>% 
  dplyr::select(1:5) %>%
  janitor::clean_names() %>%
  drop_na() %>%
  fixDate()
tree_210 <- read_csv(here::here("data",
                                "home_tree_215 2023-04-12 17_05_09 PDT (Data PDT).csv")) %>% 
  dplyr::select(1:5) %>%
  janitor::clean_names() %>%
  drop_na() %>%
  fixDate()
tree_325 <- read_csv(here::here("data",
                                "home_tree_325 2023-04-12 17_04_55 PDT (Data PDT).csv")) %>% 
  dplyr::select(1:5) %>%
  janitor::clean_names() %>%
  drop_na() %>%
  fixDate()

# Combine all the data into one dataframe
all_data <- tree_325
names(all_data) <- c("number","time","tree_325_temp","tree_325_rh","tree_325_dp")
addNewDataSeries <- function(x, y, name, dataframe)
{
  dataframe[,name] <- approx(x, y, dataframe$time)[[2]]
  return(dataframe)
}
all_data <- addNewDataSeries(tree_050$date_time_pdt, tree_050$ch_1_temperature_f, "tree_050_temp", all_data)
all_data <- addNewDataSeries(tree_050$date_time_pdt, tree_050$ch_2_rh_percent, "tree_050_rh", all_data)
all_data <- addNewDataSeries(tree_050$date_time_pdt, tree_050$dew_point_f, "tree_050_dp", all_data)

all_data <- addNewDataSeries(tree_210$date_time_pdt, tree_210$ch_1_temperature_f, "tree_210_temp", all_data)
all_data <- addNewDataSeries(tree_210$date_time_pdt, tree_210$ch_2_rh_percent, "tree_210_rh", all_data)
all_data <- addNewDataSeries(tree_210$date_time_pdt, tree_210$dew_point_f, "tree_210_dp", all_data)

all_data <- addNewDataSeries(post_210$date_time_pdt, post_210$ch_1_temperature_f, "post_210_temp", all_data)
all_data <- addNewDataSeries(post_210$date_time_pdt, post_210$ch_2_rh_percent, "post_210_rh", all_data)
all_data <- addNewDataSeries(post_210$date_time_pdt, post_210$dew_point_f, "post_210_dp", all_data)

all_data$year <- lubridate::year(as.Date(all_data$time))
all_data$month <- lubridate::month(as.Date(all_data$time))
all_data$day <- lubridate::day(as.Date(all_data$time))
all_data$hour <- lubridate::hour(all_data$time) + 8
all_data$minute <- lubridate::minute(all_data$time)
all_data$second <- lubridate::second(all_data$time)
all_data$solar_zenith <- solarPosition(julianDay(all_data$year, all_data$month, all_data$day, all_data$hour, all_data$minute, all_data$second), -119.85, 34.42)[,1]
all_data$solar_azimuth <- solarPosition(julianDay(all_data$year, all_data$month, all_data$day, all_data$hour, all_data$minute, all_data$second), -119.85, 34.42)[,2]

# Get visualization
#  For post by height
#   Absolute
#    note - added in solar zenith to help with understanding of radiative input 
ggplot(all_data) + 
  geom_line(aes(x=time, y=tree_050_temp), col="magenta") + 
  geom_line(aes(x=time, y=tree_210_temp), col="red")  + 
  geom_line(aes(x=time, y=tree_325_temp), col="blue") + 
  geom_line(aes(x=time, y=post_210_temp), col="black") + 
  geom_line(aes(x=time+3600, y=-solar_zenith/3+90), col="orange") + 
  geom_hline(yintercept=60)
#   Difference
ggplot(all_data) + 
  geom_line(aes(x=time, y=tree_210_temp-tree_050_temp)) + 
  geom_line(aes(x=time, y=tree_325_temp-tree_050_temp), col="red") +
  geom_line(aes(x=time+3600, y=-solar_zenith/3+90), col="orange") + 
  geom_hline(yintercept=0, col="blue")

#  Post dark vs. light
#   Absolute
ggplot(all_data) + 
  geom_line(aes(x=time, y=tapl_100)) + 
  geom_line(aes(x=time, y=tapd_100), col="red")
#   Difference
ggplot(all_data) + 
  geom_line(aes(x=time, y=tapl_100-tapd_100)) +
  geom_hline(yintercept=0, col="red")

# Tree by Height
ggplot(all_data) + 
  geom_line(aes(x=time, y=tatd_100)) + 
  geom_line(aes(x=time, y=tatd_170), col="red")
ggplot(all_data) + 
  geom_line(aes(x=time, y=tatd_100-tatd_170)) +
  geom_hline(yintercept=0, col="red")


# Tree vs. Post
ggplot(all_data) + 
  geom_line(aes(x=time, y=tapd_100)) + 
  geom_line(aes(x=time, y=tatd_100), col="red")
ggplot(all_data) + 
  geom_line(aes(x=time, y=tapd_100-tatd_100)) + 
  geom_hline(yintercept=0, col="red")



# Garden - shaded under tree
ggplot(garden_data) + 
  geom_line(aes(x=time, y=tagd_38)) + 
  geom_line(aes(x=time, y=tagd_62), col="red")
ggplot(garden_data) + 
  geom_line(aes(x=time, y=tagd_62-tagd_38)) + 
  geom_hline(yintercept=0, col="red")
