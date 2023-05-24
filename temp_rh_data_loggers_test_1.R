
library(tidyverse)
library(janitor)
library(here)

post_light_100 <- readxl::read_excel(here::here("data",
                                      "home_post _light_100 2023-03-17 20_32_48 PDT (Data PDT).xlsx")) %>% 
  janitor::clean_names()
post_dark_40 <- readxl::read_excel(here::here("data",
                                              "home_post_dark 40 2023-03-17 20_31_41 PDT (Data PDT).xlsx")) %>% 
  janitor::clean_names()
post_dark_100 <- readxl::read_excel(here::here("data",
                                                "home_post_dark_100 2023-03-17 20_33_12 PDT (Data PDT).xlsx")) %>% 
  janitor::clean_names()
post_dark_210 <- readxl::read_excel(here::here("data",
                                                "home_post_dark_210 2023-03-17 20_31_59 PDT (Data PDT).xlsx")) %>% 
  janitor::clean_names()

tree_dark_100 <- readxl::read_excel(here::here("data",
                                               "home_tree_dark_100 2023-03-17 20_32_29 PDT (Data PDT).xlsx")) %>% 
  janitor::clean_names()
tree_dark_170 <- readxl::read_excel(here::here("data",
                                               "home_tree_dark_170 2023-03-17 20_33_01 PDT (Data PDT).xlsx")) %>% 
  janitor::clean_names()

garden_dark_38 <- readxl::read_excel(here::here("data",
                                                "garden_bottom 2023-03-18 12_34_33 PDT (Data PDT).xlsx")) %>% 
  janitor::clean_names()
garden_dark_62 <- readxl::read_excel(here::here("data",
                                                   "garden_top 2023-03-18 12_34_43 PDT (Data PDT).xlsx")) %>% 
  janitor::clean_names()

# Combine all the data into one dataframe
all_data <- post_dark_40
names(all_data) <- c("number","time","tapd_40","rhpd_40","dppd_40")
addNewDataSeries <- function(x, y, name, dataframe)
{
  dataframe[,name] <- approx(x, y, dataframe$time)[[2]]
  return(dataframe)
}
all_data <- addNewDataSeries(post_dark_100$date_time_pdt, post_dark_100$ch_1_temperature_f, "tapd_100", all_data)
all_data <- addNewDataSeries(post_dark_100$date_time_pdt, post_dark_100$ch_2_rh_percent, "rhpd_100", all_data)
all_data <- addNewDataSeries(post_dark_100$date_time_pdt, post_dark_100$dew_point_f, "dppd_100", all_data)
all_data <- addNewDataSeries(post_dark_210$date_time_pdt, post_dark_210$ch_1_temperature_f, "tapd_210", all_data)
all_data <- addNewDataSeries(post_dark_210$date_time_pdt, post_dark_210$ch_2_rh_percent, "rhpd_210", all_data)
all_data <- addNewDataSeries(post_dark_210$date_time_pdt, post_dark_210$dew_point_f, "dppd_210", all_data)
all_data <- addNewDataSeries(post_light_100$date_time_pdt, post_light_100$ch_1_temperature_f, "tapl_100", all_data)
all_data <- addNewDataSeries(post_light_100$date_time_pdt, post_light_100$ch_2_rh_percent, "rhpl_100", all_data)
all_data <- addNewDataSeries(post_light_100$date_time_pdt, post_light_100$dew_point_f, "dppl_100", all_data)
all_data <- addNewDataSeries(tree_dark_100$date_time_pdt, tree_dark_100$ch_1_temperature_f, "tatd_100", all_data)
all_data <- addNewDataSeries(tree_dark_100$date_time_pdt, tree_dark_100$ch_2_rh_percent, "rhtd_100", all_data)
all_data <- addNewDataSeries(tree_dark_100$date_time_pdt, tree_dark_100$dew_point_f, "dptd_100", all_data)
all_data <- addNewDataSeries(tree_dark_170$date_time_pdt, tree_dark_170$ch_1_temperature_f, "tatd_170", all_data)
all_data <- addNewDataSeries(tree_dark_170$date_time_pdt, tree_dark_170$ch_2_rh_percent, "rhtd_170", all_data)
all_data <- addNewDataSeries(tree_dark_170$date_time_pdt, tree_dark_170$dew_point_f, "dptd_170", all_data)
all_data$year <- lubridate::year(as.Date(all_data$time))
all_data$month <- lubridate::month(as.Date(all_data$time))
all_data$day <- lubridate::day(as.Date(all_data$time))
all_data$hour <- lubridate::hour(all_data$time) + 8
all_data$minute <- lubridate::minute(all_data$time)
all_data$second <- lubridate::second(all_data$time)
all_data$solar_zenith <- solarPosition(julianDay(all_data$year, all_data$month, all_data$day, all_data$hour, all_data$minute, all_data$second), -119.85, 34.42)[,1]
all_data$solar_azimuth <- solarPosition(julianDay(all_data$year, all_data$month, all_data$day, all_data$hour, all_data$minute, all_data$second), -119.85, 34.42)[,2]

garden_data <- garden_dark_38
names(garden_data) <- c("number","time","tagd_38","rhgd_38","dpgd_38")
garden_data <- addNewDataSeries(garden_dark_62$date_time_pdt, garden_dark_62$ch_1_temperature_f, "tagd_62", garden_data)
garden_data <- addNewDataSeries(garden_dark_62$date_time_pdt, garden_dark_62$ch_2_rh_percent, "rhgd_62", garden_data)
garden_data <- addNewDataSeries(garden_dark_62$date_time_pdt, garden_dark_62$dew_point_f, "dpgd_62", garden_data)

# Get visualization
#  For post by height
#   Absolute
#    note - added in solar zenith to help with understanding of radiative input 
ggplot(all_data) + 
  geom_line(aes(x=time, y=tapd_40)) + 
  geom_line(aes(x=time, y=tapd_100), col="red")  + 
  geom_line(aes(x=time, y=tapd_210), col="blue") + 
  geom_line(aes(x=time+3600, y=-solar_zenith/3+90), col="orange") + 
  geom_hline(yintercept=60)
#   Difference
ggplot(all_data) + 
  geom_line(aes(x=time, y=tapd_100-tapd_40)) + 
  geom_line(aes(x=time, y=tapd_210-tapd_40), col="red") +
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
