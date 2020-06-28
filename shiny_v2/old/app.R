#0===========================================================
#<parameter>=================================================
options(stringsAsFactors = FALSE)
options(scipen = 999)
library(magrittr)
library(tidyverse)
library(ggplot2)
library(gganimate) 
library(gifski)
library(plotly)
library(mefa4)
library(dplyr)
library(gapminder)
library(countrycode)
library(tmap)
library(sp)
library(rworldmap)
library(flexdashboard)
library(shiny)
library(shinydashboard)
library(leaflet)
library(RColorBrewer)
library(highcharter)
library(shinyWidgets)

coords2continent = function(points){  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # converting points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  #indices$continent   # returns the continent (6 continent model)
  indices$REGION   # returns the continent (7 continent model)
  #indices$ADMIN  #returns country name
  #indices$ISO3 # returns the ISO3 code 
}

#setwd('C:\\Users\\Taner\\Dropbox\\covid_19\\shiny_web-master')
#1===========================================================
#<data import>===============================================

df <- read_csv(url('https://covid.ourworldindata.org/data/ecdc/full_data.csv'))
#test_df <- read_csv(url('https://covid.ourworldindata.org/data/owid-covid-data.csv'))
str(df)

data_date <- df$date %>% unique()
location <- df$location %>% unique()

Last_update_date = max(data_date)
min_date = min(data_date)
break_out_num <- as.numeric(Last_update_date - min_date)

temp_df <- expand.grid(data_date,location)
names(temp_df) <- c('date','location')
temp_df <- merge.data.frame(x = temp_df,y = df,
                            by = c('date','location'),all.x = T)
#???a?ϡB???��????Ƹɻ?
temp_df$new_cases[is.na(temp_df$new_cases)] <- 0
temp_df$new_deaths[is.na(temp_df$new_deaths)] <- 0
temp_df <- temp_df %>% dplyr::arrange(location,date)
temp_df$location <- as.character(temp_df$location)

total_cases_is_na <- temp_df[["total_cases"]] %>% is.na()
total_deaths_is_na <- temp_df[["total_deaths"]] %>% is.na()

for (string in location){
  is_location <- (temp_df[["location"]] == string)
  
  total_cases_na_index <- which(total_cases_is_na & is_location)
  total_cases_not_na_index <- which((!total_cases_is_na) & is_location)
  
  for (i in total_cases_na_index){
    
    index_evctor <- which(total_cases_not_na_index < i)
    
    if (length(index_evctor) > 0){
      
      input_index <- max(index_evctor)
      input_index <- total_cases_not_na_index[input_index]
      
      temp_df[['total_cases']][i] <- temp_df[['total_cases']][input_index] + temp_df[["new_cases"]][input_index+1]
    }else{
      temp_df[['total_cases']][i] <- 0
    }
  }
}

for (string in location){
  is_location <- (temp_df[["location"]] == string)
  
  total_deaths_na_index <- which(total_deaths_is_na & is_location)
  total_deaths_not_na_index <- which((!total_deaths_is_na) & is_location)
  
  for (i in total_deaths_na_index){
    
    index_evctor <- which(total_deaths_not_na_index < i)
    
    if (length(index_evctor) > 0){
      
      input_index <- max(index_evctor)
      input_index <- total_deaths_not_na_index[input_index]
      
      temp_df[['total_deaths']][i] <- temp_df[['total_deaths']][input_index] + temp_df[["new_deaths"]][input_index+1]
    }else{
      temp_df[['total_deaths']][i] <- 0
    }
  }
}

rm(df)
rm(index_evctor,i,string,input_index,is_location)
rm(total_cases_is_na,total_cases_na_index,total_cases_not_na_index)
rm(total_deaths_is_na,total_deaths_na_index,total_deaths_not_na_index)

world_situation <- temp_df %>% dplyr::filter(location == 'World')
area_situation <- temp_df %>% dplyr::filter(location != 'World')
rm(temp_df)

world_situation_final <- world_situation %>% dplyr::filter(date == Last_update_date)
area_situation_final <- area_situation %>% dplyr::filter(date == Last_update_date)

total_confirmed_top10 <- area_situation_final %>%
  dplyr::arrange(-total_cases) %>% head(10) %>%
  dplyr::select(date,location,total_cases)

total_death_top10 <- area_situation_final %>%
  dplyr::arrange(-total_deaths) %>% head(10) %>%
  dplyr::select(date,location,total_deaths)

total_confirmed_top10$location <- factor(total_confirmed_top10$location,levels = total_confirmed_top10$location[order(total_confirmed_top10$total_cases, decreasing = FALSE)])
total_death_top10$location <- factor(total_death_top10$location,levels = total_death_top10$location[order(total_death_top10$total_deaths, decreasing = FALSE)])
#
long_lat <- read.csv('all_area_location.csv')
area_situation <- merge.data.frame(x = area_situation,y = long_lat,by = 'location',all.x = T)
rm(long_lat)
#?U?w?ڤ??ߧ???
mid_location_data <- data.frame(continent = c('Africa','Americas',
                                              'Asia','Oceania','Europe'),
                                lat = c(11.50243,19.19829,
                                        25.20870,-18.31280,51.00000),
                                lon = c(17.75781,-99.54868,
                                        89.23437,138.51560,10.00000))

