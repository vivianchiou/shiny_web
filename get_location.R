#0========================================================
#<parameter>==============================================
options(stringsAsFactors = FALSE)
options(scipen = 999)
#library(shiny)
#library(shinydashboard)
#library(shinythemes)
#library(leaflet)
#library(ggplot2)
#library(gganimate) 
#library(gifski)
#library(plotly)
#library(mefa4)
#library(plyr)
#library(gapminder)
library(dplyr)
library(magrittr)
library(tidyverse)
library(sp)
library(rworldmap)
coords2continent = function(points)
{  
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
library(countrycode)
#1========================================================
#<read data>==============================================
df <- read_csv(url('https://covid.ourworldindata.org/data/ecdc/full_data.csv'))
df %>% head()
all_area <- df$location %>% unique()
df$location %>% table()
#2========================================================
#<get location>===========================================
long <- numeric()#ps long <=> x
lat <- numeric()#ps lat <=> y
loop_n <- 0
for (string in all_area){
  loop_n <- loop_n + 1
  print(loop_n)
  temp_location <- tmaptools::geocode_OSM(q = string)
  if (!is.null(temp_location)){
    long <- append(long,temp_location[['coords']][1])
    lat <- append(lat,temp_location[['coords']][2])
  }else{
    long <- append(long,NA)
    lat <- append(lat,NA)
  }
}
#2-extra==================================================
#<get location>===========================================
long %>% is.na() %>% which()#171
all_area[171]
temp_location <- tmaptools::geocode_OSM(q = 'Sint Maarten')
long[171] <- temp_location[['coords']][1]
lat[171] <- temp_location[['coords']][2]
#3========================================================
#<get continent>==========================================
continent <- coords2continent(data.frame(long,lat))
continent <- as.character(continent)
temp_continent <- countrycode(continent[is.na(continent)],
                              origin = "country.name",
                              destination = "continent")
continent[is.na(continent)] <- temp_continent
continent[continent == 'Australia']'Oceania'
continent[continent %in% c('North America','South America')] <- 'Americas'
#continent %>% table(useNA = 'always')
#4========================================================
#<conbind & output>=======================================
output_location <- data.frame(location = all_area,long,lat,continent)
write.csv(x = output_location,file = 'C:\\Users\\Taner\\Desktop\\all_area_location.csv',row.names = F,na = '')
#end======================================================