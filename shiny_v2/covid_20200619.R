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
#???U?a?Ϫ?????
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
#2===========================================================
#<ui>========================================================

ui <- navbarPage(title = "Covid-19 dashboard!",
                           header = tagList(
                             useShinydashboard()
                           ),
                 
                 
                tabPanel(title = tags$p(icon('globe'),'Global Situation'),setBackgroundColor("#808080"),
                         fluidRow(
                           #box1:top area
                            box(
                               width = 12,
                               valueBoxOutput(outputId = 'update_date',width = 3),
                               valueBoxOutput(outputId = 'confirmed_cases',width = 3),
                               valueBoxOutput(outputId = 'deaths',width = 3),
                               valueBoxOutput(outputId = 'c',width = 3)
                             ),
                            #box2:left
                             box(title = 'TREND',width = 6,
                                 plotlyOutput(outputId = 'global_line_chart')
                             ),
                            #box3:right
                             box(title = 'TOP 10 AREA',width = 6,
                                 tabsetPanel(type = 'tabs',
                                             tabPanel(title = 'Cumulated Comfirmed',
                                                      plotlyOutput(outputId = 'global_bar_chart_confirmed')),
                                             tabPanel(title = 'Cumulated Deaths',
                                                      plotlyOutput(outputId = 'global_bar_chart_deaths')))
                                 )
                             )
                           ),
                navbarMenu(title = tags$p(icon('chart-line'),'Trand'),
                           tabPanel(title = 'Global',setBackgroundColor("#808080")),
                           tabPanel(title = 'Taiwan',setBackgroundColor("#808080"))),
                
                navbarMenu(title = tags$p(icon('map'),'Map'),
                           tabPanel(title = 'Global',setBackgroundColor("#808080"),
                                    sidebarPanel(width = 2,
                                                 h1('World Map'),
                                                 dateInput(inputId = "obs_day",label = "Date:",
                                                           min = min_date,
                                                           max = Last_update_date,
                                                           value = Last_update_date),
                                                 selectInput(inputId = "continent",
                                                             label = "Continent:",
                                                             choices = c('All','Africa','Americas','Asia','Oceania','Europe'),
                                                             selected = 'All'),
                                                 checkboxInput('legend', 'Legend', value = FALSE, width = NULL),
                                                 h4('hey! in here can select date and continent.'),
                                                 h4('Then it will show the bubble plot in right side.')),
                                    box(width = 10,
                                      leafletOutput(outputId = "global_map",width = "100%", height = "500px")
                                      ),
                                    box(width = 6,plotlyOutput(outputId = 'area_line_chart')),
                                    box(width = 6,plotlyOutput(outputId = 'area_bar_chart'))
                                    ),
                           tabPanel(title = 'Taiwan',setBackgroundColor("#808080")))
                )

#3===========================================================
#<server>====================================================
server <- function(input, output, session) {
  ###first page
  ##top area
  output$update_date <- renderValueBox({ valueBox(value = Last_update_date,
                                                  subtitle = paste0("Outbreak ",break_out_num," days"),color="blue",icon("calendar"))})
  output$confirmed_cases <- renderValueBox({ valueBox(value = prettyNum(world_situation_final$total_cases, big.mark = ","),
                                                      subtitle = tags$p(icon("arrow-up"),"New Cases :",prettyNum(world_situation_final$new_cases, big.mark = ","),style = "font-size: 100%;"),
                                                      color = "red",icon = icon("users"))})
  output$deaths <- renderValueBox({ valueBox(prettyNum(world_situation_final$total_deaths, big.mark = ","),
                                             subtitle = tags$p(icon("arrow-up"),"New Deaths :",prettyNum(world_situation_final$new_deaths, big.mark = ","),style = "font-size: 100%;"),
                                             color = "red", icon = icon("heartbeat"))})
  output$c <- renderValueBox({ valueBox(value = 'need to do something',
                                        subtitle = "Subtitle text",color="blue",icon = icon("hand-holding-medical"))})
  ##sec area
  output$global_line_chart <- renderPlotly({ 
    world_situation %>% 
      plot_ly(x = ~date, y = ~total_cases, name = 'cumulated confirmed cases', color = I('#B22222'),
              type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
              text = ~paste('</br> Date: ', as.character(date),
                            '</br> Confirmed Cases: ', prettyNum(total_cases, big.mark = ","))) %>% 
      add_trace(y = ~total_deaths, name = 'cumulated deaths', color = I('#808080'), mode = 'lines+markers',hoverinfo = 'text',
                text = ~paste('</br> Date: ', as.character(date),
                              '</br> Deaths: ', prettyNum(total_deaths, big.mark = ",")))
  })
  output$global_bar_chart_confirmed <- renderPlotly({ 
    plot_ly(total_confirmed_top10, x = ~total_cases, y = ~location, type = 'bar',hoverinfo = 'text',
            text = ~paste('</br> Area: ', location,
                          '</br> Confirmed Cases: ', prettyNum(total_cases, big.mark = ",")),
            marker = list(color = 'rgba(222,45,38,0.8)'))})
  output$global_bar_chart_deaths <- renderPlotly({ 
    plot_ly(total_death_top10, x = ~total_deaths, y = ~location, type = 'bar',hoverinfo = 'text',
            text = ~paste('</br> Area: ', location,
                          '</br> Deaths: ', prettyNum(total_deaths, big.mark = ",")),
            marker = list(color = 'rgba(146,47,37,0.66)'))})
  ###third page
  center_city <- reactive({
    if (input$continent != 'All'){
      data.frame(lng = mid_location_data[['lon']][mid_location_data[["continent"]] == input$continent],
                 lat = mid_location_data[['lat']][mid_location_data[["continent"]] == input$continent])
    }else{NULL}
  })
  fig <- leaflet(area_situation,options = leafletOptions(scrollWheelZoom = F)) %>% addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
    fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>% 
    addEasyButton(
      easyButton(
        icon="fa-globe",
        title="Zoom to Level 1",
        onClick=JS("function(btn, map){ map.setZoom(1); }")
      )
    ) 
  output$global_map <- renderLeaflet({
    if (input$continent == 'All'){
      fig %>% setView(lng = 120,lat = 0,zoom = 1.5)
      }else{
        fig %>% setView(lng = center_city()$lng,lat = center_city()$lat,zoom = 3)
      }
    })
  
  mybins <- c(0,10,100,1000,10000,100000,1000000, Inf)
  colorpal <- colorNumeric('YlOrRd',mybins[1:7])
  mypalette <- colorBin(palette = "YlOrRd",domain = area_situation$total_cases,
                        na.color = "transparent",bins = mybins)
  
  
  #pre_filteredData <- reactive({
  #  area_situation %>% filter(date == input$obs_day)
  #})
  #filteredData <- reactive({
  #  if (input$continent != 'All')
  #  {pre_filteredData() %>% filter(continent == input$continent)}
  #  else{pre_filteredData()}
  #})
  
  filteredData <- reactive({
    if (input$continent != 'All')
    {area_situation %>% filter(date == input$obs_day & continent == input$continent)}
    else{area_situation %>% filter(date == input$obs_day)}
  })
  observe({
    leafletProxy(mapId = "global_map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~(total_cases*1.15 + 100000), weight = 1, color = "#777777",
                 fillColor = ~mypalette(total_cases),fillOpacity = 0.8,
                 popup = ~paste('Cases: ',total_cases)
      )
  })
  observe({
    proxy <- leafletProxy("global_map", data = filteredData())
    proxy %>% clearControls()
    if (input$legend) {
      proxy %>% addLegend(position = "bottomright",colors = mypalette(c(mybins[1:7])+1),
                          labels = c('0 ~ 10','10 ~ 100',
                                     '100 ~ 1k','1k ~ 10k',
                                     '10k ~ 100k','100k ~ 1kk','1kk ~ '),opacity = 0.7
      )
    }
  })
  
  filteredData2 <- reactive({
    if (input$continent != 'All')
    {area_situation %>%
        filter(continent == input$continent) %>% 
        group_by(date,continent) %>%
        summarise(CASE = sum(total_cases),DEATH = sum(total_deaths))}
    else{
      area_situation %>%
        group_by(date) %>%
        summarise(CASE = sum(total_cases),DEATH = sum(total_deaths))}
  })
  output$area_line_chart <- renderPlotly({
    filteredData2() %>%
      plot_ly(x = ~date, y = ~CASE, name = 'cumulated confirmed cases', color = I('#B22222'),
              type = 'scatter', mode = 'lines+markers',hoverinfo = 'text',
              text = ~paste('</br> Date: ', as.character(date),
                            '</br> Confirmed Cases: ', prettyNum(CASE, big.mark = ","))) %>% 
      add_trace(y = ~DEATH, name = 'cumulated deaths', color = I('#808080'), mode = 'lines+markers',hoverinfo = 'text',
                text = ~paste('</br> Date: ', as.character(date),
                              '</br> Deaths: ', prettyNum(DEATH, big.mark = ",")))
  })
  
  filteredData3 <- reactive({
    if (input$continent != 'All'){
      inner_data <- area_situation %>%
        filter(date == input$obs_day & continent == input$continent) %>%
        dplyr::arrange(-total_cases) %>% head(10) %>%
        dplyr::select(location,total_cases)
      inner_data$location <- factor(inner_data$location,levels = inner_data$location[order(inner_data$total_cases)])
      inner_data
      }else{
        inner_data <- area_situation %>% filter(date == input$obs_day) %>% 
          dplyr::arrange(-total_cases) %>% head(10) %>%
          dplyr::select(location,total_cases)
        inner_data$location <- factor(inner_data$location,levels = inner_data$location[order(inner_data$total_cases)])}
    inner_data
    })
  output$area_bar_chart <- renderPlotly({
      plot_ly(data = filteredData3(),x = ~total_cases, y = ~location, type = 'bar',hoverinfo = 'text',
              text = ~paste('</br> Area: ', location,
                            '</br> Confirmed Cases: ', prettyNum(total_cases, big.mark = ",")),
              marker = list(color = 'rgba(222,45,38,0.8)'))
    })
}
shinyApp(ui,server)

