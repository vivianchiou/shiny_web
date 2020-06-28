#final update : 2020-06-28
#1============================================================================================================
#< parameter & data import >==================================================================================
options(stringsAsFactors = FALSE,scipen = 999)
easypackages::libraries('magrittr','tidyverse','ggplot2','gganimate','dplyr')
df <- read_csv(url('https://covid.ourworldindata.org/data/ecdc/full_data.csv'))
#2============================================================================================================
#< data clean >===============================================================================================
data_date <- df$date %>% unique()
location <- df$location %>% unique()

temp_df <- expand.grid(data_date,location)
names(temp_df) <- c('date','location')
temp_df <- merge.data.frame(x = temp_df,y = df,by = c('date','location'),all.x = T)
#按地區、日期把資料補齊
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

area_situation <- temp_df %>% dplyr::filter(location != 'World')
rm(temp_df)


animate_data <- area_situation %>% 
  dplyr::select(location,date,total_cases) %>% 
  dplyr::group_by(date) %>% 
  dplyr::arrange(date,-total_cases) %>% 
  dplyr::mutate(rank = 1:n()) %>% 
  dplyr::filter(rank <= 10) %>% 
  dplyr::mutate(rank = rank,
                Value_lbl = paste0(" ",total_cases)) %>%
  mutate(total_all = sum(total_cases)) %>% 
  ungroup()


#3============================================================================================================
#< ggplot >===================================================================================================
total_text_y = 0.87*(max(animate_data$total_cases))
panel_size_y = max(animate_data$total_cases) * 1.15  
vline_original_y = seq(floor(max(animate_data$total_cases)/8), 
                       max(animate_data$total_cases), by = floor(max(animate_data$total_cases)/8))

country_font_size = 10
bar_end_num_size = 11


staticplot = ggplot(animate_data,
                    aes(rank,group = location,
                        color = as.factor(location),
                        fill = as.factor(location))) +
  
  geom_tile(aes(y = total_cases/2,height = total_cases,width = 0.9),
            alpha = 0.9, color = NA) +
  
  geom_text(aes(y = 0,label = paste(location, " ")),
            vjust = 0.2,size = country_font_size,
            fontface = "bold",hjust = 1) +
  
  geom_text(aes(y = total_cases, label = prettyNum(total_cases,big.mark = ','), hjust = 0), fontface = 'bold', size = bar_end_num_size) +
  
  geom_text(aes(x = 8,y = total_text_y,
                label = sprintf('%s\n Global Total =%s', date, format(total_all, big.mark=",", scientific=FALSE))),
                size = 7,color = 'grey') +
  
  geom_hline(yintercept = vline_original_y, size = .08, color = "grey", linetype = 'dotted') +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  coord_flip(clip = "off", expand = FALSE) +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        #axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = 'black'),
        
        #panel.background=element_blank(),
        panel.border=element_blank(),
        
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        #panel.grid.major.x = element_line( size=.1, color="grey" ),
        #panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=40, face="bold", colour="grey", vjust=1),
        plot.subtitle=element_text(size=18, face="italic", color="grey"),
        plot.caption =element_text(size=15, hjust=0.5, face="italic", color="grey"),
        #plot.background=element_blank(),
        plot.margin = margin(2,5, 2,8, "cm"),
  )
staticplot
#4============================================================================================================
#< build animate >============================================================================================
current_state_len = 0 
current_transition_len = 3  
anim = staticplot + transition_states(date, transition_length = current_transition_len, state_length = current_state_len) +
  view_follow(fixed_x = TRUE,fixed_y = c(-10, NA))  +
  labs(title = 'Cumulative Confirmed Cases : {closest_state}',  
       subtitle  =  "Top 10 area",
       caption  = "")
fig <- animate(anim,duration = 100, fps = 10,  width = 1600, height = 1000,res = 100,end_pause = 20)
#5============================================================================================================
#< save gif >=================================================================================================
anim_save(filename = "gganim20200628.gif",animation = fig)
