library(dplyr)
library(stringr)
library(ggplot2)

Sys.setlocale(category = 'LC_TIME', locale = 'en_US.UTF-8')

df <- read.csv('data.csv', stringsAsFactors = F)

# declare function to format dates

fix_dates <- function(date_vector){
  
  date_vector <- date_vector %>% 
    str_replace_all('Jan.', 'January') %>% 
    str_replace_all('Feb.', 'February') %>% 
    str_replace_all('Mar.', 'March') %>% 
    str_replace_all('Apr.', 'April') %>% 
    str_replace_all('Aug.', 'August') %>% 
    str_replace_all('Sept.', 'September') %>% 
    str_replace_all('Oct.', 'October') %>% 
    str_replace_all('Nov.', 'November') %>% 
    str_replace_all('Dec.', 'December') %>% 
    str_squish()
  
  for(i in 1:length(date_vector)) {
    
    if(str_detect(date_vector[i], '(?<=\\w )\\d\\b')) {
      
      date_vector[i] <- paste0(str_extract(date_vector[i], '^\\w+'),
                               ' 0',
                               str_extract(date_vector[i], '(?<=\\w )\\d\\b'),
                               ', ',
                               str_extract(date_vector[i], '\\b\\d{4}\\b'))
      
    }
    
  }
  
  
  date_vector <- as.Date(date_vector, format = '%B %d, %Y')
  
  return(date_vector)
  
}

# fix dates and calculate necessary variables

df <- df %>% 
  mutate(born = fix_dates(born),
         died = fix_dates(died),
         became = lag(died),
         age_became = lubridate::time_length(became - born, 'years'),
         age_died = lubridate::time_length(died - born, 'years')) %>% 
  filter(!is.na(became)) %>% 
  select(index, name, birthplace, born, became, died, age_became, age_died, gender)

# fix current titleholder dates

df$died[nrow(df)] <- Sys.Date()
df$age_died[nrow(df)] <- lubridate::time_length(df$died[nrow(df)] - df$born[nrow(df)], 'years')

# save csv

write.csv(df, file = 'data_clean.csv', row.names = F)

rm(df)
  