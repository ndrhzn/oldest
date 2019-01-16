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
  
  date_vector <- case_when(
    str_detect(date_vector, '(?<=\\w )\\d') ~ paste0(str_extract(date_vector, '^\\w+'),
                                                    ' 0',
                                                    str_extract(date_vector, '(?<=\\w )\\d'),
                                                    ', ',
                                                    str_extract(date_vector, '\\b\\d{4}\\b'))
  )

  date_vector <- as.Date(date_vector, format = '%B %d, %Y')
      
  return(date_vector)
  
}


# fix dates and calculate necessary variables

df <- df %>% 
  mutate(born = fix_dates(born),
         died = fix_dates(died),
         age = lubridate::time_length(died - born, 'years')) %>% 
  select(index, born, died, age, gender, is_a_new_record) %>% 
  mutate(reign = lead(died, n = 1) - died,
         became = lag(died),
         lost = died,
         age_became = lubridate::time_length(became - born, 'years'))

# fix current titleholder dates

df$died[nrow(df)] <- Sys.Date()
df$lost[nrow(df)] <- Sys.Date()
df$age[nrow(df)] <- lubridate::time_length(df$died[nrow(df)] - df$born[nrow(df)], 'years')

# visualize

png(filename = 'oldest.png', width = 1000, height = 600)

ggplot(df)+
  geom_segment(aes(x = became, y = age_became, xend = died, yend = age, color = gender))+
  geom_point(aes(x = died, y = age, color = gender))+
  scale_color_brewer(type = 'qual', palette = 2)+
  scale_x_date(limits = c(as.Date('1953-01-01'), as.Date('2020-01-01')),
               breaks = seq.Date(as.Date('1955-01-01'), as.Date('2020-01-01'), '5 years'), 
               date_labels = '\'%y', expand = c(0.01, 0.01))+
  scale_y_continuous(breaks = classInt::classIntervals(as.numeric(df$age), 6, 
                                                       style = 'jenks')$brks %>% 
                       round(digits = 1))+
  labs(title = 'World\'s Oldest Person Titleholders Since 1955',
       subtitle = 'Subitle subtitle',
       caption = 'Data: Gerontology Research Group | Viz: Textura.in.ua')+
  theme_minimal(base_family = 'Ubuntu Mono')+
  theme(
    legend.position = 'top',
    legend.justification = 'left',
    legend.title = element_blank(),
    legend.text = element_text(size = 13),
    legend.spacing.x = unit(5, 'pt'),
    legend.margin = margin(l = -7),
    text = element_text(color = '#5D646F'),
    axis.title = element_blank(),
    axis.text = element_text(size = 13),
    axis.text.y = element_text(vjust = -0.5, margin = margin(r = -35)),
    panel.grid.major = element_line(linetype = 'dotted', color = '#5D646F', size = 0.1),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 36, face = 'bold', margin = margin(b = 10)),
    plot.subtitle = element_text(size = 18, margin = margin(b = 10)),
    plot.caption = element_text(size = 12, margin = margin(t = 20)),
    plot.background = element_rect(fill = '#F3F7F7'),
    plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), 'cm')
  )

dev.off()