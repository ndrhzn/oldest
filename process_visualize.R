library(dplyr)
library(stringr)
library(ggplot2)

Sys.setlocale(category = 'LC_TIME', locale = 'en_US.UTF-8')

df <- read.csv('data.csv', stringsAsFactors = F)

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


df <- df %>% 
  mutate(born = fix_dates(born),
         died = fix_dates(died),
         age = died - born) %>% 
  select(index, born, died, age, gender, is_a_new_record) %>% 
  mutate(reign = lead(died, n = 1) - died,
         became = lag(died),
         lost = died,
         age_became = became - born)

df$died[nrow(df)] <- Sys.Date()
df$lost[nrow(df)] <- Sys.Date()
df$age[nrow(df)] <- df$died[nrow(df)] - df$born[nrow(df)]

# first

ggplot(df)+
  geom_linerange(aes(x = index, ymin = became, ymax = lost, color = gender),
                 size = 3)+
  scale_x_reverse()+
  scale_y_date(date_breaks = '5 years', date_labels = '%Y', minor_breaks = NULL)+
  coord_flip()+
  theme_minimal(base_family = 'Ubuntu Mono')

# second

png(filename = 'oldest.png', width = 1000, height = 600)

ggplot(df)+
  # geom_hline(data = df[df$is_a_new_record == TRUE,], aes(yintercept = age),
  #            linetype = 'dotted', color = '#5D646F', size = 0.4)+
  geom_segment(aes(x = became, y = age_became, xend = died, yend = age, color = gender))+
  geom_point(aes(x = died, y = age, color = gender))+
  scale_color_brewer(type = 'qual', palette = 2)+
  scale_x_date(date_breaks = '5 years', date_labels = '\'%y')+
  scale_y_continuous(breaks = classInt::classIntervals(as.numeric(df$age), 6, style = 'jenks')$brks)+
  labs(title = 'Title',
       subtitle = 'Subitle subtitle',
       caption = 'Data: | Viz: Textura.in.ua')+
  theme_minimal(base_family = 'Ubuntu Mono')+
  theme(
    legend.position = 'none',
    legend.justification = 'left',
    legend.title = element_blank(),
    legend.text = element_text(size = 13),
    legend.spacing.x = unit(5, 'pt'),
    legend.margin = margin(l = -7),
    text = element_text(color = '#5D646F'),
    axis.title = element_blank(),
    axis.text = element_text(size = 13),
    strip.text = element_blank(),
    axis.text.x.bottom = element_text(),
    panel.grid.major = element_line(linetype = 'dotted', color = '#5D646F', size = 0.1),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 36, face = 'bold', margin = margin(b = 10)),
    plot.subtitle = element_text(size = 18, margin = margin(b = 10)),
    plot.caption = element_text(size = 12, margin = margin(t = 20)),
    plot.background = element_rect(fill = '#F3F7F7'),
    plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), 'cm')
  )

dev.off()