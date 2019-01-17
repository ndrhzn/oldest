library(ggplot2)
library(dplyr)

df <- read.csv('data_clean.csv', stringsAsFactors = F, 
               colClasses = c(NA, NA, NA, "Date", "Date", "Date", NA, NA, NA))

legend <- data.frame(
  x = as.Date(c('1987-10-01', '1998-01-01')),
  y = c(112.7, 122.6),
  label = c('when she became\nworld\'s oldest\nperson', 'when she died')
)

# visualize

png(filename = 'oldest.png', width = 1000, height = 600)
ggplot(df)+
  geom_segment(aes(x = became, y = age_became, xend = died, yend = age_died, color = gender),
               show.legend = F)+
  geom_point(data = df[1:nrow(df)-1, ], aes(x = died, y = age_died, fill = gender), 
             pch = 21, color = '#F3F7F7', size = 3)+
  geom_label(data = legend, aes(x = x, y = y, label = label), 
            family = 'Ubuntu Mono', color = '#5D646F', fill = '#F3F7F7',
            hjust = c(0, -0.05), vjust = c(1, 0.5), label.size = NA, 
            size = 4.5, lineheight = 0.9)+
  scale_fill_brewer(type = 'qual', palette = 2, direction = -1, labels = c('female', 'male'), 
                    aesthetics = c('fill', 'color'))+
  scale_x_date(limits = c(as.Date('1953-01-01'), as.Date('2020-01-01')),
               breaks = seq.Date(as.Date('1955-01-01'), as.Date('2020-01-01'), '5 years'), 
               date_labels = '\'%y', expand = c(0.01, 0.01))+
  scale_y_continuous(breaks = classInt::classIntervals(as.numeric(df$age_died), 7, 
                                                       style = 'jenks')$brks %>% 
                       round(digits = 1))+
  guides(fill = guide_legend(override.aes = list(size = 5)))+
  labs(title = 'World\'s Oldest Person Titleholders Since 1955',
       caption = 'Data: Gerontology Research Group | Viz: Textura.in.ua',
       x = 'year', y = 'age')+
  theme_minimal(base_family = 'Ubuntu Mono')+
  theme(
    legend.position = 'top',
    legend.justification = 'left',
    legend.title = element_blank(),
    legend.text = element_text(size = 13),
    legend.spacing.x = unit(5, 'pt'),
    legend.margin = margin(l = -7),
    text = element_text(color = '#5D646F'),
    axis.title.x = element_text(hjust = 1, size = 13, margin = margin(t = 10)),
    axis.title.y = element_text(hjust = 1, size = 13, margin = margin(r = 10)),
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