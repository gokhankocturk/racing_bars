library(tidyverse)
library(gganimate)
library(hrbrthemes)
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(countrycode)
library(ggflags) # devtools::install_github("rensa/ggflags")

dataset <- read_xlsx("data.xlsx")
dataset$code <- countrycode(dataset$country, origin = "country.name", destination = "iso2c")
dataset$code <- tolower(dataset$code)

dataset <- pivot_longer(dataset, cols = c("1995":"2020"), names_to = "year", values_to = "unemployment")

dataset <- dataset %>% 
  group_by(year) %>% 
  arrange(-unemployment) %>% 
  slice(1:10) %>% 
  mutate(rank = row_number(-unemployment)) %>% 
  ungroup()

# options(gganimate.nframes = 200)

my_plot <- dataset %>%
  ggplot() +
  aes(xmin = 0, xmax = unemployment,
      y = rank, ymin = rank - .45, ymax = rank + .45,
      fill = country, group = country ) +
  geom_rect(alpha = 0.7) + 
  geom_flag(aes(x = 0, country = code, y = rank), size = 12) +
  scale_x_continuous(limits = c(-10, 30),
                     breaks = seq(0, 30, 5)) + 
  geom_text(aes(label = country, color = country), x = -1.5, hjust = "right") + 
  geom_text(aes(label = paste0(round(unemployment, 1)), color = "black"), x = dataset$unemployment, hjust = "right") + 
  scale_y_reverse() + 
  scale_color_viridis_d() + 
  scale_fill_viridis_d() + 
  transition_states(year) + 
  labs(fill = NULL,
       title = "Unemployment Rate in OECD Countries",
       subtitle = "Year: {closest_state}",
       caption = "Source: stats.oecd.org") +
  ylab("") + 
  xlab("") +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "#7B7E85"),
    panel.background = element_rect(fill = "#DDE0E5"), 
    plot.title = element_text(color = "#760615", size = 20, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(color = "#760615", size = 14, hjust = 0.5, face = "bold.italic"),
    plot.caption = element_text(color = "#760615", size = 12, face = "bold.italic")
  ) +
  ease_aes('cubic-in-out')

animated_plot <- animate(my_plot, nframes = 260, fps = 10)
anim_save("animated_plot.gif", animated_plot)

# BU branch Hali
x <- 3
y <- 4
x + y

# GITHUB uzerinden ekleme yapildi
z <- 5

# GITHUB MERGE CONFLICT DENEMESI
sss <- 666
