#### Install packages ----
install.packages('tidyverse')
install.packages('palmerpenguins')
install.packages('ggrepel')
install.packages('ggridges')
install.packages('ggdist')

#### Load libraries ----
library(tidyverse)
library(palmerpenguins)
library(ggrepel)
library(ggridges)
library(ggdist)

#### Explore data ----
penguins <- palmerpenguins::penguins
str(penguins)

#### Line charts ----
ggplot(data = drop_na(penguins), aes(body_mass_g, bill_length_mm, col = species, shape = species)) +
  geom_point(alpha = 0.8) +
  scale_colour_manual(values = c('darkorange', 'purple', 'cyan4'),name = 'Penguin species') +
  scale_shape(name = 'Penguin species') +
  geom_smooth(method = 'lm') +
  labs(x = 'Body mass (g)', y = 'Bill length (mm)') +
  theme_bw() 

#### Barplot with SD ----
penguins %>% 
  group_by(species) %>% 
  summarise(flipper_mean = mean(flipper_length_mm, na.rm = T),
            flipper_sd = sd(flipper_length_mm, na.rm = T)) -> penguins_summary

penguins_summary %>%
ggplot() +
  geom_col(aes(x = species, y = flipper_mean),
           fill = c('darkorange','purple','cyan4')) +
  geom_errorbar(aes(x = species, ymax = flipper_mean + flipper_sd, ymin = flipper_mean - flipper_sd), width = 0.5) +
  labs(x = "Penguin species", y = 'Flipper length (mm)') +
  theme_minimal()

penguins_summary %>%
ggplot() +
  geom_point(aes(x = species, y = flipper_mean, col = species)) +
  geom_errorbar(aes(x = species, ymax = flipper_mean + flipper_sd, ymin = flipper_mean - flipper_sd, col = species), width = 0.5) +
  scale_colour_manual(values = c('darkorange','purple','cyan4'), guide = 'none') +
  labs(x = "Penguin species", y = 'Flipper length (mm)') +
  theme_minimal()

#### Boxplot ----
ggplot(penguins) +
  geom_boxplot(aes(x = species, y = flipper_length_mm),
               col = c('darkorange','purple','cyan4')) +
  labs(x = "Penguin species", y = 'Flipper length (mm)') +
  theme_minimal() 

#### Histograms ----
ggplot(penguins) +
  geom_histogram(aes(x = flipper_length_mm, fill = species), position = 'identity', alpha = 0.6) +
  scale_fill_manual(values = c('darkorange','purple','cyan4')) +
  labs(x = 'Flipper length (mm)', y = 'Frequency') +
  theme_minimal()

#### Density ----
ggplot(penguins) +
  geom_density(aes(x = flipper_length_mm, fill = species), col = NA, alpha = 0.6) +
  scale_fill_manual(values = c('darkorange','purple','cyan4')) +
  labs(x = 'Flipper length (mm)', y = 'Density') +
  theme_minimal()

#### Ridges ----
ggplot(penguins) +
  geom_density_ridges(aes(x = flipper_length_mm, y = species, fill = species), col = NA, alpha = 0.6) +
  scale_fill_manual(values = c('darkorange','purple','cyan4')) +
  labs(x = 'Flipper length (mm)', y = 'Density') +
  theme_minimal()

#### Extreme distribution plots ----
ggplot() +
  stat_halfeye(data = penguins, aes(species, flipper_length_mm, fill = species, col = species), point_interval = 'median_qi', side = 'left', scale = 0.5, adjust = 0.75) +
  stat_dots(data = penguins, aes(species, flipper_length_mm, fill = species, col = species), scale = 0.5) +
  scale_fill_manual(values = colorspace::lighten(c('darkorange', 'purple', 'cyan4'), 0.75), guide = 'none') +
  scale_colour_manual(values = c('darkorange', 'purple', 'cyan4'), guide = 'none') +
  labs(x = 'Penguin species', y = 'Flipper length (mm)') +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  geom_text_repel(aes(x = 0.8, y = 196, label = 'density'),
                  nudge_y = 22, nudge_x = -0.2, 
                  segment.curvature = 0.1,
                  segment.size = 0.2, size = 3) +
  geom_text_repel(aes(x = 1.2, y = 190, label = 'points'),
                  nudge_y = 30, nudge_x = 0.2, 
                  segment.curvature = -0.1,
                  segment.size = 0.2, size = 3) +
  geom_text_repel(aes(x = 2, y = 186, label = '95% interval'),
                  nudge_y = -5, nudge_x = 0.75, 
                  segment.curvature = 0.2,
                  segment.size = 0.2, size = 3) +
  geom_text_repel(aes(x = 2, y = 196, label = 'median'),
                  nudge_y = -3, nudge_x = 0.95, 
                  segment.curvature = 0.2,
                  segment.size = 0.2, size = 3) +
  geom_text_repel(aes(x = 3, y = 220, label = '50% interval'),
                  nudge_y = 7.5, nudge_x = -0.75, 
                  segment.curvature = 0.2,
                  segment.size = 0.2, size = 3)

penguins %>% filter(species == 'Chinstrap') %>% select(flipper_length_mm) %>% median_qi(na.rm = T)


