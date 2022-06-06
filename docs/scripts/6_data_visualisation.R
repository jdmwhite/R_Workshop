#### Install packages ----
install.packages('tidyverse')
install.packages('palmerpenguins')
install.packages('ggrepel')
install.packages('ggridges')

#### Load libraries ----
library(tidyverse)
library(palmerpenguins)
library(ggrepel)
library(ggridges)

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

#### Barplot/Points with SD ----
penguins %>% 
  group_by(species) %>% 
  summarise(flipper_mean = mean(flipper_length_mm, na.rm = T),
            flipper_sd = sd(flipper_length_mm, na.rm = T)) %>% 
  ggplot() +
  geom_col(aes(x = species, y = flipper_mean),
           fill = c('darkorange','purple','cyan4')) +
  geom_errorbar(aes(x = species, ymax = flipper_mean + flipper_sd, ymin = flipper_mean - flipper_sd), width = 0.5) +
  labs(x = "Penguin species", y = 'Flipper length (mm)') +
  theme_minimal()

penguins %>% 
  group_by(species) %>% 
  summarise(flipper_mean = mean(flipper_length_mm, na.rm = T),
            flipper_sd = sd(flipper_length_mm, na.rm = T)) %>% 
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

#### Lollipop chart ----
flipper_mean_all <- mean(penguins$flipper_length_mm, na.rm = T)

peng_summary <- penguins %>% 
  group_by(species) %>% 
  summarise(flipper_mean = mean(flipper_length_mm, na.rm = T),
            flipper_sd = sd(flipper_length_mm, na.rm = T)) 

ggplot() +
   geom_segment(data = peng_summary, aes(x = species, xend = species,
                    y = flipper_mean_all, yend = flipper_mean, col = species)) +
  geom_point(data = peng_summary, aes(x = species, y = flipper_mean, col = species)) +
  scale_color_manual(values = c('darkorange','purple','cyan4'), name = 'Penguin species') +
  geom_hline(aes(yintercept = flipper_mean_all), lty = 2) +
  labs(x = '', y = 'Mean flipper length (mm)') +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = 'top') +
  geom_text_repel(aes(x = 1.5, y = flipper_mean_all, label = 'overall mean'), nudge_x = 0.5, nudge_y = 7.5, segment.curvature = -0.1, size = 3, segment.size = 0.2)



