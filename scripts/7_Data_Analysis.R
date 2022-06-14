
# Basic data analysis
1+3

1+(4*2)

1+(4^2) ##^: exponent

sin(30)

log10 (10)

log10 (20)

# Comparisons 

2 == 2

2 == 4

1 < 2

# Data analysis using a dataframe

#### Extra packages 
install.packages('broom')
install.packages('stargazer')
install.packages('effects')

#### Load libraries
library(readxl)
library(tidyverse)
library(janitor)
library(dplyr)
library(palmerpenguins)
library(broom)
library(stargazer)
library(effects)
library(ggplot2)

#### Load data ----
penguins <- palmerpenguins::penguins
str(penguins)
View(penguins)

#### Selecting and filtering data
penguins_bill <- select (penguins, species, island, bill_length_mm, bill_depth_mm)
penguins_flipper <- select (penguins, species, island, flipper_length_mm)
penguins_bodymass <- select(penguins, -flipper_length_mm, -bill_length_mm, -bill_depth_mm)
penguins_09 <- filter(penguins, year == 2009)
penguins2 <- filter(penguins, year < 2008)

#### say for instance you want the penguin bill information for 2007 and 2008 only

##### option 1
penguins_bill2 <- select (penguins, species, island, bill_length_mm, bill_depth_mm, year)
penguins_bill2 <- filter(penguins_bill2, year < 2009)
penguins_bill2
penguins_bill2 <- na.omit(penguins_bill2)
penguins_bill2

##### option 2
penguins_bill2_2 <- select(filter(penguins, year < 2009), species, island, bill_length_mm, bill_depth_mm, year)
penguins_bill2_2 <- na.omit(penguins_bill2_2)
penguins_bill2_2

##### option 3 
## you can use the pipe function
Penguins_bill<-penguins %>%
  filter(year < 2009) %>%
  select(species, island, bill_length_mm, bill_depth_mm, year) %>% 
  na.omit()
Penguins_bill

##### mutate

Penguins_bill<-Penguins_bill %>% mutate(bill_length_m = bill_length_mm/1000) #added a new column with new units for bill_length
Penguins_bill
Penguins_bill<-Penguins_bill %>% mutate(bill_depth_m = bill_depth_mm/1000)

##### group_by
Penguins_bill_mean<-Penguins_bill %>%
  group_by(species, year) %>% 
  summarise(mean_bill_length_mm = mean(bill_length_mm))
Penguins_bill_mean

Penguins_bill_mean<-Penguins_bill %>%
  group_by(species, year) %>% 
  summarise(mean_bill_length_mm = mean(bill_length_mm)) %>% 
  arrange(desc(year))
Penguins_bill_mean

Penguins_bill_mean<-Penguins_bill %>%
  group_by(species, year) %>% 
  summarise(mean_bill_length_mm = mean(bill_length_mm))
Penguins_bill_mean

alpha=0.05
Penguins_bill_Stats <- Penguins_bill %>%
  group_by(species, island, year) %>%
  summarise( 
    n=n(),
    mean_bill_length_mm=mean(na.omit(bill_length_mm)),
    bill_length_sd=sd(na.omit(bill_length_mm)),
    mean_bill_depth_mm=mean(na.omit(bill_length_mm)),
    bill_depth_sd=sd(na.omit(bill_length_mm)))

Penguins_bill_Stats

##### Plot stats 
names(Penguins_bill_Stats)
StatsPlot<-ggplot(Penguins_bill_Stats, aes(x = species, y = mean_bill_length_mm))+
  geom_line(size = 1, colour = "red")+
  geom_point(shape = 19, size = 1, colour = "red")+
  geom_errorbar(aes(ymin=mean_bill_length_mm-bill_length_sd, ymax=mean_bill_length_mm+bill_length_sd),width=.5, colour ="black", linetype = "solid")

StatsPlot<-ggplot(Penguins_bill_Stats, aes(x = species, y = mean_bill_length_mm))+
  geom_line(size = 1, colour = "red")+
  geom_point(shape = 19, size = 1, colour = "red")+
  geom_errorbar(aes(ymin=mean_bill_length_mm-bill_length_sd, ymax=mean_bill_length_mm+bill_length_sd),width=.5, colour ="black", linetype = "solid")+
  theme_classic()+
  xlab("Species_Names")+
  ylab("Bill_length (mm)")

StatsPlot<-ggplot(Penguins_bill_Stats, aes(x = species, y = mean_bill_length_mm))+
  geom_line(size = 1, colour = "red")+
  geom_point(shape = 19, size = 1, colour = "red")+
  geom_errorbar(aes(ymin=mean_bill_length_mm-bill_length_sd, ymax=mean_bill_length_mm+bill_length_sd),width=.5, colour ="black", linetype = "solid")+
  theme_classic()+
  xlab("Species_Names")+
  ylab("Bill_length (mm)")+facet_grid(rows=vars(year))

StatsPlot<-ggplot(Penguins_bill_Stats, aes(x = species, y = mean_bill_length_mm))+
  geom_line(size = 8, colour = "red")+
  geom_point(shape = 19, size = 4, colour = "red")+
  geom_errorbar(aes(ymin=mean_bill_length_mm-bill_length_sd, ymax=mean_bill_length_mm+bill_length_sd),width=.1, colour ="black", linetype = "solid")+
  theme_classic()+
  xlab("Species_Names")+
  ylab("Bill_length (mm)")+facet_grid(rows=vars(year, island))

StatsPlot

#### Categorical linear model ----
lm <- lm(bill_length_mm ~ island, data = penguins)
lm
summary(lm)

#### Analysis of Variance ----
aov <- aov(bill_length_mm ~ island, data = penguins)
aov
summary(aov)

#### Continuous linear model ----
lm2 <- lm(bill_length_mm ~ body_mass_g, data = penguins)

summary(lm2)

#### Categorical and Continuous model ----
lm3 <- lm(bill_length_mm ~ body_mass_g + island, data = penguins)

summary(lm3)

#### Summarise results ----
lm_coefficients <- tidy(aov)
lm
lm2_coefficients <- tidy(lm2)
lm3_coefficients <- tidy(lm3)

#### Plot effects ----
lm_effects <- allEffects(lm)
lm2_effects <- allEffects(lm2)
lm3_effects <- allEffects(lm3)

plot(lm_effects)
plot(lm2_effects)
plot(lm3_effects)

#### Stargazer summary table ----
# export as a text; copy and paste
stargazer::stargazer(lm, lm2, lm3, type = 'text', out = 'output/tables/lm_summaries.text')
