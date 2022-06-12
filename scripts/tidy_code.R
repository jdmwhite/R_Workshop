#### Install packages ----
install.packages('tidyverse')

#### Load libraries ----
library(tidyverse)

#### Load data ----
cedar <- read_csv('data/cedar.csv')

#### Clean data ----
cedar_clean <- cedar[-c(1:24),]
colnames(cedar_clean) <- cedar_clean[1,]
cedar_clean <- cedar_clean[-1,]

#### %>% (i.e. pipe) operator

#### Format ----
str(cedar_clean)
names(cedar_clean)

cedar_conv <- cedar_clean %>% 
  mutate(across(c(4,6:10,12), as.numeric))

str(cedar_conv)

cedar_conv <- cedar_conv %>% 
  mutate(across(.cols=c(1,3,5,11), as.factor))

str(cedar_conv)

cedar_conv %>% 
  mutate(Fire = `Fire [#]` + `Fire [#]`) %>% select(`Fire [#]`, Fire)

#### Rename ----
names(cedar_conv)

#### Janitor Rename ----
janitor::clean_names(cedar_conv)

cedar_conv %>% rename(Elevation = `Elevation [m a.s.l.]`,
                 MAP = `Precip annual total [mm/a]`,
                 MAT = `MAAT [¬∞C]`,
                 Number_fires = `Fire [#]`,
                 Tree_ID = No)  -> cedar_names
names(cedar_names)

#### Select (subset columns or reorder) ----
cedar_names %>% select(Area, Site, MAP, Number_fires) 

#### Filter ----
cedar_names %>% filter(Area == 'Welbedacht')

cedar_names %>% filter(Elevation <= 1000 | Aspect == 'East')

#### janitor::get_dupes ----

cedar_names %>% janitor::get_dupes()
test <- rbind(cedar_names, cedar_names[1,])
test %>% janitor::get_dupes()

?janitor::get_dupes
#### Case when ----
cedar_names %>% distinct(Aspect)

cedar_names %>% mutate(Aspect = case_when(
  Aspect == 'W' ~ 'West',
  Aspect == 'N' ~ 'North',
  Aspect == 'S' ~ 'South',
  Aspect == 'E' ~ 'East')) -> cedar_names

cedar_names %>% mutate(Alt = case_when(
  Elevation > 1000 ~ 'High_alt',
  Elevation <= 1000 ~ 'Low_alt'))

cedar_names %>% distinct(Aspect)

#### Arrange ----

#### Mutate ----

#### Summarise in dplyr ----
cedar_names %>% 
  group_by(Area, Site) %>%
  summarise(Number_trees = n()) -> cedar_sites_trees

# cedar_sites_trees %>%
#   ggplot(cedar_sites_trees) +
#   geom_point(aes(x = Area, y = Number_trees))

cedar_sites_trees %>% 
  group_by(Area) %>%
  summarise(mean_trees = mean(Number_trees), sd_trees = sd(Number_trees),
            median_trees = median(Number_trees))

#### Summarise with tabyl() (janitor)

#### Summarise with base R

#### Summarise with skimr::skim
skimr::skim(cedar_names)

#### Pivot_longer ----

#### Pivot_wider ----

#### Left_join ----

#### Unite (tidyr) ----

#### Separate (tidyr) ----

#### Dates (lubridate) ----