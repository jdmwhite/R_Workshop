###Wrangling
# Part 1: Data cleaning
#### Install packages ----
install.packages('tidyverse') # dplyr, tidyr and readxl 
install.packages('janitor') # cleaning names

#### Load libraries ----
library(readxl)
library(tidyverse)
library(janitor)
library(dplyr)

## Import data
#### Loading the data ----
photographer_names <- read_excel("data/raw/photographer_names.xlsx")
#photographer_names <- read.csv("path/filename.csv)

## Inspect data
#### Inspecting the data
View(photographer_names)
names(photographer_names) # gives you the names of the columns 
str(photographer_names) # gives you the structure of the data
head(photographer_names) # gives you the first 6 rows of the data
tail(photographer_names) # gives you the last 6 rows of the data

## Prepare data
#### Clean and rename data (columns)
photographers <- clean_names(photographer_names) # the new names are unique
photographers <- rename(photographers, initial_date = date_4)
photographers <- rename(photographers, repeat_date = date_6)
names(photographers) # view the new names of the columns

#### changing column format/class
class(photographers$initial_date) # view the class of the column
class(photographers$repeat_date)

photographers %>% mutate(initial_date = case_when(
  initial_date == "1986/7" ~ 
    as.character(as.Date('86/01/01', format = "%y/%m/%d")),
  initial_date != "1986/7" ~ 
    as.character(as.Date(as.numeric(initial_date), origin="1899-12-30"))
)) -> photographers

View(photographers)

#### remove or add rows
rows_delete (photographers, tibble(site_number = 1100)) # you can match the delete function to any specific row
rows_insert (photographers, tibble (site_number = 9)) # new row is added.

#### remove or add columns
select(photographers,-initial_date) # removes column
photographers <- mutate (photographers, location = " ") #adds a new column
View(photographers)

# Part 2: Data transformation
pull(photographers, latitude) # we pulled the latitude column from the dataframe
select(photographers, site_location, altitude_m) # we selected the site_location and altitude_m columns from the dataframe
New_Data <-select(photographers, site_location, altitude_m) # we selected the site_location and altitude_m columns from the photographers data frame and made a new data frame called New_Data
View(New_Data)
