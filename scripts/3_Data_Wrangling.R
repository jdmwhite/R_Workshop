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
names(photographer_names)
str(photographer_names)
head(photographer_names)
tail(photographer_names)

## Prepare data
#### Clean and structure data
photographers <- clean_names(photographer_names) #The new names are unique
photographers <- rename(photographers, initial_date = date_4)
photographers <- rename(photographers, repeat_date = date_6)
names(photographers)
#### remove or add rows
rows_delete (photographers, tibble(site_number = 1100)) #you can match the delete function to any specific row
rows_insert (photographers, tibble (site_number = 9)) #The new row is added.
#### remove or add columns
select(photographers,-initial_date) #removes column
photographers <- mutate (photographers, location = " ") #adds a new column

# Part 2: Data transformation
pull(photographers, latitude)
select(photographers, site_location, altitude_m)
