###Wrangling

#tilde -  "~"
#assignment operator "<-"

# Part 1: Data cleaning
#### Install packages ----
#install.packages('tidyverse') #dplyr, tidyr, readxl
#install.packages('janitor')

#### Load libraries ----
library(readxl)
library(tidyverse)
library(janitor)
library(dplyr)

## Import data
#### Loading the data ----
photographer_names<-read_excel("data/raw/photographer_names.xlsx")
#photographer_names <- read.csv("path/filename.csv)


## Inspect data
#### Inspecting the data
view(photographer_names)
print(photographer_names)
str(photographer_names)
head(photographer_names)
tail(photographer_names)
names(photographer_names)

## Prepare data
#### Clean and structure data
photographers <- clean_names(photographer_names) #The new names are unique
view(photographers)
photographers <- rename(photographers, initial_date = date_4)
view(photographers)
photographers <- rename(photographers, repeat_date = date_6)
names(photographers)

#### remove or add rows
photographers1<-rows_delete (photographers, tibble(site_number = 1100)) #you can match the delete function to any specific row
photographers1
Photographers1<- rows_delete(photographers, tibble(site_number = 1006))
photographers1<-rows_insert (photographers, tibble (site_number = 9, site_location = "JHB")) #The new row is added.
view(photographers1)
#### remove or add columns
photographers<-select(photographers,-initial_date) #removes column
names(photographers)


# Part 2: Data transformation
rows_insert (photographers, tibble (site_number = 9)) #The new row is added.
pull(photographers, latitude)
pull(photographers, site_location)
loc_alt<-select(photographers, site_location, altitude_m)
loc_alt
