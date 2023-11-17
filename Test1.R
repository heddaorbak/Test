# Cleaning data
# clean names: janitor::clean_names(palmerpenguins::penguins_raw)
# skimr::skim
# tibble - use dplyr::distinct() get rid of repeated rows
# missing data - naniar:: vis_miss()


library(tidyverse)
library(palmerpenguins)
library(janitor)
library(skimr)
library(naniar)


# Data cleaning
janitor::clean_names(messy_data) 
clean_data <- janitor::clean_names(messy_data) # cleans names in header 
dplyr::glimpse(clean_data)

out <- skimr::skim(clean_data)

# lower case
clean_data$age <- tolower(clean_data$age)
clean_data$sex <- tolower(clean_data$sex)

# clean capital letters
clean_data <- clean_data %>% 
  mutate(age = str_replace(age, "j$", "juvenile"),
         age = str_replace(age, "a$", "adult"),
         species_name = str_replace_all(species_name, "Lagopus muta", "Lagopus lagopus")) %>%
  mutate(species_name = str_replace(species_name, "Logopus muta", "Lagopus matu"),
         species_name = str_replace(species_name, "Lagopas lagopus", "Lagopus lagopus"))

# Fix date
library(datefixR)
clean_data <- datefixR::fix_date_df(clean_data, "date")


#Deduplication
dim(clean_data)  
clean_data_dup <- clean_data |> slice(rep(1:n(), each = 3))
dim(clean_data_dup)  
clean_data_undup <- distinct(clean_data_dup)
dim(clean_data_undup)  

clean_data |> naniar::vis_miss()

# conflicted package - sorts conflicts between packages 
conflicted::conflict_prefer("filter", "dplyr")

# pivot wider - transform to long
smallGame_long <- smallGame %>% 
  pivot_longer(-c(small_game, contents, interval_year), # m?? legge inn _
               names_to = "site",
               values_to = "count")


# package styler 
# a <- 3 means a sign for 3

# functions

miles_to_km <- function(miles) {
  if (is.numeric(miles)){
    miles*1.609344
  }else{
    print("Miles needs to be numeric")
  }} 

#

##test branching #
jkhihio





