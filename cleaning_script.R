
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(lubridate)

library(rvest)

# get ----
# data loc: 
root <- paste0("https://en.wikipedia.org/wiki/List_of_verified_oldest_people") 
tables <- read_html(root) |> html_nodes("table")

# get data by gender: 
old_women <- tables[[1]] |> 
  html_table(header = TRUE) |>
  mutate(
    gender = 'female'
  )

old_men <- tables[[2]] |> 
  html_table(header = TRUE) |>
  mutate(
    gender = 'male'
  )

# wrangle ----
# fix headers: 
cols <- c('rank', 'name', 'birth_date', 'death_date', 'age', 'place_of_death_or_residence', 'gender')

colnames(old_women) <- cols
colnames(old_men)   <- cols

# bind all ppl: 
old_ppl <- old_women |> rbind(old_men)


# remove [reference] indices, fix date coltypes & age calc in year fractions:
old_ppl <-
old_ppl |>
  mutate( # regex
    name = str_replace(name, "\\[.*]", ""),
    birth_date = str_replace(birth_date, "\\[.*]", ""),
    death_date = str_replace(death_date, "\\[.*]", ""),
    place_of_death_or_residence = str_replace(place_of_death_or_residence, "\\[.*]", "")
  ) |>
  mutate( # dates & age formatting
    birth_date = dmy(birth_date),
    death_date = dmy(death_date),
    still_alive = ifelse(is.na(death_date), "alive", "deceased"),
    age  = ifelse(still_alive == "alive", 
                  as.numeric(interval(birth_date, Sys.Date()), 'years'), 
                  as.numeric(interval(birth_date, death_date), 'years'))
    )
  