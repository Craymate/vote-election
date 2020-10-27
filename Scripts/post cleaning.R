library(haven)
library(tidyverse)
library(labelled)

raw_data <- read_dta("./Inputs/usa_00002.dta")

raw_data <- labelled::to_factor(raw_data)

reduced_data <- 
  raw_data %>%
  select(age, sex, stateicp, race, educ, hispan)

reduced_data <- as.data.frame(reduced_data)

abbs <- tibble(stateicp = str_to_lower(state.name), state = state.abb)

reduced_data <- reduced_data %>%
  left_join(abbs)

mut_race <- function(x){
  for(i in 1:length(x)){
    if (x[i] == "black/african american/negro"){
      x[i] <- "black/african american"
    };
    if (x[i] == "other race, nec"){
      x[i] <- "Other"
    };
    if (x[i] == "two major races"){
      x[i] <- "other race, nec"
    };
    if (x[i] == "three or more major races"){
      x[i] <- "other race, nec"
    }
  }
  return(x)
}

reduced_data <- reduced_data %>%
  mutate(race = mut_race(as.vector(race)))

mut_age <- function(x){
  for(i in 1:length(x)){
    if (x[i] < 30){
      x[i] <- "18 to 29"
    };
    if (x[i] < 45){
      x[i] <- "30 to 44"
    };
    if (x[i] < 60){
      x[i] <- "45 to 55"
    };
    if (x[i] == "less than 1 year old"){
      x[i] <- "18 to 29"
    };
    if (x[i] > 60){
      x[i] <- "60 plus"
    };
    if (x[i] == "90 (90+ in 1980 and 1990)"){
      x[i] <- "60 plus"
    }
  }
  return(x)
}

reduced_data <- reduced_data %>%
  mutate(age = mut_age(as.vector(age)))

mut_educ <- function(x){
  for(i in 1:length(x)){
    if (x[i] == "n/a or no schooling"){
      x[i] <- "3rd Grade or less"
    };
    if (x[i] == "grade 5, 6, 7, or 8"){
      x[i] <- "Middle School - Grades 4 - 8"
    };
    if (x[i] ==" 1 year of college"){
      x[i] <- "Completed some college, but no degree"
    };
    if (x[i] == "2 years of college"){
      x[i] <- "Completed some college, but no degree"
    };
    if (x[i] == "4 years of college"){
      x[i] <- "Completed some college, but no degree"
    };
    if (x[i] == "5+ years of college"){
      x[i] <- "College Degree (such as B.A., B.S.)"
    };
    if (x[i] == "grade 9"){
      x[i] <- "Doctorate degree"
    };
    if (x[i] == "nursery school to grade 4"){
      x[i] <- "Completed some high school"
    };
    if (x[i] == "grade 10"){
      x[i] <- "Completed some high school"
    };
    if (x[i] == "grade 11"){
      x[i] <- "Completed some high school"
    };
    if (x[i] == "grade 12"){
      x[i] <- "High school graduate"
    };
  }
  return(x)
}


reduced_data <- reduced_data %>%
  mutate(educ = mut_educ(as.vector(educ)))

census <- reduced_data %>%
  group_by(state, sex, age, race, educ, hispan) %>%
  count(name = "Number")

total <- reduced_data %>%
  group_by(state) %>%
  count()

census <- census %>%
  left_join(total)

census <- census %>%
  mutate(n = Number/n) %>%
  rename(Proportion = n) %>%
  rename(gender = sex)

write.csv(census, "./Outputs/census_data.csv")
