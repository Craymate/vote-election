library(haven)
library(tidyverse)
library(labelled)

raw_data <- read_dta("./Inputs/usa_00003.dta")

raw_data <- labelled::to_factor(raw_data)

reduced_data <- 
  raw_data %>%
  select(age, sex, stateicp, race, educd)

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

mut_educd <- function(x){
  for(i in 1:length(x)){
    if (x[i] == "1 or more years of college credit, no degree" | x[i] == "some college, but less than 1 year"){
      x[i] <- "Completed some college, but no degree"
    };
    if (x[i] == "regular high school diploma"){
      x[i] <- "High school graduate"
    };
    if (x[i] == "associate's degree, type not specified"){
      x[i] <- "Associate Degree"
    };
    if (x[i] == "bachelor's degree" | x[i] == "professional degree beyond a bachelor's degree"){
      x[i] <- "College Degree (such as B.A., B.S.)"
    };
    if (x[i] == "ged or alternative credential"){
      x[i] <- "Other post high school vocational training"
    };
    if (x[i] == "master's degree"){
      x[i] <- "Masters degree"
    };
    if (x[i] == "nursery school, preschool" | x[i] == "doctoral degree"){
      x[i] <- "Doctorate degree"
    };
    if (x[i] == "grade 1" | x[i] == "grade 2" | x[i] == "grade 3" | x[i] == "no schooling completed" | x[i] == "kindergarten" | x[i] == "n/a"){
      x[i] <- "3rd Grade or less"
    };
    if (x[i] == "grade 4" | x[i] == "grade 5" | x[i] == "grade 6" | x[i] == "grade 7" | x[i] == "grade 8"){
      x[i] <- "Middle School - Grades 4 - 8"
    };
    if (x[i] == "12th grade, no diploma" | x[i] == "grade 10" | x[i] == "grade 11" | x[i] == "grade 9"){
      x[i] <- "Completed some high school"
    }
  }
  return(x)
}


reduced_data <- reduced_data %>%
  mutate(educd = mut_educd(as.vector(educd)))

census <- reduced_data %>%
  group_by(state, sex, age, race, educd) %>%
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
