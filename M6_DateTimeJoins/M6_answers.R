library(tidyverse)

# Time and Dates ####
# Alternatively, can load the individual package
#library(lubridate)

# Links to useful resources: 
# library website: https://lubridate.tidyverse.org/
# defining date format: https://r4ds.hadley.nz/datetimes#tbl-date-formats

## Slide Prompts ####

Sys.time()
today()
as.numeric(Sys.time())
as.numeric(today())

# Showing that the numeric data type is time since 1/1/1970, approx 54.4 years ago
## Sys.time as a numeric data type is in seconds since epoch which can be shown by dividing roughly the number of seconds in a year
as.numeric(Sys.time())/(60*60*24*365) 

## today as numeric data type is in days since epoch
as.numeric(today())/365

## Exercise ####  

#1. Read in date.csv
list_data <- read.csv("data/date.csv")

# let's take a look- here are a view ways we can "look" at the data
View(list_data)
colnames(list_data)
str(list_data)

#2. Cleaning. Columns starting with date_ are currently characters. 
# convert date_infection string to date
list_data |>
    mutate(date_infection = ymd(date_infection))
# we could repeat the process for the remaining columns 
# -OR-
# A faster method would be to define the columns as dates when they are read into R. If we use readr's `read_csv` what happens? 
list_data <- read_csv("data/date.csv")
# again, let's look at the data
spec(list_data)
# the col starting with date_ are read in as dates since they all follow a standard date format. 
#3. Calculate times

list_data <- 
    list_data |>
    # 3a. calculate patient age from birth year  
      mutate(age = year(date_hospitalisation) - birth_year) |>
    # 3b. days between infection, onset, hospitalization
      mutate(time_to_recovery = date_outcome - date_onset,
             incubation_days = date_onset - date_infection,
             hosp_days = date_outcome - date_hospitalisation) |> 
    # 3c. date to epi.week
      mutate(week_reported = epiweek(date_onset))

## Challenge Exercises ####

## Exercise 1: Looking for errors. 
# Collecting dates accurately can be hard. How would you go about identifying potential entry errors? 
# Hint: Use the intervals we just calculated

# Look for negative date intervals using filter
list_data |>
  filter(time_to_recovery < 0)


## Challenge Exercise 2
# Convert the previous exercise into a function. Assume the the data will always be in the same format including column names
Processing_List_Data = function(file_name = "data/date.csv"){
  
  d_out <- 
    read_csv(file_name) |>
    # a. calculate patient age from birth year  
    mutate(age = year(date_hospitalisation) - birth_year) |>
    # b. days between infection, onset, hospitalization
    mutate(time_to_recovery = date_outcome - date_onset,
           incubation_days = date_onset - date_infection,
           hosp_days = date_outcome - date_hospitalisation) |> 
    # c. date to epi.week
    mutate(week_reported = epiweek(date_onset))

  return(d_out)
  }

# Joins ####
# Alternatively, can load the individual package
#library(dplyr)

# Links to useful resources: 
# library website: https://lubridate.tidyverse.org/
# defining date format: https://r4ds.hadley.nz/datetimes#tbl-date-formats

## Slide Prompts ####

# Practicing joins
x <- tibble(Patient_ID = c(12,25,72), Diag_Test = c("Y","N","Y"))
y <- tibble(Patient_ID = c(12,72,83), Test_Result = c("Pos","Neg","Neg"))


## Exercise ####  
# 1. Read in hospitalization.csv and contact.csv
hos <- read_csv("data/hospitalization.csv") 
con <- read_csv("data/contact.csv")
# 2. Explore data: check col names, etc. in prep for join. 
spec(hos)
spec(con)
#What column is the key?
#Are all other columns uniquely named? 

# 3. Join hos and con into a single data set using the appropriate key. What type of join should be used?
# they are the same length, so lets see if they have the same entries.
anti_join(hos, con)
anti_join(con, hos)
#the result of the anti_join(s) is NULL so we can use a any mutating join
con_hos <- 
  hos |> 
    left_join(con, by = "case_id")

# 4. Sanity checks - did the join do what we wanted
# let's look at column names
spec(con_hos)
# let's look at the number of rows
dim(con_hos)

## Challenge Exercises ####


## Exercise 1: (joins) Census data. I have downloaded the total population size for 4 states, and some cities within each state. 
#Calculate the population size in each state excluding the city populations. 
# Hint: Take advantage of the data sets similar column names. 

state <- read_csv("data/state.csv")
city <- read_csv("data/city.csv")

spec(state)
spec(city)
# 1. Prep city data
# 1a. Create a common key. The first two numbers of the city GEOID is that same as the state GEOID
# 1b. Calculate the total city population for each state (notice Texas has two cities)
city_population <- 
  city |>
    #let's hang on to the full GEOID for the city, just rename it "place"
  rename(place = GEOID) |>
  mutate(GEOID = as.numeric(str_extract(place, ".."))) |>
   #create city populations for each state
  group_by(GEOID) |>
  summarise(estimate = sum(estimate)) |>
   #add a name for clarity
  mutate(NAME = "city")
# 2. Use the key to combine city and state data
cs_join <- 
  state |>
     # it is important to include the by argument since they have same column names but different values. 
      left_join(city_population, by = "GEOID") 
# 3. Use mutate to subtrate the city estimate from the state estimate
cs_join |>
  mutate(state_remaining_population = estimate.x - estimate.y)

## Exercise 2: (data manipulation) Returning to the contact data set - What is the off-spring distribution? Does it change with generation? 
con |>
  # 1. add group to turn on by generation calculation
  #group_by(generation) |>
  # 2. count how many time a case_id shows up in the infector column 
  count(infector) |>
  # 3. calculate the mean occurance of each infector
  summarise(mean_offspring = mean(n))




