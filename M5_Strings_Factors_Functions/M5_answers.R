library(tidyverse)

# Regular expressions ####
# Alternatively, can load the individual package
#library(stringr)

# Links to useful resources: 
# RStudio regex addin: https://www.garrickadenbuie.com/project/regexplain/
# library website: https://stringr.tidyverse.org/index.html 

# Regex Practice 
test_string <- c("Apple","pineApple" ,"banana", "2323!","ID", "Regex?", "Coconut")

### Solutions ####
str_view(test_string, "A")
str_view(test_string, "[:digit:]")
str_view(test_string, "[:upper:]$")
str_view(test_string, "[?]")
str_view(test_string, "([:alpha:][:alpha:])\\1")



## Regex Exercise ####
#The simulated data contains admission date (admit_date), the patient’s name (name), the patient’s date of birth (dob), the patient’s address (address), the city the patient lives in (city), and column that contains the symptoms each patient was experiencing at admission (symptoms).
#load data
ehr <- readRDS("../Data/ehr.Rds")
# let's take a look at the data
str(ehr)
View(ehr)
#Notice the inconsistencies in the name, address, city, and symptoms columns. 

### Cleaning the name column. ####
# 1. Take a closer look. 
ehr |>
  arrange(name) |>
  count(name)
# Issues observed leading to counting unique patients incorrectly:
## 1. Case
## 2. Trailing spaces
## 3. Spacing between first and last name
## 4. Middle initial
## 5. Typo - comma at end of name

# 1. Case - let's change everything to lower case
ehr |>
  mutate(name = str_to_lower(name)) |>
  count(name)

# 2. Trailing spaces - let's trim the white spaces
ehr |>
  mutate(name = str_trim(name)) |>
  count(name)

# The remaining fixes don't have a built in function, so we will use str_replace to match the error
# 3. Spacing 
ehr |> 
  # replace two spaces with one
  mutate(name = str_replace(name, "\\s{2,}", " ")) |>
  count(name)

# 4. Middle initial
ehr |> 
  # replace any space-letter-space with a space 
  mutate(name = str_replace(name, " \\w ", " ")) |>
  count(name)
# 5. Delete comma
ehr |> 
  #replace comma with nothing
  mutate(name = str_replace(name, ",", "")) |>
  count(name)

# Put it all together
ehr <- ehr |>
  mutate(name = str_to_lower(name)) |>
  mutate(name = str_trim(name)) |>
  mutate(name = str_replace(name, "\\s{2,}", " ")) |>
  mutate(name = str_replace(name, " \\w ", " ")) |>
  mutate(name = str_replace(name, ",", "")) 

#check your work  
ehr |>
  count(name)

### Cleaning the city column. ####
ehr |>
  count(city)
# Issues observed leading to counting unique patients incorrectly:
## 1. Case
## 2. Inconsistent naming using "city of"

# 1. Case - let's change everything to lower case
ehr |>
  mutate(city = str_to_lower(city)) |>
  count(city)

# 2. Drop "city of" - a couple of ways to do this. 
ehr |>
  # Drop "ity of " and any letter preceding it
  mutate(city = str_replace(city, "[:alpha:]ity of ", "")) |>
  count(city)

ehr |>
  # use regex function to create a case insensitive pattern
  mutate(city = str_replace(city, stringr::regex("city of ", ignore_case = TRUE), "")) |>
  count(city)

# Put it all together
ehr <- 
  ehr |>
  mutate(city = str_to_lower(city)) |>
  #since it is already in lower case, don't need to worry about case
  mutate(city = str_replace(city, "city of ", "")) 
  
#check your work
ehr |>
  count(city)

### Cleaning the symptom column. ####
# This column contains the response from a check box type question, but with some funky formatting. The options were: pain, headache, and nausea
# Following tidy data, we should convert this into 3 columns: pain, headache, nausea which have binary response (yes/no)
# We know there aren't typos in the spelling of the symptoms so we can use str_detect to find the rows with a given symptom

ehr <- ehr |>
  mutate(pain = ifelse(str_detect(symptoms, "Pain") == TRUE, "Y","N"),
         headache = ifelse(str_detect(symptoms, "Headache") == TRUE, "Y","N"),
         nausea = ifelse(str_detect(symptoms, "Nausea") == TRUE, "Y","N")) 

### Bonus manipulation: Make a first and last name column

ehr |>
  mutate(first_name = str_extract(name, "^\\w+"), 
         last_name = str_extract(name, "\\w+$"), .before = name) 


# Factors #### 

# Alternative to tidyverse library can load the individual package
#library(forcats)

# Links to useful resources: 
# library website: https://forcats.tidyverse.org/

## Factors Exercise ####
# Goal: 
  # A) Want pain, headache and nausea to be factors with the levels in order of: NA, N, Y.
  # B) Want pain, headache and nausea to be reduced to 2 factors with the levels in order of: Y, N or NA 

# 1. Let's explore the ehr data structure. Are any columns factors?
str(ehr)
# -OR-  ask if a specific col is a factor using base R syntax
is.factor(ehr$pain)
#the output is FALSE, so we need to convert all of the logical columns to factors

# 2. If pain is converted to a factor, what would be the levels?
levels(as_factor(ehr$pain))
#Based on the output the NA is being ignored, but we want to keep it as a factor for future data visualizations. 

# 3. Convert to factors but set NA as a level
ehr <- 
  ehr |> 
  #function to convert to factor and keep NA as a level  
  mutate(pain = fct_na_value_to_level(pain))

# Goal A: Put the levels in the correct order
# Use base R to see what the current order is. Does it need to be changed? 
levels(ehr$pain)

# We want an output that reads NA, "N", "Y", so we need to reoder the factors
ehr <- 
  ehr |>
    mutate(pain = fct_relevel(pain, c(NA, "N","Y")))
# check to make sure the reorder worked
levels(ehr$pain)

# Goal B: Combine the N and NA levels in the correct order
ehr <- 
  ehr |> 
    mutate(pain = fct_collapse(pain, NorNA = c(NA, "N"))) |>
  #reorder so Y is first
    mutate(pain = fct_relevel(pain, c("Y", "NorNA")))
  
# check to make sure the reorder worked
glimpse(ehr$pain)

# Functions ####

# Suppose you'll be receiving monthly electronic health record data. This seems like a great opportunity to automate some of the name cleaning work we did at the beginning of the module. Convert the code chunk below to a function. The name column is not consistently named between months and can be named : name, Name, names, NAMES, FirstLastName, etc. 

# load that data again to start with a messy data set 
ehr <- readRDS("../Data/ehr.Rds")

# Code chunk to be converted to a function:
ehr |>
  mutate(name = str_to_lower(name)) |>
  mutate(name = str_trim(name)) |>
  mutate(name = str_replace(name, "\\s{2,}", " ")) |>
  mutate(name = str_replace(name, " \\w ", " ")) |>
  mutate(name = str_replace(name, ",", "")) 

# 1. Decide:
#    - which arguments are needed
#    - on the function name (google R style guides if you're stuck)

# 2. Write and test
# 3. Add documentation

#' Remove white space, middle initials, commas, and convert to lower case
#'
#' @param data the raw ehr data 
#' @param namecolumn the column will be cleaned. Must be a string. 
#'
#' @return ehr data with an additional column named "name_clean"

CleanNameTypos = function(data, namecolumn) {
  
  clean_data <- 
    data |>
      mutate(name_clean = str_to_lower(namecolumn)) |>
      mutate(name_clean = str_trim(namecolumn)) |>
      mutate(name_clean = str_replace(namecolumn, "\\s{2,}", " ")) |>
      mutate(name_clean = str_replace(namecolumn, " \\w ", " ")) |>
      mutate(name_clean = str_replace(namecolumn, ",", "")) 
  
  return(clean_data)
}

