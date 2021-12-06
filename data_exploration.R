#install.packages("pacman")
pacman::p_load(readxl, dplyr, tidyr, tidyselect)
# library(readxl)
# library(dplyr)
# library(tidyr)
# library(tidyselect)

source("my_func.R")
setwd("C:/Users/user/Documents/ENT_research")
filename <- "KimplePatientsWithSi_DATA_LABELS_2021-12-01_0858.xlsx"
raw_data <- read_excel(file.path("data",filename), sheet = 1, 
                       col_names = TRUE, col_types = NULL, na = "", skip = 0)
raw_data <- raw_data %>% 
  rename(DOB="Date of birth", allergies="List of allergies (separate by commas)")
raw_data <- raw_data %>% 
  mutate(Age = calcAge(as.Date(DOB))) %>% relocate(Age, .after = DOB)
raw_data <- raw_data %>% 
  mutate(num_allergies = countItem(allergies)) %>% relocate(num_allergies, .before = allergies)

raw_data <- raw_data %>% 
  mutate(R_endo_score=rowSums(across(vars_select(names(raw_data), starts_with("Right endoscopy", ignore.case = TRUE), -contains("total"))))) %>% 
  relocate(R_endo_score, .after = "Right endoscopy total score")
raw_data <- raw_data %>% 
  mutate(L_endo_score=rowSums(across(vars_select(names(raw_data), starts_with("Left endoscopy", ignore.case = TRUE), -contains("total"))))) %>% 
  relocate(L_endo_score, .after = "Left endoscopy total score")
raw_data <- raw_data %>% 
  mutate(endo_score=rowSums(across(vars_select(names(raw_data), one_of("R_endo_score", "L_endo_score"))))) %>% 
  relocate(endo_score, .after = "R_endo_score")

raw_data <- raw_data %>% mutate_at(c(113:142), as.numeric) # to figure out str -> var name (instead of hard-coding by index)
raw_data <- raw_data %>% 
  mutate(survey_score_cat1=rowMeans(subset(raw_data, select=c(113:142)), na.rm = TRUE)) %>% 
  relocate(survey_score_cat1, .after = "My sexual activity is affected by my problem")
# omit cat2 responses for now - too  many missing values

#cat3: col 151 - 170 (NOT reliable, must change)
factor(score, levels = c("0 never", "1 hardly", "2 some of the time", "3 often", "4 most of the time") , labels = c("0", "1", "2", "3", "4"))
# score <- c("4 most of the time", "4 most of the time", "1 hardly")
# score_f <- factor(score, levels = c("0 never", "1 hardly", "2 some of the time", "3 often", "4 most of the time") , labels = c("0", "1", "2", "3", "4"))
# as.numeric(as.character(score_f))
raw_data_test <- raw_data %>% mutate_at(c(151:170), ~factor(.x, levels = c("0 never", "1 hardly", "2 some of the time", "3 often", "4 most of the time") , labels = c("0", "1", "2", "3", "4")))
raw_data_test1 <- raw_data %>% mutate_at(raw_data, vars(Gender,Race), ~factor(.x, levels = c("0 never", "1 hardly", "2 some of the time", "3 often", "4 most of the time") , labels = c("0", "1", "2", "3", "4")))
# to do:
# - calc average survery scores (by category)
# - change diagnoses/drug use to BOOLEAN (unchecked -> F, checked -> T)
# - write email to ask questions
# - review: model for continuous & categorical 