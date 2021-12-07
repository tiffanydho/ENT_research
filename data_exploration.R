#install.packages("pacman")
pacman::p_load(readxl, dplyr, tidyr, tidyselect, tibble)
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

# omit the following categorical vars for now - will train with ML later
## gender, race, ethnicity, diagnoses
processed_data <- raw_data[, c("Study ID")]

processed_data <- processed_data %>% 
  mutate(age=calcAge(as.Date(raw_data$DOB))) %>% 
  mutate(num_allergies = countItem(raw_data$allergies)) %>%
  mutate(R_endo_score=rowSums(select(raw_data, starts_with("Right endoscopy", ignore.case = TRUE), -contains("total")))) %>% 
  mutate(L_endo_score=rowSums(select(raw_data, starts_with("Left endoscopy", ignore.case = TRUE), -contains("total")))) 
processed_data <- processed_data %>% 
  mutate(endo_score=rowSums(select(processed_data, one_of("R_endo_score", "L_endo_score"))))

# cat1: col 108-137
cat1_col <- c(108:137)
raw_data <- raw_data %>% mutate_at(cat1_col, as.numeric)
processed_data <- processed_data %>%
  mutate(survey_score_cat1=rowMeans(subset(raw_data, select=cat1_col), na.rm = TRUE)) 

# middle group (technically group 2) was omitted due to large amount of missing value

# cat2: col 145-165 
# score <- c("4 most of the time", "4 most of the time", "1 hardly")
# score_f <- factor(score, levels = c("0 never", "1 hardly", "2 some of the time", "3 often", "4 most of the time") , labels = c("0", "1", "2", "3", "4"))
# as.numeric(as.character(score_f))

cat2_col <- c(145:164)
raw_data <- raw_data %>% 
  mutate_at(cat2_col, ~factor(.x, levels = c("0 never", "1 hardly", "2 some of the time", "3 often", "4 most of the time"), labels = c("0", "1", "2", "3", "4"))) %>%  
  mutate_at(cat2_col, as.character) %>%   
  mutate_at(cat2_col, as.numeric)
processed_data <- processed_data %>%
  mutate(survey_score_cat2=rowMeans(subset(raw_data, select=cat2_col), na.rm = TRUE)) 

cat3_col <- c(166:177)
raw_data <- raw_data %>% 
  mutate_at(cat3_col, ~factor(.x, levels = c("1 Extremely Uncharacteristic", "2", "3", "4", "5 Extremely Characteristic"), labels = c("1","2","3","4","5"))) %>% 
  mutate_at(cat3_col, as.character) %>%   
  mutate_at(cat3_col, as.numeric)
processed_data <- processed_data %>%
  mutate(survey_score_cat3=rowMeans(subset(raw_data, select=cat3_col), na.rm = TRUE)) 


# to do:
# - calc average survery scores (by category)
# - change diagnoses/drug use to BOOLEAN (unchecked -> F, checked -> T)
# - write email to ask questions
# - review: model for continuous & categorical 