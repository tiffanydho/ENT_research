#install.packages("pacman")
pacman::p_load(readxl, dplyr, tidyr, tidyselect, tibble)
install.packages('xlsx')
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
  rename(DOB="Date of birth", allergies="List of allergies (separate by commas)", 
         CT_score = "Lund-Mackay total score (left+right)")

# omit the following categorical vars for now - will train with ML later
## gender, race, ethnicity, diagnoses
processed_data <- raw_data[, c("Study ID", "CT_score")]

processed_data <- processed_data %>% 
  mutate(age=calcAge(as.Date(raw_data$DOB))) %>% 
  mutate(num_allergies = countItem(raw_data$allergies)) %>%
  mutate(R_endo_score=rowSums(select(raw_data, starts_with("Right endoscopy", ignore.case = TRUE), -contains("total")))) %>% 
  mutate(L_endo_score=rowSums(select(raw_data, starts_with("Left endoscopy", ignore.case = TRUE), -contains("total")))) 
processed_data <- processed_data %>% 
  mutate(endo_score=rowSums(select(processed_data, one_of("R_endo_score", "L_endo_score"))))

# cat1: col 108-137
rsdi_col <- c(108:137)
raw_data <- raw_data %>% mutate_at(rsdi_col, as.numeric)
rsdi_emo_col <- c(108:117)
rsdi_func_col <- c(118:126)
rsdi_phys_col <- c(127:137)
processed_data <- processed_data %>%
  mutate(rsdi_emo_score=rowMeans(subset(raw_data, select=rsdi_emo_col), na.rm = TRUE)) 
processed_data <- processed_data %>%
  mutate(rsdi_func_score=rowMeans(subset(raw_data, select=rsdi_func_col), na.rm = TRUE)) 
processed_data <- processed_data %>%
  mutate(rsdi_phys_score=rowMeans(subset(raw_data, select=rsdi_phys_col), na.rm = TRUE)) 
# processed_data <- processed_data %>%
#   mutate(survey_score_cat1=rowMeans(subset(raw_data, select=cat1_col), na.rm = TRUE)) 

# cat2: col 145-165 
# score <- c("4 most of the time", "4 most of the time", "1 hardly")
# score_f <- factor(score, levels = c("0 never", "1 hardly", "2 some of the time", "3 often", "4 most of the time") , labels = c("0", "1", "2", "3", "4"))
# as.numeric(as.character(score_f))

# Modified Differential Emotions Scales (MDES)
mdes_col <- c(145:164)
raw_data <- raw_data %>% 
  mutate_at(mdes_col, ~factor(.x, levels = c("0 never", "1 hardly", "2 some of the time", "3 often", "4 most of the time"), labels = c("0", "1", "2", "3", "4"))) %>%  
  mutate_at(mdes_col, as.character) %>%   
  mutate_at(mdes_col, as.numeric)
processed_data <- processed_data %>%
  mutate(mdes_score=rowMeans(subset(raw_data, select=mdes_col), na.rm = TRUE)) 

# The Buss-Perry Aggression Questionnaire (BPAQ)
bpaq_col <- c(166:177)
raw_data <- raw_data %>% 
  mutate_at(bpaq_col, ~factor(.x, levels = c("1 Extremely Uncharacteristic", "2", "3", "4", "5 Extremely Characteristic"), labels = c("1","2","3","4","5"))) %>% 
  mutate_at(bpaq_col, as.character) %>%   
  mutate_at(bpaq_col, as.numeric)
processed_data <- processed_data %>%
  mutate(aggression_score=rowMeans(subset(raw_data, select=bpaq_col), na.rm = TRUE)) 

plot(processed_data[,c(4, 7:12)], cex=.5, 
     main="full dataset")

Q <- quantile(processed_data$num_allergies, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(processed_data$num_allergies, na.rm = TRUE)
upper <- Q[2]+1.5*iqr
lower <- Q[1]-1.5*iqr
processed_data_pruned<- subset(processed_data, 
                              processed_data$num_allergies > (Q[1] - 1.5*iqr) & 
                              processed_data$num_allergies < (Q[2] + 1.5*iqr))
plot(processed_data_pruned[,c(3, 6:11)], cex=.5,
     main=paste("num_allergies outliers rm by 1.5*IQR", "(n=", nrow(processed_data_pruned), ")"))

processed_data_loose_prune<- subset(processed_data, 
                              processed_data$num_allergies < 25)
plot(processed_data_loose_prune[,c(3, 6:11)], cex=.5, 
     main=paste("num_allergies outliers rm only > 25", "(n=", nrow(processed_data_loose_prune), ")"))

data_w_ct <- processed_data[!is.na(processed_data$CT_score), ]
plot(data_w_ct[,c(2, 4, 7:12)], cex=.5, 
     main=paste("obs w/ CT score", "(n=", nrow(data_w_ct), ")"))
######## fit model ##############
# install.packages("olsrr")
library(olsrr)
model <- lm(endo_score ~ num_allergies + rsdi_emo_score + 
              rsdi_func_score + rsdi_phys_score + mdes_score + 
              aggression_score, data = processed_data, na.action = na.exclude)
summary(model)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(model)
ols_step_best_subset(model)

model_pruned <- lm(endo_score ~ num_allergies + rsdi_emo_score + 
              rsdi_func_score + rsdi_phys_score + mdes_score + 
              aggression_score, data = processed_data_pruned, na.action = na.exclude)
ols_step_best_subset(model_pruned)
plot(model_pruned)

model_ct <- lm(endo_score ~ num_allergies + rsdi_emo_score + 
                     rsdi_func_score + rsdi_phys_score + mdes_score + 
                     aggression_score, data = data_w_ct, na.action = na.exclude)
ols_step_best_subset(model_ct)

model_test <- lm(rsdi_emo_score~ endo_score +num_allergies+
                   rsdi_func_score + rsdi_phys_score + mdes_score + 
                   aggression_score, data = processed_data_pruned, na.action = na.exclude)
ols_step_best_subset(model_test)

model_test_loose <- lm(rsdi_emo_score~ endo_score +num_allergies+
                   rsdi_func_score + rsdi_phys_score + mdes_score + 
                   aggression_score, data = processed_data_loose_prune, na.action = na.exclude)
ols_step_best_subset(model_test_loose)

######################
install.packages("writexl")
library("writexl")
no_surgery_date <- subset(raw_data, is.na(raw_data$`Date of last surgery`) & !is.na(raw_data$`Last surgery type`), select=c(1:5, 28:29))
write_xlsx(no_surgery_date, file.path("data","individuals without surgery date.xlsx"),)
