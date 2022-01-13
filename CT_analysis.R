install.packages('xlsx')
library(readxl)
pacman::p_load(readxl, dplyr, tidyr, ggplot2, purrr)


setwd("C:/Users/user/Documents/ENT_research")
filename <- "KimplePatientsWithSi_DATA_LABELS_2021-12-01_0858.xlsx"
raw_data <- read_excel(file.path("data",filename), sheet = 1,
                       col_names = TRUE, col_types = NULL, na = "", skip = 0)
obs_w_ct <- raw_data %>% 
  rename(CT_score = "Lund-Mackay total score (left+right)") %>% 
  filter(!is.na(CT_score))

test <- as.data.frame(obs_w_ct) %>%
  mutate(across(c("Right maxillary score", "left maxillary score"), ~factor(.,levels = 0:2))) %>%
  map_dfc(table)
