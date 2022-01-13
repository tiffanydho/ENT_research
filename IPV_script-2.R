setwd("C:/Users/user/Documents/ENT_research/IPV/")
# figure out how to do the following two steps in one line
pacman::p_load(dplyr, tidyr, ggplot2)
loadRData <- function(fname){
  #loads an RData file, and returns it
  load(fname)
  get(ls()[ls() != "fname"])
}

d_2012 <- loadRData(file.path("data", "NEISS-2012.rda"))
#d_2014 <- loadRData(file.path("data", "NEISS-2014.rda"))

head_n_neck <- c("(75) Head", "(76) Face", "(77) Eyeball", "(88) Mouth", "(89) Neck", "(94) Ear")

# use AGEG4_C instead of AGEG6_C for now
df <- d_2012 %>% select(-contains("SP")) %>% 
  select(-c(LOCG_C, AGEYR_C, AGEG6_C, BDYPTG_C, 
            INJURY_C, RACE2_C, INTENT, TRAFFIC, TRAF_MVO, OCCUPANT, INTCAU_T)) %>% 
  filter(BDYPT %in% head_n_neck) %>% 
  filter(!is.na(PERP))


calcFreq <- function(df, var){
  df %>% group_by(!! sym(var)) %>% summarise(n=n()) %>% 
    mutate("freq (%)" = round(n/sum(n)*100, 2)) %>% arrange(desc(`freq (%)`))
}

frq_bdypt <- calcFreq(df, "BDYPT")
frq_diag <- calcFreq(df, "DIAG")
frq_race <- calcFreq(df, "RACETH_C")

# percent stacked bar chart
freq_test <- df %>%  group_by(DIAG, SEX) %>%  summarise(n=n()) 

# proportion 
ggplot(freq_test, aes(fill=SEX, y = n, x = DIAG)) + 
  geom_bar(position="fill", stat="identity") + #, position = position_fill(reverse = TRUE)
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual("Sex", values = c("(0) UNK" = "gray", "(1) Male" = "royalblue1", "(2) Female" = "maroon1"))+
  geom_text(aes(label = n), position = position_fill(vjust = 0.5), colour="snow2") + 
  labs(x = "Diagnosis", y = "Proportion") 

# absolute value 
ggplot(freq_test, aes(x = reorder(DIAG, -n), y=n, fill=SEX)) + 
  geom_bar(stat="identity") + # position="fill"
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual("Sex", 
                    breaks = c("(1) Male", "(2) Female", "(0) UNK"), # specify the order of the legend
                    values = c("(0) UNK" = "gray", "(1) Male" = "royalblue1", "(2) Female" = "maroon1")) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), colour="snow2") +
  labs(x = "Diagnosis", y = "Count") 






  
# how to add count to plot? stat_count() only takes x OR y
# https://mq-software-carpentry.github.io/r-ggplot-extension/02-categorical-data/index.html


# # how to order by proportion??
# freq_test %>% 
#   mutate(DIAG = forcats::fct_reorder(.f = DIAG, .x = SEX,
#                                      .fun = function(.x) mean(.x == "(2) Female"),
#                                       .desc = TRUE)) %>%
#   ggplot(aes(x = DIAG, y=n, fill = SEX)) +
#   geom_bar(position = "fill",  stat="identity") +
#   labs(y = "Proportion")


# # https://cran.r-project.org/web/packages/forcats/vignettes/forcats.html
# # https://stackoverflow.com/questions/55120534/how-can-i-reorder-factors-by-proportion-in-ggplot-efficiently
# 
# dframe %>%
#   # convert variable to a factor, ordered (in descending order) by the proportion of
#   # rows where the class == "1"
#   mutate(variable = forcats::fct_reorder(.f = variable, 
#                                          .x = class,
#                                          .fun = function(.x) mean(.x == "1"),
#                                          .desc = TRUE)) %>%
#   ggplot(aes(x = variable, fill = class)) +
#   geom_bar(position = "fill") +
#   labs(y = "Proportion")

