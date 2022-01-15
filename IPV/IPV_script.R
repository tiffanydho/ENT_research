setwd("C:/Users/user/Documents/ENT_research/IPV/")
pacman::p_load(dplyr, tidyr, ggplot2, rstatix)
load.R.data <- function(fname){
  #loads an RData file and returns it
  load(fname)
  get(ls()[ls() != "fname"])
}

d_2012 <- load.R.data(file.path("data", "NEISS-2012.rda"))
#d_2014 <- load.R.data(file.path("data", "NEISS-2014.rda"))

head_n_neck <- c("(75) Head", "(76) Face", "(77) Eyeball", "(88) Mouth", "(89) Neck", "(94) Ear")

# use AGEG4_C instead of AGEG6_C for now
df <- d_2012 %>% select(-contains(c("SP_", "SPORTS"))) %>% 
  select(-c(LOCG_C, AGEYR_C, AGEG6_C, BDYPTG_C, 
            INJURY_C, RACE2_C, INTENT, TRAFFIC, TRAF_MVO, OCCUPANT, INTCAU_T)) %>% 
  filter(PERP=="(01) Spouse/partner" & INTENT_C %in% c("(1) Sexual assault", "(2) Other assault") ) %>% 
  filter(BDYPT %in% head_n_neck)


#df %>% group_by(INTENT_C, RACETH_C) %>% summarise(n=n()) %>% mutate("freq (%)" = round(n/sum(n)*100, 2)) %>% pivot_wider(names_from = INTENT_C, values_from = n)

calc.freq.per.col <-  function(x){
  #just calculating the frequency. I prefer to make my functions outside of chains
  round(x/sum(x, na.rm=TRUE)*100, 2)
}

## can delete later
sex_vs_nonsex_race <- df %>% group_by(INTENT_C, RACETH_C) %>% summarise(n=n()) %>% 
  pivot_wider(names_from = INTENT_C, values_from = n) %>%  
  mutate(across(.cols = contains("assault"), 
                .fns = calcFreqPerCol, 
                .names = "{.col}_freq (%)" ))
###


create.proportion.table <- function(df, for_var, by_var) {
    df %>% group_by(!! sym(by_var), !! sym(for_var)) %>% summarise(n=n()) %>% 
      pivot_wider(names_from = !! sym(by_var), values_from = n) %>% 
      mutate(across(.cols = c(-1), 
                    .fns = calc.freq.per.col, 
                    .names = "{.col}_prop (%)" )) 
}

mutate_all(df, ~replace(., is.na(.), 0))


create.freq.table <- function(df, for_var, by_var) {
  df %>% group_by(!! sym(by_var), !! sym(for_var)) %>% summarise(n=n()) %>% 
    pivot_wider(names_from = !! sym(by_var), values_from = n) %>% 
    mutate_all(~replace(., is.na(.), 0.5))
}
intent_sex <- create.freq.table(df, "SEX", "INTENT_C") 

# create.freq.table(d_2012, "RACETH_C", "INTENT_C")

# what to do with the NAs?  
intent_age <- create.freq.table(df, "AGEG4_C", "INTENT_C")
intent_race <- create.freq.table(df, "RACETH_C", "INTENT_C")
intent_sex <- create.freq.table(df, "SEX", "INTENT_C") 
intent_loc <- create.freq.table(df, "LOC_C", "INTENT_C")
intent_bdypt <- create.freq.table(df, "BDYPT", "INTENT_C")
intent_disp <- create.freq.table(df, "DISP", "INTENT_C")

loc_age <- create.freq.table(df, "AGEG4_C", "LOC_C")
diag_sex <- create.freq.table(df, "DIAG", "SEX")
# apply to other vars...

### test ###
chisq.test(loc_age_pooled[3:10, c(2:3, 8)], correct = FALSE)


#### calculate freq for one variable ####
calcFreq <- function(df, var){
  df %>% group_by(!! sym(var)) %>% summarise(n=n()) %>% 
    mutate("freq (%)" = round(n/sum(n)*100, 2)) %>% arrange(desc(`freq (%)`))
}
frq_bdypt <- calcFreq(df, "BDYPT") 
frq_diag <- calcFreq(df, "DIAG")
frq_race <- calcFreq(df, "RACETH_C")
#########################################

###########################################################
########### PLOTS: Demographics of IPV patients ###########
###########################################################

### By diagnosis & sex ###
diag_sex_freq <- df %>%  group_by(DIAG, SEX) %>%  summarise(n=n()) 
# proportion 
ggplot(diag_sex_freq, aes(fill=SEX, y = n, x = DIAG)) + 
  geom_bar(position="fill", stat="identity") + #, position = position_fill(reverse = TRUE)
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual("Sex", values = c("(0) UNK" = "gray", "(1) Male" = "royalblue1", "(2) Female" = "maroon1"))+
  geom_text(aes(label = n), position = position_fill(vjust = 0.5), colour="snow2") + 
  labs(x = "Diagnosis", y = "Proportion", 
       title = "IPV patients w/ Head & Neck Injuries \n by Diagnosis and Sex (proportion)")+
  theme(plot.title = element_text(hjust = 0.5))

# absolute value 
ggplot(diag_sex_freq, aes(x = reorder(DIAG, -n), y=n, fill=SEX)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual("Sex", 
                    breaks = c("(1) Male", "(2) Female", "(0) UNK"), # specify the order of the legend
                    values = c("(0) UNK" = "gray", "(1) Male" = "royalblue1", "(2) Female" = "maroon1")) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), colour="snow2") +
  labs(x = "Diagnosis", y = "Count", 
       title = "IPV patients w/ Head & Neck Injuries \n by Diagnosis and Sex (count)")+
  theme(plot.title = element_text(hjust = 0.5))


age_race_freq <- df %>%  group_by(AGEG4_C, RACETH_C) %>%  summarise(n=n()) 
ggplot(age_race_freq, aes(x = AGEG4_C, y = n, fill= RACETH_C)) + 
  geom_bar(position="fill", stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual("Race/Ethnicity", values = c("(1) White Nh" = "pink1", 
                                                 "(2) Black" = "grey39", "(3) Hispanic" = "gold", 
                                                 "(4) Asian Nh" = "peachpuff", "(5) Am Ind Nh" = "coral1", 
                                                 "(6) Other Nh" = "lightcyan", "(0) UNK" = "gray"))+
                                                 
  geom_text(aes(label = n), position = position_fill(vjust = 0.5), colour="black") + 
  labs(x = "Age Group (years)", y = "Proportion", # maybe change to percentage later
       title = "IPV patients w/ Head & Neck Injuries \n by Age Group and Race (proportion)")+
  theme(plot.title = element_text(hjust = 0.5))

age_race_freq <- df %>%  group_by(AGEG4_C, RACETH_C) %>%  summarise(n=n())
age_race_freq_s <- df %>%  group_by(AGEG4_C, RACETH_C) %>%  summarise(n=n()) %>% 
  filter( !(AGEG4_C %in% c("(01) 00-04", "(03) 10-14", "(14) 65-69", "(15) 70-74", "(16) 75-79", "(17) 80-84", "(18) 85+")))


ggplot(age_race_freq_s, aes(x = AGEG4_C, y = n, fill= RACETH_C)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_manual("Race/Ethnicity", values = c("(1) White Nh" = "pink1", 
                                                 "(2) Black" = "grey39", "(3) Hispanic" = "gold", 
                                                 "(4) Asian Nh" = "peachpuff", "(5) Am Ind Nh" = "coral1", 
                                                 "(6) Other Nh" = "lightcyan", "(0) UNK" = "gray"))+
  
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), colour="black") + 
  labs(x = "Age Group (years)", y = "Count", 
       title = "IPV patients w/ Head & Neck Injuries \n by Age Group and Race")+
  theme(plot.title = element_text(hjust = 0.5))


### By disposition and diagnosis ###
disp_diag_freq <- df %>%  group_by(DISP, DIAG) %>%  summarise(n=n()) 
disp_diag_freq_filtered <- df %>%  group_by(DISP, DIAG) %>%  summarise(n=n()) %>% 
  filter((DISP=="(1) Treated/released" | DISP =="(4) Hospitalized") & n>5)

ggplot(disp_diag_freq_filtered, aes(x = DISP, y = n, fill=DIAG)) + 
  geom_bar(position="fill", stat="identity") + 
  geom_text(aes(label = n), position = position_fill(vjust = 0.5), colour="black") + 
  labs(x = "ED Disposition", y = "Proportion", 
       title = "IPV patients w/ Head & Neck Injuries \n by ED Disposition and Diagnosis (proportion) \n [only included diag w/ count > 5]")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(disp_diag_freq_filtered, aes(x = DISP, y = n, fill=DIAG)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), colour="black") + 
  labs(x = "ED Disposition", y = "Count", 
       title = "IPV patients w/ Head & Neck Injuries \n by ED Disposition and Diagnosis (count) \n [only included diag w/ count > 5]")+
  theme(plot.title = element_text(hjust = 0.5))

### By gender and body parts ###
sex_bdypt_freq <- df %>%  group_by(SEX, BDYPT) %>%  summarise(n=n()) 

ggplot(sex_bdypt_freq, aes(x = SEX, y = n, fill=BDYPT)) + 
  geom_bar(position="fill", stat="identity") + 
  geom_text(aes(label = n), position = position_fill(vjust = 0.5), colour="black") + 
  scale_fill_discrete(name = "Injured Body Part")+
  labs(x = "Gender", y = "Proportion", 
       title = "IPV patients w/ Head & Neck Injuries \n by Gender and Injured Body Part")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(sex_bdypt_freq, aes(x = SEX, y = n, fill=BDYPT)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), colour="black") + 
  scale_fill_discrete(name = "Injured Body Part")+
  labs(x = "Gender", y = "Count", 
       title = "IPV patients w/ Head & Neck Injuries \n by Gender and Injured Body Part")+
  theme(plot.title = element_text(hjust = 0.5))

### By age groups and body parts ###
age_bdypt_freq <- df %>%  group_by(AGEG4_C, BDYPT) %>%  summarise(n=n()) 
age_bdypt_freq_s <- df %>%  group_by(AGEG4_C, BDYPT) %>%  summarise(n=n()) %>% 
  filter( !(AGEG4_C %in% c("(01) 00-04", "(03) 10-14", "(14) 65-69", "(15) 70-74", "(16) 75-79", "(17) 80-84", "(18) 85+")))


ggplot(age_bdypt_freq_s, aes(x = AGEG4_C, y = n, fill=BDYPT)) + 
  geom_bar(position="fill", stat="identity") + 
  geom_text(aes(label = n), position = position_fill(vjust = 0.5), colour="black") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_discrete(name = "Injured Body Part")+
  labs(x = "Age Group (years)", y = "Proportion", 
       title = "IPV patients w/ Head & Neck Injuries \n by Age Group and Injured Body Part")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(age_bdypt_freq_s, aes(x = AGEG4_C, y = n, fill=BDYPT)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), colour="black") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_discrete(name = "Injured Body Part")+
  labs(x = "Age Group (years)", y = "Count", 
       title = "IPV patients w/ Head & Neck Injuries \n by Age Group and Injured Body Part")+
  theme(plot.title = element_text(hjust = 0.5))


### By race and body parts ###
race_bdypt_freq <- df %>%  group_by(RACETH_C, BDYPT) %>%  summarise(n=n()) 

ggplot(race_bdypt_freq, aes(x = RACETH_C, y = n, fill=BDYPT)) + 
  geom_bar(position="fill", stat="identity") + 
  geom_text(aes(label = n), position = position_fill(vjust = 0.5), colour="black") + 
  scale_fill_discrete(name = "Injured Body Part")+
  labs(x = "Race/Ethnicity", y = "Proportion", 
       title = "IPV patients w/ Head & Neck Injuries \n by Race/Ethnicity and Injured Body Part")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(race_bdypt_freq, aes(x = RACETH_C, y = n, fill=BDYPT)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), colour="black") + 
  scale_fill_discrete(name = "Injured Body Part")+
  labs(x = "Race/Ethnicity", y = "Count", 
       title = "IPV patients w/ Head & Neck Injuries \n by Race/Ethnicity and Injured Body Part")+
  theme(plot.title = element_text(hjust = 0.5))



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

