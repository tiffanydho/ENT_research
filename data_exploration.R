#install.packages("pacman")
pacman::p_load(readxl, dplyr, tidyr, tidyselect, tibble)
library(ggplot2)
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
  rename(DOB="Date of birth", allergies="List of allergies (separate by commas)")

processed_data <- raw_data[, c("Study ID")]

processed_data <- processed_data %>% 
  mutate(age=calcAge(as.Date(raw_data$DOB))) %>% 
  mutate(num_allergies = countItem(raw_data$allergies)) %>%
  mutate(R_endo_score=rowSums(select(raw_data, starts_with("Right endoscopy", ignore.case = TRUE), -contains("total")))) %>% 
  mutate(L_endo_score=rowSums(select(raw_data, starts_with("Left endoscopy", ignore.case = TRUE), -contains("total")))) 
processed_data <- processed_data %>% 
  mutate(endo_score=rowSums(select(processed_data, one_of("R_endo_score", "L_endo_score"))))

# RSDI survey
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

##### handling CT scores ##### 
raw_data <- raw_data %>% 
  rename(max_r = "Right maxillary score", 
         eth_ra = "Right anterior ethmoid score", 
         eth_rp = "Right posterior ethmoid score", 
         sph_r = "Right sphenoid score", 
         frnt_r = "Right frontal score", 
         omc_r = "right ostiomeatal complex score", 
         max_l = "left maxillary score", 
         eth_la = "Left anterior ethmoid score", 
         eth_lp = "Left posterior ethmoid score", 
         sph_l = "Left sphenoid score", 
         frnt_l = "Left frontal score", 
         omc_l = "Left ostiomeatal complex", 
         total_CT_score = "Lund-Mackay total score (left+right)")

processed_data <- cbind(processed_data, raw_data[c(45:59)]) # c(45:59) are CT scores
data_w_ct_raw <- processed_data %>% filter(!is.na(total_CT_score)) %>% 
  mutate(max0=ifelse(max_l==0,1,0)+ifelse(max_r==0,1,0),
         max1=ifelse(max_l==1,1,0)+ifelse(max_r==1,1,0),
         max2=ifelse(max_l==2,1,0)+ifelse(max_r==2,1,0),
         eth0=ifelse(eth_la==0,1,0)+ifelse(eth_lp==0,1,0)+ifelse(eth_ra==0,1,0)+ifelse(eth_rp==0,1,0),
         eth1=ifelse(eth_la==1,1,0)+ifelse(eth_lp==1,1,0)+ifelse(eth_ra==1,1,0)+ifelse(eth_rp==1,1,0),
         eth2=ifelse(eth_la==2,1,0)+ifelse(eth_lp==2,1,0)+ifelse(eth_ra==2,1,0)+ifelse(eth_rp==2,1,0),
         sph0=ifelse(sph_l==0,1,0)+ifelse(sph_r==0,1,0),
         sph1=ifelse(sph_l==1,1,0)+ifelse(sph_r==1,1,0),
         sph2=ifelse(sph_l==2,1,0)+ifelse(sph_r==2,1,0),
         frnt0=ifelse(frnt_l==0,1,0)+ifelse(frnt_r==0,1,0),
         frnt1=ifelse(frnt_l==1,1,0)+ifelse(frnt_r==1,1,0),
         frnt2=ifelse(frnt_l==2,1,0)+ifelse(frnt_r==2,1,0),
         omc0=ifelse(omc_l==0,1,0)+ifelse(omc_r==0,1,0),
         omc1=ifelse(omc_l==1,1,0)+ifelse(omc_r==1,1,0),
         omc2=ifelse(omc_l==2,1,0)+ifelse(omc_r==2,1,0)) %>% 
  subset(select = -c(12:25)) #remove original individual CT scores

data_w_ct <- data_w_ct_raw %>% pivot_longer(starts_with(c("max","eth","sph", "frnt", "omc")),names_to="sinus",values_to="freq") %>%
  mutate(ctscore=substr(sinus,nchar(sinus),nchar(sinus)),
         sinus=substr(sinus,1,nchar(sinus)-1)) %>% 
  select(-c("age", "R_endo_score", "L_endo_score"))

variable_names <- list(
  "rsdi_emo_score" = "RSDI emotional score",
  "rsdi_func_score" = "RSDI functional score"
)
variable_labeller <- function(variable, value){
  if(variable == "scores") return(variable_names[value])
  else return(levels(data_w_ct$sinus))
}

library(reshape2)
data_w_ct_t <- melt(data_w_ct, id.vars = c("Study ID", "sinus", "freq", "ctscore"), 
                    variable.name = "scores", 
                    value.name = "values")


ggplot(data_w_ct_t,aes(x=freq,y=values,shape=factor(ctscore),color=factor(ctscore)))+
  geom_point()+
  facet_grid(scores~sinus,scales="free")+
  geom_smooth(method='lm',formula=y~x,se=F)

ggplot(data_w_ct,aes(x=freq,y=rsdi_emo_score,shape=factor(ctscore),color=factor(ctscore)))+
  geom_point()+
  facet_wrap(~sinus)+geom_smooth(method='lm',formula=y~x,se=F)

ggplot(data_w_ct,aes(x=rsdi_emo_score,y=freq,shape=factor(ctscore),color=factor(ctscore)))+
  geom_point()+
  facet_wrap(~sinus)+geom_smooth(method='lm',formula=y~x,se=F)



ggplot(data_w_ct,aes(x=freq,y=rsdi_func_score,shape=factor(ctscore),color=factor(ctscore)))+
  geom_point()+
  facet_wrap(~sinus)+geom_smooth(method='lm',formula=y~x,se=F)

ggplot(data_w_ct,aes(x=freq,y=aggression_score,shape=factor(ctscore),color=factor(ctscore)))+
  geom_point()+
  facet_wrap(~sinus)+geom_smooth(method='lm',formula=y~x,se=F)

##### scatterplots w/ correlation #####
plot(processed_data[,c(4, 7:12)], cex=.5, main="full dataset")

reg <- function(x, y, col) abline(lm(y~x), col=col)

panel.lm <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                      cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)  {
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) reg(x[ok], y[ok], col.smooth)
}

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  
  r <- cor(x, y, use = "complete.obs")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  text(0.5, 0.5, txt, cex = 1.1, font = 4)
}

# panel.cor <- function(x, y){
#   usr <- par("usr"); on.exit(par(usr))
#   par(usr = c(0, 1, 0, 1))
#   r <- round(cor(x, y, use = "complete.obs"), digits=2)
#   txt <- paste0("R = ", r)
#   cex.cor <- 0.8/strwidth(txt)
#   text(0.5, 0.5, txt, cex = cex.cor * r)
# }

pairs(processed_data[,c(4, 7:12)], upper.panel = panel.lm,
      cex = 0.5, pch = 18, col = adjustcolor(4, .4), cex.labels = 1,
      font.labels = 2, lower.panel = panel.cor)

pairs(processed_data[,c(4, 7:12)], upper.panel = panel.lm, lower.panel = panel.cor)

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

#data_w_ct <- processed_data[!is.na(processed_data$CT_score), ]
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

plot(model_pruned,3)
library(car)
durbinWatsonTest(model_pruned, na.action = na.omit) ## ERROR: missing values

model_pruned_log_endo <- lm(sqrt(endo_score) ~ num_allergies + rsdi_emo_score + 
                     rsdi_func_score + rsdi_phys_score + mdes_score + 
                     aggression_score, data = processed_data_pruned, na.action = na.exclude)
plot(model_pruned_log_endo, 3)

ols_step_best_subset(model_pruned)
ols_step_best_subset(model_pruned_log_endo)

model_pruned_sqrt <- lm(sqrt(endo_score) ~ num_allergies + sqrt(rsdi_emo_score) + 
                          sqrt(rsdi_func_score) + sqrt(rsdi_phys_score) + mdes_score + 
                          aggression_score, data = processed_data_pruned, na.action = na.exclude)

ols_step_best_subset(model_pruned_sqrt)

##add ct 
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

plot(model_test_loose, 3)

model_test_loose_sqrt_y <- lm(sqrt(rsdi_emo_score)~ endo_score +num_allergies+
                         rsdi_func_score + rsdi_phys_score + mdes_score + 
                         aggression_score, data = processed_data_loose_prune, na.action = na.exclude)

plot(model_test_loose_sqrt_y,3)

ols_step_best_subset(model_test_loose)

num_data <-processed_data_pruned[, sapply(processed_data_pruned, is.numeric)] 
library(ggplot2)
ggplot(gather(num_data, cols, value), aes(x = value)) + 
  geom_histogram(binwidth = 20) + facet_grid(.~cols)

install.packages("Hmisc")
library(Hmisc)
hist.data.frame(processed_data)
hist.data.frame(processed_data_pruned)


### 
transformed_data <- processed_data_pruned %>% 
  mutate(log_endo_score = log(endo_score), 
         log_rsdi_emo_score = log(rsdi_emo_score), 
         log_rsdi_func_score = log(rsdi_func_score), 
         log_rsdi_phys_score = log(rsdi_phys_score), 
         log_mdes_score = log(mdes_score), 
         log_aggression_score = log(aggression_score))
         
hist.data.frame(transformed_data)
model_testt <- lm(rsdi_emo_score ~ endo_score +num_allergies+
                         rsdi_func_score + rsdi_phys_score + mdes_score + 
                         aggression_score, data = processed_data_pruned, na.action = na.exclude)
ols_step_best_subset(model_testt)

model_testt_t <- lm(log_rsdi_emo_score ~ log_endo_score +num_allergies+
                         log_rsdi_func_score + log_rsdi_phys_score + mdes_score + 
                         aggression_score, data = transformed_data, na.action = na.exclude)
ols_step_best_subset(model_testt_t)



####

ggplot(as.data.frame(log_rsdi_phy_score), aes(x=log_rsdi_phy_score))+
  geom_histogram(aes(y = ..density..)) + 
  geom_density()



# reasons for this failure: 
#transformation: log or sqrt
# try: lasso, ridge

######################
# install.packages("writexl")
# library("writexl")
# no_surgery_date <- subset(raw_data, is.na(raw_data$`Date of last surgery`) & !is.na(raw_data$`Last surgery type`), select=c(1:5, 28:29))
# write_xlsx(no_surgery_date, file.path("data","individuals without surgery date.xlsx"),)
