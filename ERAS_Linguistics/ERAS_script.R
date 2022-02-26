pacman::p_load(readxl, dplyr, tidyr, ggplot2)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(naniar)
library("writexl")

setwd("C:/Users/user/Documents/ENT_research/ERAS_Linguistics")
filename <- "Data-LOR-trimmed.xlsx"
## Ref: https://www.datanovia.com/en/lessons/t-test-in-r/
# year 2020-21: sheet 1 & 2
# year 2019-20: sheet 12 & 13


# data_f_20 <- read_excel(filename, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
# data_m_20 <- read_excel(filename, sheet = 2, col_names = TRUE, col_types = NULL, na = "", skip = 0)
# data_f_19 <- read_excel(filename, sheet = 12, col_names = TRUE, col_types = NULL, na = "", skip = 0)
# data_m_19 <- read_excel(filename, sheet = 13, col_names = TRUE, col_types = NULL, na = "", skip = 0)
# data <- rbind(data_f_20, data_m_20, data_f_19, data_m_19) %>%
#   rename(step_1 = "Step 1", step_2 = "Step 2", sex = "Sex", race = "Self-Identification 1",
#          peer_art_abs = "Peer Reviewed Journal Articles/Abstracts",
#          extra_art_abs = "(Other than Published) Peer Reviewed Journal Articles/Abstracts",
#          book_chap = "Book Chapter (Peer Reviewed)", poster = "Poster Presentation",
#          oral = "Oral Presentation", online_pub_peer = "Online Publication (Peer Reviewed)",
#          online_pub_non_peer = "Non Peer Reviewed Online Publication",
#          other_art = "Other Articles") %>%
#   replace_with_na_all(condition = ~.x =="None") %>%
#   mutate(step_1 = as.numeric(step_1), step_2 = as.numeric(step_2)) %>%
#   mutate(sex = gsub("[fF]", "f", .$sex)) %>%
#   mutate(sex = gsub("[mMn]", "m", .$sex))
# data_f <- filter(data, sex == "f")
# data_m <- filter(data, sex == "m")

data_f <- read_excel(filename, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
data_m <- read_excel(filename, sheet = 2, col_names = TRUE, col_types = NULL, na = "", skip = 0)
data <- rbind(data_f, data_m) %>%
  rename(step_1 = "Step 1", step_2 = "Step 2", sex = "Sex", race = "Self-Identification 1",
         peer_art_abs = "Peer Reviewed Journal Articles/Abstracts",
         extra_art_abs = "(Other than Published) Peer Reviewed Journal Articles/Abstracts",
         book_chap = "Book Chapter (Peer Reviewed)", poster = "Poster Presentation",
         oral = "Oral Presentation", online_pub_peer = "Online Publication (Peer Reviewed)",
         online_pub_non_peer = "Non Peer Reviewed Online Publication",
         other_art = "Other Articles") %>%
  replace_with_na_all(condition = ~.x =="None") %>%
  mutate(step_1 = as.numeric(step_1), step_2 = as.numeric(step_2)) %>%
  mutate(sex = gsub("[fF]", "f", .$sex)) %>%
  mutate(sex = gsub("[mMn]", "m", .$sex))
data_f <- filter(data, sex == "f")
data_m <- filter(data, sex == "m")

################ Check t Test Assumptions ###################
### normality by groups
# p < 0.05: not normally distributed
# https://thestatsgeek.com/2013/09/28/the-t-test-and-robustness-to-non-normality/

cat.sig <- function(p.value, alpha = 0.05){
  if(p.value < alpha) return("*")
  else return("ns")
}

shap_f <- lapply(data_f[, c(2,3, 6:13, 15:25)], shapiro.test)
shapres_f <- sapply(shap_f, `[`, c("statistic","p.value")) %>% t %>% as_tibble(rownames = NA) %>% 
  mutate(sig = if_else(p.value<0.05, "*", "ns"))

shap_m <- lapply(data_m[, c(2,3, 6:13, 15:25)], shapiro.test)
shapres_m <- sapply(shap_m, `[`, c("statistic","p.value")) %>% t %>% as_tibble(rownames = NA) %>% 
  mutate(sig = if_else(p.value<0.05, "*", "ns"))

#### check outliers
boxplot(data_f$achieve, data_m$achieve, names = c("female", "male"), col = c("pink","lightblue"), 
        ylab = "'achieve' score", main = "Checking for Outliers - 'achieve' \n 2019-20")
boxplot(data_f$Grindstone, data_m$Grindstone, names = c("female", "male"), col = c("pink","lightblue"), 
        ylab = "'Grindstone' score", main = "Checking for Outliers - 'Grindstone' \n 2019-20")

#data %>% group_by(sex) %>% shapiro_test(Clout) 
#ggqqplot(data, x = "Clout", facet.by = "sex", title = "QQ plot: Clout")

### equality of variances
# p < 0.05 -> sig difference between the variances of the two groups -> use Weltch t-test
# p > 0.05 -> equal variance -> use Student t-test
data %>% levene_test(Standout ~ sex)

############ Perform Multiple Mann-Whitney U Test w/ Correction #########
data.obj <- data[,c(2:4, 6:13)] 
data.obj.long <- data.obj %>% pivot_longer(-sex, names_to = "variable", values_to = "value") 
mult.wilcox.tests.obj.bonf <- data.obj.long %>% group_by(variable)  %>% 
  wilcox_test(value~sex) %>% adjust_pvalue(method = "bonferroni") %>% add_significance()
mult.wilcox.tests.obj.bonf
mult.wilcox.tests.obj.fdr <- data.obj.long %>% group_by(variable)  %>% 
  wilcox_test(value~sex) %>% adjust_pvalue(method = "BH") %>% add_significance()
mult.wilcox.tests.obj.fdr 

data.subj <- data[,c(4, 15:25)] 
data.subj.long <- data.subj %>% pivot_longer(-sex, names_to = "variable", values_to = "value")

mult.wilcox.tests.subj.bonf <- data.subj.long %>% group_by(variable) %>% 
  wilcox_test(value~sex) %>% adjust_pvalue(method = "bonferroni") %>% add_significance()
mult.wilcox.tests.subj.bonf

mult.wilcox.tests.subj.fdr <- data.subj.long %>% group_by(variable) %>% 
  wilcox_test(value~sex) %>% adjust_pvalue(method = "fdr") %>% add_significance()
mult.wilcox.tests.subj.fdr

## individual test on sided-ness
data %>% wilcox_test(power~sex, alternative = "less")


### replace with t-test results before adjusting for multiple testing 
wilcox.tests.subj <- data.subj.long %>% group_by(variable) %>% wilcox_test(value~sex) 
t.res.standout <- data %>% t_test(Standout~sex, var.equal = TRUE) %>% add_significance()

wilcox.n.t.tests.subj.bonf <- wilcox.tests.subj %>% 
  mutate(statistic=ifelse(variable=="Standout", t.res.standout$statistic, statistic), 
         p=ifelse(variable=="Standout", t.res.standout$p, p))%>% 
  adjust_pvalue(method = "bonferroni") %>% add_significance()
wilcox.n.t.tests.subj.bonf

wilcox.n.t.tests.subj.fdr <- wilcox.tests.subj %>% 
  mutate(statistic=ifelse(variable=="Standout", t.res.standout$statistic, statistic), 
         p=ifelse(variable=="Standout", t.res.standout$p, p))%>% 
  adjust_pvalue(method = "fdr") %>% add_significance()
wilcox.n.t.tests.subj.fdr

############ Perform Multiple T-Tests w/ Correction #########
# https://www.datanovia.com/en/blog/how-to-perform-multiple-t-test-in-r-for-different-variables/
# objective vars: scores, pubications, etc.


############ Perform Multiple T-Tests w/ Correction #########
data.obj <- data[,c(2:4, 6:13)] 
data.obj.long <- data.obj %>% pivot_longer(-sex, names_to = "variable", values_to = "value")
mult.t.tests.obj.bonf <- data.obj.long %>% group_by(variable) %>% t_test(value~sex) %>% 
  adjust_pvalue(method = "bonferroni") %>% add_significance()
mult.t.tests.obj.bonf

mult.t.tests.obj.fdr <- data.obj.long %>% group_by(variable) %>% t_test(value~sex) %>% adjust_pvalue(method = "fdr") %>% add_significance()
mult.t.tests.obj.fdr

# subjective vars: from linguistic algorithm
data.subj <- data[,c(4, 15:25)] 
data.subj.long <- data.subj %>% pivot_longer(-sex, names_to = "variable", values_to = "value")
mult.t.tests.subj.bonf <- data.subj.long %>% group_by(variable) %>% t_test(value~sex) %>% adjust_pvalue(method = "bonferroni") %>% add_significance()
mult.t.tests.subj.bonf
mult.t.tests.subj.fdr <- data.subj.long %>% group_by(variable) %>% t_test(value~sex) %>% adjust_pvalue(method = "fdr") %>% add_significance()
mult.t.tests.subj.fdr

############### Account for correlation: Regression & FDR ###################
library(fuzzySim)
data.for.fdr.obj <- data[,c(4, 2, 3, 6:13)]
data.for.fdr.obj <- data.for.fdr.obj %>% mutate(
  sex = as.numeric(gsub("f", 0, gsub("m", 1, data.for.fdr.obj$sex))) ) %>% 
  as.data.frame() # the FDR func only takes dataframe
FDR(data = data.for.fdr.obj, sp.cols = 1, var.cols = 2:11, family = "binomial")

data.for.fdr.subj <- data[,c(4, 15:25)]
data.for.fdr.subj <- data.for.fdr.subj %>% mutate(
  sex = as.numeric(gsub("f", 0, gsub("m", 1, data.for.fdr.subj$sex))) ) %>% as.data.frame() 
FDR(data = data.for.fdr.subj, sp.cols = 1, var.cols = 2:12, family = "binomial")

########## Finding substitute for Step 1 score ##############
library(olsrr)
library(lessR) # shouldn't need this
### Diagnostic Scatter Plots with correlations ###
panel.lm <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                      cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...){
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) reg(x[ok], y[ok], col.smooth)
}

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  
  r <- cor(x, y, use = "complete.obs")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  text(0.5, 0.5, txt, cex = 1.1, font = 4)
}

pairs(data[,c(2,3, 6:13)], upper.panel = panel.lm, 
      cex = 0.6, pch = 20, col = adjustcolor(4, .4), cex.labels = 1,
      font.labels = 2, lower.panel = panel.cor, 
      main="Diag: possible step 1 substitute")

##### paired box plot with lines #######
library(reshape2)
data.melted.step.240 <- data[data$step_1 > 240, c(2,3)] %>%  melt() 
ggpaired(data.melted.step.240, x = "variable", y = "value",
         line.color = "gray", line.size = 0.4, palette = "npg")


data.melted.step <- data[, c(2,3)] %>%  melt() 
ggpaired(data.melted.step, x = "variable", y = "value",
         line.color = "gray", line.size = 0.4, palette = "npg")

######### scatter plot with correlation ###############
### correlation of the entire dataset
sp <- ggscatter(data, x = "step_1", y = "step_2",
                add = "reg.line",  # Add regressin line
                add.params = list(color = "red", fill = "lightgray"), # Customize reg. line
                conf.int = TRUE, # Add confidence interval
                xlim = c(180,280), ylim = c(180,280), 
                main = "2019-20 & 2020-21 \n step 1 vs step 2 scores") +
  theme(plot.title = element_text(hjust = 0.5))
sp + stat_cor(method = "pearson", label.x = 180, label.y = 275) # Add correlation coefficient

### subset analysis: 3 groups
#install.packages("devtools")
library(devtools)
#install_github("kassambara/easyGgplot2")
library(easyGgplot2)

# http://www.sthda.com/english/wiki/ggplot2-scatterplot-easy-scatter-plot-using-ggplot2-and-r-statistical-software
step.data <- data %>% select(step_1, step_2) %>% 
  mutate(step_group = ifelse(data$step_1<220, "low", ifelse(data$step_1>250, "high", "middle"))) %>% 
  na.omit() %>%  as.data.frame()

ggplot2.scatterplot(data=step.data, xName='step_1',yName='step_2', size=2,
                    groupName="step_group", 
                    groupColors=c('springgreen4','royalblue1','violetred1'),
                    addRegLine=TRUE, addConfidenceInterval=TRUE, setShapeByGroupName=TRUE,
                    backgroundColor="white", 
                    xtitle="Step 1 score", ytitle="Step 2 score",
                    mainTitle="Step 1 vs Step 2 cutoffs @ 220 and 250")


## testing correlation differences
# https://stats.stackexchange.com/questions/33013/what-test-can-i-use-to-compare-slopes-from-two-or-more-regression-models



##### CAN DELETE #####

### can remove the upper bound once corrected the data
data.melted.step <- data[data$step_1 <300, c(2,3)] %>%  melt() 
data.melted.others <- data[data$step_1 <300, c(6:13)] %>%  melt() 
#boxplot(data[,c(6:13)])
ggplot(data.melted.step, aes(x=variable, y = value)) + geom_boxplot() +
  ggtitle("Data Exploration: Distribution of Step Scores") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data.melted.others, aes(x=variable, y = value)) + geom_boxplot() +
  ggtitle("Data Exploration: Distribution of the Remaining Objective Variables") +
  theme(plot.title = element_text(hjust = 0.5))


### fitting model ###
model_log <- lm(sqrt(step_1) ~ peer_art_abs + extra_art_abs +
            book_chap + poster + oral + online_pub_peer + online_pub_non_peer + 
            other_art, data = data, na.action = na.exclude)
plot(model_log,1)
summary(model)
hist(data$step_1)

model <- lm(step_1 ~ step_2+ peer_art_abs + extra_art_abs +
              book_chap + poster + oral + online_pub_peer + online_pub_non_peer + 
              other_art, data = data, na.action = na.exclude)
plot(model,1)
summary(model)

model_2 <- lm(step_1 ~ step_2, data = data, na.action = na.exclude)
summary(model_2)

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page

plot(model_2, 3)
