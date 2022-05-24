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

### COMBINE TWO CYCLES: 2019-20 & 2020-21 ###
data_f_20 <- read_excel(filename, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
data_m_20 <- read_excel(filename, sheet = 2, col_names = TRUE, col_types = NULL, na = "", skip = 0)
data_f_19 <- read_excel(filename, sheet = 12, col_names = TRUE, col_types = NULL, na = "", skip = 0)
data_m_19 <- read_excel(filename, sheet = 13, col_names = TRUE, col_types = NULL, na = "", skip = 0)
data <- rbind(data_f_20, data_m_20, data_f_19, data_m_19) %>%
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

### INDIVIDUAL CYCLE ###
# data_f <- read_excel(filename, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
# data_m <- read_excel(filename, sheet = 2, col_names = TRUE, col_types = NULL, na = "", skip = 0)
# data <- rbind(data_f, data_m) %>%
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

################ Check t Test Assumptions ###################
### normality by groups
# p < 0.05: not normally distributed
# https://thestatsgeek.com/2013/09/28/the-t-test-and-robustness-to-non-normality/

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
# https://alvaroaguado3.github.io/forcing-regression-coefficients-in-r-part-i/
### Diagnostic Scatter Plots with correlations ###
reg <- function(x, y, col) abline(lm(y ~ x), col = col) 
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
      font.labels = 2, lower.panel = panel.cor)

######### paired box plot with lines #########
library(reshape2)
data.melted.step.240 <- data[data$step_1 > 240, c(2,3)] %>%  melt() 
ggpaired(data.melted.step.240, x = "variable", y = "value",
         line.color = "gray", line.size = 0.4, palette = "npg")


data.melted.step <- data[, c(2,3)] %>%  melt() 
ggpaired(data.melted.step, x = "variable", y = "value",
         line.color = "gray", line.size = 0.4, palette = "npg")

######### summary statistics of metrics ###############
summary(data$step_1) 
summary(data$step_2) 
summary(data$peer_art_abs) # peer_art_abs=(0, 35)
summary(data$oral) #(0, 25)
summary(data$poster) #(0, 46)

funs <- list(min=min, max=max, median=median, mean=mean, sd=sd)
sapply(funs, function(x) sapply(data[c(2,3, 6:13)], x, na.rm=TRUE))

######### scatter plot of step 1 vs step 2 for combined cycle ######### 
library(ggh4x) # for minor tick marks

gglayer_theme <- list(
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14)),
  guides(x = "axis_minor", y = "axis_minor"),
  scale_x_continuous(breaks = seq(from =200, to = 273, by = 10), 
                    minor_breaks = seq(from = 195, to =273, by = 5))
)

sp_step2 <- ggscatter(data, x = "step_1", y = "step_2", size = 1.2,
                add = "reg.line",  # Add regression line
                add.params = list(color = "red", fill = "lightgray"), # Customize reg. line
                conf.int = TRUE, # Add confidence interval
                xlim = c(195,273), ylim = c(193,282),  
                xlab = "Step 1 Score", ylab = "Step 2 Score") +
  stat_cor(method = "pearson", r.digits = 2, p.accuracy = .001,
              label.x = 195, label.y = 275)+ # Add correlation coefficient
  gglayer_theme

### step_1 vs peer reviewed abstract ###
sp_abs <- ggscatter(data, x = "step_1", y = "peer_art_abs", size = 1.2,
                      add = "reg.line", conf.int = TRUE, 
                      add.params = list(color = "red", fill = "lightgray"), 
                      xlim = c(195,273), ylim = c(0,36),
                      xlab = "Step 1 Score", ylab = "Peer Reviewed Abstracts (#)") +
  stat_cor(method = "pearson", r.digits = 2, p.accuracy = .001,
                    label.x = 195, label.y = 33.16)+
  gglayer_theme
  
### Q: abs=35 is an obv outlier, but there are many others. Should I remove them? NO
# out <- boxplot.stats(data$peer_art_abs)$out %>% unique() %>% sort()
# boxplot(data$peer_art_abs, ylab = "peer_art_abs")
# mtext(paste("Outliers: ", paste(out, collapse = ", ")))

### step_1 vs oral ###
sp_oral <- ggscatter(data, x = "step_1", y = "oral", size = 1.2,
                    add = "reg.line",  
                    add.params = list(color = "red", fill = "lightgray"), 
                    conf.int = TRUE, 
                    xlim = c(195,273), ylim = c(0,26),
                    xlab = "Step 1 Score", ylab = "Oral Presentations (#)") +
  stat_cor(method = "pearson", r.digits = 2, p.accuracy = .001,
                  label.x = 195, label.y = 23.955)+
  gglayer_theme

# out_oral <- boxplot.stats(data$oral)$out %>% unique() %>% sort()
# boxplot(data$oral, ylab = "oral")
# mtext(paste("Oral Outliers: ", paste(out_oral, collapse = ", ")))

### step_1 vs posters ###
sp_poster <- ggscatter(data, x = "step_1", y = "poster", size = 1.2,
                     add = "reg.line", 
                     add.params = list(color = "red", fill = "lightgray"), 
                     conf.int = TRUE, 
                     xlim = c(195,273), ylim = c(0,47),
                     xlab = "Step 1 Score", ylab = "Poster Presentations (#)") +
  stat_cor(method = "pearson", r.digits = 2, p.accuracy = .001,
                  label.x = 195, label.y = 43.3)+
  gglayer_theme

# out_poster <- boxplot.stats(data$poster)$out %>% unique() %>% sort()
# boxplot(data$poster, ylab = "poster")
# mtext(paste("Poster Outliers: ", paste(out_poster, collapse = ", ")))


### 4-panel correlation: step 1 vs step 2, peer reviewed abstract, posters, oral ###
library(cowplot)
plot_grid(sp_step2, sp_abs, sp_oral, sp_poster, labels=c("A", "B", "C", "D"), ncol = 2, nrow = 2)




### subset analysis: 3 groups
#install.packages("devtools")
library(devtools)
#install_github("kassambara/easyGgplot2")
library(easyGgplot2)

# http://www.sthda.com/english/wiki/ggplot2-scatterplot-easy-scatter-plot-using-ggplot2-and-r-statistical-software
step.data <- data %>% select(step_1, step_2) %>% 
  mutate(score_group = ifelse(data$step_1<220, "Low", ifelse(data$step_1>250, "High", "Middle"))) %>% 
  na.omit() %>%  as.data.frame()
step.data$score_group <- factor(step.data$score_group, levels = c("High", "Middle", "Low")) #rearranage order
#mtcars$cyl <- as.factor(mtcars$cyl)
colnames(step.data)[which(names(step.data) == "score_group")] <- "Score Tier"

ggplot2.scatterplot(data=step.data, xName='step_1',yName='step_2', size=2,
                    groupName="Score Tier", 
                    groupColors=c('springgreen4','royalblue1','violetred1'),
                    addRegLine=TRUE, addConfidenceInterval=TRUE, setShapeByGroupName=TRUE,
                    backgroundColor="white", 
                    xtitle="Step 1 score", ytitle="Step 2 score",
                    mainTitle="Step 1 vs Step 2 cutoffs @ 220 and 250")



ggplot(step.data, aes(x=step_1, y=step_2, color=score_group, shape=score_group))+ 
  geom_point() +
  geom_smooth(method=lm)
### start here ### 5/23/22

ggplot2.scatterplot(data=step.data, xName='step_1',yName='step_2', size=2,
                    groupName="score_group", 
                    groupColors=c('springgreen4','royalblue1','violetred1'),
                    addRegLine=TRUE, addConfidenceInterval=TRUE, setShapeByGroupName=TRUE,
                    backgroundColor="white", 
                    xtitle="Step 1 score", ytitle="Step 2 score",
                    mainTitle="Step 1 vs Step 2 cutoffs @ 220 and 250")


# ggplot(data=step.data, aes(x='step_1', y='step_2', color = 'score_group'))+
#   geom_point(size = 10)
       
                # size=2,
                # groupColors=c('springgreen4','royalblue1','violetred1'),
                # addRegLine=TRUE, addConfidenceInterval=TRUE, setShapeByGroupName=TRUE,
                # backgroundColor="white", 
                # xtitle="Step 1 score", ytitle="Step 2 score",
                # mainTitle="Step 1 vs Step 2 cutoffs @ 220 and 250")

##########################  REGRESSION MODEL ########################## 
library(tidyverse)
library(broom)
model <- lm(step_1 ~ step_2, data = data)
model.all <- lm(step_1 ~ step_2 + peer_art_abs + extra_art_abs + book_chap + poster + oral + online_pub_peer + online_pub_non_peer +
                  other_art, data = data)
summary(model.all)
model.all.2 <- lm(step_1 ~ step_2 + online_pub_non_peer , data = data)
summary(model.all.2)

# library(MASS)
# null<-lm(step_1~ 1, data=data.no.na) # 1 here means the intercept 
# full<-lm(step_1~ step_2 + peer_art_abs + extra_art_abs + book_chap + poster + oral + online_pub_peer + online_pub_non_peer +
#            other_art, data = data.no.na)
# stepAIC(null, scope=list(lower=null, upper=full),  data=data.no.na, direction='both')
# 
# model.diag.metrics <- augment(model)
# head(model.diag.metrics)
# 
# ggplot(model.diag.metrics, aes(step_2, step_1)) +
#   geom_point() +
#   stat_smooth(method = lm, se = FALSE) +
#   geom_segment(aes(xend = step_2, yend = .fitted), color = "red", size = 0.3)

par(mar = c(2, 2.5, 1.5, 1.5))
par(mfrow = c(2, 2))
plot(model)

library(ggfortify)
autoplot(model)
# Add observations indices and
# drop some columns (.se.fit, .sigma) for simplification
model.diag.metrics <- model.diag.metrics %>%
  mutate(index = 1:nrow(model.diag.metrics)) %>%
  select(index, everything(), -.sigma)
# Inspect the data
head(model.diag.metrics, 4)

#par("mar")
par(mar=c(1,1,1,1))
plot(model, 4, id.n = 5) # Cook's distance
plot(model, 5) # Residuals vs Leverage

model.diag.metrics %>% top_n(3, wt = .cooksd)


summary(model)

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
