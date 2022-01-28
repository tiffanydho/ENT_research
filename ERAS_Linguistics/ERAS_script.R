pacman::p_load(readxl, dplyr, tidyr, ggplot2)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(naniar)
setwd("C:/Users/user/Documents/ENT_research/ERAS_Linguistics")
filename <- "Data-LOR-trimmed.xlsx"
## Ref: https://www.datanovia.com/en/lessons/t-test-in-r/
# year 2020-21: sheet 1 & 2
# year 2019-20: sheet 12 & 13
data_f <- read_excel(filename, sheet = 12, col_names = TRUE, col_types = NULL, na = "", skip = 0)
data_m <- read_excel(filename, sheet = 13, col_names = TRUE, col_types = NULL, na = "", skip = 0)
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
  mutate(sex = gsub("[fF]", "f", .$sex), 
         sex = gsub("[mMn]", "m", .$sex))

############ Perform Multiple T-Tests w/ Correction #########
# https://www.datanovia.com/en/blog/how-to-perform-multiple-t-test-in-r-for-different-variables/
# objective numbers: scores, pubications, etc.
data.obj <- data[,c(2:4, 6:13)] 
data.obj.long <- data.obj %>% pivot_longer(-sex, names_to = "variable", values_to = "value")
mult.t.tests.obj.bonf <- data.obj.long %>% group_by(variable) %>% t_test(value~sex) %>% adjust_pvalue(method = "bonferroni") %>% add_significance()
mult.t.tests.obj.bonf
mult.t.tests.obj.fdr <- data.obj.long %>% group_by(variable) %>% t_test(value~sex) %>% adjust_pvalue(method = "fdr") %>% add_significance()
mult.t.tests.obj.fdr

plot.obj.fdr <- ggboxplot(data.obj.long, x = "sex", y = "value",
  fill = "sex", palette = "npg", legend = "none",
  ggtheme = theme_pubr(border = TRUE)) +
  facet_wrap(facets = ~variable,  scales = 'free') 
plot.obj.fdr

plot.obj.fdr + stat_compare_means(label =  "p.signif", label.x = 1.5, label.y = 5) 

### add statistical test p-values -> currently doesn't work  
# mult.t.tests.obj.fdr <- mult.t.tests.obj.fdr %>% add_xy_position(x = "sex")
# plot.obj.fdr + stat_pvalue_manual(mult.t.tests.obj.fdr, y.position = 5, label = "p.adj")


# subjective numbers: linguistic scores
data.subj <- data[,c(4, 15:23)]
data.subj.long <- data.subj %>% pivot_longer(-sex, names_to = "variable", values_to = "value")
mult.t.tests.subj.bonf <- data.subj.long %>% group_by(variable) %>% t_test(value~sex) %>% adjust_pvalue(method = "bonferroni") %>% add_significance()
mult.t.tests.subj.bonf
mult.t.tests.subj.fdr <- data.subj.long %>% group_by(variable) %>% t_test(value~sex) %>% adjust_pvalue(method = "fdr") %>% add_significance()
mult.t.tests.subj.fdr

############### Regression & FDR ###################
library(fuzzySim) 
data.for.fdr.obj <- data[,c(4, 2, 3, 6:13)]
data.for.fdr.obj <- data.for.fdr.obj %>% mutate(
  sex = as.numeric(gsub("f", 0, gsub("m", 1, data.for.fdr.obj$sex))) ) %>% 
  as.data.frame() # the FDR func only takes dataframe
FDR(data = data.for.fdr.obj, sp.cols = 1, var.cols = 2:11, family = "binomial")

data.for.fdr.subj <- data[,c(4, 15:23)]
data.for.fdr.subj <- data.for.fdr.subj %>% mutate(
  sex = as.numeric(gsub("f", 0, gsub("m", 1, data.for.fdr.subj$sex))) ) %>% as.data.frame() 
FDR(data = data.for.fdr.subj, sp.cols = 1, var.cols = 2:10, family = "binomial")


#####################################################
####### PART 1: Compare male vs female ##############
#####################################################
data %>% group_by(Sex) %>% get_summary_stats(Clout, type = "mean_sd")
ggplot(data, aes(x=Sex, y=Clout)) + geom_boxplot() # + geom_jitter()

# (1) identify outliers by groups
data %>% group_by(Sex) %>% identify_outliers(Clout) # unexpected output

# (2) normality by groups
## p < 0.05: not normally distributed
## https://thestatsgeek.com/2013/09/28/the-t-test-and-robustness-to-non-normality/
data %>% group_by(Sex) %>% shapiro_test(Clout) 
ggqqplot(data, x = "Clout", facet.by = "Sex", title = "QQ plot: Clout")

# (3) equality of variances
## p < 0.05 -> sig difference between the variances of the two groups -> use Weltch t-test
## p > 0.05 -> equal variance -> use Student t-test
data %>% levene_test(Clout ~ Sex)

data %>% t_test(Clout~Sex, var.equal = TRUE) %>% add_significance()

# Effect size: https://www.simplypsychology.org/effect-size.html
data %>%  cohens_d(Clout ~ Sex, var.equal = TRUE)

#############################################################
########## PART 2: Substitute for Step 1 Score ##############
#############################################################
library(olsrr)
### Diag: Scatter plots with correlations ###
plot(data[,c(2,3, 7:14)], cex=.5, main="Diag: possible step 1 substitute")
reg <- function(x, y, col) abline(lm(y~x), col=col)

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
pairs(data[,c(2,3, 7:14)], upper.panel = panel.lm,
      cex = 0.6, pch = 20, col = adjustcolor(4, .4), cex.labels = 1,
      font.labels = 2, lower.panel = panel.cor, 
      main="Diag: possible step 1 substitute")

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
