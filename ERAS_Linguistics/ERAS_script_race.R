pacman::p_load(readxl, tidyverse, ggpubr, rstatix)
library(naniar)
library("writexl")

setwd("C:/Users/user/Documents/ENT_research/ERAS_Linguistics")
filename <- "Data-PS-trimmed.xlsx"
## ref: https://www.datanovia.com/en/lessons/anova-in-r/
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
  filter(!is.na(race)) %>% 
  mutate(step_1 = as.numeric(step_1), step_2 = as.numeric(step_2)) %>%
  mutate(race = gsub("Asian", "Asian", .$race, ignore.case = TRUE)) %>% 
  mutate(race = gsub("^.*black.*$", "Black", .$race, ignore.case = TRUE)) %>%
  mutate(race = gsub("^.*hispanic.*$", "Hispanic", .$race, ignore.case = TRUE)) %>% 
  mutate(race = gsub("^.*egyptian.*$|^.*iranian.*$|^.*middle[ |-]eastern.*$|other", 
                     "Other", .$race, ignore.case = TRUE ))

# GOALS:
# step 1 & 11 subjective variables: control for step 1
# both years separately & combined years

### INDIVIDUAL CYCLE ###
# data_f <- read_excel(filename, sheet = 12, col_names = TRUE, col_types = NULL, na = "", skip = 0)
# data_m <- read_excel(filename, sheet = 13, col_names = TRUE, col_types = NULL, na = "", skip = 0)
# data <- rbind(data_f, data_m) %>%
#   rename(step_1 = "Step 1", step_2 = "Step 2", sex = "Sex", race = "Self-Identification 1",
#          peer_art_abs = "Peer Reviewed Journal Articles/Abstracts",
#          extra_art_abs = "(Other than Published) Peer Reviewed Journal Articles/Abstracts",
#          book_chap = "Book Chapter (Peer Reviewed)", poster = "Poster Presentation",
#          oral = "Oral Presentation", online_pub_peer = "Online Publication (Peer Reviewed)",
#          online_pub_non_peer = "Non Peer Reviewed Online Publication",
#          other_art = "Other Articles") %>%
#   replace_with_na_all(condition = ~.x =="None") %>%
#   filter(!is.na(race)) %>% 
#   mutate(step_1 = as.numeric(step_1), step_2 = as.numeric(step_2)) %>%
#   mutate(race = gsub("Asian", "Asian", .$race, ignore.case = TRUE)) %>% 
#   mutate(race = gsub("^.*black.*$", "Black", .$race, ignore.case = TRUE)) %>%
#   mutate(race = gsub("^.*hispanic.*$", "Hispanic", .$race, ignore.case = TRUE)) %>% 
#   mutate(race = gsub("^.*egyptian.*$|^.*iranian.*$|^.*middle[ |-]eastern.*$|other", 
#                      "Other", .$race, ignore.case = TRUE ))

data$race <- factor(data$race) 
summary(data$race)
# data %>% group_by(race) %>% get_summary_stats(step_1, type = "mean_sd")
### 2019-20
# gsub: multiple different templates & ignore capitalization 
# egytian, iranian, middle eastern -> other

############# CHECK ASSUMPTIONS ###################
##### extreme outliers #####
df_outliers <- data.frame(variable = character(), value = double(), 
                          race = character(), is.outlier = character(), 
                          is.extreme = character())
var_list <- names(data[c(2,3, 15:25)]) #leave out obj var (6:13) for now
num_vars <- length(c(2,3, 15:25))
for (i in 1:num_vars){
  df <- data %>% group_by(race) %>% identify_outliers(var_list[i]) %>%
    select(race, var_list[i], is.outlier, is.extreme) %>% 
    mutate(variable = toString(var_list[i]), .after = race) %>% 
    rename(value = var_list[i])
  df_outliers <- rbind(df_outliers, df)
}

df_outliers <- df_outliers[, c(2,3,1,4,5)] # reorder column
write_xlsx(df_outliers,"./ERAS_race_outlier_check_combined_PS.xlsx")

## boxplot to visualize outliers ##
boxplot_list <- list()
for (i in 1:num_vars) {
  p = ggboxplot(data, x = "race", y = var_list[i]) +
    ggtitle(paste("Outlier check:", var_list[i], sep = " "))
  boxplot_list[[i]] = p
}
pdf("ERAS_race_outlier_check_combined_PS.pdf")
for (i in 1:num_vars) {
  print(boxplot_list[[i]])
}
dev.off()
# if non-normal -> Kruskal-Wallis (non-parametric eq. to one-way ANOVA)

# Plot separate ggplot figures in a loop
QQplot_list <- list()
for (i in 1:num_vars) {
  p = ggqqplot(data, var_list[i], facet.by = "race") + 
    ggtitle(paste("Normality check:", var_list[i], sep = " "))
  QQplot_list[[i]] = p
}
pdf("ERAS_race_normality_check_combined_PS.pdf")
for (i in 1:num_vars) {
  print(QQplot_list[[i]])
}
dev.off()

# shapiro: p>0.05 -> normal
shap_output <- lapply(data[, c(2,3, 15:25)], shapiro.test)
shap_result <- sapply(shap_output, `[`, c("statistic","p.value")) %>% t %>%
  as_tibble(rownames = NA) %>% mutate(sig = if_else(p.value<0.05, "*", "ns"))

# Note that, if sample size > 50, the normal QQ plot is preferred 
# because at larger sample sizes the Shapiro-Wilk test becomes 
# very sensitive even to a minor deviation from normality.


##### homogeneity of variance #####
# levene: p > 0.05, assumption met
# if not met, use Welch one-way ANOVA, e.g. welch_anova_test()
# data %>% levene_test(step_1 ~ race)

levene_output <- lapply(data[, c(2,3, 15:25)], function(x) levene_test(x ~ race, data = data))

levene_result <- sapply(levene_output, `[`, c("statistic","p")) %>% t %>% 
  as_tibble(rownames = NA) %>% mutate(sig = if_else(p<0.05, "*", "ns"))

###################### STATISTICAL TESTS ######################
### single variable ###
# data %>% anova_test(step_1 ~ race)
# kruskal.test(step_1 ~ race, data = data) 


### all variables ###
data$race <- factor(data$race) #might combine with the first line of code
formulae <- lapply(colnames(data)[c(2:3, 15:25)], function(x) as.formula(paste0(x, " ~ race")))

# one-way ANOVA
anova_res <- lapply(formulae, function(x) summary(aov(x, data = data)))
names(anova_res) <- format(formulae)
anova_res

# Welch one-way ANOVA, welch_anova_test()
welch_anova_res <- lapply(formulae, function(x) welch_anova_test(x, data = data))
names(welch_anova_res) <- format(formulae)
welch_anova_res

# Kruskal-Wallis 
kw_res <- lapply(formulae, function(x) kruskal.test(x, data = data))
names(kw_res) <- format(formulae)
kw_res

# post-hoc test
tukey_res <- lapply(formulae, function(x) tukey_hsd(data, x))
names(tukey_res) <- format(formulae)
tukey_res

## pair-wise confidence interval 
data %>% tukey_hsd(Ability ~ race)

model <- aov(Tone ~ race, data=data)
par(mar = c(3, 6, 3, 3))
plot(TukeyHSD(model, conf.level=.95), las = 2, cex.axis=0.8)
abline(v = 0, col="red", lwd=1, lty=2)


