pacman::p_load(readxl, tidyverse, ggpubr, rstatix)
library(naniar)
library("writexl")

setwd("C:/Users/user/Documents/ENT_research/ERAS_Linguistics")
filename <- "Data-LOR-trimmed.xlsx"
## ref: https://www.datanovia.com/en/lessons/anova-in-r/
# year 2020-21: sheet 1 & 2
# year 2019-20: sheet 12 & 13

### COMBINE TWO CYCLES: 2019-20 & 2020-21 ###
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
#   filter(!is.na(race)) %>% 
#   mutate(step_1 = as.numeric(step_1), step_2 = as.numeric(step_2)) %>%
#   mutate(race = gsub("Asian", "Asian", .$race, ignore.case = TRUE)) %>% 
#   mutate(race = gsub("^.*black.*$", "Black", .$race, ignore.case = TRUE)) %>%
#   mutate(race = gsub("^.*hispanic.*$", "Hispanic", .$race, ignore.case = TRUE)) %>% 
#   mutate(race = gsub("^.*egyptian.*$|^.*iranian.*$|^.*middle[ |-]eastern.*$|other", 
#                      "Other", .$race, ignore.case = TRUE ))


### INDIVIDUAL CYCLE ###
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
  filter(!is.na(race)) %>%
  mutate(step_1 = as.numeric(step_1), step_2 = as.numeric(step_2)) %>%
  mutate(race = gsub("Asian", "Asian", .$race, ignore.case = TRUE)) %>%
  mutate(race = gsub("^.*black.*$", "Black", .$race, ignore.case = TRUE)) %>%
  mutate(race = gsub("^.*hispanic.*$", "Hispanic", .$race, ignore.case = TRUE)) %>%
  mutate(race = gsub("^.*egyptian.*$|^.*iranian.*$|^.*middle[ |-]eastern.*$|other",
                      "Other", .$race, ignore.case = TRUE ))

data$race <- factor(data$race) 
summary(data$race)

# for each variable, get summary stats for each racial group 
mylist <- list()
for(col.name in colnames(data[c(2,3, 15:25)]) ){ 
  mylist <- c(mylist, paste("-------------", "SUMMARY of", col.name,
                            "-------------", sep = " "))
  mylist <- c(mylist, tapply(data[[col.name]], data$race, summary) )
}
capture.output(mylist, file = "ERAS_race_variable_summaries_2019-20_LOR.txt")


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
write_xlsx(df_outliers,"./ERAS_race_outlier_check_combined_LOR.xlsx")

## boxplot to visualize outliers ##
boxplot_list <- list()
for (i in 1:num_vars) {
  p = ggboxplot(data, x = "race", y = var_list[i]) +
    ggtitle(paste("Outlier check:", var_list[i], sep = " "))
  boxplot_list[[i]] = p
}
pdf("ERAS_race_outlier_check_combined_LOR.pdf")
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
pdf("ERAS_race_normality_check_combined_LOR.pdf")
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

#################### GRAPH: one graph for each variable ##################
### Goal: generate a plot for each variable for the combined dataset ###
# currently using 'step 1' as example

### boxplot ###
library(ggpubr)
all.col.names <-colnames(data[c(2, 3, 15:25)]) 
all.var.names <- c("Step 1", "Step 2", "'Standout'", "'Ability'","'Grindstone'", "'Teaching'", 
                   "'Research'", "'Analytic'", "'Clout'", "'Authenticity'", "'Tone'",
                   "'Achievement'", "'Power'") 
boxplot_list <- list()
for(col.name in all.col.names){        
  i <- which(all.col.names == col.name)
  var.name <-  all.var.names[i]
  
  stat <- tukey_hsd(data, eval(parse(text = col.name)) ~ race)
  stat.sig <- stat %>% filter(p.adj.signif!="ns")
  sep <- ( max(data[col.name], na.rm = TRUE) - min(data[col.name], na.rm = TRUE) ) * 0.06 # proportional to range
  start_pos <- max(data[col.name], na.rm = TRUE) + sep
  num_compare <- nrow(stat.sig)
  num_compare <- ifelse(num_compare==0, 1, num_compare) # set num_compare that's 0 to 1: avoid creating negative sequence below 
  y_pos <- seq(start_pos, start_pos+(num_compare-1)*sep, by=sep) 
  p <- ggboxplot(data = data, x = "race", y = col.name, color = "race", palette = "npg") +
        stat_pvalue_manual(stat.sig, label = "{p.adj.signif}", y.position = y_pos) + # p = {p.adj}
        labs(title= paste(var.name,"Score by Self-Identified Race/Ethnicity \n 2019-20 & 2020-21", sep = " "),
             x ="Self-Identified Race/Ethnicity", y = paste(var.name, "Score", sep = " ") )+
        theme(plot.title = element_text(hjust = 0.5), #color = "dodgerblue3"
              plot.subtitle = element_text(hjust = 0.5))+
        theme(legend.position="none", text = element_text(size = 15),
              plot.margin = margin(10,10,10,10)) # + stat_compare_means() # KW value
  boxplot_list[[i]] = p      
}

pdf("ERAS_race_boxplots_2019-20_LOR.pdf")
for (i in 1:length(all.col.names)) {
  print(boxplot_list[[i]])
}
dev.off()

## Possible TO-DO's: remove outliers, output figures in JPG files



#### TO DELETE ####
var_list <- names(data[c(2)]) #leave out obj var (6:13 & 15:25) for now
num_vars <- length(c(2))
boxplot_list <- list()
for (i in 1:num_vars){
  #print(var_list[i])
  stat.2 <- tukey_hsd(data, eval(parse(text = var_list[i])) ~ race)
  stat.sig <- stat.2 %>% filter(p.adj.signif!="ns")
  #print( stat.2 )
  print(var_list[i])
  print(typeof(var_list[i]))
  start_pos <- max(data$var_list[i], na.rm = TRUE) + 5 # off-set by 5
  # num_comp <- nrow(stat.test.sig)
  # sep <- 6
  # y_pos <- seq(start_pos, start_pos+ (num_comp-1)*sep, by=sep)
  # 
  # p <- ggboxplot(data = data, x = "race", y = toString(var_list[i]), color = "race", palette = "npg") +
  #       stat_pvalue_manual(stat.test.sig, label = "{p.adj.signif}", y.position = y_pos) + # p = {p.adj}
  #       labs(title= paste(toString(var_list[i]),"by Self-Identified Race/Ethnicity \n 2019-20 & 2020-21", sep = " "),
  #            x ="Self-Identified Race/Ethnicity", y = toString(var_list[i]) )+
  #       theme(plot.title = element_text(hjust = 0.5, color = "dodgerblue3"), 
  #             plot.subtitle = element_text(hjust = 0.5))+
  #       theme(legend.position="none", text = element_text(size = 20), 
  #             plot.margin = margin(10,10,10,10)) +
  #       stat_compare_means() # KW value
  # boxplot_list[[i]] = p
}


pdf("ERAS_race_boxplots_combined_LOR.pdf")
for (i in 1:num_vars) {
  print(boxplot_list[[i]])
}
dev.off()


### bar graph: CAN DELETE ###

### bar graph ###
# http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
df_test <- data_summary(data = data, varname="step_1", groupnames=c("race"))

stat.test.sig.bar <- stat.test.sig %>% add_xy_position(x = "term", fun = "mean_sd",dodge = 0.8)

ggplot(df_test, aes(x=race, y=step_1, fill=race)) + 
  #stat_pvalue_manual(stat.test.sig, label = "p = {p.adj}{p.adj.signif}", y.position = y_pos)+
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=step_1-sd, ymax=step_1+sd), width=.2, position=position_dodge(.9)) +
  geom_text(aes(label= sprintf(step_1, fmt = '%#.2f')), vjust=1.6, color="white",
            position = position_dodge(0.9), size=6) +
  #coord_cartesian(ylim=c(210,265)) +
  # labs(title="Step 1 Mean Score by Race \n 2019-20 & 2020-21",
  #      x ="Race", y = "Step 1 Score")+
  # theme(plot.title = element_text(hjust = 0.5))+
  # theme(legend.position="none", text = element_text(size = 20), 
  #       plot.margin = margin(10,10,10,10))

  labs(title="Barplot w/ KW p-value, post hoc p-values and asterisks",
     subtitle = "Step 1 Mean Score by Race \n 2019-20 & 2020-21",
     x ="Race", y = "Step 1 Score")+
  theme(plot.title = element_text(hjust = 0.5, color = "dodgerblue3"), 
        plot.subtitle = element_text(hjust = 0.5))+
  theme(legend.position="none", text = element_text(size = 20), 
        plot.margin = margin(10,10,10,10)) +
  #stat_compare_means()  # KW value 
  stat_pvalue_manual(stat.test.sig.bar, label = "p.adj", tip.length = 0.01,
    bracket.nudge.y = -2) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

  
 