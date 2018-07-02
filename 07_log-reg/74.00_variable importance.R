### COMMENTS #######################################################################################
# Log Reg Variable Importance
# "For classification, ROC curve analysis is conducted on each predictor. 
# For two class problems, a series of b. 
# The sensitivity and specificity are computed for each cutoff and the ROC curve is computed. 
# The trapezoidal rule is used to compute the area under the ROC curve. 
# This area is used as the measure of variable importance."

# flags
scaled <- FALSE # true = scaled to 100, false = original score
exam <- TRUE # true = exam models, false = mean models


### LIBRARY ########################################################################################
library(caret)  # regression
library(plyr)   # joining multiple datasets
library(dplyr)  # data warangling
library(tidyr)  # data warangling
#library(lmtest) # regression diagnostics



### VARIABLE IMPORTANCE ############################################################################

# mean models
if(!exam){
vi_3EN <- varImp(model_mean_3_actual, scale = scaled)$importance
vi_3EB <- varImp(model_mean_3_smoted, scale = scaled)$importance
vi_2EN <- varImp(model_mean_2_actual, scale = scaled)$importance 
vi_2EB <- varImp(model_mean_2_smoted, scale = scaled)$importance 
vi_1EN <- varImp(model_mean_1_actual, scale = scaled)$importance
vi_1EB <- varImp(model_mean_1_smoted, scale = scaled)$importance
}

# OR exam models
if(exam){
  vi_3EN <- varImp(model_exam_3_actual, scale = scaled)$importance
  vi_3EB <- varImp(model_exam_3_smoted, scale = scaled)$importance
  vi_2EN <- varImp(model_exam_2_actual, scale = scaled)$importance 
  vi_2EB <- varImp(model_exam_2_smoted, scale = scaled)$importance 
  vi_1EN <- varImp(model_exam_1_actual, scale = scaled)$importance
  vi_1EB <- varImp(model_exam_1_smoted, scale = scaled)$importance
}

vi_3EN <- vi_3EN %>% mutate("var" = row.names(vi_3EN))
vi_3EB <- vi_3EB %>% mutate("var" = row.names(vi_3EB))
vi_2EN <- vi_2EN %>% mutate("var" = row.names(vi_2EN))
vi_2EB <- vi_2EB %>% mutate("var" = row.names(vi_2EB))
vi_1EN <- vi_1EN %>% mutate("var" = row.names(vi_1EN))
vi_1EB <- vi_1EB %>% mutate("var" = row.names(vi_1EB))

colnames(vi_3EN)[1] <- "3EN"
colnames(vi_3EB)[1] <- "3EB"
colnames(vi_2EN)[1] <- "2EN"
colnames(vi_2EB)[1] <- "2EB"
colnames(vi_1EN)[1] <- "1EN"
colnames(vi_1EB)[1] <- "1EB"

vi_table <- join_all(list(vi_3EN, vi_3EB, vi_2EN, vi_2EB, vi_1EN, vi_1EB), by='var', type='left')
vi_table <- vi_table[c(2, 1, 3:ncol(vi_table))]

if(scaled){
  vi_table_s <- vi_table
} else {
  vi_table_n <- vi_table
}



### EXPORT #########################################################################################
setwd("C:\\Users\\Lorenz\\OneDrive\\Documents\\Universität\\Veröffentlichungen\\Predicting Student Dropout\\Implementation\\working version")

path <- "07_log-reg/02_output/10_interpretation/"

if(scaled){
  write.table(
    vi_table_s,
    file = paste(path, "vi_table_scale.csv", sep = ""),
    sep = ";",
    row.names = FALSE,
    dec = ","
  )
} else {
  write.table(
    vi_table_n,
    file = paste(path, "vi_table_noscale.csv", sep = ""),
    sep = ";",
    row.names = FALSE,
    dec = ","
  )
}


