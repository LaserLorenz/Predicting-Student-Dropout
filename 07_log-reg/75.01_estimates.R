# Create Table with Model estimates ####

library(dplyr)
library(plyr)



# first all exam models ####
estimates_1EN <- data.frame("1EN" = model_exam_1_actual$finalModel$coefficients)
estimates_1EN <- estimates_1EN %>% mutate("var" = row.names(estimates_1EN))

estimates_1EB <- data.frame("1EB" = model_exam_1_smoted$finalModel$coefficients)
estimates_1EB <- estimates_1EB %>% mutate("var" = row.names(estimates_1EB))

estimates_2EN <- data.frame("2EN" = model_exam_2_actual$finalModel$coefficients)
estimates_2EN <- estimates_2EN %>% mutate("var" = row.names(estimates_2EN))

estimates_2EB <- data.frame("2EB" = model_exam_2_smoted$finalModel$coefficients)
estimates_2EB <- estimates_2EB %>% mutate("var" = row.names(estimates_2EB))

estimates_3EN <- data.frame("3EN" = model_exam_3_actual$finalModel$coefficients)
estimates_3EN <- estimates_3EN %>% mutate("var" = row.names(estimates_3EN))
estimates_3EN <-estimates_3EN[,c(2,1)]

estimates_3EB <- data.frame("3EB" = model_exam_3_smoted$finalModel$coefficients)
estimates_3EB <- estimates_3EB %>% mutate("var" = row.names(estimates_3EB))

# then the mean models
estimates_1CN <- data.frame("1CN" = model_mean_1_actual$finalModel$coefficients)
estimates_1CN <- estimates_1CN %>% mutate("var" = row.names(estimates_1CN))

estimates_1CB <- data.frame("1CB" = model_mean_1_smoted$finalModel$coefficients)
estimates_1CB <- estimates_1CB %>% mutate("var" = row.names(estimates_1CB))

estimates_2CN <- data.frame("2CN" = model_mean_2_actual$finalModel$coefficients)
estimates_2CN <- estimates_2CN %>% mutate("var" = row.names(estimates_2CN))

estimates_2CB <- data.frame("2CB" = model_mean_2_smoted$finalModel$coefficients)
estimates_2CB <- estimates_2CB %>% mutate("var" = row.names(estimates_2CB))

estimates_3CN <- data.frame("3CN" = model_mean_3_actual$finalModel$coefficients)
estimates_3CN <- estimates_3CN %>% mutate("var" = row.names(estimates_3CN))

estimates_3CB <- data.frame("3CB" = model_mean_3_smoted$finalModel$coefficients)
estimates_3CB <- estimates_3CB %>% mutate("var" = row.names(estimates_3CB))



# BUILD TABLES ####
estimate_table_exam <- join_all(
  list(
    estimates_3EN,
    estimates_3EB,
    estimates_2EN,
    estimates_2EB,
    estimates_1EN,
    estimates_1EB
  ),
  by='var',
  type='left'
)

estimate_table_mean <- join_all(
  list(
    estimates_3CN,
    estimates_3CB,
    estimates_2CN,
    estimates_2CB,
    estimates_1CN,
    estimates_1CB
  ),
  by='var',
  type='left'
)



#### CLEAN WS ####
rm(
  estimates_3EN,
  estimates_3EB,
  estimates_2EN,
  estimates_2EB,
  estimates_1EN,
  estimates_1EB,
  estimates_3CN,
  estimates_3CB,
  estimates_2CN,
  estimates_2CB,
  estimates_1CN,
  estimates_1CB
)



### write data ####
setwd("C:\\Users\\Lorenz\\OneDrive\\Documents\\Universität\\Veröffentlichungen\\Predicting Student Dropout\\Implementation\\working version")

path <- "07_log-reg/02_output/11_estimates/"

write.table(
  estimate_table_exam,
  file = paste(path, "LR_exam_estimates.csv", sep = ""),
  sep = ";",
  row.names = FALSE,
  dec = ","
)
 
write.table(
  estimate_table_mean,
  file = paste(path, "LR_mean_estimates.csv", sep = ""),
  sep = ";",
  row.names = FALSE,
  dec = ","
) 
