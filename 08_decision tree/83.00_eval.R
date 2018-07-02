### COMMENTS #######################################################################################
# model statistics
# confusion matrix
# variable importance (todo)
# plots (todo)



### LIBRARY ########################################################################################
library(caret)      # e.g. for confusion matrix
library(dplyr)      # data wrangling 
library(CORElearn)  # data mining system
library(xtable)     # LaTex



### MODEL STATISTICS ###############################################################################

model_infos_eval <- rbind(
  "3EN" = final_3EN_cmtable,
  "3CN" = final_3CN_cmtable,
  "2EN" = final_2EN_cmtable,
  "2CN" = final_2CN_cmtable,
  "1EN" = final_1EN_cmtable,
  "1CN" = final_1CN_cmtable
)

data_codes <- data.frame("Data" = row.names(model_infos_eval))
model_infos_eval <- cbind(data_codes, model_infos_eval)

print(
  xtable(
    x = model_infos_eval, 
    caption = "DT: Model Evaluation, estimator: HDDT distance",
    label = "table:dtModelEval",
    auto = TRUE,
    digits = 3
  )
)

### EXPORT #########################################################################################

setwd("C:\\Users\\Lorenz\\OneDrive\\Documents\\Universität\\Veröffentlichungen\\Predicting Student Dropout\\Implementation\\working version")

path <- "08_decision tree/02_output/"

write.table(
  model_infos_eval,
  file = paste(path, "final_dt_model comparison.csv", sep = ""),
  sep = ";",
  dec = ",",
  row.names = FALSE
)
