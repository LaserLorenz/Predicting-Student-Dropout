### COMMENTS #######################################################################################
# needs models and cm-results from log-reg build in environment
# creates overview of model parameters and performance



### LIBRARY ########################################################################################
library(xtable)  # LaTex



### FUNCTIONS ######################################################################################
cm_info <- function(cm, name = "model name", index = 1){
  
  n <- sum(cm$table[1,1], cm$table[1,2], cm$table[2,1], cm$table[2,2])
  
  list <- data.frame(
    "Code" = name,
    "Accuracy" = cm$overall["Accuracy"],
    "P-Value" = cm$overall["AccuracyPValue"],
    "Fallout" = cm$table[1,2] / (cm$table[1,2] + cm$table[2,2]),
    "Sensitivity" = cm$byClass["Sensitivity"],
    "Specificity" = cm$byClass["Specificity"],
    "Precision" = cm$byClass["Precision"],
    "F1" = cm$byClass["F1"],
    "F1,414" = 3*cm$byClass["Precision"]*cm$byClass["Sensitivity"] / 
      (2*cm$byClass["Precision"] + cm$byClass["Sensitivity"]),
    "F2" = 5*cm$byClass["Precision"]*cm$byClass["Sensitivity"] / 
      (4*cm$byClass["Precision"] + cm$byClass["Sensitivity"]),
    "Kappa" = cm$overall["Kappa"]
    #"McNemar PV" = cm$overall[7],
    #"Pos Accuracy" = cm$table[1,1] / (cm$table[1,1] + cm$table[2,1]),
    #"NIR" = cm$overall[5],
    #"Specfiicity" = cm$byClass[2],
    #"PPV" = cm$byClass[3],
    #"NPV" = cm$byClass[4],
  )
  
  return(list)
}

model_info <- function(model, name = "model name", index = 1){
  
  list <- data.frame(
    "Code" = name,
    "Predictors" = length(coef(model)),
    "Deviance" = model$deviance,
    "Null_Dev." = model$null.deviance,
    "Dev Delta" = model$null.deviance - model$deviance,
    "AIC" = model$aic, 
    "McFadden R2" = 1 - (model$deviance / model$null.deviance)
    #"adj McFadden" = 1 - ((model$deviance+length(model$coefficients)) / model$null.deviance)
  )
  
 return(list) 
}

confusion_info <- function(cm, name = "model code", index = 1){
  
  n = n <- sum(cm$table[1,1], cm$table[1,2], cm$table[2,1], cm$table[2,2])
  
  list <- data.frame(
    "Code" = name,
    "TP" = cm$table[1,1],
    "TN" = cm$table[2,2],
    "FP" = cm$table[1,2],
    "FN" = cm$table[2,1],
    "rTP" = cm$table[1,1] / n,
    "rTN" = cm$table[2,2] / n,
    "rFP" = cm$table[1,2] / n,
    "rFN" = cm$table[2,1] / n
  )
  
  return(list)
}



### BUILD INFO TABLES ##############################################################################

model_infos <- as.data.frame(
  rbind(
    model_info(model_exam_3_actual$finalModel, "3EN", 1),
    model_info(model_exam_3_smoted$finalModel, "3EB", 2),
    model_info(model_mean_3_actual$finalModel, "3CN", 3),
    model_info(model_mean_3_smoted$finalModel, "3CB", 4),
    model_info(model_exam_2_actual$finalModel, "2EN", 5),
    model_info(model_exam_2_smoted$finalModel, "2EB", 6),
    model_info(model_mean_2_actual$finalModel, "2CN", 7),
    model_info(model_mean_2_smoted$finalModel, "2CB",8),
    model_info(model_exam_1_actual$finalModel, "1EN", 9),
    model_info(model_exam_1_smoted$finalModel, "1EB", 10),
    model_info(model_mean_1_actual$finalModel, "1CN", 11),
    model_info(model_mean_1_smoted$finalModel, "1CB", 12)
  )
)

cm_infos <- as.data.frame(
  rbind(
    cm_info(cm_results_exam_3_actual, "3EN", 1),
    cm_info(cm_results_exam_3_smoted, "3EB", 2),
    cm_info(cm_results_mean_3_actual, "3CN", 3),
    cm_info(cm_results_mean_3_smoted, "3CB", 4),
    cm_info(cm_results_exam_2_actual, "2EN", 5),
    cm_info(cm_results_exam_2_smoted, "2EB", 6),
    cm_info(cm_results_mean_2_actual, "2CN", 7),
    cm_info(cm_results_mean_2_smoted, "2CB", 8),
    cm_info(cm_results_exam_1_actual, "1EN", 9),
    cm_info(cm_results_exam_1_smoted, "1EB", 10),
    cm_info(cm_results_mean_1_actual, "1CN", 11),
    cm_info(cm_results_mean_1_smoted, "1CB", 12)
  )
)

conf_infos <- as.data.frame(
  rbind(
    confusion_info(cm_results_exam_3_actual, "3EN", 1),
    confusion_info(cm_results_exam_3_smoted, "3EB", 2),
    confusion_info(cm_results_mean_3_actual, "3CN", 3),
    confusion_info(cm_results_mean_3_smoted, "3CB", 4),
    confusion_info(cm_results_exam_2_actual, "2EN", 5),
    confusion_info(cm_results_exam_2_smoted, "2EB", 6),
    confusion_info(cm_results_mean_2_actual, "2CN", 7),
    confusion_info(cm_results_mean_2_smoted, "2CB", 8),
    confusion_info(cm_results_exam_1_actual, "1EN", 9),
    confusion_info(cm_results_exam_1_smoted, "1EB", 10),
    confusion_info(cm_results_mean_1_actual, "1CN", 11),
    confusion_info(cm_results_mean_1_smoted, "1CB", 12)
  )
)



### EXPORT #########################################################################################
setwd("C:\\Users\\Lorenz\\OneDrive\\Documents\\Universität\\Veröffentlichungen\\Predicting Student Dropout\\Implementation\\working version")

path <- "07_log-reg/02_output/"

write.table(
  model_infos,
  file = paste(path, "lr_model_infos.csv", sep = ""),
  sep = ";",
  dec = ",",
  row.names = FALSE
)

write.table(
  cm_infos,
  file = paste(path, "lr_cm_infos.csv", sep = ""),
  sep = ";",
  dec = ",",
  row.names = FALSE
)

write.table(
  conf_infos,
  file = paste(path, "lr_conf_infos.csv", sep = ""),
  sep = ";",
  dec = ",",
  row.names = FALSE
)

x_model_infos <- xtable(x = model_infos, caption = "Train Model Parameters", label = "table:modelInfos", auto = TRUE, digits = 3)
x_cm_infos <- xtable(x = cm_infos, caption = "Test Model Parameters", label = "table:cmInfos", auto = TRUE, digits = 3)
x_conf_infos <- xtable(x = conf_infos, caption = "Prediction Confusion Matrix", label = "table:confInfos", auto = TRUE, digits = 3)

print(x_model_infos, include.rownames=FALSE)
print(x_cm_infos, include.rownames=FALSE)
print(x_conf_infos, include.rownames=FALSE)



### CLEAN ENVIRONMENT ##############################################################################

rm(path, model_info)





