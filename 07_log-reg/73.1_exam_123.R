


### LIBRARY ########################################################################################
library(caret)  # regression
library(dplyr)  # data warangling
library(stats)  # stepwise feature selection on AIC
library(lmtest) # regression diagnostics
library(DMwR)   # smote (KNN data balancing)

rm(list=ls())

### FUNCTIONS ######################################################################################

build_bfs_model <- function(model_in, dataset){
  
  # selection model                                                                                 bfs
  model_bfs <- step(object = model_in, direction = "backward", trace = 0)
  
  return(model_bfs)
}

build_dataset_model <- function(dataset){
  # builds a glm model from a datasetwith all variables
  
  f <- as.formula(
    paste(
      'success ~',
      paste(colnames(dataset)[c(2:ncol(dataset))],
            collapse = '+')
    )
  )
  
  model <- glm(
    f,
    data = dataset,
    family = "binomial"
  )
  
  return(model)
}

pred <- function(model, testdata, typeProb){
  # returns dataframe with two rows: predictions and observations
  # predictions are made on running a model on testdata
  
  pred <- as.data.frame(
    predict(
      object = model,
      newdata = testdata,
      type = typeProb
    )
  )
  if(typeProb=="prob"){pred <- as.data.frame(pred[,-1])}
  
  colnames(pred)[1] <- "pred"
  pred <- pred %>%
    mutate(
      obs = testdata$success
    )
  
  return(pred)
}

preparation <- function(dataset = dataset){
  
  dataset <- dataset %>%
    mutate(
      success = as.factor(success),
      staat_d = as.factor(staat_d),
      sex_m = as.factor(sex_m)
    )
  
  return(dataset)
}

run_crossvalidation <- function(dataset, f = f, folds = 10){
  
  # http://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/
  # test and train on whole data
  
  # define training control cv: cross validation
  train_control <- trainControl(method = "cv", number = folds, savePredictions = "all")
  
  # train the model
  model <- train(
    f = f,
    method = "glm", 
    family = "binomial",
    data = dataset, 
    trControl = train_control
  )
  
  # confusion matrix and results
  #print(confusionMatrix(data = model$pred$pred, model$pred$obs))
  
  return(model)
}

print_info = function(cm,  semester, type="non-smoted"){
    
    cm = cm[[semester]]  
  
    n <- sum(cm$table[1,1], cm$table[1,2], cm$table[2,1], cm$table[2,2])
    
    measures <- data.frame(
      
      "Accuracy" = cm$overall["Accuracy"],
      "P-Value" = cm$overall["AccuracyPValue"],
      "Sensitivity" = cm$byClass["Sensitivity"],
      "Specificity" = cm$byClass["Specificity"],
      "Precision" = cm$table[1,1]/(cm$table[1,1] + cm$table[1,2]),
      "Kappa" = cm$overall["Kappa"],
      "TP" = cm$table[1,1],
      "TN" = cm$table[2,2],
      "FP" = cm$table[1,2],
      "FN" = cm$table[2,1]
    )
    
    row.names(measures) = paste("Semester",semester, type)
    
    return(measures)
}
  
### READ DATA ######################################################################################

setwd("...\\Documents\\Universität\\Projekte\\Predicting Student Dropout\\Implementation\\working version")


#Now loop through three different semester perspectives (join formerly three scripts)
for (semester in 1:3){ 

dataset <- read.csv(
  file = paste0("06_multicollinearities/02_output/dataset_vif_", semester, ".csv"),
  sep = ";"
)

print(paste("Dataset: Exams, semester", semester))



### PREPARATION ####################################################################################

# Factorize dichotomous variables
dataset <- preparation(dataset)
backup <- dataset

# remove id
dataset <- dataset[, 2:ncol(dataset)]

set.seed(42)



### BALANCED DATASET ###############################################################################

dataset_smoted <- SMOTE(success ~ ., dataset, perc.over = 100, perc.under=200)
prop.table(table(dataset_smoted$success))



### CROSS VALIDATION MODELS ########################################################################

# build formula from dataset
f_cv <- as.formula(
  paste(
    'success ~',
    paste(colnames(dataset)[c(2:ncol(dataset))],
          collapse = '+')
  )
)


# build model via cross validation on actual data
model_cv <- run_crossvalidation(
  dataset = dataset,
  f = f_cv
)

# build model via cross validation on smoted data
model_cv_smoted <- run_crossvalidation(
  dataset = dataset_smoted,
  f = f_cv
)



### RESULTS ###############################################################################

pred_actual <- pred(model_cv, dataset, "raw")
pred_smoted <- pred(model_cv_smoted, dataset, "raw")

pred_prob_actual <- pred(model_cv, dataset, "prob")
pred_prob_actual_smoted <- pred(model_cv_smoted, dataset, "prob")

#Capture evaluation measures
ifelse(!exists("cm_infos"),
       cm_infos <- list(confusionMatrix(pred_actual$pred, pred_actual$obs)),
       cm_infos[[semester]] <- confusionMatrix(pred_actual$pred, pred_actual$obs)
)

ifelse(!exists("cm_infos_smoted"),
       cm_infos_smoted <- list(confusionMatrix(pred_smoted$pred, pred_smoted$obs)),
       cm_infos_smoted[[semester]] <- confusionMatrix(pred_smoted$pred, pred_smoted$obs)
)

#Capture regression output
ifelse(!exists("regression_infos"),
       regression_infos <- list(summary(model_cv)$coefficients[,c(1,4)]),
       regression_infos[[semester]] <- summary(model_cv)$coefficients[,c(1,4)]
)

ifelse(!exists("regression_infos_smoted"),
       regression_infos_smoted <- list(summary(model_cv_smoted)$coefficients[,c(1,4)]),
       regression_infos_smoted[[semester]] <- summary(model_cv_smoted)$coefficients[,c(1,4)]
)

# change model names for identification
assign(paste0("model_exam_",semester,"_actual"), model_cv)
assign(paste0("model_exam_",semester,"_smoted"), model_cv_smoted)

# merge probabilities
ifelse(!exists("total_predprob_glm"),
       total_predprob_glm <- list(pred_prob_actual[,c(2,1)]),
       total_predprob_glm[[semester]] <- pred_prob_actual[,c(2,1)]
)

ifelse(!exists("total_predprob_glm_smoted"),
       total_predprob_glm_smoted <- list(pred_prob_actual_smoted[,c(2,1)]),
       total_predprob_glm_smoted[[semester]] <- pred_prob_actual_smoted[,c(2,1)]
)


}#Here endes the semester loop



### EXPORT ################################################################################
path <- "07_log-reg/02_output/"

for (semester in 1:3){
  write.table(
    total_predprob_glm[[semester]],
    file = paste(path, paste0("final_log_model_predictionprobabilites_", semester, ".csv"), sep = ""),
    sep = ";",
    dec = ",",
    row.names = FALSE
  )
  write.table(
    total_predprob_glm_smoted[[semester]],
    file = paste(path, paste0("final_log_model_predictionprobabilites_", semester, "_smoted.csv"), sep = ""),
    sep = ";",
    dec = ",",
    row.names = FALSE
  )
}

  write.table(
    rbind(print_info(cm_infos,1,type="non-smoted"),
          print_info(cm_infos,2,type="non-smoted"),
          print_info(cm_infos,3,type="non-smoted"),
          print_info(cm_infos_smoted,1,type="smoted"),
          print_info(cm_infos_smoted,2,type="smoted"),
          print_info(cm_infos_smoted,3,type="smoted")
          ),
    file = paste(path, "lr_cm_infos.csv", sep = ""),
    sep = ";",
    dec = ",",
    col.names=NA,
    row.names = T
  )
  
  write.table(
    merge(
      merge(
        regression_infos[[1]],regression_infos[[2]],by="row.names",all=T),
      regression_infos[[3]],by.x="Row.names",by.y="row.names",all=T
      ),
    file = paste(path, "lr_regression_infos.csv", sep = ""),
    sep = ";",
    dec = ",",
    row.names = F
  )
  
### CLEAN WORKSPACE ################################################################################

rm(
  backup,
  dataset,
  dataset_smoted,
  pred_actual,
  pred_smoted,
  f_cv,
  model_cv,
  model_cv_smoted,
  build_bfs_model,
  build_dataset_model,
  preparation,
  pred,
  run_crossvalidation,
  total_predprob_glm
)


