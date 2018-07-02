### COMMENTS #######################################################################################
# Dataset: Mean, semester 1-2



### LIBRARY ########################################################################################
library(caret)  # regression
library(dplyr)  # data warangling
library(stats)  # stepwise feature selection on AIC
library(lmtest) # regression diagnostics
library(DMwR)   # smote (KNN data balancing)



### READ DATA ######################################################################################

setwd("C:\\Users\\Lorenz\\OneDrive\\Documents\\Universität\\Veröffentlichungen\\Predicting Student Dropout\\Implementation\\working version")

dataset_1 <- read.csv(
file = "06_multicollinearities/02_output/dataset_mean_vif_2.csv",
sep = ";"
)

print("Dataset: Mean, semester 2")



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

pred <- function(model, testdata){
  # returns dataframe with two rows: predictions and observations
  # predictions are made on running a model on testdata
  
  pred <- as.data.frame(
    predict(
      object = model,
      newdata = testdata,
      type = "raw"
    )
  )
  
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
  
  # define training control
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



### PREPARATION ####################################################################################

# Factorize dichotomous variables
dataset_1 <- preparation(dataset_1)
backup <- dataset_1

# remove id
dataset_1 <- dataset_1[, 2:ncol(dataset_1)]

set.seed(2)



### BALANCED DATASET ###############################################################################

dataset_1_smoted <- SMOTE(success ~ ., dataset_1, perc.over = 100, perc.under=200)
prop.table(table(dataset_1_smoted$success))



### CROSS VALIDATION MODELS ########################################################################

# build formula from dataset
f_cv <- as.formula(
  paste(
    'success ~',
    paste(colnames(dataset_1)[c(2:ncol(dataset_1))],
          collapse = '+')
  )
)


# build model via cross validation on actual data
model_1_cv <- run_crossvalidation(
  dataset = dataset_1,
  f = f_cv
)

# build model via cross validation on smoted data
model_1_cv_smoted <- run_crossvalidation(
  dataset = dataset_1_smoted,
  f = f_cv
)



### CONFUSION MATRIX ###############################################################################

pred_actual <- pred(model_1_cv, dataset_1)
pred_smoted <- pred(model_1_cv_smoted, dataset_1)

print("crossvalidation model on actual data")
cm_results_mean_2_actual <- confusionMatrix(pred_actual$pred, pred_actual$obs)
print(cm_results_mean_2_actual)

print("crossvalidation model on balanced data")
cm_results_mean_2_smoted <- confusionMatrix(pred_smoted$pred, pred_smoted$obs)
print(cm_results_mean_2_smoted)

print("model coefficients on balanced data")
summary(model_1_cv_smoted) #$coef

# change model names for identification
model_mean_2_actual <- model_1_cv
model_mean_2_smoted <- model_1_cv_smoted



### CLEAN WORKSPACE ################################################################################

rm(
  backup,
  dataset_1,
  dataset_1_smoted,
  pred_actual,
  pred_smoted,
  f_cv,
  model_1_cv,
  model_1_cv_smoted,
  build_bfs_model,
  build_dataset_model,
  preparation,
  pred,
  run_crossvalidation
)


