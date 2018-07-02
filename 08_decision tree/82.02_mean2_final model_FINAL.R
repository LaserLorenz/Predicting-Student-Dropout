### COMMENTS #######################################################################################
# Claculates model only with DKM on dataset mean 2
# creates tables with statistics and two tables for Latex: statistics and confusion matrix

print("Script Start:")
print(Sys.time())


### LIBRARY ########################################################################################
library(caret)      # e.g. for confusion matrix
library(dplyr)      # data wrangling 
library(CORElearn)  # data mining system

setwd("C:\\Users\\Lorenz\\OneDrive\\Documents\\Universität\\Veröffentlichungen\\Predicting Student Dropout\\Implementation\\working version")

### READ DATA ######################################################################################
# Semester 2, mean data
dataset <- read.csv(
  file = "02_preanalysis/02_output/dataset_mean_2.csv",
  sep = ";"
)



### FUNCTIONS ######################################################################################
# removes id as predictor and formats categorial variables
preparation <- function(dataset = dataset){
  
  dataset <- dataset[2:ncol(dataset)]
  
  dataset <- dataset %>%
    mutate(
      success = as.factor(success),
      staat_d = as.factor(staat_d),
      sex_m = as.factor(sex_m)
    )
  
  return(dataset)
}



### PREPARE DATASET ################################################################################
dataset <- preparation(dataset)

rm(preparation)


set.seed(2)

### GEN CROSS VALIDATION ###########################################################################
est <- 34 #Hellinger Distance
#est <- c(7, 10, 14, 21, 25, 26, 29, 30, 33, 34) # run CV on all relevant estimators

print("Start:")
print(Sys.time())

method = toString(infoCore(what = "attrEval")[est])
print(method)

# normal CV
ncases <- nrow(dataset)
ind <- ceiling(10*(1:ncases)/ncases)
ind <- sample(ind,length(ind))
pred <- rep(NA,ncases)
fit <- NULL

# run 10-fold cross validation in order to assess the risk of overfitting
for (i in unique(ind)) {
  
  train = dataset[ind!=i,]
  
  fit <- CoreModel(
    f = success ~ ., 
    data = train, 
    model= "tree", 
    selectionEstimator = infoCore(what = "attrEval")[est],
    minNodeWeightTree = round(nrow(dataset)*0.00625)
  )
  
  test = dataset[ind==i,]
  
  pred[ind==i] = as.numeric(as.character(predict(
    fit, 
    test,
    type="class" # options:
  )))
  
  eval <- modelEval(model = fit, predictedClass = pred[ind==i], correctClass = test$success)
  print(paste("fold",i))
  print(c("Accuracy:",eval$accuracy)); print(c("Kappa:", eval$kappa))
  
  #cm <- confusionMatrix(data = pred[ind==i], test$success)
  #display(fit)
  #rpart.plot(getRpartModel(fit, dataset), main = "DT model, data: 1CN")
  #rpart.plot(getRpartModel(fit, train))
  
  if (!is.null(fit)) destroyModels(fit) # dispose model no longer needed
}

# Finally train tree on full dataset

fit <- CoreModel(
  f = success ~ ., 
  data = dataset, 
  model= "tree", 
  selectionEstimator = infoCore(what = "attrEval")[est],
  minNodeWeightTree = round(nrow(dataset)*0.00625)
)

pred = as.numeric(as.character(predict(
  fit, 
  dataset,
  type="class" # options:
)))

core_eval <- modelEval(
  model = fit,
  correctClass = dataset$success,  # dataset$success
  predictedClass = pred
)
print(c("Accuracy:",core_eval$accuracy)); print(c("Kappa:", core_eval$kappa))

list <- data.frame(
  "Method" = infoCore(what = "attrEval")[est],
  
  "AUC" = core_eval$AUC,
  "avg cost" = core_eval$averageCost,
  
  "ACC" = core_eval$accuracy,
  "SEN" = core_eval$sensitivity,
  "SPE" = core_eval$specificity,
  "PRE" = core_eval$precision,
  "F1" = NA,
  "F1,414" = NA,
  "F2" = NA,
  "KAP" = core_eval$kappa,
  
  "TP" = core_eval$predictionMatrix[1,1],
  "TN" = core_eval$predictionMatrix[2,2],
  "FP" = core_eval$predictionMatrix[1,2],
  "FN" = core_eval$predictionMatrix[2,1]
)

cm_table <- data.frame(list)



print("End:")
print(Sys.time())


### CLEAN WORKSPACE ################################################################################
#rm(list, cm, core_eval, est, fit, flag, i, ind, method, ncases)





### EXPORT: TABLE, LATEX  ##########################################################################
path <- "08_decision tree/02_output/"

write.table(
  cm_table,
  file = paste(path, "final_dt_model_infos_2CN.csv", sep = ""),
  sep = ";",
  dec = ",",
  row.names = FALSE
)

final_2CN_model <- fit
final_2CN_predictions <- pred
final_2CN_cmtable <-cm_table

print("Script End:")
print(Sys.time())

rm(cm_table, dataset, list, pred, core_eval, est, fit, i, ind, method, ncases, path)