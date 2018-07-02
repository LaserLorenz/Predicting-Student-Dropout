### COMMENTS #######################################################################################
# Claculates model only with DKM on dataset exam 1
# creates tables with statistics and two tables for Latex: statistics and confusion matrix

print("Script Start:")
print(Sys.time())


### LIBRARY ########################################################################################
library(caret)      # e.g. for confusion matrix
library(dplyr)      # data wrangling 
library(CORElearn)  # data mining system
library(rpart.plot) # plotting tree
library(RColorBrewer)
library(rattle)
library(DMwR)       # smote (KNN data balancing)
library(ggplot2)    # here just for saving plots

setwd("...\\Universität\\Projekte\\Predicting Student Dropout\\Implementation\\working version")

#Decide whether to perform analysis on smoted data
smoted = F

#Now loop through three different semester perspectives (join formerly three scripts)

for (semester in 1:3){


### READ DATA ######################################################################################

dataset <- read.csv(
  file = paste0("03_linearization/02_output/dataset_lin_",semester,"_dt.csv"),
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

be_smoted = function(dataset = dataset){
  
  dataset_smoted = SMOTE(success ~ ., dataset, perc.over = 100, perc.under=200)
  prop.table(table(dataset_smoted$success))

  return(dataset_smoted)
    
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
  
  colnames(pred)[1] <- "pred"
  pred <- pred %>%
    mutate(
      obs = testdata$success
    )
  
  return(pred)
}

### PREPARE DATASET ################################################################################
dataset <- preparation(dataset)

#Perform analysis on smoted data?
if(smoted){dataset_smoted <- be_smoted(dataset)}

rm(preparation,be_smoted)


set.seed(42)

### GEN CROSS VALIDATION ###########################################################################
est <- 34 #Hellinger Distance
#est <- c(7, 10, 14, 21, 25, 26, 29, 30, 33, 34) # run CV on all relevant estimators

print("Start:")
print(Sys.time())

method = toString(infoCore(what = "attrEval")[est])
print(method)

# Train tree on full dataset
    
    #switch data for smoting purposes
    if(smoted==T){
      dataset_saved = dataset
      dataset = dataset_smoted
      }

fit <- CoreModel(
  f = success ~ ., 
  data = dataset, 
  model= "tree", 
  selectionEstimator = infoCore(what = "attrEval")[est],
  minNodeWeightTree = round(nrow(dataset)*0.00625)
)

    #unchange dataset for smoting purposes
    if(smoted==T){
      dataset = dataset_saved
      rm(dataset_saved)
    }

pred_results = pred(
  fit, 
  dataset, 
  type = "class")



predraw_results = as.numeric(as.character(predict(
  fit, 
  dataset,
  type="class"
)))

#Probabilities in order to calculate ROC curves
predprob_results = predict(
  fit, 
  dataset,
  type="prob"
)


core_eval <- modelEval(
  model = fit,
  correctClass = dataset$success,  # dataset$success
  predictedClass = pred_results[,1]
)
print(c("Accuracy:",core_eval$accuracy)); print(c("Kappa:", core_eval$kappa))


#Print classification tree
png(filename = paste0("08_decision tree/02_output/tree_",
                      ifelse(smoted, "smoted_","non-smoted_"),
                      semester,".png"),
    width = 1200, height = 1200
)

rpart.plot(getRpartModel(fit, dataset), type=2, extra=6,  main = "DT model, data: 1CN")
fancyRpartPlot(getRpartModel(fit, dataset), main = "DT model, data: 1EN")

dev.off()

quality <- c(
  "semester" = semester,
  "method" = infoCore(what = "attrEval")[est],
  
  "AUC" = core_eval$AUC,
  "avg cost" = core_eval$averageCost,
  
  "ACC" = core_eval$accuracy,
  "SEN" = core_eval$sensitivity,
  "SPE" = core_eval$specificity,
  "PRE" = core_eval$precision,
  "KAP" = core_eval$kappa,
  
  "TP" = core_eval$predictionMatrix[1,1],
  "TN" = core_eval$predictionMatrix[2,2],
  "FP" = core_eval$predictionMatrix[1,2],
  "FN" = core_eval$predictionMatrix[2,1]
)

ifelse(!exists("cm_table"),
       cm_table <- quality,
       cm_table <- rbind(cm_table, quality)
       )

ifelse(!exists("total_predprob"),
       total_predprob <- list(as.data.frame(cbind(
         as.numeric(as.character(dataset$success))
         ,predprob_results[,2]))),
       total_predprob[[semester]] <- as.data.frame(cbind(
         as.numeric(as.character(dataset$success))
         ,predprob_results[,2]))
)

#Variable importance (purity and size)

scores = getRpartModel(fit, dataset)$frame
score_table = cbind(
  "semester" = rep(semester, nrow(scores)),
  scores[c(1,2)],
  "purity" = scores[[9]][,c(4,5)]
)
score_table = score_table[score_table[,2]!="<leaf>",]

ifelse(!exists("var_importance"),
       var_importance <- score_table,
       var_importance <- rbind(
         var_importance,
         score_table)
)


rm(test, train, dataset, core_eval, est, eval, fit, i, ind, method, ncases, pred, predprob_results, quality, scores, score_table)

}


### EXPORT: TABLE, LATEX  ##########################################################################
path <- "08_decision tree/02_output/"

if(smoted==T){type="smoted"}
if(smoted==F){type="non_smoted"}

write.table(
  cm_table,
  file = paste0(path,"final_dt_model_infos_123_",type,".csv"),
  sep = ";",
  dec = ",",
  row.names = FALSE
)

write.table(
  var_importance,
  file = paste0(path,"variable_scores_",type,".csv"),
  sep = ";",
  dec = ",",
  row.names = FALSE
)

for (semester in 1:3){
write.table(
  total_predprob[[semester]],
  file = paste(path, paste0("final_dt_model_predictionprobabilites_", semester,"_",type,".csv"), sep = ""),
  sep = ";",
  dec = ",",
  row.names = FALSE
)}

print("Script End:")
print(Sys.time())

rm(cm_table, path, semester, total_predprob, var_importance, dataset_smoted, pred_results, predraw_results, type, smoted)
