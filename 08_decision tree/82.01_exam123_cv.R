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

setwd("C:\\Users\\Lorenz\\OneDrive\\Documents\\Universität\\Projekte\\Predicting Student Dropout\\Implementation\\working version")

set.seed(42)

#Decide whether to perform analysis on smoted data
smoted = T

#Now loop through three different semester perspectives (join formerly three scripts)
for (semester in 1:3){


### READ DATA ######################################################################################

dataset <- read.csv(
  file = paste0("03_linearization/02_output/dataset_lin_",semester,".csv"),
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

### PREPARE DATASET ################################################################################
dataset <- preparation(dataset)

#Perform analysis on smoted data?
if(smoted){dataset_smoted <- be_smoted(dataset)}

rm(preparation,be_smoted)

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
fit <- NULL
accs <- rep(0,length(unique(ind)))

# run 10-fold cross validation
for (i in unique(ind)) {

  #switch training data for smoting purposes    
  if(smoted){train = dataset_smoted[ind!=i,]}
  if(!smoted){train = dataset[ind!=i,]}
  
  fit <- CoreModel(
    f = success ~ ., 
    data = train, 
    model= "tree", 
    selectionEstimator = infoCore(what = "attrEval")[est],
    minNodeWeightTree = round(nrow(dataset)*0.00625)
  )
  
  test = dataset[ind==i,]
  
  
  pred <- predict(
    object = fit,
    newdata = test,
    type="class"
  )
  
  conf <- table(test$succes,pred)
  
  # Assign the accuracy of this model to the ith index in accs
  accs[i] <- sum(diag(conf))/sum(conf)
}


ifelse(!exists("accs_table"),
       accs_table <- accs,
       accs_table <- rbind(accs_table, accs)
       )

rm(test, train, dataset, core_eval, est, eval, fit, i, ind, method, ncases, pred, pred_prob, quality)

}


### EXPORT: TABLE, LATEX  ##########################################################################
path <- "08_decision tree/02_output/"

if(smoted==T){type="smoted"}
if(smoted==F){type="non_smoted"}

write.table(
  accs_table,
  file = paste0(path,"cv_data_",type,"_.csv"),
  sep = ";",
  dec = ",",
  row.names = FALSE
)

print("Script End:")
print(Sys.time())

rm(accs_table, path, semester, dataset_smoted)
