#########################################
#Main file to centrally run all scripts from subdirectories

setwd("C:\\Users\\Lorenz\\OneDrive\\Documents\\Universität\\Veröffentlichungen\\Predicting Student Dropout\\Implementation\\working version")

###-----------------------------------------------------------------------
#Step 1: Data integration and cleaning

  cutoffdate = "2011-10-15"
  #until which date exams shall be observed
  #just leave parameter undefined in order to use all data
  #for QPL use data before WS 11/12

source("01_database preparation\\v09.2.R")

###-----------------------------------------------------------------------
#Step 2: Preanalysis, first feature extration and reduction
source("02_preanalysis\\v16.R")

###-----------------------------------------------------------------------
#Step 3 Data transformation and linearization
source("03_linearization\\v24.04.R")

###-----------------------------------------------------------------------
#Step 4 & 5: Dimension reduction and handling of multicollinearity
source("05_correlation\\53.R")
source("06_multicollinearities\\v61.R")

###-----------------------------------------------------------------------
#Step 6a: Logistic Regression
source("07_log-reg\\73.1_exam_123.R")

source("07_log-reg\\73.1_mean_1.R")
source("07_log-reg\\73.1_mean_2.R")
source("07_log-reg\\73.1_mean_3.R")

source("07_log-reg\\73.2_model performance summaries.R")
source("07_log-reg\\74.00_variable importance.R")
source("07_log-reg\\75.01_estimates.R")

###-----------------------------------------------------------------------
#Step 6b: Decision Tree
source("08_decision tree\\82.01_exam123.R")

source("08_decision tree\\82.01_mean1_final model_FINAL.R")
source("08_decision tree\\82.02_mean2_final model_FINAL.R")
source("08_decision tree\\82.03_mean3_final model_FINAL.R")

source("08_decision tree\\83.00_eval.R")
  
