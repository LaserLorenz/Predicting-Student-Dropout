### COMMENT ########################################################################################
# deletes variables correlated > 0.8

# input: 
# dataset_lin with exams with enough observations
# dataset_mean

# reduces number of explanatory variables from 105 (597) to 61 (196) ; ds_32 (ds_all)

### LIBRARY ########################################################################################
library('caret')

### FUNCTIONS ######################################################################################

corr_removal <- function(dataset){
  
  # remove dependent variable and id
  expl_vars <- dataset[, c(3:ncol(dataset))]
  
  # delete correlated variables
  corr_expl_vars = cor(expl_vars)
  hc = findCorrelation(corr_expl_vars, cutoff = 0.9, verbose = F, names = F, exact = T)
  hc = sort(hc)
  non_corr = expl_vars[,-c(hc)]
  
  # combine with id and dependent variable
  non_corr <- cbind(dataset[, 1:2], non_corr)
  
  return(non_corr)
}


### READ DATA ######################################################################################
print("Read input data")
# read data set

setwd("...\\Documents\\Universität\\Projekte\\Predicting Student Dropout\\Implementation\\working version")

dataset_lin_1 <- read.csv(
  file = "03_linearization/02_output/dataset_lin_1.csv",
  sep = ";"
)

dataset_lin_2 <- read.csv(
  file = "03_linearization/02_output/dataset_lin_2.csv",
  sep = ";"
)

dataset_lin_3 <- read.csv(
  file = "03_linearization/02_output/dataset_lin_3.csv",
  sep = ";"
)


dataset_mean_1 <- read.csv(
  file = "02_preanalysis/02_output/dataset_mean_1.csv",
  sep = ";"
)

dataset_mean_2 <- read.csv(
  file = "02_preanalysis/02_output/dataset_mean_2.csv",
  sep = ";"
)

dataset_mean_3 <- read.csv(
  file = "02_preanalysis/02_output/dataset_mean_3.csv",
  sep = ";"
)



### MUTATION #######################################################################################

# remove col "count_sem" -> deviation = 0 --> error in corr
dataset_lin_1 <- subset(dataset_lin_1, select = -c(count_sem))
dataset_mean_1 <- subset(dataset_mean_1, select = -c(sem_max))


### REMOVE CORRELATION #############################################################################
dataset_lin_1_noncor <- corr_removal(dataset_lin_1)
dataset_lin_2_noncor <- corr_removal(dataset_lin_2)
dataset_lin_3_noncor <- corr_removal(dataset_lin_3)

dataset_mean_1_noncor <- corr_removal(dataset_mean_1)
dataset_mean_2_noncor <- corr_removal(dataset_mean_2)
dataset_mean_3_noncor <- corr_removal(dataset_mean_3)



### EXPORT #########################################################################################
# export
path <- "05_correlation/02_output/"

print("Export dataset to:")
print(path)


write.table(
  dataset_lin_1_noncor,
  file = paste(path, "dataset_noncor_1.csv", sep = ""),
  sep = ";",
  row.names = FALSE
)

write.table(
  dataset_lin_2_noncor,
  file = paste(path, "dataset_noncor_2.csv", sep = ""),
  sep = ";",
  row.names = FALSE
)

write.table(
  dataset_lin_3_noncor,
  file = paste(path, "dataset_noncor_3.csv", sep = ""),
  sep = ";",
  row.names = FALSE
)


write.table(
  dataset_mean_1_noncor,
  file = paste(path, "dataset_mean_noncor_1.csv", sep = ""),
  sep = ";",
  row.names = FALSE
)

write.table(
  dataset_mean_2_noncor,
  file = paste(path, "dataset_mean_noncor_2.csv", sep = ""),
  sep = ";",
  row.names = FALSE
)

write.table(
  dataset_mean_3_noncor,
  file = paste(path, "dataset_mean_noncor_3.csv", sep = ""),
  sep = ";",
  row.names = FALSE
)



### CLEAN WORKSPACE ################################################################################
rm(list=ls())

print("*** ENDED ***")
