### COMMENTS #######################################################################################
# Target:   deletes features which multicorrelate
# Input:    3x dataset_noncorr, 3x dataset_mean_noncor

### LIBRARY ########################################################################################
print("Build library")
library(dplyr)    # data warangling
#library(stats)    # glm
library(fmsb)     # variable inflation factor
library(VIF)      # vif for backwards feature selection


### FUNCTIONS ######################################################################################

run_vif <- function(dataset){
  
  # drop dependant variable and id
  expl_vars <- dataset[, c(3:ncol(dataset))]
  
  # factorize dependend variable: success, sex_m, staat_d
  dataset <- dataset %>%
    mutate(
      success = as.factor(success),
      sex_m = as.factor(sex_m),
      staat_d = as.factor(staat_d)
    )
  
  # run backward stepwise VIF feature selection and safe extant features in list "base_var"
  base_var <- vif_func(in_frame = expl_vars, thresh = 5, trace = T)
  
  # select only extant explanatory features
  col_nums <- which(colnames(expl_vars) %in% base_var)
  expl_vars_sub <- select(expl_vars, col_nums)
  
  # combine with dependant variable and id
  dataset <- cbind(dataset[, c(1,2)], expl_vars_sub)
  
  return(dataset)
}

# from: https://beckmw.wordpress.com/2013/02/05/collinearity-and-stepwise-vif-selection/
vif_func <- function(in_frame,thresh=10,trace=T,...){
  
  require(fmsb)
  
  if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
  
}

model_info <- function(model){
  
  list <- data.frame(
    "Predictors" = length(coef(model)),
    "Deviance" = model$deviance, 
    "AIC" = model$aic, 
    "McFadden" = 1 - (model$deviance / model$null.deviance), 
    "adj McFadden" = 1 - ((model$deviance+length(model$coefficients)) / model$null.deviance))

  return(list)
}

### READ DATA ######################################################################################
print("Read input data")
# read linearized dataset of exams with enough observations

setwd("C:\\Users\\Lorenz\\OneDrive\\Documents\\Universität\\Projekte\\Predicting Student Dropout\\Implementation\\working version")

dataset_1 <- read.csv(
  file = "05_correlation/02_output/dataset_noncor_1.csv",
  sep = ";"
)

dataset_2 <- read.csv(
  file = "05_correlation/02_output/dataset_noncor_2.csv",
  sep = ";"
)

dataset_3 <- read.csv(
  file = "05_correlation/02_output/dataset_noncor_3.csv",
  sep = ";"
)


dataset_mean_1 <- read.csv(
  file = "05_correlation/02_output/dataset_mean_noncor_1.csv",
  sep = ";"
)

dataset_mean_2 <- read.csv(
  file = "05_correlation/02_output/dataset_mean_noncor_2.csv",
  sep = ";"
)

dataset_mean_3 <- read.csv(
  file = "05_correlation/02_output/dataset_mean_noncor_3.csv",
  sep = ";"
)



### VARIANCE INFLATION FACTOR ######################################################################

dataset_vif_1 <- run_vif(dataset_1)
dataset_vif_2 <- run_vif(dataset_2)
dataset_vif_3 <- run_vif(dataset_3)

dataset_mean_vif_1 <- run_vif(dataset_mean_1)
dataset_mean_vif_2 <- run_vif(dataset_mean_2)
dataset_mean_vif_3 <- run_vif(dataset_mean_3)



### OUTPUT #########################################################################################

path <- "06_multicollinearities/02_output/"

print("Output folder :")
print(path)

print("Export dataset excluding multicollinearities, after VIF feature selection")
write.table(
  dataset_vif_1,
  file = paste(path, "dataset_vif_1.csv", sep = ""),
  sep = ";",
  row.names = FALSE
)

write.table(
  dataset_vif_2,
  file = paste(path, "dataset_vif_2.csv", sep = ""),
  sep = ";",
  row.names = FALSE
)

write.table(
  dataset_vif_3,
  file = paste(path, "dataset_vif_3.csv", sep = ""),
  sep = ";",
  row.names = FALSE
)


write.table(
  dataset_mean_vif_1,
  file = paste(path, "dataset_mean_vif_1.csv", sep = ""),
  sep = ";",
  row.names = FALSE
)

write.table(
  dataset_mean_vif_2,
  file = paste(path, "dataset_mean_vif_2.csv", sep = ""),
  sep = ";",
  row.names = FALSE
)

write.table(
  dataset_mean_vif_3,
  file = paste(path, "dataset_mean_vif_3.csv", sep = ""),
  sep = ";",
  row.names = FALSE
)

print("*** Output complete ")



### CLEAN ENVIRONMENT ##############################################################################
rm(list = ls())
