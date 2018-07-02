### COMMENTS #######################################################################################
# Creates dataset_lin from preanalysis output (incl. exams.R):
# All exam data is reordered into one row per student
# Only data from sem [1-3] is included
# No active students are included
# Only last try of each exam is included

### LIBRARY ########################################################################################
library(dplyr)        # data wrangling
library(tidyr)        # data wrangling

### READING ########################################################################################
# read input-data
print("Read input data from preanalysis")

setwd("C:\\Users\\Lorenz\\OneDrive\\Documents\\Universität\\Projekte\\Predicting Student Dropout\\Implementation\\working version")

backup <- read.csv(
  file = "02_preanalysis/02_output/dataset.csv",
  sep = ";"
)

exam_dataset_1 <- read.csv(
  file = "02_preanalysis/02_output/exams_1.csv",
  sep = ";"
)

exam_dataset_2 <- read.csv(
  file = "02_preanalysis/02_output/exams_2.csv",
  sep = ";"
)

exam_dataset_3 <- read.csv(
  file = "02_preanalysis/02_output/exams_3.csv",
  sep = ";"
)


### FUNCTIONS ######################################################################################

# builds dataset with student data and averages
build_core <- function(dataset = dataset){
  core <- dataset %>%
    group_by(
      id, 
      success, 
      sex_m, 
      staat_d, 
      age
    ) %>%
    summarise(
      avg_note = mean(pnote),
      count_sem = max(sem),
      count_be = sum(pstatus == 1),
      count_nb = sum(pstatus == 2)
    ) %>%
    filter(
      row_number() == 1
    ) %>%
    select(
      id, 
      success, 
      sex_m, 
      staat_d, 
      age,
      avg_note,
      count_sem,
      count_be,
      count_nb
    ) %>%
    ungroup() %>%
    mutate(
      avg_be = ifelse(count_be != 0, count_be/count_sem, 0),
      avg_nb = ifelse(count_nb != 0, count_nb/count_sem, 0)     
    )
  
  # sort core dataset
  dataset_core <- core[, c(1:6, 10:11, 7:9)]
  
  return(dataset_core)
}

# linearizes dataset for multiple columns
linearize <- function(dataset = dataset, core = dataset_core, fill_value = NA){
  
  print("Create linearized subsets for each feature")
  pnote <- dataset %>%
    select(
      id, pnr, pnote
    ) %>%
    spread(
      key = pnr,
      value = pnote,
      fill = fill_value
    )
  
  # add "_pnote" after the examnumber in colnames
  colnames(pnote) <- paste(colnames(pnote), 'pnote', sep = "_")
  
  # spread sem
  sem <- dataset %>%
    select(
      id, pnr, sem
    ) %>%
    spread(
      key = pnr,
      value = sem,
      fill = fill_value
    )
  
  # add "_sem" after the examnumber in colnames
  colnames(sem) <- paste(colnames(sem), 'sem', sep = "_")
  
  # spread pversuch
  pversuch <- dataset %>%
    select(
      id, pnr, pversuch
    ) %>%
    spread(
      key = pnr,
      value = pversuch,
      fill = fill_value
    )
  
  # add "_pversuch" after the examnumber in colnames
  colnames(pversuch) <- paste(colnames(pversuch), 'pversuch', sep = "_")
  
  print("Merge core data with linearized subsets")
  # merge student with exam data
  merge <- core %>%
    left_join(
      pnote,
      by = c('id' = 'id_pnote')
    ) %>% left_join(
      sem,
      by = c('id' = 'id_sem')
    ) %>% left_join(
      pversuch,
      by = c('id' = 'id_pversuch')
    )
  
  
  print("Arrange columns in correct order")
  # arrange columns by examnumber
  # save core column names
  colnames_core <- names(merge)[1:11]
  
  # save exam column names
  colnames_exams <- names(merge)[12:length(names(merge))]
  
  # sort exam column names
  colnames_exams <- sort(colnames_exams)
  
  # concat column names vectors
  colnames <- c(colnames_core, colnames_exams)
  
  # select columns following new order
  output <- merge[,colnames]
  
  print("Return linearized dataset")
  return(output)
}

# removes data from high semester
prepare_ds <- function(dataset, sem_max = max(dataset$sem)){
  
  ds <- dataset %>%
    filter(
      sem <= sem_max,
      success != 2
    ) %>%
    mutate(
      pstatus = ifelse(pstatus == 3, 2, pstatus)
    )
  
  return(ds)
}

# keeps only last try of an exam, prevents error while pivoting
remove_fails <- function(dataset){
  
  ds <- dataset %>%
    group_by(
      id, pnr
    ) %>%
    arrange(
      sem
    ) %>% 
    filter(
      row_number() == n()
    )
  
  return(ds)
}



### MUTATION #######################################################################################

# prepare datasets: adapt pstatus, remove data from high semesters
dataset_1 <- prepare_ds(backup, 1)
dataset_2 <- prepare_ds(backup, 2)
dataset_3 <- prepare_ds(backup, 3)

# build core datasets before exams are excluded
dataset_core_1 <- build_core(dataset_1)
dataset_core_2 <- build_core(dataset_2)
dataset_core_3 <- build_core(dataset_3)

# keep full datasets for decision tree, added 12.06.2017
dataset_1_dt <- dataset_1
dataset_2_dt <- dataset_2
dataset_3_dt <- dataset_3



### LOG REG PREP ###################################################################################

print("Remove exams with too little observaitons")
# keep exams from exam selection
dataset_1 <- semi_join(dataset_1, exam_dataset_1, by = "pnr") %>% arrange(id)
dataset_2 <- semi_join(dataset_2, exam_dataset_2, by = "pnr") %>% arrange(id)
dataset_3 <- semi_join(dataset_3, exam_dataset_3, by = "pnr") %>% arrange(id)

rm(exam_dataset_1, exam_dataset_2, exam_dataset_3)

# keep only latest try (for spread error prevention)
print("Remove duplicate exams: LogReg")
dataset_1 <- remove_fails(dataset_1)
dataset_2 <- remove_fails(dataset_2)
dataset_3 <- remove_fails(dataset_3)



### DECISION TREE PREP #############################################################################
# added on 12.06.2017

# keep only latest try (for spread error prevention)
print("Remove duplicate exams: DT")
dataset_1_dt <- remove_fails(dataset_1_dt)
dataset_2_dt <- remove_fails(dataset_2_dt)
dataset_3_dt <- remove_fails(dataset_3_dt)



### LINEARIZE ######################################################################################

# LOGISTIC REGRESSION
# produce linearized dataset: 0 as fill value
print("Linearize features")
dataset_lin_1 <- linearize(dataset_1, dataset_core_1, 0)
dataset_lin_2 <- linearize(dataset_2, dataset_core_2, 0)
dataset_lin_3 <- linearize(dataset_3, dataset_core_3, 0)

# delete rows containing NA
dataset_lin_1 <- dataset_lin_1[complete.cases(dataset_lin_1), ]
dataset_lin_2 <- dataset_lin_2[complete.cases(dataset_lin_2), ]
dataset_lin_3 <- dataset_lin_3[complete.cases(dataset_lin_3), ]

rm(dataset_1, dataset_2, dataset_3)


# DECISION TREES
# produce linearized dataset: NA as fill value
print("Linearize features")
dataset_lin_1_dt <- linearize(dataset_1_dt, dataset_core_1, NA)
dataset_lin_2_dt <- linearize(dataset_2_dt, dataset_core_2, NA)
dataset_lin_3_dt <- linearize(dataset_3_dt, dataset_core_3, NA)

rm(dataset_1_dt, dataset_2_dt, dataset_3_dt)



### EXPORT #########################################################################################

path <- "03_linearization/02_output/"

print("Export linearized dataset to:")
print(path)

write.table(
  dataset_lin_1,
  file = paste(path, "dataset_lin_1.csv", sep = ""),
  sep = ";",
  row.names = FALSE
)

write.table(
  dataset_lin_2,
  file = paste(path, "dataset_lin_2.csv", sep = ""),
  sep = ";",
  row.names = FALSE
)

write.table(
  dataset_lin_3,
  file = paste(path, "dataset_lin_3.csv", sep = ""),
  sep = ";",
  row.names = FALSE
)


write.table(
  dataset_lin_1_dt,
  file = paste(path, "dataset_lin_1_dt.csv", sep = ""),
  sep = ";",
  row.names = FALSE
)

write.table(
  dataset_lin_2_dt,
  file = paste(path, "dataset_lin_2_dt.csv", sep = ""),
  sep = ";",
  row.names = FALSE
)

write.table(
  dataset_lin_3_dt,
  file = paste(path, "dataset_lin_3_dt.csv", sep = ""),
  sep = ";",
  row.names = FALSE
)


print("Output complete")



### CLEAN WORKSPACE ################################################################################
rm(list=ls())
