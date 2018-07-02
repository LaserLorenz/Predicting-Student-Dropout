### COMMENTS #######################################################################################
# input         : result from folder "01_database preparation"

# output:
# dataset       : all exams --> ds for linearization
# dataset_mean  : 3x exam per id, with average features (incl sem 1, 2, 3)
# dataset_exam  : "valid" exams for analysis -> enough observations for regression

# dataset_idsem : summarises dataset by id/semester --> plots
# dataset_mean_all    : summarises dataset by id --> plots

### LIBRARY ########################################################################################
library(ggplot2)
library(dplyr)

### FUNCTIONS ######################################################################################

build_mean_ds <- function(data = dataset, sem_max = data$sem){
  
  # filter semester > sem_max, success != 2, replace pstatus
  ds <- dataset %>% 
    filter(
      sem <= sem_max, 
      success != 2
    ) %>% 
    mutate(
      pstatus = replace(
        pstatus, 
        pstatus == 3,
        2
      )
    )
  
  # extract features
  ds <- ds %>%
    group_by(
      id
    ) %>%
    summarise(
      success = mean(success),
      sex_m = mean(sex_m),
      staat_d = mean(staat_d),
      age = mean(age),
      p_count = n(),
      note_avg = mean(pnote),
      sem_max = max(sem),
      be_count = sum(pstatus == 1),
      nb_count = sum(pstatus == 2)
    )
  
  return(ds)
}

exam_analysis <- function(dataset, sem_filter = dataset$sem){
  # remove data from sem > sem_filter
  # remove active students
  dataset <- dataset %>%
    filter(
      success != 2,
      sem <= sem_filter
    )
  
  # keep only latest try of each exam per student (function from linearization)
  dataset <- dataset %>%
    group_by(
      id, pnr
    ) %>%
    arrange(
      sem
    ) %>% 
    filter(
      row_number() == n()
    )
  
  # create two datasets: 
  # sucess == 1 / 0
  # keep exam if #observations >= 15
  dataset_1 <- subset(dataset, dataset$success == 1) %>% 
    group_by(pnr) %>% 
    summarise(count = n()) %>% 
    filter(count >= 15)
  
  dataset_0 <- subset(dataset, dataset$success == 0) %>% 
    group_by(pnr) %>% 
    summarise(count = n()) %>% 
    filter(count >= 15)
  
  # merge datasets: keep only if pnr exists in both
  # 29 exams (-167)
  dataset_exam <- semi_join(dataset_1, dataset_0, by = "pnr")
  
  return(dataset_exam)
}



### READING ########################################################################################
# read input-data

setwd("C:\\Users\\Lorenz\\OneDrive\\Documents\\Universität\\Projekte\\Predicting Student Dropout\\Implementation\\working version")

dataset <- read.csv(
  file = "01_database preparation/02_output/dataset.csv", 
  sep = ";"
  )

print("Import data from database preparation")

### FORMATTING #####################################################################################
dataset <- dataset %>%
  mutate(
    anfdat = as.Date(anfdat)
  )

# keep only important information
print("Remove irrelevant information from dataset")
dataset <- dataset[, c(1,15,5:7,9,11,14,10,12)]



### MUTATION #######################################################################################

# group by id and filter by semester
dataset_mean_3 <- build_mean_ds(data = dataset, sem_max = 3)
dataset_mean_2 <- build_mean_ds(data = dataset, sem_max = 2)
dataset_mean_1 <- build_mean_ds(data = dataset, sem_max = 1)


# datasets required for plots
# group by semester
dataset_idsem <- dataset %>%
  group_by(
    id,
    sem,
    sex_m,
    success,
    staat_d,
    age
  ) %>%
  summarise(
    p_count = n(),
    note_avg = mean(pnote),
    be_count = sum(pstatus == 1),
    nb_count = sum(pstatus == 2),
    en_count = sum(pstatus == 3)
  )

dataset_idsem <- dataset_idsem[,c(1, 3:8, 2, 9:11)]

# group by id
dataset_mean_all <- build_mean_ds(dataset)



### PLOTS ##########################################################################################
#source("C:\\Users\\Lorenz\\OneDrive\\Documents\\Universität\\Veröffentlichungen\\Predicting Student Dropout\\Implementation\\working version\\02_preanalysis\\plots.R")

### EXAM ANALYSIS ##################################################################################

dataset_exam_1 <- exam_analysis(data = dataset, sem_filter = 1)
dataset_exam_2 <- exam_analysis(data = dataset, sem_filter = 2)
dataset_exam_3 <- exam_analysis(data = dataset, sem_filter = 3)
#dataset_exam_all <- exam_analysis(data = dataset)



### EXPORT #########################################################################################
print("Export dataset, dataset_mean, dataset_mean_all, dataset_idsem, exams to:")
spath <- "02_preanalysis/02_output/"
print(spath)

write.table(
  dataset,
  file = paste(spath, "dataset.csv", sep = ""),
  sep = ";",
  row.names = FALSE
)

write.table(
  dataset_mean_1,
  file = paste(spath, "dataset_mean_1.csv", sep = ""),
  sep = ";",
  row.names = FALSE
)

write.table(
  dataset_mean_2,
  file = paste(spath, "dataset_mean_2.csv", sep = ""),
  sep = ";",
  row.names = FALSE
)

write.table(
  dataset_mean_3,
  file = paste(spath, "dataset_mean_3.csv", sep = ""),
  sep = ";",
  row.names = FALSE
)

#write.table(dataset_mean_all, file = paste(spath, "dataset_mean_all.csv", sep = ""), sep = ";", row.names = FALSE)


write.table(
  dataset_exam_1,
  file = paste(spath, "dataset_exam_1.csv", sep = ""),
  sep = ";",
  row.names = FALSE
)

write.table(
  dataset_exam_2,
  file = paste(spath, "dataset_exam_2.csv", sep = ""),
  sep = ";",
  row.names = FALSE
)

write.table(
  dataset_exam_3,
  file = paste(spath, "dataset_exam_3.csv", sep = ""),
  sep = ";",
  row.names = FALSE
)

#write.table(dataset_exam_all, file = "~/Desktop/Masterarbeit_local/02_preanalysis/02_output/exams_all.csv", sep = ";", row.names = FALSE)

print("*** ENDED ***")


# clean workspace
rm(list=ls())
