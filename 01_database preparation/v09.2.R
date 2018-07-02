#### COMMENTS ####
# Combines HIS, CAS, new age, new endgrd datasets to databasis for analysis
# Exports:
#   dataset     --> contains only exam data, for further usage!
#   dataset_all --> contains module and exam data
#   ds_pnr_hash --> hash matrix to assign new to old pnr

#ADDITION: Data can be filtered in order to win a past perspective,
#In order to do so define a data parameter "cutoffdate"
#For QPL: Consider data before WS 2011/21 (<15.10.2011)
#best set parameter in main file

### LIBRARY ########################################################################################
library(ggplot2)
library(dplyr)


#### FIRST DATASET ####
print("1) Preparing dataset 1: HIS")

### READING ########################################################################################
print("Import and merge HIS dataset and age data")

setwd("C:\\Users\\Lorenz\\OneDrive\\Documents\\Universität\\Projekte\\Predicting Student Dropout\\Implementation\\working version")

# read input-data
input_data <- read.csv(
  file = "01_database preparation/01_input/dataset_1_HIS.csv", 
  sep = ";"
  )
new_age <- read.csv(
  file = "01_database preparation/01_input/dataset_new age.csv", 
  sep = ";"
  )

# standardize coloumn naming
names(new_age) <- c("id_new_age", "age")
names(input_data)[names(input_data) == "pdatum"] <- "pdat"
names(input_data)[names(input_data) == "endedat"] <- "enddat"
names(input_data)[names(input_data) == "endegrd"] <- "endgrd"

# combine input_data and new_age in dataset1
# delete coloumns which have been replaced with new_age, abschl, stg
dataset1 <- cbind(input_data, new_age) %>%
  select(
    -id_new_age, -altersklasse.studiumsbeginn, -abschl, -stg
  )

# remove input data from environment
rm(input_data, new_age)

### CLEANING #######################################################################################
print("Clean dataset 1")

# delete all spaces in staat and "" -> "N"
dataset1 <- dataset1 %>%
  mutate (
    staat = gsub(" ", "", staat)
  )

dataset1[c("staat")][dataset1[c("staat")] == ""] <- "N"

# endgrd NA --> 3
dataset1[c("endgrd")][is.na(dataset1[c("endgrd")])] <- 3                          # ENDGRD <- 3 !!!

# 5 entries: (pstatus == EN | NB) & pnote != 5.0 --> pnote == 5.0
nb_ds1 <- dataset1 %>% filter((pstatus == "EN"| pstatus == "NB") & pnote < 5)
dataset1 <- dataset1 %>% setdiff(nb_ds1, dataset1)
rm(nb_ds1)

### FORMATTING #####################################################################################
print("Format dataset 1")

#assign data types
dataset1 <- dataset1 %>%
  mutate(
    pdat = as.Date(pdat, "%d.%m.%y"),
    anfdat = as.Date(anfdat, "%d.%m.%y"),
    enddat = as.Date(enddat, "%d.%m.%y"),
    staat = factor(staat),
    pnote = as.numeric(pnote)
  )

# rearrange columns
dataset1 <- dataset1[, c(1,7,8,12,9:11,6,2,4,3,5)]

# add column "source" for dataset1
dataset1$source <- rep(1,nrow(dataset1))

# scale pnote [100, 500] --> [1.0, 5.0]
dataset1$pnote <- dataset1$pnote / 100



#### SECOND DATASET ####
print("2) Preparing dataset 2: CAS")

### READING ########################################################################################
# before importing, replace all special characters (Ã¼, Å¸ --> ue, etc.) in .csv file
print("Import CAS data")

# import data-csv file
dataset2 <- read.csv(
  file = "01_database preparation/01_input/dataset_2_CAS.csv", 
  sep = ";"
  )

print("Format dataset 2")
# standardize coloumn naming
names(dataset2)[names(dataset2) == "Geschlecht"] <- "geschl"
names(dataset2)[names(dataset2) == "Nationalitaet"] <- "staat"
names(dataset2)[names(dataset2) == "Studienbeginn"] <- "anfdat"
names(dataset2)[names(dataset2) == "Vertragsende"] <- "enddat"
names(dataset2)[names(dataset2) == "Kuendigungsgrund"] <- "endgrd"
names(dataset2)[names(dataset2) == "Pruefungsnummer"] <- "pnr"
names(dataset2)[names(dataset2) == "Note"] <- "pnote"
names(dataset2)[names(dataset2) == "Versuch"] <- "pversuch"
names(dataset2)[names(dataset2) == "pruefungsdatum"] <- "pdat"
names(dataset2)[names(dataset2) == "Altersklasse.Studiumsbeginn"] <- "age"

# add coloumn "pstatus", add coloumn "source" == 2
dataset2$pstatus <- (NA)
dataset2$source <- rep(2,nrow(dataset2))

# rearrange columns
dataset2 <- dataset2[, c(1:3,11,4:6,10,7,12,8,9,13)]


### CLEANING #######################################################################################
print("Clean dataset 2")

# format wording to fit dataset1
dataset2 <- dataset2 %>%
  mutate (
    geschl = gsub("maennlich", "M", geschl),
    geschl = gsub("weiblich", "W", geschl),
    staat = gsub("Deutschland", "D", staat),
    enddat = gsub("NULL", "IM", enddat),
    endgrd = gsub("NULL", "IM", endgrd),
    pnr = gsub("NULL", "AK", pnr),
    pnote = gsub("bestanden", "BE", pnote),
    pnote = gsub(",", ".", pnote)
  )

# format content: empty, NUll and NA data to fit dataset1
dataset2[c("staat")][dataset2[c("staat")] == ""] <- "N"
dataset2[c("staat")][dataset2[c("staat")] == "NULL"] <- NA
dataset2[c("endgrd")][is.na(dataset2[c("endgrd")])] <- 3
dataset2[c("endgrd")][dataset2[c("endgrd")] == ""] <- 3
dataset2[c("endgrd")][dataset2[c("endgrd")] == "IM"] <- 3
dataset2[c("endgrd")][dataset2[c("endgrd")] == "Beendigung n. endgueltig nicht bestand.Pruefung/VPr."] <- 8
dataset2[c("endgrd")][dataset2[c("endgrd")] == "Endgueltiger Abbruch des Studiums"] <- 6
dataset2[c("endgrd")][dataset2[c("endgrd")] == "Hochschulwechsel"] <- 4
dataset2[c("endgrd")][dataset2[c("endgrd")] == "sonstige Gruende"] <- 9
dataset2[c("endgrd")][dataset2[c("endgrd")] == "Streichung d.d.HS wg.fehlender Rueckmeldung"] <- 5
dataset2[c("endgrd")][dataset2[c("endgrd")] == "Unterbrechung des Studiums"] <- 2
dataset2[c("pnote")][dataset2[c("pnote")] == "NULL"] <- NA
dataset2[c("pversuch")][dataset2[c("pversuch")] == "NULL"] <- NA
dataset2[c("pdat")][dataset2[c("pdat")] == "NULL"] <- NA

### FORMATTING #####################################################################################
# begin generating pstatus values
dataset2$pstatus <- ifelse(dataset2$pnote == "BE", "BE", dataset2$pstatus)
dataset2$pnote <- ifelse(dataset2$pnote == "BE", NA, dataset2$pnote)

# format data types
dataset2 <- dataset2 %>%
  mutate(
    anfdat = as.Date(anfdat, "%Y-%m-%d %H:%M:%OS"),                             # NA ERZEUGUNG !!!
    enddat = as.Date(enddat, "%Y-%m-%d %H:%M:%OS"),                             # NA ERZEUGUNG !!!
    pdat = as.Date(pdat, "%Y-%m-%d %H:%M:%OS"),                                 # NA ERZEUGUNG !!!
    staat = as.character(staat),
    geschl = as.factor(dataset2$geschl),
    staat = as.factor(dataset2$staat),
    endgrd = as.factor(dataset2$endgrd),
    pnr = as.character(dataset2$pnr),
    pnote = as.numeric(dataset2$pnote)
  )

#...continue generating pstatus values 
dataset2$pstatus <- ifelse(dataset2$pnote >= 1 & dataset2$pnote < 5, "BE", dataset2$pstatus)
dataset2$pstatus <- ifelse(dataset2$pnote == 5, "NB", dataset2$pstatus)
dataset2$pstatus <- ifelse(dataset2$pnr == "AK", "AK", dataset2$pstatus)
dataset2$pstatus = as.factor(dataset2$pstatus)
dataset2$pnr <- ifelse(dataset2$pnr  == "AK", NA, dataset2$pnr)


#### THIRD DATASET ####
print("3) Combine dataset 1 and 2")

### READING ########################################################################################
print("Combine datasets and import corrected endgrd data")

# combine dataset1 and dataset2 to new dataset3, load new endgrd data
dataset3 <- rbind(dataset1, dataset2)

ds_endgrd <- read.csv(
  file = "01_database preparation/01_input/dataset_corrected endgrd.csv",
  sep = ";")

rm(dataset1, dataset2)

### CLEANING #######################################################################################
print("Remove NA in id, if neccessary")

dataset3 <- dataset3 %>%
  filter(!is.na(id))

### FORMATTING #####################################################################################
print("Format combined dataset")

# format dataset column formats
dataset3 <- dataset3 %>%
  mutate(
    anfdat = as.Date(anfdat, "%Y-%m-%d"),
    enddat = as.Date(enddat, "%Y-%m-%d"),
    pdat = as.Date(pdat, "%Y-%m-%d"),
    geschl = as.factor(dataset3$geschl),
    staat = as.factor(dataset3$staat),
    endgrd = as.numeric(dataset3$endgrd),
    pnr = as.character(dataset3$pnr),
    pnote = as.numeric(dataset3$pnote)
  )

# rename columns
ds_endgrd <- ds_endgrd %>%
  rename(endgrd = endegrd, enddat = endedat, id = ID)

# merge ds3 and ds_endgrd by "id"
# delete duplicate rows
# rename columns
# format anfdat, enddat format
dataset <- full_join(x = ds_endgrd, y = dataset3,by = "id") %>%
  rename(endgrd = endgrd.x, anfdat = anfdat.x, enddat = enddat.x) %>%
  mutate(
    anfdat = as.Date(anfdat, "%d.%m.%y"),
    enddat = as.Date(enddat, "%d.%m.%y"),
    anfdat = ifelse(is.na(anfdat), anfdat.y, anfdat),
    enddat = ifelse(is.na(enddat), enddat.y, anfdat),
    endgrd = ifelse(is.na(endgrd), endgrd.y, endgrd),
    anfdat = as.Date(anfdat, origin = "1970-01-01"),
    enddat = as.Date(enddat, origin = "1970-01-01")
  ) %>%
  select(-anfdat.y, -enddat.y, -endgrd.y)

rm(dataset3, ds_endgrd)

### NA_HANDLING ####################################################################################
print("Remove rows containing lots of NA")

# 3677 rows: delete rows containing lots of NA
na_ds3 <- dataset %>% 
  filter(
    is.na(pdat) & is.na(pnr) & is.na(pnote) & is.na(pversuch)
  )

dataset <- dataset %>% 
  setdiff(
    na_ds3, 
    dataset
  )

rm(na_ds3)

# 9 rows: if (pstatus == "EN" | pstatus == "NB") & pnote < 5 --> pnote == 5
nb_ds3 <- dataset %>% 
  filter(
    (pstatus == "NB" | pstatus == "EN") & pnote < 5
  )

dataset <- setdiff(dataset, nb_ds3)

nb_ds3$pnote <- 5.0

dataset <- bind_rows(dataset, nb_ds3)

rm(nb_ds3)



#### PNR DATASET ###################################################################################
# ds_pnr   --> Only exam pnr, excl. modules
# pnr_hash --> All pnr
# ds_mod   --> Only modules

print("4) Remove rows from modules")

# numeric parse because of mean(source)
#dataset$source <- as.numeric(dataset$source)

ds_pnr <- dataset %>%
  group_by(
    pnr, source
  )%>%
  summarise(
    count= n()
  )

#ds_pnr$source <- as.factor(ds_pnr$source)
# convert pnr_new to numeric
ds_pnr$pnr_new <- ds_pnr$pnr
ds_pnr <- within(ds_pnr, {
  pnr_new <- as.numeric(as.character(ds_pnr$pnr_new))
})

ds_pnr$index <- as.numeric(1:nrow(ds_pnr))


# type: pnr = NA --> 0,  modul --> 1, ds1 --> 2, ds2 --> 3
# 610001, 610002, 610005, 610010, 100000 --> Special case: Modul No > 9999
ds_pnr <- ds_pnr %>%
  mutate(type = as.numeric(ifelse(pnr_new < 9999 & pnr_new > 1000, 1, 2))) %>%
  mutate(type = as.numeric(ifelse(pnr_new == 610001 | pnr_new == 610002 | pnr_new == 610005 | 
                         pnr_new == 610010 | pnr_new == 100000,
                       1, type))) %>%
  mutate(type = ifelse(is.na(type), 3, type)) %>%
  mutate(type = ifelse(is.na(pnr) & is.na(pnr_new), 0, type)) %>%
  mutate(pnr_new = ifelse(type == 3, index-601, pnr_new))

# rearrange
ds_pnr <- ds_pnr[, c(1, 4, 6, 3, 2)]

# create pnr_hash incl. exams & module & NAs from ds1 & ds2
pnr_hash <- ds_pnr
# keep only exams
ds_pnr <- pnr_hash %>% filter(type != 1)
# keep only modules
ds_mod <- pnr_hash %>% filter(type == 1)

rm(ds_mod)


#### FINAL DATASET ####
print("5) Prepare final dataset")

# delete module rows from dataset
# combine ds with pnr_ds by old pnr
ds_pnr_corrected <- full_join(
    x = ds_pnr, 
    y = dataset,
    by = "pnr"
  ) %>%
  select(
    -count, 
    -source.x
  ) %>%
  rename(
    source = source.y, 
    pnr_old = pnr, 
    pnr = pnr_new
  )

# delete rows from modules
ds_pnr_corrected <- subset(ds_pnr_corrected, !is.na(type))

# rearrange
ds_pnr_corrected <- ds_pnr_corrected[, c(4:11, 2, 12:15)]
dataset_all <- dataset
dataset <- ds_pnr_corrected

rm(ds_pnr, ds_pnr_corrected)

# delete duplicate ids: 150090326, 150091579, 150096216 : 138 entries
duplicates <- dataset %>%
  filter(
    id == 150090326 & endgrd == 7 | id == 150091579 & endgrd == 4 | id == 150096216 & endgrd == 6
  )

dataset <- setdiff(
  x = dataset, 
  y = duplicates
)

rm(duplicates)

# 150099741 : endgrd = 0 <- 1
dataset <- dataset %>%
  mutate(
    endgrd = ifelse(id == 150099741, 1, endgrd)
  )

# delete 150027794, only 1 entry and endgrd = 1
dataset <- subset(dataset, id != 150027794)
# delete if pstatus = NA : 4 entries
dataset <- subset(dataset, !is.na(pstatus))
# delete if pstatus = "AN" : 7946 entries
dataset <- subset(dataset, pstatus != "AN")
# delete if pnote = NA : 1833 entries 
dataset <- subset(dataset, !is.na(pnote))

# formatting
dataset <- dataset %>%
  mutate(
    id      = as.character(id),
    anfdat  = as.Date(anfdat, "%d.%m.%y"),
    enddat  = as.Date(enddat, "%d.%m.%y"),
    endgrd  = as.numeric(endgrd),
    age     = as.numeric(age),
    pdat    = as.Date(pdat, "%d.%m.%y"),
    pstatus = as.character(pstatus),
    pnote   = as.numeric(pnote),
    pversuch = as.numeric(pversuch),
    source  = as.numeric(source)
  )

# format geschl (<- sex_m) staat (<- staat_d) as int [0, 1], pstatus as [1:4]
dataset <- dataset %>%
  mutate(geschl = ifelse(geschl == "M", 1, 0), staat = ifelse(staat == "D", 1, 0)) %>%
  rename(sex_m = geschl, staat_d = staat) %>%
  mutate(pstatus = ifelse(pstatus == "BE", 1, ifelse(pstatus == "NB", 2, ifelse(pstatus == "EN", 3, 4)))) %>%
  arrange(id)


### MUTATION #######################################################################################
print("Extract features Sem and Success")

# create semester and success (0 = fail, 1 = success, 2 = enrolled/paused)
dataset <- dataset %>%
  mutate( sem = ceiling((as.numeric(pdat) - as.numeric(anfdat)) / 180),
          success = ifelse(endgrd == 1, 1, ifelse(endgrd == 3 | endgrd == 2, 2, 0))
  )

### FALSE-HANDLING #################################################################################

# SEM
# replace false sem with median value, if not possible --> delete
sem_err <- dataset %>%
  filter(sem < 1 | sem > 12 | is.na(sem))
# ds with correct sem:
sem_corr <- setdiff(dataset, sem_err)
# ds with data to replace false data:
sem_avg <- sem_corr %>%
  group_by(pnr) %>%
  summarise(avg_sem = round(median(sem),0))
# replace data in sem_err
sem_err <- left_join(sem_err, sem_avg,by = "pnr")
sem_err <- sem_err[, c(1:13,16,15)]
sem_err <- sem_err %>%
  rename(sem = avg_sem)
# remove last is.na(sem)
sem_err <- sem_err %>%
  filter(!is.na(sem))
dataset <- rbind(sem_err, sem_corr)
rm(sem_err, sem_corr, sem_avg)

# STAAT
# staat_d == NA --> delete 37 entries
dataset <- dataset %>%
  filter(!is.na(staat_d))

# PNR
# is.na(pnr) <- 216 entries (all paused/enrolled --> not important) --> delete
dataset <- dataset %>%
  filter(!is.na(pnr))

# PVERSUCH
# is.na(pversuch) <- 23 entries (all paused/enrolled --> not important) --> delete
dataset <- dataset %>%
  filter(!is.na(pversuch))

# PNOTE
# pnote == 0 <- 375 entries --> delete
dataset <- dataset %>%
  filter(!pnote == 0)

#Filtere Daten nach einem bestimmten Tag --> gewinne neue zeitl. Perspektive
#Für QPL: Welche Informationen hatte die Universität zum Eingang in das WS 11/12?
#leave parameter undefined if you want to use all data
if(exists("cutoffdate"))  {
  dataset <- dataset %>%
  filter(pdat > cutoffdate)
  }

### REORDER ########################################################################################
dataset <- arrange(dataset, id)

### EXPORT ########################################################################################
print("Export:")
print("dataset_all (contains module and exam data),")
print("dataset (contains only exam data),")
print("pnr_hash (contains matrix to assign fitted pnrs to exams)")
print("Path: 01_database preparation/02_output/")

write.table(dataset_all, file = "01_database preparation/02_output/dataset_all.csv", sep = ";", row.names = FALSE)
write.table(dataset, file = "01_database preparation/02_output/dataset.csv", sep = ";", row.names = FALSE)
write.table(pnr_hash, "01_database preparation/02_output/pnr_hash.csv", sep = ";", row.names = FALSE)

rm(pnr_hash, dataset_all,QPLparam)

print("*** ENDED ***")
