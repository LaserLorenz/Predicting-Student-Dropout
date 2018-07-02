### LIBRARY ########################################################################################
library(ggplot2)
library(dplyr)
library("wesanderson")

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

### DATA IMPORT ##########################################################################################
setwd("C:\\Users\\Lorenz\\OneDrive\\Documents\\Universität\\Projekte\\Predicting Student Dropout\\Implementation\\working version")

spath <- "02_preanalysis/02_output/10_plots"


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

### PLOTS #####################################################################################

plot_data <- dataset %>%
  filter(
    sem <= 2
  ) %>%
  group_by(
    id
  ) %>%
  summarise(
    success = mean(success),
    p_count = n(),
    note_avg = mean(pnote),
    sem_max = max(sem),
    be_count = sum(pstatus == 1),
    nb_count = sum(pstatus == 2),
    en_count = sum(pstatus == 3)
  ) %>%
  filter(
    (be_count/sem_max) < 15 & success != 2
  )

my_plot <- ggplot(
  plot_data,
  aes(
    x = note_avg,
    y = (be_count/sem_max),
    color = as.factor(success)
  )
) + 
  geom_point(
    alpha = 0.5,
    size = 1.5,
    position = "jitter"
  ) +
  scale_color_manual(
    breaks = c("0", "1"),
    values=wes_palette(n=4, name="Zissou")[c(3,1)],
    labels=c("dropout", "success")
  ) +
  coord_cartesian(
    xlim = c(1, 5), 
    ylim = c(0, 15)
  ) +
  labs(
    # title = "Students within first/second sem: Note vs. be vs. success",
    x = "Average result (grade)", 
    y = "Count of passed examinations per semester",
    colour = ""
  ) +

  theme(text=element_text(family="CM Roman"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20)),
        axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0))
  ) +
  theme_bw()

plot(my_plot)

ggsave("ROC curves_ub.pdf", plot = my_plot, device = "pdf", path = spath,
       scale = 1, width = NA, height = NA, dpi = 500)

rm(plot_data, my_plot)
