# descriptive data for TOP

rm(list = ls())


# load packages
library(tidyverse)

# set wd
setwd("C:/Users/olgas/OneDrive - SISSA/SISSA/Projects/Valencia/Topdown_orthographic_processing/Analysis")

# import the dataset
top_data_final <- read.csv("top_data_final_EXCLUDED_21-Mar-2024_19.58.44h.csv", header = T)


# separate into two datasets for the two tasks and get rid of unecessary columns
ldt_results <- top_data_final %>% filter(task == "ldt") %>% select(-c(rt_sd, acc_sd)) %>% dplyr::rename(rt = rt_ldt, acc = acc_ldt)

sd_results <- top_data_final %>% filter(task == "sd") %>% select(-c(rt_ldt, acc_ldt)) %>% dplyr::rename(rt = rt_sd, acc = acc_sd)


# Define a function to compute the within-subject SE using Cousineau's method REACTION TIMES or ACCURACY
compute_within_subject_se <- function(data, value_col, subject_col) {
  # Calculate the mean for each subject in each condition
  subject_condition_means <- data %>%
    group_by(get(subject_col), frequency, presentation, relatedness) %>%
    dplyr::summarize(mean_value = mean(get(value_col), na.rm = TRUE), .groups = 'drop') 
  colnames(subject_condition_means)[1] <- "participant"
  # Calculate the grand mean for each condition
  condition_means <- data %>%
    group_by(frequency, presentation, relatedness) %>%
    dplyr::summarize(grand_mean = mean(get(value_col), na.rm = TRUE), .groups = 'drop')
  
  # Normalize the data
  data <- data %>%
    left_join(subject_condition_means, by = c(subject_col, "frequency", "presentation", "relatedness")) %>%
    left_join(condition_means, by = c("frequency", "presentation", "relatedness")) %>%
    mutate(normalized = get(value_col) - mean_value + grand_mean) %>%
    dplyr::select(-mean_value, -grand_mean)
  
  # Calculate the standard deviation of the normalized data for each condition
  condition_std <- data %>%
    group_by(frequency, presentation, relatedness) %>%
    summarize(std_dev = sd(normalized, na.rm = TRUE), .groups = 'drop')
  
  # Calculate the number of subjects
  num_subjects <- n_distinct(data[[subject_col]])
  
  # Calculate the standard error
  condition_std <- condition_std %>%
    mutate(se = std_dev / sqrt(num_subjects)) %>%
    select(-std_dev)
  
  return(condition_std)
}


# LDT descriptives ---------------------------------------------------------


# reaction times

# remove NAs and short RTs
rt_data_ldt <- ldt_results %>% filter(!is.na(rt) & rt>0.25)

# keep only words
rt_data_ldt <- rt_data_ldt %>% filter(lexicality == "word")

# remove incorrect responses
rt_data_ldt <- rt_data_ldt %>% filter(acc == 1)

means_rt_ldt <- aggregate(rt*1000 ~ relatedness + presentation + frequency, FUN = mean, rt_data_ldt)
means_rt_ldt$`rt * 1000` <- round(means_rt_ldt$"rt * 1000", 0)
means_rt_ldt

se_rt_ldt <- compute_within_subject_se(rt_data_ldt, "rt", "participant")
se_rt_ldt$se <- se_rt_ldt$se * 1000
se_rt_ldt

#############################
sem_data <- rt_data_ldt %>%
  group_by(participant) %>%
  summarise(
    mean_rt = mean(rt),
    sd_rt = sd(rt),
    n = n(),
    sem_rt = sd_rt / sqrt(n)
  )
sem_data
##############################
# accuracy
acc_data_ldt <- ldt_results %>% filter(lexicality == "word")

means_acc_ldt <- aggregate(acc~ relatedness + presentation + frequency, FUN = mean, acc_data_ldt)
means_acc_ldt$"acc" <- round(means_acc_ldt$"acc", 3)
means_acc_ldt$error_rate <- (1 - means_acc_ldt$acc) * 100
means_acc_ldt


se_accuracy_ldt_ER <- compute_within_subject_se_ER(acc_data_ldt, "acc", "participant")
se_accuracy_ldt_ER$se_error_rate <- se_accuracy_ldt_ER$se * 100
se_accuracy_ldt_ER

# SD descriptives ---------------------------------------------------------
# reaction times

# remove NAs and short RTs
rt_data_sd <- sd_results %>% filter(!is.na(rt) & rt>0.25)

# keep only words
rt_data_sd <- rt_data_sd %>% filter(lexicality == "word")

# only "SAME" trials
rt_data_sd <- rt_data_sd %>% filter(same_diff == "same")

# remove incorrect responses
rt_data_sd <- rt_data_sd %>% filter(acc == 1)

# convert to milliseconds
rt_data_sd$rt <- rt_data_sd$rt * 1000


means_rt_sd <- aggregate(rt ~ relatedness + presentation + frequency, FUN = mean, rt_data_sd)
means_rt_sd$"rt" <- round(means_rt_sd$"rt", 0)
means_rt_sd


# Compute the SE for RT using only correct responses
se_rt_sd <- compute_within_subject_se(rt_data_sd, "rt", "participant")
se_rt_sd

# accuracy
acc_data_sd <- sd_results %>% filter(lexicality == "word")

acc_data_sd <- acc_data_sd %>% filter(same_diff == "same")


# Compute the SE for accuracy using the original data
se_accuracy_sd_ER <- compute_within_subject_se(acc_data_sd, "acc", "participant")
se_accuracy_sd_ER$se_error_rate <- se_accuracy_sd_ER$se * 100
se_accuracy_sd_ER




# library(Rmisc) # has summarySE function
# summary_acc <- summarySE(data=rt_data_ldt, measurevar="rt", groupvars=c("frequency", "relatedness", "presentation"), na.rm=TRUE,
#                          conf.interval=.95, .drop=TRUE)
# summary_acc
# 
# 
# 
# sem_data <- rt_data_ldt %>%
#   group_by(frequency, relatedness, presentation) %>%
#   dplyr::summarise(
#     mean_rt = mean(rt),
#     sd_rt = sd(rt),
#     n = n(),
#     sem_rt = sd_rt / sqrt(n)
#   )
# sem_data




