# data analysis
# topdown captcha masked priming LDT and SD
# 14-3-2023
# Olga Solaja

#clean environment
rm(list = ls())

# import packages
library(tidyverse) # data wrangling
library(data.table) # read in data
library(ggplot2) # plots
library(lmerTest) # lmer
library(table1) # descriptives

# disable scientific notation
options(scipen = 999)


#set working directory
setwd("C:\\Users\\olgas\\OneDrive - SISSA\\SISSA\\Projects\\Valencia\\Topdown_orthographic_processing\\Analysis\\data")



# Data preparation --------------------------------------------------------

# data with list 6 have different columns for some reason so I'll import them later

# import the data
files <- list.files(pattern = ".csv")
temp <- lapply(files, fread, sep=",")
messy_results_1 <- rbindlist(temp, use.names = T)
rm(temp,files)


# get only the main trials
# messy_results <- messy_results %>% rename(item = ï..item)
messy_results_1 <- messy_results_1 %>% filter(item < 1000)


# get only the columns we need  # this also gets rid of the number of columns problem, the ppt6_prac... thing
results_1 <- select(messy_results_1, c(item, participant, list, task, lexicality, frequency, relation, presentation, same_diff, target_response_ldt.corr, target_response_ldt.rt, target_response_sd.corr, target_response_sd.rt))


# fix names
results_1 <- results_1 %>% dplyr::rename(rt_ldt = target_response_ldt.rt, rt_sd = target_response_sd.rt, acc_ldt = target_response_ldt.corr, acc_sd = target_response_sd.corr)


# fix type of data
results_1 <- results_1 %>% mutate(across(c(participant, list, task, lexicality, frequency, relation, presentation, same_diff), as.factor))

# import list 6

#set working directory
setwd("C:\\Users\\olgas\\OneDrive - SISSA\\SISSA\\Projects\\Valencia\\Topdown_orthographic_processing\\Analysis\\data\\6")

# import the data
files <- list.files(pattern = ".csv")
temp <- lapply(files, fread, sep=",")
messy_results_2 <- rbindlist(temp, use.names = T)
rm(temp,files)


# get only the main trials
# messy_results <- messy_results %>% rename(item = ï..item)
messy_results_2 <- messy_results_2 %>% filter(item < 1000)


# get only the columns we need  # this also gets rid of the number of columns problem, the ppt6_prac... thing
results_2 <- select(messy_results_2, c(item, participant, list, task, lexicality, frequency, relation, presentation, same_diff, target_response_ldt.corr, target_response_ldt.rt, target_response_sd.corr, target_response_sd.rt))


# fix names
results_2 <- results_2 %>% dplyr::rename(rt_ldt = target_response_ldt.rt, rt_sd = target_response_sd.rt, acc_ldt = target_response_ldt.corr, acc_sd = target_response_sd.corr)


# fix type of data
results_2 <- results_2 %>% mutate(across(c(participant, list, task, lexicality, frequency, relation, presentation, same_diff), as.factor))


# get the list 5 last ppts (they have an additional PROLIFIC_PID column so they cant be read with the others)

#set working directory
setwd("C:\\Users\\olgas\\OneDrive - SISSA\\SISSA\\Projects\\Valencia\\Topdown_orthographic_processing\\Analysis\\data\\5_last_ppts")

# import the data
files <- list.files(pattern = ".csv")
temp <- lapply(files, fread, sep=",")
messy_results_3 <- rbindlist(temp, use.names = T)
rm(temp,files)


# get only the main trials
# messy_results <- messy_results %>% rename(item = ï..item)
messy_results_3 <- messy_results_3 %>% filter(item < 1000)


# get only the columns we need  # this also gets rid of the number of columns problem, the ppt6_prac... thing
results_3 <- select(messy_results_3, c(item, participant, list, task, lexicality, frequency, relation, presentation, same_diff, target_response_ldt.corr, target_response_ldt.rt, target_response_sd.corr, target_response_sd.rt))


# fix names
results_3 <- results_3 %>% dplyr::rename(rt_ldt = target_response_ldt.rt, rt_sd = target_response_sd.rt, acc_ldt = target_response_ldt.corr, acc_sd = target_response_sd.corr)


# fix type of data
results_3 <- results_3 %>% mutate(across(c(participant, list, task, lexicality, frequency, relation, presentation, same_diff), as.factor))




# merge to one df
results <- rbind(results_1, results_2, results_3)

#######################################
# checking the duplicate participants
#######################################


# subset of ppts in the first 3 lists
lists_123 <- results %>% filter(list == 1 | list == 2 | list == 3)

plot(lists_123$participant) # one person did it two times
summary(lists_123) # it's ppt 5f4e8e05550a4d095a76387f

# check in which lists he appeared
summary(subset(lists_123, participant == "5f4e8e05550a4d095a76387f")) # he appears in 1 and 3; exclude him from 3

# exclude this person from list 3 but keep in list 1 and check again
lists_123_exclusion <- subset(lists_123, !(participant == "5f4e8e05550a4d095a76387f" & list == 3))

plot(lists_123_exclusion$participant) # now it's fine
sum(length(unique(lists_123_exclusion$participant))) # there's 164 ppts


# checking how many ppts from list 3 did also 4, 5, 6, 7 or 8
list_3 <- lists_123_exclusion %>% filter(list == 3)

# subset for the rest of the lists
lists_rest <- results %>% filter(list != 1 & list != 2 & list != 3)


# ppts who did both 3 and 4, 5, 6, 7 or 8
overlap_lists_3_45678 <- subset(lists_rest, (participant %in% list_3$participant))
sum(length(unique(overlap_lists_3_45678$participant))) # 57 of them

# ppts who only did 3
nonoverlap_lists_3_45678 <- subset(list_3, !(participant %in% overlap_lists_3_45678$participant))
sum(length(unique(nonoverlap_lists_3_45678$participant))) # 47 of them

# check in overlap and nonoverlap are indeed different
unique(overlap_lists_3_45678$participant) %in% unique(nonoverlap_lists_3_45678$participant)


# nonoverlap will be kept as list 3
list_3_excl <- nonoverlap_lists_3_45678

# create also df for lists 1 and 2 to be clean
lists_1_2 <- lists_123_exclusion %>% filter(list == 1 | list ==2)

# check if ppts from list 3 are contained in lists 1 and 2 (should be impossible)
unique(list_3$participant) %in% unique(lists_1_2$participant)

# check if ppts from lists 4,5,6,7 and 8 are contained in lists 1 or 2
unique(overlap_lists_3_45678$participant) %in% unique(lists_1_2$participant)

# get the people who only did lists 4,5,6,7 and 8
lists_4_5_6_7_8 <- results %>% filter(list != 1 & list != 2 & list != 3)

lists_4_5_6_7_8_excl <- subset(lists_4_5_6_7_8, !(participant %in% nonoverlap_lists_3_45678$participant))

summary(lists_4_5_6_7_8_excl)

# check if they are not anywhere else
plot(unique(lists_4_5_6_7_8_excl$participant) %in% unique(lists_1_2$participant)) # some of them are; have to exclude them

nonoverlap_45678_12 <- subset(lists_4_5_6_7_8_excl, !(participant %in% lists_1_2$participant))

# check again
plot(unique(nonoverlap_45678_12$participant) %in% unique(lists_1_2$participant)) # it's fine

# merge everything
final_results <- rbind(lists_1_2, list_3_excl, nonoverlap_45678_12)

# fix type of data
final_results <- final_results %>% mutate(across(c(participant, list, task, lexicality, frequency, relation, presentation, same_diff), as.factor))


# check if the IDs are unique values in the final_results df
plot(final_results$participant) # some morons were still sticking out; now it's fine, see steps below

summary(final_results$participant) # it's 5eaeb8b513f83a57d35db285 and 611243e1a08d9e3f1528021a

summary(final_results %>% filter(participant == "5eaeb8b513f83a57d35db285")) # this person did list 8 two times

summary(final_results %>% filter(participant == "611243e1a08d9e3f1528021a")) # this person did list 6 two times


  

# I messaged with them and have to delete them manually because I have to look at what time they did it

# check no. of ppts in each group
for (i in unique(final_results$list)){
  print(paste0("list ", i, " ", sum(length(unique(((final_results%>%filter(list==i))$participant))))))}

####################################################################
# get a subset of random 30 ppts from list 3
set.seed(1)

random_30 <- sample(unique(list_3_excl$participant),30)

list_3_30 <- list_3_excl %>% subset(participant %in% random_30)

# create the final final dataset
top_data_final <- rbind(lists_1_2, list_3_30, nonoverlap_45678_12)

# rename what should be renamed

top_data_final <- top_data_final %>% dplyr::rename(relatedness = relation)
top_data_final <- top_data_final %>% mutate(presentation = case_when(presentation == "usual" ~ "printed", presentation == "captcha" ~ "captcha"))

# fix type of data
top_data_final <- top_data_final %>% mutate(across(c(participant, list, task, lexicality, frequency, relatedness, presentation, same_diff), as.factor))


# write out the final dataset
# go up one directory
# setwd('../')
# go up one directory
# setwd('../')
# write.csv(top_data_final, paste0("top_data_final_EXCLUDED_", format(Sys.time(), "%d-%b-%Y_%H.%M.%Sh"), ".csv"), row.names = F)

# clean what I don't need
rm(overlap_lists_3_45678, results, results_1, results_2, results_3, final_results, list_3, list_3_30,
   list_3_excl, lists_1_2, lists_123, lists_123_exclusion, lists_4_5_6_7_8, lists_4_5_6_7_8_excl, lists_rest,
   messy_results_1, messy_results_2, messy_results_3, nonoverlap_45678_12, nonoverlap_lists_3_45678, i, random_30)

# Descriptives -----------------------------------------------------------


# separate into two datasets for the two tasks and get rid of unecessary columns
ldt_results <- top_data_final %>% filter(task == "ldt") %>% select(-c(rt_sd, acc_sd)) %>% dplyr::rename(rt = rt_ldt, acc = acc_ldt)

sd_results <- top_data_final %>% filter(task == "sd") %>% select(-c(rt_ldt, acc_ldt)) %>% dplyr::rename(rt = rt_sd, acc = acc_sd)


# LDT exploration ---------------------------------------------------------


# remove NAs and short RTs
rt_data_ldt <- ldt_results %>% filter(!is.na(rt) & rt>0.25)

# keep only words
rt_data_ldt <- rt_data_ldt %>% filter(lexicality == "word")

# remove incorrect responses
rt_data_ldt <- rt_data_ldt %>% filter(acc == 1)


aggregate(rt*1000 ~ relatedness + presentation + frequency, FUN = mean, rt_data_ldt)

# center variables
rt_data_ldt <- rt_data_ldt %>% mutate(relatedness_c = if_else(relatedness == "related", -0.5, 0.5))
rt_data_ldt <- rt_data_ldt %>% mutate(frequency_c = if_else(frequency == "hf", -0.5, 0.5))
rt_data_ldt <- rt_data_ldt %>% mutate(presentation_c = if_else(presentation == "captcha", -0.5, 0.5))


# model
model_rt <- lmer(log(rt) ~ relatedness_c * presentation_c * frequency_c + (1 + relatedness_c | participant) + (1 + relatedness_c | item), rt_data_ldt)
summary(model_rt)

# 
# label(rt_data_ldt$frequency) <- "Frequency"
# label(rt_data_ldt$relation) <- "Relation"
# label(rt_data_ldt$presentation) <- "Presentation"
# 
# library(table1)
# table1(~ rt | frequency + presentation, data = rt_data_ldt, caption = "LDT presentation")
# table1(~ rt | frequency + relation, data = rt_data_ldt, caption = "LDT relation")
# 
# 
# 
# dens_data_hf <- rt_data_ldt %>% filter(presentation == "usual" & frequency == "hf")
# 
# library(plyr)
# mu <- ddply(dens_data_hf, "relation", summarise, grp.mean=mean(rt))
# 
# ggplot(dens_data_hf, aes(x=rt, fill = relation)) + 
#   geom_density(alpha=0.4) +
#   geom_vline(data=mu, aes(xintercept=grp.mean, color=relation),
#              linetype="dashed") +
#   labs(title = "High frequency")
# 
# 
# dens_data_lf <- rt_data_ldt %>% filter(presentation == "usual" & frequency == "lf")
# 
# library(plyr)
# mu <- ddply(dens_data_lf, "relation", summarise, grp.mean=mean(rt))
# 
# ggplot(dens_data_lf, aes(x=rt, fill = relation)) + 
#   geom_density(alpha=0.4) +
#   geom_vline(data=mu, aes(xintercept=grp.mean, color=relation),
#              linetype="dashed") +
#   labs(title = "Low frequency")



# plot RTs
rt_data_ldt_plot <- rt_data_ldt %>% 
    group_by(relatedness, presentation, frequency) %>%
    summarise(n = n(), mean_rt = mean(rt), sd_rt = sd(rt), ci_upper = qt(0.975, df = n - 1) * sd_rt / sqrt(n), ci_lower = qt(0.025, df = n - 1) * sd_rt / sqrt(n))

freq_labs <- c("High frequency", "Low frequency")
names(freq_labs) <- c("hf", "lf")


ggplot(rt_data_ldt_plot, aes(x = factor(relatedness), y = mean_rt*1000)) +
  geom_point(aes(group = presentation, color = factor(presentation)), size = 4, position = position_dodge(0.3)) +
  geom_line(aes(group=presentation, color = factor(presentation)), size = 0.9, position = position_dodge(0.3)) +
  facet_grid(.~frequency, labeller = labeller(frequency = freq_labs)) +
  geom_errorbar(aes(ymin = mean_rt*1000 + ci_lower*1000, ymax = mean_rt*1000 + ci_upper*1000, alpha = 0.3, color = factor(presentation)), width = 0.1, size = 0.9, position = position_dodge(0.3)) +
  theme_light()+
  theme(legend.position = "right",  strip.text.x = element_text(size = 18), legend.text = element_text(size=15),legend.title = element_text(size=15), axis.text=element_text(size=18), axis.title=element_text(size=18), axis.title.x=element_blank(),
        plot.title=element_text(size=20,face="bold"), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  guides(alpha = "none") +
  labs(y="Mean RT(ms)") +
  scale_y_continuous(limits = c(500, 700), breaks = seq(500, 700, 20)) +
  scale_colour_manual(values = c("#bc5090","#003f5c"), name="Prime presentation", labels = c("Captcha", "Printed")) +
  scale_x_discrete(labels = c("Related", "Unrelated"), position = "bottom") +
  labs(title = "Lexical decision task - Reading times")

# save the plot to the current directory
# ggsave(filename = "top_ldt_rt.png", width = 250, height = 200, units = "mm")

# plot accuracy

acc_data_ldt <- ldt_results %>% filter(lexicality == "word")

acc_data_ldt_plot <- acc_data_ldt %>%
  group_by(relatedness, presentation, frequency) %>%
  summarise(n = n(), mean_acc= mean(acc), sd_acc = sd(acc), ci_upper = qt(0.975, df = n - 1) * sd_acc / sqrt(n), ci_lower = qt(0.025, df = n - 1) * sd_acc / sqrt(n))

freq_labs <- c("High frequency", "Low frequency")
names(freq_labs) <- c("hf", "lf")


ggplot(acc_data_ldt_plot, aes(x = factor(relatedness), y = mean_acc)) +
  geom_point(aes(group = presentation, color = factor(presentation)), size = 4, position = position_dodge(0.3)) +
  geom_line(aes(group=presentation, color = factor(presentation)), size = 0.9, position = position_dodge(0.3)) +
  facet_grid(.~frequency, labeller = labeller(frequency = freq_labs)) +
  geom_errorbar(aes(ymin = mean_acc + ci_lower, ymax = mean_acc + ci_upper, alpha = 0.3, color = factor(presentation)), width = 0.1, size = 0.9, position = position_dodge(0.3)) +
  theme_light()+
  theme(legend.position = "right",  strip.text.x = element_text(size = 18), legend.text = element_text(size=15),legend.title = element_text(size=15), axis.text=element_text(size=18), axis.title=element_text(size=18), axis.title.x=element_blank(),
        plot.title=element_text(size=20,face="bold"), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  guides(alpha = "none") +
  labs(y="Accuracy") +
  scale_y_continuous(limits = c(0.8, 1), breaks = seq(0.8, 1, 0.05)) +
  scale_colour_manual(values = c("#bc5090","#003f5c"), name="Prime presentation", labels = c("Captcha", "Printed")) +
  scale_x_discrete(labels = c("Related", "Unrelated"), position = "bottom") +
  labs(title = "Lexical decision task - Accuracy")

# save the plot to the current directory
# ggsave(filename = "top_ldt_acc.png", width = 250, height = 200, units = "mm")

# SD explorations ---------------------------------------------------------


# SD - exclude different trials and nonwords
aggregate(rt*1000 ~ relatedness + presentation + frequency, FUN = mean, sd_results %>% filter(same_diff == "same"))

# remove NAs and short RTs
rt_data_sd <- sd_results %>% filter(!is.na(rt) & rt>0.25)

# remove incorrect responses
rt_data_sd <- sd_results %>% filter(acc == 1)

label(sd_results$frequency) <- "Frequency"
label(sd_results$relatedness) <- "Relatedness"
label(sd_results$presentation) <- "Presentation"

table1(~ rt | frequency + presentation, data = sd_results %>% filter(lexicality=="word" & same_diff == "same"), caption = "SD presentation")
table1(~ rt | frequency + relatedness, data = sd_results %>% filter(lexicality=="word" & same_diff == "same"), caption = "SD relation")



# plot RTs
rt_data_sd_plot <- rt_data_sd %>% filter(lexicality == "word" & same_diff == "same") %>% 
  group_by(relatedness, presentation, frequency) %>%
  summarise(n = n(), mean_rt = mean(rt), sd_rt = sd(rt), ci_upper = qt(0.975, df = n - 1) * sd_rt / sqrt(n), ci_lower = qt(0.025, df = n - 1) * sd_rt / sqrt(n))


ggplot(rt_data_sd_plot, aes(x = factor(relatedness), y = mean_rt*1000)) +
  geom_point(aes(group = presentation, color = factor(presentation)), size = 4, position = position_dodge(0.3)) +
  geom_line(aes(group=presentation, color = factor(presentation)), size = 0.9, position = position_dodge(0.3)) +
  facet_grid(.~frequency, labeller = labeller(frequency = freq_labs)) +
  geom_errorbar(aes(ymin = mean_rt*1000 + ci_lower*1000, ymax = mean_rt*1000 + ci_upper*1000, alpha = 0.3, color = factor(presentation)), width = 0.1, size = 0.9, position = position_dodge(0.3)) +
  theme_light()+
  theme(legend.position = "right",  strip.text.x = element_text(size = 18), legend.text = element_text(size=15),legend.title = element_text(size=15), axis.text=element_text(size=18), axis.title=element_text(size=18), axis.title.x=element_blank(),
        plot.title=element_text(size=20,face="bold"), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  guides(alpha = "none") +
  labs(y="Mean RT(ms)") +
  scale_y_continuous(limits = c(420, 560), breaks = seq(420, 560, 20)) +
  scale_colour_manual(values = c("#bc5090","#003f5c"), name="Prime presentation", labels = c("Captcha", "Printed")) +
  scale_x_discrete(labels = c("Related", "Unrelated"), position = "bottom") +
  labs(title = "Same-different task - Reading times")


# save the plot to the current directory
# ggsave(filename = "top_sd_rt.png", width = 250, height = 200, units = "mm")


# plot accuracyu
acc_data_sd_plot <- sd_results %>% filter(lexicality == "word" & same_diff == "same") %>% 
  group_by(relatedness, presentation, frequency) %>%
  summarise(n = n(), mean_acc = mean(acc), sd_acc = sd(acc), ci_upper = qt(0.975, df = n - 1) * sd_acc / sqrt(n), ci_lower = qt(0.025, df = n - 1) * sd_acc / sqrt(n))


ggplot(acc_data_sd_plot, aes(x = factor(relatedness), y = mean_acc)) +
  geom_point(aes(group = presentation, color = factor(presentation)), size = 4, position = position_dodge(0.3)) +
  geom_line(aes(group=presentation, color = factor(presentation)), size = 0.9, position = position_dodge(0.3)) +
  facet_grid(.~frequency, labeller = labeller(frequency = freq_labs)) +
  geom_errorbar(aes(ymin = mean_acc + ci_lower, ymax = mean_acc + ci_upper, alpha = 0.3, color = factor(presentation)), width = 0.1, size = 0.9, position = position_dodge(0.3)) +
  theme_light()+
  theme(legend.position = "right",  strip.text.x = element_text(size = 18), legend.text = element_text(size=15),legend.title = element_text(size=15), axis.text=element_text(size=18), axis.title=element_text(size=18), axis.title.x=element_blank(),
        plot.title=element_text(size=20,face="bold"), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  guides(alpha = "none") +
  labs(y="Accuracy") +
  scale_y_continuous(limits = c(0.8, 1), breaks = seq(0.8, 1, 0.05)) +
  scale_colour_manual(values = c("#bc5090","#003f5c"), name="Prime presentation", labels = c("Captcha", "Printed")) +
  scale_x_discrete(labels = c("Related", "Unrelated"), position = "bottom") +
  labs(title = "Same-different task - Accuracy")

# save the plot to the current directory
ggsave(filename = "top_sd_acc.png", width = 250, height = 200, units = "mm")



dens_data_sd_lf <- rt_data_sd %>% filter(lexicality=="word" & same_diff == "same" & presentation == "usual" & frequency == "lf")

library(plyr)
mu <- ddply(dens_data_sd_lf, "relation", summarise, grp.mean=mean(rt))

ggplot(dens_data_sd_lf, aes(x=rt, fill = relation)) + 
  geom_density(alpha=0.4) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=relation),
             linetype="dashed") +
  labs(title = "Low frequency")


dens_data_sd_hf <- rt_data_sd %>% filter(lexicality=="word" & same_diff == "same" & presentation == "usual" & frequency == "hf")

library(plyr)
mu <- ddply(dens_data_sd_hf, "relation", summarise, grp.mean=mean(rt))

ggplot(dens_data_sd_hf, aes(x=rt, fill = relation)) + 
  geom_density(alpha=0.4) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=relation),
             linetype="dashed") +
  labs(title = "High frequency")






# Modeling ----------------------------------------------------------------


# Power simulation --------------------------------------------------------
library(simr)


par(mfrow=c(1,4))
hist(ldt_results$rt, 20)
hist(ldt_results$rt[ldt_results$rt])
hist(log(ldt_results$rt))
hist(1/ldt_results$rt)

model_ldt <- lmer(1000/rt ~ relation*frequency*presentation + (1|participant) + (1|item), ldt_results)
summary(model_ldt)

fixed <- c(1908.262165, -116.881490, -152.884311, -247.109627, 18.907019, 32.112599, 59.287870, 6.055024, 5.303282, 4.692809, -3.359920, -30.602250)

## Random intercepts for participants clustered by class
rand <- list(0.5, 0.1)

fixef(model_ldt)
ranef(model_ldt)


# effect of relation (without frequency and presentation) - I exclude RELATION from the model

sim_treat <- powerSim(model_ext_participant, nsim=10, test = fcompare(rt ~ frequency*presentation))

model_ext_participant <- extend(model_ldt, along="participant", n=20)




################
################

# check accuracy exclusion criteria

sd_results$acc <- as.numeric(sd_results$acc)

# Calculate mean accuracy per participant
mean_accuracy_per_participant <- aggregate(acc ~ participant, data = sd_results, FUN = mean)

# Check which participants have mean accuracy lower than 0.80
participants_below_80_mean_accuracy <- mean_accuracy_per_participant[mean_accuracy_per_participant$acc < 0.80, ]

# Show the participants with mean accuracy below 0.80
print(participants_below_80_mean_accuracy)





