
###############################################
# Georgina Haines-Woodhouse - Exercise script #
###############################################

# Data and script setup ####

rm(list = ls())

library(tidyverse)
library(data.table)

masterdata <- fread("C:/Users/georginahw/Documents/Data Handling/data_example/data.csv")
mydata <- masterdata

variable_names <- data.table(names(mydata))
head(mydata)

# Question 1 - How many samples have failed the contamination check and have contamination of over 5.0%? ####

q1 <- mydata %>%
  filter(confindr.contam_status.check_result == 'FAILURE' & confindr.percentage_contamination.metric_value > 5.0) %>%
  count()

# There are 57 samples that have failed the contamination check and have contamination of over 5.0%

# Question 2 - How many samples are there that have less than or equal to 50 contigs and a N50 value of greater than or equal to 750,000? ####

q2 <- mydata %>%
  filter(.[[7]] <= 50 & quast.N50.metric_value >= 750000) %>%
  count()

# There are 49 samples with less than or equal to 50 contigs and a N50 value of greater than or equal to 750,000 

# Question 3 - Select all numeric columns and rename them to remove the .quast prefix and the .metric_value suffix and rename confindr.percentage_contamination to contamination_percent ####  

names(mydata) <- gsub("quast.", "", names(mydata))
names(mydata) <- gsub(".metric_value", "", names(mydata))

colnames(mydata)[colnames(mydata) == 'confindr.percentage_contamination'] <- 'contamination_percent'

# Question 4 - Make a box plot of Total length (>= 1000bp) ####

total_length <- data.table(mydata$`Total length (>= 1000 bp)`)

q4_plot <- ggplot(total_length, aes(x = factor(0),y = V1)) +
  geom_boxplot()+
  scale_y_continuous()+
  labs(x = NULL, y = "Total length (>=1000bp)", title = 'Question 4 - Boxplot of Total Length (>=1000bp)')
  
print(q4_plot)

# Question 5 - Pivot the data to tidy it with one observation per row and have final column headings of sample name, metric, value ####

names(mydata) <- gsub("#", "", names(mydata))

q5_tidy_data <- mydata %>%
  select(1,3,5,7,9,11) %>%
  rename(contamination_status = 2, percentage_contamination = 3, contigs = 4, N50 = 5, total_length = 6) %>%
  pivot_longer(!sample_name,names_to = "metric", values_to = "value")

write.csv(q5_tidy_data,'C:/Users/georginahw/Documents/Data Handling/data_example/q5_tidy_data.csv')

# Question 6 - Make a violin plot for each of the numeric values in a single plot and include jittered data points ####

q6_data <-q5_tidy_data %>%
  filter(metric != 'contamination_status')

# Set up violin plot

q6_data$metric <- as.factor(q6_data$metric)

q6_data_plot <- ggplot(q6_data, aes(x = metric, y = log(value), fill = metric))+
  geom_point(aes(y = log(value))) +
  geom_jitter(alpha = 0.1) +
  geom_violin() +
  theme_classic()


print(q6_data_plot)











