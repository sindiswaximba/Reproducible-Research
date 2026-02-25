## Load libraries
library(tidyverse)

## Read data
birds <- read.csv("data/BeachBirds.csv")

## Inspect
head(birds)
str(birds)
### WHY: Checking the structure ensures variables are read correctly and helps identify issues early.

## Convert categorical variables into factors
birds$Site <- as.factor(birds$Site)
birds$Species <- as.factor(birds$Species)
birds$Sex <- as.factor(birds$Sex)
### WHY: Factors tell R these are categorical variables, which improves summaries and plotting.

## Check for missing values
sum(is.na(birds))
### WHY: Missing data can affect calculations like means and plots, so we check if any values are missing.

## Calculate summary statistics for flush distance by species
summary_data <- birds %>%
  group_by(Species) %>%
  summarise(
    mean_flush = mean(flush.dist, na.rm = TRUE),
    sd_flush = sd(flush.dist, na.rm = TRUE),
    sample_size = n()
  )

## View the summary table
print(summary_data)
### WHY: Provides a numerical comparison among species for flush distance.

## Save the summary table to a CSV file
write.csv(summary_data, "summary_flush_distance.csv", row.names = FALSE)
### WHY: Saving the table ensures reproducibility and allows others to use the processed data without rerunning the code.

## Create a boxplot of flush distance by species
plot1 <- ggplot(birds, aes(x = Species, y = flush.dist, fill = Species)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    x = "Species",
    y = "Flush Distance (m)",
    fill = "Species"
  )
plot1

### Figure 1: Flush Distance by Bird Species

### WHY: Compares variability and differences in escape behaviour between species.

## Save the plot
ggsave("flush_distance_plot.png", plot = plot1, width = 6, height = 4)
### WHY: Saving the figure ensures reproducibility and inclusion in reports.

## Compare flush distance between males and females
sex_summary <- birds %>%
  group_by(Sex) %>%
  summarise(
    mean_flush = mean(flush.dist, na.rm = TRUE),
    sd_flush = sd(flush.dist, na.rm = TRUE),
    n = n()
  )

print(sex_summary)
### WHY: This explores whether flush distance differs between males and females.

## Create a boxplot of flush distance by sex
plot2 <- ggplot(birds, aes(x = Sex, y = flush.dist, fill = Sex)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    x = "Sex",
    y = "Flush Distance (m)"
  )
plot2

### Figure 2: Flush Distance by Sex

### WHY: Compares variability and differences in escape behaviour between sexes.
  
## Save the plot
ggsave("flush_distance_by_sex.png", plot = plot2, width = 6, height = 4)
  
## Create a boxplot of flush distance by site
plot3 <- ggplot(birds, aes(x = Site, y = flush.dist, fill = Site)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    x = "Site",
    y = "Flush Distance (m)",
    fill = "Site"
  )
plot3

### Figure 3: Flush Distance Across Sites
### WHY: Compares escape behaviour across sampling locations to see whether disturbance varies among sites.

## Save the figure
ggsave("flush_distance_by_site.png", plot = plot3, width = 6, height = 4)
