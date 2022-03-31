# Guiding principles:
## Document everything
## Format into a human-readable framework
## Consider the communication needs of all humans

# Goal: plot the effect of diet on chick weight gain
# using the chickweight dataset from the 
# built-in datasets package

library(tidyverse)

Chick_Weight_Avg <- ChickWeight %>% # Take ChickWeight data from datasets package
                    group_by(Diet, Time) %>% # Group by diet and time
                    mutate(Mean_Weight = mean(weight)) %>% # Average weight among all chicks
                    select(Diet, Time, Mean_Weight) %>% # Select mean weight, diet, and time columns
                    distinct() %>% # Keep only unique rows
                    as.data.frame() # Extract data as an ungrouped dataframe

# Plot chick weight
# Make color-blind safe palette

cb_unsafe <- c("#FFBE0B", "#FB5607","#FF006E", "#8338EC")

cb_safe <- c("#03045E", "#0077B6","#00B4D8", "#90E0EF")

# Plot w/ color-blind friendly colors and large text

ggplot(Chick_Weight_Avg) +
  geom_smooth(aes(x = Time, y = Mean_Weight, color = Diet), size = 2) +
  scale_color_manual(values = cb_safe) +
  labs(x = "Time (days)", y = "Weight (grams)") +
  theme_classic() +
  theme(axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 32, color = "black"))

