# Load necessary libraries
rm(list = ls())
dev.off()

library(ggplot2)
library(dplyr)
library(readxl)
library(ggthemes)

# Data frame with studies and their respective data points
df <- read_excel("data/mpox_meta_results.xlsx")
df <- df %>% filter(Parameter == "Serial Interval",
                    !Study == "Heterogeneity")
df$Study[df$Study=="Random effects model"] <- "RE Model"
df_nomodel <- df %>% filter(!Study == "RE Model")
df$StudyLevels <- factor(df$Study,
                         levels = c("RE Model", df_nomodel$Study))
effect_model <- df %>% filter(Study == "RE Model")

# Function to create diamond shape
create_diamond <- function(center_x, center_y, width, height) {
  half_width <- width / 2
  half_height <- height / 2
  return(data.frame(
    x = c(center_x, center_x + half_width, center_x, center_x - half_width, center_x),
    y = c(center_y + half_height, center_y, center_y - half_height, center_y, center_y + half_height)
  ))
}

diamond_data <- create_diamond(center_x = 1, 
                               center_y = effect_model$Mean, 
                               width = 0.5, 
                               height = 5)

# Your existing ggplot code

ggplot(df, aes(x = StudyLevels, y = Mean)) +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.01, color = "black", size = 0.3) + # Confidence interval error bars
  geom_hline(yintercept = effect_model$Mean, linetype = "dashed", color = "darkblue") + # Vertical line at x = 1
  geom_point(aes(size = Weight), fill = "grey", pch = 22, colour = "black") + # Use color to differentiate points
  geom_point(colour = "black", size = 0.1) + # Use color to differentiate points
  # geom_errorbar(data = effect_model, aes(ymin = LowerCI, ymax = UpperCI), 
  #               width = 0, size = 2, color = "blue") + # Confidence interval error bars
  geom_polygon(data = diamond_data, aes(x = x, y = y), fill = "darkblue") + # Add the filled diamond shape
  geom_text(data = effect_model, 
            aes(label = "8.25 [6.45;10.55]"),
            hjust = 0, vjust = 1, nudge_x = 0.5, nudge_y = 3, 
            size = 4, colour = "darkblue") + # Label for the summary effect
  cowplot::theme_minimal_grid() +
  scale_y_continuous(breaks = seq(0, 20, by = 2)) +
  coord_flip() + # Flip coordinates for the forest plot
  # theme(legend.position = "none", # No legend required
  #       axis.text.x = element_text(angle = 0, hjust = 1), # Rotate x-axis text
  #       axis.title.x = element_blank(), # No axis title
  #       plot.title = element_text(hjust = 0.5)) + # Center the plot title
  labs(title = "Serial Interval", x = "", y = "Days") + # Title of the plot 
  theme_bw()+
  theme(legend.position = c(0.8, 0.5), legend.key.size = unit(1, 'mm'), 
        legend.text = element_text(size=8), legend.title = element_text(size=8))  +
  theme(legend.position = "none")






