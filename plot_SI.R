

library(tidyverse)
library(readxl)

bd <- read_excel("data/bd.xlsx", sheet = "serial_interval")
bd$ref <- paste0(bd$author, " (", bd$year, ")")
bd$distribution[is.na(bd$distribution)] <- "No information"
bd$distribution <- factor(bd$distribution, levels = c("gamma",
                                                      "normal",
                                                      "No information") )


ggplot(data = bd) +
  geom_errorbar(aes(x = ref, 
                      ymin = mean_uncertainty_lower_value, 
                      ymax = mean_uncertainty_upper_value), width = 0.1)  +
  geom_point(aes(y = mean, x = ref, pch = distribution, colour = distribution), 
             fill = "red", size = 3) +
  coord_flip() +
  theme_minimal() + 
  labs(x= "", y = "Mean", colour = "Probability\nDistribution", 
       pch = "Probability\nDistribution")
  

