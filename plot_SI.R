#plot r0


library(tidyverse)
library(readxl)

bd <- read_excel("data/bd.xlsx", sheet = "R0")
bd$ref <- paste0(bd$author, " (", bd$year, ")")


ggplot(data = bd) +
  geom_errorbar(aes(x = ref, 
                      ymin = mean_uncertainty_lower_value, 
                      ymax = mean_uncertainty_upper_value), width = 0.2)  +
  geom_point(aes(y = mean, x = ref)) +
  coord_flip() +
  theme_bw() + 
  labs(y= "Basic reproduction number", x = "")



# plor rt


library(tidyverse)
library(readxl)

bd <- read_excel("data/bd.xlsx", sheet = "Rt")
bd$ref <- paste0(bd$author, " (", bd$year, ")")


ggplot(data = bd) +
  geom_errorbar(aes(x = ref, 
                    ymin = mean_uncertainty_lower_value, 
                    ymax = mean_uncertainty_upper_value), width = 0.2)  +
  geom_point(aes(y = mean, x = ref)) +
  coord_flip() +
  theme_bw() + 
  labs(y= "Effective reproduction number", x = "")