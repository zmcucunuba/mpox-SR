
rm(list =ls())

studies <- data.frame(Design=c("Case series","Cohorts","Cross-sectional", 
                               "Mathematical models") , 
                      Frequency=c(13,2,8,24))  
library("ggplot2")  

# Create lollipop plot with reordered data 
A <- 
ggplot(studies, 
       aes(x=reorder(Design,Frequency),y=Frequency)) + 
  geom_point(size = 2, colour = "black") +  
  geom_segment(aes(xend = Design, yend = 0), size = 1.5) + 
  geom_segment( aes(x=Design, 
                    xend=Design, y=0, yend=Frequency),  
                color="blue", size=1.5) + 
  geom_point( color="orange", size=5) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Study designs", 
       y = "Number of studies",
  ) 



### Waffle plot  (parameters)

library(waffle)

B <- waffle(
  c('Incubation period' = 26, 'Case fatality rate' = 24,  
    'Basic reproduction number' = 9,
    'Effective reproduction number' = 8, 
    'Serial interval' = 5, 'Generation time' = 2,
    'Infectious period' = 1), 
  rows = 10, 
  size = 2,
  flip=TRUE, 
  glyph_size = 30, xlab="1 square = 1 reference") +
  coord_flip() +
  theme_bw(12) +
  theme(legend.position = "bottom", 
        legend.spacing.y = unit(2, 'mm'),
        legend
        legend.direction = "horizontal") +
  guides(fill=guide_legend(nrow=4,byrow=FALSE)) +
  theme(legend.key.height= unit(2, 'mm'),
        legend.key.width= unit(4, 'mm')) +
  theme(axis.text = element_blank(), axis.ticks = element_blank())


cowplot::plot_grid(A, B, 
                   rel_widths = c(0.3, 0.8), 
                   rel_heights = c(1, 1))

###################



## map 

library(ggplot2)             
library(tidyverse)   
library(dplyr)
library(readxl)

bd <- read_excel("data/bd.xlsx", sheet = "map")

mapdata <- map_data("world") ##ggplot2
# View(mapdata)


mapdata <- left_join(mapdata, bd, by="region")
mapdata <- mapdata %>% filter(region != "Antarctica")

# map1 <- 
ggplot(mapdata, aes( x = long, y = lat, group=group)) +
  geom_polygon(aes(fill = References), color = "black", size = 0.1) + 
  scale_fill_viridis_c(breaks = c(2, 4, 6, 8, 10), direction = -1,   
                       option = "D", na.value = "grey90") +
  theme_bw(12) +
  # scale_fill_gradient(low = "light blue", high =  "dark blue", na.value = "grey50")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank()) 


### figure 2



