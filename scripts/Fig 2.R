
rm(list =ls())
library(ggplot2)  
library(cowplot)
library(tidyverse)   
library(dplyr)
library(readxl)
library(waffle)

size_text <- 35



studies <- data.frame(Design=c("Case series","Cohorts","Cross-sectional", 
                               "Mathematical models") , 
                      Frequency=c(13,2,8,24))  


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
  theme_bw(size_text) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Study designs", 
       y = "Number of studies",
  ) 



### Waffle plot  (parameters)


my_palette <- c("#66c2a5", "#9970ab", "orange", 
                "#fb9a99", "#2b8cbe", "#f0027f",
                "#bf5b17", "white")

B <- waffle(
  c('Incubation period' = 26, 
    'Case fatality rate' = 24,  
    'Basic reproduction number' = 9,
    'Effective reproduction number' = 8, 
    'Serial interval' = 5, 
    'Generation time' = 2,
    'Infectious period' = 1), 
  rows = 10, 
  size = 2,
  flip=TRUE, 
  glyph_size = 30, xlab="1 square = 1 reference") +
  coord_flip() +
  theme_bw(size_text) +
  theme(legend.position = "bottom", 
        legend.spacing.y = unit(2, 'mm'),
        legend.direction = "horizontal") +
  guides(fill=guide_legend(nrow=4,byrow=FALSE)) +
  theme(legend.key.height= unit(2, 'mm'),
        legend.key.width= unit(4, 'mm')) +
  # scale_fill_brewer(palette = "Set1", na.value = "grey90") +
  # scale_fill_tron() +
  # scale_fill_brewer(palette = "Set1") +
  scale_fill_manual (values = my_palette) +
  labs(fill ="") +
  theme(axis.text = element_blank(), axis.ticks = element_blank())



bd <- read_excel("data/map_and_qualityassesment.xlsx", sheet = "map")
mapdata <- map_data("world")
mapdata <- left_join(mapdata, bd, by="region")
mapdata <- mapdata %>% filter(region != "Antarctica")

C <- 
ggplot(mapdata, aes( x = long, y = lat, group=group)) +
  geom_polygon(aes(fill = References), color = "black", size = 0.1) + 
  scale_fill_viridis_c(breaks = c(2, 4, 6, 8, 10), direction = -1,   
                       option = "D", na.value = "grey90") +
  theme_bw(size_text) +
  # scale_fill_gradient(low = "light blue", high =  "dark blue", na.value = "grey50")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank()) 

p1 <- cowplot::plot_grid(A, NULL, B, nrow = 1,
                   rel_widths = c(0.3, 0.1, 0.8), 
                   rel_heights = c(1, 1), 
                   labels= c("B", "C"), label_size = size_text * 0.7)


png(filename = "figures/Fig 2.png",
    width = 480*4, height = 480*3, units = "px", 
    pointsize = 12,
    bg = "white")

cowplot::plot_grid(C, p1, nrow = 2, labels = c("A", "", ""), 
                   label_size = size_text * 0.7)

dev.off()


### figure 2



