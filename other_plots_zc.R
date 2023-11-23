################Plots###################


#circular plot
# 
# library(tidyverse)
# 
# # Create dataset
# data <- data.frame(
#   individual=paste( " ", seq(1,47), sep=""),
#   Designs=c( rep('Mathematical models', 24), rep('Case series', 13), rep('Cross-sectional', 8), rep('Cohorts', 2)) ,
#   value=sample( seq(10,100), 47, replace=T)
# )
# 
# 
# # Set a number of 'empty bar' to add at the end of each group
# empty_bar <- 4
# to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$Designs), ncol(data)) )
# colnames(to_add) <- colnames(data)
# to_add$Designs <- rep(levels(data$Designs), each=empty_bar)
# data <- rbind(data, to_add)
# data <- data %>% arrange(Designs)
# data$id <- seq(1, nrow(data))
# 
# # Get the name and the y position of each label
# label_data <- data
# number_of_bar <- nrow(label_data)
# angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
# label_data$hjust <- ifelse( angle < -90, 1, 0)
# label_data$angle <- ifelse(angle < -90, angle+180, angle)
# 
# # Make the plot
# A <- ggplot(data, aes(x=as.factor(id), y=value, fill=Designs)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
#   geom_bar(stat="identity", alpha=0.5) +
#   ylim(-100,120) +
#   theme_void() +
#   theme(
#     axis.text = element_blank(),
#     axis.title = element_blank(),
#     panel.grid = element_blank(),
#     plot.margin = unit(rep(-1,4), "cm") 
#   ) +
#   coord_polar() + 
#   geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE )



### lolipop plot (types of studies)

rm(list =ls())

studies <- data.frame(Design=c("Case series","Cohorts","Cross-sectional", 
                               "Mathematical models") , 
                      Frequency=c(13,2,8,24))  
library("ggplot2")  

# Create lollipop plot with reordered data 
A <- ggplot(studies, aes(x=reorder(Design,Frequency),y=Frequency)) + 
  geom_point(size = 3, colour = "black") +  
  geom_segment(aes(xend = Design, yend = 0), size = 2) + 
  geom_segment( aes(x=Design, xend=Design, y=0, yend=Frequency),  
                color="darkgray", size=3) + 
  geom_point( color="orange", size=10) + theme_light() + coord_flip()  +
  labs(x = "Study designs", 
       y = "Absolute frequency",
  )



### Waffle plot  (parameters)

library(waffle)

B <- waffle(
  c('Incubation period' = 26, 'Case fatality rate' = 24,  'Basic Reproductive number' = 9,'Effective reproductive number' = 8, 
    'Serial interval' = 5, 'Generation time' = 2,
    'Infectious period' = 1), 
  rows = 10, 
  size = 2,
  flip=TRUE, glyph_size = 12, xlab="1 square = 1 reference") 

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
  scale_fill_viridis_c(breaks = 1:10, direction = -1,   
                       option = "D", na.value = "grey90") +
  theme_minimal(10) +
  # scale_fill_gradient(low = "light blue", high =  "dark blue", na.value = "grey50")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        rect = element_blank()) 
map1

### figure 2

figure2 <- cowplot::plot_grid(A, map1, B, nrow = 2, labels= "AUTO");figure2 


