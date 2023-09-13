
# WAFFLE PLOT -----

# 01.Instalacion de librerias -----

install.packages("waffle")
install.packages("patchwork")

# 02.Cargando Librerias -----

library(waffle)
library(patchwork)


g1 <- waffle(
  c('Mathematical model' = 19, 'Other' = 26), 
  rows = 9, 
  colors = c("#79CDCD", "#DCDCDC"),
  title = 'Mathematical models',
  size = 0.5,
  flip=TRUE
  ) +
  scale_x_reverse()+
  annotate("text", x = 5, y = 1.5, label = "42.2%",size=8)+
  theme(legend.position = 'none',
        plot.title = element_text(face = 'bold',hjust = 0.5))

g2 <- waffle(
  c('Case series' = 4, 'Other' = 41), 
  rows = 9, 
  colors = c("#CD96CD", "#DCDCDC"),
  title = 'Case series',
  size = 0.5,
  flip=TRUE
) +
  scale_x_reverse()+
  annotate("text", x = 2.5, y = 1.0, label = "8.8%",size=8)+
  theme(legend.position = 'none',
        plot.title = element_text(face = 'bold',hjust = 0.5))


g3 <- waffle(
  c('Cross-sectional/surveillance studies' = 8, 'Other' = 37), 
  rows = 9, 
  colors = c("#8570FF8D", "#DCDCDC"),
  title = 'Cross-sectional/surveillance studies',
  size = 0.5,
  flip=TRUE
) +
  scale_x_reverse()+
  annotate("text", x = 5.0, y = 1.0, label = "17.7%",size=8)+
  theme(legend.position = 'none',
        plot.title = element_text(face = 'bold',hjust = 0.5))

g4 <- waffle(
  c('Cohort studies' = 14, 'Other' = 31), 
  rows = 9, 
  colors = c("#D4B69BC2", "#DCDCDC"),
  title = 'Cohort studies',
  size = 0.5,
  flip=TRUE
) +
  scale_x_reverse()+
  annotate("text", x = 5, y = 1.0, label = "31.1%",size=8)+
  theme(legend.position = 'none',
        plot.title = element_text(face = 'bold',hjust = 0.5))


g1+g2+g3+g4

