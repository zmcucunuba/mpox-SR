rm(list=ls())

library(meta)

#serial interval

study <- c("Guo (2022)", "Madewell (2022)", "Miura (2022)", "Ward (2023)", "UKHSA (2022)")
n <- c(21,57,34,79,34) 
mean <- c(5.6,8.5,10.1,9.5,9.8)
sd <- c(1.5,5,6.1,10.9,19.9)
df <- data.frame(study,n,mean,sd)


m1 <- metamean(n = n,
               mean = mean,
               sd = sd,
               studlab = study,
               data = df,
               sm = 'MLN',
               random = TRUE,
               common = F,
               warn = F,
               prediction = F,
               title = "Serial interval")


png(file='plot_SI.png', width = 700, height = 250)
forest(m1, col.diamond = "blue", col.diamond.lines = "black", xlab = "Serial interval")
dev.off()


#incubation period 
study2 <- c("Angelo (2023)","Català (2022)","Charniga (2022)","Choudury (2022)", "Cobos (2023)", "Gaspari (2022)", "Gomez-Garberi (2022)", 
            "Guzzeta (2022)", "Kroger (2023)", "McFarland (2023)", "Madawell (2022)", "Mailhe (2023)","Maldonado (2022)", "Miura (2022)", "Miura (2023)", 
            "Moschese (2023)","Nunez (2023)", "O Laughlin (2022)", "Suarez Rodriguez (2022)","Tarin-Vicente (2022)", "Thornhill (2022)", "Thornhill (2022)", 
            "Ward (2023)", "Wei (2022)", "UKHSA (1)","UKHSA (2)")

n2 <- c(226,77,22,179,19,30,14,30,209,122,35,112,205,18,36,32,18,230,45,181,23,51,54,77,33,33) 


mean2 <- c(8,6,7.6,7,8,9,13,9.1,8.3,8,5.6,6,7,9.1,8.1,11,8,7,7,7,7,7,7.8,8.23,9.2,9.2)


sd2 <- c(8.5,7,1.8,8,7.5,14,29.25,9.275, 5.2,19,4.4,7.25,12.5,9.425,4.4,10.5,7,7,18.75,19.25,19.25,23.75,7.55,20.175,19.75,13.225)


df2 <- data.frame(study2,n2,mean2,sd2)

m2 <- metamean(n = n2,
               mean = mean2,
               sd = sd2,
               studlab = study2,
               data = df2,
               sm = 'MLN',
               random = TRUE,
               common = F,
               warn = F,
               prediction = F,
               title = "Incubation period")

png(file='plot_IP.png', width = 1100, height = 540)
forest(m2, col.diamond = "blue", col.diamond.lines = "black", xlab = "Incubation period")
dev.off() 


#generation time

study5 <- c("Guzzeta (2022)","Wei (2022)")

n5 <- c(16,77) 

mean5 <- c(12.5,10.38)

sd5 <- c(15.42,15.3)

df5 <- data.frame(study5,n5,mean5,sd5)

m5 <- metamean(n = n5,
               mean = mean5,
               sd = sd5,
               studlab = study5,
               data = df5,
               sm = 'MLN',
               random = TRUE,
               common = F,
               warn = F,
               prediction = F,
               title = "Generation time")

png(file='plot_GT.png', width = 700, height = 200)
forest(m5, col.diamond = "blue", col.diamond.lines = "black", xlab = "Generation time")
dev.off()

###########CFR 

library(meta)
library(readxl)

CFR <- read_excel("data/bd.xlsx", sheet = "CFR")
CFR$ref <- paste0(CFR$authors, " (", CFR$year, ")")

CFR_m <- metaprop(data=CFR,
                  n=total,
                  event = event,
                  studlab = ref,
                  sm="PLOGIT",
                  method = "GLMM",
                  fixed = F,
                  random = T)

summary(CFR_m)

png(file='plot_CFR.png', width = 700, height = 400)
forest(CFR_m, digits = 4L, col.diamond = "blue", col.diamond.lines = "black", xlab = "Case fatality rate")
dev.off()


library(png)
library(grid)
library(gridExtra)

plot_SI <- readPNG('plot_SI.png')
plot_IP <- readPNG('plot_IP.png')
plot_GT <- readPNG('plot_GT.png')
plot_CFR <- readPNG('plot_CFR.png')

grid.arrange(ncol = 1,
             rasterGrob(plot_IP),
             rasterGrob(plot_SI),
             rasterGrob(plot_GT),
             rasterGrob(plot_CFR) 
)

###########Subgroup analysis#############



#incubation (only mathematical models)

study3 <- c("Charniga (2022)","Guzzeta (2022)", "McFarland (2023)", "Madawell (2022)",  "Miura (2022)", "Miura (2023)", 
            "Ward (2023)", "Wei (2022)", "UKHSA (1)","UKHSA (2)")

n3 <- c(22,30,122,35,18,36,54,77,33,33) 


mean3 <- c(7.6,9.1,8,5.6,9.1,8.1,7.8,8.23,9.2,9.2)


sd3 <- c(1.8,9.275,19,4.4,9.42,4.4,7.55,20.17,19.7,13.2)


df3 <- data.frame(study3,n3,mean3,sd3)

m3 <- metamean(n = n3,
               mean = mean3,
               sd = sd3,
               studlab = study3,
               data = df3,
               sm = 'MLN',
               random = TRUE,
               common = F,
               warn = F,
               prediction = F,
               title = "Incubation period")

png(file='plot_IP_m.png', width = 700, height = 330)
forest(m3, col.diamond = "blue", col.diamond.lines = "black", xlab = "Incubation period (mathematical models)")
dev.off() 

#incubation period (only non mathematical models)

study4 <- c("Angelo (2023)","Català (2022)","Choudury (2022)", "Cobos (2023)", "Gaspari (2022)", "Gomez-Garberi (2022)", 
            "Kroger (2023)",   "Mailhe (2023)","Maldonado (2022)", "Moschese (2023)","Nunez (2023)", "O Laughlin (2022)", 
            "Suarez Rodriguez (2022)","Tarin-Vicente (2022)", "Thornhill (2022)", "Thornhill (2022)")

n4 <- c(226,77,179,19,30,14,209,112,205,32,18,230,45,181,23,51) 


mean4 <- c(8,6,7,8,9,13,8.3,6,7,11,8,7,7,7,7,7)


sd4 <- c(8.5,7,8,7.5,14,29.2,5.2,7.25,12.5,10.5,7,7,18.7,19.25,19.25,23.75)


df4 <- data.frame(study4,n4,mean4,sd4)

m4 <- metamean(n = n4,
               mean = mean4,
               sd = sd4,
               studlab = study4,
               data = df4,
               sm = 'MLN',
               random = TRUE,
               common = F,
               warn = F,
               prediction = F,
               title = "Incubation period")


png(file='plot_IP_p.png', width = 700, height = 400)
forest(m4, col.diamond = "blue", col.diamond.lines = "black", xlab = "Incubation period (primary study designs)")
dev.off()


#incubation period 










