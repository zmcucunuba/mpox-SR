

library(meta)
library(metafor)

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
               random = T,
               common = F,
               warn = F,
               prediction = F,
               title = "Serial interval")

forest(m1, col.diamond = "blue", col.diamond.lines = "black", xlab = "Serial interval")



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


forest2 <- forest(m2, col.diamond = "blue", col.diamond.lines = "black", xlab = "Incubation period");forest2



#generation time

study3 <- c("Guzzeta (2022)","Wei (2022)")

n3 <- c(16,77) 

mean3 <- c(12.5,10.38)

sd3 <- c(15.42,15.3)

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
               title = "Generation time")


forest3 <- forest(m3, col.diamond = "blue", col.diamond.lines = "black", xlab = "Generation time");forest3


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


forest4 <- forest(CFR_m, digits = 3L, col.diamond = "blue", col.diamond.lines = "black", xlab = "Case fatality rate");forest4

###########Sensitivity analysis#############

#incubation (only mathematical models)

study5 <- c("Charniga (2022)","Guzzeta (2022)", "McFarland (2023)", "Madawell (2022)",  "Miura (2022)", "Miura (2023)", 
            "Ward (2023)", "Wei (2022)", "UKHSA (1)","UKHSA (2)")

n5 <- c(22,30,122,35,18,36,54,77,33,33) 


mean5 <- c(7.6,9.1,8,5.6,9.1,8.1,7.8,8.23,9.2,9.2)


sd5 <- c(1.8,9.275,19,4.4,9.42,4.4,7.55,20.17,19.7,13.2)


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
               title = "Incubation period")


forest5 <- forest(m5, col.diamond = "blue", col.diamond.lines = "black", xlab = "Incubation period (mathematical models)")


#incubation period (only non mathematical models)

study6 <- c("Angelo (2023)","Català (2022)","Choudury (2022)", "Cobos (2023)", "Gaspari (2022)", "Gomez-Garberi (2022)", 
            "Kroger (2023)",   "Mailhe (2023)","Maldonado (2022)", "Moschese (2023)","Nunez (2023)", "O Laughlin (2022)", 
            "Suarez Rodriguez (2022)","Tarin-Vicente (2022)", "Thornhill (2022)", "Thornhill (2022)")

n6 <- c(226,77,179,19,30,14,209,112,205,32,18,230,45,181,23,51) 


mean6 <- c(8,6,7,8,9,13,8.3,6,7,11,8,7,7,7,7,7)


sd6 <- c(8.5,7,8,7.5,14,29.2,5.2,7.25,12.5,10.5,7,7,18.7,19.25,19.25,23.75)


df6 <- data.frame(study6,n6,mean6,sd6)

m6 <- metamean(n = n6,
               mean = mean6,
               sd = sd6,
               studlab = study6,
               data = df6,
               sm = 'MLN',
               random = TRUE,
               common = F,
               warn = F,
               prediction = F,
               title = "Incubation period")



forest6 <- forest(m4, col.diamond = "blue", col.diamond.lines = "black", xlab = "Incubation period (non-mathematical models)")



#cfr (non endemic countries, excluding global studies and nigeria study)


CFR2 <- read_excel("data/bd.xlsx", sheet = "CFR2")
CFR2$ref <- paste0(CFR2$authors, " (", CFR2$year, ")")

CFR2 <- metaprop(data=CFR2,
                 n=total,
                 event = event,
                 studlab = ref,
                 sm="PLOGIT",
                 method = "GLMM",
                 fixed = F,
                 random = T)


forest7 <- forest(CFR2, digits = 3L, col.diamond = "blue", col.diamond.lines = "black", xlab = "CFR non endemic countries")


