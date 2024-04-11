

library(meta)
library(metafor)

#serial interval

study <- c("Guo (2022)", "Madewell (2022)", "Miura (2022)", "Ward (2023)", "UKHSA (2022b)", "Zhang (2023)")


n <- c(21,57,34,79,34, 121) 

mean <- c(5.6,8.5,10.1,9.5,9.8, 8.8)

sd <- c(1.5,5,6.1,10.9,19.9, 15.25)

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

summary(m1)

forest(m1, col.diamond = "blue", col.diamond.lines = "black", xlab = "Serial interval")



#incubation period 
study2 <- c("Angelo (2023)",
            "Català (2022)",
            "Charniga (2022)",
            "Choudury (2022)",
           "Cobos (2023)",
            "Gaspari (2022)",
            "Gomez-Garberi (2022)",
            "Guzzeta (2022)",
            "Kroger (2023)",
            "McFarland (2023)",
            "Madawell (2022)",
            "Mailhe (2023)",
           "Maldonado (2022)",
            "Miura (2023a)",
            "Miura (2023b)",
            "Moschese (2023)",
            "Nunez (2023)",
            "O Laughlin (2022)",
            "Suarez Rodriguez (2022)",
            "Tarin-Vicente (2022)",
            "Thornhill (2022a)",
            "Thornhill (2022b)",
            "Ward (2023)",
            "UKHSA (2022a)",
            "UKHSA (2022b)",
           "Zhang (2023)")

n2 <- c(226,
        77,
        22,
        179,
        19,
        30,
        14,
        30,
        209,
        122,
        35,
        112,
        205,
        36,
        36,
        32,
        18,
        230,
        45,
        181,
        51,
        23,
        54,
        33,
        33,
        75) 


mean2 <- c(8,
           6,
           7.6,
           7,
           8,
           9,
           13,
           9.1,
           8.3,
           8.6,
           5.6,
           6,
           7,
           8.1,
           9,
           11,
           8,
           7,
           7,
           7,
           7,
           7,
           7.8,
           9.2,
           9.2,
           6.9)


sd2 <- c(8.5,7,1.8,8,7.5,14,29.25,9.275, 5.2,19,4.4,7.25,12.5,9.425,4.4,10.5,7,7,18.75,19.25,19.25,23.75,7.55,19.75,13.225, 11.9)


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
summary(m2)


forest2 <- forest(m2, col.diamond = "blue", col.diamond.lines = "black", xlab = "Incubation period");forest2




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
            "Ward (2023)",  "UKHSA (1)","UKHSA (2)")

n5 <- c(22,30,122,35,18,36,54,33,33) 


mean5 <- c(7.6,9.1,8,5.6,9.1,8.1,7.8,9.2,9.2)


sd5 <- c(1.8,9.275,19,4.4,9.42,4.4,7.55,19.7,13.2)


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

summary(m5)


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

summary(m6)



forest6 <- forest(m6, col.diamond = "blue", col.diamond.lines = "black", xlab = "Incubation period (non-mathematical models)")



#cfr (global)


CFR_global <- read_excel("data/bd.xlsx", sheet = "Global")

?metaprop


CFR_global<- metaprop(data=CFR_global,
                 n=Total,
                 event = Events,
                 studlab = Study,
                 sm="PLOGIT",
                 method = "GLMM",
                 fixed = F,
                 random = T)


forest7 <- forest(CFR_global, digits = 3L, col.diamond = "blue", col.diamond.lines = "black", xlab = "CFR (Global studies)")

summary(CFR_global)



#cfr (americas)


CFR_americas <- read_excel("data/bd.xlsx", sheet = "Americas")


CFR_americas<- metaprop(data=CFR_americas,
                      n=Total,
                      event = Events,
                      studlab = Study,
                      sm="PLOGIT",
                      method = "GLMM",
                      fixed = F,
                      random = T)


forest8 <- forest(CFR_americas, digits = 3L, col.diamond = "blue", col.diamond.lines = "black", xlab = "CFR (Global studies)")

summary(CFR_americas)


#cfr (europa)


CFR_Europe <- read_excel("data/bd.xlsx", sheet = "Europa")


CFR_Europe<- metaprop(data=CFR_Europe,
                        n=Total,
                        event = Events,
                        studlab = Study,
                        sm="PLOGIT",
                        method = "GLMM",
                        fixed = F,
                        random = T)


forest9 <- forest(CFR_Europe, digits = 3L, col.diamond = "blue", col.diamond.lines = "black", xlab = "CFR (Global studies)")

summary(CFR_Europe)

#cfr (africa)


CFR_Africa <- read_excel("data/bd.xlsx", sheet = "Africa")


CFR_Africa<- metaprop(data=CFR_Africa,
                      n=Total,
                      event = Events,
                      studlab = Study,
                      sm="PLOGIT",
                      method = "GLMM",
                      fixed = F,
                      random = T)


forest10 <- forest(CFR_Africa, digits = 3L, col.diamond = "blue", col.diamond.lines = "black", xlab = "CFR (Global studies)")

summary(CFR_Africa)



