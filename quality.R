library(ggplot2)
library(readxl)
library(tidyverse)
library(ggtext)

#quality mathematical models

mm <- read_excel("data/bd.xlsx", sheet = "quality_mm")

mm$question <- factor(mm$question,                                    
                      levels = c("Q11", "Q10", "Q9","Q8",
                                 "Q7","Q6","Q5",
                                 "Q4","Q3","Q2",
                                 "Q1"))


mm$answer <- factor(mm$answer,                                    
                    levels = c("Yes", "Not clear", "No"))

mm_g <- ggplot(mm, aes(fill = answer, y = proportion, x = question)) +
  geom_bar(colour= "black", position = "stack", stat = "identity") + 
  coord_flip() + labs(title="Quality assessment of mathematical models",
                      x="Question", y = "", fill="Judgement", caption="Q1.Fitted distribution, Q2.Sample size, Q3.Time frame, Q4.Location, Q5.Parameters of the distribution, 
        Q6. Central/Dispersion tendency measures, Q7.Uncertainty values, Q8.Censored data, Q9.Truncated data, 
        Q10. Phase bias adjustment, Q11. Data and code availability") + 
  theme(plot.caption.position="plot") + scale_fill_manual(values=c("#00B159","#F0E442","#ff0000"))
mm_g


#quality cohorts

coh <- read_excel("data/bd.xlsx", sheet = "quality_cohort")

coh$question <- factor(coh$question,                                    
                       levels = c("Q6","Q5",
                                  "Q4","Q3","Q2",
                                  "Q1"))


coh$answer <- factor(coh$answer,                                    
                     levels = c("Yes", "Not clear", "No"))

coh_g <- ggplot(coh, aes(fill = answer, y = proportion, x = question)) +
  geom_bar(colour= "black", position = "stack", stat = "identity") + 
  coord_flip() + labs(title="Quality assessment of cohort studies",
                      x="Question", y = "", fill="Judgement", 
                      caption="Q1.Participants free of the outcome at the moment of exposure, Q2.Outcomes measured in a valid way, Q3. Follow up time sufficient,
                      Q4.Follow up complete, Q5.Strategies to address incomplete follow up, Q6.Appropriate statistical analysis") + 
  theme(plot.caption.position="plot") + scale_fill_manual(values=c("#00B159","#F0E442","#ff0000"))
coh_g


#quality case series


cse <- read_excel("data/bd.xlsx", sheet = "quality_cseries")

cse$question <- factor(cse$question,                                    
                       levels = c("Q9","Q8","Q7","Q6","Q5",
                                  "Q4","Q3","Q2",
                                  "Q1"))


cse$answer <- factor(cse$answer,                                    
                     levels = c("Yes", "Not clear", "No"))

cse_g <- ggplot(cse, aes(fill = answer, y = proportion, x = question)) +
  geom_bar(colour= "black", position = "stack", stat = "identity") + 
  coord_flip() + labs(title="Quality assessment of case series",
                      x="Question", y = "", fill="Judgement", caption="Q1.Clear criteria for inclusion, Q2. Condition measured in a standard, reliable way, Q3. Valid methods used for identification of the condition,
                      Q4. Consecutive inclusion of participants, Q5.Complete inclusion of participants, Q6.Clear reporting of the demographics of participants,
                      Q7. Clear reporting of clinical information, Q8. Clear reporting of the presenting site(s) demographics, Q9. Statistical analysis appropriate") + 
  theme(plot.caption.position="plot") + scale_fill_manual(values=c("#00B159","#F0E442","#ff0000"))
cse_g


#quality cross sectional


csec <- read_excel("data/bd.xlsx", sheet = "quality_csectional")

csec$question <- factor(csec$question,                                    
                        levels = c("Q5",
                                   "Q4","Q3","Q2",
                                   "Q1"))


csec$answer <- factor(csec$answer,                                    
                      levels = c("Yes", "Not clear", "No"))

csec_g <- ggplot(csec, aes(fill = answer, y = proportion, x = question)) +
  geom_bar(colour= "black", position = "stack", stat = "identity") + 
  coord_flip() + labs(title="Quality assessment of cross-sectional studies",
                      x="Question", y = "", fill="Judgement", 
                      caption="Q1.Clear criteria for inclusion, Q2.Detailed description of subjects and settings, 
                      Q3.Standard criteria for measurement of the condition, Q4.Outcomes measured in a valid way, Q5.Appropriate statistical analysis") + 
  theme(plot.caption.position="plot") + scale_fill_manual(values=c("#00B159","#F0E442","#ff0000"))
csec_g

overall <- cowplot::plot_grid(mm_g, cse_g, coh_g, csec_g, nrow = 2, labels = "AUTO")  

ggsave(plot = overall,        
       filename = "overall.png", 
       width = 15.5,                 # Width of image 
       height = 9,             # Height of image 
       dpi = 1000) 

