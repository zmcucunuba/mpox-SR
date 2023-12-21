################################################################
##        Figure 4. CFR estimates for mpox
##        Díaz-Brochero et al. 2023.
################################################################

library(ggplot2)
library(dplyr)
library(readxl)
library(ggthemes)
library(meta)

rm(list = ls())

dd <- read_excel("data/mpox_meta_results.xlsx", sheet = "CFR")
dd0 <- dd %>% filter(! Study %in% c("Random effects model", "Heterogeneity" ))

CFR <- metaprop(data=dd0,
                n=Total,
                event = Events,
                subgroup = Continent,
                studlab = Study,
                sm="PLOGIT",
                method = "GLMM",
                fixed = F,
                random = T)

summary(CFR)

png(filename = "figures/Fig 4 metaprop.png",
    width = 480 *3, height = 480 *2, units = "px", 
    pointsize = 12,
    bg = "white")
CFR_forest <- forest(CFR, 
                     digits = 5L, 
                     col.diamond = "blue", 
                     col.diamond.lines = "black", 
                     xlab = "CFR")
dev.off()



get_cfr <- function(continent = "Americas", 
                    colour_fill = "red", 
                    size_text = 10)  {
  dt0 <- read_excel("data/mpox_meta_results.xlsx", sheet = "CFR")
  dat <- dt0 %>% filter(Continent == continent)
  table(dat$Parameter)
  param_name <- "CFR"
  lable_name <- "%"
  df <- dat %>% filter(Parameter == param_name,
                       !Study == "Heterogeneity")
  df$Mean <- df$Mean*100
  df$LowerCI <- df$LowerCI*100
  df$UpperCI <- df$UpperCI*100
  df$Study[df$Study=="Random effects model"] <- "RE Model"
  df_nomodel <- df %>% filter(!Study == "RE Model")
  names_studies <- sort(df_nomodel$Study, decreasing = TRUE)
  df$StudyLevels <- factor(df$Study,
                           levels = c("RE Model", names_studies))
  effect_model <- df %>% filter(Study == "RE Model")
  
  # Function to create diamond shape
  create_diamond <- function(center_x, center_y, width, height) {
    half_width <- width / 2
    half_height <- height / 2
    return(data.frame(
      x = c(center_x, center_x + half_width, center_x, center_x - half_width, center_x),
      y = c(center_y + half_height, center_y, center_y - half_height, center_y, center_y + half_height)
    ))
  }
  
  diamond_data <- create_diamond(center_x = 1, 
                                 center_y = effect_model$Mean, 
                                 width = 1, 
                                 height = 1)
  
  label_effect_model <-  paste0(round(effect_model$Mean,2), " [",
                                round(effect_model$LowerCI,2), ";",
                                round(effect_model$UpperCI,2), "]")
  df$Weight <- df$Total
  df$Weight[df$Study == "RE Model"  ] <- NA
  
  ggplot(df, aes(x = StudyLevels, y = Mean)) +
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), color = colour_fill, width = 0.01, size = size_text/10) + # Confidence interval error bars
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.01, color = "black", size = 0.3) + # Confidence interval error bars
    geom_hline(yintercept = effect_model$Mean, linetype = "dashed", color = "black", size = size_text/10) + # Vertical line at x = 1
    geom_point(aes(size = Weight), fill = colour_fill, pch = 22, colour = "black") + # Use color to differentiate points
    geom_point(colour = "black", size = 0.1) + # Use color to differentiate points
    geom_polygon(data = diamond_data, aes(x = x, y = y), fill = "black") + # Add the filled diamond shape
    cowplot::theme_minimal_grid()  +
    # scale_y_log10(limits = c(0.1, 30)) +
    coord_flip(ylim = c(0, 25)) + # Flip coordinates for the forest plot
    labs(title = param_name, x = "", y = lable_name) + # Title of the plot 
    theme_bw(size_text)+
    theme(legend.position = c(0.8, 0.5), legend.key.size = unit(1, 'mm'), 
          legend.text = element_text(size=8), legend.title = element_text(size=8))  +
    labs(title = continent, x = "", y = lable_name, subtitle = label_effect_model) + # Title of the plot 
    theme(plot.subtitle=element_text(size=size_text, hjust=0, face="italic", color="black")) +
    theme(legend.position = "none") 
}

africa <- get_cfr(continent = "Africa", size_text = 30)
americas <- get_cfr(continent = "Americas", size_text = 30)
europe <- get_cfr(continent = "Europe", size_text = 30)
several <- get_cfr(continent = "Americas & Europe", size_text = 30)
all <- get_cfr(continent = "All", size_text = 30)

table(dd$Continent) / sum(table(dd$Continent))

png(filename = "figures/Fig 4_lineal.png",
    width = 480 *2, height = 480 *4, units = "px", 
    pointsize = 12,
    bg = "white")
cowplot::plot_grid(africa, americas, europe, several, all,
                   rel_heights = 
                     c(0.2, 0.25, 0.4, 0.3, 0.15),
                   nrow = 5, align = "hv",
                   labels = "AUTO", label_size = 30)
dev.off()

