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

dd <- read_excel("data/mpox_meta_results.xlsx", sheet = "CFR-exact")
dd0 <- dd %>% filter(! Study %in% c("Random effects model", "Heterogeneity" ))

CFR <- metaprop(data=dd0,
                n=Total,
                event = Events,
                subgroup = Continent,
                studlab = Study,
                sm="PLOGIT",
                method = "Inverse",
                # method.incr = "if0all",
                fixed = FALSE,
                random = TRUE)

summary(CFR)

png(filename = "figures/Fig 4 metaprop_LOGIT_Inverse.png",
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
                    size_text = 10)  {
  dt0 <- read_excel("data/mpox_meta_results.xlsx", sheet = "CFR-exact")
  dat <- dt0 %>% filter(Continent == continent)
  table(dat$Parameter)
  param_name <- "CFR"
  lable_name <- "%"
  df <- dat %>% filter(Parameter == param_name,
                       !Study == "Heterogeneity")
  df$Mean <- df$Mean*100
  df$LowerCI <- df$LowerCI*100
  df$UpperCI <- df$UpperCI*100
  RE_model_name <- "Random Effects Model"
  df$Study[df$Study=="Random effects model"] <- RE_model_name
  df_nomodel <- df %>% filter(!Study == RE_model_name)
  names_studies <- df_nomodel$Study[order(df_nomodel$Continent, decreasing = TRUE)]
  df$StudyLevels <- factor(df$Study,
                           levels = c(RE_model_name, names_studies))
  effect_model <- df %>% filter(Study == RE_model_name)
  
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
  df$Weight[df$Study == RE_model_name] <- NA

  
  ggplot(df, aes(x = StudyLevels, y = Mean)) +
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0, color = "black", size = size_text/8) + # Confidence interval error bars
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI, colour = Continent),width = 0, size = size_text/10) + # Confidence interval error bars
    geom_hline(yintercept = effect_model$Mean, linetype = "dashed", color = "black", size = size_text/20) + # Vertical line at x = 1
    geom_point(aes(size = Weight, fill = Continent), pch = 22, colour = "black") + # Use color to differentiate points
    geom_point(colour = "black", size = 0.1) + # Use color to differentiate points
    geom_errorbar(data = effect_model, aes(ymin = LowerCI, ymax = UpperCI), 
                  colour = "black", width = 0.0, size = size_text/10) + # Confidence interval error bars
    geom_polygon(data = diamond_data, aes(x = x, y = y), fill = "black") + # Add the filled diamond shape
    cowplot::theme_minimal_grid()  +
    coord_flip(ylim = c(0, 25)) + # Flip coordinates for the forest plot
    theme_bw(size_text) +
    labs(title = continent, x = "", y = "", subtitle = label_effect_model) + # Title of the plot 
    theme(plot.subtitle=element_text(size=size_text, hjust=0, face="italic", color="black")) +
    theme(legend.position = "none") +
    theme(plot.margin = unit(c(1, 1, 1, 1), "mm")) +
    scale_color_manual(values = c( "Americas" = "#fb8072",
                                   "Africa" = "#80b1d3",
                                   "Europe"= "#bebada")) +
    scale_fill_manual(values = c( "Americas" = "#fb8072",
                                  "Africa" = "#80b1d3",
                                  "Europe"= "#bebada")) +
    scale_size_continuous(range = c(1, 15)) +
    theme(
      plot.margin = margin(5.5, 5.5, 5.5, 3.5),
      plot.subtitle = element_text(margin = margin(b = -1, unit = "pt"))  # Adjust bottom margin of subtitle
    )
}

africa <- get_cfr(continent = "Africa", size_text = 25)
americas <- get_cfr(continent = "Americas", size_text = 25)
europe <- get_cfr(continent = "Europe", size_text = 25)

 # + labs(y = "Case Fatality Ratio (%)")

table(dd$Continent) / sum(table(dd$Continent))

png(filename = "figures/Fig 4.png",
    width = 480 *2, height = 480 *3, units = "px", 
    pointsize = 12,
    bg = "white")
cowplot::plot_grid(africa, americas, europe, 
                   rel_heights = 
                     c(0.22, 0.29, 0.4),
                   nrow = 5, align = "hv"
                   # labels = "AUTO", 
                   # label_size = 30
                   )
dev.off()

