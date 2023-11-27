

# Load necessary libraries


library(ggplot2)
library(dplyr)
library(readxl)
library(ggthemes)

# Data frame with studies and their respective data points


rm(list = ls())
dev.off()

get_GT <- function(par_colour = "#f0027f")  {
  dat <- read_excel("data/mpox_meta_results.xlsx")
  table(dat$Parameter)
  param_name <- "Generation Time"
  lable_name <- "Days"
  df <- dat %>% filter(Parameter == param_name,
                       !Study == "Heterogeneity")
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
                                 width = 0.5, 
                                 height = 5)
  label_effect_model <-  paste0(round(effect_model$Mean,2), " [",
                                round(effect_model$LowerCI,2), ";",
                                round(effect_model$UpperCI,2), "]")
  ggplot(df, aes(x = StudyLevels, y = Mean)) +
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.01, color = "black", size = 0.3) + # Confidence interval error bars
    geom_hline(yintercept = effect_model$Mean, linetype = "dashed", color = "darkblue") + # Vertical line at x = 1
    geom_point(aes(size = Weight), fill = par_colour, pch = 22, colour = "black") + # Use color to differentiate points
    geom_point(colour = "black", size = 0.1) + # Use color to differentiate points
    geom_polygon(data = diamond_data, aes(x = x, y = y), fill = "darkblue") + # Add the filled diamond shape
    cowplot::theme_minimal_grid() +
    coord_flip(ylim = c(0, 20)) + # Flip coordinates for the forest plot
    theme_bw()+
    theme(legend.position = c(0.8, 0.5), legend.key.size = unit(1, 'mm'), 
          legend.text = element_text(size=8), legend.title = element_text(size=8))  +
    labs(title = param_name, x = "", y = lable_name, subtitle = label_effect_model) + # Title of the plot 
    theme(plot.subtitle=element_text(size=8, hjust=0, face="italic", color="darkblue")) +
    theme(legend.position = "none")
}

get_SI <- function(par_colour = "#2b8cbe")  { 
  dat <- read_excel("data/mpox_meta_results.xlsx")
  table(dat$Parameter)
  param_name <- "Serial Interval"
  lable_name <- "Days"
  df <- dat %>% filter(Parameter == param_name,
                       !Study == "Heterogeneity")
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
                                 width = 0.5, 
                                 height = 5)
  label_effect_model <-  paste0(round(effect_model$Mean,2), " [",
                                round(effect_model$LowerCI,2), ";",
                                round(effect_model$UpperCI,2), "]")
  ggplot(df, aes(x = StudyLevels, y = Mean)) +
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.01, color = "black", size = 0.3) + # Confidence interval error bars
    geom_hline(yintercept = effect_model$Mean, linetype = "dashed", color = "darkblue") + # Vertical line at x = 1
    geom_point(aes(size = Weight), fill = par_colour, pch = 22, colour = "black") + # Use color to differentiate points
    geom_point(colour = "black", size = 0.1) + # Use color to differentiate points
    geom_polygon(data = diamond_data, aes(x = x, y = y), fill = "darkblue") + # Add the filled diamond shape
    # geom_text(data = effect_model, 
    #           aes(label = label_effect_model),
    #           hjust = 0, vjust = 1, nudge_x = 0.5, nudge_y = 3, 
    #           size = 4, colour = "darkblue") + # Label for the summary effect
    cowplot::theme_minimal_grid() +
    coord_flip(ylim = c(0,20)) + # Flip coordinates for the forest plot
    theme_bw()+
    theme(legend.position = c(0.8, 0.5), legend.key.size = unit(1, 'mm'), 
          legend.text = element_text(size=8), legend.title = element_text(size=8))  +
    labs(title = param_name, x = "", y = lable_name, subtitle = label_effect_model) + # Title of the plot 
    theme(plot.subtitle=element_text(size=8, hjust=0, face="italic", color="darkblue")) +
    theme(legend.position = "none")
}

get_IP <- function(par_colour = "#66c2a5")  {
  dat <- read_excel("data/mpox_meta_results.xlsx")
  table(dat$Parameter)
  param_name <- "Incubation Period"
  lable_name <- "Days"
  df <- dat %>% filter(Parameter == param_name,
                       !Study == "Heterogeneity")
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
                                 width = 0.5, 
                                 height = 5)
  label_effect_model <-  paste0(round(effect_model$Mean,2), " [",
                                round(effect_model$LowerCI,2), ";",
                                round(effect_model$UpperCI,2), "]")
  ggplot(df, aes(x = StudyLevels, y = Mean)) +
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.01, color = "black", size = 0.3) + # Confidence interval error bars
    geom_hline(yintercept = effect_model$Mean, linetype = "dashed", color = "darkblue") + # Vertical line at x = 1
    geom_point(aes(size = Weight), fill = par_colour, pch = 22, colour = "black") + # Use color to differentiate points
    geom_point(colour = "black", size = 0.1) + # Use color to differentiate points
    geom_polygon(data = diamond_data, aes(x = x, y = y), fill = "darkblue") + # Add the filled diamond shape
    cowplot::theme_minimal_grid() +
    coord_cartesian(xlim = c(0,20)) +
    coord_flip(ylim=c(0,20)) + # Flip coordinates for the forest plot
    theme_bw() +
    theme(legend.position = c(0.8, 0.5), legend.key.size = unit(1, 'mm'), 
          legend.text = element_text(size=8), legend.title = element_text(size=8))  +
    labs(title = param_name, x = "", y = lable_name, subtitle = label_effect_model) + # Title of the plot 
    theme(plot.subtitle=element_text(size=8, hjust=0, face="italic", color="darkblue")) +
    theme(legend.position = "none")
}

get_cfr <- function(colour_fill = "#9970ab")  {
  dat <- read_excel("data/mpox_meta_results.xlsx")
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
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.01, color = "black", size = 0.3) + # Confidence interval error bars
    geom_hline(yintercept = effect_model$Mean, linetype = "dashed", color = "darkblue") + # Vertical line at x = 1
    geom_point(aes(size = Weight), fill = colour_fill, pch = 22, colour = "black") + # Use color to differentiate points
    geom_point(colour = "black", size = 0.1) + # Use color to differentiate points
    geom_polygon(data = diamond_data, aes(x = x, y = y), fill = "darkblue") + # Add the filled diamond shape
    cowplot::theme_minimal_grid() +
    coord_flip(ylim = c(0, 8)) + # Flip coordinates for the forest plot
    labs(title = param_name, x = "", y = lable_name) + # Title of the plot 
    theme_bw()+
    theme(legend.position = c(0.8, 0.5), legend.key.size = unit(1, 'mm'), 
          legend.text = element_text(size=8), legend.title = element_text(size=8))  +
    labs(title = param_name, x = "", y = lable_name, subtitle = label_effect_model) + # Title of the plot 
    theme(plot.subtitle=element_text(size=8, hjust=0, face="italic", color="darkblue")) +
    theme(legend.position = "none") 
}



ip <- get_IP()
gt <- get_GT()
si <- get_SI()
cfr <- get_cfr ()

library(cowplot)

p1 <- plot_grid(ip, si, gt, 
                nrow = 3, align = "hv",
                rel_heights = c(3, 1.5, 1.2))
p2 <- plot_grid(p1, cfr, nrow = 1)

get_R <- function(par_colour = "orange",   param_name = "R0")  {
  dat <- read_excel("data/mpox_meta_results.xlsx")
  lable_name <- ""
  df <- dat %>% filter(Parameter == param_name,
                       !Study == "Heterogeneity")
  df$Study[df$Study=="Random effects model"] <- "RE Model"
  df_nomodel <- df %>% filter(!Study == "RE Model")
  names_studies <- sort(df_nomodel$Study, decreasing = FALSE)
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
  
  df$Weight <- 1
  ggplot(df, aes(x = StudyLevels, y = Mean)) +
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.2, color = par_colour, size = 1) + # Confidence interval error bars
    geom_point(aes(size = Weight), fill = par_colour, pch = 21, colour = "black") + # Use color to differentiate points
    geom_point(colour = "black", size = 0.1) + # Use color to differentiate points
    cowplot::theme_minimal_grid() +
    coord_cartesian(ylim = c(0, 5)) +
    # coord_flip(ylim = c(0, 5)) + # Flip coordinates for the forest plot
    theme_bw()+
    theme(legend.position = c(0.8, 0.5), legend.key.size = unit(1, 'mm'), 
          legend.text = element_text(size=8), legend.title = element_text(size=8))  +
    labs(title = param_name, x = "", y = lable_name) + # Title of the plot 
    theme(plot.subtitle=element_text(size=8, hjust=0, face="italic", color="darkblue")) +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

  }

r0 <- get_R(param_name = "R0", par_colour = "orange")
rt <- get_R(param_name = "R(t)", par_colour = "#fb9a99")

p3 <- plot_grid(r0, rt, nrow = 1, rel_widths = c(1, 1.5), align = "hv")

min(rt$data$Mean, na.rm = TRUE)
max(rt$data$Mean, na.rm = TRUE)

