

# Load necessary libraries


library(ggplot2)
library(dplyr)
library(readxl)
library(ggthemes)
library(cowplot)


# Data frame with studies and their respective data points


rm(list = ls())
dev.off()


get_GT <- function(par_colour = "#f0027f", size_text = size_text)  {
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
                                 width = 1, 
                                 height = 4)
  label_effect_model <-  paste0(round(effect_model$Mean,2), " [",
                                round(effect_model$LowerCI,2), ";",
                                round(effect_model$UpperCI,2), "]")
  ggplot(df, aes(x = StudyLevels, y = Mean)) +
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI, color = Continent), width = 0.01, size = size_text/10) + # Confidence interval error bars
    geom_hline(yintercept = effect_model$Mean, linetype = "dashed", color = "black", size = size_text/10) + # Vertical line at x = 1
    geom_point(aes(size = Weight, fill = Continent), pch = 22, colour = "black") + # Use color to differentiate points
    geom_point(colour = "black", size = 0.1) + # Use color to differentiate points
    geom_polygon(data = diamond_data, aes(x = x, y = y), fill = "black") + # Add the filled diamond shape
    cowplot::theme_minimal_grid() +
    coord_flip(ylim = c(3, 15)) + # Flip coordinates for the forest plot
    theme_bw(size_text)+
    theme(legend.position = c(0.8, 0.5), legend.key.size = unit(1, 'mm'), 
          legend.text = element_text(size=8), legend.title = element_text(size=8))  +
    labs(title = param_name, x = "", y = lable_name, subtitle = label_effect_model) + # Title of the plot 
    theme(plot.subtitle=element_text(size=size_text, hjust=0, face="italic", color="black")) +
    theme(legend.position = "none")
}

get_SI <- function(par_colour = "#2b8cbe", size_text = size_text)  { 
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
                                 width = 1, 
                                 height = 3.5)
  label_effect_model <-  paste0(round(effect_model$Mean,2), " [",
                                round(effect_model$LowerCI,2), ";",
                                round(effect_model$UpperCI,2), "]")
  ggplot(df, aes(x = StudyLevels, y = Mean)) +
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI, color = Continent), width = 0.01, size = size_text/10) + # Confidence interval error bars
    geom_hline(yintercept = effect_model$Mean, linetype = "dashed", color = "black", size = size_text/10) + # Vertical line at x = 1
    # geom_point(aes(size = Weight), fill = par_colour, pch = 22, colour = "black") + # Use color to differentiate points
    geom_point(aes(size = Weight, fill = Continent), pch = 22, colour = "black") + # Use color to differentiate points
    geom_point(colour = "black", size = 0.1) + # Use color to differentiate points
    geom_polygon(data = diamond_data, aes(x = x, y = y), fill = "black") + # Add the filled diamond shape
    cowplot::theme_minimal_grid() +
    coord_flip(ylim = c(3,15)) + # Flip coordinates for the forest plot
    theme_bw(size_text)+
    theme(legend.position = c(0.8, 0.5), legend.key.size = unit(1, 'mm'), 
          legend.text = element_text(size=size_text), legend.title = element_text(size=8))  +
    labs(title = param_name, x = "", y = lable_name, subtitle = label_effect_model) + # Title of the plot 
    theme(plot.subtitle=element_text(size=size_text, hjust=0, face="italic", color="black")) +
    theme(legend.position = "none")
}

get_IP <- function(par_colour = "#66c2a5",  size_text = size_text)  {
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
                                 width = 1.5, 
                                 height = 2)
  label_effect_model <-  paste0(round(effect_model$Mean,2), " [",
                                round(effect_model$LowerCI,2), ";",
                                round(effect_model$UpperCI,2), "]")
  ggplot(df, aes(x = StudyLevels, y = Mean)) +
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI, color = Continent), width = 0.01, size = size_text/10) + # Confidence interval error bars
    geom_hline(yintercept = effect_model$Mean, linetype = "dashed", color = "black", size = size_text/10) + # Vertical line at x = 1
    geom_point(aes(size = Weight, fill = Continent), pch = 22, colour = "black") + # Use color to differentiate points
    geom_point(colour = "black", size = 0.1) + # Use color to differentiate points
    geom_polygon(data = diamond_data, aes(x = x, y = y), fill = "black") + # Add the filled diamond shape
    cowplot::theme_minimal_grid() +
    # coord_cartesian(xlim = c(0,20)) +
    coord_flip(ylim = c(3,15)) + # Flip coordinates for the forest plot
    theme_bw(size_text) +
    theme(legend.position = c(0.8, 0.5), legend.key.size = unit(1, 'mm'), 
          legend.text = element_text(size=8), legend.title = element_text(size=8))  +
    labs(title = param_name, x = "", y = lable_name, subtitle = label_effect_model) + # Title of the plot 
    theme(plot.subtitle=element_text(size=size_text, hjust=0, face="italic", color="black")) +
    theme(legend.position = "none")
}

get_R <- function(par_colour = "orange",   param_name = "R0", size_text)  {
  dat <- read_excel("data/mpox_meta_results.xlsx")
  lable_name <- ""
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
  
  df$Weight <- 1
  ggplot(df, aes(x = StudyLevels, y = Mean)) +
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI, color = Continent), width = 0.01, size = size_text/10) + # Confidence interval error bars
    geom_point(aes(size = Weight, fill = Continent), pch = 21, colour = "black") + # Use color to differentiate points
    geom_point(colour = "black", size = 0.1) + # Use color to differentiate points
    cowplot::theme_minimal_grid() +
    theme_bw(size_text)+
    theme(legend.position = c(0.8, 0.5), legend.key.size = unit(1, 'mm'), 
          legend.text = element_text(size=8), legend.title = element_text(size=8))  +
    labs(title = param_name, x = "", y = lable_name) + # Title of the plot 
    theme(plot.subtitle=element_text(size=size_text, hjust=0, face="italic", color="black")) +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5)) +
    coord_flip(ylim = c(0, 8))

}

size_text <- 30

ip <- get_IP(size_text = size_text )
gt <- get_GT(size_text = size_text)
si <- get_SI(size_text = size_text )

r0 <- get_R(param_name = "R0", par_colour = "orange", size_text = size_text )
rt <- get_R(param_name = "R(t)", par_colour = "#fb9a99", size_text = size_text )

delays <- plot_grid(ip, si, gt, 
                    nrow = 3, align = "hv",
                    rel_heights = c(3, 1.5, 1.2), labels = "AUTO", label_size = 30)

rs <- plot_grid(r0, rt,
                nrow = 2, align = "hv",
                rel_heights = c(.4, NULL, 0.7), labels = c("C", "D"), label_size = 30)

p3 <- plot_grid(delays, rs, nrow = 1, rel_widths = c(1, 1), align = "hv")



png(filename = "figures/Fig 3.png",
    width = 480*4, height = 480*4, units = "px", 
    pointsize = 12,
    bg = "white")
p3

dev.off()


min(rt$data$Mean, na.rm = TRUE)
max(rt$data$Mean, na.rm = TRUE)


