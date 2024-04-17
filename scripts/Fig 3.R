

# Load necessary libraries


library(ggplot2)
library(dplyr)
library(readxl)
library(ggthemes)
library(cowplot)


# Data frame with studies and their respective data points


rm(list = ls())
# dev.off()



get_SI <- function(par_colour = "#2b8cbe", size_text = size_text)  { 
  dat <- read_excel("data/mpox_meta_results_march_2024.xlsx")
  table(dat$Parameter)
  param_name <- "Serial Interval"
  lable_name <- "Days"
  df <- dat %>% filter(Parameter == param_name,
                       !Study == "Heterogeneity")
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
  ggplot(df, aes(x = StudyLevels, y = Mean)) +
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), color = "black", width = 0.001, size = size_text/8) + # Confidence interval error bars
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI, color = Continent), width = 0.01, size = size_text/10) + # Confidence interval error bars
    geom_hline(yintercept = effect_model$Mean, linetype = "dashed", color = "black", size = size_text/15) + # Vertical line at x = 1
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
    theme(legend.position = "none") +
    scale_color_manual(values = c( "Americas" = "#fb8072",
                                   "Africa" = "#80b1d3",
                                   "Europe"= "#bebada", 
                                   "Several continents" = "#8dd3c7",
                                   "All" = "black")) +
    scale_fill_manual(values = c( "Americas" = "#fb8072",
                                  "Africa" = "#80b1d3",
                                  "Europe"= "#bebada", 
                                  "Several continents" = "#8dd3c7",
                                  "All" = "black")) +
    scale_size_continuous(range = c(1, 15)) 
  
  
}

get_IP <- function(size_text = size_text)  {
  dat <- read_excel("data/mpox_meta_results_march_2024.xlsx")
  table(dat$Parameter)
  param_name <- "Incubation Period"
  lable_name <- "Days"
  df <- dat %>% filter(Parameter == param_name,
                       !Study == "Heterogeneity")
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
  ggplot(df, aes(x =  reorder (StudyLevels, Continent), y = Mean)) +
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), color = "black", width = 0.001, size = size_text/8) + # Confidence interval error bars
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI, color = Continent), width = 0.001, size = size_text/10) + # Confidence interval error bars
    geom_hline(yintercept = effect_model$Mean, linetype = "dashed", color = "black", size = size_text/15) + # Vertical line at x = 1
    geom_point(aes(size = Weight, fill = Continent), pch = 22, colour = "black") + # Use color to differentiate points
    geom_polygon(data = diamond_data, aes(x = x, y = y), fill = "black") + # Add the filled diamond shape
    cowplot::theme_minimal_grid() +
    coord_flip(ylim = c(3,15)) + # Flip coordinates for the forest plot
    theme_bw(size_text) +
    theme(legend.position = c(0.8, 0.5), legend.key.size = unit(1, 'mm'), 
          legend.text = element_text(size=8), legend.title = element_text(size=8))  +
    labs(title = param_name, x = "", y = lable_name, subtitle = label_effect_model) + # Title of the plot 
    theme(plot.subtitle=element_text(size=size_text, hjust=0, face="italic", color="black")) +
    theme(legend.position = "none") +
    scale_color_manual(values = c( "Americas" = "#fb8072",
                                   "Africa" = "#80b1d3",
                                   "Europe"= "#bebada", 
                                   "Several continents" = "#8dd3c7",
                                   "All" = "black")) +
    scale_fill_manual(values = c( "Americas" = "#fb8072",
                                  "Africa" = "#80b1d3",
                                  "Europe"= "#bebada", 
                                  "Several continents" = "#8dd3c7",
                                  "All" = "black")) +
    scale_size_continuous(range = c(1, 15)) 
  
  
}

get_R <- function(par_colour = "orange",   param_name = "R(t)",  
                  lable_name = expression(R[t]), size_text)  {
  dat <- read_excel("data/mpox_meta_results_march_2024.xlsx")
  df <- dat %>% filter(Parameter == param_name,
                       !Study == "Heterogeneity")
  RE_model_name <- "Random Effects Model"
  df$Study[df$Study=="Random effects model"] <- RE_model_name
  df_nomodel <- df %>% filter(!Study == RE_model_name)
  names_studies <- df_nomodel$Study[order(df_nomodel$Continent, decreasing = TRUE)]
  df$StudyLevels <- factor(df$Study,
                           levels = c(RE_model_name, names_studies))
  effect_model <- df %>% filter(Study == RE_model_name)
  
  
  ggplot(df, aes(x = StudyLevels, y = Mean)) +
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), color = "black", width = 0.001, size = size_text/8) + # Confidence interval error bars
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI, color = Continent), width = 0.001, size = size_text/10) + # Confidence interval error bars
    geom_point(aes(fill = Continent, size = 1), pch = 22, colour = "black") + # Use color to differentiate points
    geom_point(colour = "black", size = 0.1) + # Use color to differentiate points
    cowplot::theme_minimal_grid() +
    theme_bw(size_text)+
    theme(legend.position = c(0.8, 0.5), legend.key.size = unit(1, 'mm'), 
          legend.text = element_text(size=8), legend.title = element_text(size=8))  +
    labs(title = lable_name, x = "", y = "") + # Title of the plot 
    theme(plot.subtitle=element_text(size=size_text, hjust=0, face="italic", color="black")) +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5)) +
    coord_flip(ylim = c(0, 8)) +
    scale_color_manual(values = c( "Americas" = "#fb8072",
                                   "Africa" = "#80b1d3",
                                   "Europe"= "#bebada", 
                                   "Several continents" = "#8dd3c7",
                                   "Oceania" = "#9970ab",
                                   "Asia" = "#fb9a99")) +
    scale_fill_manual(values = c( "Americas" = "#fb8072",
                                  "Africa" = "#80b1d3",
                                  "Europe"= "#bebada", 
                                  "Several continents" = "#8dd3c7",
                                  "Oceania" = "#9970ab",
                                  "Asia" = "#fb9a99")) +
    scale_size_continuous(range = c(1, 15)) 
  
  
}


legend_plot <- function(size_text)  {
  dat <- read_excel("data/mpox_meta_results_march_2024.xlsx")
  
  ggplot(dat, aes(x = Study, y = Mean)) +
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI, color = Continent), 
                  width = 0, size = size_text/5) + # Confidence interval error bars
    geom_point(aes(fill = Continent), pch = 22, size = size_text/3) + # Use color to differentiate points
    scale_color_manual(values = c( "Americas" = "#fb8072",
                                   "Africa" = "#80b1d3",
                                   "Europe"= "#bebada", 
                                   "Several continents" = "#8dd3c7",
                                   "Oceania" = "#9970ab",
                                   "Asia" = "#fb9a99")) +
    scale_fill_manual(values = c( "Americas" = "#fb8072",
                                  "Africa" = "#80b1d3",
                                  "Europe"= "#bebada", 
                                  "Several continents" = "#8dd3c7",
                                  "Oceania" = "#9970ab",
                                  "Asia" = "#fb9a99")) +
    theme(legend.key = element_rect(fill = "white")) +
    theme(legend.spacing.y = unit(0.1, "cm"), legend.spacing.x = unit(0.1, "cm"),
          legend.text = element_text(size=size_text), legend.title = element_text(size=size_text)) +
    scale_size_continuous(range = c(1, 15)) 
 
}

size_text <- 35

ip <- get_IP(size_text = size_text )
si <- get_SI(size_text = size_text )

r0 <- get_R(param_name = "R0", lable_name = expression(R[0]), size_text = size_text )
rt <- get_R(param_name = "R(t)", lable_name = expression(R[t]), size_text = size_text )
rs <- plot_grid(r0, rt,
                nrow = 2, align = "hv",
                rel_heights = c(.4, 0.7), 
                labels = c("C", "D"), label_size = size_text)
legend_manual <- get_legend(legend_plot(size_text = size_text))

left_part <- plot_grid(ip, si,  
                       nrow = 2, align = "hv",
                       rel_heights = c(3, 1.5, 1.2), labels = "AUTO", label_size = size_text)
right_part <- plot_grid(rs, legend_manual, 
                        nrow = 2, align = "hv",
                        rel_heights = c(0.8,0.2),label_size = size_text)


png(filename = "figures/Fig 3.png",
    width = 480*4, height = 480*4, units = "px", 
    pointsize = 12,
    bg = "white")
plot_grid(left_part, right_part, 
          nrow = 1, align = "hv")
dev.off()




